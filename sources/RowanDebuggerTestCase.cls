"Filed out from Dolphin Smalltalk 7"!

JadeiteAbstractTestCase subclass: #RowanDebuggerTestCase
	instanceVariableNames: 'debugger gsProcess process'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanDebuggerTestCase guid: (GUID fromString: '{f159d434-7740-414b-9e64-d9fdde995061}')!
RowanDebuggerTestCase comment: ''!
!RowanDebuggerTestCase categoriesForClass!SUnit! !
!RowanDebuggerTestCase methodsFor!

closeDebuggers

	| list semaphore |
	list := RowanMethodListBrowser allInstances , RowanSystemBrowser allInstances , RowanDebugger allInstances.
	semaphore := Semaphore new.
	list do: [:each | 
		session removeEventsTriggeredFor: each.
		[
			[
				each view close. 
				each become: Object new.
				semaphore signal.
				SessionManager inputState prod.
			] on: ProcessTermination do: [:ex | 	"A debugger wants to terminate the current process!!"
				[
					each become: Object new.
					semaphore signal.
					SessionManager inputState prod.
				] forkAt: Processor userBackgroundPriority.
				ex pass.
			].
		] fork.
		semaphore wait.
		debugger == each ifTrue: [debugger := nil].
	].
	list := RowanMethodListBrowser allInstances , RowanSystemBrowser allInstances , RowanDebugger allInstances.
	self assert: list isEmpty.
!

debuggerDo: aBlock

	self assert: RowanDebugger semaphore isNil.
	RowanDebugger semaphore: Semaphore new.
	aBlock value.
	RowanDebugger semaphore wait.
	RowanDebugger semaphore: nil.
!

getDebugger

	100 timesRepeat: [
		debugger := RowanDebugger allInstances detect: [:each | true] ifNone: [nil].
		(debugger notNil and: [debugger documentPresenter view selection notEmpty]) ifTrue: [^self].
		(Delay forMilliseconds: 10) wait.
	].
!

methodListBrowser

	^RowanMethodListBrowser allInstances detect: [:each | true] ifNone: [nil]
!

openDebuggerOn: aString

	self 
		assert: RowanDebugger allInstances isEmpty;
		debuggerDo: [process := [session executeString: aString] fork];		"Typically ~ 180ms"
		getDebugger;
		yourself.
!

setUp

	| list1 list2 |
	super setUp.
	self closeDebuggers.
	RowanDebugger debuggerClass: RowanDebugger.
	list1 := RowanSystemBrowser allInstances.
	list2 := RowanMethodListBrowser allInstances.
	self 
		assert: session notNil;
		assert: list1 isEmpty;
		assert: list2 isEmpty;
		yourself.
!

systemBrowser

	^RowanSystemBrowser allInstances detect: [:each | true] ifNone: [nil]
!

tearDown

	process ifNotNil: [process terminate. process := nil].
	self 
		terminateGsProcess;
		closeDebuggers;
		yourself.
	RowanDebugger debuggerClass: nil.
	RowanDebugger semaphore: nil.
	super tearDown.
	(Delay forMilliseconds: 50) wait.!

terminateGsProcess

	| string |
	gsProcess ifNil: [^self].
	string := '
| gsProcess |
gsProcess := Object _objectForOop: ' ,gsProcess printString , '.
gsProcess ifNotNil: [gsProcess terminate. (Delay forMilliseconds: 10) wait].
Processor allProcesses includes: gsProcess'.
	gsProcess := nil.
	5 timesRepeat: [
		(debugger gciSession executeString: string) ifFalse: [^self].
	].
	self assert: false.
!

test1
	"Basics of opening a debugger, viewing the UI, and resuming."

	| x |
	self 
		openDebuggerOn: 'nil halt';
		assert: (x := debugger errorMessagePresenter value) = 'a Halt occurred (error 2709)';
		assert: (x := debugger processListPresenter list size) == 1;
		assert: (x := debugger frameListPresenter list size) == 2;
		assert: (x := debugger frameListPresenter list first) = 'Executed Code  @2 line 1';
		assert: (x := debugger variableListPresenter list size) == 1;
		assert: (x := debugger variableListPresenter list first) class == RowanVariableService;		assert: x oop == 20;
		assert: x _key == #'receiver';
		assert: x value = 'nil';
		debuggerDo: [debugger resumeProcess];
		assert: (x := debugger view) class == DeafObject;
		yourself.
!

test2
	"Stepping (note that 3.2.15 has some odd behaviors)"

	self 
		openDebuggerOn: '| x | 
nil halt.
x := 1 + 2 * 3.
1 == 1 ifTrue: [x := 1].
1 to: 1 by: 1 do: [:i | x := i ].
[x := 2] value.
x yourself';
		assert: (debugger errorMessagePresenter value) equals: 'a Halt occurred (error 2709)';
		assert: (debugger frameListPresenter list first) equals: 'Executed Code  @2 line 2';
		assert: (debugger documentPresenter view selection) equals: 'halt';
		debuggerDo: [debugger stepOver];
		assert: (debugger documentPresenter view selection) equals: '+ ';
		debuggerDo: [debugger stepOver];
		assert: (debugger documentPresenter view selection) equals: '* ';
		debuggerDo: [debugger stepOver];
		assert: (debugger documentPresenter view selection) equals: ':=';
		debuggerDo: [debugger stepOver];
		assert: (debugger documentPresenter view selection) equals: '==';
		debuggerDo: [debugger stepOver];
		assert: (debugger documentPresenter view selection) equals: 'ifTrue:';
		debuggerDo: [debugger stepOver];
		assert: (debugger documentPresenter view selection) = ':=';
		debuggerDo: [debugger stepOver];
		debuggerDo: [debugger stepOver];
		assert: (debugger documentPresenter view selection) = 'to:';
		debuggerDo: [debugger stepOver];
		assert: (debugger documentPresenter view selection) = ':=';
		debuggerDo: [debugger stepOver];
		assert: (debugger documentPresenter view selection) equals: 'to:';
		debuggerDo: [debugger stepOver];
		debuggerDo: [debugger stepOver];
		assert: (debugger documentPresenter view selection) equals: 'value';
		debuggerDo: [debugger stepInto];
		assert: (debugger documentPresenter view selection) equals: ':=';
		debuggerDo: [debugger stepOver];
		debuggerDo: [debugger stepOut];
		assert: (debugger documentPresenter view selection) equals: 'yourself';
		debuggerDo: [debugger resumeProcess];
		yourself.
!

test3
	"browse stack class"

	| browser |
	self 
		openDebuggerOn: '| x | 
nil halt.
x := 1 -> 2.
x yourself';
		debuggerDo: [debugger stepOver];
		assert: debugger documentPresenter view selection equals: '->';
		debuggerDo: [debugger stepInto];
		debuggerDo: [debugger browseFrameMethod];
		assert: (browser := self systemBrowser) notNil;
		assert: browser currentCard selectedClasses = #('Object');
		assert: browser currentCard selectedMethodName = '->';
		debuggerDo: [debugger resumeProcess];
		yourself.
!

test4
	"browse variable class"

	| browser |
	self 
		openDebuggerOn: '| x | 
nil halt.
x := 1 -> 2.
x yourself';
		debuggerDo: [debugger stepOver];
		debuggerDo: [debugger stepOver];
		debuggerDo: [debugger stepOver];
		assert: (debugger variableListPresenter selectionByIndex: 2) notNil;
		debuggerDo: [debugger browseObjectClass];
		assert: (browser := self systemBrowser) notNil;
		assert: browser currentCard selectedClasses = #('Association');
		debuggerDo: [debugger resumeProcess];
		yourself.
!

test5
	"browse implementors"

	| browser list |
	self 
		openDebuggerOn: '| x | 
nil halt.
x := 1 -> 2.
x yourself';
		debuggerDo: [debugger stepOver];
		assert: debugger documentPresenter view selection equals: '->';
		debuggerDo: [debugger stepInto];

		debuggerDo: [debugger browseImplementors];
		assert: (browser := self methodListBrowser) notNil;		assert: (list := browser primaryPresenter methodListPresenter primaryPresenter list) notEmpty;
		assert: list size < 10;

		debuggerDo: [debugger resumeProcess];
		yourself.
!

test6
	"browse senders"

	| browser list |
	[
		500 timesRepeat: [
			(Delay forMilliseconds: 10) wait.
			SessionManager inputState prod.
		].
	] forkAt: Processor userBackgroundPriority.
	self 
		openDebuggerOn: '| x | 
nil halt.
x := 1 -> 2.
x yourself';
		assert: debugger documentPresenter view selection equals: 'halt';
		debuggerDo: [debugger stepOver];
		assert: debugger documentPresenter view selection equals: '->';
		debuggerDo: [debugger stepInto];

		debuggerDo: [debugger browseSenders];
		assert: (browser := self methodListBrowser) notNil;
		assert: (list := browser primaryPresenter methodListPresenter primaryPresenter list) notEmpty;
		assert: list size > 10;

		debuggerDo: [debugger resumeProcess];
		yourself.
!

test7
	"set/clear breakpoints"

	| indicator |
	self 
		openDebuggerOn: '| x | 
nil halt.
x := 1 -> 2.
x yourself';
		assert: (indicator := debugger methodSourcePresenter documentPresenter view indicators at: 2) notNil;
		assert: indicator range = (12 to: 15);
		assert: indicator styleName = 8;
		assert: indicator tag = 'step point #2';

		debuggerDo: [debugger methodSourcePresenter setBreakAtStepPoint: 2];
		assert: (indicator := debugger methodSourcePresenter documentPresenter view indicators at: 2) notNil;
		assert: indicator range = (12 to: 15);
		assert: indicator styleName = 9;
		assert: indicator tag = 'Break at step point #2';

		debuggerDo: [debugger methodSourcePresenter clearBreakAtStepPoint: 2];
		assert: (indicator := debugger methodSourcePresenter documentPresenter view indicators at: 2) notNil;
		assert: indicator range = (12 to: 15);
		assert: indicator styleName = 8;
		assert: indicator tag = 'step point #2';

		debuggerDo: [debugger resumeProcess];
		yourself.
!

test8
	"https://github.com/ericwinger/Jade/issues/20"

	self 
		assert: RowanDebugger allInstances isEmpty;
		debuggerDo: [process := [session debugString: '#(abc def) do:[:symbol | symbol size]' fromContext: nil environment: 0] fork];
		getDebugger;
		yourself.
	self debuggerDo: [debugger resumeProcess].
!

testBug74
	"Open debugger on method with breakpoint"

	session executeString: '(Repository compiledMethodAt: #pageSize) setBreakAtStepPoint: 2'.
	self 
		openDebuggerOn: 'SystemRepository pageSize';
		assert: debugger documentPresenter view selection equals: 'zeroArgPrim:50';
		debuggerDo: [debugger resumeProcess];
		yourself.
!

testBug82
	"Terminate a process other than the primary one"

	| confirmProcess |
	gsProcess := session executeString: '[(Delay forSeconds: 9999) wait] fork asOop'.
	self 
		openDebuggerOn: 'nil halt';
		assert: debugger _processList size equals: 2;
		assert: debugger processListPresenter list size equals: 2;
		assert: (debugger processListPresenter selectionByIndex: 2) notNil;
		assert: (debugger _processList at: 2) oopType value equals: gsProcess;
		yourself.
	confirmProcess := [
		[
			((View active printString subStrings: $') at: 2) = 'GemStone/S Process Termi...'.
		] whileFalse: [
			(Delay forMilliseconds: 100) wait.
		].
		View active
			postMessage: 	16r100 "WM_KEYDOWN" 	wParam: 13 "VK_RETURN" lParam: 0;
			postMessage: 	16r101 "WM_KEYUP" 		wParam: 13 "VK_RETURN" lParam: 0;
			yourself.
	] fork.
	[
		self
			assert: debugger terminateProcess notNil;
			assert: debugger _processList size equals: 1;
			assert: debugger processListPresenter list size equals: 1;
			debuggerDo: [debugger resumeProcess];
			yourself.
	] ensure: [
		confirmProcess terminate.
	].!

testContextMenu
	"https://github.com/ericwinger/Jade/issues/68"
 
	| error time view |
	ShortcutMenuStatus reset.
	self openDebuggerOn: 'nil halt'.
	view := debugger documentPresenter view.
	[
		| counter |
		counter := 0.
		[ShortcutMenuStatus status isNil] whileTrue: [(Delay forMilliseconds: 10) wait].
		self assert: ShortcutMenuStatus isOpen.
		[
			counter := counter + 1.
			(Delay forMilliseconds: 100) wait.
			ShortcutMenuStatus isOpen and: [counter <= 10].
		] whileTrue: [
			view
				postMessage: 	16r100 "WM_KEYDOWN" 	wParam: 27 "VK_ESCAPE" lParam: 0;
				postMessage: 	16r101 "WM_KEYUP" 		wParam: 27 "VK_ESCAPE" lParam: 0;
				yourself.
		].
		Transcript show: '
testContextMenu - counter = ' , counter printString.
	] fork.
	view
		postMessage: 16r204 "WM_RBUTTONDOWN" 	wParam: 0 lParam: 16r12C0064;		" (300 @ 100) "
		postMessage: 16r205 "WM_RBUTTONUP" 		wParam: 0 lParam: 16r12C0064;	
		yourself.
	time := Time millisecondClockValue + 2000.
	[
		SessionManager current inputState loopWhile: [ShortcutMenuStatus isClosed not and: [Time millisecondClockValue < time]].
	] on: Error do: [:ex | 
		error := ex.
		ex return.
	].
	self 
		assert: error isNil;
		debuggerDo: [debugger resumeProcess];
		yourself.
!

testErrorInPrintString
	"https://github.com/ericwinger/Jade/issues/49"

	self 
		openDebuggerOn: '(ScaledDecimal _basicNew instVarAt: 1 put: 1; instVarAt: 2 put: nil; printString)';
		assert: debugger errorMessagePresenter value equals: 'a MessageNotUnderstood occurred (error 2010), a UndefinedObject does not understand  #''_generality''';
		assert: (debugger frameListPresenter list at: 3) equals: 'ScaledDecimal >> asString @22 line 15';
		assert: (debugger frameListPresenter selectionByIndex: 3) notNil;
		assert: debugger variableListPresenter list first value equals: '(ScaledDecimal printString error: a MessageNotUnderstood occurred (error 2010), a UndefinedObject does not understand  #''_generality'')';
		debuggerDo: [[debugger _terminateProcess] fork];
		assert: (Delay forMilliseconds: 50) wait notNil;
		assert: (debugger isNil or: [debugger view class == DeafObject]);
		yourself.
!

testSaveMethod
	"https://github.com/ericwinger/Jade/issues/62"

	| number view |
	number := Time millisecondClockValue.
	self 
		openDebuggerOn: 'RowanFrameService new testMethod';
		assert: (view := debugger documentPresenter view) notNil;
		assert: (view selection) equals: 'halt';
		assert: (view selectionRange: (26 to: 99)) selection first equals: $^;
		assert: (view replaceSelection: '^' , number printString) notNil;
		assert: debugger saveMethod notNil;
		halt;
		yourself.! !
!RowanDebuggerTestCase categoriesFor: #closeDebuggers!public! !
!RowanDebuggerTestCase categoriesFor: #debuggerDo:!public! !
!RowanDebuggerTestCase categoriesFor: #getDebugger!public! !
!RowanDebuggerTestCase categoriesFor: #methodListBrowser!public! !
!RowanDebuggerTestCase categoriesFor: #openDebuggerOn:!public! !
!RowanDebuggerTestCase categoriesFor: #setUp!public!running! !
!RowanDebuggerTestCase categoriesFor: #systemBrowser!public! !
!RowanDebuggerTestCase categoriesFor: #tearDown!public!running! !
!RowanDebuggerTestCase categoriesFor: #terminateGsProcess!public!running! !
!RowanDebuggerTestCase categoriesFor: #test1!public!tests! !
!RowanDebuggerTestCase categoriesFor: #test2!public!tests! !
!RowanDebuggerTestCase categoriesFor: #test3!public!tests! !
!RowanDebuggerTestCase categoriesFor: #test4!public!tests! !
!RowanDebuggerTestCase categoriesFor: #test5!public!tests! !
!RowanDebuggerTestCase categoriesFor: #test6!public!tests! !
!RowanDebuggerTestCase categoriesFor: #test7!public!tests! !
!RowanDebuggerTestCase categoriesFor: #test8!public!tests! !
!RowanDebuggerTestCase categoriesFor: #testBug74!public!tests! !
!RowanDebuggerTestCase categoriesFor: #testBug82!public!tests! !
!RowanDebuggerTestCase categoriesFor: #testContextMenu!public!tests! !
!RowanDebuggerTestCase categoriesFor: #testErrorInPrintString!public!tests! !
!RowanDebuggerTestCase categoriesFor: #testSaveMethod!public!tests! !

