"Filed out from Dolphin Smalltalk 7"!

JadeiteAbstractTestCase subclass: #JadeiteDebuggerTestCase
	instanceVariableNames: 'debugger gsProcess process secondDebugger'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteDebuggerTestCase guid: (GUID fromString: '{f159d434-7740-414b-9e64-d9fdde995061}')!
JadeiteDebuggerTestCase comment: 'Tests the Jadeite debugger. 

It''s recommended to run this set of tests with a fresh login to avoid counting stale processes. '!
!JadeiteDebuggerTestCase categoriesForClass!SUnit! !
!JadeiteDebuggerTestCase methodsFor!

closeDebuggers

	| list semaphore |
	list := JadeiteMethodListBrowser allInstances , RowanSystemBrowser allInstances , RowanDebugger allInstances.
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
	list := JadeiteMethodListBrowser allInstances , RowanSystemBrowser allInstances , RowanDebugger allInstances.
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

getSecondDebugger
	"Private - we opened up one debugger in the test, then another. find the second"

	100 timesRepeat: 
			[secondDebugger := RowanDebugger allInstances detect: [:each | true] ifNone: [nil].
			(secondDebugger notNil and: 
					[secondDebugger gsProcess notNil
						and: [secondDebugger documentPresenter view selection notEmpty and: [secondDebugger ~~ debugger]]])
				ifTrue: [^self].
			(Delay forMilliseconds: 10) wait]!

menuItemIsEnabled: menuItem
	| commandPolicy commandQuery |
	commandPolicy := CommandPolicy defaultClass commandSource: debugger view.
	commandQuery := menuItem queryAlong: commandPolicy.
	^commandQuery isEnabled!

methodListBrowser

	^JadeiteMethodListBrowser allInstances detect: [:each | true] ifNone: [nil]
!

openDebuggerOn: aString

	self 
		assert: RowanDebugger allInstances isEmpty;
		debuggerDo: [process := [session executeString: aString] fork];		
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
	process
		ifNotNil: 
			[process terminate.
			process := nil].
	self
		terminateGsProcess;
		closeDebuggers;
		yourself.
	RowanDebugger debuggerClass: nil.
	RowanDebugger semaphore: nil.
	super tearDown.
	(Delay forMilliseconds: 50) wait.
	projectsBrowser ifNotNil: [projectsBrowser destroy]!

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

test_browseClass
	| browser |
	self testsIssue: #issue169 withTitle: 'Debugger menu option Browse Class broken'.
	self openProjectsBrowser.
	self ensureRowanSample1Available.
	
	[self selectRowanSample1Class.
	self saveIssue169Method.
	self openDebuggerOn: 'RowanSample1 new issue169'.
	debugger documentPresenter view selectionRange: (81 to: 85).	"Select Rowan"
	browser := debugger methodSourcePresenter browseSelectedClass.
	
	[self assert: (browser isKindOf: JadeiteProjectsBrowserPresenter).
	self assert: browser classListPresenter selection name = 'Rowan']
			ensure: [browser topShell destroy].
	debugger documentPresenter view selectionRange: (105 to: 110).	"Select String"
	browser := debugger methodSourcePresenter browseSelectedClass.
	
	[self assert: (browser isKindOf: RowanSystemBrowser).
	self assert: ('*String*' match: browser currentCard selectedClasses first)]
			ensure: [browser topShell destroy]]
			ensure: [self unloadSampleProject]!

test_restartInBlock
	| restartProcess |
	self testsIssue: #issue126
		withTitle: 'Debugger opens second debugger, loses stack in original debugger'.
	self openProjectsBrowser.
	self ensureRowanSample1Available.
	self selectRowanSample1Class.
	self saveInterestingLoopMethod.
	self methodListPresenter selection setBreakAtStepPoint: 8 session: session.	"inside the block"
	
	[self openDebuggerOn: 'RowanSample1 new interestingLoop'.
	restartProcess := [self debuggerDo: [debugger restart]] fork.	"can't restart inside block. expect notifier"
	
	[self getSecondDebugger.
	self denyIsNil: secondDebugger.
	self deny: debugger == secondDebugger.
	self deny: debugger gsProcess oop = secondDebugger gsProcess oop.
	self deny: debugger frameListPresenter list size = secondDebugger frameListPresenter list size]
			ensure: [restartProcess terminate]]
			ensure: 
				[self methodListPresenter selection clearBreakAtStepPoint: 8 session: session.
				self unloadSampleProject]!

test_terminateDoesNotWalkback
	"Terminate a process other than the primary one"

	| terminateProcess |
	self testsIssue: #issue82 withTitle: 'Pressing Terminate button gives walkback'.
	TestMessageBox enableJadeiteTestMessageBox.
	
	[gsProcess := session executeString: '[(Delay forSeconds: 9999) wait] fork asOop'.
	self
		openDebuggerOn: 'nil halt';
		assert: debugger _processList size equals: 2;
		assert: debugger processListPresenter list size equals: 2;
		assert: (debugger processListPresenter selectionByIndex: 2) notNil;
		assert: (debugger _processList at: 2) oopType value equals: gsProcess.
	TestMessageBox plannedResult: #yes.	"confirm we want to terminate process"
	terminateProcess := 
			[self
				assert: debugger terminateProcess notNil;
				assert: debugger _processList size equals: 1;
				assert: debugger processListPresenter list size equals: 1]
					fork.
	self debuggerDo: [debugger resumeProcess]]
			ensure: 
				[TestMessageBox disableJadeiteTestMessageBox.
				terminateProcess terminate]!

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

testBug77

	| string view |
	string := '"
2 + 3
x
"
| x |
x := 4 + 8.
x halt.
'.
	self 
		openDebuggerOn: string;
		assert: (view := debugger documentPresenter view) class equals: ScintillaView;
		assert: (view selectionRange: (3 to: 7)) notNil;
		"debuggerDo: [debugger jadeDisplay];
		assert: view selection equals: ' 5';
		halt;"
		yourself.
!

testContextMenu
	| error time view |
	self testsIssue: #issue68 withTitle: 'Debuggers (old and new) text panes get error on popup menu'.
	ShortcutMenuStatus reset.
	self openDebuggerOn: 'nil halt'.
	view := debugger documentPresenter view.
	
	[| counter |
	counter := 0.
	[ShortcutMenuStatus status isNil] whileTrue: [(Delay forMilliseconds: 10) wait].
	self assert: ShortcutMenuStatus isOpen.
	
	[counter := counter + 1.
	(Delay forMilliseconds: 100) wait.
	ShortcutMenuStatus isOpen and: [counter <= 10]]
			whileTrue: 
				[view
					postMessage: 16r100
						wParam: 27
						lParam: 0;
					postMessage: 16r101
						wParam: 27
						lParam: 0;
					yourself	"WM_KEYDOWN"	"VK_ESCAPE"	"WM_KEYUP"	"VK_ESCAPE"].
	Transcript show: '
testContextMenu - counter = ' , counter printString]
			fork.
	view
		postMessage: 16r204
			wParam: 0
			lParam: 16r12C0064;
		postMessage: 16r205
			wParam: 0
			lParam: 16r12C0064;
		yourself.	"WM_RBUTTONDOWN"	" (300 @ 100) "	"WM_RBUTTONUP"
	time := Time millisecondClockValue + 2000.
	
	[SessionManager current inputState
		loopWhile: [ShortcutMenuStatus isClosed not and: [Time millisecondClockValue < time]]]
			on: Error
			do: 
				[:ex |
				error := ex.
				ex return].
	self
		assert: error isNil;
		debuggerDo: [debugger resumeProcess];
		yourself!

testDebuggerCopyToClipboard
	"Make sure to turn off test message box if you need to
	debug this test. "

	self testsIssue: #issue120 withTitle: 'Debugger, edits in text, copy to clipboard, get error'.
	TestMessageBox enableJadeiteTestMessageBox.
	
	[self openDebuggerOn: 'nil halt'.
	self assert: debugger frameListPresenter list first equals: 'Executed Code  @2 line 1'.
	'notCopiedYet' copyToClipboard.
	self assert: Clipboard current getText = 'notCopiedYet'.
	debugger methodSourcePresenter documentPresenter value: 'new text'.
	debugger methodSourcePresenter documentPresenter view isModified: true.
	TestMessageBox plannedResult: #yes.	"confirm we want to copy to clipboard"
	debugger frameListPresenter selection: (debugger frameListPresenter list at: 2).
	self assert: Clipboard current getText = 'new text']
			ensure: [TestMessageBox disableJadeiteTestMessageBox]!

testDebuggerDoNotCopyToClipboard
	"Make sure to turn off test message box if you need to
	debug this test. "

	self testsIssue: #issue120 withTitle: 'Debugger, edits in text, copy to clipboard, get error'.
	TestMessageBox enableJadeiteTestMessageBox.
	
	[self openDebuggerOn: 'nil halt'.
	self assert: debugger frameListPresenter list first equals: 'Executed Code  @2 line 1'.
	'notCopiedYet' copyToClipboard.
	self assert: Clipboard current getText = 'notCopiedYet'.
	debugger methodSourcePresenter documentPresenter value: 'new text'.
	debugger methodSourcePresenter documentPresenter view isModified: true.
	TestMessageBox plannedResult: #no.	"reject offer to copy to clip board"
	debugger frameListPresenter selection: (debugger frameListPresenter list at: 2).
	self assert: Clipboard current getText = 'notCopiedYet']
			ensure: [TestMessageBox disableJadeiteTestMessageBox]!

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
	session executeString: '(RowanFrameService compiledMethodAt: #testMethod) setBreakAtStepPoint: 1'.
	self 
		openDebuggerOn: 'RowanFrameService new testMethod';
		assert: (view := debugger documentPresenter view) notNil;
		assert: (view selection) equals: 'testMethod';
		assert: (view selectionRange: (14 to: 99)) selection first equals: $^;
		assert: (view replaceSelection: '^' , number printString) notNil;
		assert: debugger saveMethod notNil;
		assert: (view selection) equals: 'testMethod';
		debuggerDo: [debugger resumeProcess];
		assert: (session executeString: 'RowanFrameService new testMethod') equals: number;
		yourself.!

testStackMenuEnablement
	| frameContextMenu |
	self testsIssue: #issue103 withTitle: 'debugger, ''Executed Code'' stack frame, pop up menu items'.
	self openDebuggerOn: 'nil halt'.
	self assert: debugger _processList size equals: 1.
	self assert: debugger frameListPresenter list first equals: 'Executed Code  @2 line 1'.
	frameContextMenu := debugger frameListPresenter view contextMenu.
	frameContextMenu itemsDo: [:item | self deny: (self menuItemIsEnabled: item)].
	debugger frameListPresenter selection: (debugger frameListPresenter list at: 2).
	frameContextMenu itemsDo: [:item | self assert: (self menuItemIsEnabled: item)].
! !
!JadeiteDebuggerTestCase categoriesFor: #closeDebuggers!private!support! !
!JadeiteDebuggerTestCase categoriesFor: #debuggerDo:!private!support! !
!JadeiteDebuggerTestCase categoriesFor: #getDebugger!private!support! !
!JadeiteDebuggerTestCase categoriesFor: #getSecondDebugger!private!support! !
!JadeiteDebuggerTestCase categoriesFor: #menuItemIsEnabled:!private!support! !
!JadeiteDebuggerTestCase categoriesFor: #methodListBrowser!private! !
!JadeiteDebuggerTestCase categoriesFor: #openDebuggerOn:!private! !
!JadeiteDebuggerTestCase categoriesFor: #setUp!private!running! !
!JadeiteDebuggerTestCase categoriesFor: #systemBrowser!private! !
!JadeiteDebuggerTestCase categoriesFor: #tearDown!private!running! !
!JadeiteDebuggerTestCase categoriesFor: #terminateGsProcess!private!running! !
!JadeiteDebuggerTestCase categoriesFor: #test_browseClass!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #test_restartInBlock!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #test_terminateDoesNotWalkback!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #test1!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #test2!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #test3!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #test4!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #test5!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #test6!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #test7!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #test8!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #testBug74!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #testBug77!public! !
!JadeiteDebuggerTestCase categoriesFor: #testContextMenu!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #testDebuggerCopyToClipboard!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #testDebuggerDoNotCopyToClipboard!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #testErrorInPrintString!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #testSaveMethod!public!tests! !
!JadeiteDebuggerTestCase categoriesFor: #testStackMenuEnablement!public!tests! !

