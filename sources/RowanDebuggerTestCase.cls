"Filed out from Dolphin Smalltalk 7"!

JadeiteAbstractTestCase subclass: #RowanDebuggerTestCase
	instanceVariableNames: 'debugger gciSession process'
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
	self closeDebuggers.
	RowanDebugger debuggerClass: nil.
	RowanDebugger semaphore: nil.
	super tearDown.
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
! !
!RowanDebuggerTestCase categoriesFor: #closeDebuggers!public!running! !
!RowanDebuggerTestCase categoriesFor: #debuggerDo:!public!running! !
!RowanDebuggerTestCase categoriesFor: #getDebugger!public!running! !
!RowanDebuggerTestCase categoriesFor: #methodListBrowser!public!running! !
!RowanDebuggerTestCase categoriesFor: #openDebuggerOn:!public!running! !
!RowanDebuggerTestCase categoriesFor: #setUp!public!running! !
!RowanDebuggerTestCase categoriesFor: #systemBrowser!public!running! !
!RowanDebuggerTestCase categoriesFor: #tearDown!public!running! !
!RowanDebuggerTestCase categoriesFor: #test1!public!running!tests! !
!RowanDebuggerTestCase categoriesFor: #test2!public!running!tests! !
!RowanDebuggerTestCase categoriesFor: #test3!public!running!tests! !
!RowanDebuggerTestCase categoriesFor: #test4!public!running!tests! !
!RowanDebuggerTestCase categoriesFor: #test5!public!running!tests! !
!RowanDebuggerTestCase categoriesFor: #test6!public!running!tests! !
!RowanDebuggerTestCase categoriesFor: #test7!public!running!tests! !
!RowanDebuggerTestCase categoriesFor: #test8!public!running!tests! !

