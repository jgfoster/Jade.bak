"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #RowanDebuggerTestCase
	instanceVariableNames: 'debugger gciSession process'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanDebuggerTestCase guid: (GUID fromString: '{f159d434-7740-414b-9e64-d9fdde995061}')!
RowanDebuggerTestCase comment: ''!
!RowanDebuggerTestCase categoriesForClass!SUnit! !
!RowanDebuggerTestCase methodsFor!

closeDebuggers

	RowanDebugger allInstances do: [:each | 
		each view close. 
		GciSession allInstances first removeEventsTriggeredFor: each.
		each become: Object new.
	].!

debuggerDo: aBlock

	self assert: RowanDebugger semaphore isNil.
	RowanDebugger semaphore: Semaphore new.
	aBlock value.
	RowanDebugger semaphore wait.
	RowanDebugger semaphore: nil.
!

openDebuggerOn: aString

	self 
		assert: RowanDebugger allInstances isEmpty;
		debuggerDo: [process := [gciSession executeString: aString] fork];		"Typically ~ 180ms"
		yourself.
	debugger := RowanDebugger allInstances detect: [:each | true].
!

setUp

	super setUp.
	self closeDebuggers.
	RowanDebugger debuggerClass: RowanDebugger.
	gciSession := GciSession allInstances detect: [:each | true] ifNone: [nil].
	self assert: (gciSession notNil and: [gciSession isValidSession]).!

tearDown

	process ifNotNil: [process terminate. process := nil].
	debugger ifNotNil: [debugger view close. debugger := nil].
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

	| x |
	self 
		openDebuggerOn: '| x | 
nil halt.
x := 1 + 2 * 3.
1 == 1 ifTrue: [x := 1].
1 to: 1 by: 1 do: [:i | x := i ].
[x := 2] value.
x yourself';
		assert: (x := debugger errorMessagePresenter value) = 'a Halt occurred (error 2709)';
		assert: (x := debugger frameListPresenter list first) = 'Executed Code  @2 line 2';
		assert: (x := debugger documentPresenter view selection) = 'halt';
		debuggerDo: [debugger stepOver];
		assert: (x := debugger documentPresenter view selection) = '+ ';
		debuggerDo: [debugger stepOver];
		assert: (x := debugger documentPresenter view selection) = '* ';
		debuggerDo: [debugger stepOver];
		assert: (x := debugger documentPresenter view selection) = ':=';
		debuggerDo: [debugger stepOver];
		assert: (x := debugger documentPresenter view selection) = '==';
		debuggerDo: [debugger stepOver];
		assert: (x := debugger documentPresenter view selection) = 'ifTrue:';
		debuggerDo: [debugger stepOver];
		assert: (x := debugger documentPresenter view selection) = ':=';
		debuggerDo: [debugger stepOver];
		debuggerDo: [debugger stepOver];
		assert: (x := debugger documentPresenter view selection) = 'to:';
		debuggerDo: [debugger stepOver];
		assert: (x := debugger documentPresenter view selection) = ':=';
		debuggerDo: [debugger stepOver];
		assert: (x := debugger documentPresenter view selection) = 'to:';
		debuggerDo: [debugger stepOver];
		debuggerDo: [debugger stepOver];
		assert: (x := debugger documentPresenter view selection) = 'value';
		debuggerDo: [debugger stepInto];
		assert: (x := debugger documentPresenter view selection) = ':=';
		debuggerDo: [debugger stepOver];
		debuggerDo: [debugger stepOut];
		assert: (x := debugger documentPresenter view selection) = 'yourself';
		debuggerDo: [debugger resumeProcess];
		yourself.
	x yourself.!

test3
	"browse stack class"

	[
		| browser x |
		browser := RowanSystemBrowser allInstances detect: [:each | true] ifNone: [nil].
		self 
			assert: browser isNil;
			openDebuggerOn: '| x | 
	nil halt.
	x := 1 -> 2.
	x yourself';
			debuggerDo: [debugger stepOver];
			assert: (x := debugger documentPresenter view selection) = '->';
			debuggerDo: [debugger stepInto];
			debuggerDo: [debugger browseFrameMethod];
			yourself.
		browser := RowanSystemBrowser allInstances detect: [:each | true] ifNone: [nil].
		self
			assert: browser notNil;
			assert: browser currentCard selectedClasses = #('Object');
			assert: browser currentCard selectedMethodName = '->';
			debuggerDo: [debugger resumeProcess];
			yourself.
		x yourself.
	] ensure: [
		RowanSystemBrowser allInstances do: [:each | each view close].
	].!

test4
	"browse variable class"

	[
		| browser |
		browser := RowanSystemBrowser allInstances detect: [:each | true] ifNone: [nil].
		self 
			assert: browser isNil;
			openDebuggerOn: '| x | 
	nil halt.
	x := 1 -> 2.
	x yourself';
			debuggerDo: [debugger stepOver];
			debuggerDo: [debugger stepOver];
			debuggerDo: [debugger stepOver];
			assert: (debugger variableListPresenter selectionByIndex: 2) notNil;
			debuggerDo: [debugger browseObjectClass];
			yourself.
		browser := RowanSystemBrowser allInstances detect: [:each | true] ifNone: [nil].
		self
			assert: browser notNil;
			assert: browser currentCard selectedClasses = #('Association');
			yourself.
	] ensure: [
		RowanSystemBrowser allInstances do: [:each | each view close].
	].! !
!RowanDebuggerTestCase categoriesFor: #closeDebuggers!public!running! !
!RowanDebuggerTestCase categoriesFor: #debuggerDo:!public!running! !
!RowanDebuggerTestCase categoriesFor: #openDebuggerOn:!public!running! !
!RowanDebuggerTestCase categoriesFor: #setUp!public!running! !
!RowanDebuggerTestCase categoriesFor: #tearDown!public!running! !
!RowanDebuggerTestCase categoriesFor: #test1!public!running! !
!RowanDebuggerTestCase categoriesFor: #test2!public!running! !
!RowanDebuggerTestCase categoriesFor: #test3!public!running! !
!RowanDebuggerTestCase categoriesFor: #test4!public!running! !

