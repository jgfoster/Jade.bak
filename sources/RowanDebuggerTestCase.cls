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

openDebuggerOn: aString

	self 
		assert: RowanDebugger allInstances isEmpty;
		assert: RowanDebugger semaphore isNil;
		yourself.
	RowanDebugger semaphore: Semaphore new.
	process := [gciSession executeString: aString] fork.
	RowanDebugger semaphore wait.	"Typically ~ 180ms"
	RowanDebugger semaphore: nil.
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
	RowanDebugger debuggerClass: nil.
	self closeDebuggers.
	super tearDown.
!

test1
	"Basics of opening a debugger, viewing the UI, and resuming."

	| x |
	self openDebuggerOn: 'nil halt'.
	self
		assert: (x := debugger errorMessagePresenter value) = 'a Halt occurred (error 2709)';
		assert: (x := debugger processListPresenter list size) == 1;
		assert: (x := debugger frameListPresenter list size) == 2;
		assert: (x := debugger frameListPresenter list first) = 'Executed Code  @2 line 1';
		assert: (x := debugger variableListPresenter list size) == 1;
		assert: (x := debugger variableListPresenter list first) class == RowanVariableService;		assert: x oop == 20;
		assert: x _key == #'receiver';
		assert: x value = 'nil';
		assert: RowanDebugger semaphore isNil;
		yourself.
	RowanDebugger semaphore: Semaphore new.
	debugger resumeProcess.
	RowanDebugger semaphore wait.
	self assert: (x := debugger view) class == DeafObject.! !
!RowanDebuggerTestCase categoriesFor: #closeDebuggers!public!running! !
!RowanDebuggerTestCase categoriesFor: #openDebuggerOn:!public!running! !
!RowanDebuggerTestCase categoriesFor: #setUp!public!running! !
!RowanDebuggerTestCase categoriesFor: #tearDown!public!running! !
!RowanDebuggerTestCase categoriesFor: #test1!public!running! !

