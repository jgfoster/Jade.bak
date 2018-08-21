"Filed out from Dolphin Smalltalk 7"!

JadeDebugger subclass: #RowanDebugger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'debuggerClass semaphore'!
RowanDebugger guid: (GUID fromString: '{cae790de-8a45-49eb-813a-6cb32999cd4d}')!
RowanDebugger comment: ''!
!RowanDebugger categoriesForClass!Unclassified! !
!RowanDebugger methodsFor!

_terminateProcess: aRowanProcessService

	gciSession terminate: aRowanProcessService oopType.
!

browseFrameMethod

	| method |
	method := frame gsMethod.
	(RowanSystemBrowser showOnSession: self gciSession)
		selectClass: method className asString
		selector: method selector asString.
!

browseImplementors

	^(RowanMethodListBrowser showOnSession: self gciSession) primaryPresenter
		browseImplementorsOf: frame gsMethod selector;
		yourself.
!

browseObjectClass

	| variable |
	variable := variableListPresenter selection.
	(RowanSystemBrowser showOnSession: self gciSession)
		selectClass: variable className asString
		selector: ''.
!

browseSenders

	^(RowanMethodListBrowser showOnSession: self gciSession) primaryPresenter
		browseSendersOf: frame gsMethod selector;
		yourself.
!

createSchematicWiring

	super createSchematicWiring.
	frameListPresenter when: #actionPerformed send: #browseClass to: self.
!

documentPresenter

	^documentPresenter!

errorMessagePresenter
	^errorMessagePresenter!

frameListPresenter
	^frameListPresenter!

getProcessList
	"#getProcessList copied from RowanProjectsBrowserPresenter>>updatePresenters"

	self updateServices: (Array with: (RowanDebuggerService onProcess: gsProcess)).!

issueCommand: services
	| stonString stonResultString |
	services do:[:service | service prepareForReplication].
	stonString := STON toString: services.
	[stonResultString := self gciSession serverPerform: #updateFromSton: with: stonString]
		ensure: [services do: [:service | service clearCommand]].
	^self update: services afterStonReplication: stonResultString.!

methodSourcePresenter

	^codePane!

onIdleEntered
	"Support for automated testing so we know when UI is ready"

	| cachedClass cachedSession |
	cachedClass := self class.
	cachedSession := gciSession.
	[
		[
			(Delay forMilliseconds: 10) wait.
			cachedSession isCallInProgress.
		] whileTrue: [].
		cachedClass signalSemaphore.
	] forkAt: Processor userBackgroundPriority.
	super onIdleEntered.!

onViewClosed
	"copied from RowanSystemBrowserPresenter>>onViewClosed"

	processList := processListPresenter list collect: [:each | GsProcess session: gciSession oopType: each oopType].
	super onViewClosed.
	subPresenters do: [:presenter |
		BrowserUpdate current removeEventsTriggeredFor: presenter.
	].
	"Allow test to know when view is closed"
	self class signalSemaphore.
!

onViewOpened
	"copied from RowanProjectsBrowserPresenter>>onViewOpened"

	super onViewOpened.
	self 
		registerPresentersForUpdates;
		"updatePresenters;"	"see #getProcessList"
		yourself.
!

processListPresenter
	^processListPresenter!

queryCommand: query

	processList := processListPresenter list collect: [:each | GsProcess session: gciSession oopType: each oopType].
	(#(#'resumeProcess' #'runToCursor' #'stepInto' #'stepOut' #'stepOver') includes: query commandSymbol) ifTrue: [
		query isEnabled: processListPresenter selectionByIndex == 1.
		^true
	].
	(#(#'terminateProcess') includes: query commandSymbol) ifTrue: [
		query isEnabled: (processList notEmpty and: [processList first ~~ gsProcess]).
		^true
	].
	(#(#'implement') includes: query commandSymbol) ifTrue: [
		query isEnabled: false. "frameListPresenter model first subStrings first = 'MessageNotUnderstood'."
		^true
	].
	(#(#'browseObjectClass') includes: query commandSymbol) ifTrue: [
		query isEnabled: variableListPresenter hasSelection.
		^true
	].
	^super queryCommand: query
!

registerPresentersForUpdates
	"copied from RowanProjectsBrowserPresenter>>registerPresentersForUpdates"

	BrowserUpdate current
		register: processListPresenter 
		selector: #processListUpdate:
!

saveMethod

	| classService result string |
	string := self getDocumentData reject: [:each | each == Character cr].
	classService := RowanClassService new
		name: frame method className;
		meta: frame method meta;
		yourself.
	classService
		command: #saveMethodSource:category:;
		commandArgs: (Array with: string with: frame method category).
self halt.
	[self issueCommand: (Array with: classService)] on: GsCompileError
		do: [:ex | ^self reportCompileError: ex list in: nil].
	(self compilationWarningFrom: classService selectedMethods)
		ifNotNil: [:warning | self statusBarText: warning]
"
self halt.
	result := gciSession
		serverPerform: #'compile:frame:process:' 
		with: (self getDocumentData reject: [:each | each == Character cr])
		with: frameListPresenter selectionByIndex + frameListOffset
		with: gsProcess.
	(result isKindOf: Boolean) ifTrue: [
		result ifTrue: [
			documentPresenter isModified: false.
			gsProcess trimStackToLevel: frameListPresenter selectionByIndex + frameListOffset.
			^self update.
		] ifFalse: [
			MessageBox notify: 'New method has different selector so will not be on stack!!'.
			^self selectedFrame.
		].
	].
	MessageBox notify: result.
"!

selectedFrame 

	| index breaks source |
	(index := frameListPresenter selectionByIndex) = 0 ifTrue: [^self].
	index := index + frameListOffset.
	(frame := gsProcess frameForLevel: index) isNil ifTrue: [^self].
	frame stepPoint ifNil: [RowanDebuggerStepPointError signal].		"see comment in  RowanFrameService>>initializeProcess:level:organizer:"
	source := frame source.
	breaks := frame breaks collect: [:each | frame offsets at: each].
	breaks := breaks collect: [:each | ((source copyFrom: 1 to: each) select: [:char | char = Character lf]) size + 1].
	breaks := breaks asSet asSortedCollection asArray.
	variableListPresenter list: frame vars.
	codePane 
		gsMethod: frame gsMethod;
		showStepPoint: frame stepPoint;
		yourself.
	"breaks do: [:each | 
		documentPresenter view 
			addMarkerType: #'breakpoint' 
			at: each.
	]."
	(self view viewNamed: 'toolbar') update.!

stepInto

	gciSession
		stepInto: (GsProcess session: gciSession oopType: gsProcess oopType)
		inFrame: frameListPresenter selectionByIndex + frameListOffset.
	[
		self getProcessList.
	] on: RowanDebuggerStepPointError do: [:ex | 
		self stepInto.
	].!

stepOut

	gciSession
		stepOut: (GsProcess session: gciSession oopType: gsProcess oopType)
		inFrame: frameListPresenter selectionByIndex + frameListOffset.
	[
		self getProcessList.
	] on: RowanDebuggerStepPointError do: [:ex | 
		self stepInto.
	].!

stepOver

	gciSession
		stepOver: (GsProcess session: gciSession oopType: gsProcess oopType)
		inFrame: frameListPresenter selectionByIndex + frameListOffset.
	[
		self getProcessList.
	] on: RowanDebuggerStepPointError do: [:ex | 
		self stepInto.
	].!

update: services afterStonReplication: stonResults
	BrowserUpdate current update: services afterStonReplication: stonResults!

updateServices: services
	"copied from RowanProjectsBrowserPresenter>>updateServices:"

	| stonString stonResults |
	services do: [:service | service prepareForReplication].
	stonString := STON toString: services.
	stonResults := self gciSession serverPerform: #updateFromSton: with: stonString.
	self update: services afterStonReplication: stonResults!

variableDataPresenter
	^variableDataPresenter!

variableListPresenter
	^variableListPresenter! !
!RowanDebugger categoriesFor: #_terminateProcess:!public! !
!RowanDebugger categoriesFor: #browseFrameMethod!public! !
!RowanDebugger categoriesFor: #browseImplementors!public! !
!RowanDebugger categoriesFor: #browseObjectClass!public! !
!RowanDebugger categoriesFor: #browseSenders!public! !
!RowanDebugger categoriesFor: #createSchematicWiring!public! !
!RowanDebugger categoriesFor: #documentPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #errorMessagePresenter!Accessing!public! !
!RowanDebugger categoriesFor: #frameListPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #getProcessList!public! !
!RowanDebugger categoriesFor: #issueCommand:!public! !
!RowanDebugger categoriesFor: #methodSourcePresenter!public! !
!RowanDebugger categoriesFor: #onIdleEntered!public! !
!RowanDebugger categoriesFor: #onViewClosed!public! !
!RowanDebugger categoriesFor: #onViewOpened!public! !
!RowanDebugger categoriesFor: #processListPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #queryCommand:!public! !
!RowanDebugger categoriesFor: #registerPresentersForUpdates!public! !
!RowanDebugger categoriesFor: #saveMethod!public! !
!RowanDebugger categoriesFor: #selectedFrame!public! !
!RowanDebugger categoriesFor: #stepInto!public! !
!RowanDebugger categoriesFor: #stepOut!public! !
!RowanDebugger categoriesFor: #stepOver!public! !
!RowanDebugger categoriesFor: #update:afterStonReplication:!public! !
!RowanDebugger categoriesFor: #updateServices:!public! !
!RowanDebugger categoriesFor: #variableDataPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #variableListPresenter!Accessing!public! !

!RowanDebugger class methodsFor!

aboutToShowErrorShellOn: gsError
	[
		^self openDebuggerOnException: gsError
	] on: GsError do: [:ex | 
		ex debugger view close.
		MessageBox warning: ex description.
		Processor activeProcess terminate. 
	]!

debuggerClass

	^debuggerClass ifNil: [JadeDebugger]!

debuggerClass: aDebugger
"
	RowanDebugger debuggerClass: nil.
	RowanDebugger debuggerClass: JadeDebugger.
	RowanDebugger debuggerClass: RowanDebugger.
"
	debuggerClass := aDebugger.
!

semaphore
	"Support for automated testing so we know when UI is ready"

	^semaphore!

semaphore: aSemaphore
	"Support for automated testing so we know when UI is ready"

	semaphore := aSemaphore.
!

signalSemaphore

	self semaphore ifNotNil: [:value | 
		[
			value signal.
			SessionManager inputState prod.
		] forkAt: Processor userBackgroundPriority.
	].! !
!RowanDebugger class categoriesFor: #aboutToShowErrorShellOn:!public! !
!RowanDebugger class categoriesFor: #debuggerClass!public! !
!RowanDebugger class categoriesFor: #debuggerClass:!public! !
!RowanDebugger class categoriesFor: #semaphore!public! !
!RowanDebugger class categoriesFor: #semaphore:!public! !
!RowanDebugger class categoriesFor: #signalSemaphore!public! !

