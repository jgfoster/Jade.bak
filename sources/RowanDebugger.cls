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

documentPresenter

	^documentPresenter!

errorMessagePresenter
	^errorMessagePresenter!

frameListPresenter
	^frameListPresenter!

getProcessList
	"#getProcessList copied from RowanProjectsBrowserPresenter>>updatePresenters"

	self updateServices: (Array with: (RowanDebuggerService onProcess: gsProcess)).!

onIdleEntered
	"Support for automated testing so we know when UI is ready"

	gciSession isCallInProgress ifFalse: [
		self class semaphore ifNotNil: [:semaphore | 
			semaphore signal.
		].
	].!

onViewClosed
	"copied from RowanSystemBrowserPresenter>>onViewClosed"

	super onViewClosed.
	subPresenters do: [:presenter |
		BrowserUpdate current removeEventsTriggeredFor: presenter.
	].
	"Allow test to know when view is closed"
	self class semaphore ifNotNil: [:value | [value signal] forkAt: Processor userBackgroundPriority].!

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

registerPresentersForUpdates
	"copied from RowanProjectsBrowserPresenter>>registerPresentersForUpdates"

	BrowserUpdate current
		register: processListPresenter selector: #processListUpdate:;
		yourself
	"register: frameListPresenter 	with: #'frameListUpdate:'		;
		register: variableListPresenter 	with: #'variableListUpdate:'	;
		register: variableDataPresenter 	with: #'variableDataUpdate:'	;"!

stepOver

	gciSession
		stepOver: (GsProcess session: gciSession oopType: gsProcess oopType)		inFrame: frameListPresenter selectionByIndex + frameListOffset.
	self getProcessList!

update: services afterStonReplication: stonResults
	BrowserUpdate current update: services afterStonReplication: stonResults!

updateServices: services
	"copied from RowanProjectsBrowserPresenter>>updateServices:"

	| stonString stonResults |
	stonString := STON toString: services.
	stonResults := self gciSession serverPerform: #updateFromSton: with: stonString.
	self update: services afterStonReplication: stonResults!

variableDataPresenter
	^variableDataPresenter!

variableListPresenter
	^variableListPresenter! !
!RowanDebugger categoriesFor: #documentPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #errorMessagePresenter!Accessing!public! !
!RowanDebugger categoriesFor: #frameListPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #getProcessList!public! !
!RowanDebugger categoriesFor: #onIdleEntered!public! !
!RowanDebugger categoriesFor: #onViewClosed!public! !
!RowanDebugger categoriesFor: #onViewOpened!public! !
!RowanDebugger categoriesFor: #processListPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #registerPresentersForUpdates!public! !
!RowanDebugger categoriesFor: #stepOver!public! !
!RowanDebugger categoriesFor: #update:afterStonReplication:!public! !
!RowanDebugger categoriesFor: #updateServices:!public! !
!RowanDebugger categoriesFor: #variableDataPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #variableListPresenter!Accessing!public! !

!RowanDebugger class methodsFor!

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
! !
!RowanDebugger class categoriesFor: #debuggerClass!public! !
!RowanDebugger class categoriesFor: #debuggerClass:!public! !
!RowanDebugger class categoriesFor: #semaphore!public! !
!RowanDebugger class categoriesFor: #semaphore:!public! !

