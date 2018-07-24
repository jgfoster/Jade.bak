"Filed out from Dolphin Smalltalk 7"!

JadeDebugger subclass: #RowanDebugger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanDebugger guid: (GUID fromString: '{cae790de-8a45-49eb-813a-6cb32999cd4d}')!
RowanDebugger comment: ''!
!RowanDebugger categoriesForClass!Unclassified! !
!RowanDebugger methodsFor!

onViewClosed
	
	super onViewClosed.
	subPresenters do: [:presenter |
		BrowserUpdate current removeEventsTriggeredFor: presenter.
	].
!

onViewOpened

	super onViewOpened.
	self 
		registerPresentersForUpdates;
		updatePresenters;
		yourself.
!

registerPresentersForUpdates

	BrowserUpdate current 
		register: processListPresenter 	with: #'processListUpdate:'		;
		register: frameListPresenter 	with: #'frameListUpdate:'		;
		register: variableListPresenter 	with: #'variableListUpdate:'	;
		register: variableDataPresenter 	with: #'variableDataUpdate:'	;
		yourself.
!

update: services afterStonReplication: stonResults
	"assume we get back the 'same' services as we sent"

	| updatedServices browserUpdate |
	browserUpdate := BrowserUpdate current initializeUpdates.
	updatedServices := STON fromString: stonResults.
	updatedServices do: [:newService | services do: [:service | service replicateFrom: newService]].
	browserUpdate updates: updatedServices.!

updatePresenters

	self updateServices: (Array with: RowanDebuggerService new)!

updateServices: services

	| stonString stonResults |
	stonString := STON toString: services.
	stonResults := self gciSession serverPerform: #updateFromSton: with: stonString.
	self update: services afterStonReplication: stonResults! !
!RowanDebugger categoriesFor: #onViewClosed!public! !
!RowanDebugger categoriesFor: #onViewOpened!public! !
!RowanDebugger categoriesFor: #registerPresentersForUpdates!public! !
!RowanDebugger categoriesFor: #update:afterStonReplication:!public! !
!RowanDebugger categoriesFor: #updatePresenters!public! !
!RowanDebugger categoriesFor: #updateServices:!public! !

