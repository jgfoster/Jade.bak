"Filed out from Dolphin Smalltalk 7"!

Model subclass: #BrowserUpdate
	instanceVariableNames: 'updates debug'
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BrowserUpdate guid: (GUID fromString: '{9ec78c86-d74d-477a-bb5a-254ba1c54272}')!
BrowserUpdate comment: 'Coordinate client presenters with updates from the server'!
!BrowserUpdate categoriesForClass!MVP-Models! !
!BrowserUpdate methodsFor!

debug
	^debug!

debug: aBoolean
	aBoolean
		ifTrue: 
			[Smalltalk at: #roundTrips put: 0].
	debug := aBoolean!

initialize
	self initializeUpdates.
	debug := false. !

initializeUpdates
	updates := OrderedCollection new.
	!

register: presenter with: selector
	"send the update to the presenter so that when the presenter gets
	removed, it's events go with it."

	self
		when: #updateReady
		send: #update:withSelector:
		to: presenter
		withArguments: (Array with: self with: selector)!

update: services afterStonReplication: stonResults
	"assume we get back the 'same' services as we sent"

	| updatedServices |
	self initializeUpdates.
	updatedServices := STON fromString: stonResults.
	updatedServices do: [:newService | services do: [:service | service replicateFrom: newService]].
	self updates: updatedServices.
	updatedServices do: [:service | service releaseReferencedServices]!

updateReady
	updates isEmpty ifFalse: [self trigger: #updateReady]!

updates
	^updates!

updates: aCollection
	"don't just reinitialize the updates instance variable because
	presenters are registered to that object"

	updates removeAll: updates.
	updates addAll: aCollection.
	self updateReady!

updatesPerform: selector with: presenter
	"the update may know which client updates 
	it wants us to care about. Respect that."

	updates do: 
			[:update |
			update updateType
				ifNil: [update perform: selector with: presenter]
				ifNotNil: [:type | type == selector ifTrue: [update perform: type with: presenter]]]! !
!BrowserUpdate categoriesFor: #debug!accessing!private! !
!BrowserUpdate categoriesFor: #debug:!accessing!private! !
!BrowserUpdate categoriesFor: #initialize!initialization!public! !
!BrowserUpdate categoriesFor: #initializeUpdates!initialization!public! !
!BrowserUpdate categoriesFor: #register:with:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #update:afterStonReplication:!public!updating! !
!BrowserUpdate categoriesFor: #updateReady!public!updating! !
!BrowserUpdate categoriesFor: #updates!accessing!public! !
!BrowserUpdate categoriesFor: #updates:!accessing!public! !
!BrowserUpdate categoriesFor: #updatesPerform:with:!public!updating! !

!BrowserUpdate class methodsFor!

clearCurrent
"
	BrowserUpdate clearCurrent.
"
	Current := nil!

current
	^Current ifNil: [Current := self new]! !
!BrowserUpdate class categoriesFor: #clearCurrent!accessing!public! !
!BrowserUpdate class categoriesFor: #current!accessing!public! !

