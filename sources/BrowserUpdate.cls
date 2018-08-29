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

performUpdate: updateBlock with: selector
	updates do: 
			[:update |
			update updateType
				ifNil: [updateBlock value: update]
				ifNotNil: [:type | type == selector ifTrue: [updateBlock value: update]].
			]!

register: presenter selector: selector
	"send the update to the presenter so that when the presenter gets
	removed, it's events go with it."

	self
		when: #updateReady
		send: #update:withSelector:
		to: presenter
		withArguments: (Array with: self with: selector)!

register: presenter selector: selector browser: browser
	"send the update to the presenter so that when the presenter gets
	removed, it's events go with it."
	self
		when: #updateReady
		send: #update:withSelector:browser:
		to: presenter
		withArguments: (Array
				with: self
				with: selector
				with: browser)!

update: services afterStonReplication: stonResults
	"assume we get back the 'same' services as we sent"

	| updatedServices |
	self initializeUpdates.
	updatedServices := STON fromString: stonResults.
	updatedServices do: [:newService | services do: [:service | service replicateFrom: newService]].
	self updates: updatedServices.
	updatedServices do: [:service | service releaseReferencedServices].
	^updatedServices!

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

updatesPerform: selector presenter: presenter
	"the update may know which client updates 
	it wants us to care about. Respect that."

	| updateBlock |
	updateBlock := [:update | update perform: selector with: presenter].
	self performUpdate: updateBlock with: selector!

updatesPerform: selector presenter: presenter browser: browser
	"the update may know which client updates 
	it wants us to care about. Respect that."

	| updateBlock |
	updateBlock := 
			[:update |
			update
				perform: selector
				with: presenter
				with: browser].
	self performUpdate: updateBlock with: selector! !
!BrowserUpdate categoriesFor: #debug!accessing!private! !
!BrowserUpdate categoriesFor: #debug:!accessing!private! !
!BrowserUpdate categoriesFor: #initialize!initialization!public! !
!BrowserUpdate categoriesFor: #initializeUpdates!initialization!public! !
!BrowserUpdate categoriesFor: #performUpdate:with:!private!updating! !
!BrowserUpdate categoriesFor: #register:selector:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #register:selector:browser:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #update:afterStonReplication:!public!updating! !
!BrowserUpdate categoriesFor: #updateReady!public!updating! !
!BrowserUpdate categoriesFor: #updates!accessing!public! !
!BrowserUpdate categoriesFor: #updates:!accessing!public! !
!BrowserUpdate categoriesFor: #updatesPerform:presenter:!public!updating! !
!BrowserUpdate categoriesFor: #updatesPerform:presenter:browser:!public!updating! !

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

