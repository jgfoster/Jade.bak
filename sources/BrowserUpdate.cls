"Filed out from Dolphin Smalltalk 7"!

Model subclass: #BrowserUpdate
	instanceVariableNames: 'updates'
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BrowserUpdate guid: (GUID fromString: '{9ec78c86-d74d-477a-bb5a-254ba1c54272}')!
BrowserUpdate comment: 'Coordinate client presenters with updates from the server'!
!BrowserUpdate categoriesForClass!MVP-Models! !
!BrowserUpdate methodsFor!

dictionaryUpdate: presenter

	updates do:[:update | update dictionaryUpdate: presenter]!

globalsUpdate: presenter

	updates do:[:update | update globalsUpdate: presenter]!

initialize
	self initializeUpdates!

initializeUpdates
	updates := OrderedCollection new.
	!

packagesUpdate: presenter

	updates do:[:update | update packagesUpdate: presenter]!

register: presenter with: selector
	
	self when: #updateReady send: selector to: self with: presenter !

unregister: aPresenter

	self removeEventsTriggeredFor: aPresenter!

updatePresenters

	updates do: [:update | update trigger: #updateReady]. !

updateReady
	self trigger: #updateReady!

updates
	^updates!

updates: aCollection
	"don't just reinitialize the updates instance variable because
	presenters are registered to that object"

	updates removeAll: updates.
	updates addAll: aCollection.
	self updateReady! !
!BrowserUpdate categoriesFor: #dictionaryUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #globalsUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #initialize!initialization!public! !
!BrowserUpdate categoriesFor: #initializeUpdates!initialization!public! !
!BrowserUpdate categoriesFor: #packagesUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #register:with:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #unregister:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #updatePresenters!public!updating! !
!BrowserUpdate categoriesFor: #updateReady!public!updating! !
!BrowserUpdate categoriesFor: #updates!accessing!private! !
!BrowserUpdate categoriesFor: #updates:!accessing!private! !

!BrowserUpdate class methodsFor!

current
	^Current ifNil: [Current := self new]! !
!BrowserUpdate class categoriesFor: #current!accessing!public! !

