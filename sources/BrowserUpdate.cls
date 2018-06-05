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

initialize
	self initializeUpdates!

initializeUpdates
	updates := OrderedCollection new.
	!

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
	updates addAll: aCollection.
	self updateReady. 
	updates removeAll: aCollection.! !
!BrowserUpdate categoriesFor: #dictionaryUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #initialize!initialization!public! !
!BrowserUpdate categoriesFor: #initializeUpdates!initialization!public! !
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

