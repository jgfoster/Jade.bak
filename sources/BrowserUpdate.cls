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

classCategoryUpdate: presenter

	updates do:[:update | update classCategoryUpdate: presenter]!

classCommentUpdate: presenter

	updates do:[:update | update classCommentUpdate: presenter]!

classDefinitionUpdate: presenter
	updates do: [:update | update classDefinitionUpdate: presenter]!

classesHierarchyUpdate: presenter

	updates do:[:update | update classesHierarchyUpdate: presenter]!

classesUpdate: presenter

	updates do:[:update | update classesUpdate: presenter]!

classHierarchyUpdate: presenter

	updates do:[:update | update classHierarchyUpdate: presenter]!

dictionaryUpdate: presenter

	updates do:[:update | update dictionaryUpdate: presenter]!

globalsUpdate: presenter

	updates do:[:update | update globalsUpdate: presenter]!

initialize
	self initializeUpdates!

initializeUpdates
	updates := OrderedCollection new.
	!

methodFilterUpdate: presenter
	updates do: [:update | update methodFilterUpdate: presenter]!

methodsUpdate: presenter

	updates do:[:update | update methodsUpdate: presenter]!

packagesUpdate: presenter

	updates do:[:update | update packagesUpdate: presenter]!

register: presenter with: selector
	"send the update to the presenter so that when the presenter gets
	removed, it's events go with it."
	self
		when: #updateReady
		send: #updateReady:
		to: presenter
		with: [self updates do: [:update | update perform: selector with: presenter]]!

superclassListUpdate: presenter
	updates do: [:update | update superclassListUpdate: presenter]!

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
!BrowserUpdate categoriesFor: #classCategoryUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #classCommentUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #classDefinitionUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #classesHierarchyUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #classesUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #classHierarchyUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #dictionaryUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #globalsUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #initialize!initialization!public! !
!BrowserUpdate categoriesFor: #initializeUpdates!initialization!public! !
!BrowserUpdate categoriesFor: #methodFilterUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #methodsUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #packagesUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #register:with:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #superclassListUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #unregister:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #updatePresenters!public!updating! !
!BrowserUpdate categoriesFor: #updateReady!public!updating! !
!BrowserUpdate categoriesFor: #updates!accessing!private! !
!BrowserUpdate categoriesFor: #updates:!accessing!private! !

!BrowserUpdate class methodsFor!

current
	^Current ifNil: [Current := self new]! !
!BrowserUpdate class categoriesFor: #current!accessing!public! !

