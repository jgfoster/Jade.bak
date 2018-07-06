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

debug
	^debug!

debug: aBoolean
	aBoolean
		ifTrue: 
			[Smalltalk at: #triggers put: 0.
			Smalltalk at: #roundTrips put: 0].
	debug := aBoolean!

dictionaryUpdate: presenter

	updates do:[:update | update dictionaryUpdate: presenter]!

globalsUpdate: presenter

	updates do:[:update | update globalsUpdate: presenter]!

initialize
	self initializeUpdates.
	debug := false. !

initializeUpdates
	updates := OrderedCollection new.
	!

methodFilterUpdate: presenter
	updates do: [:update | update methodFilterUpdate: presenter]!

methodsUpdate: presenter

	updates do:[:update | update methodsUpdate: presenter]!

packagesUpdate: presenter

	updates do:[:update | update packagesUpdate: presenter]!

performDebugUpdateSelector: selector presenter: presenter
	Transcript
		cr;
		show: '====Begin Updates===='.
	self updates do: 
			[:update |
			Transcript
				cr;
				show: update printString.
			update perform: selector with: presenter].
	^Transcript
		cr;
		show: '====End Updates===='!

performUpdateSelector: selector presenter: presenter
	^debug
		ifTrue: [self performDebugUpdateSelector: selector presenter: presenter]
		ifFalse: [self updates do: [:update | update perform: selector with: presenter]]!

projectPackagesUpdate: presenter

	updates do:[:update | update projectPackagesUpdate: presenter]!

projectSelectionUpdate: presenter

	updates do:[:update | update projectSelectionUpdate: presenter]!

projectsUpdate: presenter

	updates do:[:update | update projectsUpdate: presenter]!

register: presenter with: selector
	"send the update to the presenter so that when the presenter gets
	removed, it's events go with it."
	self
		when: #updateReady
		send: #updateReady:
		to: presenter
		with: [self performUpdateSelector: selector presenter: presenter]!

superclassListUpdate: presenter
	updates do: [:update | update superclassListUpdate: presenter]!

unregister: aPresenter

	self removeEventsTriggeredFor: aPresenter!

updateReady
	Smalltalk at: #triggers put: (Smalltalk at: #triggers) + 1.
	updates isEmpty ifFalse: [self trigger: #updateReady]!

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
!BrowserUpdate categoriesFor: #debug!accessing!private! !
!BrowserUpdate categoriesFor: #debug:!accessing!private! !
!BrowserUpdate categoriesFor: #dictionaryUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #globalsUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #initialize!initialization!public! !
!BrowserUpdate categoriesFor: #initializeUpdates!initialization!public! !
!BrowserUpdate categoriesFor: #methodFilterUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #methodsUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #packagesUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #performDebugUpdateSelector:presenter:!public!updating! !
!BrowserUpdate categoriesFor: #performUpdateSelector:presenter:!public!updating! !
!BrowserUpdate categoriesFor: #projectPackagesUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #projectSelectionUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #projectsUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #register:with:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #superclassListUpdate:!public!updating! !
!BrowserUpdate categoriesFor: #unregister:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #updateReady!public!updating! !
!BrowserUpdate categoriesFor: #updates!accessing!private! !
!BrowserUpdate categoriesFor: #updates:!accessing!private! !

!BrowserUpdate class methodsFor!

current
	^Current ifNil: [Current := self new]! !
!BrowserUpdate class categoriesFor: #current!accessing!public! !

