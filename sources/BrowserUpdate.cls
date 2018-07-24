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

register: presenter with: selector
	"send the update to the presenter so that when the presenter gets
	removed, it's events go with it."
	self
		when: #updateReady
		send: #updateReadyForPresenter:selector:
		to: self
		withArguments: (Array with: presenter with: selector) !

unregister: aPresenter

	self removeEventsTriggeredFor: aPresenter!

updateReady
	updates isEmpty ifFalse: [self trigger: #updateReady]!

updateReadyForPresenter: presenter selector: selector
	updates do: [:update | update perform: selector with: presenter]!

updates
	^updates!

updates: aCollection
	"don't just reinitialize the updates instance variable because
	presenters are registered to that object"

	updates removeAll: updates.
	updates addAll: aCollection.
	self updateReady! !
!BrowserUpdate categoriesFor: #debug!accessing!private! !
!BrowserUpdate categoriesFor: #debug:!accessing!private! !
!BrowserUpdate categoriesFor: #initialize!initialization!public! !
!BrowserUpdate categoriesFor: #initializeUpdates!initialization!public! !
!BrowserUpdate categoriesFor: #performDebugUpdateSelector:presenter:!public!updating! !
!BrowserUpdate categoriesFor: #register:with:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #unregister:!public!registeringPresenters! !
!BrowserUpdate categoriesFor: #updateReady!public!updating! !
!BrowserUpdate categoriesFor: #updateReadyForPresenter:selector:!public!updating! !
!BrowserUpdate categoriesFor: #updates!accessing!public! !
!BrowserUpdate categoriesFor: #updates:!accessing!public! !

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

