"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #JadeShell
	instanceVariableNames: 'myPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeShell guid: (GUID fromString: '{D1D000EE-0B26-4AF9-8C27-396116CB864D}')!
JadeShell comment: ''!
!JadeShell categoriesForClass!Unclassified! !
!JadeShell methodsFor!

createComponents

	super createComponents.
	self presenterClass ifNotNil: [:class | myPresenter := self add: class new name: 'myPresenter'].
!

logoutRequested: aValueHolder
	"Private - Opportunity to save changes."

	aValueHolder value: true.
!

model: aGciSession

	super model: aGciSession.
	myPresenter notNil ifTrue: [myPresenter model: aGciSession].
	model
		when: #'logoutRequested:'	send: #'logoutRequested:'	to: self;
		when: #'logoutPending'		send: #'exit'						to: self;
		yourself.
!

onViewClosed

	model notNil ifTrue: [
		| temp |
		temp := model.
		model := nil.
		temp removeEventsTriggeredFor: self.
	].
	super onViewClosed.
!

onViewOpened
	"If we have a presenter that is responsible for the entire window, then let it set up the MenuBar"

	super onViewOpened.
	[
		| menuBar |
		menuBar := self view menuBar.
		menuBar isNil ifTrue: [menuBar := MenuBar new].
		myPresenter notNil ifTrue: [myPresenter updateMenuBar: menuBar].
		self view menuBar: menuBar.
	] on: Error do: [:ex | 
		SessionManager current logError: ex.
		MessageBox
			notify: ex description 
			caption: 'Unable to Open ' , self class name.
		Keyboard default isShiftDown ifTrue: [ex halt].
		self view close.
	].

!

presenterClass

	^nil.
!

shellName

	^'Jade Shell'.
!

updateCaption

	self caption: (model titleBarFor: self shellName).
! !
!JadeShell categoriesFor: #createComponents!private! !
!JadeShell categoriesFor: #logoutRequested:!private! !
!JadeShell categoriesFor: #model:!private! !
!JadeShell categoriesFor: #onViewClosed!private! !
!JadeShell categoriesFor: #onViewOpened!private! !
!JadeShell categoriesFor: #presenterClass!overrides!private! !
!JadeShell categoriesFor: #shellName!overrides!private! !
!JadeShell categoriesFor: #updateCaption!overrides!private! !

!JadeShell class methodsFor!

icon

	^Icon fromFile: 'Jade\icons\GS32x32.ico'.
! !
!JadeShell class categoriesFor: #icon!public! !

