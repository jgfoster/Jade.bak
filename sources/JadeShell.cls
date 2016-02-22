"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #JadeShell
	instanceVariableNames: 'gciSession myPresenter'
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

gciSession: aGciSession

	gciSession := aGciSession.
	myPresenter notNil ifTrue: [myPresenter gciSession: aGciSession].
	gciSession
		when: #'logoutRequested:'	send: #'logoutRequested:'	to: self;
		when: #'logoutPending'	send: #'exit'			to: self;
		yourself.
!

logoutRequested: aValueHolder
	"Private - Opportunity to save changes."

	aValueHolder value: true.
!

onViewClosed

	gciSession notNil ifTrue: [
		| temp |
		temp := gciSession.
		gciSession := nil.
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
		SessionManager current pause.
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

	self caption: (gciSession titleBarFor: self shellName).
! !
!JadeShell categoriesFor: #createComponents!private! !
!JadeShell categoriesFor: #gciSession:!private! !
!JadeShell categoriesFor: #logoutRequested:!private! !
!JadeShell categoriesFor: #onViewClosed!private! !
!JadeShell categoriesFor: #onViewOpened!private! !
!JadeShell categoriesFor: #presenterClass!overrides!private! !
!JadeShell categoriesFor: #shellName!overrides!private! !
!JadeShell categoriesFor: #updateCaption!overrides!private! !

!JadeShell class methodsFor!

icon

	^Icon fromFile: 'icons\GS32.ico'.
!

showOnSession: aGciSession

	^self new 
		gciSession: aGciSession;
		createView: self defaultView;
		showShell;
		yourself.
! !
!JadeShell class categoriesFor: #icon!public! !
!JadeShell class categoriesFor: #showOnSession:!public! !

