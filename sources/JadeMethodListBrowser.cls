"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadeMethodListBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMethodListBrowser guid: (GUID fromString: '{046cebd5-f142-4382-99ff-b3fd8b8032b9}')!
JadeMethodListBrowser comment: ''!
!JadeMethodListBrowser categoriesForClass!Unclassified! !
!JadeMethodListBrowser methodsFor!

defaultPresenterName

	^JadeMethodListPresenter getJadeShellViewName!

presenterClass

	 "^JadeMethodListPresenter."

	"^JadeTreeMethodListPresenter."

	^JadeMethodListPresenter getJadeMethodBrowserPresenterClass!

shellName

	^'Method List Browser'.
!

statusBarText: aString

	(self view viewNamed: 'statusBarField') model: (ValueHolder with: aString).
!

switchToListView

	JadeMethodListPresenter setListView.

	self view destroy.!

switchToTreeView

	JadeMethodListPresenter setTreeView.

	self view destroy.! !
!JadeMethodListBrowser categoriesFor: #defaultPresenterName!public! !
!JadeMethodListBrowser categoriesFor: #presenterClass!overrides!private! !
!JadeMethodListBrowser categoriesFor: #shellName!overrides!private! !
!JadeMethodListBrowser categoriesFor: #statusBarText:!private! !
!JadeMethodListBrowser categoriesFor: #switchToListView!public! !
!JadeMethodListBrowser categoriesFor: #switchToTreeView!public! !

!JadeMethodListBrowser class methodsFor!

showOnSession: aGciSession

	aGciSession hasServer ifFalse: [
		MessageBox
			warning: 'Server initialization failed at login.'
			caption: 'Unable to Open Browser'.
	].
	^self new 
		gciSession: aGciSession;
		createView: JadeMethodListPresenter getJadeShellViewName;
		showShell;
		yourself.
! !
!JadeMethodListBrowser class categoriesFor: #showOnSession:!public! !

