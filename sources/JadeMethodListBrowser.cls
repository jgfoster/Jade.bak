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

	^JadeMethodListPresenter getJadeShellViewName
!

presenterClass

	 "^JadeMethodListPresenter."

	"^JadeTreeMethodListPresenter."

	^JadeMethodListPresenter getJadeMethodBrowserPresenterClass
!

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

	self view destroy.
! !
!JadeMethodListBrowser categoriesFor: #defaultPresenterName!public! !
!JadeMethodListBrowser categoriesFor: #presenterClass!overrides!private! !
!JadeMethodListBrowser categoriesFor: #shellName!overrides!private! !
!JadeMethodListBrowser categoriesFor: #statusBarText:!private! !
!JadeMethodListBrowser categoriesFor: #switchToListView!public! !
!JadeMethodListBrowser categoriesFor: #switchToTreeView!public! !

!JadeMethodListBrowser class methodsFor!

resource_List_view
	"Answer the literal data from which the 'List view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_List_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 549 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.StatusBar) 34 18 nil 416 34 2 8 1409288460 1 608 nil 480 nil 7 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 nil 608 nil 8 1921162360 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 608 nil 459270 ##(Smalltalk.Message) #displayString 8 #() 850 #iconImageIndex 8 #() 1049926 1 ##(Smalltalk.IconImageManager) 8 'statusBarField' 34 1 832 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 608 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 679 514 1169 45 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 83 1 0 0 72 2 0 0 105 1 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 1282 #fixedParentRight 1 1282 #fixedParentBottom -43 1282 #fixedViewTop 45 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 1360 nil 480 nil 7 nil nil nil 1360 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeMethodListPresenter) #resource_Default_view nil 1026 138 144 34 1 1090 #createAt:extent: 34 2 514 1 1 514 1169 679 1360 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 83 1 0 0] 8 #() 1232 nil 27 1250 1296 1 1312 1 1282 #fixedParentTop 1 1328 -43 170 192 34 2 608 8 'statusBar' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1026 138 144 34 3 1090 #createAt:extent: 34 2 514 3059 21 514 1201 801 416 1090 #text: 34 1 8 'Jade Method List Browser' 416 1090 #updateMenuBar 1600 416 1170 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 5 0 0 10 0 0 0 81 8 0 0 154 1 0 0] 34 2 1360 608 1232 nil 27 )!

resource_Tree_view
	"Answer the literal data from which the 'Tree view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Tree_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 549 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.StatusBar) 34 18 nil 416 34 2 8 1409288460 1 608 nil 480 nil 7 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 nil 608 nil 8 1921162360 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 608 nil 459270 ##(Smalltalk.Message) #displayString 8 #() 850 #iconImageIndex 8 #() 1049926 1 ##(Smalltalk.IconImageManager) 8 'statusBarField' 34 1 832 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 608 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 679 514 1169 45 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 83 1 0 0 72 2 0 0 105 1 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 1282 #fixedParentRight 1 1282 #fixedParentBottom -43 1282 #fixedViewTop 45 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 1360 nil 480 nil 7 nil nil nil 1360 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeTreeMethodListPresenter) #resource_Default_view nil 1026 138 144 34 1 1090 #createAt:extent: 34 2 514 1 1 514 1169 679 1360 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 83 1 0 0] 8 #() 1232 nil 27 1250 1296 1 1312 1 1282 #fixedParentTop 1 1328 -43 170 192 34 2 608 8 'statusBar' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1026 138 144 34 3 1090 #createAt:extent: 34 2 514 3059 21 514 1201 801 416 1090 #text: 34 1 8 'Jade Method List Browser' 416 1090 #updateMenuBar 1600 416 1170 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 5 0 0 10 0 0 0 81 8 0 0 154 1 0 0] 34 2 1360 608 1232 nil 27 )!

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
!JadeMethodListBrowser class categoriesFor: #resource_List_view!public!resources-views! !
!JadeMethodListBrowser class categoriesFor: #resource_Tree_view!public!resources-views! !
!JadeMethodListBrowser class categoriesFor: #showOnSession:!public! !

