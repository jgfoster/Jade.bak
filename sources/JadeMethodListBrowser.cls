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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13565952 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ReferenceView) 34 14 0 416 34 2 8 1140850688 131073 608 0 466 8 4278190080 0 7 0 0 0 608 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeMethodListPresenter) #resource_Default_view 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 1169 639 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 63 1 0 0] 8 #() 514 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 994 #fixedParentRight 1 994 #fixedParentTop 1 994 #fixedParentBottom -43 410 ##(Smalltalk.StatusBar) 34 18 0 416 34 2 8 1409288460 1 1072 0 466 8 4278190080 0 7 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 0 1072 0 8 4294904165 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 1072 0 459270 ##(Smalltalk.Message) #displayString 8 #() 1346 #iconImageIndex 8 #() 1049926 1 ##(Smalltalk.IconImageManager) 8 'statusBarField' 34 1 1328 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 1072 0 0 738 138 144 34 1 802 #createAt:extent: 34 2 514 1 639 514 1169 45 1072 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 63 1 0 0 72 2 0 0 85 1 0 0] 8 #() 944 0 27 962 1008 1 1024 1 1056 -43 994 #fixedViewTop 45 170 192 34 2 1072 8 'statusBar' 0 461638 4 ##(Smalltalk.MenuBar) 0 true 34 1 265030 4 ##(Smalltalk.Menu) 0 true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #switchToTreeView 8 'Switch' 1 1 0 0 0 8 'Switch to Tree View' 0 134217729 0 0 11255 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 738 138 144 34 3 802 #createAt:extent: 34 2 514 2719 21 514 1201 801 416 802 #text: 34 1 8 'Jade Method List Browser' 416 802 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 5 0 0 10 0 0 0 102 7 0 0 154 1 0 0] 34 2 608 1072 944 0 27 )!

resource_Tree_view
	"Answer the literal data from which the 'Tree view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Tree_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13565952 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ReferenceView) 34 14 0 416 34 2 8 1140850688 131073 608 0 466 8 4278190080 0 7 0 0 0 608 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeTreeMethodListPresenter) #resource_Default_view 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 1169 639 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 63 1 0 0] 8 #() 514 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 994 #fixedParentRight 1 994 #fixedParentTop 1 994 #fixedParentBottom -43 410 ##(Smalltalk.StatusBar) 34 18 0 416 34 2 8 1409288460 1 1072 0 466 8 4278190080 0 7 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 0 1072 0 8 4294904165 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 1072 0 459270 ##(Smalltalk.Message) #displayString 8 #() 1346 #iconImageIndex 8 #() 1049926 1 ##(Smalltalk.IconImageManager) 8 'statusBarField' 34 1 1328 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 1072 0 0 738 138 144 34 1 802 #createAt:extent: 34 2 514 1 639 514 1169 45 1072 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 63 1 0 0 72 2 0 0 85 1 0 0] 8 #() 944 0 27 962 1008 1 1024 1 1056 -43 994 #fixedViewTop 45 170 192 34 2 1072 8 'statusBar' 0 461638 4 ##(Smalltalk.MenuBar) 0 true 34 1 265030 4 ##(Smalltalk.Menu) 0 true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #switchToListView 8 'Switch' 1 1 0 0 0 8 'Switch to List View' 0 134217729 0 0 11859 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 738 138 144 34 3 802 #createAt:extent: 34 2 514 2719 21 514 1201 801 416 802 #text: 34 1 8 'Jade Method List Browser' 416 802 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 5 0 0 10 0 0 0 102 7 0 0 154 1 0 0] 34 2 608 1072 944 0 27 )!

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
!JadeMethodListBrowser class categoriesFor: #resource_List_view!public! !
!JadeMethodListBrowser class categoriesFor: #resource_Tree_view!public! !
!JadeMethodListBrowser class categoriesFor: #showOnSession:!public! !

