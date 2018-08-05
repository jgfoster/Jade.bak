"Filed out from Dolphin Smalltalk 7"!

RowanSystemBrowser subclass: #JadeiteProjectsSystemBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteProjectsSystemBrowser guid: (GUID fromString: '{8b442830-2c48-4a43-aff6-375f4d49530d}')!
JadeiteProjectsSystemBrowser comment: ''!
!JadeiteProjectsSystemBrowser categoriesForClass!Unclassified! !
!JadeiteProjectsSystemBrowser methodsFor!

addSystemBrowser
	self addSystemBrowserWithLayoutInfo: nil!

addSystemBrowserWithLayoutInfo: each
	| presenter |
	self rowanFixMe. "ignore layout info. Roll our own later"
	presenter := (self systemBrowserPresenter createIn: cardsPresenter gciSession: gciSession).
	presenter updateTabLabel.
	presenter ensureVisible.
	
!

layoutFilePath

	^SessionManager current imageBase , 'Rowan System Browser Layout.stb'!

shellName

	^'Jadeite Projects Browser'.
!

systemBrowserPresenter
	^JadeiteProjectsBrowserPresenter! !
!JadeiteProjectsSystemBrowser categoriesFor: #addSystemBrowser!layout!public! !
!JadeiteProjectsSystemBrowser categoriesFor: #addSystemBrowserWithLayoutInfo:!layout!public! !
!JadeiteProjectsSystemBrowser categoriesFor: #layoutFilePath!layout!public! !
!JadeiteProjectsSystemBrowser categoriesFor: #shellName!overrides!private! !
!JadeiteProjectsSystemBrowser categoriesFor: #systemBrowserPresenter!public! !

!JadeiteProjectsSystemBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 2521 1511 551 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 8 410 ##(Smalltalk.StatusBar) 34 18 nil 416 34 2 8 1409288460 1 608 nil 466 8 4278190080 nil 7 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 nil 608 nil 8 4294902865 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 608 nil 459270 ##(Smalltalk.Message) #displayString 8 #() 882 #iconImageIndex 8 #() 1049926 1 ##(Smalltalk.IconImageManager) 8 'statusBarField' 34 1 864 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 608 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1349 514 2489 45 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 162 2 0 0 220 4 0 0 184 2 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 1314 #fixedParentRight 1 1314 #fixedParentBottom -43 1360 1 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409288972 131137 1376 nil 466 8 4278190080 nil 519 nil 706 nil true 738 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 1376 466 1456 8 4294902419 170 192 8 #() 170 192 34 18 40077 853766 ##(Smalltalk.ToolbarButton) 40077 nil 1376 1 1180998 4 ##(Smalltalk.CommandDescription) #abortTransaction 8 'Abort Transaction' 1 1 nil 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 514 1857 33 1 40079 1634 40079 nil 1376 1 1666 #commitTransaction 8 'Commit Transaction' 1 1 nil 1728 27 40081 1246982 ##(Smalltalk.ToolbarSystemButton) 40081 nil 1376 1 1666 #addWorkspace 8 'Add Workspace' 1 1 nil 1 13 40083 1634 40083 nil 1376 1 1666 #addSystemBrowser 8 'Add System Browser' 1 1 nil 1728 17 40085 1634 40085 nil 1376 1 1666 #browseUsers 8 'Browse Users' 1 1 nil 1728 75 40087 1115910 ##(Smalltalk.ToolbarIconButton) 40087 nil 1376 1 1666 #maximizeCode 8 'Maximize Code' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1760 8 'Shell.ico' 1808 1714 nil true nil nil nil nil 3 514 33 33 1 40089 2066 40089 nil 1376 1 1666 #jadeBrowseTests 8 'Open SUnit Browser' 1 1 2130 nil true 1760 8 'icons\UnitTestPlugin.ico' nil 1714 nil true nil nil nil nil 3 514 33 33 1 40091 2066 40091 nil 1376 17 1666 #historyBack 8 'History Back' 1 1 2130 nil true 1760 8 'HistoryBack.ico' 1808 1714 nil true nil nil nil nil 3 514 33 33 1 40093 2066 40093 nil 1376 17 1666 #historyForward 8 'History Forward' 1 1 2130 nil true 1760 8 'HistoryForward.ico' 1808 1714 nil true nil nil nil nil 3 514 33 33 1 34 13 1648 1856 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 1376 3 nil 1 1920 1968 2016 2562 nil nil 1376 3 nil 1 2080 2562 nil nil 1376 3 nil 1 2208 2562 nil nil 1376 3 nil 1 2320 2432 170 176 34 12 1 117 1728 1 2288 149 2176 147 2400 151 2512 153 nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 1058 138 144 34 2 1122 #createAt:extent: 34 2 514 -1 1 514 2331 51 1376 1122 #updateSizePosted 1584 1376 1202 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 140 4 0 0 25 0 0 0] 8 #() 1264 nil 27 1282 1328 -1 1344 -159 1314 #fixedParentTop 1 1314 #fixedViewTop 51 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 2960 nil 466 8 4278190080 nil 1031 nil nil nil 2960 655878 ##(Smalltalk.CardLayout) 138 144 1584 nil 170 192 1584 nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 2960 34 2 8 1140916736 1 3120 590662 2 ##(Smalltalk.ListModel) 138 144 1584 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) nil nil 1 nil nil nil 3120 nil 8 4294906647 787814 3 ##(Smalltalk.BlockClosure) 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 3296 7 257 nil 3282 0 nil 3314 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 3360 7 257 nil 976 nil nil nil nil nil #smallIcons nil nil nil nil nil 1058 138 144 34 2 1122 #createAt:extent: 34 2 514 1 1 514 2505 1307 3120 1122 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 3120 1202 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 228 4 0 0 141 2 0 0] 8 #() 1264 nil 27 1058 138 144 34 1 1122 #createAt:extent: 34 2 514 -7 51 514 2505 1307 2960 1202 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 252 255 255 255 25 0 0 0 224 4 0 0 166 2 0 0] 34 1 3120 1264 nil 27 1282 1328 -7 1344 9 2928 51 1360 -35 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409288972 131137 3776 nil 466 8 4278190080 nil 519 nil 706 nil true 738 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 3776 466 3856 8 4294902419 170 192 1584 170 192 34 6 40095 1634 40095 nil 3776 1 1666 #moveCardLeft 8 'Move Card Left' 1 1 nil 657990 3 ##(Smalltalk.DIBSection) nil true 1760 8 'IdeaSpaceCardBar.bmp' nil nil 7 514 161 33 17 nil 1 40097 1634 40097 nil 3776 1 1666 #moveCardRight 8 'Move Card Right' 1 1 nil 4080 3 40099 1634 40099 nil 3776 1 1666 #closeCard 8 'Close Card' 1 1 nil 4080 5 34 4 2562 nil nil 3776 3 nil 1 4016 4128 4176 170 176 34 2 4080 1 nil 1 nil 514 33 33 514 45 45 nil 2706 1 1 1 1058 138 144 34 2 1122 #createAt:extent: 34 2 514 2329 1 514 161 51 3776 1122 #updateSizePosted 1584 3776 1202 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 4 0 0 0 0 0 0 220 4 0 0 25 0 0 0] 8 #() 1264 nil 27 1282 1344 -159 1314 #fixedViewLeft 161 2928 1 2944 51 170 192 34 8 608 8 'statusBar' 1376 8 'leftToolbar' 2960 8 'cardContainer' 3776 8 'rightToolbar' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 1 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1666 #close 8 'Exit' 1025 1 nil nil nil 8 'Jade' nil 134217729 nil nil 40075 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1058 138 144 34 3 1122 #createAt:extent: 34 2 514 7679 21 514 2521 1511 416 1122 #text: 34 1 8 'Rowan System Browser' 416 1122 #updateMenuBar 1584 416 1202 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 235 19 0 0 253 2 0 0] 34 4 1376 3776 2960 608 1264 nil 27 )! !
!JadeiteProjectsSystemBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

