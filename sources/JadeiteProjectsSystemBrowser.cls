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

abortTransaction
	super abortTransaction.
	self currentCard refreshFromServer!

addSystemBrowser
	self addSystemBrowserWithLayoutInfo: nil!

addSystemBrowserWithLayoutInfo: each
	| presenter |
	self rowanFixMe. "ignore layout info. Roll our own later"
	presenter := (self systemBrowserPresenter createIn: cardsPresenter gciSession: gciSession).
	presenter updateTabLabel.
	presenter ensureVisible.
	
!

commitTransaction
	super commitTransaction.
	self currentCard refreshFromServer!

layoutFilePath

	^SessionManager current imageBase , 'Rowan System Browser Layout.stb'!

shellName

	^'Projects Browser'.
!

systemBrowserPresenter
	^JadeiteProjectsBrowserPresenter! !
!JadeiteProjectsSystemBrowser categoriesFor: #abortTransaction!public! !
!JadeiteProjectsSystemBrowser categoriesFor: #addSystemBrowser!layout!public! !
!JadeiteProjectsSystemBrowser categoriesFor: #addSystemBrowserWithLayoutInfo:!layout!public! !
!JadeiteProjectsSystemBrowser categoriesFor: #commitTransaction!public! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 2521 1511 551 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 8 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409288972 131137 608 nil 466 8 4278190080 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 608 466 688 8 4294906151 170 192 8 #() 170 192 34 12 40813 1115910 ##(Smalltalk.ToolbarIconButton) 40813 nil 608 1 1180998 4 ##(Smalltalk.CommandDescription) #maximizeCode 8 'Maximize Code' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Shell.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil 395334 3 ##(Smalltalk.Bitmap) nil true nil nil nil nil 3 514 33 33 1 40815 898 40815 nil 608 1 930 #jadeBrowseTests 8 'Open SUnit Browser' 1 1 978 nil true 1024 8 'icons\UnitTestPlugin.ico' nil 1106 nil true nil nil nil nil 3 514 33 33 1 40803 853766 ##(Smalltalk.ToolbarButton) 40803 nil 608 1 930 #abortTransaction 8 'Abort Transaction' 1 1 nil 1106 nil true 1024 8 'Tools.bmp' 1072 nil 7 514 1857 33 1 40805 1266 40805 nil 608 1 930 #commitTransaction 8 'Commit Transaction' 1 1 nil 1328 27 40807 1246982 ##(Smalltalk.ToolbarSystemButton) 40807 nil 608 1 930 #addWorkspace 8 'Add Workspace' 1 1 nil 1 13 40809 1266 40809 nil 608 1 930 #addSystemBrowser 8 'Add System Browser' 1 1 nil 1328 17 34 9 1280 1376 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 608 3 nil 1 1440 1488 1554 nil nil 608 3 nil 1 912 1554 nil nil 608 3 nil 1 1152 170 176 34 8 1 117 1328 1 1232 149 1120 147 nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 -1 1 514 2331 51 608 1778 #updateSizePosted 848 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 140 4 0 0 25 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft -1 1986 #fixedParentRight -159 1986 #fixedParentTop 1 1986 #fixedViewTop 51 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 2064 nil 466 8 4278190080 nil 1031 nil nil nil 2064 655878 ##(Smalltalk.CardLayout) 138 144 848 nil 170 192 848 nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 2064 34 2 8 1140916736 1 2224 590662 2 ##(Smalltalk.ListModel) 138 144 848 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) nil nil 1 nil nil nil 2224 nil 8 4294906977 787814 3 ##(Smalltalk.BlockClosure) 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 2400 7 257 nil 2386 0 nil 2418 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 2464 7 257 nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #smallIcons nil nil nil nil nil 1714 138 144 34 2 1778 #createAt:extent: 34 2 514 1 1 514 2505 1307 2224 1778 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2224 1874 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 228 4 0 0 141 2 0 0] 8 #() 1936 nil 27 1714 138 144 34 1 1778 #createAt:extent: 34 2 514 -7 51 514 2505 1307 2064 1874 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 252 255 255 255 25 0 0 0 224 4 0 0 166 2 0 0] 34 1 2224 1936 nil 27 1954 2000 -7 2016 9 2032 51 1986 #fixedParentBottom -35 410 ##(Smalltalk.StatusBar) 34 18 nil 416 34 2 8 1409288460 1 2928 nil 466 8 4278190080 nil 7 nil 706 nil true 738 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 nil 2928 nil 8 4294903695 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 2928 nil 459270 ##(Smalltalk.Message) #displayString 8 #() 3170 #iconImageIndex 8 #() 2528 8 'statusBarField' 34 1 3152 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 2928 nil nil 1714 138 144 34 1 1778 #createAt:extent: 34 2 514 1 1349 514 2489 45 2928 1874 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 162 2 0 0 220 4 0 0 184 2 0 0] 8 #() 1936 nil 27 1954 2000 1 2016 1 2912 -43 2912 1 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409288972 131137 3488 nil 466 8 4278190080 nil 519 nil 706 nil true 738 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 3488 466 3568 8 4294906151 170 192 848 170 192 34 6 40821 1266 40821 nil 3488 1 930 #moveCardLeft 8 'Move Card Left' 1 1 nil 657990 3 ##(Smalltalk.DIBSection) nil true 1024 8 'IdeaSpaceCardBar.bmp' nil nil 7 514 161 33 17 nil 1 40823 1266 40823 nil 3488 1 930 #moveCardRight 8 'Move Card Right' 1 1 nil 3792 3 40825 1266 40825 nil 3488 1 930 #closeCard 8 'Close Card' 1 1 nil 3792 5 34 4 1554 nil nil 3488 3 nil 1 3728 3840 3888 170 176 34 2 3792 1 nil 1 nil 514 33 33 514 45 45 nil 1682 1 1 1 1714 138 144 34 2 1778 #createAt:extent: 34 2 514 2329 1 514 161 51 3488 1778 #updateSizePosted 848 3488 1874 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 4 0 0 0 0 0 0 220 4 0 0 25 0 0 0] 8 #() 1936 nil 27 1954 2016 -159 1986 #fixedViewLeft 161 2032 1 2048 51 170 192 34 8 608 8 'leftToolbar' 2064 8 'cardContainer' 2928 8 'statusBar' 3488 8 'rightToolbar' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 1 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 930 #close 8 'Exit' 1025 1 nil nil nil 8 'Jade' nil 134217729 nil nil 40801 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1714 138 144 34 3 1778 #createAt:extent: 34 2 514 7679 21 514 2521 1511 416 1778 #text: 34 1 8 'Rowan System Browser' 416 1778 #updateMenuBar 848 416 1874 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 235 19 0 0 253 2 0 0] 34 4 608 3488 2064 2928 1936 nil 27 )! !
!JadeiteProjectsSystemBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

