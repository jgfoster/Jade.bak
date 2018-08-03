"Filed out from Dolphin Smalltalk 7"!

JadeSystemBrowser subclass: #RowanSystemBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanSystemBrowser guid: (GUID fromString: '{dbac925c-5c0f-4800-b343-1784dfb07a6e}')!
RowanSystemBrowser comment: ''!
!RowanSystemBrowser categoriesForClass!Unclassified! !
!RowanSystemBrowser methodsFor!

addSystemBrowserWithLayoutInfo: each
	| presenter |
	presenter := (self systemBrowserPresenter createIn: cardsPresenter gciSession: gciSession)
				layoutInfo: each.
	presenter projectDictionaryChanged!

restoreLayoutAndContentsFromFile
	self rowanFixMe. "Need to restore from saved state via services"
	^false. !

shellName

	^'Rowan System Browser'.
!

systemBrowserPresenter	
	^RowanSystemBrowserPresenter! !
!RowanSystemBrowser categoriesFor: #addSystemBrowserWithLayoutInfo:!public! !
!RowanSystemBrowser categoriesFor: #restoreLayoutAndContentsFromFile!public! !
!RowanSystemBrowser categoriesFor: #shellName!overrides!private! !
!RowanSystemBrowser categoriesFor: #systemBrowserPresenter!public! !

!RowanSystemBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1801 1201 551 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 8 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409288972 131137 608 nil 466 8 4278190080 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 608 466 688 8 4294902419 170 192 8 #() 170 192 34 18 39331 1115910 ##(Smalltalk.ToolbarIconButton) 39331 nil 608 17 1180998 4 ##(Smalltalk.CommandDescription) #historyBack 8 'History Back' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'HistoryBack.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil 395334 3 ##(Smalltalk.Bitmap) nil true nil nil nil nil 3 514 33 33 1 39333 898 39333 nil 608 17 930 #historyForward 8 'History Forward' 1 1 978 nil true 1024 8 'HistoryForward.ico' 1072 1106 nil true nil nil nil nil 3 514 33 33 1 39317 853766 ##(Smalltalk.ToolbarButton) 39317 nil 608 1 930 #abortTransaction 8 'Abort Transaction' 1 1 nil 1106 nil true 1024 8 'Tools.bmp' 1072 nil 7 514 1857 33 1 39319 1266 39319 nil 608 1 930 #commitTransaction 8 'Commit Transaction' 1 1 nil 1328 27 39321 1246982 ##(Smalltalk.ToolbarSystemButton) 39321 nil 608 1 930 #addWorkspace 8 'Add Workspace' 1 1 nil 1 13 39323 1266 39323 nil 608 1 930 #addSystemBrowser 8 'Add System Browser' 1 1 nil 1328 17 39325 1266 39325 nil 608 1 930 #browseUsers 8 'Browse Users' 1 1 nil 1328 75 39327 898 39327 nil 608 1 930 #maximizeCode 8 'Maximize Code' 1 1 978 nil true 1024 8 'Shell.ico' 1072 1106 nil true nil nil nil nil 3 514 33 33 1 39329 898 39329 nil 608 1 930 #jadeBrowseTests 8 'Open SUnit Browser' 1 1 978 nil true 1024 8 'icons\UnitTestPlugin.ico' nil 1106 nil true nil nil nil nil 3 514 33 33 1 34 13 1280 1376 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 608 3 nil 1 1440 1488 1536 1826 nil nil 608 3 nil 1 1584 1826 nil nil 608 3 nil 1 1696 1826 nil nil 608 3 nil 1 912 1152 170 176 34 12 1 117 1328 1 1776 149 1664 147 1120 151 1232 153 nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 -1 1 514 2001 51 608 2066 #updateSizePosted 848 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 231 3 0 0 25 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft -1 2274 #fixedParentRight -159 2274 #fixedParentTop 1 2274 #fixedViewTop 51 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409288972 131137 2352 nil 466 8 4278190080 nil 519 nil 706 nil true 738 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 2352 466 2432 8 4294902419 170 192 848 170 192 34 6 39335 1266 39335 nil 2352 1 930 #moveCardLeft 8 'Move Card Left' 1 1 nil 657990 3 ##(Smalltalk.DIBSection) nil true 1024 8 'IdeaSpaceCardBar.bmp' nil nil 7 514 161 33 17 nil 1 39337 1266 39337 nil 2352 1 930 #moveCardRight 8 'Move Card Right' 1 1 nil 2656 3 39339 1266 39339 nil 2352 1 930 #closeCard 8 'Close Card' 1 1 nil 2656 5 34 4 1826 nil nil 2352 3 nil 1 2592 2704 2752 170 176 34 2 2656 1 nil 1 nil 514 33 33 514 45 45 nil 1970 1 1 1 2002 138 144 34 2 2066 #createAt:extent: 34 2 514 1999 1 514 161 51 2352 2066 #updateSizePosted 848 2352 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 231 3 0 0 0 0 0 0 55 4 0 0 25 0 0 0] 8 #() 2224 nil 27 2242 2304 -159 2274 #fixedViewLeft 161 2320 1 2336 51 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 3120 nil 466 8 4278190080 nil 1031 nil nil nil 3120 655878 ##(Smalltalk.CardLayout) 138 144 848 nil 170 192 848 nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 3120 34 2 8 1140916736 1 3280 590662 2 ##(Smalltalk.ListModel) 138 144 848 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) nil nil 1 nil nil nil 3280 nil 8 4294906647 787814 3 ##(Smalltalk.BlockClosure) 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 3456 7 257 nil 3442 0 nil 3474 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 3520 7 257 nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #smallIcons nil nil nil nil nil 2002 138 144 34 2 2066 #createAt:extent: 34 2 514 1 1 514 2175 1307 3280 2066 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 3280 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 63 4 0 0 141 2 0 0] 8 #() 2224 nil 27 2002 138 144 34 1 2066 #createAt:extent: 34 2 514 -7 51 514 2175 1307 3120 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 252 255 255 255 25 0 0 0 59 4 0 0 166 2 0 0] 34 1 3280 2224 nil 27 2242 2288 -7 2304 9 2320 51 2274 #fixedParentBottom -35 410 ##(Smalltalk.StatusBar) 34 18 nil 416 34 2 8 1409288460 1 3984 nil 466 8 4278190080 nil 7 nil 706 nil true 738 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 nil 3984 nil 8 4294902865 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 3984 nil 459270 ##(Smalltalk.Message) #displayString 8 #() 4226 #iconImageIndex 8 #() 3584 8 'statusBarField' 34 1 4208 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 3984 nil nil 2002 138 144 34 1 2066 #createAt:extent: 34 2 514 1 1349 514 2159 45 3984 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 162 2 0 0 55 4 0 0 184 2 0 0] 8 #() 2224 nil 27 2242 2288 1 2304 1 3968 -43 3968 1 170 192 34 8 608 8 'leftToolbar' 2352 8 'rightToolbar' 3120 8 'cardContainer' 3984 8 'statusBar' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 1 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 930 #close 8 'Exit' 1025 1 nil nil nil 8 'Jade' nil 134217729 nil nil 39315 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 2002 138 144 34 3 2066 #createAt:extent: 34 2 514 7679 21 514 2191 1511 416 2066 #text: 34 1 8 'Rowan System Browser' 416 2066 #updateMenuBar 848 416 2162 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 70 19 0 0 253 2 0 0] 34 4 608 2352 3120 3984 2224 nil 27 )! !
!RowanSystemBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

