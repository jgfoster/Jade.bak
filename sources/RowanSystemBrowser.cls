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

restoreLayoutAndContentsFromFile
	"corrupted stb files can give unnecessary walkbacks. Just return false 
	and let the default browser open"

	[^super restoreLayoutAndContentsFromFile] on: Error do: [:ex | ^false]!

shellName

	^'Rowan System Browser'.
!

systemBrowserPresenter
	^RowanSystemBrowserPresenter! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1801 1201 551 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 8 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 608 nil 466 8 4278190080 nil 1031 nil nil nil 608 655878 ##(Smalltalk.CardLayout) 138 144 8 #() nil 170 192 752 nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 608 34 2 8 1140916736 1 784 590662 2 ##(Smalltalk.ListModel) 138 144 752 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) nil nil 1 nil nil nil 784 nil 8 4294902785 787814 3 ##(Smalltalk.BlockClosure) 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 960 7 257 nil 946 0 nil 978 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 1024 7 257 nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #smallIcons nil nil nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 1785 997 784 1170 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 784 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 242 1 0 0] 8 #() 514 193 193 nil 27 1106 138 144 34 1 1170 #createAt:extent: 34 2 514 -7 51 514 1785 997 608 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 252 255 255 255 25 0 0 0 120 3 0 0 11 2 0 0] 34 1 784 1344 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft -7 1554 #fixedParentRight 9 1554 #fixedParentTop 51 1554 #fixedParentBottom -35 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409288972 131137 1632 nil 466 8 4278190080 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 1632 466 1712 8 4294902623 170 192 752 170 192 34 18 41999 853766 ##(Smalltalk.ToolbarButton) 41999 nil 1632 1 1180998 4 ##(Smalltalk.CommandDescription) #commitTransaction 8 'Commit Transaction' 1 1 nil 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 514 1857 33 27 42001 1246982 ##(Smalltalk.ToolbarSystemButton) 42001 nil 1632 1 1938 #addWorkspace 8 'Add Workspace' 1 1 nil 1 13 42003 1906 42003 nil 1632 1 1938 #addSystemBrowser 8 'Add System Browser' 1 1 nil 2000 17 42005 1906 42005 nil 1632 1 1938 #browseUsers 8 'Browse Users' 1 1 nil 2000 75 42007 1115910 ##(Smalltalk.ToolbarIconButton) 42007 nil 1632 1 1938 #maximizeCode 8 'Maximize Code' 1 1 263494 3 ##(Smalltalk.Icon) nil true 2032 8 'Shell.ico' 2080 1986 nil true nil nil nil nil 3 514 33 33 1 42009 2290 42009 nil 1632 1 1938 #jadeBrowseTests 8 'Open SUnit Browser' 1 1 2354 nil true 2032 8 'icons\UnitTestPlugin.ico' nil 1986 nil true nil nil nil nil 3 514 33 33 1 42011 2290 42011 nil 1632 17 1938 #historyBack 8 'History Back' 1 1 2354 nil true 2032 8 'HistoryBack.ico' 2080 1986 nil true nil nil nil nil 3 514 33 33 1 42013 2290 42013 nil 1632 17 1938 #historyForward 8 'History Forward' 1 1 2354 nil true 2032 8 'HistoryForward.ico' 2080 1986 nil true nil nil nil nil 3 514 33 33 1 41997 1906 41997 nil 1632 1 1938 #abortTransaction 8 'Abort Transaction' 1 1 nil 2000 1 34 13 2768 1920 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 1632 3 nil 1 2144 2192 2240 2834 nil nil 1632 3 nil 1 2304 2834 nil nil 1632 3 nil 1 2432 2834 nil nil 1632 3 nil 1 2544 2656 170 176 34 12 1 117 2000 1 2512 149 2400 147 2624 151 2736 153 nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 1106 138 144 34 2 1170 #createAt:extent: 34 2 514 -1 1 514 1611 51 1632 1170 #updateSizePosted 752 1632 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 36 3 0 0 25 0 0 0] 8 #() 1344 nil 27 1522 1568 -1 1584 -159 1600 1 1554 #fixedViewTop 51 410 ##(Smalltalk.StatusBar) 34 18 nil 416 34 2 8 1409288460 1 3216 nil 466 8 4278190080 nil 7 nil 1730 nil true 1762 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 nil 3216 nil 8 4294902993 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 3216 nil 459270 ##(Smalltalk.Message) #displayString 8 #() 3458 #iconImageIndex 8 #() 1088 8 'statusBarField' 34 1 3440 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 3216 nil nil 1106 138 144 34 1 1170 #createAt:extent: 34 2 514 1 1039 514 1769 45 3216 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 7 2 0 0 116 3 0 0 29 2 0 0] 8 #() 1344 nil 27 1522 1568 1 1584 1 1616 -43 1616 1 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409288972 131137 3776 nil 466 8 4278190080 nil 519 nil 1730 nil true 1762 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 3776 466 3856 8 4294902623 170 192 752 170 192 34 6 42015 1906 42015 nil 3776 1 1938 #moveCardLeft 8 'Move Card Left' 1 1 nil 657990 3 ##(Smalltalk.DIBSection) nil true 2032 8 'IdeaSpaceCardBar.bmp' nil nil 7 514 161 33 17 nil 1 42017 1906 42017 nil 3776 1 1938 #moveCardRight 8 'Move Card Right' 1 1 nil 4080 3 42019 1906 42019 nil 3776 1 1938 #closeCard 8 'Close Card' 1 1 nil 4080 5 34 4 2834 nil nil 3776 3 nil 1 4016 4128 4176 170 176 34 2 4080 1 nil 1 nil 514 33 33 514 45 45 nil 2978 1 1 1 1106 138 144 34 2 1170 #createAt:extent: 34 2 514 1609 1 514 161 51 3776 1170 #updateSizePosted 752 3776 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 36 3 0 0 0 0 0 0 116 3 0 0 25 0 0 0] 8 #() 1344 nil 27 1522 1584 -159 1554 #fixedViewLeft 161 1600 1 3200 51 170 192 34 8 608 8 'cardContainer' 1632 8 'leftToolbar' 3216 8 'statusBar' 3776 8 'rightToolbar' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 1 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1938 #close 8 'Exit' 1025 1 nil nil nil 8 'Jade' nil 134217729 nil nil 41995 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1106 138 144 34 3 1170 #createAt:extent: 34 2 514 3839 21 514 1801 1201 416 1170 #text: 34 1 8 'Rowan System Browser' 416 1170 #updateMenuBar 752 416 1282 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 3 11 0 0 98 2 0 0] 34 4 1632 3776 608 3216 1344 nil 27 )! !
!RowanSystemBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

