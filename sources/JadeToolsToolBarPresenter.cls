"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #JadeToolsToolBarPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeToolsToolBarPresenter guid: (GUID fromString: '{7109cdbe-8fa3-4a9d-a3d2-0c1295a3256e}')!
JadeToolsToolBarPresenter comment: ''!
!JadeToolsToolBarPresenter categoriesForClass!Unclassified! !
!JadeToolsToolBarPresenter methodsFor!

abort

	gciSession abort.
	Sound informationBeep.
!

commit

	gciSession commit ifTrue: [
		Sound informationBeep.
	] ifFalse: [
		MessageBox warning: 'Commit failed!!'.
	].
!

fileNew

	JadeWorkspace showOnSession: gciSession.
!

fileOpen

	JadeWorkspace openOnSession: gciSession.
! !
!JadeToolsToolBarPresenter categoriesFor: #abort!Jade!public! !
!JadeToolsToolBarPresenter categoriesFor: #commit!public! !
!JadeToolsToolBarPresenter categoriesFor: #fileNew!public! !
!JadeToolsToolBarPresenter categoriesFor: #fileOpen!public! !

!JadeToolsToolBarPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 0 0 34 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 2 410 ##(Smalltalk.ToolbarA) 34 25 0 416 34 2 8 1140851500 131137 544 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 517 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 544 610 8 4278190080 8 4294905921 0 170 192 34 16 11845 853766 ##(Smalltalk.ToolbarButton) 11845 0 544 1 1180998 4 ##(Smalltalk.CommandDescription) #abort 8 'Abort Transaction' 1 1 0 395334 3 ##(Smalltalk.Bitmap) 0 true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 0 7 738 1857 33 1 11847 850 11847 0 544 1 882 #commit 8 'Commit Transaction' 1 1 0 944 27 11849 1246982 ##(Smalltalk.ToolbarSystemButton) 11849 0 544 1 882 #fileNew 8 'New Workspace' 1 1 0 1 13 11851 1122 11851 0 544 1 882 #fileOpen 8 'Open Workspace' 1 1 0 1 15 11853 850 11853 0 544 1 882 #browseUsers 8 'Browse Users' 1 1 0 944 75 11855 850 11855 0 544 1 882 #browseClasses 8 'Open System Browser' 1 1 0 944 17 11857 850 11857 0 544 1 882 #browseMonticello 8 'Open Monticello Browser' 1 1 0 944 3 11859 1115910 ##(Smalltalk.ToolbarIconButton) 11859 0 544 1 882 #browseTests 8 'Open Test Browser' 1 1 263494 3 ##(Smalltalk.Icon) 0 true 976 8 'icons\UnitTestPlugin.ico' 0 930 0 true 0 0 0 0 3 738 33 33 1 34 10 864 1072 1050118 ##(Smalltalk.ToolbarSeparator) 0 0 544 3 0 1 1136 1184 1538 0 0 544 3 0 1 1232 1280 1328 1392 170 176 34 6 1 117 944 1 1488 147 0 1 0 738 33 33 738 45 45 0 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 738 1 1 738 441 51 544 1714 #updateSizePosted 8 #() 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 25 0 0 0] 8 #() 738 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 1938 #fixedParentRight 1 1938 #fixedParentTop 1 1938 #fixedParentBottom 1 170 192 1808 0 1650 138 144 34 1 1714 #createAt:extent: 34 2 738 5119 21 738 441 51 416 1826 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 219 10 0 0 35 0 0 0] 34 1 544 1888 0 27 )! !
!JadeToolsToolBarPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

