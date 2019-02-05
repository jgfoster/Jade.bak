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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil 328198 ##(Smalltalk.Point) 521 51 5 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 2 410 ##(Smalltalk.ToolbarA) 34 25 nil 416 34 2 8 1140851500 131137 576 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 517 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 482 193 193 nil 576 656 8 1949831345 nil 170 192 34 20 28199 1115206 1 ##(Smalltalk.ToolbarIconButton) 576 1 1180998 4 ##(Smalltalk.CommandDescription) #fileNew 8 'New Workspace' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'NewWorkspace.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil 28199 27011 834 576 1 866 #abort 8 'Abort Transaction' 1 1 914 nil true 960 8 'Panic.ico' 1008 27011 32767 834 576 1 866 #browseTests 8 'Open Test Browser' 1 1 914 nil true 960 8 'TestRunner.ico' 1008 32767 28491 834 576 1 866 #fileOpen 8 'Open Workspace' 1 1 914 nil true 960 8 'OpenWorkspace.ico' 1008 28491 31949 834 576 1 866 #browseClasses 8 'Open System Browser' 1 1 914 nil true 960 8 'SystemBrowserShell.ico' 1008 31949 27625 834 576 1 866 #commit 8 'Commit Transaction' 1 1 914 nil true 960 8 'Snapshot.ico' 1008 27625 26991 1246790 1 ##(Smalltalk.ToolbarSystemButton) 576 1 866 #jadePreferences 8 'Jade Preferences' 1 1 nil 26991 1 21 26993 834 576 1 866 #profStef 8 'Open ProfStef Smalltalk Tutorial' 1 1 788806 1 ##(Smalltalk.TextTileIcon) $\x1F393 nil 196934 1 ##(Smalltalk.RGB) 18393531 786694 ##(Smalltalk.IndexedColor) 33554471 nil nil 1 nil nil 26993 32609 834 576 1 866 #browseMonticello 8 'Open Monticello Browser' 1 1 914 nil true 960 8 'SourceBrowser.ico' 1008 32609 31459 834 576 1 866 #browseUsers 8 'Browse Users' 1 1 914 nil true 960 8 'MethodProtocol.ico' 1008 31459 34 13 1040 1360 1049158 1 ##(Smalltalk.ToolbarSeparator) 576 1 848 1200 1826 576 1 1728 1280 1648 1120 1456 1826 576 1 1504 nil nil 1 nil 482 33 33 482 45 45 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 482 1 1 482 521 51 576 1986 #updateSizePosted 8 #() 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 4 1 0 0 25 0 0 0] 8 #() 482 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2210 #fixedParentRight 1 2210 #fixedParentTop 1 2210 #fixedParentBottom 1 170 192 2080 nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 482 2201 21 482 521 51 416 2098 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 76 4 0 0 10 0 0 0 80 5 0 0 35 0 0 0] 34 1 576 2160 nil 27 )! !
!JadeToolsToolBarPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

