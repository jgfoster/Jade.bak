"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadeMethodListBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMethodListBrowser guid: (GUID fromString: '{046CEBD5-F142-4382-99FF-B3FD8B8032B9}')!
JadeMethodListBrowser comment: ''!
!JadeMethodListBrowser categoriesForClass!Unclassified! !
!JadeMethodListBrowser methodsFor!

browseImplementorsOf: aGsMethod

	myPresenter browseImplementorsOf: aGsMethod.
!

browseMethodsContaining: aString

	myPresenter browseMethodsContaining: aString.
!

browseMethodsFromString: aString

	myPresenter browseMethodsFromString: aString.
!

browseSendersOf: aGsMethodOrString

	myPresenter browseSendersOf: aGsMethodOrString.
!

presenterClass

	^JadeMethodListPresenter.
!

selecting: aString

	myPresenter selecting: aString.
!

shellName

	^'Method List Browser'.
!

statusBarText: aString

	(self view viewNamed: 'statusBarField') model: (ValueHolder with: aString).
! !
!JadeMethodListBrowser categoriesFor: #browseImplementorsOf:!private! !
!JadeMethodListBrowser categoriesFor: #browseMethodsContaining:!private! !
!JadeMethodListBrowser categoriesFor: #browseMethodsFromString:!private! !
!JadeMethodListBrowser categoriesFor: #browseSendersOf:!private! !
!JadeMethodListBrowser categoriesFor: #presenterClass!overrides!private! !
!JadeMethodListBrowser categoriesFor: #selecting:!private! !
!JadeMethodListBrowser categoriesFor: #shellName!overrides!private! !
!JadeMethodListBrowser categoriesFor: #statusBarText:!private! !

!JadeMethodListBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.StatusBar)  98 18 0 416 98 2 8 1409288460 1 624 0 482 8 4278190080 0 7 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 530 193 193 0 624 0 8 4294902483 234 256 98 2 853766 ##(Smalltalk.StatusBarItem)  1 -1 624 0 459270 ##(Smalltalk.Message)  8 #displayString 98 0 914 8 #iconImageIndex 98 0 1049670 1 ##(Smalltalk.IconImageManager)  8 'statusBarField' 98 1 896 1115142 ##(Smalltalk.StatusBarNullItem)  513 1 624 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 689 530 1185 45 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 88 1 0 0 80 2 0 0 110 1 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 1394 8 #fixedParentRight 1 1394 8 #fixedParentBottom -43 1394 8 #fixedViewTop 45 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 1536 0 482 8 4278190080 0 7 0 0 0 1536 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.JadeMethodListPresenter)  8 #resource_Default_view 0 1122 202 208 98 1 1186 1216 98 2 530 1 1 530 1185 689 1536 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 80 2 0 0 88 1 0 0] 98 0 1344 0 27 1362 1408 1 1440 1 1394 8 #fixedParentTop 1 1472 -43 234 256 98 2 624 8 'statusBar' 0 0 0 0 0 1 0 0 0 0 1 0 0 1122 202 208 98 3 1186 1216 98 2 530 2879 21 530 1201 801 416 1186 8 #text: 98 1 8 'Jade Method List Browser' 416 1186 8 #updateMenuBar 1856 416 1282 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 98 2 1536 624 1344 0 27 )! !
!JadeMethodListBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

