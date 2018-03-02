"Filed out from Dolphin Smalltalk 7"!

JadeMethodListBrowser subclass: #RowanMethodListBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodListBrowser guid: (GUID fromString: '{68b0fe86-7704-4d47-bfa2-976beaad877a}')!
RowanMethodListBrowser comment: ''!
!RowanMethodListBrowser categoriesForClass!Unclassified! !
!RowanMethodListBrowser methodsFor!

presenterClass

	^RowanMethodListPresenter!

shellName

	^'Rowan Method List Browser'.
! !
!RowanMethodListBrowser categoriesFor: #presenterClass!private! !
!RowanMethodListBrowser categoriesFor: #shellName!private! !

!RowanMethodListBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 551 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.StatusBar) 34 18 nil 416 34 2 8 1409288460 1 608 nil 466 8 4278190080 nil 7 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 nil 608 nil 8 4294905163 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 608 nil 459270 ##(Smalltalk.Message) #displayString 8 #() 882 #iconImageIndex 8 #() 1049926 1 ##(Smalltalk.IconImageManager) 8 'statusBarField' 34 1 864 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 608 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 5 1079 514 1565 41 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 27 2 0 0 16 3 0 0 47 2 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 5 1314 #fixedViewLeft 1565 1314 #fixedParentTop 1079 1314 #fixedViewTop 41 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 1392 nil nil nil 7 nil nil nil 1392 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.RowanMethodListPresenter) #resource_Default_view nil 1058 138 144 34 1 1122 #createAt:extent: 34 2 514 5 1 514 1565 1079 1392 1202 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 0 0 0 0 16 3 0 0 27 2 0 0] 8 #() 1264 nil 27 1282 1328 5 1344 1565 1360 1 1376 1079 170 192 34 2 608 8 'statusBar' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1058 138 144 34 3 1122 #createAt:extent: 34 2 514 3839 21 514 1601 1201 416 1122 #text: 34 1 8 'Rowan Method List Browser' 416 1122 #updateMenuBar 1632 416 1202 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 159 10 0 0 98 2 0 0] 34 2 1392 608 1264 nil 27 )!

showOnSession: aGciSession

	aGciSession hasServer ifFalse: [
		MessageBox
			warning: 'Server initialization failed at login.'
			caption: 'Unable to Open Browser'.
	]. 
	^self new 
		gciSession: aGciSession;
		createView: self defaultView;
		showShell;
		yourself.! !
!RowanMethodListBrowser class categoriesFor: #resource_Default_view!public!resources-views! !
!RowanMethodListBrowser class categoriesFor: #showOnSession:!public! !

