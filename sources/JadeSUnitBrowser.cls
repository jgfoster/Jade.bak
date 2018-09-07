"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadeSUnitBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSUnitBrowser guid: (GUID fromString: '{9f573c71-de71-43fe-b9c0-f2e9e68007eb}')!
JadeSUnitBrowser comment: ''!
!JadeSUnitBrowser categoriesForClass!Unclassified! !
!JadeSUnitBrowser methodsFor!

presenterClass

	^JadeSUnitPresenter!

shellName
 
	^'Rowan SUnit Browser'.
! !
!JadeSUnitBrowser categoriesFor: #presenterClass!private! !
!JadeSUnitBrowser categoriesFor: #shellName!overrides!private! !

!JadeSUnitBrowser class methodsFor!

icon
	^Icon fromFile: 'UnitTestPlugin.ico'!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 551 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 640 nil 466 8 4278190080 nil 7 nil nil nil 640 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 1569 1123 640 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 49 2 0 0] 592 514 193 193 nil 27 8 'myPresenter' nil nil nil nil nil 1 nil 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'UnitTestPlugin.ico' nil nil nil 1 nil nil 770 138 144 34 3 834 #createAt:extent: 34 2 514 7679 21 514 1601 1201 416 834 #text: 34 1 8 'Jade SUnit Browser' 416 834 #updateMenuBar 592 416 914 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 31 18 0 0 98 2 0 0] 34 1 640 960 nil 27 )! !
!JadeSUnitBrowser class categoriesFor: #icon!public! !
!JadeSUnitBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

