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
 
	^'SUnit Browser'.
! !
!JadeSUnitBrowser categoriesFor: #presenterClass!private! !
!JadeSUnitBrowser categoriesFor: #shellName!overrides!private! !

!JadeSUnitBrowser class methodsFor!

icon

	^SUnitBrowser icon!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13565952 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 0 416 34 2 8 1140850688 131073 608 0 466 8 4278190080 0 7 0 0 0 608 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitPresenter) #resource_Default_view 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 1 850 1599 1103 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 31 3 0 0 39 2 0 0] 560 850 193 193 0 27 8 'myPresenter' 0 0 0 0 0 1 0 0 0 0 1 0 0 738 138 144 34 3 802 #createAt:extent: 34 2 850 2559 21 850 1631 1181 416 802 #text: 34 1 8 'Jade SUnit Browser' 416 802 #updateMenuBar 560 416 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 46 8 0 0 88 2 0 0] 34 1 608 944 0 27 )! !
!JadeSUnitBrowser class categoriesFor: #icon!public! !
!JadeSUnitBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

