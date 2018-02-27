"Filed out from Dolphin Smalltalk 7"!

RowanPresenter subclass: #RowanMethodListPresenter
	instanceVariableNames: 'methodList service methodListPresenter methodSourcePresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodListPresenter guid: (GUID fromString: '{eae7647f-1178-4328-8c70-76e118a2c731}')!
RowanMethodListPresenter comment: ''!
!RowanMethodListPresenter categoriesForClass!Unclassified! !
!RowanMethodListPresenter methodsFor!

browseImplementorsOf: aString

	| definitions |
	definitions := service implementorsOf: aString!

createComponents

	super createComponents.
	methodListPresenter 	:= self add: PackageMethodListPresenter	new name: 'methodList'.
	methodSourcePresenter	:= (self add: MethodSourcePresenter	new name: 'methodSource') menuTitle: '&Method Source'; yourself.! !
!RowanMethodListPresenter categoriesFor: #browseImplementorsOf:!browsing!public! !
!RowanMethodListPresenter categoriesFor: #createComponents!public! !

!RowanMethodListPresenter class methodsFor!

defaultModel
	^RowanMethodDefinitionService new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 34 2 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 576 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 517 nil nil nil 576 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 405 802 1461 11 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 202 0 0 0 218 2 0 0 207 0 0 0] 8 #() 802 193 193 nil 27 8 'Splitter' nil 690 138 144 34 1 754 #createAt:extent: 34 2 802 3839 21 802 1461 821 416 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 89 10 0 0 164 1 0 0] 34 3 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140916224 131073 1104 nil 642 8 4278190080 nil 5 nil nil nil 1104 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.PackageMethodListPresenter) #resource_MultiClass_view nil 690 138 144 34 1 754 #createAt:extent: 34 2 802 1 1 802 1461 405 1104 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 2 0 0 202 0 0 0] 528 912 nil 27 576 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 1376 nil 642 8 4278190080 nil 5 nil nil nil 1376 1202 ##(Smalltalk.MethodSourcePresenter) #resource_Default_view nil 690 138 144 34 1 754 #createAt:extent: 34 2 802 1 415 802 1461 407 1376 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 207 0 0 0 218 2 0 0 154 1 0 0] 528 912 nil 27 912 nil 27 )! !
!RowanMethodListPresenter class categoriesFor: #defaultModel!public! !
!RowanMethodListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

