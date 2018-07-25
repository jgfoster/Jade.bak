"Filed out from Dolphin Smalltalk 7"!

RowanMethodListPresenter subclass: #JadeiteMethodListPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteMethodListPresenter guid: (GUID fromString: '{c27cfd96-c166-40cb-83b6-6073a885a25f}')!
JadeiteMethodListPresenter comment: ''!
!JadeiteMethodListPresenter categoriesForClass!Unclassified! !
!JadeiteMethodListPresenter methodsFor!

createComponents
	methodListPresenter := self add: JadeiteBasicMethodListPresenter new name: 'methodList'.
	methodSourcePresenter := (self add: JadeiteMethodSourcePresenter new name: 'methodSource')
				menuTitle: '&Method Source';
				yourself!

methods: services selecting: string
	services isEmpty ifTrue: [^self].
	methodListPresenter
		list: services;
		selectFirstMethod.
	self selecting: string!

updateSource
	| selection |
	selection := methodListPresenter selectionOrNil. 
	methodSourcePresenter methodService: selection. 
	methodSourcePresenter update. ! !
!JadeiteMethodListPresenter categoriesFor: #createComponents!public! !
!JadeiteMethodListPresenter categoriesFor: #methods:selecting:!public! !
!JadeiteMethodListPresenter categoriesFor: #updateSource!public! !

!JadeiteMethodListPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 34 6 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 576 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 576 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeiteMethodSourcePresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 415 834 1461 407 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 207 0 0 0 218 2 0 0 154 1 0 0] 528 834 193 193 nil 27 8 'methodSource' 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140916224 131073 960 nil 642 8 4278190080 nil 5 nil nil nil 960 690 ##(Smalltalk.JadeiteBasicMethodListPresenter) #resource_Default_view nil 722 138 144 34 1 786 #createAt:extent: 34 2 834 1 1 834 1461 405 960 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 2 0 0 202 0 0 0] 528 928 nil 27 8 'methodList' 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 1232 nil 642 8 4278190080 nil 517 nil nil nil 1232 722 138 144 34 1 786 #createAt:extent: 34 2 834 1 405 834 1461 11 1232 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 202 0 0 0 218 2 0 0 207 0 0 0] 8 #() 928 nil 27 8 'Splitter' nil 722 138 144 34 1 786 #createAt:extent: 34 2 834 7679 21 834 1461 821 416 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 217 17 0 0 164 1 0 0] 34 3 960 1232 576 928 nil 27 )! !
!JadeiteMethodListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

