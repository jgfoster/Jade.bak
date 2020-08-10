"Filed out from Dolphin Smalltalk 7"!

JadeRewriteMethodsList subclass: #JadeRewriteSearchMethodsList
	instanceVariableNames: 'sourcePresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeRewriteSearchMethodsList guid: (GUID fromString: '{903a1851-cf23-4293-98ef-81974bea6a11}')!
JadeRewriteSearchMethodsList comment: ''!
!JadeRewriteSearchMethodsList categoriesForClass!Unclassified! !
!JadeRewriteSearchMethodsList methodsFor!

createComponents

	super createComponents.

	sourcePresenter := self add: TextPresenter new name: 'source'.!

onSelectorChanged

	methodsPresenter hasSelection ifFalse: [^sourcePresenter model: '' asValue].

	sourcePresenter model: methodsPresenter selection source asValue.!

onViewOpened

	super onViewOpened.

	sourcePresenter view 
		textStyles: SmalltalkWorkspace textStyles;
		styler: SmalltalkMethodStyler new
! !
!JadeRewriteSearchMethodsList categoriesFor: #createComponents!public! !
!JadeRewriteSearchMethodsList categoriesFor: #onSelectorChanged!public! !
!JadeRewriteSearchMethodsList categoriesFor: #onViewOpened!public! !

!JadeRewriteSearchMethodsList class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 528 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 2731 21 674 1591 1061 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 85 5 0 0 10 0 0 0 112 8 0 0 28 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 784 nil nil nil 5 nil nil nil 784 482 170 176 528 false 170 192 34 2 410 ##(Smalltalk.EditableListView) 34 40 nil 784 34 2 8 1140920397 1025 912 590662 2 ##(Smalltalk.ListModel) 138 144 528 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 nil nil nil 912 nil 8 1936057711 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil 674 65 65 nil nil 138 144 34 4 1447750 15 ##(Smalltalk.EditableListViewColumn) 8 '' 51 #left 459270 ##(Smalltalk.Message) #displayString 8 #() 1250 #<= 1280 nil nil 912 nil 1 nil nil false nil 1052998 20 ##(Smalltalk.EmbeddedTextEdit) nil nil 8 #(67174400 0) 1328 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 1072 nil 5 nil nil nil 1328 nil nil 852486 ##(Smalltalk.NullConverter) nil 1232 67 nil nil nil nil nil nil nil nil nil 1202 8 'Class' 501 #left 1250 #printClass 1280 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a printClass <= b printClass]' 8 #[30 105 226 0 226 32 130 106] #printClass 1520 7 513 nil nil nil 912 nil 1 nil nil false nil 1314 nil nil 8 #(67174400 0) 1664 1362 nil false 1408 nil nil nil 5 nil nil nil 1664 nil nil 1426 nil 8 '' 67 nil nil nil nil nil nil nil nil nil 1202 8 'Selector' 701 #left 1250 #methodName 8 #() 1506 0 nil 1538 2 1 1248 8 'doIt' 8 '[:a :b | a methodName <= b methodName]' 8 #[30 105 226 0 226 32 130 106] #methodName 1808 7 513 nil nil nil 912 nil 1 nil nil false nil 1314 nil nil 8 #(67174400 0) 1888 1362 nil false 1408 nil 1072 nil 5 nil nil nil 1888 nil nil 1426 nil 1232 67 nil nil nil nil nil nil nil nil nil 1202 8 'Applied' 121 #left 1250 #applied 8 #() 1506 0 nil 1538 2 1 1248 8 'doIt' 8 '[:a :b | a applied <= b applied]' 8 #[30 105 226 0 226 32 130 106] #applied 2016 7 513 nil nil nil 912 nil 1 nil nil false nil 1314 nil nil 8 #(67174528 0) 2096 1362 nil false 1408 nil 1072 nil 5 nil nil nil 2096 nil nil nil 1 nil nil nil nil nil nil nil nil nil #report 528 nil 131169 nil 34 4 nil nil 674 1 1 nil nil nil nil nil nil 3 nil nil nil nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 1 674 1591 525 912 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 27 3 0 0 6 1 0 0] 8 #() 674 193 193 nil 33 8 'methods' nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 1 674 1591 525 784 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 27 3 0 0 6 1 0 0] 34 1 912 2336 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 2528 nil 1072 nil 517 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 2528 nil 1 #left nil nil nil 674 1 1 674 9 9 nil 2624 nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 525 674 1591 11 2528 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 6 1 0 0 27 3 0 0 11 1 0 0] 8 #() 2336 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 2816 nil nil nil 5 nil nil nil 2816 482 170 176 528 false 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 2816 34 2 8 1140850688 131073 2944 nil 1072 nil 5 nil nil nil 2944 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.TextPresenter) #resource_Scintilla_view nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 1 674 1591 527 2944 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 27 3 0 0 7 1 0 0] 528 2336 nil 27 8 'source' nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 535 674 1591 527 2816 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 11 1 0 0 27 3 0 0 18 2 0 0] 34 1 2944 2336 nil 27 2336 nil 27 )! !
!JadeRewriteSearchMethodsList class categoriesFor: #resource_Default_view!public!resources-views! !

