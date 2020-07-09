"Filed out from Dolphin Smalltalk 7"!

JadeRewriteMethodsList subclass: #JadeRewriteReplaceMethodsList
	instanceVariableNames: 'differencePresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeRewriteReplaceMethodsList guid: (GUID fromString: '{5f2b2feb-5f0f-41f7-949a-b22e26f695d4}')!
JadeRewriteReplaceMethodsList comment: ''!
!JadeRewriteReplaceMethodsList categoriesForClass!Unclassified! !
!JadeRewriteReplaceMethodsList methodsFor!

apply
	methodsPresenter hasSelection ifFalse: [^self].

	self applyTo: methodsPresenter selection!

applyAll

	methodsPresenter model do: [:each | self applyTo: each]!

applyTo: aJadeGsMethodShape 

	aJadeGsMethodShape applied: true.

	self compile: aJadeGsMethodShape replacementSource in: aJadeGsMethodShape !

compile: aString in: aJadeGsMethodShape 
	| targetClass gsCommand |

	targetClass := aJadeGsMethodShape isInstanceMethod ifTrue: [aJadeGsMethodShape ownerClass] ifFalse: [aJadeGsMethodShape ownerClass, ' class'].

	gsCommand := String new writeStream.
	gsCommand nextPutAll: targetClass, ' compile: ''';
		nextPutAll: (parentPresenter quoteSourceCode: aString);
		nextPutAll:  '''.'.

	^parentPresenter gciSession executeString: gsCommand contents!

createComponents

	super createComponents.

	differencePresenter := self add: DifferencesPresenter new name: 'difference'.!

gemStoneConfigurableFormatterOptions

	^'RBConfigurableFormatter
newLinesAfterTemporaries: 2;
newLinesAfterMethodComment: 1;
keepBlockInMessage: true;
yourself.'!

gemStoneReplacementCode

	^parentPresenter isMethodPresenterValue 
		ifTrue: [self gemStoneReplacementCodeForMethod] 
		ifFalse: [self gemStoneReplacementCodeForExpression]!

gemStoneReplacementCodeForExpression

	^'| progamNode rewriter |
	progamNode := RBParseTreeSearcher buildTree: ''', (parentPresenter quoteSourceCode: methodsPresenter selection source), ''' method: true.
	rewriter := RBParseTreeRewriter new.
	rewriter replace: ''', parentPresenter currentRule , ''' with: ''', parentPresenter replaceText, '''.
	rewriter executeTree: progamNode.
	^', JadeRewriteToolPreference default formatterClass, ' new format: rewriter tree'
	!

gemStoneReplacementCodeForMethod

	^'| progamNode rewriter |
	progamNode := RBParseTreeSearcher buildTree: ''', (parentPresenter quoteSourceCode: methodsPresenter selection source), ''' method: true.
	rewriter := RBParseTreeRewriter new.
	rewriter replaceMethod: ''', parentPresenter currentRule , ''' with: ''', parentPresenter replaceText, '''.
	rewriter executeTree: progamNode.
	^', JadeRewriteToolPreference default formatterClass, ' new format: rewriter tree'
	!

onSelectorChanged

	methodsPresenter hasSelection ifFalse: [ differencePresenter beforeText: ''. ^differencePresenter afterText: ''. ].

	differencePresenter beforeText: methodsPresenter selection source.

	parentPresenter isReplacing ifFalse: [^self].

	methodsPresenter selection replacementSource: (parentPresenter gciSession executeString: self gemStoneReplacementCode).
	differencePresenter afterText: methodsPresenter selection replacementSource.

	differencePresenter refresh!

onViewOpened

	super onViewOpened.

	differencePresenter
		textStyles: SmalltalkWorkspace textStyles;
		styler: SmalltalkMethodStyler new;
		beforeTitle: 'Current Method';
		afterTitle: 'After Change'
!

rollback

	methodsPresenter hasSelection ifFalse: [^self].

	self rollback: methodsPresenter selection!

rollback: aJadeGsMethodShape

	aJadeGsMethodShape applied: false .

	self compile: aJadeGsMethodShape source in: aJadeGsMethodShape !

rollbackAll

	methodsPresenter model do: [:each | self rollback: each]! !
!JadeRewriteReplaceMethodsList categoriesFor: #apply!code actions!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #applyAll!code actions!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #applyTo:!code actions!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #compile:in:!code actions!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #createComponents!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #gemStoneConfigurableFormatterOptions!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #gemStoneReplacementCode!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #gemStoneReplacementCodeForExpression!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #gemStoneReplacementCodeForMethod!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #onSelectorChanged!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #onViewOpened!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #rollback!code actions!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #rollback:!code actions!public! !
!JadeRewriteReplaceMethodsList categoriesFor: #rollbackAll!code actions!public! !

!JadeRewriteReplaceMethodsList class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 528 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 2731 21 674 1591 1061 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 85 5 0 0 10 0 0 0 112 8 0 0 28 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 784 nil nil nil 5 nil nil nil 784 482 170 176 528 false 170 192 34 2 410 ##(Smalltalk.EditableListView) 34 40 nil 784 34 2 8 1140920397 1025 912 590662 2 ##(Smalltalk.ListModel) 138 144 528 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 7 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #apply 8 'Apply' 1 1 nil nil nil 1154 1 1186 #applyAll 8 'Apply All' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 1154 1 1186 #rollback 8 'Rollback' 1 1 nil nil nil 1154 1 1186 #rollbackAll 8 'Rollback All' 1 1 nil nil nil 1282 4097 1154 1 1186 #removeFromList 8 'Remove from List' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 912 nil 8 1936057711 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil 674 65 65 nil nil 138 144 34 4 1447750 15 ##(Smalltalk.EditableListViewColumn) 8 '' 51 #left 459270 ##(Smalltalk.Message) #displayString 8 #() 1634 #<= 1664 nil nil 912 nil 1 nil nil false nil 1052998 20 ##(Smalltalk.EmbeddedTextEdit) nil nil 8 #(67174400 0) 1712 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 1072 nil 5 nil nil nil 1712 nil nil 852486 ##(Smalltalk.NullConverter) nil 1616 67 nil nil nil nil nil nil nil nil nil 1586 8 'Class' 501 #left 1634 #printClass 1664 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a printClass <= b printClass]' 8 #[30 105 226 0 226 32 130 106] #printClass 1904 7 513 nil nil nil 912 nil 1 nil nil false nil 1698 nil nil 8 #(67174400 0) 2048 1746 nil false 1792 nil nil nil 5 nil nil nil 2048 nil nil 1810 nil 8 '' 67 nil nil nil nil nil nil nil nil nil 1586 8 'Selector' 701 #left 1634 #methodName 8 #() 1890 0 nil 1922 2 1 1632 8 'doIt' 8 '[:a :b | a methodName <= b methodName]' 8 #[30 105 226 0 226 32 130 106] #methodName 2192 7 513 nil nil nil 912 nil 1 nil nil false nil 1698 nil nil 8 #(67174400 0) 2272 1746 nil false 1792 nil 1072 nil 5 nil nil nil 2272 nil nil 1810 nil 1616 67 nil nil nil nil nil nil nil nil nil 1586 8 'Applied' 121 #left 1634 #applied 8 #() 1890 0 nil 1922 2 1 1632 8 'doIt' 8 '[:a :b | a applied <= b applied]' 8 #[30 105 226 0 226 32 130 106] #applied 2400 7 513 nil nil nil 912 nil 1 nil nil false nil 1698 nil nil 8 #(67174528 0) 2480 1746 nil false 1792 nil 1072 nil 5 nil nil nil 2480 nil nil nil 1 nil nil nil nil nil nil nil nil nil #report 528 nil 131169 nil 34 4 nil nil 674 1 1 nil nil nil nil nil nil 3 nil nil nil nil 562 138 144 34 2 626 #createAt:extent: 34 2 674 1 1 674 1591 525 912 626 #contextMenu: 34 1 1120 912 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 27 3 0 0 6 1 0 0] 8 #() 674 193 193 nil 33 8 'methods' nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 1 674 1591 525 784 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 27 3 0 0 6 1 0 0] 34 1 912 2752 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 2944 nil 1072 nil 517 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 2944 nil 1 #left nil nil nil 674 1 1 674 9 9 nil 3040 nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 525 674 1591 11 2944 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 6 1 0 0 27 3 0 0 11 1 0 0] 8 #() 2752 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 3232 nil nil nil 5 nil nil nil 3232 482 170 176 528 false 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 3232 34 2 8 1140850688 131073 3360 nil 1072 nil 5 nil nil nil 3360 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.DifferencesPresenter) #resource_Default_view nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 1 674 1591 527 3360 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 27 3 0 0 7 1 0 0] 528 2752 nil 27 8 'difference' nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 535 674 1591 527 3232 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 11 1 0 0 27 3 0 0 18 2 0 0] 34 1 3360 2752 nil 27 2752 nil 27 )! !
!JadeRewriteReplaceMethodsList class categoriesFor: #resource_Default_view!public!resources-views! !

