"Filed out from Dolphin Smalltalk 7"!

RowanMethodListPresenter subclass: #JadeiteMethodListPresenter
	instanceVariableNames: 'textAreaTabs comparisonTab comparisonPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteMethodListPresenter guid: (GUID fromString: '{c27cfd96-c166-40cb-83b6-6073a885a25f}')!
JadeiteMethodListPresenter comment: ''!
!JadeiteMethodListPresenter categoriesForClass!Unclassified! !
!JadeiteMethodListPresenter methodsFor!

comparisonTab
	^comparisonTab
		ifNil: 
			[comparisonTab := self textTabs
						detect: [:each | (each class canUnderstand: #text) ifTrue: [each text = 'Comparison'] ifFalse: [each = 'Comparison']]]!

createComponents
	methodListPresenter := self add: JadeiteBasicMethodListPresenter new name: 'methodList'.
	methodSourcePresenter := (self add: JadeiteMethodSourcePresenter new name: 'methodSource')
				menuTitle: '&Method Source'.
	comparisonPresenter := self add: DifferencesPresenter new name: 'comparisonTab'.!

createSchematicWiring
	super createSchematicWiring.
	self getViews.
	methodListPresenter
		when: #methodSelectionChanged	send: #updateMethodSource 	to: self!

getViews 
	textAreaTabs := parentPresenter view viewNamed: 'textAreaTabs'!

methods: services selecting: string
	services isEmpty ifTrue: [^self].
	methodListPresenter
		list: services;
		selectFirstMethod.
	self selecting: string!

removeComparisonTab
	self textTabs remove: self comparisonTab ifAbsent: []!

showComparisonTab
	self textTabs add: self comparisonTab.
	comparisonPresenter ensureVisible!

textTabs
	^(textAreaTabs subViews detect: [:each | each class == TabViewXP]) model!

updateMethodSource
	methodListPresenter selections isEmpty ifTrue: [^methodSourcePresenter value: String new].
	methodListPresenter selections size = 1 ifTrue:[^self updateSingleMethodSource].	
	self updateMultiMethodComparison
	!

updateMultiMethodComparison
	comparisonPresenter
		beforeTitle: (methodListPresenter selections at: 1) classMethodDisplayString;
		afterTitle: (methodListPresenter selections at: 2) classMethodDisplayString;
		compare: (methodListPresenter selections at: 1) source
			with: (methodListPresenter selections at: 2) source.!

updateSingleMethodSource
	methodSourcePresenter value: methodListPresenter selection source.
	methodSourcePresenter ensureVisible!

updateSource
	| selection |
	selection := methodListPresenter selectionOrNil. 
	methodSourcePresenter methodService: selection. 
	methodSourcePresenter update. ! !
!JadeiteMethodListPresenter categoriesFor: #comparisonTab!private!selection!text tabs! !
!JadeiteMethodListPresenter categoriesFor: #createComponents!public! !
!JadeiteMethodListPresenter categoriesFor: #createSchematicWiring!public! !
!JadeiteMethodListPresenter categoriesFor: #getViews!public! !
!JadeiteMethodListPresenter categoriesFor: #methods:selecting:!public! !
!JadeiteMethodListPresenter categoriesFor: #removeComparisonTab!private!selection!text tabs! !
!JadeiteMethodListPresenter categoriesFor: #showComparisonTab!private!selection!text tabs! !
!JadeiteMethodListPresenter categoriesFor: #textTabs!private!text tabs! !
!JadeiteMethodListPresenter categoriesFor: #updateMethodSource!public!updating! !
!JadeiteMethodListPresenter categoriesFor: #updateMultiMethodComparison!private!text tabs! !
!JadeiteMethodListPresenter categoriesFor: #updateSingleMethodSource!private!text tabs! !
!JadeiteMethodListPresenter categoriesFor: #updateSource!public! !

!JadeiteMethodListPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 34 6 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140916224 131073 576 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 576 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeiteBasicMethodListPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 1 834 1461 405 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 2 0 0 202 0 0 0] 528 834 193 193 nil 27 8 'methodList' 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 960 nil 642 8 4278190080 nil 5 nil nil nil 960 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 8 'Method Source' 410 ##(Smalltalk.ReferenceView) 34 14 nil 960 34 2 8 1140850688 131073 1168 nil 642 8 4278190080 nil 5 nil nil nil 1168 690 ##(Smalltalk.JadeiteMethodSourcePresenter) #resource_Default_view nil 722 138 144 34 1 786 #createAt:extent: 34 2 834 9 55 834 1445 345 1168 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 214 2 0 0 199 0 0 0] 528 928 nil 27 1122 8 'Comparison' 410 ##(Smalltalk.ShellView) 34 27 nil 960 34 2 8 1078919168 131073 1456 nil 642 8 4278190080 nil 517 nil nil nil 1456 788230 ##(Smalltalk.BorderLayout) 1 1 nil nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 1456 34 2 8 1140850688 131073 1584 nil 642 1536 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 5 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #toggleIndentGuides 8 'Indentation &Guides' 1 1 nil nil nil 1714 1 1746 #toggleVisibleLineEndings 8 'Line &Endings' 1 1 nil nil nil 1714 1 1746 #toggleLineNumberMargins 8 'Line N&umbers' 1 1 nil nil nil 1714 1 1746 #toggleVisibleWhitespace 8 'W&hitespace' 1 1 nil nil nil 1714 1 1746 #toggleWrapping 8 '&Word Wrap' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 1584 482 170 176 528 false 170 192 528 nil 722 138 144 34 2 786 #createAt:extent: 34 2 834 1 1 834 1413 313 1584 786 #contextMenu: 34 1 1680 1584 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 194 2 0 0 156 0 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 1584 34 2 8 1140850688 131073 2240 nil 642 1536 nil 5 nil nil nil 2240 1554 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 2240 34 2 8 1140850945 262145 2336 nil nil nil 5 nil nil nil 2336 nil 8 1660708144 852486 ##(Smalltalk.NullConverter) nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 834 1 273 834 701 41 2336 786 #text: 34 1 8 '
' 2336 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 136 0 0 0 94 1 0 0 156 0 0 0] 8 #() 928 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 2240 34 2 8 1140920580 1025 2656 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 642 1536 nil 5 nil nil nil 2656 nil 8 4294902887 2418 nil nil 11 nil 170 192 34 12 #indentGuide 1182790 1 ##(Smalltalk.ScintillaTextStyle) 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 2866 3 786694 ##(Smalltalk.IndexedColor) 33554445 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 2866 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 2866 77 2914 33554457 2914 33554471 1 nil nil nil nil #callTip nil nil nil #normal 2866 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 2866 5 nil 196934 1 ##(Smalltalk.RGB) 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 3008 2896 3024 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 2944 nil nil nil 2880 nil nil 1310982 ##(Smalltalk.DiffsScintillaStyler) 8 #() 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 2656 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 528 nil 170 176 528 9215 nil nil nil nil 2914 33554447 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 2832 nil nil nil nil 1 nil 170 192 34 6 8 'indicator0' 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 2656 33554459 13 false nil nil nil nil nil nil nil 8 'indicator1' 3346 3 2656 33423361 5 false nil nil nil nil nil nil nil 8 'indicator2' 3346 5 2656 511 1 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 2866 1029 nil 3042 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 2866 1027 3042 60239 3042 31326207 1 nil nil nil nil #Warning nil nil nil #Error 2866 1031 3042 335 3042 31317503 1 nil nil nil nil #Error nil nil nil nil 722 138 144 34 7 786 #createAt:extent: 34 2 834 1 1 834 701 273 2656 786 #modificationEventMask: 8 #(4607) 2656 786 #caretPeriod: 8 #(530) 2656 786 #wordWrap: 8 #(true) 2656 786 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 2656 49 3 nil nil nil nil 3858 3 2656 1 nil nil 67108863 nil nil 3858 5 2656 1 nil nil nil nil nil 2656 786 #canHScroll: 8 #(false) 2656 786 #maxCompletionListHeight: 8 #(9) 2656 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 136 0 0 0] 8 #() 928 nil 29 170 192 34 4 2656 8 'before' 2336 8 'beforeTitle' nil 722 138 144 34 1 786 #createAt:extent: 34 2 834 1 1 834 701 313 2240 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 156 0 0 0] 34 2 2656 2336 928 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 1584 34 2 8 1140850688 1 4256 nil 3042 66047 nil 517 nil nil nil 4256 722 138 144 34 1 786 #createAt:extent: 34 2 834 701 1 834 11 313 4256 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 94 1 0 0 0 0 0 0 99 1 0 0 156 0 0 0] 8 #() 928 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 1584 34 2 8 1140850688 131073 4496 nil 642 1536 nil 5 nil nil nil 4496 1554 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 4496 34 2 8 1140850945 262145 4592 nil nil nil 5 nil nil nil 4592 nil 8 1660708144 2418 nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 834 1 273 834 703 41 4592 786 #text: 34 1 8 '
' 4592 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 136 0 0 0 95 1 0 0 156 0 0 0] 8 #() 928 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 4496 34 2 8 1140920580 1025 4896 2722 nil false 2768 nil 642 1536 nil 5 nil nil nil 4896 nil 8 4294902887 2418 nil nil 11 nil 170 192 34 12 #indentGuide 2866 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 2866 3 2928 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 2866 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 2866 77 2976 2992 1 nil nil nil nil #callTip nil nil nil #normal 2866 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 2866 5 nil 3042 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 5120 5072 5136 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 5088 nil nil nil 5056 nil nil 3090 3120 170 192 34 2 #default 3170 1 nil nil nil 4896 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 528 nil 170 176 528 9215 nil nil nil nil 3232 nil nil 65 nil nil nil 3248 3 170 192 34 2 #container 5024 nil nil nil nil 1 nil 170 192 34 6 8 'indicator0' 3346 1 4896 33554459 13 false nil nil nil nil nil nil nil 8 'indicator1' 3346 3 4896 33423361 5 false nil nil nil nil nil nil nil 8 'indicator2' 3346 5 4896 511 1 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 2866 1029 nil 3042 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 2866 1027 3042 60239 3042 31326207 1 nil nil nil nil #Warning nil nil nil #Error 2866 1031 3042 335 3042 31317503 1 nil nil nil nil #Error nil nil nil nil 722 138 144 34 7 786 #createAt:extent: 34 2 834 1 1 834 703 273 4896 786 #modificationEventMask: 8 #(4607) 4896 786 #caretPeriod: 8 #(530) 4896 786 #wordWrap: 8 #(true) 4896 786 #margins: 34 1 34 3 3858 1 4896 49 3 nil nil nil nil 3858 3 4896 1 nil nil 67108863 nil nil 3858 5 4896 1 nil nil nil nil nil 4896 786 #canHScroll: 8 #(false) 4896 786 #maxCompletionListHeight: 8 #(9) 4896 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 95 1 0 0 136 0 0 0] 8 #() 928 nil 29 170 192 34 4 4592 8 'afterTitle' 4896 8 'after' nil 722 138 144 34 1 786 #createAt:extent: 34 2 834 711 1 834 703 313 4496 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 99 1 0 0 0 0 0 0 194 2 0 0 156 0 0 0] 34 2 4896 4592 928 nil 27 928 nil 27 170 192 34 2 1584 8 'diffs' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 1666 nil true 34 4 1714 1 1746 #selectUpper 8 'Select &upper' 1 1 nil nil nil 1714 1 1746 #selectLower 8 'Select &lower' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 1714 1 1746 #exit 8 '&Close' 1 1 nil nil nil 8 '&File' nil 1 nil nil 54755 nil nil 1666 nil true 34 3 1714 1 1746 #character 8 '&Character' 1 1 nil nil nil 1714 1 1746 #word 8 '&Word' 1 1 nil nil nil 1714 1 1746 #line 8 '&Line' 1 1 nil nil nil 8 '&Mode' nil 1 nil nil 54757 nil nil 1666 nil true 34 1 1714 1 1746 #about 8 '&About' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 54759 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 722 138 144 34 3 786 #createAt:extent: 34 2 834 9 55 834 1445 345 1456 786 #text: 34 1 8 'Differences' 1456 786 #updateMenuBar 528 1456 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 214 2 0 0 199 0 0 0] 34 1 1584 928 nil 27 1456 170 192 34 4 1456 8 'comparisonTab' 1168 8 'methodSource' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 960 34 2 8 1140916736 1 7152 590662 2 ##(Smalltalk.ListModel) 138 144 34 2 1152 1440 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) nil nil 1 nil nil nil 7152 nil 8 4294907939 787814 3 ##(Smalltalk.BlockClosure) 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 7344 7 257 nil 7330 0 nil 7362 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 7408 7 257 nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #noIcons nil nil nil nil nil 722 138 144 34 3 786 #createAt:extent: 34 2 834 1 1 834 1461 407 7152 786 #basicSelectionsByIndex: 34 1 8 #(2) 7152 786 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 7152 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 2 0 0 203 0 0 0] 8 #() 928 nil 27 722 138 144 34 2 786 #createAt:extent: 34 2 834 1 415 834 1461 407 960 786 #text: 34 1 8 'textAreaTabs' 960 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 207 0 0 0 218 2 0 0 154 1 0 0] 34 3 1168 1456 7152 928 nil 27 8 'textAreaTabs' 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 7952 nil 642 8 4278190080 nil 517 nil nil nil 7952 722 138 144 34 1 786 #createAt:extent: 34 2 834 1 405 834 1461 11 7952 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 202 0 0 0 218 2 0 0 207 0 0 0] 8 #() 928 nil 27 8 'Splitter' nil 722 138 144 34 1 786 #createAt:extent: 34 2 834 7679 21 834 1461 821 416 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 217 17 0 0 164 1 0 0] 34 3 576 7952 960 928 nil 27 )! !
!JadeiteMethodListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

