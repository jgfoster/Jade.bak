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

browseImplementorsOf: string
	| service |
	service := RowanQueryService command: #implementorsOf: withArgs: (Array with: string asSymbol).
	self issueCommand: (Array with: service).
	self methods: service queryResults selecting: string.!

browseSendersOf: string
	| service |
	service := RowanQueryService command: #sendersOf: withArgs: (Array with: string asSymbol).
	self issueCommand: (Array with: service).
	self methods: service queryResults selecting: string!

comparisonTab
	^comparisonTab
		ifNil: 
			[comparisonTab := self textTabs
						detect: [:each | (each class canUnderstand: #text) ifTrue: [each text = 'Comparison'] ifFalse: [each = 'Comparison']]]!

contextObject
	^nil!

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

fileSave

	self halt. !

getViews 
	textAreaTabs := parentPresenter view viewNamed: 'textAreaTabs'!

methodListSelection
	^methodListPresenter selectionOrNil!

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
	self updateMultiMethodComparison.
	self refreshBreakPointsIn: methodSourcePresenter
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
!JadeiteMethodListPresenter categoriesFor: #browseImplementorsOf:!menu handlers!public! !
!JadeiteMethodListPresenter categoriesFor: #browseSendersOf:!menu handlers!public! !
!JadeiteMethodListPresenter categoriesFor: #comparisonTab!private!selection!text tabs! !
!JadeiteMethodListPresenter categoriesFor: #contextObject!public! !
!JadeiteMethodListPresenter categoriesFor: #createComponents!public! !
!JadeiteMethodListPresenter categoriesFor: #createSchematicWiring!public! !
!JadeiteMethodListPresenter categoriesFor: #fileSave!public! !
!JadeiteMethodListPresenter categoriesFor: #getViews!public! !
!JadeiteMethodListPresenter categoriesFor: #methodListSelection!public! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140916224 131073 544 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 544 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeiteBasicMethodListPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 1 802 1461 271 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 2 0 0 135 0 0 0] 8 #() 802 193 193 nil 27 524806 ##(Smalltalk.Fraction) 3 5 true 170 192 34 6 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 992 nil 610 8 4278190080 nil 517 nil nil nil 992 690 138 144 34 1 754 #createAt:extent: 34 2 802 1 271 802 1461 11 992 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 135 0 0 0 218 2 0 0 140 0 0 0] 8 #() 912 nil 27 8 'Splitter' 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 1264 nil 610 8 4278190080 nil 5 nil nil nil 1264 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 8 'Comparison' 410 ##(Smalltalk.ShellView) 34 27 nil 1264 34 2 8 1078919168 131073 1472 nil 610 8 4278190080 nil 517 nil nil nil 1472 788230 ##(Smalltalk.BorderLayout) 1 1 nil nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 1472 34 2 8 1140850688 131073 1600 nil 610 1552 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 5 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #toggleIndentGuides 8 'Indentation &Guides' 1 1 nil nil nil 1730 1 1762 #toggleVisibleLineEndings 8 'Line &Endings' 1 1 nil nil nil 1730 1 1762 #toggleLineNumberMargins 8 'Line N&umbers' 1 1 nil nil nil 1730 1 1762 #toggleVisibleWhitespace 8 'W&hitespace' 1 1 nil nil nil 1730 1 1762 #toggleWrapping 8 '&Word Wrap' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 1600 482 170 176 896 false 170 192 896 nil 690 138 144 34 2 754 #createAt:extent: 34 2 802 1 1 802 1413 447 1600 754 #contextMenu: 34 1 1696 1600 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 194 2 0 0 223 0 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 1600 34 2 8 1140850688 131073 2256 nil 610 1552 nil 5 nil nil nil 2256 1570 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 2256 34 2 8 1140850945 262145 2352 nil nil nil 5 nil nil nil 2352 nil 8 1695245616 852486 ##(Smalltalk.NullConverter) nil nil nil 690 138 144 34 2 754 #createAt:extent: 34 2 802 1 407 802 701 41 2352 754 #text: 34 1 8 '
' 2352 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 203 0 0 0 94 1 0 0 223 0 0 0] 8 #() 912 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 2256 34 2 8 1140920580 1025 2672 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 610 1552 nil 5 nil nil nil 2672 nil 8 4294909987 2434 nil nil 11 nil 170 192 34 12 #indentGuide 1182790 1 ##(Smalltalk.ScintillaTextStyle) 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 2882 3 786694 ##(Smalltalk.IndexedColor) 33554445 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 2882 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 2882 77 2930 33554457 2930 33554471 1 nil nil nil nil #callTip nil nil nil #normal 2882 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 2882 5 nil 196934 1 ##(Smalltalk.RGB) 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 3024 2912 3040 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 2960 nil nil nil 2896 nil nil 1310982 ##(Smalltalk.DiffsScintillaStyler) 8 #() 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 2672 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 896 nil 170 176 896 9215 nil nil nil nil 2930 33554447 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 2848 nil nil nil nil 1 nil 170 192 34 6 8 'indicator0' 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 2672 33554459 13 false nil nil nil nil nil nil nil 8 'indicator2' 3362 5 2672 511 1 false nil nil nil nil nil nil nil 8 'indicator1' 3362 3 2672 33423361 5 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 2882 1029 nil 3058 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 2882 1027 3058 60239 3058 31326207 1 nil nil nil nil #Warning nil nil nil #Error 2882 1031 3058 335 3058 31317503 1 nil nil nil nil #Error nil nil nil nil 690 138 144 34 7 754 #createAt:extent: 34 2 802 1 1 802 701 407 2672 754 #modificationEventMask: 8 #(4607) 2672 754 #caretPeriod: 8 #(530) 2672 754 #wordWrap: 8 #(true) 2672 754 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 2672 49 3 nil nil nil nil 3874 3 2672 1 nil nil 67108863 nil nil 3874 5 2672 1 nil nil nil nil nil 2672 754 #canHScroll: 8 #(false) 2672 754 #maxCompletionListHeight: 8 #(9) 2672 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 203 0 0 0] 8 #() 912 nil 29 170 192 34 4 2672 8 'before' 2352 8 'beforeTitle' nil 690 138 144 34 1 754 #createAt:extent: 34 2 802 1 1 802 701 447 2256 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 223 0 0 0] 34 2 2672 2352 912 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 1600 34 2 8 1140850688 1 4272 nil 3058 66047 nil 517 nil nil nil 4272 690 138 144 34 1 754 #createAt:extent: 34 2 802 701 1 802 11 447 4272 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 94 1 0 0 0 0 0 0 99 1 0 0 223 0 0 0] 8 #() 912 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 1600 34 2 8 1140850688 131073 4512 nil 610 1552 nil 5 nil nil nil 4512 1570 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 4512 34 2 8 1140850945 262145 4608 nil nil nil 5 nil nil nil 4608 nil 8 1695245616 2434 nil nil nil 690 138 144 34 2 754 #createAt:extent: 34 2 802 1 407 802 703 41 4608 754 #text: 34 1 8 '
' 4608 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 203 0 0 0 95 1 0 0 223 0 0 0] 8 #() 912 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 4512 34 2 8 1140920580 1025 4912 2738 nil false 2784 nil 610 1552 nil 5 nil nil nil 4912 nil 8 4294909987 2434 nil nil 11 nil 170 192 34 12 #indentGuide 2882 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 2882 3 2944 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 2882 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 2882 77 2992 3008 1 nil nil nil nil #callTip nil nil nil #normal 2882 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 2882 5 nil 3058 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 5136 5088 5152 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 5104 nil nil nil 5072 nil nil 3106 3136 170 192 34 2 #default 3186 1 nil nil nil 4912 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 896 nil 170 176 896 9215 nil nil nil nil 3248 nil nil 65 nil nil nil 3264 3 170 192 34 2 #container 5040 nil nil nil nil 1 nil 170 192 34 6 8 'indicator1' 3362 3 4912 33423361 5 false nil nil nil nil nil nil nil 8 'indicator2' 3362 5 4912 511 1 false nil nil nil nil nil nil nil 8 'indicator0' 3362 1 4912 33554459 13 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 2882 1029 nil 3058 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 2882 1027 3058 60239 3058 31326207 1 nil nil nil nil #Warning nil nil nil #Error 2882 1031 3058 335 3058 31317503 1 nil nil nil nil #Error nil nil nil nil 690 138 144 34 7 754 #createAt:extent: 34 2 802 1 1 802 703 407 4912 754 #modificationEventMask: 8 #(4607) 4912 754 #caretPeriod: 8 #(530) 4912 754 #wordWrap: 8 #(true) 4912 754 #margins: 34 1 34 3 3874 1 4912 49 3 nil nil nil nil 3874 3 4912 1 nil nil 67108863 nil nil 3874 5 4912 1 nil nil nil nil nil 4912 754 #canHScroll: 8 #(false) 4912 754 #maxCompletionListHeight: 8 #(9) 4912 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 95 1 0 0 203 0 0 0] 8 #() 912 nil 29 170 192 34 4 4912 8 'after' 4608 8 'afterTitle' nil 690 138 144 34 1 754 #createAt:extent: 34 2 802 711 1 802 703 447 4512 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 99 1 0 0 0 0 0 0 194 2 0 0 223 0 0 0] 34 2 4912 4608 912 nil 27 912 nil 27 170 192 34 2 1600 8 'diffs' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 1682 nil true 34 4 1730 1 1762 #selectUpper 8 'Select &upper' 1 1 nil nil nil 1730 1 1762 #selectLower 8 'Select &lower' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 1730 1 1762 #exit 8 '&Close' 1 1 nil nil nil 8 '&File' nil 1 nil nil 11723 nil nil 1682 nil true 34 3 1730 1 1762 #character 8 '&Character' 1 1 nil nil nil 1730 1 1762 #word 8 '&Word' 1 1 nil nil nil 1730 1 1762 #line 8 '&Line' 1 1 nil nil nil 8 '&Mode' nil 1 nil nil 11725 nil nil 1682 nil true 34 1 1730 1 1762 #about 8 '&About' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 11727 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 690 138 144 34 3 754 #createAt:extent: 34 2 802 9 55 802 1445 479 1472 754 #text: 34 1 8 'Differences' 1472 754 #updateMenuBar 896 1472 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 214 2 0 0 10 1 0 0] 34 1 1600 912 nil 27 1426 8 'Method Source' 410 ##(Smalltalk.ReferenceView) 34 14 nil 1264 34 2 8 1140850688 131073 7136 nil 610 8 4278190080 nil 5 nil nil nil 7136 658 ##(Smalltalk.JadeiteMethodSourcePresenter) #resource_Default_view nil 690 138 144 34 1 754 #createAt:extent: 34 2 802 9 55 802 1445 479 7136 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 214 2 0 0 10 1 0 0] 896 912 nil 27 7136 170 192 34 4 1472 8 'comparisonTab' 7136 8 'methodSource' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 1264 34 2 8 1140916736 1 7456 590662 2 ##(Smalltalk.ListModel) 138 144 34 2 7120 1456 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) nil nil 1 nil nil nil 7456 nil 8 4294906617 787814 3 ##(Smalltalk.BlockClosure) 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 7648 7 257 nil 7634 0 nil 7666 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 7712 7 257 nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #noIcons nil nil nil nil nil 690 138 144 34 3 754 #createAt:extent: 34 2 802 1 1 802 1461 541 7456 754 #basicSelectionsByIndex: 34 1 8 #(1) 7456 754 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 7456 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 2 0 0 14 1 0 0] 8 #() 912 nil 27 690 138 144 34 2 754 #createAt:extent: 34 2 802 1 281 802 1461 541 1264 754 #text: 34 1 8 'textAreaTabs' 1264 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 140 0 0 0 218 2 0 0 154 1 0 0] 34 3 7136 1472 7456 912 nil 27 8 'textAreaTabs' 544 8 'methodList' nil 690 138 144 34 1 754 #createAt:extent: 34 2 802 7679 21 802 1461 821 416 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 217 17 0 0 164 1 0 0] 34 3 544 992 1264 912 nil 27 )! !
!JadeiteMethodListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

