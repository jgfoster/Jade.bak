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
	self methods: service queryResults selecting: string!

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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140916224 131073 544 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 544 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeiteBasicMethodListPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 1 802 1461 271 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 2 0 0 135 0 0 0] 8 #() 802 193 193 nil 27 524806 ##(Smalltalk.Fraction) 3 5 true 170 192 34 6 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 992 nil 610 8 4278190080 nil 517 nil nil nil 992 690 138 144 34 1 754 #createAt:extent: 34 2 802 1 271 802 1461 11 992 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 135 0 0 0 218 2 0 0 140 0 0 0] 8 #() 912 nil 27 8 'Splitter' 544 8 'methodList' 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 1280 nil 610 8 4278190080 nil 5 nil nil nil 1280 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 8 'Method Source' 410 ##(Smalltalk.ReferenceView) 34 14 nil 1280 34 2 8 1140850688 131073 1488 nil 610 8 4278190080 nil 5 nil nil nil 1488 658 ##(Smalltalk.JadeiteMethodSourcePresenter) #resource_Default_view nil 690 138 144 34 1 754 #createAt:extent: 34 2 802 9 55 802 1445 479 1488 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 214 2 0 0 10 1 0 0] 896 912 nil 27 1442 8 'Comparison' 410 ##(Smalltalk.ShellView) 34 27 nil 1280 34 2 8 1078919168 131073 1776 nil 610 8 4278190080 nil 517 nil nil nil 1776 788230 ##(Smalltalk.BorderLayout) 1 1 nil nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 1776 34 2 8 1140850688 131073 1904 nil 610 1856 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 5 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #toggleIndentGuides 8 'Indentation &Guides' 1 1 nil nil nil 2034 1 2066 #toggleVisibleLineEndings 8 'Line &Endings' 1 1 nil nil nil 2034 1 2066 #toggleLineNumberMargins 8 'Line N&umbers' 1 1 nil nil nil 2034 1 2066 #toggleVisibleWhitespace 8 'W&hitespace' 1 1 nil nil nil 2034 1 2066 #toggleWrapping 8 '&Word Wrap' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 1904 482 170 176 896 false 170 192 896 nil 690 138 144 34 2 754 #createAt:extent: 34 2 802 1 1 802 1413 447 1904 754 #contextMenu: 34 1 2000 1904 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 194 2 0 0 223 0 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 1904 34 2 8 1140850688 131073 2560 nil 610 1856 nil 5 nil nil nil 2560 1874 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 2560 34 2 8 1140850945 262145 2656 nil nil nil 5 nil nil nil 2656 nil 8 1695245616 852486 ##(Smalltalk.NullConverter) nil nil nil 690 138 144 34 2 754 #createAt:extent: 34 2 802 1 407 802 701 41 2656 754 #text: 34 1 8 '
' 2656 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 203 0 0 0 94 1 0 0 223 0 0 0] 8 #() 912 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 2560 34 2 8 1140920580 1025 2976 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 610 1856 nil 5 nil nil nil 2976 nil 8 4294905231 2738 nil nil 11 nil 170 192 34 12 #indentGuide 1182790 1 ##(Smalltalk.ScintillaTextStyle) 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 3186 3 786694 ##(Smalltalk.IndexedColor) 33554445 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 3186 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 3186 77 3234 33554457 3234 33554471 1 nil nil nil nil #callTip nil nil nil #normal 3186 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 3186 5 nil 196934 1 ##(Smalltalk.RGB) 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 3328 3216 3344 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 3264 nil nil nil 3200 nil nil 1310982 ##(Smalltalk.DiffsScintillaStyler) 8 #() 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 2976 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 896 nil 170 176 896 9215 nil nil nil nil 3234 33554447 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 3152 nil nil nil nil 1 nil 170 192 34 6 8 'indicator1' 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 3 2976 33423361 5 false nil nil nil nil nil nil nil 8 'indicator0' 3666 1 2976 33554459 13 false nil nil nil nil nil nil nil 8 'indicator2' 3666 5 2976 511 1 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 3186 1029 nil 3362 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 3186 1027 3362 60239 3362 31326207 1 nil nil nil nil #Warning nil nil nil #Error 3186 1031 3362 335 3362 31317503 1 nil nil nil nil #Error nil nil nil nil 690 138 144 34 7 754 #createAt:extent: 34 2 802 1 1 802 701 407 2976 754 #modificationEventMask: 8 #(4607) 2976 754 #caretPeriod: 8 #(530) 2976 754 #wordWrap: 8 #(true) 2976 754 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 2976 49 3 nil nil nil nil 4178 3 2976 1 nil nil 67108863 nil nil 4178 5 2976 1 nil nil nil nil nil 2976 754 #canHScroll: 8 #(false) 2976 754 #maxCompletionListHeight: 8 #(9) 2976 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 203 0 0 0] 8 #() 912 nil 29 170 192 34 4 2976 8 'before' 2656 8 'beforeTitle' nil 690 138 144 34 1 754 #createAt:extent: 34 2 802 1 1 802 701 447 2560 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 223 0 0 0] 34 2 2976 2656 912 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 1904 34 2 8 1140850688 1 4576 nil 3362 66047 nil 517 nil nil nil 4576 690 138 144 34 1 754 #createAt:extent: 34 2 802 701 1 802 11 447 4576 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 94 1 0 0 0 0 0 0 99 1 0 0 223 0 0 0] 8 #() 912 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 1904 34 2 8 1140850688 131073 4816 nil 610 1856 nil 5 nil nil nil 4816 1874 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 4816 34 2 8 1140850945 262145 4912 nil nil nil 5 nil nil nil 4912 nil 8 1695245616 2738 nil nil nil 690 138 144 34 2 754 #createAt:extent: 34 2 802 1 407 802 703 41 4912 754 #text: 34 1 8 '
' 4912 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 203 0 0 0 95 1 0 0 223 0 0 0] 8 #() 912 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 4816 34 2 8 1140920580 1025 5216 3042 nil false 3088 nil 610 1856 nil 5 nil nil nil 5216 nil 8 4294905231 2738 nil nil 11 nil 170 192 34 12 #indentGuide 3186 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 3186 3 3248 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 3186 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 3186 77 3296 3312 1 nil nil nil nil #callTip nil nil nil #normal 3186 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 3186 5 nil 3362 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 5440 5392 5456 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 5408 nil nil nil 5376 nil nil 3410 3440 170 192 34 2 #default 3490 1 nil nil nil 5216 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 896 nil 170 176 896 9215 nil nil nil nil 3552 nil nil 65 nil nil nil 3568 3 170 192 34 2 #container 5344 nil nil nil nil 1 nil 170 192 34 6 8 'indicator0' 3666 1 5216 33554459 13 false nil nil nil nil nil nil nil 8 'indicator1' 3666 3 5216 33423361 5 false nil nil nil nil nil nil nil 8 'indicator2' 3666 5 5216 511 1 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 3186 1029 nil 3362 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 3186 1027 3362 60239 3362 31326207 1 nil nil nil nil #Warning nil nil nil #Error 3186 1031 3362 335 3362 31317503 1 nil nil nil nil #Error nil nil nil nil 690 138 144 34 7 754 #createAt:extent: 34 2 802 1 1 802 703 407 5216 754 #modificationEventMask: 8 #(4607) 5216 754 #caretPeriod: 8 #(530) 5216 754 #wordWrap: 8 #(true) 5216 754 #margins: 34 1 34 3 4178 1 5216 49 3 nil nil nil nil 4178 3 5216 1 nil nil 67108863 nil nil 4178 5 5216 1 nil nil nil nil nil 5216 754 #canHScroll: 8 #(false) 5216 754 #maxCompletionListHeight: 8 #(9) 5216 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 95 1 0 0 203 0 0 0] 8 #() 912 nil 29 170 192 34 4 4912 8 'afterTitle' 5216 8 'after' nil 690 138 144 34 1 754 #createAt:extent: 34 2 802 711 1 802 703 447 4816 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 99 1 0 0 0 0 0 0 194 2 0 0 223 0 0 0] 34 2 5216 4912 912 nil 27 912 nil 27 170 192 34 2 1904 8 'diffs' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 1986 nil true 34 4 2034 1 2066 #selectUpper 8 'Select &upper' 1 1 nil nil nil 2034 1 2066 #selectLower 8 'Select &lower' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 2034 1 2066 #exit 8 '&Close' 1 1 nil nil nil 8 '&File' nil 1 nil nil 21707 nil nil 1986 nil true 34 3 2034 1 2066 #character 8 '&Character' 1 1 nil nil nil 2034 1 2066 #word 8 '&Word' 1 1 nil nil nil 2034 1 2066 #line 8 '&Line' 1 1 nil nil nil 8 '&Mode' nil 1 nil nil 21709 nil nil 1986 nil true 34 1 2034 1 2066 #about 8 '&About' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 21711 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 690 138 144 34 3 754 #createAt:extent: 34 2 802 9 55 802 1445 479 1776 754 #text: 34 1 8 'Differences' 1776 754 #updateMenuBar 896 1776 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 214 2 0 0 10 1 0 0] 34 1 1904 912 nil 27 1776 170 192 34 4 1776 8 'comparisonTab' 1488 8 'methodSource' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 1280 34 2 8 1140916736 1 7472 590662 2 ##(Smalltalk.ListModel) 138 144 34 2 1472 1760 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) nil nil 1 nil nil nil 7472 nil 8 4294905427 787814 3 ##(Smalltalk.BlockClosure) 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 7664 7 257 nil 7650 0 nil 7682 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 7728 7 257 nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #noIcons nil nil nil nil nil 690 138 144 34 3 754 #createAt:extent: 34 2 802 1 1 802 1461 541 7472 754 #basicSelectionsByIndex: 34 1 8 #(2) 7472 754 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 7472 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 2 0 0 14 1 0 0] 8 #() 912 nil 27 690 138 144 34 2 754 #createAt:extent: 34 2 802 1 281 802 1461 541 1280 754 #text: 34 1 8 'textAreaTabs' 1280 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 140 0 0 0 218 2 0 0 154 1 0 0] 34 3 1488 1776 7472 912 nil 27 8 'textAreaTabs' nil 690 138 144 34 1 754 #createAt:extent: 34 2 802 7679 21 802 1461 821 416 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 217 17 0 0 164 1 0 0] 34 3 544 992 1280 912 nil 27 )! !
!JadeiteMethodListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

