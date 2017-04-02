"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #JadeUserPreferencesPresenter
	instanceVariableNames: 'normalExplorerPresenter treeExplorerPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeUserPreferencesPresenter guid: (GUID fromString: '{86fba099-1250-466b-9269-383895be897c}')!
JadeUserPreferencesPresenter comment: ''!
!JadeUserPreferencesPresenter categoriesForClass!Unclassified! !
!JadeUserPreferencesPresenter methodsFor!

createComponents

	super createComponents.

	normalExplorerPresenter := self add: BooleanPresenter new name: 'option1'.
	treeExplorerPresenter := self add: BooleanPresenter new name: 'option2'.
!

createSchematicWiring

	normalExplorerPresenter when: #valueChanged send: #onOption1Changed to: self.

	treeExplorerPresenter when: #valueChanged send: #onOption2Changed to: self.!

onOption1Changed

	normalExplorerPresenter value ifTrue: [^JadeMethodListPresenter setListView].

	^JadeMethodListPresenter setTreeView!

onOption2Changed

	treeExplorerPresenter value ifTrue: [^JadeMethodListPresenter setTreeView]!

onViewOpened

	super onViewOpened.

	normalExplorerPresenter model: JadeMethodListPresenter isTreeBrowser not asValue.
	treeExplorerPresenter model: JadeMethodListPresenter isTreeBrowser asValue.! !
!JadeUserPreferencesPresenter categoriesFor: #createComponents!public! !
!JadeUserPreferencesPresenter categoriesFor: #createSchematicWiring!public! !
!JadeUserPreferencesPresenter categoriesFor: #onOption1Changed!public! !
!JadeUserPreferencesPresenter categoriesFor: #onOption2Changed!public! !
!JadeUserPreferencesPresenter categoriesFor: #onViewOpened!public! !

!JadeUserPreferencesPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 0 0 34 2 8 1140850688 131073 416 0 721158 ##(Smalltalk.SystemColor) 31 0 7 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 560 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 2719 21 706 591 261 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 79 5 0 0 10 0 0 0 118 6 0 0 140 0 0 0] 34 1 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 816 0 0 0 7 0 0 0 816 0 170 192 560 0 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 591 261 816 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 39 1 0 0 130 0 0 0] 34 2 410 ##(Smalltalk.GroupBox) 34 14 0 816 34 2 8 1140850695 65 1056 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 7 0 0 0 1056 0 8 4294904183 594 138 144 34 2 658 #createAt:extent: 34 2 706 21 31 706 511 191 1056 658 #text: 34 1 8 'Method Browser Type' 1056 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 15 0 0 0 9 1 0 0 110 0 0 0] 8 #() 706 193 193 0 27 410 ##(Smalltalk.ContainerView) 34 15 0 816 34 2 8 1409286144 131073 1408 0 1122 8 4278190080 0 7 0 0 0 1408 656390 ##(Smalltalk.GridLayout) 1 3 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 0 1408 34 2 8 1140924425 1 1568 721990 2 ##(Smalltalk.ValueHolder) 0 0 1376774 ##(Smalltalk.PluggableSearchPolicy) 459270 ##(Smalltalk.Message) #= 8 #() 1698 #hash 8 #() false 1122 1488 0 7 0 0 0 1568 0 8 4294904183 852486 ##(Smalltalk.NullConverter) 0 0 0 594 138 144 34 2 658 #createAt:extent: 34 2 706 1 71 706 441 71 1568 658 #text: 34 1 8 'Tree Method Explorer' 1568 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 35 0 0 0 220 0 0 0 70 0 0 0] 8 #() 1392 0 27 8 'option2' 410 ##(Smalltalk.RadioButton) 34 16 0 1408 34 2 8 1141055497 1 2064 1634 0 0 1666 1698 #= 8 #() 1698 #hash 8 #() false 1122 1488 0 7 0 0 0 2064 0 8 4294904183 1810 0 0 0 594 138 144 34 2 658 #createAt:extent: 34 2 706 1 1 706 441 71 2064 658 #text: 34 1 8 'Normal Method Explorer' 2064 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 35 0 0 0] 8 #() 1392 0 27 8 'option1' 0 594 138 144 34 1 658 #createAt:extent: 34 2 706 51 71 706 441 141 1408 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 0 0 0 35 0 0 0 245 0 0 0 105 0 0 0] 34 2 2064 1568 1392 0 27 1392 0 27 1392 0 27 )! !
!JadeUserPreferencesPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

