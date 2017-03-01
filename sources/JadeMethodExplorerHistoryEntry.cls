"Filed out from Dolphin Smalltalk 7"!

JadeAbstractHistoryEntry subclass: #JadeMethodExplorerHistoryEntry
	instanceVariableNames: 'treeSelection methodCategory gsMethod'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMethodExplorerHistoryEntry guid: (GUID fromString: '{40265F1F-8BAB-4DD7-9CB4-5D69D798C49C}')!
JadeMethodExplorerHistoryEntry comment: ''!
!JadeMethodExplorerHistoryEntry categoriesForClass!Unclassified! !
!JadeMethodExplorerHistoryEntry methodsFor!

gsMethod
	^gsMethod!

gsMethod: anObject
	gsMethod := anObject!

methodCategory
	^methodCategory!

methodCategory: anObject
	methodCategory := anObject!

populateFrom: aJadeTreeMethodListPresenter

	gsMethod := aJadeTreeMethodListPresenter selectedMethod.

	methodName := gsMethod name.
	className := gsMethod gsClass name.
	classCategory := gsMethod gsClass category.
	methodCategory := gsMethod category.

	treeSelection := aJadeTreeMethodListPresenter selectedExplorerNode.!

treeSelection
	^treeSelection!

treeSelection: anObject
	treeSelection := anObject! !
!JadeMethodExplorerHistoryEntry categoriesFor: #gsMethod!accessing!private! !
!JadeMethodExplorerHistoryEntry categoriesFor: #gsMethod:!accessing!private! !
!JadeMethodExplorerHistoryEntry categoriesFor: #methodCategory!accessing!private! !
!JadeMethodExplorerHistoryEntry categoriesFor: #methodCategory:!accessing!private! !
!JadeMethodExplorerHistoryEntry categoriesFor: #populateFrom:!public! !
!JadeMethodExplorerHistoryEntry categoriesFor: #treeSelection!accessing!private! !
!JadeMethodExplorerHistoryEntry categoriesFor: #treeSelection:!accessing!private! !

!JadeMethodExplorerHistoryEntry class methodsFor!

newFrom: aJadeTreeMethodListPresenter

	^super new initialize
		populateFrom: aJadeTreeMethodListPresenter;
		yourself! !
!JadeMethodExplorerHistoryEntry class categoriesFor: #newFrom:!public! !

