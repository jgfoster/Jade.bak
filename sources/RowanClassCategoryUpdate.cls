"Filed out from Dolphin Smalltalk 7"!

RowanCommandResult subclass: #RowanClassCategoryUpdate
	instanceVariableNames: 'categories selection cache treeModel'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanClassCategoryUpdate guid: (GUID fromString: '{16db26a6-218c-475f-9cfb-039e5613152f}')!
RowanClassCategoryUpdate comment: ''!
!RowanClassCategoryUpdate categoriesForClass!Unclassified! !
!RowanClassCategoryUpdate methodsFor!

newTreeModel
	treeModel := TreeModel new
				searchPolicy: SearchPolicy equality;
				reset;
				add: self root asChildOf: nil;
				yourself.
	^treeModel!

root
	^'--Categories--'!

update: aPresenter
	| selectionNode |
	super update: aPresenter.
	self newTreeModel.
	categories asSortedCollection do: 
			[:each |
			| parent |
			parent := self root.
			(each subStrings: $-) do: 
					[:categoryName |
					parent := treeModel getNodeFor: categoryName
								ifAbsent: [treeModel add: categoryName asChildOf: parent]]].
	aPresenter model: treeModel.
	selectionNode := treeModel getNodeFor: selection ifAbsent: [self root].
	aPresenter selection: selectionNode.
	aPresenter view ensureSelectionVisible! !
!RowanClassCategoryUpdate categoriesFor: #newTreeModel!private! !
!RowanClassCategoryUpdate categoriesFor: #root!private! !
!RowanClassCategoryUpdate categoriesFor: #update:!public!updating! !

