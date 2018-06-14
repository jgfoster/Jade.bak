"Filed out from Dolphin Smalltalk 7"!

RowanCommandResult subclass: #RowanClassCategoryUpdate
	instanceVariableNames: 'categories selection treeModel'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanClassCategoryUpdate guid: (GUID fromString: '{16db26a6-218c-475f-9cfb-039e5613152f}')!
RowanClassCategoryUpdate comment: ''!
!RowanClassCategoryUpdate categoriesForClass!Unclassified! !
!RowanClassCategoryUpdate methodsFor!

classCategoryUpdate: aPresenter
	| cache root |
	self newTreeModel.
	root := treeModel roots first.
	cache := Dictionary new
				at: root key put: root;
				yourself.
	categories asSortedCollection do: 
			[:each |
			| array |
			array := each subStrings: $-.
			1 to: array size
				do: 
					[:i |
					| childName parentName child parent |
					childName := array copyFrom: 1 to: i.
					parentName := array copyFrom: 1 to: i - 1.
					parent := cache at: parentName.
					(cache includesKey: childName)
						ifFalse: 
							[cache at: childName put: (child := childName -> each).
							treeModel add: child asChildOf: parent]]].
	aPresenter model: treeModel!

newTreeModel
	treeModel := TreeModel new
				searchPolicy: SearchPolicy equality;
				reset;
				add: self root asChildOf: nil;
				yourself.
	^treeModel!

root
	^#() -> '--Categories--'!

updateClassCategorySelectionsFor: presenter
	| newSelection |
	newSelection := selection isEmpty
				ifTrue: [treeModel roots first key]
				ifFalse: [selection subStrings: $-].
	(presenter model asBag asArray select: [:item | item key size >= newSelection size]) do: 
			[:assoc |
			(assoc key first: newSelection size) = newSelection ifTrue: [presenter expand: assoc].
			assoc key = newSelection ifTrue: [presenter selection: assoc]].
	presenter view ensureSelectionVisible! !
!RowanClassCategoryUpdate categoriesFor: #classCategoryUpdate:!public!updating! !
!RowanClassCategoryUpdate categoriesFor: #newTreeModel!private! !
!RowanClassCategoryUpdate categoriesFor: #root!private! !
!RowanClassCategoryUpdate categoriesFor: #updateClassCategorySelectionsFor:!public!updating! !

