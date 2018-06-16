"Filed out from Dolphin Smalltalk 7"!

RowanClassesUpdate subclass: #RowanClassHierarchyUpdate
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanClassHierarchyUpdate guid: (GUID fromString: '{0aaf509a-f34e-453b-bdf7-0dbbd42b0d3e}')!
RowanClassHierarchyUpdate comment: ''!
!RowanClassHierarchyUpdate categoriesForClass!Unclassified! !
!RowanClassHierarchyUpdate methodsFor!

classesHierarchyUpdate: presenter

	| list paths treeModel x y  |
	list := classNames.
	presenter ensureVisible.
	list := list collect: [:each | each reverse].
	paths := Set new.
	list do: [:eachClass | 
		| string |
		string := ''.
		eachClass do: [:each | 
			string := string , each.
			paths add: string.
			string := string , Character tab asString.
		].
	].
	paths := paths asSortedCollection asArray.
	treeModel := TreeModel new
		searchPolicy: SearchPolicy equality;
		reset;
		yourself.
	paths do: [:each | 
		| path parent |
		path := each subStrings: Character tab.
		parent := path copyFrom: 1 to: path size - 1.
		parent isEmpty ifTrue: [parent := nil].
		treeModel add: path asChildOf: parent.
	].
	x := (treeModel asBag collect: [:each | each printString]) asSortedCollection asArray.
	y := (presenter model asBag collect: [:each | each printString]) asSortedCollection asArray.
	x = y ifFalse: [
		presenter
			model: treeModel;
			yourself.
	].!

classesUpdate: presenter
!

updateClassHierarchySelectionFor: presenter
	| newSelection |
	self rowanFixMe.	"Need to incorporate test case classes into the display See #updateClassHierarchy"
	presenter selectionOrNil: nil.
	newSelection := presenter model asBag asArray detect: [:each | each last = selectionNames first]
				ifNone: [nil].
	newSelection
		ifNotNil: 
			[presenter
				selection: newSelection;
				expand: newSelection;
				yourself]!

updateClassSelectionFor: presenter

	! !
!RowanClassHierarchyUpdate categoriesFor: #classesHierarchyUpdate:!public!updating! !
!RowanClassHierarchyUpdate categoriesFor: #classesUpdate:!public!updating! !
!RowanClassHierarchyUpdate categoriesFor: #updateClassHierarchySelectionFor:!public!updating! !
!RowanClassHierarchyUpdate categoriesFor: #updateClassSelectionFor:!public!updating! !

