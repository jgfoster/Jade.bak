"Filed out from Dolphin Smalltalk 7"!

RowanCommandResult subclass: #RowanClassesUpdate
	instanceVariableNames: 'type classNames selectionNames'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanClassesUpdate guid: (GUID fromString: '{25ac5151-b39b-4ede-b826-a3d41a790bf4}')!
RowanClassesUpdate comment: ''!
!RowanClassesUpdate categoriesForClass!Unclassified! !
!RowanClassesUpdate methodsFor!

classes
	^classes!

classes: anObject
	classes := anObject!

classesUpdate: presenter

	presenter list: classNames!

selections
	^selections!

selections: anObject
	selections := anObject!

type
	^type!

type: anObject
	type := anObject!

update: aPresenter
	super update: aPresenter.
	type = 'classHierarchy' ifTrue: [^aPresenter updateClassHierarchyFrom: self].
	aPresenter list: classes.
	aPresenter selections: selections.
	aPresenter view ensureSelectionVisible.!

updateClassSelectionFor: presenter
	presenter selections: selectionNames! !
!RowanClassesUpdate categoriesFor: #classes!accessing!private! !
!RowanClassesUpdate categoriesFor: #classes:!accessing!private! !
!RowanClassesUpdate categoriesFor: #classesUpdate:!public!updating! !
!RowanClassesUpdate categoriesFor: #selections!accessing!private! !
!RowanClassesUpdate categoriesFor: #selections:!accessing!private! !
!RowanClassesUpdate categoriesFor: #type!accessing!private! !
!RowanClassesUpdate categoriesFor: #type:!accessing!private! !
!RowanClassesUpdate categoriesFor: #update:!public! !
!RowanClassesUpdate categoriesFor: #updateClassSelectionFor:!public!updating! !

