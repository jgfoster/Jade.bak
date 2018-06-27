"Filed out from Dolphin Smalltalk 7"!

RowanClassesUpdate subclass: #RowanMethodsUpdate
	instanceVariableNames: 'methods methodFilters selectors selectedClass selectedSelectors inheritedClasses inheritedClass type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodsUpdate guid: (GUID fromString: '{253447c0-f306-48b9-87c4-7bfa4516f3b3}')!
RowanMethodsUpdate comment: ''!
!RowanMethodsUpdate categoriesForClass!Unclassified! !
!RowanMethodsUpdate methodsFor!

classesUpdate: presenter!

inheritedClass
	^inheritedClass!

inheritedClass: anObject
	inheritedClass := anObject!

inheritedClasses
	^inheritedClasses!

inheritedClasses: anObject
	inheritedClasses := anObject!

methodFilters
	^methodFilters!

methodFilters: anObject
	methodFilters := anObject!

methodFilterUpdate: presenter
	presenter list: methodFilters asArray reverse.
	!

methods
	^methods!

methods: anObject
	methods := anObject!

methodsUpdate: presenter
	| methodList |
	methodList := OrderedCollection new.
	methods do: 
			[:update |
			| displayArray |
			displayArray := Array new: 5.
			displayArray 
				at: 1 put: update selector asString;
				at: 2 put: update isOverride;
				at: 3 put: update isTestClass;
				at: 4 put: false; "unused"
				at: 5 put: update selector asString. 
			methodList add: displayArray].
	presenter list: methodList!

selectedClass
	^selectedClass!

selectedClass: anObject
	selectedClass := anObject!

selectedSelectors
	^selectedSelectors!

selectedSelectors: anObject
	selectedSelectors := anObject!

selectors
	^selectors!

selectors: anObject
	selectors := anObject!

type
	^type!

type: anObject
	type := anObject!

update: aPresenter

	aPresenter updateMethodFilterFrom: self. 
	aPresenter updateMethodsFrom: self. !

updateClassSelectionFor: presenter!

updateMethodFilterSelectionFor: presenter
	Transcript cr; show: selectionNames printString. 
	presenter selections: selectionNames.
! !
!RowanMethodsUpdate categoriesFor: #classesUpdate:!public!updating! !
!RowanMethodsUpdate categoriesFor: #inheritedClass!accessing!public! !
!RowanMethodsUpdate categoriesFor: #inheritedClass:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #inheritedClasses!accessing!public! !
!RowanMethodsUpdate categoriesFor: #inheritedClasses:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #methodFilters!accessing!public! !
!RowanMethodsUpdate categoriesFor: #methodFilters:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #methodFilterUpdate:!public!updating! !
!RowanMethodsUpdate categoriesFor: #methods!accessing!public! !
!RowanMethodsUpdate categoriesFor: #methods:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #methodsUpdate:!public!updating! !
!RowanMethodsUpdate categoriesFor: #selectedClass!accessing!public! !
!RowanMethodsUpdate categoriesFor: #selectedClass:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #selectedSelectors!accessing!public! !
!RowanMethodsUpdate categoriesFor: #selectedSelectors:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #selectors!accessing!public! !
!RowanMethodsUpdate categoriesFor: #selectors:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #type!accessing!private! !
!RowanMethodsUpdate categoriesFor: #type:!accessing!private! !
!RowanMethodsUpdate categoriesFor: #update:!public!updating! !
!RowanMethodsUpdate categoriesFor: #updateClassSelectionFor:!public!updating! !
!RowanMethodsUpdate categoriesFor: #updateMethodFilterSelectionFor:!public!updating! !

