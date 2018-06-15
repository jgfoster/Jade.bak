"Filed out from Dolphin Smalltalk 7"!

RowanClassesUpdate subclass: #RowanMethodsUpdate
	instanceVariableNames: 'methods methodFilters selectors selectedClass selectedSelectors inheritedClasses inheritedClass'
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

methods
	^methods!

methods: anObject
	methods := anObject!

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

update: aPresenter

	aPresenter updateMethodFilterFrom: self. 
	aPresenter updateMethodsFrom: self. !

updateClassSelectionFor: presenter! !
!RowanMethodsUpdate categoriesFor: #classesUpdate:!public!updating! !
!RowanMethodsUpdate categoriesFor: #inheritedClass!accessing!public! !
!RowanMethodsUpdate categoriesFor: #inheritedClass:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #inheritedClasses!accessing!public! !
!RowanMethodsUpdate categoriesFor: #inheritedClasses:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #methodFilters!accessing!public! !
!RowanMethodsUpdate categoriesFor: #methodFilters:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #methods!accessing!public! !
!RowanMethodsUpdate categoriesFor: #methods:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #selectedClass!accessing!public! !
!RowanMethodsUpdate categoriesFor: #selectedClass:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #selectedSelectors!accessing!public! !
!RowanMethodsUpdate categoriesFor: #selectedSelectors:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #selectors!accessing!public! !
!RowanMethodsUpdate categoriesFor: #selectors:!accessing!public! !
!RowanMethodsUpdate categoriesFor: #update:!public! !
!RowanMethodsUpdate categoriesFor: #updateClassSelectionFor:!public!updating! !

