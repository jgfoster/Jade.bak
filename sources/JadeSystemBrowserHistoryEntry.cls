"Filed out from Dolphin Smalltalk 7"!

JadeAbstractHistoryEntry subclass: #JadeSystemBrowserHistoryEntry
	instanceVariableNames: 'isPackageSelected isHierarchySelected isClassSideSelected isCategoriesSelected packageOrDictionary categoriesOrVariables'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSystemBrowserHistoryEntry guid: (GUID fromString: '{C38CEE7E-DEE2-4387-AF72-86B7D7378670}')!
JadeSystemBrowserHistoryEntry comment: ''!
!JadeSystemBrowserHistoryEntry categoriesForClass!Unclassified! !
!JadeSystemBrowserHistoryEntry methodsFor!

= aJadeHistoryEntry

	^self getId = aJadeHistoryEntry getId!

categoriesOrVariables
	^categoriesOrVariables!

categoriesOrVariables: anObject
	categoriesOrVariables := anObject!

getId

	^self getClassName, '.',self getMethodName!

initialize

	super initialize.

	isPackageSelected := true.
	isHierarchySelected := true.
	isClassSideSelected := false.
	isCategoriesSelected := true.
!

isCategoriesSelected
	^isCategoriesSelected!

isCategoriesSelected: anObject
	isCategoriesSelected := anObject!

isClassSideSelected
	^isClassSideSelected!

isClassSideSelected: anObject
	isClassSideSelected := anObject!

isHierarchySelected
	^isHierarchySelected!

isHierarchySelected: anObject
	isHierarchySelected := anObject!

isPackageSelected
	^isPackageSelected!

isPackageSelected: anObject
	isPackageSelected := anObject!

packageOrDictionary
	^packageOrDictionary!

packageOrDictionary: anObject
	packageOrDictionary := anObject!

populateFrom: aJadeAutoSystemBrowserPresenter

	self isPackageSelected: aJadeAutoSystemBrowserPresenter isPackageListTabSelected.
	self isHierarchySelected: aJadeAutoSystemBrowserPresenter isClassListTabSelected not.
	self isClassSideSelected: aJadeAutoSystemBrowserPresenter isClassSideSelected.
	self isCategoriesSelected: aJadeAutoSystemBrowserPresenter isCategoriesTabSelected.

	packageOrDictionary := isPackageSelected ifTrue: [aJadeAutoSystemBrowserPresenter packageListPresenter selections last] 
						ifFalse: [aJadeAutoSystemBrowserPresenter dictionaryListPresenter selections last].

	classCategory := aJadeAutoSystemBrowserPresenter classCategoryPresenter selectionOrNil. 

	className := isHierarchySelected ifTrue: [aJadeAutoSystemBrowserPresenter classHierarchyPresenter selection] 
					ifFalse: [aJadeAutoSystemBrowserPresenter classListPresenter selection].

	categoriesOrVariables := isCategoriesSelected ifTrue: [aJadeAutoSystemBrowserPresenter categoryListPresenter selectionOrNil ifNotNil: [:value | value allButFirst]] 
						ifFalse: [aJadeAutoSystemBrowserPresenter variableListPresenter selectionOrNil].

	methodName := aJadeAutoSystemBrowserPresenter methodListPresenter selection
! !
!JadeSystemBrowserHistoryEntry categoriesFor: #=!public! !
!JadeSystemBrowserHistoryEntry categoriesFor: #categoriesOrVariables!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #categoriesOrVariables:!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #getId!public! !
!JadeSystemBrowserHistoryEntry categoriesFor: #initialize!accessing!public! !
!JadeSystemBrowserHistoryEntry categoriesFor: #isCategoriesSelected!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #isCategoriesSelected:!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #isClassSideSelected!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #isClassSideSelected:!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #isHierarchySelected!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #isHierarchySelected:!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #isPackageSelected!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #isPackageSelected:!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #packageOrDictionary!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #packageOrDictionary:!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #populateFrom:!public! !

!JadeSystemBrowserHistoryEntry class methodsFor!

newFrom: aJadeAutoSystemBrowserPresenter

	^super new initialize
		populateFrom: aJadeAutoSystemBrowserPresenter;
		yourself! !
!JadeSystemBrowserHistoryEntry class categoriesFor: #newFrom:!public! !

