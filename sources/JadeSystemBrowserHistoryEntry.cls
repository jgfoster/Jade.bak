"Filed out from Dolphin Smalltalk 7"!

JadeAbstractHistoryEntry subclass: #JadeSystemBrowserHistoryEntry
	instanceVariableNames: 'isPackageSelected isHierarchySelected isClassSideSelected isCategoriesSelected packageOrDictionary categoriesOrVariables'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSystemBrowserHistoryEntry guid: (GUID fromString: '{0e2310b4-f7e0-4c25-baa6-b5d3cea0738e}')!
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

	^self getClassName, '.',self getMethodName
!

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

	self packageOrDictionary: (isPackageSelected 
						ifTrue: [aJadeAutoSystemBrowserPresenter packageListPresenter selections] 
						ifFalse: [aJadeAutoSystemBrowserPresenter dictionaryListPresenter selections]).

	self classCategory: aJadeAutoSystemBrowserPresenter classCategoryPresenter selectionOrNil. 

	self className: (isHierarchySelected 
				ifTrue: [aJadeAutoSystemBrowserPresenter classHierarchyPresenter selectionOrNil ] 
				ifFalse: [aJadeAutoSystemBrowserPresenter classListPresenter selectionOrNil ]).

	self categoriesOrVariables: (isCategoriesSelected 
						ifTrue: [aJadeAutoSystemBrowserPresenter categoryListPresenter selectionOrNil ifNotNil: [:value | value allButFirst]] 
						ifFalse: [aJadeAutoSystemBrowserPresenter variableListPresenter selectionOrNil]).

	self methodName: aJadeAutoSystemBrowserPresenter methodListPresenter selectionOrNil.
! !
!JadeSystemBrowserHistoryEntry categoriesFor: #=!public! !
!JadeSystemBrowserHistoryEntry categoriesFor: #categoriesOrVariables!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #categoriesOrVariables:!accessing!private! !
!JadeSystemBrowserHistoryEntry categoriesFor: #getId!public! !
!JadeSystemBrowserHistoryEntry categoriesFor: #initialize!public! !
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

