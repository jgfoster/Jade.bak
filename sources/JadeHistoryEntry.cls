"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadeHistoryEntry
	instanceVariableNames: 'isPackageSelected isHierarchySelected isClassSideSelected isCategoriesSelected packageOrDictionary classCategory className categoriesOrVariables methodName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeHistoryEntry guid: (GUID fromString: '{C38CEE7E-DEE2-4387-AF72-86B7D7378670}')!
JadeHistoryEntry comment: ''!
!JadeHistoryEntry categoriesForClass!Unclassified! !
!JadeHistoryEntry methodsFor!

= aJadeHistoryEntry

	^self getId = aJadeHistoryEntry getId!

categoriesOrVariables
	^categoriesOrVariables!

categoriesOrVariables: anObject
	categoriesOrVariables := anObject!

classCategory
	^classCategory!

classCategory: anObject
	classCategory := anObject!

className
	^className!

className: anObject
	className := anObject!

getClassName
	"<className> is anArray with all hierarchy names"

	(className isKindOf: String) ifTrue: [^className].

	(className isNil or: [className isEmpty]) ifTrue: [^''].

	^className last!

getId

	^self getClassName, '.',self getMethodName!

getMethodName
	"<methodName> is anArray with different GemStone values"

	(methodName isKindOf: String) ifTrue: [^methodName].

	(methodName isNil or: [methodName isEmpty]) ifTrue: [^''].

	^methodName first!

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

methodName
	^methodName!

methodName: anObject
	methodName := anObject!

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
!JadeHistoryEntry categoriesFor: #=!public! !
!JadeHistoryEntry categoriesFor: #categoriesOrVariables!accessing!private! !
!JadeHistoryEntry categoriesFor: #categoriesOrVariables:!accessing!private! !
!JadeHistoryEntry categoriesFor: #classCategory!accessing!private! !
!JadeHistoryEntry categoriesFor: #classCategory:!accessing!private! !
!JadeHistoryEntry categoriesFor: #className!accessing!private! !
!JadeHistoryEntry categoriesFor: #className:!accessing!private! !
!JadeHistoryEntry categoriesFor: #getClassName!public! !
!JadeHistoryEntry categoriesFor: #getId!public! !
!JadeHistoryEntry categoriesFor: #getMethodName!public! !
!JadeHistoryEntry categoriesFor: #initialize!accessing!public! !
!JadeHistoryEntry categoriesFor: #isCategoriesSelected!accessing!private! !
!JadeHistoryEntry categoriesFor: #isCategoriesSelected:!accessing!private! !
!JadeHistoryEntry categoriesFor: #isClassSideSelected!accessing!private! !
!JadeHistoryEntry categoriesFor: #isClassSideSelected:!accessing!private! !
!JadeHistoryEntry categoriesFor: #isHierarchySelected!accessing!private! !
!JadeHistoryEntry categoriesFor: #isHierarchySelected:!accessing!private! !
!JadeHistoryEntry categoriesFor: #isPackageSelected!accessing!private! !
!JadeHistoryEntry categoriesFor: #isPackageSelected:!accessing!private! !
!JadeHistoryEntry categoriesFor: #methodName!accessing!private! !
!JadeHistoryEntry categoriesFor: #methodName:!accessing!private! !
!JadeHistoryEntry categoriesFor: #packageOrDictionary!accessing!private! !
!JadeHistoryEntry categoriesFor: #packageOrDictionary:!accessing!private! !
!JadeHistoryEntry categoriesFor: #populateFrom:!public! !

!JadeHistoryEntry class methodsFor!

newFrom: aJadeAutoSystemBrowserPresenter

	^super new initialize
		populateFrom: aJadeAutoSystemBrowserPresenter;
		yourself! !
!JadeHistoryEntry class categoriesFor: #newFrom:!public! !

