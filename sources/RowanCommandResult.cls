"Filed out from Dolphin Smalltalk 7"!

Object subclass: #RowanCommandResult
	instanceVariableNames: 'executionTime'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanCommandResult guid: (GUID fromString: '{4ed93b8d-f706-49f3-b224-97cd3ebf739c}')!
RowanCommandResult comment: ''!
!RowanCommandResult categoriesForClass!Unclassified! !
!RowanCommandResult methodsFor!

classCategoryUpdate: aPresenter
	!

dictionaryUpdate: aPresenter
	!

doNothing: anObject

	"as the name implies"!

globalsUpdate: aPresenter!

packagesUpdate: aPresenter!

update: aPresenter
	aPresenter ensureVisible!

updateClassCategorySelectionsFor: presenter!

updateDictionarySelectionsFor: presenter
	!

updateMethod

	^#doNothing:!

updatePackageSelectionsFor: presenter
	! !
!RowanCommandResult categoriesFor: #classCategoryUpdate:!public!updating! !
!RowanCommandResult categoriesFor: #dictionaryUpdate:!public!updating! !
!RowanCommandResult categoriesFor: #doNothing:!private! !
!RowanCommandResult categoriesFor: #globalsUpdate:!public!updating! !
!RowanCommandResult categoriesFor: #packagesUpdate:!public!updating! !
!RowanCommandResult categoriesFor: #update:!public!updating! !
!RowanCommandResult categoriesFor: #updateClassCategorySelectionsFor:!public!updating! !
!RowanCommandResult categoriesFor: #updateDictionarySelectionsFor:!public!updating! !
!RowanCommandResult categoriesFor: #updateMethod!public! !
!RowanCommandResult categoriesFor: #updatePackageSelectionsFor:!public!updating! !

