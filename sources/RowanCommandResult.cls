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

dictionaryUpdate: aPresenter
	!

doNothing: anObject

	"as the name implies"!

globalsUpdate: aPresenter!

update: aPresenter
	aPresenter ensureVisible!

updateMethod

	^#doNothing:! !
!RowanCommandResult categoriesFor: #dictionaryUpdate:!public!updating! !
!RowanCommandResult categoriesFor: #doNothing:!private! !
!RowanCommandResult categoriesFor: #globalsUpdate:!public!updating! !
!RowanCommandResult categoriesFor: #update:!public!updating! !
!RowanCommandResult categoriesFor: #updateMethod!public! !

