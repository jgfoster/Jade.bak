"Filed out from Dolphin Smalltalk 7"!

MCPatchOperation subclass: #MCRemoval
	instanceVariableNames: 'definition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCRemoval guid: (GUID fromString: '{ba962c07-8639-4e40-89aa-6ce4eb10ab50}')!
MCRemoval comment: ''!
!MCRemoval categoriesForClass!Unclassified! !
!MCRemoval methodsFor!

className

	^definition className.
!

detailsString

	^definition detailsString.
!

isRemoval

	^true!

obsoletionMemo

	^definition displayMemo.
!

obsoletionText

	^definition displayText.
!

selector

	^definition selector.
! !
!MCRemoval categoriesFor: #className!public! !
!MCRemoval categoriesFor: #detailsString!public! !
!MCRemoval categoriesFor: #isRemoval!public! !
!MCRemoval categoriesFor: #obsoletionMemo!public! !
!MCRemoval categoriesFor: #obsoletionText!public! !
!MCRemoval categoriesFor: #selector!public! !

