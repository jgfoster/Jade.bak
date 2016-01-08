"Filed out from Dolphin Smalltalk 7"!

MCPatchOperation subclass: #MCRemoval
	instanceVariableNames: 'definition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCRemoval guid: (GUID fromString: '{BA962C07-8639-4E40-89AA-6CE4EB10AB50}')!
MCRemoval comment: ''!
!MCRemoval categoriesForClass!Unclassified! !
!MCRemoval methodsFor!

className

	^definition className.
!

detailsString

	^definition detailsString.
!

initialize: aStream

	definition := MCDefinition fromStream: aStream.
!

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
!MCRemoval categoriesFor: #initialize:!public! !
!MCRemoval categoriesFor: #obsoletionMemo!public! !
!MCRemoval categoriesFor: #obsoletionText!public! !
!MCRemoval categoriesFor: #selector!public! !

