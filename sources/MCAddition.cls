"Filed out from Dolphin Smalltalk 7"!

MCPatchOperation subclass: #MCAddition
	instanceVariableNames: 'definition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCAddition guid: (GUID fromString: '{B78CB54F-D86C-4398-A734-2C5CFA934104}')!
MCAddition comment: ''!
!MCAddition categoriesForClass!Unclassified! !
!MCAddition methodsFor!

className

	^definition className.
!

detailsString

	^definition detailsString.
!

initialize: aStream

	definition := MCDefinition fromStream: aStream.
!

modificationMemo

	^definition displayMemo.
!

modificationText

	^definition displayText.
!

selector

	^definition selector.
! !
!MCAddition categoriesFor: #className!public! !
!MCAddition categoriesFor: #detailsString!public! !
!MCAddition categoriesFor: #initialize:!public! !
!MCAddition categoriesFor: #modificationMemo!public! !
!MCAddition categoriesFor: #modificationText!public! !
!MCAddition categoriesFor: #selector!public! !

