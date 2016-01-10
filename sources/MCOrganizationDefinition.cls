"Filed out from Dolphin Smalltalk 7"!

MCDefinition subclass: #MCOrganizationDefinition
	instanceVariableNames: 'categories'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCOrganizationDefinition guid: (GUID fromString: '{80167200-683E-4723-81D0-1471BCA0142B}')!
MCOrganizationDefinition comment: ''!
!MCOrganizationDefinition categoriesForClass!Unclassified! !
!MCOrganizationDefinition methodsFor!

className

	^''.
!

detailsString

	^'Class Categories'.
!

displayMemo

	| stream |
	stream := WriteStream on: String new.
	categories do: [:each | 
		stream nextPutAll: each; cr.
	].
	^stream contents.
!

displayText

	^'Class Categories'.
!

initialize: aStream

	categories := aStream nextLine subStrings: Character tab.
! !
!MCOrganizationDefinition categoriesFor: #className!public! !
!MCOrganizationDefinition categoriesFor: #detailsString!public! !
!MCOrganizationDefinition categoriesFor: #displayMemo!public! !
!MCOrganizationDefinition categoriesFor: #displayText!public! !
!MCOrganizationDefinition categoriesFor: #initialize:!public! !

