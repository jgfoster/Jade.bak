"Filed out from Dolphin Smalltalk 7"!

Object subclass: #MCDefinition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCDefinition guid: (GUID fromString: '{055475AC-74A4-4268-BBF9-805E3C53183B}')!
MCDefinition comment: ''!
!MCDefinition categoriesForClass!Unclassified! !
!MCDefinition methodsFor!

selector

	^''.
! !
!MCDefinition categoriesFor: #selector!public! !

!MCDefinition class methodsFor!

fromStream: aStream

	| char class |
	char := aStream next.
	aStream next.
	class := 
		char = $M ifTrue: [MCMethodDefinition] ifFalse: [
		char = $O ifTrue: [MCOrganizationDefinition] ifFalse: [
		char = $C ifTrue: [MCClassDefinition] ifFalse: [
		self error: 'Invalid MCDefinition']]].
	^class new
		initialize: aStream;
		yourself.
! !
!MCDefinition class categoriesFor: #fromStream:!public! !

