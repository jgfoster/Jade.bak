"Filed out from Dolphin Smalltalk 7"!

Object subclass: #MCDefinition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCDefinition guid: (GUID fromString: '{055475ac-74a4-4268-bbf9-805e3c53183b}')!
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

