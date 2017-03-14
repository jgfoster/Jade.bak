"Filed out from Dolphin Smalltalk 7"!

DWORD subclass: #OopType32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OopType32 guid: (GUID fromString: '{5e753c2a-33e0-438c-8b48-549e19ccc28e}')!
OopType32 comment: ''!
!OopType32 categoriesForClass!Unclassified! !
!OopType32 methodsFor!

is64BitBoolean

	^(self value bitAnd: 16r3F) = 16r0C.
!

is64BitCharacter

	^(self value bitAnd: 16rFFFFC01F) = 16r1C.

!

is64BitSmallInteger

	^(self value bitAnd: 3) = 2.

!

is6xBoolean

	^(self value bitAnd: 16r1F) = 16r06.
!

is6xCharacter

	^(self value bitAnd: (16rFFFFE000 bitOr: 16r1F)) = 16r0E.

!

is6xSmallInteger

	^(self value bitAnd: 3) = 3.

!

isGsNil

	^self  value = 10.
!

isSmallDouble

	^false.
!

printOn: aStream

	aStream nextPutAll: 'Oop(' , self value printString , ')'.
! !
!OopType32 categoriesFor: #is64BitBoolean!public! !
!OopType32 categoriesFor: #is64BitCharacter!public! !
!OopType32 categoriesFor: #is64BitSmallInteger!public! !
!OopType32 categoriesFor: #is6xBoolean!public! !
!OopType32 categoriesFor: #is6xCharacter!public! !
!OopType32 categoriesFor: #is6xSmallInteger!public! !
!OopType32 categoriesFor: #isGsNil!public! !
!OopType32 categoriesFor: #isSmallDouble!public! !
!OopType32 categoriesFor: #printOn:!public! !

!OopType32 class methodsFor!

elementSize

	^4.
! !
!OopType32 class categoriesFor: #elementSize!public! !

