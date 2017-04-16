"Filed out from Dolphin Smalltalk 7"!

ULARGE_INTEGER subclass: #OopType64
	instanceVariableNames: 'objectInfo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OopType64 guid: (GUID fromString: '{e5d12262-5936-4c83-b2c9-026c8197fbc9}')!
OopType64 comment: '#define OOP_TAG_RAM_OOP     0x0 /* 2r000  memory pointer ObjSType* */
#define OOP_TAG_POM_OOP     0x1 /* 2r001  disk objId */
  // disk objIds have the form 0x0000nnnnnnnnnn01 , with the 
  //  oopNumber shifted left by 8 before adding the pom tag bit 
#define OOP_TAG_SMALLINT    0x2 /* 2r010  SmallInteger */
#define OOP_TAG_SMALLDOUBLE 0x6 /* 2r110  SmallDouble */
#define OOP_TAG_SPECIAL     0x4 /* 2r100  true,false,nil, Char, JISChar */

#define OOP_NUM_TAG_BITS 3 /* number of tag bits on a disk objId */

#define OOP_POM_TAG_MASK    0x1

#define OOP_TAG_SPECIAL_MASK  0x6    /* any special oop */
'!
!OopType64 categoriesForClass!Unclassified! !
!OopType64 methodsFor!

= anOopType64

	^self class == anOopType64 class and: [self value == anOopType64 value]!

asFraction
	"SmallFractions are special objects that can represent Fractions
	  with   -268435456 <= numerator   <= 268435455
	  and             0 <  denominator <= 134217727 . 

	 A SmallFraction contains the bits
		snnnnnnn|nnnnnnnn|nnnnnnnn|nnnnnddd|dddddddd|dddddddd|dddddddd|tttttttt
	 where bits are shown with least-significant on the right.
	 The bits  sn...n  are a 29 bit signed twos-complement numerator ,
	 The bits  d...d   are a 27 bit unsigned denominator .
	 The 8 tag bits have the constant value  16r2C . "

	| numerator denominator fraction |
	numerator := ((bytes dwordAtOffset: 4) bitAnd: 16rFFFFFFF8) bitShift: -3.
	0 ~~ (numerator bitAnd: 16r10000000) ifTrue: [
		| temp |
		temp := ByteArray new: 4.
		numerator := numerator bitOr: 16rF0000000.
		temp dwordAtOffset: 0 put: numerator.
		numerator := temp sdwordAtOffset: 0.
	].
	denominator := (bytes dwordAtOffset: 1) bitAnd: 16r7FFFFFF.
	fraction := numerator / denominator.
	^fraction
!

asSmallInteger

	| value myBytes |
	value := self value.
	(value anyMask: 16r8000000000000000) ifFalse: [^value >> 3].
	value := value bitOr: 7.
	myBytes := ByteArray new: 8.
	myBytes qwordAtOffset: 0 put: value.
	value := myBytes sqwordAtOffset: 0.
	^value >> 3.
!

isBoolean
"#define OOP_FALSE           ((OopType)0x0C)
#define OOP_TRUE           ((OopType)0x10C) 
"
	^self value = 16r00C or: [self value = 16r10C].!

isCharacter

	^(self value bitAnd: 16rFF) = 16r1C.
!

isGsNil

	^self  value = 20.
!

isImmediate

	^(self value bitAnd: 6) > 0.
!

isOopIllegal

	^self  value = 1.
!

isSmallDouble

	^(self value bitAnd: 7) = 6.
!

isSmallFraction

	^(self value bitAnd: 16rFF) = 16r2C.
!

isSmallInteger

	^(self value bitAnd: 7) = 2.

!

isSpecial
	"true,false,nil, Char, JISChar"

	^(self value bitAnd: 7) = 4.
!

objectInfo
	^objectInfo!

objectInfo: anObject
	objectInfo := anObject!

printOn: aStream

	aStream nextPutAll: 'Oop(' , self value printString , ')'.
!

smallDoubleAsFloat

	| bits sign expn frac |
	bits := (bytes qwordAtOffset: 0) bitAnd: 7 bitInvert.
	bits := bits printStringRadix: 2 showRadix: false.
	[bits size < 64] whileTrue: [bits := '0' , bits].
	expn := ('2r' , (bits copyFrom: 1 to: 8)) asNumber.
	frac := ('2r' , (bits copyFrom: 9 to: 59)) asNumber.
	sign := (bits at: 61) = $1 ifTrue: [-1] ifFalse: [1].
	frac = 0 ifTrue: [
		expn = 0 ifTrue: [^0.0].
		expn = 16rFF ifTrue: [^1.0 / 0.0 * sign].
	].
	^(frac + 16r8000000000000 / 16r8000000000000 * (2 raisedTo: expn - 127) * sign) asFloat.
! !
!OopType64 categoriesFor: #=!public! !
!OopType64 categoriesFor: #asFraction!public! !
!OopType64 categoriesFor: #asSmallInteger!public! !
!OopType64 categoriesFor: #isBoolean!public! !
!OopType64 categoriesFor: #isCharacter!public! !
!OopType64 categoriesFor: #isGsNil!public! !
!OopType64 categoriesFor: #isImmediate!public! !
!OopType64 categoriesFor: #isOopIllegal!public! !
!OopType64 categoriesFor: #isSmallDouble!public! !
!OopType64 categoriesFor: #isSmallFraction!public! !
!OopType64 categoriesFor: #isSmallInteger!public! !
!OopType64 categoriesFor: #isSpecial!public! !
!OopType64 categoriesFor: #objectInfo!accessing!public! !
!OopType64 categoriesFor: #objectInfo:!accessing!public! !
!OopType64 categoriesFor: #printOn:!public! !
!OopType64 categoriesFor: #smallDoubleAsFloat!public! !

