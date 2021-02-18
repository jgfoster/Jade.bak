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

	^self class == anOopType64 class and: [
		self value = anOopType64 value]	"Don't use == since OopType might not be a SmallInteger"
!

asDate
	"Delegate is assumed to represent a server SmallDate. 
	 Answer the corresponding local Date.

	SmallDate represents instances of Date with year representable in 47 bits
	The bit format of an instance is
	yyyyyyyy|yyyyyyyy|yyyyyyyy|yyyyyyyy|yyyyyyyy|yyyyyyyd|dddddddd|01001100
	bits d 9 bits unsigned integer dayOfYear
	bits y 47 bits signed, holding a 31 bit signed integer year.

	This means that oop byte 1 can be ignored, since it just contains redundant sign bits."

	| myBytes highByte splitByte day year |
	myBytes := bytes reverse.
	highByte := myBytes at: 2.	"If the sign bit is negative, start it with a pre-shifted -1, otherwise with a pre-shifted 0, which is 0"
	splitByte := myBytes at: 6.
	year := ((((highByte ~~ 0 ifTrue: [-16r1] ifFalse: [0]) * 16r100
				+ (myBytes at: 3)) * 16r100
				+ (myBytes at: 4)) * 16r100
				+ (myBytes at: 5)) * 16r80
				+ (splitByte bitShift: -1).
	day := (splitByte bitAnd: 16r1) * 16r100 + (myBytes at: 7).
	^Date newDay: day year: year.
!

asDateAndTime
	"OOP is assumed to represent a server SmallDateAndTime. 
	 Answer the corresponding local DateAndTime.

	A SmallDateAndTime is a DateAndTime whose offset in hours is in the range -16..15
	and whose time is in the range 2001 to 2072 .
	The resolution is 1 microsecond.

	The bit format of an instance is
	tttttttt|tttttttt|tttttttt|tttttttt|tttttttt|tttttttt|tttooooo|00111100
	bits o   5 bits signed  offset  from UTC in hours
	bits t  51 bits unsigned time since 2001 in units of microseconds."

	| myBytes oneCenturyMilliseconds microseconds byte0 offset |
	myBytes := bytes reverse.
	oneCenturyMilliseconds := 3155760000000000.	"100 y x 365.25d/y x 86400 s/d x 1000000 ms/s"
	byte0 := myBytes at: 7.
	offset := byte0 bitAnd: 2r11111.
	"Convert 5 bits two's complement back to signed integer."
	offset > 15 ifTrue: [offset := (offset - 1 bitXor: 2r11111) negated].
	microseconds := ((((((
				(myBytes at: 1)) * 16r100
				+ (myBytes at: 2)) * 16r100
				+ (myBytes at: 3)) * 16r100
				+ (myBytes at: 4)) * 16r100
				+ (myBytes at: 5)) * 16r100
				+ (myBytes at: 6)) * 16r8
				+ (byte0 bitShift: -5).
	^DateAndTime
		utcSeconds: microseconds + oneCenturyMilliseconds / 1000000
		offset: offset * 3600.
!

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

asScaledDecimal
	"Oop is assumed to represent a server SmallScaledDecimal. 
	 Answer the corresponding local ScaledDecimal.

	SmallScaledDecimal is used to represent ScaledDecimals that have 
	scale <= 31 and mantissa in the range -1125899906842624..1125899906842623 .
 	The bit format of an instances is
   	mmmmmmmm|mmmmmmmm|mmmmmmmm|mmmmmmmm|mmmmmmmm|mmmmmmmm|mmmsssss|00110100
	bits m 51 bit signed integer mantissa
	s 5 bit unsigned scale."

	| myBytes highByte lowByte mantissa scale |
	myBytes := bytes reverse.
	highByte := myBytes at: 1.	"If the sign bit is negative, start it with a pre-shifted -1, otherwise with a pre-shifted 0, which is 0"
	lowByte := myBytes at: 7.
	mantissa := ((((((((highByte bitAnd: 16r80) ~~ 0
				ifTrue: [-16r100]
				ifFalse: [0]) + highByte)
				* 16r100 + (myBytes at: 2))
				* 16r100 + (myBytes at: 3))
				* 16r100 + (myBytes at: 4))
				* 16r100 + (myBytes at: 5))
				* 16r100 + (myBytes at: 6))
				* 16r8 + (lowByte bitShift: -5).
	scale := lowByte bitAnd: 16r1F.
	^ScaledDecimal newFromNumber: mantissa / (10 raisedTo: scale) scale: scale
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

asTime
	"OOP is assumed to represent a server SmallTime. 
	 Answer the corresponding local Time.

	The bit format of an instance is
	tttttttt|tttttttt|tttttttt|tttttttt|tttttttt |tttttttt|tttttttt| 01000100
	bits t  56 bits unsigned integer , local time since midnight  , units microseconds
	The maximum value of local time is  86399999999 .

	Thus, the upper two bytes will always be zero and can be ignored here."

	| myBytes microseconds |
	myBytes := bytes reverse.
	microseconds := (((
				(myBytes at: 3) * 16r100
				+ (myBytes at: 4)) * 16r100
				+ (myBytes at: 5)) * 16r100
				+ (myBytes at: 6)) * 16r100
				+ (myBytes at: 7).
	^Time fromSeconds: microseconds / 1000000!

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

isSmallDate

	^(self value bitAnd: 16rFF) = 2r01001100
!

isSmallDateAndTime

	^(self value bitAnd: 16rFF) = 2r00111100
!

isSmallDouble

	^(self value bitAnd: 7) = 6.
!

isSmallFraction

	^(self value bitAnd: 16rFF) = 2r00101100 "16r2C"
!

isSmallInteger

	^(self value bitAnd: 7) = 2.

!

isSmallScaledDecimal

	^(self value bitAnd: 16rFF) = 2r00110100
!

isSmallTime

	^(self value bitAnd: 16rFF) = 2r01000100
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
	| bits sign expn frac bitsStream |

	bits := (bytes qwordAtOffset: 0) bitAnd: 7 bitInvert.
	bits printOn: (bitsStream := String writeStream: 16) base: 2 showRadix: false.
	bits := bitsStream contents.
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
!OopType64 categoriesFor: #asDate!converting!public! !
!OopType64 categoriesFor: #asDateAndTime!converting!public! !
!OopType64 categoriesFor: #asFraction!converting!public! !
!OopType64 categoriesFor: #asScaledDecimal!converting!public! !
!OopType64 categoriesFor: #asSmallInteger!converting!public! !
!OopType64 categoriesFor: #asTime!converting!public! !
!OopType64 categoriesFor: #isBoolean!public!testing! !
!OopType64 categoriesFor: #isCharacter!public!testing! !
!OopType64 categoriesFor: #isGsNil!public!testing! !
!OopType64 categoriesFor: #isImmediate!public!testing! !
!OopType64 categoriesFor: #isOopIllegal!public!testing! !
!OopType64 categoriesFor: #isSmallDate!public!testing! !
!OopType64 categoriesFor: #isSmallDateAndTime!public!testing! !
!OopType64 categoriesFor: #isSmallDouble!public!testing! !
!OopType64 categoriesFor: #isSmallFraction!public!testing! !
!OopType64 categoriesFor: #isSmallInteger!public!testing! !
!OopType64 categoriesFor: #isSmallScaledDecimal!public!testing! !
!OopType64 categoriesFor: #isSmallTime!public!testing! !
!OopType64 categoriesFor: #isSpecial!public!testing! !
!OopType64 categoriesFor: #objectInfo!accessing!public! !
!OopType64 categoriesFor: #objectInfo:!accessing!public! !
!OopType64 categoriesFor: #printOn:!public! !
!OopType64 categoriesFor: #smallDoubleAsFloat!converting!public! !

