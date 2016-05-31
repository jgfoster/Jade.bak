"Filed out from Dolphin Smalltalk 7"!

GciSingleThreadedLibrary subclass: #Gcilw6x
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Gcilw6x guid: (GUID fromString: '{903182EE-3A32-4868-8468-77731D1B2FD5}')!
Gcilw6x comment: ''!
!Gcilw6x categoriesForClass!Unclassified! !
!Gcilw6x methodsFor!

errorStructureClass

	^GciErrSType32.
!

gciClearStack: anOopType

	<cdecl: void GciClearStack OopType32>
	^self invalidCall
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType32*>
	^self invalidCall

!

gciFetchBytes: anOopType _: startIndex _: cString _: maxSize

	<cdecl: sdword GciFetchBytes OopType32 sdword lpvoid dword>
	^self invalidCall
!

gciFetchChars: anOopType _: startIndex _: cString _: maxSize

	<cdecl: sdword GciFetchChars OopType32 sdword lpstr dword>
	^self invalidCall
!

gciFetchClass: oop

	<cdecl: OopType32 GciFetchClass OopType32>
	^self invalidCall
!

gciFetchObjImpl: anObject

	<cdecl: sdword GciFetchObjImpl OopType32>
	^self invalidCall
!

gciFetchSize: anObject

	<cdecl: sdword GciFetchSize OopType32>
	^self invalidCall
!

gciFetchVaryingOops: anObject _: startIndex _: theOops _: numOops

	<cdecl: sdword GciFetchVaryingOops OopType32 sdword OopType32Array* sdword>
	^self invalidCall
!

gciLongToOop: anInteger

	<cdecl: OopType32 GciLongToOop sdword>
	^self invalidCall
!

gciNbContinueWith: process _: replaceTopOfStack _: flags _: error

	<cdecl: void GciNbContinueWith OopType32 OopType32 sdword GciErrSType32*>
	^self invalidCall
!

gciNbExecuteStrFromContext: string _: context _: symbolList

	<cdecl: OopType32 GciNbExecuteStrFromContext lpstr OopType32 OopType32>
	^self invalidCall
!

gciNbPerform: receiver _: selector _: args _: numArgs

	<cdecl: void GciNbPerform OopType32 lpstr OopType32* dword>
	^self invalidCall
!

gciNewString: aString

	<cdecl: OopType32 GciNewString lpstr >
	^self invalidCall
!

gciOopToChr: anObject

	<cdecl: sdword GciOopToChr OopType32>
	^self invalidCall
!

gciReleaseOops: args _: numArgs

	<cdecl: void GciReleaseOops OopType32* dword>
	^self invalidCall
!

oopAsciiNul

	^OopType32 fromInteger: 14.!

oopAt: anExternalAddress

	^OopType32 fromInteger: (anExternalAddress sdwordAtOffset: 0).
!

oopClassArray

	^OopType32 fromInteger: 1045.
!

oopClassByteArray

	^OopType32 fromInteger: 1617.!

oopClassDoubleByteString

	^OopType32 fromInteger: 0.!

oopClassQuadByteString

	^OopType32 fromInteger: 0.!

oopClassString

	^OopType32 fromInteger: 1169.!

oopClassSymbol

	^OopType32 fromInteger: 1733.
!

oopClassSystem

	^OopType32 fromInteger: 1189.!

oopClassUnicode16

	^OopType32 fromInteger: 0.!

oopClassUnicode32

	^OopType32 fromInteger: 0.!

oopClassUnicode7

	^OopType32 fromInteger: 0.!

oopFalse

	^OopType32 fromInteger: 6.!

oopForInteger: anInteger

	| int bytes |
	bytes := ByteArray new: 4.
	bytes 
		dwordAtOffset: 0 
		put: anInteger.
	int := bytes sdwordAtOffset: 0.
	^self gciLongToOop: int.
!

oopGemStoneError

	^OopType32 fromInteger: 3613.!

oopIllegal

	^OopType32 fromInteger: 1.!

oopMaxSmallInteger

	^OopType32 fromInteger: 16r7FFFFFFF.!

oopMinSmallInteger

	^OopType32 fromInteger: -16r7FFFFFFF.!

oopMinusOne

	^OopType32 fromInteger: -1.!

oopNil

	^OopType32 fromInteger: 10.	"0xA"!

oopOne

	^OopType32 fromInteger: 7.!

oopRemoteNil

	^OopType32 fromInteger: 42.!

oopTrue

	^OopType32 fromInteger: 38.!

oopTwo

	^OopType32 fromInteger: 11.!

oopTypeArrayClass

	^OopType32Array.!

oopTypeClass

	^OopType32.!

oopTypeWithOop: anInteger

	| int bytes |
	bytes := ByteArray new: 4.
	bytes 
		dwordAtOffset: 0 
		put: anInteger.
	int := bytes sdwordAtOffset: 0.
	^OopType32 fromInteger: int.
!

oopZero

	^OopType32 fromInteger: 3.!

specialFromOop: anOop

	anOop is6xBoolean ifTrue: [
		^anOop = self oopTrue.
	].
	anOop is6xCharacter ifTrue: [
		^Character value: (self gciOopToChr: anOop).
	].
	anOop is6xSmallInteger ifTrue: [
		^anOop value // 4.
	].
	^nil.
! !
!Gcilw6x categoriesFor: #errorStructureClass!private! !
!Gcilw6x categoriesFor: #gciClearStack:!private! !
!Gcilw6x categoriesFor: #gciErr:!private! !
!Gcilw6x categoriesFor: #gciFetchBytes:_:_:_:!private! !
!Gcilw6x categoriesFor: #gciFetchChars:_:_:_:!private! !
!Gcilw6x categoriesFor: #gciFetchClass:!private! !
!Gcilw6x categoriesFor: #gciFetchObjImpl:!private! !
!Gcilw6x categoriesFor: #gciFetchSize:!private! !
!Gcilw6x categoriesFor: #gciFetchVaryingOops:_:_:_:!private! !
!Gcilw6x categoriesFor: #gciLongToOop:!private! !
!Gcilw6x categoriesFor: #gciNbContinueWith:_:_:_:!private! !
!Gcilw6x categoriesFor: #gciNbExecuteStrFromContext:_:_:!private! !
!Gcilw6x categoriesFor: #gciNbPerform:_:_:_:!private! !
!Gcilw6x categoriesFor: #gciNewString:!private! !
!Gcilw6x categoriesFor: #gciOopToChr:!private! !
!Gcilw6x categoriesFor: #gciReleaseOops:_:!private! !
!Gcilw6x categoriesFor: #oopAsciiNul!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopAt:!public! !
!Gcilw6x categoriesFor: #oopClassArray!public! !
!Gcilw6x categoriesFor: #oopClassByteArray!public! !
!Gcilw6x categoriesFor: #oopClassDoubleByteString!public! !
!Gcilw6x categoriesFor: #oopClassQuadByteString!public! !
!Gcilw6x categoriesFor: #oopClassString!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopClassSymbol!public! !
!Gcilw6x categoriesFor: #oopClassSystem!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopClassUnicode16!public! !
!Gcilw6x categoriesFor: #oopClassUnicode32!public! !
!Gcilw6x categoriesFor: #oopClassUnicode7!public! !
!Gcilw6x categoriesFor: #oopFalse!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopForInteger:!public! !
!Gcilw6x categoriesFor: #oopGemStoneError!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopIllegal!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopMaxSmallInteger!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopMinSmallInteger!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopMinusOne!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopNil!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopOne!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopRemoteNil!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopTrue!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopTwo!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopTypeArrayClass!public! !
!Gcilw6x categoriesFor: #oopTypeClass!public! !
!Gcilw6x categoriesFor: #oopTypeWithOop:!public! !
!Gcilw6x categoriesFor: #oopZero!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #specialFromOop:!public! !

!Gcilw6x class methodsFor!

fileNameSearch

	^self name , '.dll'
! !
!Gcilw6x class categoriesFor: #fileNameSearch!public! !

