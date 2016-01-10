"Filed out from Dolphin Smalltalk 7"!

ExternalArray subclass: #OopType64Array
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OopType64Array guid: (GUID fromString: '{729F7C08-E907-4B03-A8FA-562774423BBA}')!
OopType64Array comment: ''!
!OopType64Array categoriesForClass!Unclassified! !
!OopType64Array methodsFor!

elementClass

	^OopType64.
!

size: anInteger

	bytes := bytes copyFrom: 1 to: self class elementSize * anInteger.
!

uncheckedAt: index 

	^OopType64 fromInteger: (bytes qwordAtOffset: (index - 1) * 8)!

uncheckedAt: index  put: anOopType64

	bytes 
		qwordAtOffset: (index - 1) * 8
		put: anOopType64 value.

! !
!OopType64Array categoriesFor: #elementClass!constants!public! !
!OopType64Array categoriesFor: #size:!public! !
!OopType64Array categoriesFor: #uncheckedAt:!accessing!private! !
!OopType64Array categoriesFor: #uncheckedAt:put:!accessing!private! !

!OopType64Array class methodsFor!

elementSize

	^8.
!

fromBytes: aByteArray

	^(self new: aByteArray size / self elementSize)
		bytes: aByteArray;
		yourself.
! !
!OopType64Array class categoriesFor: #elementSize!instance creation!private! !
!OopType64Array class categoriesFor: #fromBytes:!public! !

