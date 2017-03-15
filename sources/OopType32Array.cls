"Filed out from Dolphin Smalltalk 7"!

ExternalArray subclass: #OopType32Array
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OopType32Array guid: (GUID fromString: '{73aa2718-0f19-4b5e-af7f-a3d2d8fbb1c4}')!
OopType32Array comment: ''!
!OopType32Array categoriesForClass!Unclassified! !
!OopType32Array methodsFor!

elementClass

	^OopType32.!

uncheckedAt: index 

	^OopType32 fromInteger: (bytes dwordAtOffset: (index - 1) * 4).
!

uncheckedAt: index  put: anOopType32

	bytes 
		dwordAtOffset: (index - 1) * 4
		put: anOopType32 value.

! !
!OopType32Array categoriesFor: #elementClass!constants!public! !
!OopType32Array categoriesFor: #uncheckedAt:!accessing!private! !
!OopType32Array categoriesFor: #uncheckedAt:put:!accessing!private! !

!OopType32Array class methodsFor!

elementSize

	^4.
! !
!OopType32Array class categoriesFor: #elementSize!instance creation!private! !

