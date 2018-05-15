"Filed out from Dolphin Smalltalk 7"!

STONStreamWriter subclass: #STONListWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STONListWriter guid: (GUID fromString: '{d472c72c-a164-46ad-8eb7-66bd6354359d}')!
STONListWriter comment: ''!
!STONListWriter categoriesForClass!Unclassified! !
!STONListWriter methodsFor!

add: anObject
	first ifTrue: [ first := false ] ifFalse: [ writer listElementSeparator ].
	writer nextPut: anObject
! !
!STONListWriter categoriesFor: #add:!accessing!public! !

