"Filed out from Dolphin Smalltalk 7"!

STONListWriter subclass: #STONShortListWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STONShortListWriter guid: (GUID fromString: '{26199d15-3911-48eb-91c4-2a2af72c1345}')!
STONShortListWriter comment: ''!
!STONShortListWriter categoriesForClass!Unclassified! !
!STONShortListWriter methodsFor!

add: anObject
	first ifTrue: [ first := false ] ifFalse: [ writer shortListElementSeparator ].
	writer nextPut: anObject
! !
!STONShortListWriter categoriesFor: #add:!public! !

