"Filed out from Dolphin Smalltalk 7"!

Error subclass: #GciError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciError guid: (GUID fromString: '{071e2ed5-4ebd-4862-b435-48de5b29cfd6}')!
GciError comment: ''!
!GciError categoriesForClass!Unclassified! !
!GciError class methodsFor!

signal: aString

	self error: 'use #''signalWith:'''.
!

signalWith: aGciErrSType

	^self
		signal: aGciErrSType message
		with: aGciErrSType.
! !
!GciError class categoriesFor: #signal:!public! !
!GciError class categoriesFor: #signalWith:!public! !

