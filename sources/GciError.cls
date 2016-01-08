"Filed out from Dolphin Smalltalk 7"!

Error subclass: #GciError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciError guid: (GUID fromString: '{071E2ED5-4EBD-4862-B435-48DE5B29CFD6}')!
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

