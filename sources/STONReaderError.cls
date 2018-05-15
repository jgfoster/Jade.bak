"Filed out from Dolphin Smalltalk 7"!

Error subclass: #STONReaderError
	instanceVariableNames: 'streamPosition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STONReaderError guid: (GUID fromString: '{2c12fc6d-058e-4e28-93e6-33607c9344b9}')!
STONReaderError comment: ''!
!STONReaderError categoriesForClass!Unclassified! !
!STONReaderError methodsFor!

messageText
	^streamPosition
		ifNil: [super messageText]
		ifNotNil: [:pos | 'At character {1}: {2}' format: (Array with: streamPosition with: super messageText)]!

streamPosition
	^streamPosition!

streamPosition: aNumber
	streamPosition := aNumber
! !
!STONReaderError categoriesFor: #messageText!accessing!public! !
!STONReaderError categoriesFor: #streamPosition!accessing!public! !
!STONReaderError categoriesFor: #streamPosition:!accessing!public! !

!STONReaderError class methodsFor!

signal: aString streamPosition: streamPosition 
	^ self new
		streamPosition: streamPosition;
		signal: aString;
		yourself
! !
!STONReaderError class categoriesFor: #signal:streamPosition:!public! !

