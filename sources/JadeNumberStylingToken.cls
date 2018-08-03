"Filed out from Dolphin Smalltalk 7"!

StNumberLiteralToken subclass: #JadeNumberStylingToken
	instanceVariableNames: 'numberClass'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeNumberStylingToken guid: (GUID fromString: '{C7C638B9-53CD-4316-803A-A5AC217D7F73}')!
JadeNumberStylingToken comment: ''!
!JadeNumberStylingToken categoriesForClass!Unclassified! !
!JadeNumberStylingToken methodsFor!

isNegative
	^self source first == $-!

numberClass
	^numberClass ifNil: [self value class]!

numberClass: aClass 
	numberClass := aClass!

value
	value isNil ifTrue: [value := [Number readFrom: self source readStream] on: Error do: [0]].
	^value! !
!JadeNumberStylingToken categoriesFor: #isNegative!accessing!public! !
!JadeNumberStylingToken categoriesFor: #numberClass!public! !
!JadeNumberStylingToken categoriesFor: #numberClass:!accessing!private! !
!JadeNumberStylingToken categoriesFor: #value!accessing!public! !

