"Filed out from Dolphin Smalltalk 7"!

GsAnsiError subclass: #GsTestFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsTestFailure guid: (GUID fromString: '{BD39CA74-6D0F-4A45-A5ED-2ACF574AB10F}')!
GsTestFailure comment: ''!
!GsTestFailure categoriesForClass!Unclassified! !
!GsTestFailure class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType message = 'aTestFailure signal: ''Assertion failed'''.
! !
!GsTestFailure class categoriesFor: #wantsToHandle:session:!public! !

