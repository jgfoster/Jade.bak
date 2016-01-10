"Filed out from Dolphin Smalltalk 7"!

GsRuntimeError subclass: #GsDoesNotUnderstand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsDoesNotUnderstand guid: (GUID fromString: '{47A598C1-9046-44F2-BDB5-954383DD1EF5}')!
GsDoesNotUnderstand comment: ''!
!GsDoesNotUnderstand categoriesForClass!Unclassified! !
!GsDoesNotUnderstand class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isDoesNotUnderstandInSession: aGciSession.
! !
!GsDoesNotUnderstand class categoriesFor: #wantsToHandle:session:!public! !

