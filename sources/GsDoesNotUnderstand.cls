"Filed out from Dolphin Smalltalk 7"!

GsRuntimeError subclass: #GsDoesNotUnderstand
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsDoesNotUnderstand guid: (GUID fromString: '{47a598c1-9046-44f2-bdb5-954383dd1ef5}')!
GsDoesNotUnderstand comment: ''!
!GsDoesNotUnderstand categoriesForClass!Unclassified! !
!GsDoesNotUnderstand class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isDoesNotUnderstandInSession: aGciSession.
! !
!GsDoesNotUnderstand class categoriesFor: #wantsToHandle:session:!public! !

