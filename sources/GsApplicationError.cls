"Filed out from Dolphin Smalltalk 7"!

GsRuntimeError subclass: #GsApplicationError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsApplicationError guid: (GUID fromString: '{d764751f-9e17-4f7e-abc9-5ba5384631a1}')!
GsApplicationError comment: ''!
!GsApplicationError categoriesForClass!Unclassified! !
!GsApplicationError class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isApplicationErrorInSession: aGciSession.
! !
!GsApplicationError class categoriesFor: #wantsToHandle:session:!public! !

