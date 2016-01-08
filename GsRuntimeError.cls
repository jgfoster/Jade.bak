"Filed out from Dolphin Smalltalk 7"!

GsError subclass: #GsRuntimeError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsRuntimeError guid: (GUID fromString: '{A3B64446-755D-4647-9CCA-2354531DC693}')!
GsRuntimeError comment: ''!
!GsRuntimeError categoriesForClass!Unclassified! !
!GsRuntimeError methodsFor!

isResumableInGem

	^true.
! !
!GsRuntimeError categoriesFor: #isResumableInGem!public! !

!GsRuntimeError class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isRuntimeErrorInSession: aGciSession.
! !
!GsRuntimeError class categoriesFor: #wantsToHandle:session:!public! !

