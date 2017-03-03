"Filed out from Dolphin Smalltalk 7"!

GsFatalError subclass: #GsInvalidSessionError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsInvalidSessionError guid: (GUID fromString: '{343d0696-b2fc-405f-9e96-01929dfb6cb6}')!
GsInvalidSessionError comment: ''!
!GsInvalidSessionError categoriesForClass!Unclassified! !
!GsInvalidSessionError methodsFor!

defaultAction

	MessageBox errorMsg: 'Invalid Session!!'.
	self gciSession forceLogout.
	Processor forkMainIfMain.
	Processor activeProcess terminate.
! !
!GsInvalidSessionError categoriesFor: #defaultAction!public! !

!GsInvalidSessionError class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isInvalidSessionInSession: aGciSession.
! !
!GsInvalidSessionError class categoriesFor: #wantsToHandle:session:!public! !

