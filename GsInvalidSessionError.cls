"Filed out from Dolphin Smalltalk 7"!

GsFatalError subclass: #GsInvalidSessionError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsInvalidSessionError guid: (GUID fromString: '{343D0696-B2FC-405F-9E96-01929DFB6CB6}')!
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

