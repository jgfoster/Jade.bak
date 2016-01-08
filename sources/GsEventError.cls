"Filed out from Dolphin Smalltalk 7"!

GsError subclass: #GsEventError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsEventError guid: (GUID fromString: '{F7917B60-1D40-4EDE-ACF3-C036F1EA7A01}')!
GsEventError comment: ''!
!GsEventError categoriesForClass!Unclassified! !
!GsEventError class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isEventErrorInSession: aGciSession.
! !
!GsEventError class categoriesFor: #wantsToHandle:session:!public! !

