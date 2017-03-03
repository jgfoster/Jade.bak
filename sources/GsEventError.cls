"Filed out from Dolphin Smalltalk 7"!

GsError subclass: #GsEventError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsEventError guid: (GUID fromString: '{f7917b60-1d40-4ede-acf3-c036f1ea7a01}')!
GsEventError comment: ''!
!GsEventError categoriesForClass!Unclassified! !
!GsEventError class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isEventErrorInSession: aGciSession.
! !
!GsEventError class categoriesFor: #wantsToHandle:session:!public! !

