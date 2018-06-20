"Filed out from Dolphin Smalltalk 7"!

GsEventError subclass: #GsSoftBreak
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsSoftBreak guid: (GUID fromString: '{1b6ba16b-e5cd-47a9-827c-4c945ced0db8}')!
GsSoftBreak comment: ''!
!GsSoftBreak categoriesForClass!Unclassified! !
!GsSoftBreak class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isSoftBreakInSession: aGciSession.
! !
!GsSoftBreak class categoriesFor: #wantsToHandle:session:!public! !

