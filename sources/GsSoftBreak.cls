"Filed out from Dolphin Smalltalk 7"!

GsEventError subclass: #GsSoftBreak
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsSoftBreak guid: (GUID fromString: '{1B6BA16B-E5CD-47A9-827C-4C945CED0DB8}')!
GsSoftBreak comment: ''!
!GsSoftBreak categoriesForClass!Unclassified! !
!GsSoftBreak class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isSoftBreakInSession: aGciSession.
! !
!GsSoftBreak class categoriesFor: #wantsToHandle:session:!public! !

