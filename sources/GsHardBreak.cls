"Filed out from Dolphin Smalltalk 7"!

GsEventError subclass: #GsHardBreak
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsHardBreak guid: (GUID fromString: '{DDC5CCC6-21B0-4B56-BAD1-F688B283CA6C}')!
GsHardBreak comment: ''!
!GsHardBreak categoriesForClass!Unclassified! !
!GsHardBreak class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isHardBreakInSession: aGciSession.
! !
!GsHardBreak class categoriesFor: #wantsToHandle:session:!public! !

