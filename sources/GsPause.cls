"Filed out from Dolphin Smalltalk 7"!

GsEventError subclass: #GsPause
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsPause guid: (GUID fromString: '{2072DCC5-82C2-4AE5-8173-C6A980C0DC7D}')!
GsPause comment: ''!
!GsPause categoriesForClass!Unclassified! !
!GsPause class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isPauseInSession: aGciSession.
! !
!GsPause class categoriesFor: #wantsToHandle:session:!public! !

