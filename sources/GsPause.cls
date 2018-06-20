"Filed out from Dolphin Smalltalk 7"!

GsEventError subclass: #GsPause
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsPause guid: (GUID fromString: '{2072dcc5-82c2-4ae5-8173-c6a980c0dc7d}')!
GsPause comment: ''!
!GsPause categoriesForClass!Unclassified! !
!GsPause class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isPauseInSession: aGciSession.
! !
!GsPause class categoriesFor: #wantsToHandle:session:!public! !

