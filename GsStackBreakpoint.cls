"Filed out from Dolphin Smalltalk 7"!

GsEventError subclass: #GsStackBreakpoint
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsStackBreakpoint guid: (GUID fromString: '{601667A8-F7D5-4FAB-97F1-C753A26EBC27}')!
GsStackBreakpoint comment: ''!
!GsStackBreakpoint categoriesForClass!Unclassified! !
!GsStackBreakpoint methodsFor!

isStackBreakpoint

	^true.
! !
!GsStackBreakpoint categoriesFor: #isStackBreakpoint!public! !

!GsStackBreakpoint class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isStackBreakpointInSession: aGciSession.
! !
!GsStackBreakpoint class categoriesFor: #wantsToHandle:session:!public! !

