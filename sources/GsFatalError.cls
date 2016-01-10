"Filed out from Dolphin Smalltalk 7"!

GsError subclass: #GsFatalError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsFatalError guid: (GUID fromString: '{71EABB54-3A5A-4CA2-BCA3-44B39D453586}')!
GsFatalError comment: ''!
!GsFatalError categoriesForClass!Unclassified! !
!GsFatalError methodsFor!

defaultAction

	MessageBox 
		errorMsg: gciErrSType message
		caption: 'Fatal GemStone Error'.
	Processor activeProcess terminate.
! !
!GsFatalError categoriesFor: #defaultAction!public! !

!GsFatalError class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType number // 1000 = 4.
! !
!GsFatalError class categoriesFor: #wantsToHandle:session:!public! !

