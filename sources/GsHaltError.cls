"Filed out from Dolphin Smalltalk 7"!

GsApplicationError subclass: #GsHaltError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsHaltError guid: (GUID fromString: '{86ca4ef5-c363-4a19-820b-21230e7d7949}')!
GsHaltError comment: ''!
!GsHaltError categoriesForClass!Unclassified! !
!GsHaltError class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType message = 'User defined error, ''#halt encountered''' or: [
		aGciErrorSType message = 'a Halt occurred (error 2709)'].

! !
!GsHaltError class categoriesFor: #wantsToHandle:session:!public! !

