"Filed out from Dolphin Smalltalk 7"!

GsApplicationError subclass: #GsHaltError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsHaltError guid: (GUID fromString: '{86CA4EF5-C363-4A19-820B-21230E7D7949}')!
GsHaltError comment: ''!
!GsHaltError categoriesForClass!Unclassified! !
!GsHaltError class methodsFor!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType message = 'User defined error, ''#halt encountered''' or: [
		aGciErrorSType message = 'a Halt occurred (error 2709)'].

! !
!GsHaltError class categoriesFor: #wantsToHandle:session:!public! !

