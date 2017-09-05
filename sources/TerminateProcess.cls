"Filed out from Dolphin Smalltalk 7"!

Notification subclass: #TerminateProcess
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TerminateProcess guid: (GUID fromString: '{7389ae5e-cb19-4bfe-923b-860dcadadb60}')!
TerminateProcess comment: ''!
!TerminateProcess categoriesForClass!Unclassified! !
!TerminateProcess methodsFor!

defaultAction

	Processor forkMainIfMain.
	Processor terminateActive.
! !
!TerminateProcess categoriesFor: #defaultAction!public! !

