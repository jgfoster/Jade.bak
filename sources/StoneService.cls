"Filed out from Dolphin Smalltalk 7"!

GsWin32Service subclass: #StoneService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StoneService guid: (GUID fromString: '{54671F79-0372-445B-96A3-C9DF632955F4}')!
StoneService comment: ''!
!StoneService categoriesForClass!Unclassified! !
!StoneService methodsFor!

serviceName

	^'Stone'.
!

start

	^StartStone new 
		stoneName: name;
		result.
!

stop

	^StopStone new 
		stoneName: name;
		result.
! !
!StoneService categoriesFor: #serviceName!public! !
!StoneService categoriesFor: #start!public! !
!StoneService categoriesFor: #stop!public! !

