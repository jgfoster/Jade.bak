"Filed out from Dolphin Smalltalk 7"!

GsWin32Service subclass: #StoneService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StoneService guid: (GUID fromString: '{54671f79-0372-445b-96a3-c9df632955f4}')!
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

