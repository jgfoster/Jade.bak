"Filed out from Dolphin Smalltalk 7"!

GsWin32Service subclass: #NetLDIService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NetLDIService guid: (GUID fromString: '{543d39f7-0a12-4645-a8b2-560938e69d84}')!
NetLDIService comment: ''!
!NetLDIService categoriesForClass!Unclassified! !
!NetLDIService methodsFor!

serviceName

	^'NetLDI'.
!

start

	^NetLDICommand new 
		command: 'start';
		result.
!

stop

	^NetLDICommand new 
		command: 'stop';
		result.
! !
!NetLDIService categoriesFor: #serviceName!public! !
!NetLDIService categoriesFor: #start!public! !
!NetLDIService categoriesFor: #stop!public! !

