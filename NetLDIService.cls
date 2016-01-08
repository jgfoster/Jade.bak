"Filed out from Dolphin Smalltalk 7"!

GsWin32Service subclass: #NetLDIService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NetLDIService guid: (GUID fromString: '{543D39F7-0A12-4645-A8B2-560938E69D84}')!
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

