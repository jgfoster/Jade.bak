"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanDebuggerService
	instanceVariableNames: 'processes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanDebuggerService guid: (GUID fromString: '{d8a038cb-84e2-4e03-bff9-1dc3bf097d57}')!
RowanDebuggerService comment: ''!
!RowanDebuggerService categoriesForClass!Kernel-Objects! !
!RowanDebuggerService methodsFor!

processesUpdate: presenter

	presenter list: processes.!

processListUpdate: aPresenter

	self halt.
! !
!RowanDebuggerService categoriesFor: #processesUpdate:!public! !
!RowanDebuggerService categoriesFor: #processListUpdate:!Debugger!public!updating! !

