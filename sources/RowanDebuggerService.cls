"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanDebuggerService
	instanceVariableNames: 'initialProcessOop processes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanDebuggerService guid: (GUID fromString: '{d8a038cb-84e2-4e03-bff9-1dc3bf097d57}')!
RowanDebuggerService comment: ''!
!RowanDebuggerService categoriesForClass!Kernel-Objects! !
!RowanDebuggerService methodsFor!

initialize: aGsProcess

	initialProcessOop := aGsProcess oopType value.
!

processes
	^processes!

processListUpdate: aPresenter
	aPresenter parentPresenter gsProcess oop = processes first oop ifFalse: [^self].
	aPresenter list: processes! !
!RowanDebuggerService categoriesFor: #initialize:!public! !
!RowanDebuggerService categoriesFor: #processes!accessing!public! !
!RowanDebuggerService categoriesFor: #processListUpdate:!Debugger!public!updating! !

!RowanDebuggerService class methodsFor!

onProcess: aGsProcess

	^self basicNew
		initialize: aGsProcess;
		yourself! !
!RowanDebuggerService class categoriesFor: #onProcess:!public! !

