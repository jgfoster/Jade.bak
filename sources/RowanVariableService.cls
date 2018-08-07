"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanVariableService
	instanceVariableNames: 'oop key value className'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanVariableService guid: (GUID fromString: '{7fef01ea-b803-4a8a-bb67-843c948bbc31}')!
RowanVariableService comment: ''!
!RowanVariableService categoriesForClass!Kernel-Objects! !
!RowanVariableService methodsFor!

_key

	^key!

className

	^className!

key
	"provided to match expectation of UI for JadeDebugger"

	^oop printString -> key!

oop

	^oop!

value

	^value! !
!RowanVariableService categoriesFor: #_key!public! !
!RowanVariableService categoriesFor: #className!public! !
!RowanVariableService categoriesFor: #key!public! !
!RowanVariableService categoriesFor: #oop!public! !
!RowanVariableService categoriesFor: #value!public! !

