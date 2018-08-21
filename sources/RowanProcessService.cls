"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanProcessService
	instanceVariableNames: 'frames oop status'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProcessService guid: (GUID fromString: '{ef0255f2-38fa-473c-8886-4f6beea18afa}')!
RowanProcessService comment: ''!
!RowanProcessService categoriesForClass!Kernel-Objects! !
!RowanProcessService methodsFor!

frameForLevel: anInteger 

	^frames at: anInteger!

gsProcessForSession: session
	^GsProcess session: session oopType: self oopType!

oop

	^oop!

oopType

	^OopType64 fromInteger: oop!

stack

	^frames collect: [:each | each printString]! !
!RowanProcessService categoriesFor: #frameForLevel:!public! !
!RowanProcessService categoriesFor: #gsProcessForSession:!public! !
!RowanProcessService categoriesFor: #oop!public! !
!RowanProcessService categoriesFor: #oopType!public! !
!RowanProcessService categoriesFor: #stack!public! !

