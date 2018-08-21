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

oop

	^oop!

oopType

	^OopType64 fromInteger: oop!

printOn: aStream

	aStream
		nextPutAll: 'GsProcess(';
		print: oop;
		nextPutAll: ') with ';
		print: frames size;
		nextPutAll: ' frames';
		yourself.
!

stack

	^frames collect: [:each | each printString]! !
!RowanProcessService categoriesFor: #frameForLevel:!public! !
!RowanProcessService categoriesFor: #oop!public! !
!RowanProcessService categoriesFor: #oopType!public! !
!RowanProcessService categoriesFor: #printOn:!public! !
!RowanProcessService categoriesFor: #stack!public! !

