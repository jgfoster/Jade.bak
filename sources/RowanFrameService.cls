"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanFrameService
	instanceVariableNames: 'label method source stepPoint vars'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanFrameService guid: (GUID fromString: '{65407af5-8317-4fa0-a86d-7b9543e85cd0}')!
RowanFrameService comment: ''!
!RowanFrameService categoriesForClass!Kernel-Objects! !
!RowanFrameService methodsFor!

breaks

	^method breakPoints!

gsMethod

	^method!

indexOfSubCollection: aString 

	^source indexOfSubCollection: aString !

printOn: aStream

	aStream nextPutAll: (label ifNil: ['?'])
!

source

	^method source!

stepPoint
	^stepPoint!

vars
	^vars! !
!RowanFrameService categoriesFor: #breaks!public! !
!RowanFrameService categoriesFor: #gsMethod!public! !
!RowanFrameService categoriesFor: #indexOfSubCollection:!public! !
!RowanFrameService categoriesFor: #printOn:!public! !
!RowanFrameService categoriesFor: #source!public! !
!RowanFrameService categoriesFor: #stepPoint!public! !
!RowanFrameService categoriesFor: #vars!public! !

