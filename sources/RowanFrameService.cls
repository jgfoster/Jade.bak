"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanFrameService
	instanceVariableNames: 'label method stepPoint vars'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanFrameService guid: (GUID fromString: '{65407af5-8317-4fa0-a86d-7b9543e85cd0}')!
RowanFrameService comment: ''!
!RowanFrameService categoriesForClass!Kernel-Objects! !
!RowanFrameService methodsFor!

breaks

	^method breakPoints!

copyBasicsFrom: oldService

	method := RowanMethodService new copyBasicsFrom: oldService method.
!

gsMethod

	^method!

indexOfSubCollection: aString 

	^method source indexOfSubCollection: aString !

isExecutedBlockContext

	^method className isNil!

method
	^method!

offsets

	^method stepPoints collect: [:each | each key start]!

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
!RowanFrameService categoriesFor: #copyBasicsFrom:!public! !
!RowanFrameService categoriesFor: #gsMethod!public! !
!RowanFrameService categoriesFor: #indexOfSubCollection:!public! !
!RowanFrameService categoriesFor: #isExecutedBlockContext!public!testing! !
!RowanFrameService categoriesFor: #method!public! !
!RowanFrameService categoriesFor: #offsets!public! !
!RowanFrameService categoriesFor: #printOn:!public! !
!RowanFrameService categoriesFor: #source!public! !
!RowanFrameService categoriesFor: #stepPoint!public! !
!RowanFrameService categoriesFor: #vars!public! !

