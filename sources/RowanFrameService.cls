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

method
	^method!

printOn: aStream

	aStream nextPutAll: (label ifNil: ['?'])
!

saveMethod: aString session: aGciSession

	| service stonString stonResults |
self halt.
	"aGciSession
		serverPerform: #'setBreakAtStepPoint:inMethod:'
		with: anInteger
		with: self oopType."	service := self class new 
		copyBasicsFrom: self; 	"No need to send everything to the server (in fact, it results in an error!!)"
		command: #'saveMethod:';
		commandArgs: (Array with: aString);
		yourself.
	stonString := STON toString: (Array with: service).
	stonResults := aGciSession serverPerform: #updateFromSton: with: stonString.
	BrowserUpdate current update: (Array with: self) afterStonReplication: stonResults.
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
!RowanFrameService categoriesFor: #method!public! !
!RowanFrameService categoriesFor: #printOn:!public! !
!RowanFrameService categoriesFor: #saveMethod:session:!public! !
!RowanFrameService categoriesFor: #source!public! !
!RowanFrameService categoriesFor: #stepPoint!public! !
!RowanFrameService categoriesFor: #vars!public! !

