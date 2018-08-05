"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanAnswerService
	instanceVariableNames: 'answer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanAnswerService guid: (GUID fromString: '{d2ef2348-8273-4df1-b823-ba4509d53308}')!
RowanAnswerService comment: 'An answer service doesn''t understand any update commands. 
He just responds with an answer object. '!
!RowanAnswerService categoriesForClass!Kernel-Objects! !
!RowanAnswerService methodsFor!

answer
	^answer!

answer: anObject
	answer := anObject! !
!RowanAnswerService categoriesFor: #answer!accessing!private! !
!RowanAnswerService categoriesFor: #answer:!accessing!private! !

