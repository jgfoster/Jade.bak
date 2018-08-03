"Filed out from Dolphin Smalltalk 7"!

StoneCommand subclass: #StartStone
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StartStone guid: (GUID fromString: '{9b0d628f-c8ea-44bc-a3ce-5fc0a51dec5e}')!
StartStone comment: ''!
!StartStone categoriesForClass!Unclassified! !
!StartStone methodsFor!

exeConfig: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/e ' , aString.
!

initialize

	super initialize.
	command := 'start'.
!

log: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/l ' , aString.
!

sysConfig: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/z ' , aString.
!

temp

	command := 'tmpstart'.
! !
!StartStone categoriesFor: #exeConfig:!public! !
!StartStone categoriesFor: #initialize!public! !
!StartStone categoriesFor: #log:!public! !
!StartStone categoriesFor: #sysConfig:!public! !
!StartStone categoriesFor: #temp!public! !

!StartStone class methodsFor!

sample1
	^StartStone new help! !
!StartStone class categoriesFor: #sample1!public! !

