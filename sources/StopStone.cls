"Filed out from Dolphin Smalltalk 7"!

StoneCommand subclass: #StopStone
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StopStone guid: (GUID fromString: '{6E6EC07C-E746-4E12-BF17-08F6B080D49E}')!
StopStone comment: ''!
!StopStone categoriesForClass!Unclassified! !
!StopStone methodsFor!

immediate

	arguments add: '/i'.
!

initialize

	super initialize.
	command := 'stop'.
!

password: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/p ' , aString.
!

temp

	command := 'tmpstop'.
!

user: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/u ' , aString.
! !
!StopStone categoriesFor: #immediate!public! !
!StopStone categoriesFor: #initialize!public! !
!StopStone categoriesFor: #password:!public! !
!StopStone categoriesFor: #temp!public! !
!StopStone categoriesFor: #user:!public! !

!StopStone class methodsFor!

sample1
	^StopStone new help! !
!StopStone class categoriesFor: #sample1!public! !

