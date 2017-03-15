"Filed out from Dolphin Smalltalk 7"!

StoneCommand subclass: #StopStone
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StopStone guid: (GUID fromString: '{6e6ec07c-e746-4e12-bf17-08f6b080d49e}')!
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

