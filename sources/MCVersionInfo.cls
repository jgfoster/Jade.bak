"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #MCVersionInfo
	instanceVariableNames: 'date time author id ancestors stepChildren message'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCVersionInfo guid: (GUID fromString: '{202A46BC-0A0E-407F-A82D-5AD5B9DE8B87}')!
MCVersionInfo comment: ''!
!MCVersionInfo categoriesForClass!Unclassified! !
!MCVersionInfo methodsFor!

gciSession: aGciSession oop: anInteger 

	| string stream |
	gciSession := aGciSession.
	oopType := gciSession oopTypeWithOop: anInteger.
	string := gciSession
		serverPerform: #'mcVersionInfoFrom:' 
		with: oopType.
	stream := ReadStream on: string.
	stream nextLine. "OOP"
	name := stream nextLine.
	date := stream nextLine.
	time := stream nextLine.
	author := stream nextLine.
	id := stream nextLine.
	ancestors := stream nextLine subStrings: Character tab.
	stepChildren := stream nextLine subStrings: Character tab.
	message := stream upToEnd.
!

message
	^message!

timeStamp

	^date , ' ' , time.
! !
!MCVersionInfo categoriesFor: #gciSession:oop:!public! !
!MCVersionInfo categoriesFor: #message!public! !
!MCVersionInfo categoriesFor: #timeStamp!public! !

!MCVersionInfo class methodsFor!

gciSession: aGciSession oop: anInteger 

	^self new
		gciSession: aGciSession 
		oop: anInteger.
! !
!MCVersionInfo class categoriesFor: #gciSession:oop:!public! !

