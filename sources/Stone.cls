"Filed out from Dolphin Smalltalk 7"!

GsHostProcess subclass: #Stone
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Stone guid: (GUID fromString: '{30d3a885-42ab-423a-8752-c86ce1f268c9}')!
Stone comment: ''!
!Stone categoriesForClass!Unclassified! !
!Stone methodsFor!

applyToLogin: aJadeLogin

	aJadeLogin
		stoneHost: 	self stoneHost;
		stoneName:  self stoneName;
		yourself.
!

processType

	^'Stone'.
!

stoneHost

	^(self name includes: $:)
		ifTrue:  [(self name subStrings: $:) first]
		ifFalse: [''].
!

stoneName

	^(self name subStrings: $:) last.
! !
!Stone categoriesFor: #applyToLogin:!public! !
!Stone categoriesFor: #processType!public! !
!Stone categoriesFor: #stoneHost!public! !
!Stone categoriesFor: #stoneName!public! !

!Stone class methodsFor!

test

	| extentPath config copyResult startResult |
	extentPath := 'c:\gemstone\614\data\myExtent0.dbf'.
	(File exists: extentPath) ifTrue: [File delete: extentPath].
	copyResult := CopyDBF new
		sourceNRS: 		(GsNRS body: GsConfiguration emptyExtentPath);
		destinationNRS: 	(GsNRS body: extentPath);
		result.
	config := GsConfiguration new
		extentNames: extentPath;
		write;
		yourself.
	startResult := StartStone new
		sysConfig: config path;
		stoneName: 'myStone';
		command: 'tmpstart';
		result.
	self halt.
!

test1

	| extentPath config copyResult |
	extentPath := 'c:/gemstone/614/data/myExtent0.dbf'.
	(GsFile existsOnServer: extentPath) ifTrue: [GsFile removeServerFile: extentPath].
	copyResult := CopyDBF new
		sourceNRS: 		(GsNRS body: GsConfiguration emptyExtentPath);
		destinationNRS: 	(GsNRS body: extentPath);
		result.
	config := GsConfiguration new
		dbfExtentNames: extentPath;
		stnTranLogPrefix: 'myTranLog';
		write;
		yourself.
	^StartStone new
		sysConfig: config path;
		stoneName: 'myStone';
		command: 'tmpstart';
		result.
!

test2

	^StopStone new
		stoneName: 'myStone';
		command: 'tmpstop';
		user: 'SystemUser';
		password: 'swordfish';
		result.
!

type

	^'Stone'.
! !
!Stone class categoriesFor: #test!public! !
!Stone class categoriesFor: #test1!public! !
!Stone class categoriesFor: #test2!public! !
!Stone class categoriesFor: #type!public! !

