"Filed out from Dolphin Smalltalk 7"!

TestResource subclass: #GciLibraryTestResource
	instanceVariableNames: 'library session'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciLibraryTestResource guid: (GUID fromString: '{689B0A0E-E39A-43AB-AE67-060AF181BB24}')!
GciLibraryTestResource comment: ''!
!GciLibraryTestResource categoriesForClass!Unclassified! !
!GciLibraryTestResource methodsFor!

libraryClass

	^self subclassResponsibility
!

session

	^session!

setUp
	super setUp.
	library := self libraryClass default.
	session ifNil: [
		session := library 
			loginUser: 'DataCurator'
			password: 'swordfish'
			gemNRS: '!!@portugal#netldi:ldijfoster1#task!!gemnetobject'
			stoneNRS: 'jfoster1'.
	]!

tearDown

	session ifNotNil: [
		library logoutSession: session.
		session := nil.
	].
	super tearDown.
! !
!GciLibraryTestResource categoriesFor: #libraryClass!private! !
!GciLibraryTestResource categoriesFor: #session!accessing!public! !
!GciLibraryTestResource categoriesFor: #setUp!private! !
!GciLibraryTestResource categoriesFor: #tearDown!private! !

