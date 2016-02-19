"Filed out from Dolphin Smalltalk 7"!

TestResource subclass: #GciMtLibraryTestResource
	instanceVariableNames: 'library session'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciMtLibraryTestResource guid: (GUID fromString: '{689B0A0E-E39A-43AB-AE67-060AF181BB24}')!
GciMtLibraryTestResource comment: ''!
!GciMtLibraryTestResource categoriesForClass!Unclassified! !
!GciMtLibraryTestResource methodsFor!

session

	^session!

setUp
	super setUp.
	library := LibGciMt_3_3 default.
	session 
		ifNil: 
			[session := library 
						loginUser: 'DataCurator'
						password: 'swordfish'
						gemNRS: '!!@portugal#netldi:ldijfoster1#task!!gemnetobject'
						stoneNRS: 'jfoster1']!

tearDown

	session ifNotNil: [
		library logoutSession: session.
		session := nil.
	].
	super tearDown.
! !
!GciMtLibraryTestResource categoriesFor: #session!accessing!public! !
!GciMtLibraryTestResource categoriesFor: #setUp!private! !
!GciMtLibraryTestResource categoriesFor: #tearDown!private! !

