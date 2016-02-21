"Filed out from Dolphin Smalltalk 7"!

GciLibraryTestCase subclass: #GciMtLibraryTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciMtLibraryTestCase guid: (GUID fromString: '{78C22419-3B80-4F29-83FC-85940ADA7460}')!
GciMtLibraryTestCase comment: ''!
!GciMtLibraryTestCase categoriesForClass!Unclassified! !
!GciMtLibraryTestCase methodsFor!

setUp
	super setUp.
	library := LibGciMt_3_3 default.
	session := GciMtLibraryTestResource current session! !
!GciMtLibraryTestCase categoriesFor: #setUp!private! !

!GciMtLibraryTestCase class methodsFor!

resources

	^IdentitySet with: GciMtLibraryTestResource! !
!GciMtLibraryTestCase class categoriesFor: #resources!public! !

