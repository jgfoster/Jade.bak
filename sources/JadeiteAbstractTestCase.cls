"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #JadeiteAbstractTestCase
	instanceVariableNames: 'session transcript'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteAbstractTestCase guid: (GUID fromString: '{883797a0-f9e4-49bf-8635-85de41e33c6b}')!
JadeiteAbstractTestCase comment: ''!
!JadeiteAbstractTestCase categoriesForClass!Unclassified! !
!JadeiteAbstractTestCase methodsFor!

setUp
	super setUp.
	session := JadeiteTestResource current session.
	transcript := JadeiteTestResource current transcript.
!

tearDown
	super tearDown.
! !
!JadeiteAbstractTestCase categoriesFor: #setUp!private! !
!JadeiteAbstractTestCase categoriesFor: #tearDown!private! !

!JadeiteAbstractTestCase class methodsFor!

isAbstract
	^self == JadeiteAbstractTestCase! !
!JadeiteAbstractTestCase class categoriesFor: #isAbstract!public! !

