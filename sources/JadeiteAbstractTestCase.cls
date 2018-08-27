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

abortTransaction
	transcript abortTransaction!

openWindow: aBlock
	"evaluate a block which is expected to return an object that responds to #view.
	Wait until the view affirms it is open before continuing"

	| presenter count |
	presenter := aBlock value.
	count := 0.
	[presenter view isOpen] whileFalse: 
			[(Delay forMilliseconds: 50) wait.
			count := count + 1.
			count > 10 ifTrue: [^presenter]].
	^presenter!

setUp
	super setUp.
	session := JadeiteTestResource current session.
	transcript := JadeiteTestResource current transcript.
!

tearDown
	super tearDown.
!

testsIssue: aSymbol withTitle: anObject
	"send this to help identify which tests test which issues"

	" a helpful parameter format is #issue<issueNumber>"

	! !
!JadeiteAbstractTestCase categoriesFor: #abortTransaction!private!setup teardown! !
!JadeiteAbstractTestCase categoriesFor: #openWindow:!browser test support!private! !
!JadeiteAbstractTestCase categoriesFor: #setUp!private! !
!JadeiteAbstractTestCase categoriesFor: #tearDown!private! !
!JadeiteAbstractTestCase categoriesFor: #testsIssue:withTitle:!public!support! !

!JadeiteAbstractTestCase class methodsFor!

isAbstract
	^self == JadeiteAbstractTestCase! !
!JadeiteAbstractTestCase class categoriesFor: #isAbstract!public! !

