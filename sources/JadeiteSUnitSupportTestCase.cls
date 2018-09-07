"Filed out from Dolphin Smalltalk 7"!

JadeiteAbstractTestCase subclass: #JadeiteSUnitSupportTestCase
	instanceVariableNames: 'sunitBrowser'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteSUnitSupportTestCase guid: (GUID fromString: '{afd06491-29d7-4988-bf71-09264248baaa}')!
JadeiteSUnitSupportTestCase comment: ''!
!JadeiteSUnitSupportTestCase categoriesForClass!Unclassified! !
!JadeiteSUnitSupportTestCase methodsFor!

setUp
	super setUp. 
	sunitBrowser := self openWindow: [transcript jadeBrowseTests]!

sunitPresenter
	^sunitBrowser primaryPresenter!

tearDown
	sunitBrowser destroy!

test_openSunitBrowser
	"browser opened in setup"

	self assert: (sunitBrowser isKindOf: JadeSUnitBrowser)!

test_sunitBrowserHasRowanPackages
	self assert: (self sunitPresenter isKindOf: JadeSUnitPresenter).	"a different presenter is used in the projects browser"
	self assert: self sunitPresenter packageListPresenter list size > 0.
	self denyIsNil: (self serviceNamed: 'Rowan-Tests' in: self sunitPresenter packageListPresenter).
	self
		denyIsNil: (self serviceNamed: 'Rowan-Services-Tests' in: self sunitPresenter packageListPresenter)!

test_sunitBrowserHasRowanTestClasses
	self assert: self sunitPresenter classListPresenter list size = 0.
	self selectServiceNamed: 'Rowan-Tests' in: self sunitPresenter packageListPresenter.
	self denyIsNil: (self serviceNamed: 'RwMoveTest' in: self sunitPresenter classListPresenter).
	self assert: self sunitPresenter classListPresenter list size > 0.
	self denyIsNil: (self serviceNamed: 'RwBrowserToolApiTest' in: self sunitPresenter classListPresenter).!

test_sunitBrowserHasRowanTestMethods
	self selectServiceNamed: 'Rowan-Tests' in: self sunitPresenter packageListPresenter.
	self assert: self sunitPresenter methodListPresenter list size = 0.
	self selectServiceNamed: 'RwMoveTest' in: self sunitPresenter classListPresenter.
	self assert: self sunitPresenter methodListPresenter list size > 0.! !
!JadeiteSUnitSupportTestCase categoriesFor: #setUp!public!support! !
!JadeiteSUnitSupportTestCase categoriesFor: #sunitPresenter!public!support! !
!JadeiteSUnitSupportTestCase categoriesFor: #tearDown!public!support! !
!JadeiteSUnitSupportTestCase categoriesFor: #test_openSunitBrowser!public!tests! !
!JadeiteSUnitSupportTestCase categoriesFor: #test_sunitBrowserHasRowanPackages!public!tests! !
!JadeiteSUnitSupportTestCase categoriesFor: #test_sunitBrowserHasRowanTestClasses!public!tests! !
!JadeiteSUnitSupportTestCase categoriesFor: #test_sunitBrowserHasRowanTestMethods!public!tests! !

!JadeiteSUnitSupportTestCase class methodsFor!

resources
	^IdentitySet with: JadeiteTestResource! !
!JadeiteSUnitSupportTestCase class categoriesFor: #resources!public! !

