"Filed out from Dolphin Smalltalk 7"!

JadeiteAbstractTestCase subclass: #JadeiteDictionaryBrowserTestCase
	instanceVariableNames: 'dictionaryBrowser'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteDictionaryBrowserTestCase guid: (GUID fromString: '{3d66b039-3986-43f7-9e95-6a137a1473f7}')!
JadeiteDictionaryBrowserTestCase comment: ''!
!JadeiteDictionaryBrowserTestCase categoriesForClass!Unclassified! !
!JadeiteDictionaryBrowserTestCase methodsFor!

primaryPresenter
	^dictionaryBrowser currentCard!

selectClass: className
	| classPresenter selection |
	classPresenter := self primaryPresenter classListPresenter.
	classPresenter resetSelection.
	selection := classPresenter list detect: [:string | string = className]
				ifNone: [self error: 'Class ' , className , ' not found'].
	classPresenter selection: selection!

selectClassCategory: classCategoryName
	| classCategoryPresenter selection |
	classCategoryPresenter := self primaryPresenter classCategoryPresenter.
	classCategoryPresenter resetSelection.
	selection := classCategoryPresenter list detect: [:assoc | assoc key = classCategoryName]
				ifNone: [self error: 'Class Category ' , classCategoryName , ' not found'].
	classCategoryPresenter selection: selection!

selectDictionary: dictionaryName
	| dictionaryPresenter selection |
	dictionaryPresenter := self primaryPresenter dictionaryListPresenter.
	dictionaryPresenter resetSelection.
	selection := dictionaryPresenter list detect: [:assoc | assoc key = dictionaryName]
				ifNone: [self error: 'Dictionary ' , dictionaryName , ' not found'].
	dictionaryPresenter selection: selection!

selectMethod: selector
	| methodListPresenter selection |
	methodListPresenter := self primaryPresenter methodListPresenter.
	methodListPresenter resetSelection.
	selection := methodListPresenter list detect: [:assoc | assoc first = selector]
				ifNone: [self error: 'Method ' , selector , ' not found'].
	methodListPresenter selection: selection!

selectWellKnownMethod
	"select some method in the method list pane"
	self selectDictionary: 'Globals'. 
	self selectClass: 'Behavior'. 
	self selectMethod: 'basicNew'. 
!

setUp
	super setUp.
	dictionaryBrowser := self openWindow: [transcript openDictionaryBrowser]!

tearDown
	super tearDown.
	self abortTransaction.
	dictionaryBrowser ifNotNil: [dictionaryBrowser destroy]!

test_browseReferencesInDictionaryBrowser
	| methodListBrowser source |
	self selectDictionary: 'Globals'.
	self selectClass: 'Date'.
	methodListBrowser := self primaryPresenter browseClassReferences.
	
	[source := methodListBrowser primaryPresenter methodSourcePresenter documentPresenter value.
	self assert: ('*Date*' match: source)]
			ensure: [methodListBrowser destroy]!

test_menusDontAddBreaks
	"not a great test, but make sure we don't add a breakpoint menu item in 
	the menu bar"

	| menuBar jadeMenu methodsMenu |
	self testsIssue: #issue162 withTitle: 'In method list browser, get new breakpoint menu items'.
	menuBar := dictionaryBrowser view menuBar.
	jadeMenu := self menuNamed: 'Jade' in: menuBar.
	methodsMenu := self menuNamed: 'Methods' in: menuBar.
	self noBreakIn: jadeMenu.
	self noBreakIn: methodsMenu.
	self selectWellKnownMethod. "the method we're testing requires a selection"
	self primaryPresenter methodMenuBreak: jadeMenu.
	self primaryPresenter methodMenuBreak: methodsMenu.
	self noBreakIn: jadeMenu.
	self noBreakIn: methodsMenu!

test_selectTestClass
	self testsIssue: #issue177 withTitle: 'Dictionary browser error/s'.
	self selectDictionary: 'Rowan'.
	self selectClassCategory: 'Rowan-Tests'.
	self selectClass: 'RwBrowserToolApiTest'.	"<<< walkback occurred here"
	self assert: self primaryPresenter classListPresenter selection name equals: 'RwBrowserToolApiTest'! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #primaryPresenter!private! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #selectClass:!private!selection! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #selectClassCategory:!private!selection! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #selectDictionary:!private!selection! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #selectMethod:!private!selection! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #selectWellKnownMethod!private!selection! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #setUp!public!setup teardown! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #tearDown!public!setup teardown! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #test_browseReferencesInDictionaryBrowser!public!test! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #test_menusDontAddBreaks!public!test! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #test_selectTestClass!public!test! !

!JadeiteDictionaryBrowserTestCase class methodsFor!

resources
	^IdentitySet with: JadeiteTestResource! !
!JadeiteDictionaryBrowserTestCase class categoriesFor: #resources!public! !

