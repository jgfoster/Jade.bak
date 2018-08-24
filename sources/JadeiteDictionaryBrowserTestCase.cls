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

selectDictionary: dictionaryName
	| dictionaryPresenter selection |
	dictionaryPresenter := self primaryPresenter dictionaryListPresenter.
	dictionaryPresenter resetSelection.
	selection := dictionaryPresenter list detect: [:assoc | assoc key = dictionaryName]
				ifNone: [self error: 'Dictionary ' , dictionaryName , ' not found'].
	dictionaryPresenter selection: selection!

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
			ensure: [methodListBrowser destroy]! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #primaryPresenter!private! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #selectClass:!private!selection! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #selectDictionary:!private!selection! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #setUp!public!setup teardown! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #tearDown!public!setup teardown! !
!JadeiteDictionaryBrowserTestCase categoriesFor: #test_browseReferencesInDictionaryBrowser!public!test! !

!JadeiteDictionaryBrowserTestCase class methodsFor!

resources
	^IdentitySet with: JadeiteTestResource! !
!JadeiteDictionaryBrowserTestCase class categoriesFor: #resources!public! !

