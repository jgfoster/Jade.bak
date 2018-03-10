"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #RowanDefinitionServiceTest
	instanceVariableNames: 'shell methodService'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanDefinitionServiceTest guid: (GUID fromString: '{f46b0d8e-4841-4dc3-8f02-bcbdd5083fb7}')!
RowanDefinitionServiceTest comment: ''!
!RowanDefinitionServiceTest categoriesForClass!SUnit! !
!RowanDefinitionServiceTest methodsFor!

browseImplementorsOf: aString
	| gciSession |
	gciSession := RowanDefinitionService new transport.
	shell := RowanMethodListBrowser showOnSession: gciSession. 
	shell primaryPresenter
		browseImplementorsOf: aString;
		yourself!

firstListElement
	^shell primaryPresenter methodListPresenter subPresenters first model list first!

setUp
	super setUp.
	methodService := RowanMethodDefinitionService new.!

tearDown
	super tearDown.
	shell ifNotNil: [:shell | shell exit].
	RowanPackageDefinitionService new removeSampleSymbolDictionary.!

test_addNewMethod
	methodService createSampleMethod.
	self browseImplementorsOf: 'sampleMethod'.	"first list element should be selected"
	self updateMethodSource: 'sampleMethod2
		^123'.
	!

test_createSampleMethod
	"creating the sample method through Rowan will test
	that Rowan created a method, class, package, and project.
	It should also compile the method behind the scenes such
	that an old style implementors call will discover it. "

	| listElement |
	methodService createSampleMethod.
	self browseImplementorsOf: 'sampleMethod'.
	self assert: (shell isKindOf: RowanMethodListBrowser).
	listElement := self firstListElement.
	self assert: listElement gsClass name = 'SampleClassName'.
	self assert: listElement name = 'sampleMethod'.
	self assert: listElement category = 'sample'!

test_openMethodBrowser
	| gciSession |
	gciSession := RowanDefinitionService new transport.
	shell := RowanMethodListBrowser showOnSession: gciSession.
	self assert: (shell isKindOf: RowanMethodListBrowser).
	self assert: (shell primaryPresenter isKindOf: RowanMethodListPresenter)!

test_sampleMethodDefinitions
	"test values hard coded on remote GemStone server"

	| services |
	services := RowanMethodDefinitionService sampleDefinitions.
	self assert: services first source = 'sampleMethod ^''some text'''.
	self assert: services first selector = 'sampleMethod'.
	self assert: services size = 1!

updateMethodSource: aString
	| documentPresenter |
	documentPresenter := shell primaryPresenter methodSourcePresenter documentPresenter.
	documentPresenter model value: aString.
	documentPresenter isModified: true.
	shell primaryPresenter methodSourcePresenter fileSave. ! !
!RowanDefinitionServiceTest categoriesFor: #browseImplementorsOf:!browsing!private! !
!RowanDefinitionServiceTest categoriesFor: #firstListElement!private!support! !
!RowanDefinitionServiceTest categoriesFor: #setUp!private!support! !
!RowanDefinitionServiceTest categoriesFor: #tearDown!private!support! !
!RowanDefinitionServiceTest categoriesFor: #test_addNewMethod!public! !
!RowanDefinitionServiceTest categoriesFor: #test_createSampleMethod!public! !
!RowanDefinitionServiceTest categoriesFor: #test_openMethodBrowser!public! !
!RowanDefinitionServiceTest categoriesFor: #test_sampleMethodDefinitions!public! !
!RowanDefinitionServiceTest categoriesFor: #updateMethodSource:!private! !

