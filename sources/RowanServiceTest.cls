"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #RowanServiceTest
	instanceVariableNames: 'shell methodService gciSession classService'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanServiceTest guid: (GUID fromString: '{f46b0d8e-4841-4dc3-8f02-bcbdd5083fb7}')!
RowanServiceTest comment: ''!
!RowanServiceTest categoriesForClass!SUnit! !
!RowanServiceTest methodsFor!

browseClassNamed: className

	shell := RowanSystemBrowser showOnSession: gciSession.
	shell selectClass: className selector: String new.

	

!

browseImplementorsOf: aString
	| gciSession |
	gciSession := RowanService new transport.
	shell := JadeMethodListBrowser showOnSession: gciSession.
	shell primaryPresenter
		browseImplementorsOf: aString;
		yourself!

compileMethodSource: aString
	| documentPresenter |
	documentPresenter := shell primaryPresenter methodSourcePresenter documentPresenter.
	documentPresenter model value: aString.
	documentPresenter isModified: true.
	shell primaryPresenter methodSourcePresenter fileSave!

firstListElement
	^shell primaryPresenter methodListPresenter subPresenters first model list first!

removeSampleProject
	"ensure the test project is removed from Rowan"

	| service |
	service := RowanProjectService new.
	service removeSampleProject.
	service removeSampleSymbolDictionary!

setUp
	super setUp.
	methodService := RowanMethodService new.
	classService := RowanClassService new.
	gciSession := methodService transport.
	self removeSampleProject!

systemBrowserPresenter
	^shell currentCard!

tearDown
	super tearDown.
	shell ifNotNil: [:shell | shell exit].
	RowanPackageService new removeSampleSymbolDictionary.
	methodService transport abort.
	self removeSampleProject!

test_addNewMethod
	| instanceOop |
	methodService createSampleMethod.
	instanceOop := methodService sampleClassInstance.
	self should: [gciSession send: #sampleMethod2 to: instanceOop] raise: GsDoesNotUnderstand.
	self browseImplementorsOf: 'sampleMethod'.	"first list element should be selected"  
	self compileMethodSource: 'testMethod
		^123'.
	self assert: (gciSession send: #testMethod to: instanceOop) = 123.!

test_createSampleClass
	classService createSampleClass.
	self browseClassNamed: classService sampleClassName.
	self assert: self systemBrowserPresenter selectedClassNameWithoutVersion = classService sampleClassName !

test_createSampleMethod
	"creating the sample method through Rowan will test
	that Rowan created a method, class, package, and project.
	It should also compile the method behind the scenes such
	that an old style implementors call will discover it. "

	| listElement | 
	methodService createSampleMethod. 
	self browseImplementorsOf: 'sampleMethod'.
	self assert: (shell isKindOf: JadeMethodListBrowser).
	listElement := self firstListElement.
	self assert: listElement gsClass name = 'SampleClassName'.
	self assert: listElement name = 'sampleMethod'.
	self assert: listElement category = 'sample'.  
	self assert: listElement packageName = 'SamplePackageName'!

test_openMethodBrowser
	| gciSession |
	gciSession := RowanService new transport.
	shell := JadeMethodListBrowser showOnSession: gciSession.
	self assert: (shell isKindOf: RowanMethodListBrowser).
	self assert: (shell primaryPresenter isKindOf: RowanMethodListPresenter)!

test_sampleMethodDefinitions
	"test values hard coded on remote GemStone server"

	| services |
	services := RowanMethodService sampleDefinitions.
	self assert: services first source = 'sampleMethod ^''some text'''.
	self assert: services first selector = 'sampleMethod'.
	self assert: services size = 1! !
!RowanServiceTest categoriesFor: #browseClassNamed:!browsing!private! !
!RowanServiceTest categoriesFor: #browseImplementorsOf:!browsing!private! !
!RowanServiceTest categoriesFor: #compileMethodSource:!private! !
!RowanServiceTest categoriesFor: #firstListElement!private!support! !
!RowanServiceTest categoriesFor: #removeSampleProject!private!support! !
!RowanServiceTest categoriesFor: #setUp!private!support! !
!RowanServiceTest categoriesFor: #systemBrowserPresenter!private!support! !
!RowanServiceTest categoriesFor: #tearDown!private!support! !
!RowanServiceTest categoriesFor: #test_addNewMethod!public! !
!RowanServiceTest categoriesFor: #test_createSampleClass!public! !
!RowanServiceTest categoriesFor: #test_createSampleMethod!public! !
!RowanServiceTest categoriesFor: #test_openMethodBrowser!public! !
!RowanServiceTest categoriesFor: #test_sampleMethodDefinitions!public! !

