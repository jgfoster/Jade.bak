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
	| gciSession shell |
	gciSession := RowanDefinitionService new transport.
	shell := RowanMethodListBrowser showOnSession: gciSession.
	^(RowanMethodListBrowser showOnSession: self gciSession) primaryPresenter
		browseImplementorsOf: aString;
		yourself!

setUp
	super setUp.
	methodService := RowanMethodDefinitionService new.!

tearDown
	super tearDown.
	shell ifNotNil: [:shell | shell exit]!

test_addMethod

	methodService addSampleMethod. 
	shell := self browseImplementorsOf: 'aTestMethod'.
	self halt. 
	!

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
	self assert: services size = 1! !
!RowanDefinitionServiceTest categoriesFor: #browseImplementorsOf:!private! !
!RowanDefinitionServiceTest categoriesFor: #setUp!private! !
!RowanDefinitionServiceTest categoriesFor: #tearDown!private! !
!RowanDefinitionServiceTest categoriesFor: #test_addMethod!public! !
!RowanDefinitionServiceTest categoriesFor: #test_openMethodBrowser!public! !
!RowanDefinitionServiceTest categoriesFor: #test_sampleMethodDefinitions!public! !

