"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #RowanDefinitionServiceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanDefinitionServiceTest guid: (GUID fromString: '{f46b0d8e-4841-4dc3-8f02-bcbdd5083fb7}')!
RowanDefinitionServiceTest comment: ''!
!RowanDefinitionServiceTest categoriesForClass!SUnit! !
!RowanDefinitionServiceTest methodsFor!

test_sampleMethodDefinition
	"test values hard coded on server"

	| service |
	service := RowanMethodDefinitionService sampleService.
	self assert: service source = 'sampleMethod ^''some text'''.
	self assert: service selector = #sampleMethod! !
!RowanDefinitionServiceTest categoriesFor: #test_sampleMethodDefinition!public! !

