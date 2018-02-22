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

test_sampleMethodDefinitions
	"test values hard coded on remote GemStone server"

	| services |
	services := RowanMethodDefinitionService sampleDefinitions.
	self assert: services first source = 'sampleMethod ^''some text'''.
	self assert: services first selector = 'sampleMethod'.
	self assert: services size = 1! !
!RowanDefinitionServiceTest categoriesFor: #test_sampleMethodDefinitions!public! !

