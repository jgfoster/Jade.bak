"Filed out from Dolphin Smalltalk 7"!

RowanDefinitionService subclass: #RowanMethodDefinitionService
	instanceVariableNames: 'methodDefinitions'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodDefinitionService guid: (GUID fromString: '{f5550fdc-dbb4-4382-af82-88c561c0a9d1}')!
RowanMethodDefinitionService comment: ''!
!RowanMethodDefinitionService categoriesForClass!Kernel-Objects! !
!RowanMethodDefinitionService methodsFor!

addDefaultMethod

	| anArrayOop |
	anArrayOop := transport executeString: self remoteServiceName asString , ' new addSampleMethod'.
	^self createClientDefinitionFrom: anArrayOop!

addSampleMethod

	| anArrayOop |
	anArrayOop := transport executeString: self remoteServiceName asString , ' new addSampleMethod'.
	^self createClientDefinitionFrom: anArrayOop!

clientDefinitionClass

	^RowanMethodDefinition!

implementorsOf: aString

	^methodDefinitions!

methodDefinitions
	^methodDefinitions!

methodDefinitions: anObject
	methodDefinitions := anObject.
	self trigger: #changed!

remoteAccessors
	^#(#source #selector)! !
!RowanMethodDefinitionService categoriesFor: #addDefaultMethod!examples!public! !
!RowanMethodDefinitionService categoriesFor: #addSampleMethod!examples!public! !
!RowanMethodDefinitionService categoriesFor: #clientDefinitionClass!private! !
!RowanMethodDefinitionService categoriesFor: #implementorsOf:!method accessing!public! !
!RowanMethodDefinitionService categoriesFor: #methodDefinitions!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #methodDefinitions:!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #remoteAccessors!private! !

