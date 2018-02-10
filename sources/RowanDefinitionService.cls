"Filed out from Dolphin Smalltalk 7"!

Object subclass: #RowanDefinitionService
	instanceVariableNames: 'remoteServiceOop gciSession'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanDefinitionService guid: (GUID fromString: '{c91bf577-a5a9-4782-b6be-c05df3222bc9}')!
RowanDefinitionService comment: ''!
!RowanDefinitionService categoriesForClass!Kernel-Objects! !
!RowanDefinitionService class methodsFor!

sampleService
	| service |
	service := self new.
	service gciSession: GciSession allInstances first. "for now"
	


	! !
!RowanDefinitionService class categoriesFor: #sampleService!examples!public! !

