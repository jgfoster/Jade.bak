"Filed out from Dolphin Smalltalk 7"!

Object subclass: #RowanDefinitionService
	instanceVariableNames: 'definitionOop transport'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanDefinitionService guid: (GUID fromString: '{c91bf577-a5a9-4782-b6be-c05df3222bc9}')!
RowanDefinitionService comment: ''!
!RowanDefinitionService categoriesForClass!Kernel-Objects! !
!RowanDefinitionService methodsFor!

definitionOop
	^definitionOop!

definitionOop: anObject
	definitionOop := anObject!

initializeFromOop: anOop

	^self subclassResponsibility!

initializeTransport
	"Need to initialize this better."

	^transport := GciSession allInstances first!

remoteDefinitionClassName
	^transport rowanRemoteDefinitionNameFor: definitionOop!

sampleServiceOop
	^transport executeString: self class name asString , ' sampleService'!

transport
	^transport!

transport: anObject
	^transport := anObject! !
!RowanDefinitionService categoriesFor: #definitionOop!accessing!private! !
!RowanDefinitionService categoriesFor: #definitionOop:!accessing!private! !
!RowanDefinitionService categoriesFor: #initializeFromOop:!initialization!public! !
!RowanDefinitionService categoriesFor: #initializeTransport!private! !
!RowanDefinitionService categoriesFor: #remoteDefinitionClassName!accessing!public! !
!RowanDefinitionService categoriesFor: #sampleServiceOop!examples!public! !
!RowanDefinitionService categoriesFor: #transport!accessing!private! !
!RowanDefinitionService categoriesFor: #transport:!accessing!private! !

!RowanDefinitionService class methodsFor!

sampleService
	| service |
	service := self new.
	service transport: service initializeTransport.
	service initializeFromOop: service sampleServiceOop.
	^service! !
!RowanDefinitionService class categoriesFor: #sampleService!examples!public! !

