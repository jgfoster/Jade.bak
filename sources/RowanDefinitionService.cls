"Filed out from Dolphin Smalltalk 7"!

Model subclass: #RowanDefinitionService
	instanceVariableNames: 'transport remoteService name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanDefinitionService guid: (GUID fromString: '{c91bf577-a5a9-4782-b6be-c05df3222bc9}')!
RowanDefinitionService comment: ''!
!RowanDefinitionService categoriesForClass!Kernel-Objects! !
!RowanDefinitionService methodsFor!

classNameFrom: aClassOop

	^transport send: #name to: aClassOop!

clientDefinitionClass

	^self subclassResponsibility!

createClientDefinitionFrom: anArrayOop
	"ignores round trips but is straight forward"

	| definitions remoteSize |
	remoteSize := transport send: #size to: anArrayOop.
	definitions := Array new: remoteSize.
	1 to: remoteSize
		do: 
			[:idx |
			| elementOop |
			elementOop := transport
						send: #at:
						to: anArrayOop
						withAll: (Array with: idx).
			definitions at: idx put: (self createClientDefinitionFromRemoteDefinition: elementOop)]. 
	^definitions!

createClientDefinitionFromRemoteDefinition: anOop
	"expect simple objects to be replicated in each
	slot of our local definition object"

	| clientDefinition |
	clientDefinition := self clientDefinitionClass new.
	self remoteAccessors do: 
			[:symbol |
			| value |
			value := transport send: symbol to: anOop.
			clientDefinition perform: (symbol , ':') asSymbol with: value].
	^clientDefinition!

initialize
	
	super initialize.
	self initializeTransport. 
	remoteService := transport send: #new to: (transport executeString: self remoteServiceName)!

initializeTransport
	"keep sessions properly invalidated and this
	will work. Need to initialize this better eventually.

	Do This: 
	1. From the Jade Transcript -> Exit Jade.
	2. Evaluate -> GciSession allInstances do:[:each | each beInvalidSession].
	3. If the Jade Login is not open -> JadeLoginShell show. 
	4. From the Jade Shell -> Login"

	transport := GciSession allInstances detect: [:session | session isValidSession] ifNone:[self halt].!

name
	^name!

name: anObject
	name := anObject!

remoteAccessors

	^self subclassResponsibility!

remoteServiceName
	"The remote service name is the same as this class"

	^self class name!

removeSampleSymbolDictionary

	transport send: #removeSampleSymbolDictionary to: self remoteService!

sampleDefinitions
	| anArrayOop | 
	anArrayOop := transport send: #sampleDefinitions to: remoteService.
	^self createClientDefinitionFrom: anArrayOop!

transport
	^transport!

transport: anObject
	transport := anObject! !
!RowanDefinitionService categoriesFor: #classNameFrom:!public! !
!RowanDefinitionService categoriesFor: #clientDefinitionClass!private! !
!RowanDefinitionService categoriesFor: #createClientDefinitionFrom:!private! !
!RowanDefinitionService categoriesFor: #createClientDefinitionFromRemoteDefinition:!private! !
!RowanDefinitionService categoriesFor: #initialize!initialization!private! !
!RowanDefinitionService categoriesFor: #initializeTransport!initialization!private! !
!RowanDefinitionService categoriesFor: #name!accessing!private! !
!RowanDefinitionService categoriesFor: #name:!accessing!private! !
!RowanDefinitionService categoriesFor: #remoteAccessors!private! !
!RowanDefinitionService categoriesFor: #remoteServiceName!examples!private! !
!RowanDefinitionService categoriesFor: #removeSampleSymbolDictionary!public! !
!RowanDefinitionService categoriesFor: #sampleDefinitions!examples!public! !
!RowanDefinitionService categoriesFor: #transport!accessing!private! !
!RowanDefinitionService categoriesFor: #transport:!accessing!private! !

!RowanDefinitionService class methodsFor!

new
	^super new initialize!

on: aSession

	^self new transport: aSession!

sampleDefinitions
	^self new sampleDefinitions! !
!RowanDefinitionService class categoriesFor: #new!initialization!public! !
!RowanDefinitionService class categoriesFor: #on:!initialization!public! !
!RowanDefinitionService class categoriesFor: #sampleDefinitions!examples!public! !

