"Filed out from Dolphin Smalltalk 7"!

Model subclass: #RowanDefinitionService
	instanceVariableNames: 'transport remoteService'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanDefinitionService guid: (GUID fromString: '{c91bf577-a5a9-4782-b6be-c05df3222bc9}')!
RowanDefinitionService comment: ''!
!RowanDefinitionService categoriesForClass!Kernel-Objects! !
!RowanDefinitionService methodsFor!

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
	do this. 
	GciSession allInstances do:[:each | each beInvalidSession].
	JadeLoginShell show. (if the Jade Login is not open)
	Press login"

	transport := GciSession allInstances detect: [:session | session isValidSession] ifNone:[self halt].!

remoteAccessors

	^self subclassResponsibility!

remoteServiceName
	"The remote service name is the same as this class"

	^self class name!

sampleDefinitions
	| anArrayOop |
	anArrayOop := transport send: #sampleDefinitions to: remoteService.
	^self createClientDefinitionFrom: anArrayOop!

transport
	^transport!

transport: anObject
	transport := anObject! !
!RowanDefinitionService categoriesFor: #clientDefinitionClass!private! !
!RowanDefinitionService categoriesFor: #createClientDefinitionFrom:!private! !
!RowanDefinitionService categoriesFor: #createClientDefinitionFromRemoteDefinition:!private! !
!RowanDefinitionService categoriesFor: #initialize!private! !
!RowanDefinitionService categoriesFor: #initializeTransport!private! !
!RowanDefinitionService categoriesFor: #remoteAccessors!private! !
!RowanDefinitionService categoriesFor: #remoteServiceName!examples!private! !
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

