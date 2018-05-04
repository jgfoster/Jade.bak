"Filed out from Dolphin Smalltalk 7"!

Model subclass: #RowanService
	instanceVariableNames: 'transport remoteService name isDirty'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService guid: (GUID fromString: '{c91bf577-a5a9-4782-b6be-c05df3222bc9}')!
RowanService comment: ''!
!RowanService categoriesForClass!Kernel-Objects! !
!RowanService methodsFor!

classNameFrom: aClassOop

	^transport send: #name to: aClassOop!

clientDefinitionClass

	^self subclassResponsibility!

createClientFrom: anArrayOop
	"ignores round trips but is straight forward"

	| services remoteSize |
	remoteSize := transport send: #size to: anArrayOop.
	services := Array new: remoteSize.
	1 to: remoteSize
		do: 
			[:idx |
			| elementOop |
			elementOop := transport
						send: #at:
						to: anArrayOop
						withAll: (Array with: idx).
			services at: idx put: (self createClientFromRemote: elementOop)].
	^services!

createClientFromRemote: oop
	"expect simple objects to be replicated in each
	slot of our local definition object"

	| clientDefinition |
	clientDefinition := self clientDefinitionClass new.
	self remoteAccessors do: 
			[:symbol |
			| value |
			value := transport send: symbol to: oop.
			clientDefinition perform: (symbol , ':') asSymbol with: value].
	^clientDefinition!

initialize 
	super initialize.
	self initializeTransport.
	remoteService := transport executeString: self remoteServiceName , ' new'!

initializeTransport
	"keep sessions properly invalidated and this
	will work. Need to initialize this better eventually.

	Do This: 
	1. From the Jade Transcript -> Exit Jade.
	2. Evaluate -> GciSession allInstances do:[:each | each beInvalidSession].
	3. If the Jade Login is not open -> JadeLoginShell show. 
	4. From the Jade Shell -> Login"

	transport := GciSession allInstances detect: [:session | session isValidSession] ifNone:[nil].!

inspectDefinition
	| definition |
	definition := transport send: #definition to: remoteService.
	JadeInspector showOn: definition session: transport!

isDirty
	^isDirty!

isDirty: anObject
	isDirty := anObject!

name
	^name!

name: aString
	name := aString.
	transport send: #name: to: remoteService withAll: (Array with: aString)!

remoteAccessors

	^self subclassResponsibility!

remoteService
	^remoteService!

remoteServiceName
	"The remote service name is the same as this class"

	^self class name!

removeSampleSymbolDictionary

	transport send: #removeSampleSymbolDictionary to: remoteService!

sampleDefinitions
	| anArrayOop |
	anArrayOop := transport send: #sampleDefinitions to: remoteService.
	^self createClientFrom: anArrayOop!

setRemoteName

	transport send: #name: to: remoteService withAll: (Array with: name)!

transport
	^transport!

transport: anObject
	transport := anObject! !
!RowanService categoriesFor: #classNameFrom:!public! !
!RowanService categoriesFor: #clientDefinitionClass!private! !
!RowanService categoriesFor: #createClientFrom:!private! !
!RowanService categoriesFor: #createClientFromRemote:!private! !
!RowanService categoriesFor: #initialize!initialization!private! !
!RowanService categoriesFor: #initializeTransport!initialization!private! !
!RowanService categoriesFor: #inspectDefinition!public!rowan! !
!RowanService categoriesFor: #isDirty!accessing!private! !
!RowanService categoriesFor: #isDirty:!accessing!private! !
!RowanService categoriesFor: #name!accessing!private! !
!RowanService categoriesFor: #name:!accessing!private! !
!RowanService categoriesFor: #remoteAccessors!private! !
!RowanService categoriesFor: #remoteService!public! !
!RowanService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanService categoriesFor: #removeSampleSymbolDictionary!public! !
!RowanService categoriesFor: #sampleDefinitions!examples!public! !
!RowanService categoriesFor: #setRemoteName!accessing!public! !
!RowanService categoriesFor: #transport!accessing!private! !
!RowanService categoriesFor: #transport:!accessing!private! !

!RowanService class methodsFor!

named: projectName
	| inst |
	inst := self new name: projectName.
	inst setRemoteName.
	^inst!

new
	^super new initialize!

on: aSession

	^self new transport: aSession!

sampleDefinitions
	^self new sampleDefinitions! !
!RowanService class categoriesFor: #named:!instance creation!public! !
!RowanService class categoriesFor: #new!initialization!public! !
!RowanService class categoriesFor: #on:!initialization!public! !
!RowanService class categoriesFor: #sampleDefinitions!examples!public! !

