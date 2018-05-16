"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanClassService
	instanceVariableNames: 'comment instVarNames classVarNames classInstVarNames superclassName subclassType poolDictionaryNames classType packageService meta'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanClassService guid: (GUID fromString: '{ab96bbd9-d725-4add-b635-94cec9f12a19}')!
RowanClassService comment: ''!
!RowanClassService categoriesForClass!Kernel-Objects! !
!RowanClassService methodsFor!

classCreationTemplate

	^transport send: #classCreationTemplate to: self remoteService!

createSampleClass
	transport send: #createSampleClass to: remoteService.  !

remoteServiceName
	^'Rowan classServiceClass'!

removeSelector: selector

	transport send: #removeSelector: to: remoteService withAll: (Array with: selector)!

sampleClassName
	^transport send: #sampleClassName to: remoteService.  !

subclassCreationTemplate
	^transport send: #subclassCreationTemplate to: self remoteService! !
!RowanClassService categoriesFor: #classCreationTemplate!public! !
!RowanClassService categoriesFor: #createSampleClass!public! !
!RowanClassService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanClassService categoriesFor: #removeSelector:!public!rowan! !
!RowanClassService categoriesFor: #sampleClassName!public! !
!RowanClassService categoriesFor: #subclassCreationTemplate!public! !

!RowanClassService class methodsFor!

replicateSampleService

	| session string |
	session := GciSession allInstances detect:[:ea | ea isValidSession].
	string := session executeString: 'RowanClassService sampleServiceString'.
	^STON fromString: string! !
!RowanClassService class categoriesFor: #replicateSampleService!public! !

