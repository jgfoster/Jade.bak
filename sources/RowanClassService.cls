"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanClassService
	instanceVariableNames: ''
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

removeSelector: selector

	transport send: #removeSelector: to: remoteService withAll: (Array with: selector)!

sampleClassName
	^transport send: #sampleClassName to: remoteService.  !

subclassCreationTemplate
	^transport send: #subclassCreationTemplate to: self remoteService!

subclassCreationTemplate: packageName
	^transport
		send: #subclassCreationTemplate:
		to: self remoteService
		withAll: (Array with: packageName)! !
!RowanClassService categoriesFor: #classCreationTemplate!public! !
!RowanClassService categoriesFor: #createSampleClass!public! !
!RowanClassService categoriesFor: #removeSelector:!public!rowan! !
!RowanClassService categoriesFor: #sampleClassName!public! !
!RowanClassService categoriesFor: #subclassCreationTemplate!public! !
!RowanClassService categoriesFor: #subclassCreationTemplate:!public! !

