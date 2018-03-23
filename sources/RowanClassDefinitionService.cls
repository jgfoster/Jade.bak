"Filed out from Dolphin Smalltalk 7"!

RowanDefinitionService subclass: #RowanClassDefinitionService
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanClassDefinitionService guid: (GUID fromString: '{ab96bbd9-d725-4add-b635-94cec9f12a19}')!
RowanClassDefinitionService comment: ''!
!RowanClassDefinitionService categoriesForClass!Kernel-Objects! !
!RowanClassDefinitionService methodsFor!

classCreationTemplate

	^transport send: #classCreationTemplate to: self remoteService!

createSampleClass
	transport send: #createSampleClass to: remoteService.  !

remoteService
	| stream |
	stream := WriteStream on: String new.
	stream nextPutAll: 'RowanClassDefinitionService new '.
	^transport executeString: stream contents!

sampleClassName
	^transport send: #sampleClassName to: remoteService.  ! !
!RowanClassDefinitionService categoriesFor: #classCreationTemplate!public! !
!RowanClassDefinitionService categoriesFor: #createSampleClass!public! !
!RowanClassDefinitionService categoriesFor: #remoteService!private! !
!RowanClassDefinitionService categoriesFor: #sampleClassName!public! !

!RowanClassDefinitionService class methodsFor!

on: className
	^self new
		name: className;
		yourself! !
!RowanClassDefinitionService class categoriesFor: #on:!instance creation!public! !

