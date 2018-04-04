"Filed out from Dolphin Smalltalk 7"!

RowanDefinitionService subclass: #RowanClassService
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

remoteService
	| stream |
	stream := WriteStream on: String new.
	stream nextPutAll: 'RowanClassDefinitionService new '.
	remoteService := transport executeString: stream contents.
	transport send: #name: to: remoteService withAll: (Array with: name).
	^remoteService!

remoteServiceName
	"The remote service name is the same as this class"

	^'RowanClassDefinitionService'!

sampleClassName
	^transport send: #sampleClassName to: remoteService.  ! !
!RowanClassService categoriesFor: #classCreationTemplate!public! !
!RowanClassService categoriesFor: #createSampleClass!public! !
!RowanClassService categoriesFor: #remoteService!private! !
!RowanClassService categoriesFor: #remoteServiceName!examples!private! !
!RowanClassService categoriesFor: #sampleClassName!public! !

!RowanClassService class methodsFor!

on: className
	^self new
		name: className;
		yourself! !
!RowanClassService class categoriesFor: #on:!instance creation!public! !

