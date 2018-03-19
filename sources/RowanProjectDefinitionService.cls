"Filed out from Dolphin Smalltalk 7"!

RowanDefinitionService subclass: #RowanProjectDefinitionService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectDefinitionService guid: (GUID fromString: '{d65732a3-f4fb-489b-ae7e-d104cddbf3d8}')!
RowanProjectDefinitionService comment: ''!
!RowanProjectDefinitionService categoriesForClass!Unclassified! !
!RowanProjectDefinitionService methodsFor!

remoteService
	| stream |
	stream := WriteStream on: String new.
	stream nextPutAll: 'RowanProjectDefinitionService new '.
	^transport executeString: stream contents!

removeProjectNamed: projectName

	transport send: #removeProjectNamed: to: self remoteService withAll: (Array with: projectName)!

removeSampleProject
	self removeProjectNamed: self sampleProjectName!

sampleProjectName
	^transport send: #sampleProjectName to: self remoteService! !
!RowanProjectDefinitionService categoriesFor: #remoteService!private! !
!RowanProjectDefinitionService categoriesFor: #removeProjectNamed:!public! !
!RowanProjectDefinitionService categoriesFor: #removeSampleProject!public! !
!RowanProjectDefinitionService categoriesFor: #sampleProjectName!public! !

