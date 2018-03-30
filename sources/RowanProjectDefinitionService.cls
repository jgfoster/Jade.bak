"Filed out from Dolphin Smalltalk 7"!

RowanDefinitionService subclass: #RowanProjectDefinitionService
	instanceVariableNames: 'sha branch'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectDefinitionService guid: (GUID fromString: '{d65732a3-f4fb-489b-ae7e-d104cddbf3d8}')!
RowanProjectDefinitionService comment: ''!
!RowanProjectDefinitionService categoriesForClass!Unclassified! !
!RowanProjectDefinitionService methodsFor!

branch
	^branch!

branch: anObject
	branch := anObject!

clientDefinitionClass

	^self class!

projects
	| names |
	names := transport send: #projectNames to: remoteService.
	^self createClientDefinitionFrom: names!

remoteAccessors
	^#(#name #sha #branch)!

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
	^transport send: #sampleProjectName to: self remoteService!

sha
	^sha!

sha: anObject
	sha := anObject! !
!RowanProjectDefinitionService categoriesFor: #branch!accessing!public! !
!RowanProjectDefinitionService categoriesFor: #branch:!accessing!public! !
!RowanProjectDefinitionService categoriesFor: #clientDefinitionClass!private! !
!RowanProjectDefinitionService categoriesFor: #projects!public!rowan! !
!RowanProjectDefinitionService categoriesFor: #remoteAccessors!public! !
!RowanProjectDefinitionService categoriesFor: #remoteService!private! !
!RowanProjectDefinitionService categoriesFor: #removeProjectNamed:!public! !
!RowanProjectDefinitionService categoriesFor: #removeSampleProject!public! !
!RowanProjectDefinitionService categoriesFor: #sampleProjectName!public! !
!RowanProjectDefinitionService categoriesFor: #sha!accessing!public! !
!RowanProjectDefinitionService categoriesFor: #sha:!accessing!public! !

!RowanProjectDefinitionService class methodsFor!

defaultIcon
	"Answer a suitable default icon to use for this class. Not all classes use their 
	default icon; one must define an appropriate #icon method in each class where the 
	icon required differs from that of the superclass."

	^(self environment at: #Icon) fromId: self defaultIconName.
!

defaultIconName
	"Answer a filename to use for an icon of this class."

	^File composeStem: 'Collection' extension: 'ico'.!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon! !
!RowanProjectDefinitionService class categoriesFor: #defaultIcon!private! !
!RowanProjectDefinitionService class categoriesFor: #defaultIconName!private! !
!RowanProjectDefinitionService class categoriesFor: #icon!private! !

