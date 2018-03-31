"Filed out from Dolphin Smalltalk 7"!

RowanDefinitionService subclass: #RowanProjectDefinitionService
	instanceVariableNames: 'sha branch'
	classVariableNames: 'DefaultProjectName'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectDefinitionService guid: (GUID fromString: '{d65732a3-f4fb-489b-ae7e-d104cddbf3d8}')!
RowanProjectDefinitionService comment: ''!
!RowanProjectDefinitionService categoriesForClass!Unclassified! !
!RowanProjectDefinitionService methodsFor!

addProjectNamed: projectName
	transport
		send: #createProjectNamed:in:
		to: self remoteService
		withAll: (Array with: projectName with: self class defaultProjectName).!

branch
	^branch!

branch: anObject
	branch := anObject!

clientDefinitionClass

	^self class!

defaultProjectName: aString
	"The server is the gold copy"

	transport
		send: #defaultProjectName:
		to: self remoteService
		withAll: (Array with: aString).
	self class defaultProjectName: aString!

nameWithDefaultIndication
	^name = self class defaultProjectName ifTrue: [name , '*'] ifFalse: [name]!

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
!RowanProjectDefinitionService categoriesFor: #addProjectNamed:!public! !
!RowanProjectDefinitionService categoriesFor: #branch!accessing!public! !
!RowanProjectDefinitionService categoriesFor: #branch:!accessing!public! !
!RowanProjectDefinitionService categoriesFor: #clientDefinitionClass!private! !
!RowanProjectDefinitionService categoriesFor: #defaultProjectName:!accessing!public! !
!RowanProjectDefinitionService categoriesFor: #nameWithDefaultIndication!accessing!public! !
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

defaultProjectName
	"cached from the server class var"

	^DefaultProjectName!

defaultProjectName: aString

	DefaultProjectName := aString!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon! !
!RowanProjectDefinitionService class categoriesFor: #defaultIcon!private! !
!RowanProjectDefinitionService class categoriesFor: #defaultIconName!private! !
!RowanProjectDefinitionService class categoriesFor: #defaultProjectName!public! !
!RowanProjectDefinitionService class categoriesFor: #defaultProjectName:!public! !
!RowanProjectDefinitionService class categoriesFor: #icon!private! !

