"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanProjectService
	instanceVariableNames: 'sha branch'
	classVariableNames: 'DefaultProjectName'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectService guid: (GUID fromString: '{d65732a3-f4fb-489b-ae7e-d104cddbf3d8}')!
RowanProjectService comment: ''!
!RowanProjectService categoriesForClass!Unclassified! !
!RowanProjectService methodsFor!

addProjectNamed: projectName
	transport
		send: #createProjectNamed:in:
		to: self remoteService
		withAll: (Array with: projectName with: self class defaultProjectName).!

branch
	^branch!

branch: anObject
	branch := anObject!

changes

	^transport send: #changes to: remoteService  !

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
!RowanProjectService categoriesFor: #addProjectNamed:!public! !
!RowanProjectService categoriesFor: #branch!accessing!public! !
!RowanProjectService categoriesFor: #branch:!accessing!public! !
!RowanProjectService categoriesFor: #changes!public!rowan! !
!RowanProjectService categoriesFor: #clientDefinitionClass!private! !
!RowanProjectService categoriesFor: #defaultProjectName:!accessing!public! !
!RowanProjectService categoriesFor: #nameWithDefaultIndication!accessing!public! !
!RowanProjectService categoriesFor: #projects!public!rowan! !
!RowanProjectService categoriesFor: #remoteAccessors!public! !
!RowanProjectService categoriesFor: #remoteService!private! !
!RowanProjectService categoriesFor: #removeProjectNamed:!public! !
!RowanProjectService categoriesFor: #removeSampleProject!public! !
!RowanProjectService categoriesFor: #sampleProjectName!public! !
!RowanProjectService categoriesFor: #sha!accessing!public! !
!RowanProjectService categoriesFor: #sha:!accessing!public! !

!RowanProjectService class methodsFor!

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
!RowanProjectService class categoriesFor: #defaultIcon!private! !
!RowanProjectService class categoriesFor: #defaultIconName!private! !
!RowanProjectService class categoriesFor: #defaultProjectName!public! !
!RowanProjectService class categoriesFor: #defaultProjectName:!public! !
!RowanProjectService class categoriesFor: #icon!private! !

