"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanProjectService
	instanceVariableNames: 'sha branch isSkew'
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
		to: remoteService
		withAll: (Array with: projectName with: self class defaultProjectName).!

branch
	^branch!

branch: anObject
	branch := anObject!

changes

	^transport send: #changes to: remoteService  !

clientDefinitionClass

	^self class!

commitWithMessage: message

	transport send: #commitWithMessage: to: remoteService withAll: (Array with: message)  !

defaultProjectName: aString
	"The server is the gold copy"

	transport
		send: #defaultProjectName:
		to: remoteService
		withAll: (Array with: aString).
	self class defaultProjectName: aString!

isSkew
	^isSkew!

isSkew: anObject
	isSkew := anObject!

loadProjectNamed: aName
	
	transport
		send: #loadProjectNamed:
		to: remoteService
		withAll: (Array with: aName)!

log

	^transport send: #log to: remoteService  !

nameWithDefaultIndication
	^name = self class defaultProjectName ifTrue: [name , '*'] ifFalse: [name]!

newGitProject: url root: rootPath useSsh: aBoolean
	
	transport
		send: #newGitProject:root:useSsh:
		to: remoteService
		withAll: (Array
				with: url
				with: rootPath
				with: aBoolean)!

packageNameString

	^transport send: #packageNameString to: remoteService  !

projects
	| oops services |
	oops := transport send: #projectNames to: remoteService.
	services := self createClientFrom: oops.
	^services!

pullFromGit

	^transport send: #pullFromGit to: remoteService  !

pushToGit

	^transport send: #pushToGit to: remoteService  !

remoteAccessors
	^#(#name #sha #branch #isSkew)!

removeProjectNamed: projectName

	transport send: #removeProjectNamed: to: remoteService withAll: (Array with: projectName)!

removeSampleProject
	self removeProjectNamed: self sampleProjectName!

sampleProjectName
	^transport send: #sampleProjectName to: remoteService!

sha
	^sha!

sha: anObject
	sha := anObject!

write

	^transport send: #write to: remoteService  ! !
!RowanProjectService categoriesFor: #addProjectNamed:!public! !
!RowanProjectService categoriesFor: #branch!accessing!public! !
!RowanProjectService categoriesFor: #branch:!accessing!public! !
!RowanProjectService categoriesFor: #changes!public!rowan! !
!RowanProjectService categoriesFor: #clientDefinitionClass!private! !
!RowanProjectService categoriesFor: #commitWithMessage:!public!rowan! !
!RowanProjectService categoriesFor: #defaultProjectName:!accessing!public! !
!RowanProjectService categoriesFor: #isSkew!accessing!private! !
!RowanProjectService categoriesFor: #isSkew:!accessing!private! !
!RowanProjectService categoriesFor: #loadProjectNamed:!public!rowan! !
!RowanProjectService categoriesFor: #log!public!rowan! !
!RowanProjectService categoriesFor: #nameWithDefaultIndication!accessing!public! !
!RowanProjectService categoriesFor: #newGitProject:root:useSsh:!public!rowan! !
!RowanProjectService categoriesFor: #packageNameString!public!rowan! !
!RowanProjectService categoriesFor: #projects!public!rowan! !
!RowanProjectService categoriesFor: #pullFromGit!public!rowan! !
!RowanProjectService categoriesFor: #pushToGit!public!rowan! !
!RowanProjectService categoriesFor: #remoteAccessors!public! !
!RowanProjectService categoriesFor: #removeProjectNamed:!public! !
!RowanProjectService categoriesFor: #removeSampleProject!public! !
!RowanProjectService categoriesFor: #sampleProjectName!public! !
!RowanProjectService categoriesFor: #sha!accessing!public! !
!RowanProjectService categoriesFor: #sha:!accessing!public! !
!RowanProjectService categoriesFor: #write!public!rowan! !

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

	^##(self) defaultIcon!

initialize
	DefaultProjectName := nil! !
!RowanProjectService class categoriesFor: #defaultIcon!private! !
!RowanProjectService class categoriesFor: #defaultIconName!private! !
!RowanProjectService class categoriesFor: #defaultProjectName!public! !
!RowanProjectService class categoriesFor: #defaultProjectName:!public! !
!RowanProjectService class categoriesFor: #icon!private! !
!RowanProjectService class categoriesFor: #initialize!public! !

