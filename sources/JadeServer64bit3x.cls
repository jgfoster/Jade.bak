"Filed out from Dolphin Smalltalk 7"!

JadeServer64bit24 subclass: #JadeServer64bit3x
	instanceVariableNames: 'environment'
	classVariableNames: 'Admonition ClientForwarderSend CompileError CompileWarning RubyHash SessionTemps Transcript'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer64bit3x guid: (GUID fromString: '{1dc3debb-81ec-4b7b-872e-82229e88781b}')!
JadeServer64bit3x comment: '(System _sessionStateAt: 3).
GciSession allInstances do: [:each | each initializeServer].'!
!JadeServer64bit3x categoriesForClass!Unclassified! !
!JadeServer64bit3x methodsFor!

_allSelectors

	| allSelectors |
	allSelectors := IdentitySet new.
	self classOrganizer classes do: [:each | 
		allSelectors addAll: (each selectorsForEnvironment: environment); addAll: (each class selectorsForEnvironment: environment).
	].
	^allSelectors!

asString: anObject

	(anObject isKindOf: String) ifTrue: [^anObject].
	(anObject _class name == #'ClientForwarder') ifTrue: [^'aClientForwarder(' , (self asString: anObject clientObject) , ')'].
	^[
		anObject printString.
	] on: Error , Admonition do: [:ex | 
		ex return: '<<printString error: ' , ex description , '>>'.
	].
!

classOrganizer

	| org |
	^(org := classOrganizers at: environment + 1) notNil
		ifTrue: [org]
		ifFalse: [classOrganizers at: environment + 1 put: (ClassOrganizer newForEnvironment: environment)]
!

executeString: aString fromContext: anObject

	[
		^super executeString: aString fromContext: anObject.
	] on: CompileWarning do: [:ex | 
		ex resume.
	].
!

installTranscript

	| sessionTemps |
	Transcript class name == #'TranscriptStreamPortable' ifFalse: [^super installTranscript].
	sessionTemps := SessionTemps current.
	(sessionTemps at: #'TranscriptStream_SessionStream' ifAbsent: [nil]) notNil ifTrue: [^super installTranscript].
	sessionTemps at: #'TranscriptStream_SessionStream' put: self.
!

nextPutAll: anObject

	| exception |
	exception := ClientForwarderSend new 
		receiver: self 
		clientObj: 2
		selector:#'nextPutAll:'
		args: (Array with: (self asString: anObject)).
	exception defaultAction.  "return error direct to GCI"! !
!JadeServer64bit3x categoriesFor: #_allSelectors!Debugger!GsObject!public!System Browser! !
!JadeServer64bit3x categoriesFor: #asString:!public!Transcript! !
!JadeServer64bit3x categoriesFor: #classOrganizer!public! !
!JadeServer64bit3x categoriesFor: #executeString:fromContext:!public! !
!JadeServer64bit3x categoriesFor: #installTranscript!public!Transcript! !
!JadeServer64bit3x categoriesFor: #nextPutAll:!public!Transcript! !

!JadeServer64bit3x class methodsFor!

gsMethodClassName

	^'GsNMethod'
! !
!JadeServer64bit3x class categoriesFor: #gsMethodClassName!public! !

