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

_describeMethod: aMethod

	(aMethod class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , aMethod class name].
	^super _describeMethod: aMethod!

asString: anObject

	(anObject isKindOf: String) ifTrue: [^anObject].
	(anObject _class name == #'ClientForwarder') ifTrue: [^'aClientForwarder(' , (self asString: anObject clientObject) , ')'].
	^[
		anObject printString.
	] on: Error , Admonition do: [:ex | 
		ex return: '<<printString error: ' , ex description , '>>'.
	].
!

debugString: aString fromContext: anObject environment: anInteger

	[
		^super debugString: aString fromContext: anObject environment: anInteger.
	] on: CompileWarning do: [:ex | 
		ex resume.
	].
!

describeMethod: aMethod
	"Provide info needed to create a GsMethod in Jade client"

	(aMethod class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , aMethod class name].
	^super describeMethod: aMethod!

executeString: aString fromContext: anObject environment: anInteger

	[
		^super executeString: aString fromContext: anObject environment: anInteger.
	] on: CompileWarning do: [:ex | 
		ex resume.
	].
!

homeMethodFor: aGsMethod

	| result |
	result := super homeMethodFor: aGsMethod.
	(result class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , result class name].
	^result!

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
!JadeServer64bit3x categoriesFor: #_describeMethod:!public! !
!JadeServer64bit3x categoriesFor: #asString:!public!Transcript! !
!JadeServer64bit3x categoriesFor: #debugString:fromContext:environment:!public! !
!JadeServer64bit3x categoriesFor: #describeMethod:!public! !
!JadeServer64bit3x categoriesFor: #executeString:fromContext:environment:!public! !
!JadeServer64bit3x categoriesFor: #homeMethodFor:!public! !
!JadeServer64bit3x categoriesFor: #installTranscript!public!Transcript! !
!JadeServer64bit3x categoriesFor: #nextPutAll:!public!Transcript! !

!JadeServer64bit3x class methodsFor!

gsMethodClassName

	^'GsNMethod'
! !
!JadeServer64bit3x class categoriesFor: #gsMethodClassName!public! !

