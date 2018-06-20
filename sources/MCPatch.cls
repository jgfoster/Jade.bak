"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #MCPatch
	instanceVariableNames: 'operations'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCPatch guid: (GUID fromString: '{f78f623f-4cc4-4e69-bc3d-f6320fc96009}')!
MCPatch comment: ''!
!MCPatch categoriesForClass!Unclassified! !
!MCPatch methodsFor!

initialize: aString session: aGciSession

	| stream |
	stream := ReadStream on: aString.
	super
		initialize: stream nextLine
		session: aGciSession.
	operations := OrderedCollection new.
	[
		stream atEnd not.
	] whileTrue: [
		operations add: (MCPatchOperation
			fromStream: stream
			session: aGciSession).
	].
!

operations
	^operations! !
!MCPatch categoriesFor: #initialize:session:!public! !
!MCPatch categoriesFor: #operations!public! !

