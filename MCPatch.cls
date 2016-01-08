"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #MCPatch
	instanceVariableNames: 'operations'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCPatch guid: (GUID fromString: '{F78F623F-4CC4-4E69-BC3D-F6320FC96009}')!
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

