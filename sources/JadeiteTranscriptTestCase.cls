"Filed out from Dolphin Smalltalk 7"!

JadeiteAbstractTestCase subclass: #JadeiteTranscriptTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteTranscriptTestCase guid: (GUID fromString: '{5527e061-53db-41d8-b3ef-bea5af13e6aa}')!
JadeiteTranscriptTestCase comment: ''!
!JadeiteTranscriptTestCase categoriesForClass!Unclassified! !
!JadeiteTranscriptTestCase methodsFor!

test_transcriptNamedConsole
	self testsIssue: #issue161 withTitle: 'Rename transcrip to console'.
	self assert: ('Console*' match: transcript view caption)! !
!JadeiteTranscriptTestCase categoriesFor: #test_transcriptNamedConsole!public!tests! !

!JadeiteTranscriptTestCase class methodsFor!

resources
	^IdentitySet with: JadeiteTestResource! !
!JadeiteTranscriptTestCase class categoriesFor: #resources!public! !

