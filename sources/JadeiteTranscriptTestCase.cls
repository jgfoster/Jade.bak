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

test_multiSelectProject
	| projectServices projectCount |
	self testsIssue: #issue163
		withTitle: 'Project list in transcript/console should be multi-select for multi-load'.
	projectServices := transcript projectListPresenter projectListPresenter list.
	projectCount := projectServices size.
	transcript projectListPresenter projectListPresenter selections: projectServices.
	self assert: transcript projectListPresenter projectListPresenter selections size equals: projectCount!

test_transcriptNamedConsole
	self testsIssue: #issue161 withTitle: 'Rename transcrip to console'.
	self assert: ('Console*' match: transcript view caption)! !
!JadeiteTranscriptTestCase categoriesFor: #test_multiSelectProject!public!tests! !
!JadeiteTranscriptTestCase categoriesFor: #test_transcriptNamedConsole!public!tests! !

!JadeiteTranscriptTestCase class methodsFor!

resources
	^IdentitySet with: JadeiteTestResource! !
!JadeiteTranscriptTestCase class categoriesFor: #resources!public! !

