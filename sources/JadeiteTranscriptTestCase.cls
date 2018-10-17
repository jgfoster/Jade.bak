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

test_browseClassMenuItem
	| jadeiteMenu browseClassItem |
	self testsIssue: #issue235 withTitle: 'Need Find Class/Method from console'.
	jadeiteMenu := transcript view menuBar items detect: [:menu | '*Jadeite*' match: menu text]
				ifNone: [].
	self denyIsNil: jadeiteMenu.
	browseClassItem := jadeiteMenu items detect: [:command | '*Browse*Class*' match: command text]
				ifNone: [].
	self denyIsNil: browseClassItem.
	self assert: browseClassItem command equals: #browseClass.
	self assert: (transcript class canUnderstand: #browseClass)!

test_browseImplementors
	| methodListBrowser methodList |
	self testsIssue: #issue235 withTitle: 'Need Find Class/Method from console'.
	TestMessageBox enableJadeiteTestMessageBox.
	
	[TestPrompter plannedResult: 'size'.
	methodListBrowser := transcript browseImplementors.
	methodList := methodListBrowser primaryPresenter methodListPresenter methodListPresenter list.
	self assert: methodList notEmpty.
	self assert: ('Implementors of ''size'' for*' match: methodListBrowser caption).]
			ensure: 
				[TestMessageBox disableJadeiteTestMessageBox.
				methodListBrowser destroy]!

test_browseImplementorsMenuItem
	| jadeiteMenu browseImplementorsItem |
	self testsIssue: #issue235 withTitle: 'Need Find Class/Method from console'.
	jadeiteMenu := transcript view menuBar items detect: [:menu | '*Jadeite*' match: menu text]
				ifNone: [].
	self denyIsNil: jadeiteMenu.
	browseImplementorsItem := jadeiteMenu items
				detect: [:command | '*Browse*Implementors*' match: command text]
				ifNone: [].
	self denyIsNil: browseImplementorsItem.
	self assert: browseImplementorsItem command equals: #browseImplementors.
	self assert: (transcript class canUnderstand: #browseImplementors)!

test_browseSenders
	| methodListBrowser methodList |
	self testsIssue: #issue235 withTitle: 'Need Find Class/Method from console'.
	TestMessageBox enableJadeiteTestMessageBox.
	
	[TestPrompter plannedResult: 'abort'.
	methodListBrowser := transcript browseSenders.
	methodList := methodListBrowser primaryPresenter methodListPresenter methodListPresenter list.
	self assert: methodList notEmpty.
	self assert: ('Senders of ''abort'' for*' match: methodListBrowser caption).]
			ensure: 
				[TestMessageBox disableJadeiteTestMessageBox.
				methodListBrowser destroy]!

test_browseSendersMenuItem
	| jadeiteMenu browseSendersItem |
	self testsIssue: #issue235 withTitle: 'Need Find Class/Method from console'.
	jadeiteMenu := transcript view menuBar items detect: [:menu | '*Jadeite*' match: menu text]
				ifNone: [].
	self denyIsNil: jadeiteMenu.
	browseSendersItem := jadeiteMenu items
				detect: [:command | '*Browse*Senders*' match: command text]
				ifNone: [].
	self denyIsNil: browseSendersItem.
	self assert: browseSendersItem command equals: #browseSenders.
	self assert: (transcript class canUnderstand: #browseSenders)!

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
!JadeiteTranscriptTestCase categoriesFor: #test_browseClassMenuItem!public!tests! !
!JadeiteTranscriptTestCase categoriesFor: #test_browseImplementors!public!tests! !
!JadeiteTranscriptTestCase categoriesFor: #test_browseImplementorsMenuItem!public!tests! !
!JadeiteTranscriptTestCase categoriesFor: #test_browseSenders!public!tests! !
!JadeiteTranscriptTestCase categoriesFor: #test_browseSendersMenuItem!public!tests! !
!JadeiteTranscriptTestCase categoriesFor: #test_multiSelectProject!public!tests! !
!JadeiteTranscriptTestCase categoriesFor: #test_transcriptNamedConsole!public!tests! !

!JadeiteTranscriptTestCase class methodsFor!

resources
	^IdentitySet with: JadeiteTestResource! !
!JadeiteTranscriptTestCase class categoriesFor: #resources!public! !

