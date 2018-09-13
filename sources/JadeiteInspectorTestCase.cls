"Filed out from Dolphin Smalltalk 7"!

JadeiteAbstractTestCase subclass: #JadeiteInspectorTestCase
	instanceVariableNames: 'inspector'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteInspectorTestCase guid: (GUID fromString: '{73901ab8-5805-4614-b9e4-7092e55c6317}')!
JadeiteInspectorTestCase comment: ''!
!JadeiteInspectorTestCase categoriesForClass!Unclassified! !
!JadeiteInspectorTestCase methodsFor!

commandQueryOn: commandSymbol
	^CommandQuery new commandDescription: (CommandDescription new command: commandSymbol)
		source: inspector view!

enableForward: forwardQuery andBack: backQuery
	"menu items are disabled based on the command query. Simulate a button enablement
	by creating a command query and evaluating the enablement after passing it through the
	enabler method"

	inspector queryCommand: backQuery.
	inspector queryCommand: forwardQuery!

inspectInstVarAt: index
	| instVarPresenter |
	instVarPresenter := inspector instVarListPresenter.
	instVarPresenter selection: (instVarPresenter list at: index).
	inspector inspectInstVar. !

tearDown
	inspector ifNotNil: [inspector destroy]!

test_caption
	| remoteObject |
	remoteObject := session
				executeString: '123'
				fromContext: nil
				environment: 0.
	inspector := JadeInspector showOn: remoteObject session: session.
	self assert: ('Inspector on SmallInteger for*' match: inspector caption).!

test_captionChange
	| remoteObject instVarPresenter |
	self testsIssue: #issue154 withTitle: 'Change inspector title on dive #154'.
	remoteObject := session
				executeString: 'Array with: (Dictionary new at: #oc put: (OrderedCollection with: #leaf); yourself)'
				fromContext: nil
				environment: 0.
	inspector := JadeInspector showOn: remoteObject session: session.
	self assert: ('Inspector on Array for*' match: inspector caption).
	self inspectInstVarAt: 3.
	self assert: ('Inspector on Dictionary for*' match: inspector caption).
	self inspectInstVarAt: 3.
	self assert: ('Inspector on OrderedCollection for*' match: inspector caption).
	self inspectInstVarAt: 3.
	self assert: ('Inspector on Symbol for*' match: inspector caption).
	inspector getPreviousObject.
	self assert: ('Inspector on OrderedCollection for*' match: inspector caption).
	inspector getPreviousObject.
	self assert: ('Inspector on Dictionary for*' match: inspector caption).
	inspector getPreviousObject.
	self assert: ('Inspector on Array for*' match: inspector caption)!

test_forwardBack
	| remoteObject backQuery forwardQuery |
	self testsIssue: #issue153 withTitle: 'Inspector Back goes too far'.
	remoteObject := session
				executeString: 'Array with: (Dictionary new at: #oc put: (OrderedCollection with: #leaf); yourself)'
				fromContext: nil
				environment: 0.
	inspector := JadeInspector showOn: remoteObject session: session.
	backQuery := self commandQueryOn: #getPreviousObject.
	forwardQuery := self commandQueryOn: #getNextObject.
	self enableForward: forwardQuery andBack: backQuery.
	self deny: backQuery isEnabled.
	self deny: forwardQuery isEnabled.
	self inspectInstVarAt: 3.	"dictionary"
	self enableForward: forwardQuery andBack: backQuery.
	self assert: backQuery isEnabled.
	self deny: forwardQuery isEnabled.
	self inspectInstVarAt: 3.	"ordered collection"
	self enableForward: forwardQuery andBack: backQuery.
	self assert: backQuery isEnabled.
	self deny: forwardQuery isEnabled.
	self inspectInstVarAt: 3.	"symbol"
	self enableForward: forwardQuery andBack: backQuery.
	self assert: backQuery isEnabled.
	self deny: forwardQuery isEnabled.
	inspector getPreviousObject. "ordered collection"
	self enableForward: forwardQuery andBack: backQuery.
	self assert: backQuery isEnabled.
	self assert: forwardQuery isEnabled.
	inspector getPreviousObject. "dictionary"
	self enableForward: forwardQuery andBack: backQuery.
	self assert: backQuery isEnabled.
	self assert: forwardQuery isEnabled.
	inspector getPreviousObject. "array - root" 
	self enableForward: forwardQuery andBack: backQuery.
	self deny: backQuery isEnabled.
	self assert: forwardQuery isEnabled.
!

test_inspectString
	| remoteObject |
	remoteObject := session
				executeString: '''abc'''
				fromContext: nil
				environment: 0.
	inspector := JadeInspector showOn: remoteObject session: session.
	self assert: (inspector isKindOf: JadeInspector).
	self assert: inspector model = 'abc' ! !
!JadeiteInspectorTestCase categoriesFor: #commandQueryOn:!private! !
!JadeiteInspectorTestCase categoriesFor: #enableForward:andBack:!private! !
!JadeiteInspectorTestCase categoriesFor: #inspectInstVarAt:!private! !
!JadeiteInspectorTestCase categoriesFor: #tearDown!private! !
!JadeiteInspectorTestCase categoriesFor: #test_caption!public!test! !
!JadeiteInspectorTestCase categoriesFor: #test_captionChange!public!test! !
!JadeiteInspectorTestCase categoriesFor: #test_forwardBack!public!test! !
!JadeiteInspectorTestCase categoriesFor: #test_inspectString!public!test! !

