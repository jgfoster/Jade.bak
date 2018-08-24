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

test_inspectString
	| remoteObject |
	remoteObject := session
				executeString: '''abc'''
				fromContext: nil
				environment: 0.
	inspector := JadeInspector showOn: remoteObject session: session.
	self assert: (inspector isKindOf: JadeInspector).
	self assert: inspector model = 'abc' ! !
!JadeiteInspectorTestCase categoriesFor: #tearDown!private! !
!JadeiteInspectorTestCase categoriesFor: #test_caption!public!test! !
!JadeiteInspectorTestCase categoriesFor: #test_inspectString!public!test! !

