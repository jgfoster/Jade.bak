"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #JadeLoginTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeLoginTest guid: (GUID fromString: '{C02B6503-FFD6-49CF-A51E-8A389FB7E5F8}')!
JadeLoginTest comment: ''!
!JadeLoginTest categoriesForClass!SUnit! !
!JadeLoginTest methodsFor!

loginShell

	|  list |
	list := JadeLoginShell allInstances.
	list isEmpty ifTrue: [
		| additionalTools jadeTool |
		additionalTools := Smalltalk developmentSystem systemFolder childrenOf: Smalltalk developmentSystem additionalToolsFolder.
		jadeTool := additionalTools detect: [:each | each description = 'Jade Login'].
		jadeTool openBlock value: nil value: nil.
		list := JadeLoginShell allInstances.
		self assert: list size == 1.
	].
	^list at: 1.
!

test

	| transcript |
	transcript := self transcriptShell.
!

transcriptShell

	| list |
	list := JadeTranscript allInstances.
	list isEmpty ifTrue: [
		self loginShell login.
		list := JadeTranscript allInstances.
	].
	^list first.
! !
!JadeLoginTest categoriesFor: #loginShell!public! !
!JadeLoginTest categoriesFor: #test!public! !
!JadeLoginTest categoriesFor: #transcriptShell!public! !

