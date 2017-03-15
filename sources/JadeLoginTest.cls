"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #JadeLoginTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeLoginTest guid: (GUID fromString: '{c02b6503-ffd6-49cf-a51e-8a389fb7e5f8}')!
JadeLoginTest comment: 'Transcript:
	Transcript show: ''Transcript test''.	"Copy, Delete"
My Session:
	UserGlobals at: #''James'' put: 42.	"Commit"
	UserGlobals at: #''James''.
	UserGlobals at: #''James'' put: 24.
	UserGlobals at: #''James''.				"Abort"
	UserGlobals at: #''James''.
All Sessions:
	"Menus"
	"Auto-update"
Workspace:
	2 + 3.	"Display, Execute, Inspect"
	2 -> 3.	"Inspect"
		self key.
		self value.
User Browser:
	"New user: Seaside"
	"Missing edit of Details & Password"
'!
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

