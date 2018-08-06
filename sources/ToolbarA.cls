"Filed out from Dolphin Smalltalk 7"!

Toolbar subclass: #ToolbarA
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ToolbarA guid: (GUID fromString: '{e387189d-713f-4b04-8016-160928617636}')!
ToolbarA comment: ''!
!ToolbarA categoriesForClass!MVP-Views! !
!ToolbarA methodsFor!

commandSource
	"Answer the <View> which is the starting point for routing commands issued by the receiver.
	Delegate to the parent."

	^self parentView commandSource! !
!ToolbarA categoriesFor: #commandSource!commands!public! !

