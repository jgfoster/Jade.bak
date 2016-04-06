"Filed out from Dolphin Smalltalk 7"!

Toolbar subclass: #ToolbarA
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ToolbarA guid: (GUID fromString: '{E387189D-713F-4B04-8016-160928617636}')!
ToolbarA comment: ''!
!ToolbarA categoriesForClass!MVP-Views! !
!ToolbarA methodsFor!

commandSource
	"Answer the <View> which is the starting point for routing commands issued by the receiver.
	Delegate to the parent."

	^self parentView commandSource! !
!ToolbarA categoriesFor: #commandSource!commands!public! !

