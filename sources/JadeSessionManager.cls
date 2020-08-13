"Filed out from Dolphin Smalltalk 7"!

RuntimeSessionManager subclass: #JadeSessionManager
	instanceVariableNames: ''
	classVariableNames: 'Version'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSessionManager guid: (GUID fromString: '{8d7a4642-b8e1-43ea-94d5-e13390ca1f79}')!
JadeSessionManager comment: ''!
!JadeSessionManager categoriesForClass!Unclassified! !
!JadeSessionManager methodsFor!

defaultResLibPath
	"Answer the path of the development resource library."

	^'bin\Jade'!

main
"
	JadeSessionManager basicNew main.
"
	Keyboard default isShiftDown 
		ifTrue: [self mainShellClass show: 'Compact view']
		ifFalse: [self mainShellClass show].
!

pause

	Keyboard default isShiftDown ifTrue: [self error: 'Generate walkback!!'].
!

version

	^Version notNil 
		ifTrue: [Version]
		ifFalse: ['(Development)'].
!

windowSystemStartup

	ScintillaLibrary open: 'bin\SciLexer.dll'.		"Since this isn't adjacent to the executable, it needs a path to find it"
	super windowSystemStartup.
! !
!JadeSessionManager categoriesFor: #defaultResLibPath!public! !
!JadeSessionManager categoriesFor: #main!public! !
!JadeSessionManager categoriesFor: #pause!public! !
!JadeSessionManager categoriesFor: #version!public! !
!JadeSessionManager categoriesFor: #windowSystemStartup!public! !

!JadeSessionManager class methodsFor!

getVersion
"
	JadeSessionManager getVersion.
"
	| string |
	File workingDirectory: SessionManager current imageBase.
	string := [SessionManager current class git: 'describe'] on: ExternalProcessExecuteError do: [:ex | ^'?<' , ex description , '>?'].
	^(ReadStream on: string) nextLine
!

mainShellClass

	^JadeLoginShell.
!

setVersion

	Version := self getVersion.!

version

	^Version notNil 
		ifTrue: [Version]
		ifFalse: ['Development'].
! !
!JadeSessionManager class categoriesFor: #getVersion!public! !
!JadeSessionManager class categoriesFor: #mainShellClass!public! !
!JadeSessionManager class categoriesFor: #setVersion!public! !
!JadeSessionManager class categoriesFor: #version!public! !

