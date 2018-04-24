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
! !
!JadeSessionManager categoriesFor: #defaultResLibPath!public! !
!JadeSessionManager categoriesFor: #main!public! !
!JadeSessionManager categoriesFor: #pause!public! !
!JadeSessionManager categoriesFor: #version!public! !

!JadeSessionManager class methodsFor!

getUrl

	| string |
	string := [SessionManager current class git: 'git config --get remote.origin.url'] on: ExternalProcessExecuteError do: [:ex | ^'???'].
	^(ReadStream on: string) nextLine
!

getVersion

	| string |
	string := [SessionManager current class git: 'describe'] on: ExternalProcessExecuteError do: [:ex | ^'???'].
	^(ReadStream on: string) nextLine
!

sessionStarted

	ScintillaLibrary open: 'bin\SciLexer.dll'.
	GciLibrary sessionStarted.!

setVersion
	Version := self getVersion , ' url: ' , self getUrl!

version

	^Version notNil 
		ifTrue: [Version]
		ifFalse: ['(Development)'].
! !
!JadeSessionManager class categoriesFor: #getUrl!public! !
!JadeSessionManager class categoriesFor: #getVersion!public! !
!JadeSessionManager class categoriesFor: #sessionStarted!public! !
!JadeSessionManager class categoriesFor: #setVersion!public! !
!JadeSessionManager class categoriesFor: #version!public! !

