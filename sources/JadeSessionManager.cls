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

getVersion

	^(ReadStream on: (SessionManager current class git: 'describe')) nextLine!

mainShellClass

	^JadeLoginShell.
!

sessionStarted

	GciLibrary sessionStarted!

setVersion

	Version := self getVersion.!

version

	^Version notNil 
		ifTrue: [Version]
		ifFalse: ['(Development)'].
! !
!JadeSessionManager class categoriesFor: #getVersion!public! !
!JadeSessionManager class categoriesFor: #mainShellClass!public! !
!JadeSessionManager class categoriesFor: #sessionStarted!public! !
!JadeSessionManager class categoriesFor: #setVersion!public! !
!JadeSessionManager class categoriesFor: #version!public! !

