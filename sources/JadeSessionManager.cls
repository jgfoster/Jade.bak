"Filed out from Dolphin Smalltalk 7"!

RuntimeSessionManager subclass: #JadeSessionManager
	instanceVariableNames: ''
	classVariableNames: 'Version'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSessionManager guid: (GUID fromString: '{8D7A4642-B8E1-43EA-94D5-E13390CA1F79}')!
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

version

	^Version notNil 
		ifTrue: [Version]
		ifFalse: ['(Development)'].
! !
!JadeSessionManager categoriesFor: #defaultResLibPath!public! !
!JadeSessionManager categoriesFor: #main!public! !
!JadeSessionManager categoriesFor: #version!public! !

!JadeSessionManager class methodsFor!

getVersion

	^'2.000'!

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

