"Filed out from Dolphin Smalltalk 7"!

JadeServer64bit3x subclass: #JadeServer64bit32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer64bit32 guid: (GUID fromString: '{b2cb5f19-4347-4b6e-922e-705e99f7cb99}')!
JadeServer64bit32 comment: ''!
!JadeServer64bit32 categoriesForClass!Unclassified! !
!JadeServer64bit32 class methodsFor!

sessionStateCode
	"Avoid deprecated method and still use 'Topaz session state' (since we know Topaz isn't running!!)"

	^'System __sessionStateAt: 3 put: server.'! !
!JadeServer64bit32 class categoriesFor: #sessionStateCode!public! !

