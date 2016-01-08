"Filed out from Dolphin Smalltalk 7"!

LibGciRpc64 subclass: #LibGciRpc64_3_0
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_0 guid: (GUID fromString: '{A8A6C6D6-29C1-4E0D-9C00-EC47F1078D4D}')!
LibGciRpc64_3_0 comment: ''!
!LibGciRpc64_3_0 categoriesForClass!Unclassified! !
!LibGciRpc64_3_0 methodsFor!

errorStructureClass

	^GciErrSType64_30.
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType64_30*>
	^self invalidCall
! !
!LibGciRpc64_3_0 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64_3_0 categoriesFor: #gciErr:!private! !

!LibGciRpc64_3_0 class methodsFor!

displayName

	^'64-bit 3.0.x'.
! !
!LibGciRpc64_3_0 class categoriesFor: #displayName!public! !

