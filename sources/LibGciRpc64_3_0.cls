"Filed out from Dolphin Smalltalk 7"!

LibGciRpc64 subclass: #LibGciRpc64_3_0
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_0 guid: (GUID fromString: '{a8a6c6d6-29c1-4e0d-9c00-ec47f1078d4d}')!
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
!

fileNameSearch

	^'libgcirpc64-301.dll'
! !
!LibGciRpc64_3_0 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_0 class categoriesFor: #fileNameSearch!public! !

