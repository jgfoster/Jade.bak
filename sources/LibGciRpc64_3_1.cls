"Filed out from Dolphin Smalltalk 7"!

LibGciRpc64 subclass: #LibGciRpc64_3_1
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_1 guid: (GUID fromString: '{B9F761F8-7AA2-47AB-804A-8A2C76AFDB1D}')!
LibGciRpc64_3_1 comment: ''!
!LibGciRpc64_3_1 categoriesForClass!Unclassified! !
!LibGciRpc64_3_1 methodsFor!

errorStructureClass

	^GciErrSType64_31.
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType64_31*>
	^self invalidCall
! !
!LibGciRpc64_3_1 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64_3_1 categoriesFor: #gciErr:!private! !

!LibGciRpc64_3_1 class methodsFor!

displayName

	^'64-bit 3.1.0'.
!

fileNameSearch

	^'libgcirpc-3.1.0-32.dll'.
! !
!LibGciRpc64_3_1 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_1 class categoriesFor: #fileNameSearch!public! !

