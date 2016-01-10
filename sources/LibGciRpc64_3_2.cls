"Filed out from Dolphin Smalltalk 7"!

LibGciRpc64 subclass: #LibGciRpc64_3_2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_2 guid: (GUID fromString: '{25CBF893-B55F-4C84-B927-F0E27DAF8114}')!
LibGciRpc64_3_2 comment: ''!
!LibGciRpc64_3_2 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2 methodsFor!

errorStructureClass

	^GciErrSType64_31.
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType64_31*>
	^self invalidCall
! !
!LibGciRpc64_3_2 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64_3_2 categoriesFor: #gciErr:!private! !

!LibGciRpc64_3_2 class methodsFor!

displayName

	^'64-bit 3.2'.
!

fileNameSearch

	^'libgcits-3.2.0-32.dll'.
! !
!LibGciRpc64_3_2 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2 class categoriesFor: #fileNameSearch!public! !

