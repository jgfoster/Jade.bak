"Filed out from Dolphin Smalltalk 7"!

GciThreadSafeLibrary subclass: #LibGciTs_3_3_0
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciTs_3_3_0 guid: (GUID fromString: '{2312615D-0E2E-4E4E-8334-B0B5E5586E53}')!
LibGciTs_3_3_0 comment: ''!
!LibGciTs_3_3_0 categoriesForClass!Unclassified! !
!LibGciTs_3_3_0 methodsFor!

errorStructureClass

	^GciErrSType64_31.
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType64_31*>
	^self invalidCall
! !
!LibGciTs_3_3_0 categoriesFor: #errorStructureClass!private! !
!LibGciTs_3_3_0 categoriesFor: #gciErr:!private! !

!LibGciTs_3_3_0 class methodsFor!

displayName

	^'64-bit 3.3.0 (TS)'.
!

fileNameSearch

	^'libgcits-3.3.0-32.dll'.
! !
!LibGciTs_3_3_0 class categoriesFor: #displayName!public! !
!LibGciTs_3_3_0 class categoriesFor: #fileNameSearch!public! !

