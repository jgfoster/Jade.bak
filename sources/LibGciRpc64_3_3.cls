"Filed out from Dolphin Smalltalk 7"!

LibGciRpc64 subclass: #LibGciRpc64_3_3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_3 guid: (GUID fromString: '{905626B7-882D-4151-8165-7B33D4BC9095}')!
LibGciRpc64_3_3 comment: ''!
!LibGciRpc64_3_3 categoriesForClass!Unclassified! !
!LibGciRpc64_3_3 methodsFor!

errorStructureClass

	^GciErrSType64_31.
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType64_31*>
	^self invalidCall
!

specialFromOop: anOop

	anOop isSmallFraction ifTrue: [
		^anOop asFraction.
	].
	^super specialFromOop: anOop
! !
!LibGciRpc64_3_3 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64_3_3 categoriesFor: #gciErr:!private! !
!LibGciRpc64_3_3 categoriesFor: #specialFromOop:!public! !

!LibGciRpc64_3_3 class methodsFor!

displayName

	^'64-bit 3.3.0'.
!

fileNameSearch

	^'libgcirpc-3.3.0-32.dll'.
! !
!LibGciRpc64_3_3 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_3 class categoriesFor: #fileNameSearch!public! !

