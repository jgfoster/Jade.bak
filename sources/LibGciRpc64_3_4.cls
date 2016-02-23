"Filed out from Dolphin Smalltalk 7"!

LibGciRpc64 subclass: #LibGciRpc64_3_4
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_4 guid: (GUID fromString: '{96C1E982-82A2-431A-9CC0-ABFB2FC4E7E1}')!
LibGciRpc64_3_4 comment: ''!
!LibGciRpc64_3_4 categoriesForClass!Unclassified! !
!LibGciRpc64_3_4 methodsFor!

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
!LibGciRpc64_3_4 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64_3_4 categoriesFor: #gciErr:!private! !
!LibGciRpc64_3_4 categoriesFor: #specialFromOop:!public! !

!LibGciRpc64_3_4 class methodsFor!

displayName

	^'64-bit 3.4.0'.
!

fileNameSearch

	^'libgcirpc-3.4.0-32.dll'.
! !
!LibGciRpc64_3_4 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_4 class categoriesFor: #fileNameSearch!public! !

