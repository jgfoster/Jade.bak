"Filed out from Dolphin Smalltalk 7"!

LibGciRpc64 subclass: #LibGciRpc64_3_4
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_4 guid: (GUID fromString: '{96c1e982-82a2-431a-9cc0-abfb2fc4e7e1}')!
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

gsHasPragmas

	^true!

session: session execute: stringOrOop context: contextOop environment: anInteger

	self critical: [
		self gciSetSessionId: session.
		self
			gciNbExecuteStrFromContext_: stringOrOop
			_: contextOop
			_: self oopNil
			_: anInteger.
		^self nbResult.
	].
!

specialFromOop: anOop

	anOop isSmallFraction ifTrue: [
		^anOop asFraction.
	].
	^super specialFromOop: anOop
! !
!LibGciRpc64_3_4 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64_3_4 categoriesFor: #gciErr:!private! !
!LibGciRpc64_3_4 categoriesFor: #gsHasPragmas!public! !
!LibGciRpc64_3_4 categoriesFor: #session:execute:context:environment:!public!required! !
!LibGciRpc64_3_4 categoriesFor: #specialFromOop:!public! !

!LibGciRpc64_3_4 class methodsFor!

fileNameSearch

	^'libgcirpc-3.4.0-32.dll'.
! !
!LibGciRpc64_3_4 class categoriesFor: #fileNameSearch!public! !

