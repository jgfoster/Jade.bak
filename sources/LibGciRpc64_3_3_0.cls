"Filed out from Dolphin Smalltalk 7"!

LibGciRpc64_3_1_0 subclass: #LibGciRpc64_3_3_0
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_3_0 guid: (GUID fromString: '{905626b7-882d-4151-8165-7b33d4bc9095}')!
LibGciRpc64_3_3_0 comment: ''!
!LibGciRpc64_3_3_0 categoriesForClass!Unclassified! !
!LibGciRpc64_3_3_0 methodsFor!

gsHasPragmas

	^true!

specialFromOop: anOop

	anOop isSmallFraction ifTrue: [
		^anOop asFraction.
	].
	^super specialFromOop: anOop
! !
!LibGciRpc64_3_3_0 categoriesFor: #gsHasPragmas!public! !
!LibGciRpc64_3_3_0 categoriesFor: #specialFromOop:!public! !

