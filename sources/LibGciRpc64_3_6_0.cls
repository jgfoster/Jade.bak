"Filed out from Dolphin Smalltalk 7"!

LibGciRpc64_3_4_0 subclass: #LibGciRpc64_3_6_0
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_6_0 guid: (GUID fromString: '{11934b97-bac4-4eb9-8a55-2080654d1d72}')!
LibGciRpc64_3_6_0 comment: ''!
!LibGciRpc64_3_6_0 categoriesForClass!Unclassified! !
!LibGciRpc64_3_6_0 methodsFor!

specialFromOop: anOop

	anOop isSmallScaledDecimal	ifTrue: [^anOop asScaledDecimal	].
	anOop isSmallDateAndTime	ifTrue: [^anOop asDateAndTime	].
	anOop isSmallTime			ifTrue: [^anOop asTime			].
	anOop isSmallDate			ifTrue: [^anOop asDate			].
	^super specialFromOop: anOop
! !
!LibGciRpc64_3_6_0 categoriesFor: #specialFromOop:!public! !

