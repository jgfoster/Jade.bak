"Filed out from Dolphin Smalltalk 7"!

LibGciRpc64_3_0 subclass: #LibGciRpc64_3_1_0
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_1_0 guid: (GUID fromString: '{b9f761f8-7aa2-47ab-804a-8a2c76afdb1d}')!
LibGciRpc64_3_1_0 comment: ''!
!LibGciRpc64_3_1_0 categoriesForClass!Unclassified! !
!LibGciRpc64_3_1_0 methodsFor!

errorStructureClass

	^GciErrSType64_31.
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType64_31*>
	^self invalidCall: _failureCode
! !
!LibGciRpc64_3_1_0 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64_3_1_0 categoriesFor: #gciErr:!private! !

!LibGciRpc64_3_1_0 class methodsFor!

displayName

	^'64-bit ' , self version!

fileNameSearch

	^'libgcirpc-' , self version , '-32.dll'.
!

version

	| dot list stream string |
	string := self name.
	string := string copyFrom: 'LibGciRpc64' size + 2 to: string size.
	list := string subStrings: '_'.
	list := list collect: [:each | (1 < each size and: [each first == $0]) ifTrue: [each copyFrom: 2 to: each size] ifFalse: [each]].
	stream := WriteStream on: String new.
	dot := ''.
	list do: [:each | 
		stream nextPutAll: dot; nextPutAll: each.
		dot := '.'.
	].
	^stream contents! !
!LibGciRpc64_3_1_0 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_1_0 class categoriesFor: #fileNameSearch!public! !
!LibGciRpc64_3_1_0 class categoriesFor: #version!public! !

