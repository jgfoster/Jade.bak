"Filed out from Dolphin Smalltalk 7"!

GciSession30 subclass: #GciSession33
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciSession33 guid: (GUID fromString: '{085e991d-a17a-4475-bae7-c9c88012e5e4}')!
GciSession33 comment: ''!
!GciSession33 categoriesForClass!Unclassified! !
!GciSession33 methodsFor!

decode: aString
	"asAnsiString before 3.3.0; asUtf8String from 3.3.0 on."

	^aString asUtf8String!

encode: aString
	"asAnsiString before 3.3.0; asUtf8String from 3.3.0 on."

	^aString asUtf8String!

gsHasPragmas

	^true! !
!GciSession33 categoriesFor: #decode:!public! !
!GciSession33 categoriesFor: #encode:!public! !
!GciSession33 categoriesFor: #gsHasPragmas!public! !

