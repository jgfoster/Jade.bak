"Filed out from Dolphin Smalltalk 7"!

ArrayedCollection subclass: #CharacterCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CharacterCollection guid: (GUID fromString: '{4408ec10-3b63-4d56-a60f-56c628c34605}')!
CharacterCollection comment: ''!
!CharacterCollection categoriesForClass!Collections-Abstract! !
!CharacterCollection methodsFor!

asUtf8String

	| writeStream |
	writeStream := WriteStream on: Utf8String new.
	self do: [:each | writeStream nextPut: each].
	^writeStream contents! !
!CharacterCollection categoriesFor: #asUtf8String!public! !

