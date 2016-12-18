"Filed out from Dolphin Smalltalk 7"!

ReadStream subclass: #JadeServerReadStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServerReadStream guid: (GUID fromString: '{ED24CD81-A45E-4685-BBE8-B0DD53109F59}')!
JadeServerReadStream comment: ''!
!JadeServerReadStream categoriesForClass!Unclassified! !
!JadeServerReadStream methodsFor!

nextLineAsList

	^(self nextLine subStrings: Character tab) reject: [:each | each isEmpty].
!

nextList

	| list line |
	list := OrderedCollection new.
	[
		line := self nextLineAsList.
		line notEmpty and: [line first = '%'].
	] whileFalse: [
		list add: line.
	].
	^list.
!

nextParagraph

	| line writeStream |
	writeStream := WriteStream on: String new.
	[
		line := self upTo: Character lf.
		self atEnd or: [line = '%'].
	] whileFalse: [
		writeStream nextPutAll: line; lf.
	].
	^writeStream contents.
!

upToTab

	^self upTo: Character tab
! !
!JadeServerReadStream categoriesFor: #nextLineAsList!public! !
!JadeServerReadStream categoriesFor: #nextList!public! !
!JadeServerReadStream categoriesFor: #nextParagraph!public! !
!JadeServerReadStream categoriesFor: #upToTab!public! !

