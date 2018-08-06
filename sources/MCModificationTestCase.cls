"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #MCModificationTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCModificationTestCase guid: (GUID fromString: '{c463b1c3-c30b-4a60-9509-7a646157e7da}')!
MCModificationTestCase comment: ''!
!MCModificationTestCase categoriesForClass!Unclassified! !
!MCModificationTestCase methodsFor!

methods: anArray

	| oldString newString stream |
	oldString := anArray first reject: [:each | each = Character cr].
	newString := anArray last reject: [:each | each = Character cr].
	stream := (WriteStream on: String new)
		nextPutAll: 'M	123	M	JamesFoster 12/23/2010 20:16	ClassName	false	MethodCategory	methodName'; tab;
		nextPutAll: oldString size printString; tab;
		nextPutAll: oldString; 
		nextPut: Character lf;
		nextPutAll: 'M	JamesFoster 12/23/2010 20:16	ClassName	false	MethodCategory	methodName'; tab;
		nextPutAll: newString size printString; tab;
		nextPutAll: newString;
		nextPut: Character lf;
		yourself.
	^MCPatchOperation 
		fromStream: (ReadStream on: stream contents) 
		session: self.
!

methods01

^#(
'aaa
	bbb
	ccc'
'aaa
	BBB
	ccc').!

methods02

^#(
'aaa
	bbb
	ddd'
'aaa
	bbb
	ccc
	ddd').!

methods03

^#(
'aaa
	bbb
	ccc
	ddd'
'aaa
	bbb
	ddd').!

methods04

^#(
'aaa
	bbb
'
'aaa
	bbb

').!

oopTypeWithOop: anObject
!

test01

	| modification oldMemo oldMarkers newMemo newMarkers |
	modification := self methods: self methods01.
	oldMemo := modification obsoletionMemo subStrings: Character lf.
	oldMarkers := modification obsoletionMemoMarkers.
	newMemo := modification modificationMemo subStrings: Character lf.
	newMarkers := modification modificationMemoMarkers.
	self
		assert: oldMemo size = 3;
		assert: oldMarkers = (Array with: #'changed' -> 2);
		assert: newMemo size = 3;
		assert: newMarkers = (Array with: #'changed' -> 2);
		yourself.
!

test02
 
	| modification oldMemo oldMarkers newMemo newMarkers |
	modification := self methods: self methods02.
	oldMemo := modification obsoletionMemo subStrings: Character lf.
	oldMarkers := modification obsoletionMemoMarkers.
	newMemo := modification modificationMemo subStrings: Character lf.
	newMarkers := modification modificationMemoMarkers.
	self
		assert: oldMemo size = 4;
		assert: oldMarkers = #();
		assert: newMemo size = 4;
		assert: newMarkers = (Array with: #'added' -> 3);
		yourself.
!

test03
 
	| modification oldMemo oldMarkers newMemo newMarkers |
	modification := self methods: self methods03.
	oldMemo := modification obsoletionMemo subStrings: Character lf.
	oldMarkers := modification obsoletionMemoMarkers.
	newMemo := modification modificationMemo subStrings: Character lf.
	newMarkers := modification modificationMemoMarkers.
	self
		assert: oldMemo size = 4;
		assert: oldMarkers = (Array with: #'removed' -> 3);
		assert: newMemo size = 4;
		assert: newMarkers = #();
		yourself.
!

test04
 
	| modification oldMemo oldMarkers newMemo newMarkers |
	modification := self methods: self methods04.
	oldMemo := modification obsoletionMemo subStrings: Character lf.
	oldMarkers := modification obsoletionMemoMarkers.
	newMemo := modification modificationMemo subStrings: Character lf.
	newMarkers := modification modificationMemoMarkers.
	self
		assert: oldMemo size = 4;
		assert: oldMarkers = #();
		assert: newMemo size = 4;
		assert: newMarkers = (Array with: #'added' -> 4);
		yourself.
! !
!MCModificationTestCase categoriesFor: #methods:!public! !
!MCModificationTestCase categoriesFor: #methods01!public! !
!MCModificationTestCase categoriesFor: #methods02!public! !
!MCModificationTestCase categoriesFor: #methods03!public! !
!MCModificationTestCase categoriesFor: #methods04!public! !
!MCModificationTestCase categoriesFor: #oopTypeWithOop:!public! !
!MCModificationTestCase categoriesFor: #test01!public! !
!MCModificationTestCase categoriesFor: #test02!public! !
!MCModificationTestCase categoriesFor: #test03!public! !
!MCModificationTestCase categoriesFor: #test04!public! !

