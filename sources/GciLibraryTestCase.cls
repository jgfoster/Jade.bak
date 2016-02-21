"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #GciLibraryTestCase
	instanceVariableNames: 'library session'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciLibraryTestCase guid: (GUID fromString: '{14F7F08C-7854-44C6-BDAA-E1B5055DC094}')!
GciLibraryTestCase comment: ''!
!GciLibraryTestCase categoriesForClass!Unclassified! !
!GciLibraryTestCase methodsFor!

test_abort

	library abortSession: session.
!

test_begin

	library beginSession: session.
!

test_break

	| semaphore a b c d e t1 time |
	library 
		softBreakSession: session;
		hardBreakSession: session;
		session: session breakHard: false;
		session: session breakHard: true;
		yourself.
	semaphore := Semaphore new.
	t1 := Time millisecondClockValue.
	[
		[semaphore signal] forkAt: Processor userBackgroundPriority. 
		a := [
			b := library session: session execute: '(Delay forSeconds: 5) wait. $b'.
		] on: Error do: [:ex | 
			c := ex. 
			ex return: $a.
		].
		[semaphore signal] forkAt: Processor userBackgroundPriority. 
	] fork.
	semaphore wait.
	(Delay forMilliseconds: 50) wait.
	[
		d := library softBreakSession: session.
	] on: Error do: [:ex | 
		e := ex.
	].
	semaphore wait.
	time := Time millisecondClockValue - t1.
	library session: session clearStack: c tag contextOop.
	self
		assert: time < 2500;
		assert: a == $a;
		assert: b isNil;
		assert: (c isKindOf: GciError);
		assert: d == library;
		assert: e isNil;
		yourself.
!

test_charToOop

	| x |
	x := library oopForCharacter: $a.
	self assert: x value == 24860.
!

test_clearStack

	| context |
	[
		library session: session execute: 'nil halt'.
	] on: Error do: [:ex |
		context :=ex tag contextOop.
	].
	self assert: context notNil.
	library session: session clearStack: context.
!

test_commit

	library commitSession: session.
!

test_continue

	| error result  |
	[
		result := library session: session execute: 'nil halt. 5'.
	] on: GciError do: [:ex |
		error := ex.
	].
	self
		assert: result isNil;
		assert: error notNil;
		yourself.
	result := library session: session continue: error tag contextOop.
	self assert: result == 5.
!

test_doubleToOop

	| x |
	x := library session: session oopForDouble: 1.5.
	self assert: x value = 16r7F80000000000006.
!

test_doubleToSmallDouble

	| x |
	x := library oopForSmallDouble: 1.5.
	self assert: x value = 16r7F80000000000006.
!

test_execute

	| x |
	self 
		assert: (x := library session: session execute: '0') == 0;
		assert: (x := library session: session execute: 'nil') == nil;
		assert: (x := library session: session execute: 'true') == true;
		assert: (x := library session: session execute: 'false') == false;
		assert: (x := library session: session execute: '$a') == $a;
		assert: (x := library session: session execute: '1.5') = 1.5;
		"assert: (x := library session: session execute: '1 / 2') = (1 / 2);"

		assert: (x := library session: session execute: 'String with: $a with: $b') = 'ab';
		assert: (x := library session: session execute: 'ByteArray with: 0 with: 255') = #[0 255];
		yourself.
!

test_fetchBytes

	| x |
	self 
		should: [library session: session fetchBytes: library oopIllegal]
		raise: GciError.
	x := library session: session execute: 'ByteArray new asOop'.
	x := library session: session fetchBytes: (OopType64 fromInteger: x).
	self 
		assert: x class == ByteArray;
		assert: x size == 0;
		yourself.
	x := library session: session execute: '(ByteArray new: 999) atAllPut: 1; asOop'.
	x := library session: session fetchBytes: (OopType64 fromInteger: x).
	self 
		assert: x class == ByteArray;
		assert: x size == 999;
		assert: x asSet any == 1;
		yourself.
	x := library session: session execute: '(ByteArray new: 1000) atAllPut: 1; asOop'.
	x := library session: session fetchBytes: (OopType64 fromInteger: x).
	self 
		assert: x class == ByteArray;
		assert: x size == 1000;
		assert: x asSet any == 1;
		yourself.
	x := library session: session execute: '(ByteArray new: 1001) atAllPut: 1; asOop'.
	x := library session: session fetchBytes: (OopType64 fromInteger: x).
	self 
		assert: x class == ByteArray;
		assert: x size == 1001;
		assert: x asSet any == 1;
		yourself.

!

test_fetchClass

	| x y z |
	x := library session: session execute: 'String new asOop'.
	y := library session: session execute: 'String'.
	z := library session: session fetchClass: (OopType64 fromInteger: x).
	self assert: y = z.

!

test_fetchObject1
	"Pointer (OOPs) object"

	| oop class policy namedSize object |
	oop := library session: session execute: 'System myUserProfile'.
	class := library session: session execute: 'UserProfile'.
	policy := library session: session execute: 'System myUserProfile objectSecurityPolicyId'.
	namedSize := library session: session execute: 'UserProfile instVarNames size'.
	object := library session: session fetchObject: oop.
	self
		assert: object objId == oop value;
		assert: object objClassOop = class;
		assert: object objectSecurityPolicyId == policy;
		assert: object namedSize == namedSize;
		assert: object isWriteable;
		deny: object isInvariant;
		deny: object isIndexable;
		assert: object implementation == 0;
		assert: object data size == 10;
		yourself.
!

test_fetchObject2
	"String object"

	| oop object |
	oop := library session: session execute: 'System myUserProfile userId asOop'.
	object := library session: session fetchObject: (OopType64 fromInteger: oop).
	self assert: object data = 'DataCurator'.!

test_fetchObject3
	"Symbol object"

	| oop object |
	oop := library session: session execute: '#UserGlobals asOop'.
	object := library session: session fetchObject: (OopType64 fromInteger: oop).
	self assert: object data = #'UserGlobals'.!

test_fetchObject4
	"ByteArray object"

	| oop object |
	oop := library session: session execute: '#[0 1 2 3 4 5 6 7] asOop'.
	object := library session: session fetchObject: (OopType64 fromInteger: oop).
	self assert: object data = #[0 1 2 3 4 5 6 7].!

test_fetchObjects

	| object objects |
	object := library session: session execute: '#()'.
	objects := library session: session fetchObjects: object.
	self assert: objects isEmpty.
	object := library session: session execute: '#(true false nil)'.
	objects := library session: session fetchObjects: object.
	self 
		assert: objects class == Array;
		assert: (objects at: 1) = library oopTrue;
		assert: (objects at: 2) = library oopFalse;
		assert: (objects at: 3) = library oopNil;
		yourself.
!

test_fetchSize

	| x y |
	x := library session: session fetchSize: library oopNil.
	self assert: x == 0.
	x := library session: session execute: '(String new: 1005) atAllPut: $a; asOop'.
	y := library session: session fetchSize: (OopType64 fromInteger: x).
	self assert: y == 1005.

!

test_fetchSpecialClass

	| x y z |
	x := library session: session execute: 'nil asOop'.
	y := library session: session execute: 'nil class'.
	z := library classForSpecial: (OopType64 fromInteger: x).
	self assert: y = z.

	x := library session: session execute: '0 asOop'.
	y := library session: session execute: '0 class'.
	z := library classForSpecial: (OopType64 fromInteger: x).
	self assert: y = z.

	x := library session: session execute: 'true asOop'.
	y := library session: session execute: 'true class'.
	z := library classForSpecial: (OopType64 fromInteger: x).
	self assert: y = z.

	x := library session: session execute: '$a asOop'.
	y := library session: session execute: '$a class'.
	z := library classForSpecial: (OopType64 fromInteger: x).
	self assert: y = z.
!

test_fetchString

	| x |
	self 
		should: [library session: session fetchString: library oopIllegal ]
		raise: GciError.
	x := library session: session execute: 'String new asOop'.
	x := library session: session fetchString: (OopType64 fromInteger: x).
	self 
		assert: x class == String;
		assert: x size == 0;
		yourself.
	x := library session: session execute: '(String new: 999) atAllPut: $a; asOop'.
	x := library session: session fetchString: (OopType64 fromInteger: x).
	self 
		assert: x class == String;
		assert: x size == 999;
		assert: x asSet any == $a;
		yourself.
	x := library session: session execute: '(String new: 1000) atAllPut: $a; asOop'.
	x := library session: session fetchString: (OopType64 fromInteger: x).
	self 
		assert: x class == String;
		assert: x size == 1000;
		assert: x asSet any == $a;
		yourself.
	x := library session: session execute: '(String new: 1001) atAllPut: $a; asOop'.
	x := library session: session fetchString: (OopType64 fromInteger: x).
	self 
		assert: x class == String;
		assert: x size == 1001;
		assert: x asSet any == $a;
		yourself.

!

test_fetchVaryingSize

	| x y |
	x := library session: session fetchSize: library oopNil.
	self assert: x == 0.

	x := library session: session execute: 'Association new'.
	y := library session: session fetchVaryingSize: x.
	self assert: y == 0.

	x := library session: session execute: '(String new: 10) asOop'.
	y := library session: session fetchVaryingSize: (OopType64 fromInteger: x).
	self assert: y == 10.

	x := library session: session execute: 'Array new: 10'.
	y := library session: session fetchVaryingSize: x.
	self assert: y == 10.
!

test_gemTrace

	| value |
	value := library session: session gemTrace: 1.
	self assert: value == 0.
	value := library session: session gemTrace: 2.
	self assert: value == 1.
	value := library session: session gemTrace: 3.
	self assert: value == 2.
	value := library session: session gemTrace: 0.
	self assert: value == 3.
!

test_i64ToOop

	| x |
	x := library session: session oopForInteger: 0.
	self assert: x = library oopZero.
	x := library session: session  oopForInteger: 1.
	self assert: x = library oopOne.
	x := library session: session  oopForInteger: -1.
	self assert: x value = 16rFFFFFFFFFFFFFFFA.
!

test_login

	self assert: session notNil.
!

test_newString

	| x |
	x := library session: session oopForString: 'UserGlobals'.
	self assert: 0 < x value.
!

test_objExists

	| x |
	x := library session: session execute: 'Globals'.
	x := library session: session objectExists: x.
	self assert: x.
	x := library session: session objectExists: library oopIllegal.
	self deny: x.
!

test_oopIsSpecial

	| x |
	x := library oopIsSpecial: library oopNil.
	self assert: x.
	x := library session: session execute: 'AllUsers'.
	x := library oopIsSpecial: x.
	self deny: x.
!

test_oopToChar

	| x |
	x := library session: session execute: '$a asOop'.
	x := library characterForOop: (OopType64 fromInteger: x).
	self assert: x == $a.
!

test_oopToDouble

	| x |
	x := library session: session doubleForOop: (OopType64 fromInteger: 16r7F80000000000006).
	self assert: x = 1.5.
!

test_oopToI64

	| x |
	x := library session: session integerForOop: library oopZero.
	self assert: x == 0.
	x := library session: session  integerForOop: library oopOne.
	self assert: x == 1.
	x := library session: session  integerForOop: library oopMinusOne.
	self assert: x == -1.
!

test_perform

	| args x |
	x := library session: session send: 'yourself' to: library oopZero.
	self assert: x  == 0.
	args := library oopTypeArrayClass with: (OopType64 fromInteger: 26 "3").
	x := library session: session send: '+' to: (OopType64 fromInteger: 18) "2" with: args.
	self assert: x == 5.
	args := (library oopTypeArrayClass new: 2)
		at: 1 put: (OopType64 fromInteger: 18 "2");
		at: 2 put: (OopType64 fromInteger: 42 "5");
		yourself.
	x := library session: session send: 'between:and:' to: (OopType64 fromInteger: 26 "3") with: args.
	self assert: x == true.
!

test_releaseAllObjs

	library releaseAllObjectsInSession: session.
!

test_releaseOops

	| x |
	x := library session: session execute: 'Object new'.
	library session: session releaseOops: (library oopTypeArrayClass with: x).
!

test_resolveSymbol

	| x |
	x := library session: session execute: 'System myUserProfile symbolList'.
	x := library session: session objectNamed: 'Array' inSymbolList: x.
	self assert: x = library oopClassArray.
!

test_resolveSymbolOop

	| stringOop symbolList x y |
	stringOop := library session: session oopForString: 'UserGlobals'.
	symbolList := library session: session execute: 'System myUserProfile symbolList'.
	x := library session: session objectNamedOop: stringOop inSymbolList: symbolList.
	y := library session: session execute: 'UserGlobals'.
	self assert: x = y.
!

test_sessionIsRemote

	self 
		assert: (library sessionIsRemote: session);
		should: [library sessionIsRemote: session + 1] raise: Error;
		yourself.

!

test_version

	| string |
	string := library version.
	self 
		assert: string notEmpty;
		assert: string last codePoint ~~ 0;
		assert: (string beginsWith: '3.3');
		yourself.
!

testUserActionCallback

	| x |
	self 
		assert: (x := library session: session execute: '2 + 3') == 5;
		assert: (x := library session: session execute: 'GsFile existsOnServer: ''foo.bar''') == false;
		assert: (x := library session: session execute: 'GsFile exists: ''foo.bar''') == false;
		yourself.
! !
!GciLibraryTestCase categoriesFor: #test_abort!public! !
!GciLibraryTestCase categoriesFor: #test_begin!public! !
!GciLibraryTestCase categoriesFor: #test_break!public! !
!GciLibraryTestCase categoriesFor: #test_charToOop!public! !
!GciLibraryTestCase categoriesFor: #test_clearStack!public! !
!GciLibraryTestCase categoriesFor: #test_commit!public! !
!GciLibraryTestCase categoriesFor: #test_continue!public! !
!GciLibraryTestCase categoriesFor: #test_doubleToOop!public! !
!GciLibraryTestCase categoriesFor: #test_doubleToSmallDouble!public! !
!GciLibraryTestCase categoriesFor: #test_execute!public! !
!GciLibraryTestCase categoriesFor: #test_fetchBytes!public! !
!GciLibraryTestCase categoriesFor: #test_fetchClass!public! !
!GciLibraryTestCase categoriesFor: #test_fetchObject1!public! !
!GciLibraryTestCase categoriesFor: #test_fetchObject2!public! !
!GciLibraryTestCase categoriesFor: #test_fetchObject3!public! !
!GciLibraryTestCase categoriesFor: #test_fetchObject4!public! !
!GciLibraryTestCase categoriesFor: #test_fetchObjects!public! !
!GciLibraryTestCase categoriesFor: #test_fetchSize!public! !
!GciLibraryTestCase categoriesFor: #test_fetchSpecialClass!public! !
!GciLibraryTestCase categoriesFor: #test_fetchString!public! !
!GciLibraryTestCase categoriesFor: #test_fetchVaryingSize!public! !
!GciLibraryTestCase categoriesFor: #test_gemTrace!public! !
!GciLibraryTestCase categoriesFor: #test_i64ToOop!public! !
!GciLibraryTestCase categoriesFor: #test_login!public! !
!GciLibraryTestCase categoriesFor: #test_newString!public! !
!GciLibraryTestCase categoriesFor: #test_objExists!public! !
!GciLibraryTestCase categoriesFor: #test_oopIsSpecial!public! !
!GciLibraryTestCase categoriesFor: #test_oopToChar!public! !
!GciLibraryTestCase categoriesFor: #test_oopToDouble!public! !
!GciLibraryTestCase categoriesFor: #test_oopToI64!public! !
!GciLibraryTestCase categoriesFor: #test_perform!public! !
!GciLibraryTestCase categoriesFor: #test_releaseAllObjs!public! !
!GciLibraryTestCase categoriesFor: #test_releaseOops!public! !
!GciLibraryTestCase categoriesFor: #test_resolveSymbol!public! !
!GciLibraryTestCase categoriesFor: #test_resolveSymbolOop!public! !
!GciLibraryTestCase categoriesFor: #test_sessionIsRemote!public! !
!GciLibraryTestCase categoriesFor: #test_version!public! !
!GciLibraryTestCase categoriesFor: #testUserActionCallback!public! !

!GciLibraryTestCase class methodsFor!

isAbstract

	^self name == #'GciLibraryTestCase'
! !
!GciLibraryTestCase class categoriesFor: #isAbstract!public! !

