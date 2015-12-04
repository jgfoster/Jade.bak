| package |
package := Package name: 'GemStone C Interface'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.160'.

package basicScriptAt: #postinstall put: '''Loaded: GemStone C Interface'' yourself.'.

package classNames
	add: #DoubleByteString;
	add: #GciCommitFailure;
	add: #GciError;
	add: #GciErrSType;
	add: #GciErrSType32;
	add: #GciErrSType64;
	add: #GciErrSType64_30;
	add: #GciErrSType64_31;
	add: #GciLibrary;
	add: #GciLoginFailed;
	add: #Gcilw61;
	add: #Gcilw63;
	add: #Gcilw65;
	add: #Gcilw66;
	add: #Gcilw6x;
	add: #GciMtLibraryTestCase;
	add: #GciMtLibraryTestResource;
	add: #GciMultiThreadedLibrary;
	add: #Gcirw62;
	add: #GciSessionId;
	add: #GciSingleThreadedLibrary;
	add: #GciTsObjInfo;
	add: #LibGciRpc64;
	add: #LibGciRpc64_20;
	add: #LibGciRpc64_21;
	add: #LibGciRpc64_22;
	add: #LibGciRpc64_23;
	add: #LibGciRpc64_24;
	add: #LibGciRpc64_3_0;
	add: #LibGciRpc64_3_1;
	add: #LibGciRpc64_3_2;
	add: #LibGciRpc64_3_2_01;
	add: #LibGciRpc64_3_2_02;
	add: #LibGciRpc64_3_2_03;
	add: #LibGciRpc64_3_2_04;
	add: #LibGciRpc64_3_2_05;
	add: #LibGciRpc64_3_2_06;
	add: #LibGciRpc64_3_2_07;
	add: #LibGciRpc64_3_2_08;
	add: #LibGciRpc64_3_2_09;
	add: #LibGciRpc64_3_2_10;
	add: #LibGciRpc64_3_2_11;
	add: #LibGciRpc64_3_3;
	add: #LibGciRpc64_310x;
	add: #OopType32;
	add: #OopType32Array;
	add: #OopType32Field;
	add: #OopType64;
	add: #OopType64Array;
	add: #OopType64Field;
	add: #QuadByteString;
	add: #Unicode16;
	add: #Unicode32;
	add: #Unicode7;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

DWORDField subclass: #OopType32Field
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
QWORDField subclass: #OopType64Field
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ByteArray variableByteSubclass: #DoubleByteString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ByteArray variableByteSubclass: #QuadByteString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ByteArray variableByteSubclass: #Unicode16
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ByteArray variableByteSubclass: #Unicode32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ByteArray variableByteSubclass: #Unicode7
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #GciError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciError subclass: #GciCommitFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciError subclass: #GciLoginFailed
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #GciLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciLibrary subclass: #GciMultiThreadedLibrary
	instanceVariableNames: ''
	classVariableNames: 'OOP_ILLEGAL OOP_NIL'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciLibrary subclass: #GciSingleThreadedLibrary
	instanceVariableNames: 'semaphore'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciMultiThreadedLibrary subclass: #LibGciRpc64_3_3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciSingleThreadedLibrary subclass: #Gcilw6x
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciSingleThreadedLibrary subclass: #LibGciRpc64
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Gcilw6x subclass: #Gcilw61
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Gcilw6x subclass: #Gcilw63
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Gcilw6x subclass: #Gcilw65
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Gcilw6x subclass: #Gcilw66
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Gcilw6x subclass: #Gcirw62
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64 subclass: #LibGciRpc64_20
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64 subclass: #LibGciRpc64_21
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64 subclass: #LibGciRpc64_22
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64 subclass: #LibGciRpc64_23
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64 subclass: #LibGciRpc64_24
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64 subclass: #LibGciRpc64_3_0
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64 subclass: #LibGciRpc64_3_1
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64 subclass: #LibGciRpc64_3_2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_1 subclass: #LibGciRpc64_310x
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_2 subclass: #LibGciRpc64_3_2_01
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_2 subclass: #LibGciRpc64_3_2_02
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_2 subclass: #LibGciRpc64_3_2_03
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_2 subclass: #LibGciRpc64_3_2_04
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_2 subclass: #LibGciRpc64_3_2_05
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_2 subclass: #LibGciRpc64_3_2_06
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_2 subclass: #LibGciRpc64_3_2_07
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_2 subclass: #LibGciRpc64_3_2_08
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_2 subclass: #LibGciRpc64_3_2_09
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_2 subclass: #LibGciRpc64_3_2_10
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_2 subclass: #LibGciRpc64_3_2_11
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #GciErrSType
	instanceVariableNames: 'args stack'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #GciTsObjInfo
	instanceVariableNames: 'data'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalArray subclass: #OopType32Array
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalArray subclass: #OopType64Array
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DWORD subclass: #GciSessionId
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DWORD subclass: #OopType32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ULARGE_INTEGER subclass: #OopType64
	instanceVariableNames: 'objectInfo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciErrSType subclass: #GciErrSType32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciErrSType subclass: #GciErrSType64
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciErrSType subclass: #GciErrSType64_30
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciErrSType subclass: #GciErrSType64_31
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #GciMtLibraryTestCase
	instanceVariableNames: 'library session'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestResource subclass: #GciMtLibraryTestResource
	instanceVariableNames: 'library session'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

OopType32Field guid: (GUID fromString: '{1C990F84-794B-4897-9C03-F58DF5ACAE69}')!
OopType32Field comment: ''!
!OopType32Field categoriesForClass!Unclassified! !
!OopType32Field methodsFor!

fieldClass

	^OopType32.! !
!OopType32Field categoriesFor: #fieldClass!public! !

OopType64Field guid: (GUID fromString: '{7ED7D17B-7FC5-4857-89C3-5A27AC9DE8AD}')!
OopType64Field comment: ''!
!OopType64Field categoriesForClass!Unclassified! !
!OopType64Field methodsFor!

fieldClass

	^OopType64.! !
!OopType64Field categoriesFor: #fieldClass!public! !

DoubleByteString guid: (GUID fromString: '{48552D00-1D75-4CBC-B980-D5401B007A59}')!
DoubleByteString comment: ''!
!DoubleByteString categoriesForClass!Unclassified! !
QuadByteString guid: (GUID fromString: '{6A0947B2-6DC5-44AD-A5FF-91BA5DBEDE53}')!
QuadByteString comment: ''!
!QuadByteString categoriesForClass!Unclassified! !
Unicode16 guid: (GUID fromString: '{CDDA698B-61EC-417C-9E6B-88450C68280E}')!
Unicode16 comment: ''!
!Unicode16 categoriesForClass!Unclassified! !
Unicode32 guid: (GUID fromString: '{4C897256-00C7-4134-8787-08DB5F1E66E8}')!
Unicode32 comment: ''!
!Unicode32 categoriesForClass!Unclassified! !
Unicode7 guid: (GUID fromString: '{70586536-7E1D-4EFF-B354-53290842C46E}')!
Unicode7 comment: ''!
!Unicode7 categoriesForClass!Unclassified! !
GciError guid: (GUID fromString: '{071E2ED5-4EBD-4862-B435-48DE5B29CFD6}')!
GciError comment: ''!
!GciError categoriesForClass!Unclassified! !
!GciError class methodsFor!

signal: aString

	self error: 'use #''signalWith:'''.
!

signalWith: aGciErrSType

	^self
		signal: aGciErrSType message
		with: aGciErrSType.
! !
!GciError class categoriesFor: #signal:!public! !
!GciError class categoriesFor: #signalWith:!public! !

GciCommitFailure guid: (GUID fromString: '{0282A2E9-2264-46A8-8391-61EDE92B12C7}')!
GciCommitFailure comment: ''!
!GciCommitFailure categoriesForClass!Unclassified! !
GciLoginFailed guid: (GUID fromString: '{48341733-FBF0-4F01-A585-CCA55E4FF300}')!
GciLoginFailed comment: ''!
!GciLoginFailed categoriesForClass!Unclassified! !
GciLibrary guid: (GUID fromString: '{AD9A79E0-D6EF-4D77-83FC-015B118B2331}')!
GciLibrary comment: ''!
!GciLibrary categoriesForClass!Unclassified! !
!GciLibrary methodsFor!

abortSession: anInteger

	self subclassResponsibility.
!

beginSession: anInteger

	self subclassResponsibility.
!

commitSession: anInteger

	self subclassResponsibility.
!

errorStructureClass

	self subclassResponsibility.
!

hardBreakSession: anInteger

	self subclassResponsibility.
!

loginHostUser: hostUser hostPassword: hostPassword gsUser: gsUser gsPassword: gsPassword gemNRS: gemString stoneNRS: stoneString

	self subclassResponsibility.
!

logoutSession: anInteger

	self subclassResponsibility.
!

nbResult

	self subclassResponsibility.
!

oopAsciiNul

	self subclassResponsibility.
!

oopAt: anExternalAddress

	self subclassResponsibility.
!

oopClassArray

	self subclassResponsibility.
!

oopClassByteArray

	self subclassResponsibility.
!

oopClassDoubleByteString

	self subclassResponsibility.
!

oopClassQuadByteString

	self subclassResponsibility.
!

oopClassString

	self subclassResponsibility.
!

oopClassSymbol

	self subclassResponsibility.
!

oopClassSystem

	self subclassResponsibility.
!

oopClassUnicode16

	self subclassResponsibility.
!

oopClassUnicode32

	self subclassResponsibility.
!

oopClassUnicode7

	self subclassResponsibility.
!

oopFalse

	self subclassResponsibility.
!

oopForInteger: anInteger

	self subclassResponsibility.
!

oopGemStoneError

	self subclassResponsibility.
!

oopIllegal

	self subclassResponsibility.
!

oopMaxSmallInteger

	self subclassResponsibility.
!

oopMinSmallInteger

	self subclassResponsibility.
!

oopMinusOne

	self subclassResponsibility.
!

oopNil

	self subclassResponsibility.
!

oopOne

	self subclassResponsibility.
!

oopRemoteNil

	self subclassResponsibility.
!

oopTrue

	self subclassResponsibility.
!

oopTwo

	self subclassResponsibility.
!

oopTypeArrayClass

	self subclassResponsibility.
!

oopTypeClass

	self subclassResponsibility.
!

oopTypeWithOop: anInteger

	self subclassResponsibility.
!

oopZero

	self subclassResponsibility.
!

pollForSignalSession: anInteger

	self subclassResponsibility.
!

sendInterpreted: aString to: anOopType with: anArray session: anInteger

	self subclassResponsibility.
!

session: session clearStack: processOop

	self subclassResponsibility.
!

session: session continue: gsProcessOop

	^self 
		session: session 
		continue: gsProcessOop 
		withObject: self oopNil
!

session: session continue: gsProcessOop withObject: anOop

	self subclassResponsibility.
!

session: session execute: stringOrOop context: contextOop

	self subclassResponsibility.
!

session: session fetchBytes: anOop

	self subclassResponsibility.
!

session: session oopForInteger: anInteger

	self subclassResponsibility.
!

session: session oopForString: aString

	self subclassResponsibility.
!

session: session releaseOops: oopList

	self subclassResponsibility.
!

session: anInteger send: aString to: anOopType with: anArray 

	self subclassResponsibility.
!

softBreakSession: anInteger

	self subclassResponsibility.
!

version

	self subclassResponsibility.
! !
!GciLibrary categoriesFor: #abortSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #beginSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #commitSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #errorStructureClass!private!subclassResponsibility! !
!GciLibrary categoriesFor: #hardBreakSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #loginHostUser:hostPassword:gsUser:gsPassword:gemNRS:stoneNRS:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #logoutSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #nbResult!public!subclassResponsibility! !
!GciLibrary categoriesFor: #oopAsciiNul!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopAt:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassArray!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassByteArray!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassDoubleByteString!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassQuadByteString!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassString!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassSymbol!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassSystem!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassUnicode16!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassUnicode32!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassUnicode7!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopFalse!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopForInteger:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #oopGemStoneError!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopIllegal!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopMaxSmallInteger!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopMinSmallInteger!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopMinusOne!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopNil!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopOne!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopRemoteNil!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopTrue!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopTwo!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopTypeArrayClass!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopTypeClass!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopTypeWithOop:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #oopZero!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #pollForSignalSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #sendInterpreted:to:with:session:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:clearStack:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:continue:!public! !
!GciLibrary categoriesFor: #session:continue:withObject:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:execute:context:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:fetchBytes:!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #session:oopForInteger:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:oopForString:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:releaseOops:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:send:to:with:!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #softBreakSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #version!public!subclassResponsibility! !

!GciLibrary class methodsFor!

addMissingMethods
	"The external function address is cached in the method and if it is inherited we will use the first one called.
	Having our own method allows us to have multiple libraries loaded and to call the proper function."

	self allSelectors do: [:each |
		| class method |
		class := self whichClassIncludesSelector: each.
		class ~~ self ifTrue: [
			method := class methodDictionary at: each.
			method isExternalCall ifTrue: [
				self addSelector: each withMethod: method copy clear; yourself.
			].
		].
	].!

displayName

	^nil.
!

fileName

	| list |
	list := OrderedCollection new.
	File
		for: self fileNameSearch
		in: SessionManager current imageBase , 'bin'
		do: [:each | list add: each fileName].
	list := list reject: [:each | each = '.'].	"This is needed after upgrading to Fusion 2 beta 1"
	list isEmpty ifTrue: [self error: 'library not found!!'].
	^list first.
!

onStartup2

	default := nil.
!

open: pathString

	| currentWorkingDirectory result |
	(default notNil and: [default handle notNil]) ifTrue: [^default].
	self addMissingMethods.
	currentWorkingDirectory := File workingDirectory.
	[
		File workingDirectory: SessionManager current imageBase , 'bin'.
		result := super open: pathString.
		self default: result.
	] ensure: [
		File workingDirectory: currentWorkingDirectory.
	].
	^result.
!

sessionStarted
"
	GciLibrary sessionStarted.
"
	GciErrSType32 new.
	GciErrSType64 new.
	GciErrSType64_30 new.
	GciErrSType64_31 new.
	GciTsObjInfo new.
	OopType32Array new.
	OopType64Array new.
	OopType32 new.
	OopType64 new.
!

withDisplayName: aString

	^self allSubclasses detect: [:each | each displayName = aString].
! !
!GciLibrary class categoriesFor: #addMissingMethods!public! !
!GciLibrary class categoriesFor: #displayName!public! !
!GciLibrary class categoriesFor: #fileName!public! !
!GciLibrary class categoriesFor: #onStartup2!public! !
!GciLibrary class categoriesFor: #open:!public! !
!GciLibrary class categoriesFor: #sessionStarted!public! !
!GciLibrary class categoriesFor: #withDisplayName:!public! !

GciMultiThreadedLibrary guid: (GUID fromString: '{FB4CDA56-2508-4FD1-BAC4-FF32805C1DD5}')!
GciMultiThreadedLibrary comment: ''!
!GciMultiThreadedLibrary categoriesForClass!Unclassified! !
!GciMultiThreadedLibrary methodsFor!

abortSession: session

	| didAbort gciErrSType |
	gciErrSType := self errorStructureClass new.
	didAbort := self GciMtAbort: session _: gciErrSType asParameter.
	didAbort ifFalse: [self gciError: gciErrSType].
!

beginSession: session

	| didBegin gciErrSType |
	gciErrSType := self errorStructureClass new.
	didBegin := self GciMtBegin: session _: gciErrSType asParameter.
	didBegin ifFalse: [self gciError: gciErrSType].
!

characterForOop: anOop

	^Character codePoint: (self GciMtOopToChar: anOop)

!

classForSpecial: anOop

	^self GciMtFetchSpecialClass: anOop

!

commitSession: session

	| didCommit gciErrSType |
	gciErrSType := self errorStructureClass new.
	didCommit := self GciMtCommit: session _: gciErrSType asParameter.
	didCommit ifFalse: [self gciError: gciErrSType].
!

DllRegisterServer
	<stdcall: hresult DllRegisterServer>
	^self invalidCall!

expect: typeName errorIfResultIs: errorResult inThreadDo: aTwoArgumentBlock

	| callback doneFlag gciErrSType forkResult mainResult |
	gciErrSType := self errorStructureClass new.
	doneFlag := false.
	callback := ExternalCallback 
		block: [:result | mainResult := result. doneFlag := true. 0]
		argumentTypes: typeName.
	forkResult := aTwoArgumentBlock value: gciErrSType asParameter value: callback asParameter.
	forkResult == 0 ifFalse: [self error: 'CreateThread() returned error #' , forkResult printString].
	[doneFlag] whileFalse: [SessionManager inputState pumpMessages. (Delay forMilliseconds: 1) wait].
	mainResult = errorResult ifTrue: [self gciError: gciErrSType].
	^mainResult!

fetchObjImpl: anOopType

	self subclassResponsibility.
!

gciError: aGciErrSType

	GciError signalWith: aGciErrSType.
!

GciMtAbort: session _: error

	<cdecl: bool GciMtAbort GciSessionId lpvoid>
	^self invalidCall
!

GciMtBegin: session _: error

	<cdecl: bool GciMtBegin GciSessionId lpvoid>
	^self invalidCall
!

GciMtBreak: session _: isHard _: error

	<cdecl: bool GciMtBreak GciSessionId bool lpvoid>
	^self invalidCall
!

GciMtCharToOop: myChar

	<cdecl: OopType64 GciMtCharToOop dword>
	^self invalidCall
!

GciMtClearStack: session _: process _: error

	<cdecl: bool GciMtClearStack GciSessionId OopType64 lpvoid>
	^self invalidCall
!

GciMtCommit: session _: error

	<cdecl: bool GciMtCommit GciSessionId lpvoid>
	^self invalidCall
!

GciMtContinueWith: session _: gsProcess _: replaceTopOfStack _: continueWithError _: flags _: error _: callback
	"Remember, this doesn't return an OopType64 but an error code on the thread fork attempt.
	The actual result is sent to the callback."

	<cdecl: sdword GciMtContinueWith GciSessionId OopType64 OopType64 lpvoid sdword lpvoid lpvoid>
	^self invalidCall
!

GciMtDoubleToOop: session _: double _: error

	<cdecl: OopType64 GciMtDoubleToOop GciSessionId double lpvoid>
	^self invalidCall
!

GciMtDoubleToSmallDouble: aFloat

	<cdecl: OopType64 GciMtDoubleToSmallDouble double>
	^self invalidCall
!

GciMtExecute: session _: sourceString _: sourceOop _: contextOop _: symbolListOop _: flags _: environment _: error _: callback
	"Remember, this doesn't return an OopType64 but an error code on the thread fork attempt.
	The actual result is sent to the callback."

	<cdecl: sdword GciMtExecute GciSessionId lpvoid OopType64 OopType64 OopType64 sdword word lpvoid lpvoid>
	^self invalidCall
!

GciMtFetchBytes: session _: theObject _: startIndex _: dest _: numBytes _: error

	<cdecl: sqword GciMtFetchBytes GciSessionId OopType64 sqword lpvoid sqword lpvoid>
	^self invalidCall
!

GciMtFetchChars: session _: theObject _: startIndex _: cString _: maxSize _: error

	<cdecl: sqword GciMtFetchChars GciSessionId OopType64 sqword lpvoid sqword lpvoid>
	^self invalidCall
!

GciMtFetchClass: session _: theObject _: error

	<cdecl: OopType64 GciMtFetchClass GciSessionId OopType64 lpvoid>
	^self invalidCall
!

GciMtFetchObjInfo: session _: objId _: addToExportSet _: result _: buffer _: bufsize _: error

	<cdecl: sqword GciMtFetchObjInfo GciSessionId OopType64 bool lpvoid lpvoid sdword lpvoid>
	^self invalidCall
"
EXTERN_GCI_DEC(int64) GciTsFetchObjInfo(GciSession sess, OopType objId, 
	BoolType addToExportSet, GciTsObjInfo *result, 
	ByteType *buffer, size_t bufSize, GciErrSType *err);
  // Function result is >= 0 if *result filled in,
  //   -1 if an error was returned in *err .
  // client side handling of special objects as before.
  // addToExportSet has effect only if function result is 1
  // if buffer not NULL, then up to bufSize bytes of the body of the object
  // are returned in *buffer, and function result is the number of instVars returned
  // If read authorization is denied for objId, then result->access == 0 ,
  // the rest of *result other than result->objId is zero , and function result is zero.
"
!

GciMtFetchOops: session _: theObject _: startIndex _: theOops _: numOops _: error

	<cdecl: sdword GciMtFetchOops GciSessionId OopType64 sqword OopType64Array* sdword lpvoid>
	^self invalidCall
!

GciMtFetchSize: session _: theObject _: error

	<cdecl: sqword GciMtFetchSize GciSessionId OopType64 lpvoid>
	^self invalidCall
!

GciMtFetchSpecialClass: anOop

	<cdecl: OopType64 GciMtFetchSpecialClass OopType64>
	^self invalidCall
!

GciMtFetchVaryingSize: session _: theObject _: error

	<cdecl: sqword GciMtFetchVaryingSize GciSessionId OopType64 lpvoid>
	^self invalidCall
!

GciMtGemTrace: session _: enable _: error

	<cdecl: sdword GciMtGemTrace GciSessionId sdword lpvoid>
	^self invalidCall
!

GciMtI64ToOop: session _: anInteger _: error

	<cdecl: OopType64 GciMtI64ToOop GciSessionId sqword lpvoid>
	^self invalidCall
!

GciMtInit: gciLibraryPath

	<cdecl: dword GciMtInit lpstr>
	^self invalidCall
!

GciMtLogin: stoneName 
	_:hostUserId 
	_: hostPassword 
	_: hostPwIsEncrypted 
	_: gemServiceNRS 
	_: gsUsername 
	_: gsPassword 
	_: loginFlags 
	_: haltOnErrNum 
	_: gciErrSType 
	_: callback
	"Remember, this doesn't return a session but an error code on the thread fork attempt.
	The actual result is sent to the callback."

	<cdecl: sdword GciMtLogin lpvoid lpvoid lpvoid bool lpvoid lpvoid lpvoid dword sdword lpvoid lpvoid>
	^self invalidCall
!

GciMtLogout: session _: error

	<cdecl: bool GciMtLogout GciSessionId lpvoid>
	^self invalidCall
!

GciMtNewString: session _: string _: error

	<cdecl: OopType64 GciMtNewString GciSessionId lpstr lpvoid>
	^self invalidCall
!

GciMtObjExists: session _: anOop

	<cdecl: bool GciMtObjExists GciSessionId OopType64>
	^self invalidCall
!

GciMtOopIsSpecial: anOop

	<cdecl: bool GciMtOopIsSpecial OopType64>
	^self invalidCall
!

GciMtOopToChar: anOop

	<cdecl: sdword GciMtOopToChar OopType64>
	^self invalidCall
!

GciMtOopToDouble: session _: anOop _: result _: error

	<cdecl: bool GciMtOopToDouble GciSessionId OopType64 lpvoid lpvoid>
	^self invalidCall
!

GciMtOopToI64: session _: anOop _: result _: error

	<cdecl: bool GciMtOopToI64 GciSessionId OopType64 lpvoid lpvoid>
	^self invalidCall
!

GciMtPerform: session _: reciverOop _: symbolOop _: selectorString _: argumentList _: argumentCount _: flags _: environment _: error _: callback
	"Remember, this doesn't return an OopType64 but an error code on the thread fork attempt.
	The actual result is sent to the callback."

	<cdecl: sdword GciMtPerform GciSessionId OopType64 OopType64 lpvoid qword* sdword sdword word lpvoid lpvoid>
	^self invalidCall
!

GciMtReleaseAllObjs: session _: error

	<cdecl: bool GciMtReleaseAllObjs GciSessionId lpvoid>
	^self invalidCall
!

GciMtReleaseObjs: session _: oopList _: count _: error

	<cdecl: bool GciMtReleaseAllObjs GciSessionId OopType64Array* sdword lpvoid>
	^self invalidCall
!

GciMtResolveSymbol: session _: string _: anOop _: error

	<cdecl: OopType64 GciMtResolveSymbol GciSessionId lpstr OopType64 lpvoid>
	^self invalidCall
!

GciMtResolveSymbolObj: session _: stringOop _: symbolListOop _: error

	<cdecl: OopType64 GciMtResolveSymbolObj GciSessionId OopType64 OopType64 lpvoid>
	^self invalidCall
!

GciMtSessionIsRemote: sessionId

	<cdecl: sdword GciMtSessionIsRemote GciSessionId>
	^self invalidCall
!

GciMtVersion: buffer _: size

	<cdecl: dword GciMtVersion lpstr dword>
	^self invalidCall
!

hardBreakSession: session

	self session: session breakHard: true.
!

initialize
"
	LibGciRpc64_3_3 new initialize.
"
	| errorNumber |
	OOP_ILLEGAL := OopType64 fromInteger: 1.
	OOP_NIL := OopType64 fromInteger: 20.
	SessionManager current 
		setenv: 'GEMSTONE_LIB' 
		value: File workingDirectory.
	errorNumber := self GciMtInit: self threadSafeLibraryName.
	errorNumber == 0 ifTrue: [^self].
	self error: 'initialize returned ' , errorNumber printString.
!

loginHostUser: hostUser hostPassword: hostPassword gsUser: gsUser gsPassword: gsPassword gemNRS: gemString stoneNRS: stoneString

	^self
		loginToStone: stoneString
		hostUser: hostUser
		hostPassword: hostPassword
		hostPasswordIsEncrypted: false 
		gemNRS: gemString 
		gsUser: gsUser
		gsPassword: gsPassword 
		loginFlags: 0 
		haltOnErrorNumber: 0 
!

loginToStone: stoneNRS
	hostUser: hostUser
	hostPassword: hostPassword 
	hostPasswordIsEncrypted: isEncrypted 
	gemNRS: gemNRS 
	gsUser: gsUser
	gsPassword: gsPassword 
	loginFlags: loginFlags 
	haltOnErrorNumber: errNum 

	| stoneNrsEx hostUserEx hostPasswordEx gemNrsEx gsUserEx gsPasswordEx |
	stoneNrsEx := ExternalMemory fromString: stoneNRS.
	hostUserEx := ExternalMemory fromString: hostUser.
	hostPasswordEx := ExternalMemory fromString: hostPassword.
	gemNrsEx := ExternalMemory fromString: gemNRS.
	gsUserEx := ExternalMemory fromString: gsUser.
	gsPasswordEx := ExternalMemory fromString: gsPassword.

	^self expect: 'GciSessionId' errorIfResultIs: 0 inThreadDo: [:gciErrSType :callback |
		self 
			GciMtLogin: stoneNrsEx asParameter
			_: hostUserEx asParameter
			_: hostPasswordEx asParameter
			_: isEncrypted
			_: gemNrsEx asParameter
			_: gsUserEx asParameter
			_: gsPasswordEx asParameter
			_: loginFlags
			_: errNum
			_: gciErrSType 
			_: callback.
	].
!

loginUser: userString password: passwordString gemNRS: gemString stoneNRS: stoneString

	^self
		loginToStone: stoneString
		hostUser: ''
		hostPassword: '' 
		hostPasswordIsEncrypted: false 
		gemNRS: gemString 
		gsUser: userString
		gsPassword: passwordString 
		loginFlags: 0 
		haltOnErrorNumber: 0 
!

logoutSession: session

	| didLogout gciErrSType | 
	gciErrSType := self errorStructureClass new.
	didLogout := self GciMtLogout: session _: gciErrSType asParameter.
	didLogout ifFalse: [self gciError: gciErrSType].
!

oopAsciiNul

	^OopType64 fromInteger: 28. "16r1C"
!

oopClassArray

	^OopType64 fromInteger: 66817.
!

oopClassByteArray

	^OopType64 fromInteger: 103425.!

oopClassCharacter

	^OopType64 fromInteger: 68353.
!

oopClassDoubleByteString

	^OopType64 fromInteger: 143873.
!

oopClassQuadByteString

	^OopType64 fromInteger: 144385.
!

oopClassSmallDouble

	^OopType64 fromInteger: 121345.
!

oopClassSmallFraction

	^OopType64 fromInteger: 156161.
!

oopClassSmallInteger

	^OopType64 fromInteger: 74241.
!

oopClassString

	^OopType64 fromInteger: 74753.
!

oopClassSymbol

	^OopType64 fromInteger: 110849.
!

oopClassSystem

	^OopType64 fromInteger: 76033.
!

oopClassUnicode16

	^OopType64 fromInteger: 154625.
!

oopClassUnicode32

	^OopType64 fromInteger: 154881.
!

oopClassUnicode7

	^OopType64 fromInteger: 154369.
!

oopFalse

	^OopType64 fromInteger: 12. "16r0C"
!

oopForCharacter: aCharacter

	^self GciMtCharToOop: aCharacter codePoint

!

oopForSmallDouble: aFloat

	| oop |
	oop := self GciMtDoubleToSmallDouble: aFloat.
	oop == OOP_ILLEGAL ifTrue: [self error: 'Unable to convert ' , aFloat printString , ' to a SmallDouble'].
	^oop!

oopGemStoneError

	^OopType64 fromInteger:  231169.!

oopIllegal

	^OopType64 fromInteger: 1. "16r01"
!

oopIsSpecial: anOop

	^self GciMtOopIsSpecial: anOop

!

oopMaxSmallInteger

	^OopType64 fromInteger: 16r7FFFFFFFFFFFFFFA
!

oopMinSmallInteger

	^OopType64 fromInteger: -16r7FFFFFFFFFFFFFFE
!

oopMinusOne

	^OopType64 fromInteger: -6.
!

oopNil

	^OopType64 fromInteger: 20. "16r14"
!

oopOne

	^OopType64 fromInteger: 10.
!

oopRemoteNil

	^OopType64 fromInteger: 276. "16r114"
!

oopTrue

	^OopType64 fromInteger: 268.	"16r10C"
!

oopTwo

	^OopType64 fromInteger: 18.
!

oopTypeArrayClass

	^OopType64Array.!

oopTypeClass

	^OopType64.
!

oopTypeWithOop: anInteger

	| int bytes |
	bytes := ByteArray new: 8.
	bytes 
		qwordAtOffset: 0 
		put: anInteger.
	int := bytes sqwordAtOffset: 0.
	^OopType64 fromInteger: int.
!

oopZero

	^OopType64 fromInteger: 2.
!

releaseAllObjectsInSession: session

	| didRelease gciErrSType |
	gciErrSType := self errorStructureClass new.
	didRelease := self GciMtReleaseAllObjs: session _: gciErrSType asParameter.
	didRelease ifFalse: [self gciError: gciErrSType].
!

session: session breakHard: isHard

	| flag gciErrSType |
	gciErrSType := self errorStructureClass new.
	flag := self GciMtBreak: session _: isHard _: gciErrSType asParameter.
	flag ifFalse: [self gciError: gciErrSType].
!

session: session clearStack: process

	| didAbort gciErrSType |
	gciErrSType := self errorStructureClass new.
	didAbort := self GciMtClearStack: session _: process _: gciErrSType asParameter.
	didAbort ifFalse: [self gciError: gciErrSType].
!

session: session continue: gsProcessOop with: anOop error: aGciErrSType

	^self 
		session: session 
		continue: gsProcessOop 
		with: anOop 
		error: aGciErrSType 
		flags: 1 "GCI_PERFORM_FLAG_ENABLE_DEBUG"
!

session: session continue: gsProcessOop with: anOop error: aGciErrSType flags: flags

	| result |
	result := self expect: 'OopType64' errorIfResultIs: OOP_ILLEGAL inThreadDo: [:gciErrSType :callback |
		self 
			GciMtContinueWith: session
			_: gsProcessOop 
			_: anOop 
			_: aGciErrSType 
			_: flags 
			_: gciErrSType  
			_: callback.
	].
	^self session: session valueOfOop: result

!

session: session continue: gsProcessOop withError: aGciErrSType

	^self 
		session: session 
		continue: gsProcessOop 
		with: OOP_ILLEGAL 
		error: aGciErrSType
!

session: session continue: gsProcessOop withObject: anOop

	^self 
		session: session 
		continue: gsProcessOop 
		with: anOop 
		error: nil
!

session: session doubleForOop: anOop

	| bytes flag gciErrSType  |
	bytes := ByteArray new: 8.
	gciErrSType := self errorStructureClass new.
	flag := self GciMtOopToDouble: session _: anOop _: bytes asParameter _: gciErrSType asParameter.
	flag ifFalse: [self gciError: gciErrSType].
	^bytes doubleAtOffset: 0!

session: session execute: stringOrOop

	^self session: session execute: stringOrOop context: OOP_ILLEGAL symbolList: OOP_NIL.!

session: session execute: stringOrOop context: contextOop

	^self session: session execute: stringOrOop context: contextOop symbolList: OOP_NIL.!

session: session execute: stringOrOop context: contextOop symbolList: symbolListOop

	| result stringEx stringOop |
	(stringOrOop isKindOf: String) ifTrue: [
		stringEx := ExternalMemory fromString: stringOrOop.
		stringOop := self oopClassString.
	] ifFalse: [
		stringEx := OOP_NIL.
		stringOop := stringOrOop.
	].
	result := self expect: 'OopType64' errorIfResultIs: OOP_ILLEGAL inThreadDo: [:gciErrSType :callback |
		self
			GciMtExecute: session 
			_: stringEx asParameter 
			_: stringOop  
			_: contextOop 
			_: symbolListOop 
			_: 1 "GCI_PERFORM_FLAG_ENABLE_DEBUG"
			_: 0 "environment" 
			_: gciErrSType  
			_: callback.
	].
	^self session: session valueOfOop: result
!

session: session fetchBytes: anOop

	| bytes gciErrSType result |
	gciErrSType := self errorStructureClass new.
	bytes := ByteArray new: 1000.
	result := self GciMtFetchBytes: session _: anOop _: 1 _: bytes _: bytes size _: gciErrSType.
	result < 0 ifTrue: [self gciError: gciErrSType].
	(0 <= result and: [result < bytes size]) ifTrue: [^bytes copyFrom: 1 to: result].
	bytes := ByteArray new: (self session: session fetchSize: anOop).
	result := self GciMtFetchBytes: session _: anOop _: 1 _: bytes _: bytes size _: gciErrSType.
	^bytes!

session: session fetchClass: anOop

	| gciErrSType result |
	gciErrSType := self errorStructureClass new.
	result := self GciMtFetchClass: session _: anOop _: gciErrSType asParameter.
	result == OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^result!

session: session fetchObject: anOop

	| buffer class gciErrSType gciTsObjInfo implementation result |
	gciErrSType := self errorStructureClass new.
	gciTsObjInfo := GciTsObjInfo new.
	buffer := ByteArray new: 1000.
	result := self 
		GciMtFetchObjInfo: session 
		_: anOop 
		_: false "addToExportSet"
		_: gciTsObjInfo asParameter 
		_: buffer asParameter
		_: buffer size
		_: gciErrSType asParameter.
	result < 0 ifTrue: [self gciError: gciErrSType].
	gciTsObjInfo data: anOop.
	(implementation := gciTsObjInfo implementation) == 0 "OOP pointers" ifTrue: [
		buffer := buffer copyFrom: 1 to: result * 8.
		gciTsObjInfo data: (self oopTypeArrayClass fromBytes: buffer).
		^gciTsObjInfo.
	]. 
	implementation == 2 "NSC" ifTrue: [^gciTsObjInfo].
	class := gciTsObjInfo objClass.
	implementation == 3 "Special" ifTrue: [
		anOop = self oopNil 						ifTrue: [gciTsObjInfo data: nil	] ifFalse: [
		anOop = self oopTrue 					ifTrue: [gciTsObjInfo data: true	] ifFalse: [
		anOop = self oopFalse 					ifTrue: [gciTsObjInfo data: false	] ifFalse: [
		class = self oopClassSmallInteger 	ifTrue: [gciTsObjInfo data: (self session: session integerForOop: anOop)	] ifFalse: [
		class = self oopClassSmallDouble 	ifTrue: [gciTsObjInfo data: (self session: session doubleForOop: anOop)	] ifFalse: [
		class = self oopClassCharacter		ifTrue: [gciTsObjInfo data: (self characterForOop: anOop)						] ifFalse: [
		class = self oopClassSmallFraction	ifTrue: [gciTsObjInfo data: anOop asFraction											] ifFalse: [
		self error: 'Unrecognized special: ' , anOop printString]]]]]]].
		^gciTsObjInfo
	].
	"bytes"
	result < gciTsObjInfo objSize ifTrue: [
			buffer := ByteArray new: gciTsObjInfo objSize.
			result := self GciMtFetchBytes: session _: anOop _: 1 _: buffer _: buffer size _: gciErrSType.
			result < 0 ifTrue: [self gciError: gciErrSType].
	].
	buffer := buffer copyFrom: 1 to: result.
	class = self oopClassByteArray 			value ifTrue: [gciTsObjInfo data: buffer											] ifFalse: [
	class = self oopClassString 					value ifTrue: [gciTsObjInfo data: buffer asString								] ifFalse: [
	class = self oopClassSymbol 					value ifTrue: [gciTsObjInfo data: buffer asString asSymbol				] ifFalse: [
	class = self oopClassUnicode7 				value ifTrue: [gciTsObjInfo data: (Unicode7 withAll: buffer				)] ifFalse: [
	class = self oopClassUnicode16 			value ifTrue: [gciTsObjInfo data: (Unicode16 withAll: buffer			)] ifFalse: [
	class = self oopClassUnicode32 			value ifTrue: [gciTsObjInfo data: (Unicode32 withAll: buffer			)] ifFalse: [
	class = self oopClassDoubleByteString 	value ifTrue: [gciTsObjInfo data: (DoubleByteString withAll: buffer	)] ifFalse: [
	class = self oopClassQuadByteString 		value ifTrue: [gciTsObjInfo data: (QuadByteString withAll: buffer	)] ifFalse: [
	self halt]]]]]]]].
	^gciTsObjInfo!

session: session fetchObjects: anOop

	| count gciErrSType objects result |
	gciErrSType := self errorStructureClass new.
	count := 100.
	objects := self oopTypeArrayClass new: count.
	result := self GciMtFetchOops: session _: anOop _: 1 _: objects _: count _: gciErrSType.
	result < 0 ifTrue: [self gciError: gciErrSType].
	result == 0 ifTrue: [^self oopTypeArrayClass new].
	result == count ifTrue: [
		count := self session: session fetchSize: anOop.
		objects := self oopTypeArrayClass new: count.
		result := self GciMtFetchOops: session _: anOop _: 1 _: objects _: count _: gciErrSType.
		result < 0 ifTrue: [self gciError: gciErrSType].
	] ifFalse: [
		objects size: count.
	].
	^objects!

session: session fetchSize: anOop

	| gciErrSType result |
	gciErrSType := self errorStructureClass new.
	result := self GciMtFetchSize: session _: anOop _: gciErrSType asParameter.
	result < 0 ifTrue: [self gciError: gciErrSType].
	^result!

session: session fetchString: anOop

	| string gciErrSType result |
	gciErrSType := self errorStructureClass new.
	string := String new: 1000.
	result := self GciMtFetchChars: session _: anOop _: 1 _: string _: string size _: gciErrSType.
	result < 0 ifTrue: [self gciError: gciErrSType].
	result == 0 ifTrue: [^''].
	result + 1 < string size ifTrue: [^string copyFrom: 1 to: result].
	string := String new: (self session: session fetchSize: anOop) + 1.
	result := self GciMtFetchChars: session _: anOop _: 1 _: string _: string size _: gciErrSType.
	^string copyFrom: 1 to: result!

session: session fetchVaryingSize: anOop

	| gciErrSType result |
	gciErrSType := self errorStructureClass new.
	result := self GciMtFetchVaryingSize: session _: anOop _: gciErrSType asParameter.
	result < 0 ifTrue: [self gciError: gciErrSType].
	^result!

session: session gemTrace: anInteger

	| oldValue gciErrSType |
	gciErrSType := self errorStructureClass new.
	oldValue := self GciMtGemTrace: session _: anInteger _: gciErrSType asParameter.
	oldValue < 0 ifTrue: [self gciError: gciErrSType].
	^oldValue!

session: session integerForOop: anOop

	| bytes flag gciErrSType  |
	bytes := ByteArray new: 8.
	gciErrSType := self errorStructureClass new.
	flag := self GciMtOopToI64: session _: anOop _: bytes asParameter _: gciErrSType asParameter.
	flag ifFalse: [self gciError: gciErrSType].
	^bytes sqwordAtOffset: 0!

session: session objectExists: anOop

	^self GciMtObjExists: session _: anOop
!

session: session objectNamed: aString

	^self session: session objectNamed: aString inSymbolList: OOP_NIL
!

session: session objectNamed: aString inSymbolList: symbolList

	| oop gciErrSType |
	gciErrSType := self errorStructureClass new.
	oop := self GciMtResolveSymbol: session _: aString asParameter _: symbolList _: gciErrSType asParameter.
	oop == OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^oop
!

session: session objectNamedOop: stringOop inSymbolList: symbolList

	| oop gciErrSType |
	gciErrSType := self errorStructureClass new.
	oop := self GciMtResolveSymbolObj: session _: stringOop _: symbolList _: gciErrSType asParameter.
	oop == OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^oop
!

session: session oopForDouble: aFloat

	| oop gciErrSType |
	gciErrSType := self errorStructureClass new.
	oop := self GciMtDoubleToOop: session _: aFloat _: gciErrSType asParameter.
	oop == OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^oop!

session: session oopForInteger: anInteger

	| oop gciErrSType |
	gciErrSType := self errorStructureClass new.
	oop := self GciMtI64ToOop: session _: anInteger asParameter _: gciErrSType asParameter.
	oop == OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^oop
!

session: session oopForString: aString

	| oop gciErrSType |
	gciErrSType := self errorStructureClass new.
	oop := self GciMtNewString: session _: aString asParameter _: gciErrSType asParameter.
	oop == OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^oop
!

session: session releaseOops: anOopType64Array

	| didRelease gciErrSType |
	gciErrSType := self errorStructureClass new.
	didRelease := self GciMtReleaseObjs: session _: anOopType64Array _: anOopType64Array size _: gciErrSType asParameter.
	didRelease ifFalse: [self gciError: gciErrSType].
!

session: session send: stringOrOop to: receiverOop

	^self session: session send: stringOrOop to: receiverOop with: self oopTypeArrayClass new!

session: session send: stringOrOop to: receiverOop with: anOopType64Array

	| argumentsEx result string selectorOop |
	argumentsEx := ExternalMemory fromString: anOopType64Array bytes asString.
	(stringOrOop isKindOf: String) ifTrue: [
		string := ExternalMemory fromString: stringOrOop.
		selectorOop := OOP_ILLEGAL.
	] ifFalse: [
		string := OOP_NIL.
		selectorOop := stringOrOop.
	].
	result := self expect: 'OopType64' errorIfResultIs: OOP_ILLEGAL inThreadDo: [:gciErrSType :callback |
		self 
			GciMtPerform: session 
			_: receiverOop
			_: selectorOop 
			_: string asParameter 
			_: argumentsEx asParameter
			_: anOopType64Array size 
			_: 1 "GCI_PERFORM_FLAG_ENABLE_DEBUG"
			_: 0 "environment" 
			_: gciErrSType asParameter 
			_: callback asParameter].
	^self session: session valueOfOop: result!

session: session valueOfOop: anOop

	| data objectInfo |
	objectInfo := self session: session fetchObject: anOop.
	anOop objectInfo: objectInfo.
	data := objectInfo data.
	^(data isKindOf: OopType64Array) 
		ifTrue: [anOop]
		ifFalse: [data]
!

sessionIsRemote: session

	| resultCode |
	resultCode := self GciMtSessionIsRemote: session.
	resultCode == -1 ifTrue: [self error: 'invalid session'].
	^resultCode == 1!

softBreakSession: session

	self session: session breakHard: false.!

specialFromOop: anOop

	anOop isSmallFraction ifTrue: [
		^anOop asFraction.
	].
	^super specialFromOop: anOop
!

threadSafeLibraryName

	self subclassResponsibility!

version

	| productCode string |
	string := String new: 1024.
	productCode := self GciMtVersion: string _: string size.
	productCode == 3 ifFalse: [self error: 'Unexpected product code (' , productCode printString , ')'].
	string := string copyFrom: 1 to: (string indexOf: (Character codePoint: 0)) - 1.
	^string! !
!GciMultiThreadedLibrary categoriesFor: #abortSession:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #beginSession:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #characterForOop:!public! !
!GciMultiThreadedLibrary categoriesFor: #classForSpecial:!public! !
!GciMultiThreadedLibrary categoriesFor: #commitSession:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #DllRegisterServer!private! !
!GciMultiThreadedLibrary categoriesFor: #expect:errorIfResultIs:inThreadDo:!private!threaded! !
!GciMultiThreadedLibrary categoriesFor: #fetchObjImpl:!private!subclassResponsibility! !
!GciMultiThreadedLibrary categoriesFor: #gciError:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtAbort:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtBegin:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtBreak:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtCharToOop:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtClearStack:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtCommit:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtContinueWith:_:_:_:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtDoubleToOop:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtDoubleToSmallDouble:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtExecute:_:_:_:_:_:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtFetchBytes:_:_:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtFetchChars:_:_:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtFetchClass:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtFetchObjInfo:_:_:_:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtFetchOops:_:_:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtFetchSize:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtFetchSpecialClass:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtFetchVaryingSize:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtGemTrace:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtI64ToOop:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtInit:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtLogin:_:_:_:_:_:_:_:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtLogout:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtNewString:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtObjExists:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtOopIsSpecial:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtOopToChar:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtOopToDouble:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtOopToI64:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtPerform:_:_:_:_:_:_:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtReleaseAllObjs:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtReleaseObjs:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtResolveSymbol:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtResolveSymbolObj:_:_:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtSessionIsRemote:!private! !
!GciMultiThreadedLibrary categoriesFor: #GciMtVersion:_:!private! !
!GciMultiThreadedLibrary categoriesFor: #hardBreakSession:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #initialize!private! !
!GciMultiThreadedLibrary categoriesFor: #loginHostUser:hostPassword:gsUser:gsPassword:gemNRS:stoneNRS:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #loginToStone:hostUser:hostPassword:hostPasswordIsEncrypted:gemNRS:gsUser:gsPassword:loginFlags:haltOnErrorNumber:!public!threaded! !
!GciMultiThreadedLibrary categoriesFor: #loginUser:password:gemNRS:stoneNRS:!public! !
!GciMultiThreadedLibrary categoriesFor: #logoutSession:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #oopAsciiNul!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassArray!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassByteArray!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassCharacter!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassDoubleByteString!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassQuadByteString!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassSmallDouble!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassSmallFraction!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassSmallInteger!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassString!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassSymbol!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassSystem!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassUnicode16!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassUnicode32!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopClassUnicode7!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopFalse!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopForCharacter:!public! !
!GciMultiThreadedLibrary categoriesFor: #oopForSmallDouble:!public! !
!GciMultiThreadedLibrary categoriesFor: #oopGemStoneError!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopIllegal!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopIsSpecial:!public! !
!GciMultiThreadedLibrary categoriesFor: #oopMaxSmallInteger!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopMinSmallInteger!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopMinusOne!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopNil!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopOne!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopRemoteNil!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopTrue!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopTwo!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopTypeArrayClass!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #oopTypeClass!public! !
!GciMultiThreadedLibrary categoriesFor: #oopTypeWithOop:!public! !
!GciMultiThreadedLibrary categoriesFor: #oopZero!public!Reserved OOPs! !
!GciMultiThreadedLibrary categoriesFor: #releaseAllObjectsInSession:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #session:breakHard:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:clearStack:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #session:continue:with:error:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:continue:with:error:flags:!public!threaded! !
!GciMultiThreadedLibrary categoriesFor: #session:continue:withError:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:continue:withObject:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #session:doubleForOop:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:execute:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:execute:context:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #session:execute:context:symbolList:!public!threaded! !
!GciMultiThreadedLibrary categoriesFor: #session:fetchBytes:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #session:fetchClass:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:fetchObject:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:fetchObjects:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #session:fetchSize:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:fetchString:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:fetchVaryingSize:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:gemTrace:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:integerForOop:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:objectExists:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:objectNamed:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:objectNamed:inSymbolList:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:objectNamedOop:inSymbolList:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:oopForDouble:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:oopForInteger:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:oopForString:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #session:releaseOops:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #session:send:to:!public! !
!GciMultiThreadedLibrary categoriesFor: #session:send:to:with:!public!required!threaded! !
!GciMultiThreadedLibrary categoriesFor: #session:valueOfOop:!public! !
!GciMultiThreadedLibrary categoriesFor: #sessionIsRemote:!public! !
!GciMultiThreadedLibrary categoriesFor: #softBreakSession:!public!required! !
!GciMultiThreadedLibrary categoriesFor: #specialFromOop:!public! !
!GciMultiThreadedLibrary categoriesFor: #threadSafeLibraryName!private! !
!GciMultiThreadedLibrary categoriesFor: #version!public! !

!GciMultiThreadedLibrary class methodsFor!

fileName

	^'GciMt.dll'.
! !
!GciMultiThreadedLibrary class categoriesFor: #fileName!public! !

GciSingleThreadedLibrary guid: (GUID fromString: '{AFFFAFEB-5777-4D4B-A7D0-931922158468}')!
GciSingleThreadedLibrary comment: 'JadeLoginShell show.
GciLibrary current gciCommit.
GciLibrary current nbCallResult.
GciLibrary current nbCallResultOop.


| library |
library := GciLibrary current.
Array 
	with: library nbCallDone
	with: library gciNbCommit
	with: library nbCallDone
	with: library nbCallResult.
'!
!GciSingleThreadedLibrary categoriesForClass!Unclassified! !
!GciSingleThreadedLibrary methodsFor!

_semaphore

	^semaphore.
!

abortSession: anInteger

	self critical: [
		self 
			gciSetSessionId: anInteger;
			gciAbort;
			signalIfError;
			yourself.
	].
!

beginSession: anInteger

	self critical: [
		self 
			gciSetSessionId: anInteger;
			gciBegin;
			signalIfError;
			yourself.
	].
!

close

	[
		self gciShutdown.
	] on: Error do: [:ex | 
		ex return.
	].
	^super close.
!

commitSession: anInteger

	self critical: [
		self gciSetSessionId: anInteger.
		self gciCommit ifTrue: [^self].
		self signalIfError.
		GciCommitFailure signal.
	].
!

critical: aBlock
	"Previous implementation had a small window when semaphore could be acquired and then process terminated.
	This should (!!) close that window."

	| array result |
	array := Array with: nil.
	[
		[
			semaphore wait: -1 "INFINITE" ret: array.		"If we are successful, the Array will contain a non-nil value"
			result := aBlock value.
			self signalSemaphoreIfNeeded: array.
		] ifCurtailed: [
			self signalSemaphoreIfNeeded: array.
		].
		^result.
	] on: Error do: [:ex | 
		self signalSemaphoreIfNeeded: array.
		result := ex pass.
		SessionManager current isRuntime ifFalse: [self halt].
		^result.
	].
!

fetchBytes: anOopType

	| oopClass |
	oopClass := self fetchClass: anOopType.
	oopClass = self oopClassString 						ifTrue: [^self fetchChars: anOopType].
	oopClass = self oopClassSymbol 					ifTrue: [^(self fetchChars: anOopType) asSymbol].
	oopClass = self oopClassByteArray 				ifTrue: [^self fetchBytes: anOopType class: ByteArray].
	oopClass = self oopClassUnicode7 				ifTrue: [^self fetchBytes: anOopType class: Unicode7].
	oopClass = self oopClassUnicode16 				ifTrue: [^self fetchBytes: anOopType class: Unicode16].
	oopClass = self oopClassUnicode32 				ifTrue: [^self fetchBytes: anOopType class: Unicode32].
	oopClass = self oopClassDoubleByteString 	ifTrue: [^self fetchBytes: anOopType class: DoubleByteString].
	oopClass = self oopClassQuadByteString		ifTrue: [^self fetchBytes: anOopType class: QuadByteString].
	^anOopType.
!

fetchBytes: anOopType class: aClass

	| size bytes result |
	size := self gciFetchSize: anOopType.
	bytes := aClass new: size.
	result := self 
		gciFetchBytes: anOopType
		_: 1
		_: bytes
		_: size.
	result = size ifTrue: [^bytes].
	self signalLastError.
!

fetchChars: anOopType

	| size string result |
	size := self gciFetchSize: anOopType.
	string := String new: size + 1.
	result := self 
		gciFetchBytes: anOopType
		_: 1
		_: string
		_: size.
	result = size ifTrue: [^string copyFrom: 1 to: size].
	self signalLastError.
!

fetchClass: anOopType

	| result |
	result := self gciFetchClass: anOopType.
	result = self oopNil ifTrue: [self signalLastError].
	^result.
!

fetchObjImpl: anOopType
	"0=pointer, 1=byte, 2=NSC, or 3=special
	If object is special, then we know on the client; otherwise we need to go to the server."

	| result |
	result := self gciFetchObjImpl: anOopType.
	result ~~ 3 ifTrue: [self signalIfError].
	^result.
!

gciAbort

	<cdecl: void GciAbort>
	^self invalidCall
!

gciBegin

	<cdecl: void GciBegin>
	^self invalidCall
!

gciCallInProgress

	<cdecl: bool GciCallInProgress>
	^self invalidCall
!

gciClearStack: processOop

	self subclassResponsibility.
!

gciCommit

	<cdecl: bool GciCommit>
	^self invalidCall
!

gciErr: errorReport

	self subclassResponsibility.!

gciFetchBytes: anOopType _: startIndex _: cString _: maxSize

	self subclassResponsibility.!

gciFetchChars: anOopType _: startIndex _: cString _: maxSize

	self subclassResponsibility!

gciFetchClass: oop

	self subclassResponsibility!

gciFetchObjImpl: anObject

	self subclassResponsibility!

gciFetchSize: anObject

	self subclassResponsibility!

gciFetchVaryingOops: anObject _: startIndex _: theOops _: numOops

	self subclassResponsibility!

gciGemTrace: anInteger
"$GEMSTONE/include/gci.hf line 5098

/* GciGemTrace
    For use in debugging the implementation or client code.
    level = 0 none, 1 commands, 2 commands+args , 3 even more
    Function result is previous value of the tracing state.
    Also enabled by  export GS_LGC_DEBUG=<level>
    such as   export GS_LGC_DEBUG=2 
    in enviroments of libgcirpc.so and of netldi .
    The when level > 0 the gem process will write trace information to
    it's log file; the libgcirpc.so will write trace information
    to a gci<pid>trace.log file in the current directory of the client process.
 */
  EXTERN_GCI_DEC(int) 
GciGemTrace(int level);"

	"Interpreted as #int32 from #( #'int32' )"

	<cdecl: sdword GciGemTrace sdword>
	^self invalidCall
!

gciGetSessionId

	<cdecl: sdword GciGetSessionId>
	^self invalidCall
!

gciHardBreak

	<cdecl: void GciHardBreak>
	^self invalidCall
!

gciInit

	<cdecl: bool GciInit>
	^self invalidCall
!

gciLogin: userName _: password

	<cdecl: bool GciLogin lpstr lpstr>
	^self invalidCall
!

gciLogout

	<cdecl: void GciLogout>
	^self invalidCall
!

gciNbContinueWith: process _: replaceTopOfStack _: flags _: error

	self subclassResponsibility!

gciNbEnd: result
	"GciNbProgressEType GciNbEnd(void ** result);"

	<cdecl: sdword GciNbEnd lppvoid>
	^self invalidCall

"
/* Nonblocking support */
typedef enum {
  GCI_RESULT_NOT_READY,		/* nothing happened */
  GCI_RESULT_PROGRESSED,	/* a packet was received */
  GCI_RESULT_READY		/* your result is now ready */
  } GciNbProgressEType;

result	The address at which GciNbEnd should place a pointer to the result of the nonblocking call when it is complete.

	lppvoid	Pointer to pointer. Used for functions which take a parameter into which they write
		an address. The corresponding argument must be an ExternalAddress (or other indirection
		object), or an object whose first instance variable is such (e.g. an ExternalStructure).
		The address of the ExternalAddress itself is passed, so that on return it contains the
		address written back by the external function. nil is not a valid argument value.
		As a return type answers a pointer instance of LPVOID (i.e. LPVOID* = void**).
"

!

gciNbExecuteStrFromContext: string _: context _: symbolList

	self subclassResponsibility!

gciNbPerform: receiver _: selector _: args _: numArgs

	self subclassResponsibility!

gciNbPerformNoDebug: receiver _: selector _: args _: numArgs

	self subclassResponsibility!

gciNewString: string

	self subclassResponsibility!

gciOopToChr: anObject

	self subclassResponsibility!

gciPollForSignal

	<cdecl: bool GciPollForSignal>
	^self invalidCall
!

gciReleaseOops: args _: numArgs

	self subclassResponsibility!

gciSetNet: stoneName _: hostUserID _: hostPassword _: gemService

	<cdecl: void GciSetNet lpstr lpstr lpstr lpstr>
	^self invalidCall

!

gciSetSessionId: anInteger

	<cdecl: void GciSetSessionId sdword >
	^self invalidCall

!

gciShutdown

	<cdecl: void GciShutdown>
	^self invalidCall
!

gciSoftBreak

	<cdecl: void GciSoftBreak>
	^self invalidCall
!

gciVersion

	<cdecl: lpstr GciVersion>
	^self invalidCall
!

gemTrace: anInteger

	^self gciGemTrace: anInteger.
!

hardBreakSession: anInteger

	| priorSession |
	priorSession := self gciGetSessionId.
	[
		self gciSetSessionId: anInteger.
		self gciHardBreak.
	] ensure: [
		self gciSetSessionId: priorSession.
	].
!

initialize

	super initialize.
	semaphore := Semaphore forMutualExclusion.
	self class sessionStarted.
	self gciInit ifFalse: [self error: 'GciInit() failed!!'].
!

lastError

	| errorReport result |
	errorReport := self errorStructureClass new.
	result := self gciErr: errorReport.
	^result
		ifTrue: [errorReport]
		ifFalse: [nil].
!

loginAs: userName password: password

	self critical: [
		| success error |
		success := self
			gciLogin: userName
			_: password.
		success ifTrue: [^self gciGetSessionId].
		error := self lastError.
		GciLoginFailed 
			signal: error message
			with: error.
	].

!

loginHostUser: hostUser hostPassword: hostPassword gsUser: gsUser gsPassword: gsPassword gemNRS: gemString stoneNRS: stoneString

	^self
		gciSetNet: stoneString _: hostUser _: hostPassword _: gemString;
		loginAs: gsUser password: gsPassword!

logoutSession: anInteger

	self critical: [
		self 
			gciSetSessionId: anInteger;
			gciLogout;
			signalIfError;
			yourself.
	].
!

nbResult

	| address delay |
	address := ExternalAddress new.
	delay := 1.
	[true] whileTrue: [
		| result |
		result := self gciNbEnd: address.
		(result isKindOf: SmallInteger ) ifFalse: [self error: 'Result of GciNbEnd() should be a SmallInteger but is ' , result printString , '!!?'].
		result = 2 ifTrue: [
			result := self oopAt: address.
			self signalIfError.
			^self valueOfOop: result.
		].
		Processor sleep: delay.
		delay := delay + 1 min: 100.
	].
!

pollForSignalSession: anInteger

	self critical: [
		self gciSetSessionId: anInteger.
		self gciPollForSignal ifFalse: [^self].
		self signalLastError.
	].
!

sendInterpreted: aString to: anOopType with: anArray session: anInteger

	^self 
		session: anInteger
		send: aString 
		to: anOopType 
		with: anArray 
!

session: session clearStack: processOop

	self critical: [
		self gciSetSessionId: session.
		self gciClearStack: processOop.
	].
!

session: session continue: gsProcessOop withObject: anOop

	self critical: [
		self gciSetSessionId: session.
		self
			gciNbContinueWith: gsProcessOop
			_: anOop
			_: 1 "GCI_PERFORM_FLAG_ENABLE_DEBUG"
			_: nil.
		^self nbResult.
	].
!

session: session execute: aString context: contextOop

	self critical: [
		self gciSetSessionId: session.
		self
			gciNbExecuteStrFromContext: aString
			_: contextOop
			_: self oopNil.
		^self nbResult.
	].
!

session: anInteger fetchBytes: anOopType 

	self critical: [
		self gciSetSessionId: anInteger.
		^self fetchBytes: anOopType.
	].
!

session: anInteger fetchObjects: anOopType 

	anOopType isSpecial ifTrue: [^self specialFromOop: anOopType].
	self critical: [
		| type size array result |
		self gciSetSessionId: anInteger.
		(type := self fetchObjImpl: anOopType) = 0 ifFalse: [self error: 'Expected 0 but got ' , type printString].
		size := self gciFetchSize: anOopType.
		array := self oopTypeArrayClass new: size.
		result := self 
			gciFetchVaryingOops: anOopType
			_: 1
			_: array
			_: size.
		result = size ifTrue: [^array].
		self signalLastError.
	].
!

session: session oopForInteger: anInteger

	self critical: [
		self gciSetSessionId: session.
		^self oopForInteger: anInteger.
	].
!

session: session oopForString: aString

	self critical: [
		self gciSetSessionId: session.
		^self gciNewString: aString.
	].
!

session: anInteger releaseOops: anArray

	self critical: [
		| array |
		self gciSetSessionId: anInteger.
		array := self oopTypeArrayClass new: anArray size.
		1 to: anArray size do: [:i | 
			array at: i put: (anArray at: i).
		].
		self gciReleaseOops: array _: array size.
	].
!

session: anInteger send: aString to: anOopType with: anArray 

	self critical: [
		self gciSetSessionId: anInteger.
		self
			gciNbPerform: anOopType 
			_: aString 
			_: anArray 
			_: anArray size.
		^self nbResult.
	].
!

signalIfError

	| error |
	(error := self lastError) notNil ifTrue: [GciError signalWith: error].
!

signalLastError

	GciError signalWith: self lastError.
!

signalSemaphoreIfNeeded: anArray
	"If the semaphore was acquired, then anArray will have a non-nil value"

	(anArray basicAt: 1) == nil ifTrue: [^self].
	anArray basicAt: 1 put: nil.
	semaphore signal.
!

softBreakSession: anInteger

	| priorSession |
	priorSession := self gciGetSessionId.
	[
		self gciSetSessionId: anInteger.
		self gciSoftBreak.
	] ensure: [
		self gciSetSessionId: priorSession.
	].
!

specialFromOop: anOopType

	self subclassResponsibility!

valueOfOop: anOopType

	| type |
	type := self fetchObjImpl: anOopType.
	type = 1 ifTrue: [^self fetchBytes: anOopType].
	type = 3 ifTrue: [^self specialFromOop: anOopType].
	^anOopType.
!

version

	^self gciVersion.
! !
!GciSingleThreadedLibrary categoriesFor: #_semaphore!private! !
!GciSingleThreadedLibrary categoriesFor: #abortSession:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #beginSession:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #close!public! !
!GciSingleThreadedLibrary categoriesFor: #commitSession:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #critical:!private! !
!GciSingleThreadedLibrary categoriesFor: #fetchBytes:!not subclassResponsibility!private! !
!GciSingleThreadedLibrary categoriesFor: #fetchBytes:class:!private! !
!GciSingleThreadedLibrary categoriesFor: #fetchChars:!private! !
!GciSingleThreadedLibrary categoriesFor: #fetchClass:!private! !
!GciSingleThreadedLibrary categoriesFor: #fetchObjImpl:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciAbort!private! !
!GciSingleThreadedLibrary categoriesFor: #gciBegin!private! !
!GciSingleThreadedLibrary categoriesFor: #gciCallInProgress!private! !
!GciSingleThreadedLibrary categoriesFor: #gciClearStack:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciCommit!private! !
!GciSingleThreadedLibrary categoriesFor: #gciErr:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciFetchBytes:_:_:_:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciFetchChars:_:_:_:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciFetchClass:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciFetchObjImpl:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciFetchSize:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciFetchVaryingOops:_:_:_:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciGemTrace:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciGetSessionId!private! !
!GciSingleThreadedLibrary categoriesFor: #gciHardBreak!private! !
!GciSingleThreadedLibrary categoriesFor: #gciInit!private! !
!GciSingleThreadedLibrary categoriesFor: #gciLogin:_:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciLogout!private! !
!GciSingleThreadedLibrary categoriesFor: #gciNbContinueWith:_:_:_:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciNbEnd:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciNbExecuteStrFromContext:_:_:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciNbPerform:_:_:_:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciNbPerformNoDebug:_:_:_:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciNewString:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciOopToChr:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciPollForSignal!private! !
!GciSingleThreadedLibrary categoriesFor: #gciReleaseOops:_:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciSetNet:_:_:_:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciSetSessionId:!private! !
!GciSingleThreadedLibrary categoriesFor: #gciShutdown!private! !
!GciSingleThreadedLibrary categoriesFor: #gciSoftBreak!private! !
!GciSingleThreadedLibrary categoriesFor: #gciVersion!private! !
!GciSingleThreadedLibrary categoriesFor: #gemTrace:!public!to investigate! !
!GciSingleThreadedLibrary categoriesFor: #hardBreakSession:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #initialize!private! !
!GciSingleThreadedLibrary categoriesFor: #lastError!private! !
!GciSingleThreadedLibrary categoriesFor: #loginAs:password:!public!to investigate! !
!GciSingleThreadedLibrary categoriesFor: #loginHostUser:hostPassword:gsUser:gsPassword:gemNRS:stoneNRS:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #logoutSession:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #nbResult!public!to investigate! !
!GciSingleThreadedLibrary categoriesFor: #pollForSignalSession:!public! !
!GciSingleThreadedLibrary categoriesFor: #sendInterpreted:to:with:session:!public! !
!GciSingleThreadedLibrary categoriesFor: #session:clearStack:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:continue:withObject:!public! !
!GciSingleThreadedLibrary categoriesFor: #session:execute:context:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:fetchBytes:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:fetchObjects:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:oopForInteger:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:oopForString:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:releaseOops:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:send:to:with:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #signalIfError!private! !
!GciSingleThreadedLibrary categoriesFor: #signalLastError!private! !
!GciSingleThreadedLibrary categoriesFor: #signalSemaphoreIfNeeded:!private! !
!GciSingleThreadedLibrary categoriesFor: #softBreakSession:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #specialFromOop:!private! !
!GciSingleThreadedLibrary categoriesFor: #valueOfOop:!private! !
!GciSingleThreadedLibrary categoriesFor: #version!public! !

LibGciRpc64_3_3 guid: (GUID fromString: '{68DB187D-A739-41BE-AD58-A476218B1B04}')!
LibGciRpc64_3_3 comment: ''!
!LibGciRpc64_3_3 categoriesForClass!Unclassified! !
!LibGciRpc64_3_3 methodsFor!

errorStructureClass

	^GciErrSType64_31.
!

threadSafeLibraryName

	^'libgcits-3.3.0-32.dll'
! !
!LibGciRpc64_3_3 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64_3_3 categoriesFor: #threadSafeLibraryName!private! !

!LibGciRpc64_3_3 class methodsFor!

displayName

	^'64-bit 3.3'.
!

fileNameSearch

	^'libgcimt-3.3.0-32.dll'.
! !
!LibGciRpc64_3_3 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_3 class categoriesFor: #fileNameSearch!public! !

Gcilw6x guid: (GUID fromString: '{903182EE-3A32-4868-8468-77731D1B2FD5}')!
Gcilw6x comment: ''!
!Gcilw6x categoriesForClass!Unclassified! !
!Gcilw6x methodsFor!

errorStructureClass

	^GciErrSType32.
!

gciClearStack: anOopType

	<cdecl: void GciClearStack OopType32>
	^self invalidCall
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType32*>
	^self invalidCall

!

gciFetchBytes: anOopType _: startIndex _: cString _: maxSize

	<cdecl: sdword GciFetchBytes OopType32 sdword lpvoid dword>
	^self invalidCall
!

gciFetchChars: anOopType _: startIndex _: cString _: maxSize

	<cdecl: sdword GciFetchChars OopType32 sdword lpstr dword>
	^self invalidCall
!

gciFetchClass: oop

	<cdecl: OopType32 GciFetchClass OopType32>
	^self invalidCall
!

gciFetchObjImpl: anObject

	<cdecl: sdword GciFetchObjImpl OopType32>
	^self invalidCall
!

gciFetchSize: anObject

	<cdecl: sdword GciFetchSize OopType32>
	^self invalidCall
!

gciFetchVaryingOops: anObject _: startIndex _: theOops _: numOops

	<cdecl: sdword GciFetchVaryingOops OopType32 sdword OopType32Array* sdword>
	^self invalidCall
!

gciLongToOop: anInteger

	<cdecl: OopType32 GciLongToOop sdword>
	^self invalidCall
!

gciNbContinueWith: process _: replaceTopOfStack _: flags _: error

	<cdecl: void GciNbContinueWith OopType32 OopType32 sdword GciErrSType32*>
	^self invalidCall
!

gciNbExecuteStrFromContext: string _: context _: symbolList

	<cdecl: OopType32 GciNbExecuteStrFromContext lpstr OopType32 OopType32>
	^self invalidCall
!

gciNbPerform: receiver _: selector _: args _: numArgs

	<cdecl: void GciNbPerform OopType32 lpstr OopType32* dword>
	^self invalidCall
!

gciNewString: aString

	<cdecl: OopType32 GciNewString lpstr >
	^self invalidCall
!

gciOopToChr: anObject

	<cdecl: sdword GciOopToChr OopType32>
	^self invalidCall
!

gciReleaseOops: args _: numArgs

	<cdecl: void GciReleaseOops OopType32* dword>
	^self invalidCall
!

oopAsciiNul

	^OopType32 fromInteger: 14.!

oopAt: anExternalAddress

	^OopType32 fromInteger: (anExternalAddress sdwordAtOffset: 0).
!

oopClassArray

	^OopType32 fromInteger: 1045.
!

oopClassByteArray

	^OopType32 fromInteger: 1617.!

oopClassDoubleByteString

	^OopType32 fromInteger: 0.!

oopClassQuadByteString

	^OopType32 fromInteger: 0.!

oopClassString

	^OopType32 fromInteger: 1169.!

oopClassSymbol

	^OopType32 fromInteger: 1733.
!

oopClassSystem

	^OopType32 fromInteger: 1189.!

oopClassUnicode16

	^OopType32 fromInteger: 0.!

oopClassUnicode32

	^OopType32 fromInteger: 0.!

oopClassUnicode7

	^OopType32 fromInteger: 0.!

oopFalse

	^OopType32 fromInteger: 6.!

oopForInteger: anInteger

	| int bytes |
	bytes := ByteArray new: 4.
	bytes 
		dwordAtOffset: 0 
		put: anInteger.
	int := bytes sdwordAtOffset: 0.
	^self gciLongToOop: int.
!

oopGemStoneError

	^OopType32 fromInteger: 3613.!

oopIllegal

	^OopType32 fromInteger: 1.!

oopMaxSmallInteger

	^OopType32 fromInteger: 16r7FFFFFFF.!

oopMinSmallInteger

	^OopType32 fromInteger: -16r7FFFFFFF.!

oopMinusOne

	^OopType32 fromInteger: -1.!

oopNil

	^OopType32 fromInteger: 10.	"0xA"!

oopOne

	^OopType32 fromInteger: 7.!

oopRemoteNil

	^OopType32 fromInteger: 42.!

oopTrue

	^OopType32 fromInteger: 38.!

oopTwo

	^OopType32 fromInteger: 11.!

oopTypeArrayClass

	^OopType32Array.!

oopTypeClass

	^OopType32.!

oopTypeWithOop: anInteger

	| int bytes |
	bytes := ByteArray new: 4.
	bytes 
		dwordAtOffset: 0 
		put: anInteger.
	int := bytes sdwordAtOffset: 0.
	^OopType32 fromInteger: int.
!

oopZero

	^OopType32 fromInteger: 3.!

specialFromOop: anOop

	anOop is6xBoolean ifTrue: [
		^anOop = self oopTrue.
	].
	anOop is6xCharacter ifTrue: [
		^Character value: (self gciOopToChr: anOop).
	].
	anOop is6xSmallInteger ifTrue: [
		^anOop value // 4.
	].
	^nil.
! !
!Gcilw6x categoriesFor: #errorStructureClass!private! !
!Gcilw6x categoriesFor: #gciClearStack:!private! !
!Gcilw6x categoriesFor: #gciErr:!private! !
!Gcilw6x categoriesFor: #gciFetchBytes:_:_:_:!private! !
!Gcilw6x categoriesFor: #gciFetchChars:_:_:_:!private! !
!Gcilw6x categoriesFor: #gciFetchClass:!private! !
!Gcilw6x categoriesFor: #gciFetchObjImpl:!private! !
!Gcilw6x categoriesFor: #gciFetchSize:!private! !
!Gcilw6x categoriesFor: #gciFetchVaryingOops:_:_:_:!private! !
!Gcilw6x categoriesFor: #gciLongToOop:!private! !
!Gcilw6x categoriesFor: #gciNbContinueWith:_:_:_:!private! !
!Gcilw6x categoriesFor: #gciNbExecuteStrFromContext:_:_:!private! !
!Gcilw6x categoriesFor: #gciNbPerform:_:_:_:!private! !
!Gcilw6x categoriesFor: #gciNewString:!private! !
!Gcilw6x categoriesFor: #gciOopToChr:!private! !
!Gcilw6x categoriesFor: #gciReleaseOops:_:!private! !
!Gcilw6x categoriesFor: #oopAsciiNul!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopAt:!public! !
!Gcilw6x categoriesFor: #oopClassArray!public! !
!Gcilw6x categoriesFor: #oopClassByteArray!public! !
!Gcilw6x categoriesFor: #oopClassDoubleByteString!public! !
!Gcilw6x categoriesFor: #oopClassQuadByteString!public! !
!Gcilw6x categoriesFor: #oopClassString!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopClassSymbol!public! !
!Gcilw6x categoriesFor: #oopClassSystem!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopClassUnicode16!public! !
!Gcilw6x categoriesFor: #oopClassUnicode32!public! !
!Gcilw6x categoriesFor: #oopClassUnicode7!public! !
!Gcilw6x categoriesFor: #oopFalse!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopForInteger:!public! !
!Gcilw6x categoriesFor: #oopGemStoneError!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopIllegal!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopMaxSmallInteger!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopMinSmallInteger!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopMinusOne!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopNil!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopOne!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopRemoteNil!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopTrue!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopTwo!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #oopTypeArrayClass!public! !
!Gcilw6x categoriesFor: #oopTypeClass!public! !
!Gcilw6x categoriesFor: #oopTypeWithOop:!public! !
!Gcilw6x categoriesFor: #oopZero!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #specialFromOop:!public! !

LibGciRpc64 guid: (GUID fromString: '{CE7FE05A-C8EE-4A13-B4AA-7C8AEF295620}')!
LibGciRpc64 comment: ''!
!LibGciRpc64 categoriesForClass!Unclassified! !
!LibGciRpc64 methodsFor!

errorStructureClass

	^GciErrSType64.
!

gciClearStack: processOop

	<cdecl: void GciClearStack OopType64>
	^self invalidCall
!

gciDbgEstablishToFile: aString
	"BoolType GciDbgEstablishToFile( const char * fileName );"

	<cdecl: bool GciDbgEstablishToFile lpstr>
	^self invalidCall
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType64*>
	^self invalidCall
!

gciFetchBytes: anOopType _: startIndex _: cString _: maxSize

	<cdecl: sdword GciFetchBytes_ OopType64 sqword lpvoid qword>
	^self invalidCall
!

gciFetchChars: anOopType _: startIndex _: cString _: maxSize

	<cdecl: sdword GciFetchChars_ OopType64 sqword lpstr qword>
	^self invalidCall
!

gciFetchClass: oop

	<cdecl: OopType64 GciFetchClass OopType64>
	^self invalidCall
!

gciFetchObjImpl: anObject

	<cdecl: sdword GciFetchObjImpl OopType64>
	^self invalidCall
!

gciFetchSize: anObject

	<cdecl: sdword GciFetchSize_ OopType64>
	^self invalidCall
!

gciFetchVaryingOops: anObject _: startIndex _: theOops _: numOops

	<cdecl: sdword GciFetchVaryingOops OopType64 sqword OopType64Array* sdword>
	^self invalidCall
!

gciFltToOop: aFloat

	<cdecl: OopType64 GciFltToOop double>
	^self invalidCall
!

gciI64ToOop: anInteger

	<cdecl: OopType64 GciI64ToOop sdword>
	^self invalidCall
!

gciNbContinueWith: process _: replaceTopOfStack _: flags _: error

	<cdecl: void GciNbContinueWith OopType64 OopType64 sdword GciErrSType64*>
	^self invalidCall
!

gciNbExecuteStrFromContext: string _: context _: symbolList

	<cdecl: OopType64 GciNbExecuteStrFromContext lpstr OopType64 OopType64>
	^self invalidCall
!

gciNbPerform: receiver _: selector _: args _: numArgs

	<cdecl: void GciNbPerform OopType64 lpstr OopType64* dword>
	^self invalidCall
!

gciNbPerformNoDebug: receiver _: selector _: args _: numArgs

	self error: 'Function takes five arguments in 64-bit'.

!

gciNbPerformNoDebug: receiver _: selector _: args _: numArgs _: flags
"	/* bits in flags argument to GciPerformNoDebug.  */
enum { 
  GCI_PERFORM_FLAG_ENABLE_DEBUG = 1,  
   /* make like GciPerform() with respect to debugging. default is debugger is
    * disabled until GciPerformNoDebug returns */

  GCI_PERFORM_FLAG_DISABLE_ASYNC_EVENTS = 2  ,
   /* disable async events.  Used for GSI to be able to execute an exception
    * block or method to service RT_ERR_SIGNAL_ABORT, 
    *  RT_ERR_SIGNAL_GEMSTONE_SESSION, or ABORT_ERR_LOST_OT_ROOT */

  GCI_PERFORM_FLAG_SINGLE_STEP = 4, 
   /* place a single step break-point at the start of the method to be
    * performed, and then execute to hit that breakpoint.
    * if true, also implies GCI_PERFORM_FLAG_INTERPRETED
    */

  GCI_PERFORM_MAP_SYMBOLS = 8, // obsolete, not used

  GCI_PERFORM_FLAG_INTERPRETED = 0x20, /* disable native code for the execution*/

  GCI_PERFORM_RUBY = 0x40  /* for use by GciExecuteFromContextDbg*
                            *   for implementing Ruby doit in ruby topaz */
 };"

	<cdecl: void GciNbPerformNoDebug OopType64 lpstr OopType64* dword dword>
	^self invalidCall
!

gciNewString: string

	<cdecl: OopType64 GciNewString lpstr >
	^self invalidCall
!

gciOopToChr: anObject

	<cdecl: sdword GciOopToChr OopType64>
	^self invalidCall
!

gciReleaseOops: args _: numArgs

	<cdecl: void GciReleaseOops OopType64* dword>
	^self invalidCall
!

oopAsciiNul

	^OopType64 fromInteger: 28. "16r1C"
!

oopAt: anExternalAddress

	^OopType64 fromInteger: (anExternalAddress sqwordAtOffset: 0).
!

oopClassArray

	^OopType64 fromInteger: 66817.
!

oopClassByteArray

	^OopType64 fromInteger: 103425.!

oopClassDoubleByteString

	^OopType64 fromInteger: 143873.
!

oopClassQuadByteString

	^OopType64 fromInteger: 144385.
!

oopClassString

	^OopType64 fromInteger: 74753.
!

oopClassSymbol

	^OopType64 fromInteger: 110849.
!

oopClassSystem

	^OopType64 fromInteger: 76033.
!

oopClassUnicode16

	^OopType64 fromInteger: 154625.
!

oopClassUnicode32

	^OopType64 fromInteger: 154881.
!

oopClassUnicode7

	^OopType64 fromInteger: 154369.
!

oopFalse

	^OopType64 fromInteger: 12. "16r0C"
!

oopForInteger: anInteger

	| int bytes |
	bytes := ByteArray new: 8.
	bytes 
		qwordAtOffset: 0 
		put: anInteger.
	int := bytes sdwordAtOffset: 0.
	^self gciI64ToOop: int.
!

oopGemStoneError

	^OopType64 fromInteger:  231169.!

oopIllegal

	^OopType64 fromInteger: 1. "16r01"
!

oopMaxSmallInteger

	^OopType64 fromInteger: 9223372036854775802.	"'16r7FFFFFFFFFFFFFFA'"
!

oopMinSmallInteger

	^OopType64 fromInteger: -9223372036854775806.	"'-16r7FFFFFFFFFFFFFFE'"
!

oopMinusOne

	^OopType64 fromInteger: -6.
!

oopNil

	^OopType64 fromInteger: 20. "16r14"
!

oopOne

	^OopType64 fromInteger: 10.
!

oopRemoteNil

	^OopType64 fromInteger: 276. "16r114"
!

oopTrue

	^OopType64 fromInteger: 268.	"16r10C"
!

oopTwo

	^OopType64 fromInteger: 18.
!

oopTypeArrayClass

	^OopType64Array.!

oopTypeClass

	^OopType64.
!

oopTypeWithOop: anInteger

	| int bytes |
	bytes := ByteArray new: 8.
	bytes 
		qwordAtOffset: 0 
		put: anInteger.
	int := bytes sqwordAtOffset: 0.
	^OopType64 fromInteger: int.
!

oopZero

	^OopType64 fromInteger: 2.
!

sendInterpreted: aString to: anOopType with: anArray session: anInteger

	self critical: [
		self gciSetSessionId: anInteger.
		self
			gciNbPerformNoDebug: anOopType 
			_: aString 
			_: anArray 
			_: anArray size
			_: 16r21.		"GCI_PERFORM_FLAG_ENABLE_DEBUG + GCI_PERFORM_FLAG_INTERPRETED"
		^self nbResult.
	].
!

specialFromOop: anOop

	anOop isBoolean ifTrue: [
		^anOop = self oopTrue.
	].
	anOop isCharacter ifTrue: [
		^Character value: (self gciOopToChr: anOop).
	].
	anOop isSmallInteger ifTrue: [
		^anOop asSmallInteger.
	].
	anOop isSmallDouble ifTrue: [
		^anOop smallDoubleAsFloat.
	].
	^nil.
! !
!LibGciRpc64 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64 categoriesFor: #gciClearStack:!private! !
!LibGciRpc64 categoriesFor: #gciDbgEstablishToFile:!private! !
!LibGciRpc64 categoriesFor: #gciErr:!private! !
!LibGciRpc64 categoriesFor: #gciFetchBytes:_:_:_:!private! !
!LibGciRpc64 categoriesFor: #gciFetchChars:_:_:_:!private! !
!LibGciRpc64 categoriesFor: #gciFetchClass:!private! !
!LibGciRpc64 categoriesFor: #gciFetchObjImpl:!private! !
!LibGciRpc64 categoriesFor: #gciFetchSize:!private! !
!LibGciRpc64 categoriesFor: #gciFetchVaryingOops:_:_:_:!private! !
!LibGciRpc64 categoriesFor: #gciFltToOop:!private! !
!LibGciRpc64 categoriesFor: #gciI64ToOop:!private! !
!LibGciRpc64 categoriesFor: #gciNbContinueWith:_:_:_:!private! !
!LibGciRpc64 categoriesFor: #gciNbExecuteStrFromContext:_:_:!private! !
!LibGciRpc64 categoriesFor: #gciNbPerform:_:_:_:!private! !
!LibGciRpc64 categoriesFor: #gciNbPerformNoDebug:_:_:_:!private! !
!LibGciRpc64 categoriesFor: #gciNbPerformNoDebug:_:_:_:_:!private! !
!LibGciRpc64 categoriesFor: #gciNewString:!private! !
!LibGciRpc64 categoriesFor: #gciOopToChr:!private! !
!LibGciRpc64 categoriesFor: #gciReleaseOops:_:!private! !
!LibGciRpc64 categoriesFor: #oopAsciiNul!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopAt:!private! !
!LibGciRpc64 categoriesFor: #oopClassArray!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopClassByteArray!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopClassDoubleByteString!public! !
!LibGciRpc64 categoriesFor: #oopClassQuadByteString!public! !
!LibGciRpc64 categoriesFor: #oopClassString!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopClassSymbol!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopClassSystem!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopClassUnicode16!public! !
!LibGciRpc64 categoriesFor: #oopClassUnicode32!public! !
!LibGciRpc64 categoriesFor: #oopClassUnicode7!public! !
!LibGciRpc64 categoriesFor: #oopFalse!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopForInteger:!public! !
!LibGciRpc64 categoriesFor: #oopGemStoneError!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopIllegal!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopMaxSmallInteger!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopMinSmallInteger!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopMinusOne!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopNil!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopOne!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopRemoteNil!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopTrue!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopTwo!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #oopTypeArrayClass!public! !
!LibGciRpc64 categoriesFor: #oopTypeClass!public! !
!LibGciRpc64 categoriesFor: #oopTypeWithOop:!public! !
!LibGciRpc64 categoriesFor: #oopZero!public!Reserved OOPs! !
!LibGciRpc64 categoriesFor: #sendInterpreted:to:with:session:!public! !
!LibGciRpc64 categoriesFor: #specialFromOop:!public! !

Gcilw61 guid: (GUID fromString: '{496FFCA3-1D56-463F-8FDE-4838ABC55E9D}')!
Gcilw61 comment: ''!
!Gcilw61 categoriesForClass!Unclassified! !
!Gcilw61 class methodsFor!

displayName

	^'32-bit 6.1.x'.
! !
!Gcilw61 class categoriesFor: #displayName!public! !

Gcilw63 guid: (GUID fromString: '{0B266543-A4AF-485C-8572-C3ADD8C9DA70}')!
Gcilw63 comment: ''!
!Gcilw63 categoriesForClass!Unclassified! !
!Gcilw63 class methodsFor!

displayName

	^'32-bit 6.3.x'.
! !
!Gcilw63 class categoriesFor: #displayName!public! !

Gcilw65 guid: (GUID fromString: '{4BFE3A86-E099-4D8E-9FD8-CF0FD761D8AD}')!
Gcilw65 comment: ''!
!Gcilw65 categoriesForClass!Unclassified! !
!Gcilw65 class methodsFor!

displayName

	^'32-bit 6.5.x'.
! !
!Gcilw65 class categoriesFor: #displayName!public! !

Gcilw66 guid: (GUID fromString: '{F878E629-FEAF-4205-A72A-C5140524DA62}')!
Gcilw66 comment: ''!
!Gcilw66 categoriesForClass!Unclassified! !
!Gcilw66 class methodsFor!

displayName

	^'32-bit 6.6.x'.
! !
!Gcilw66 class categoriesFor: #displayName!public! !

Gcirw62 guid: (GUID fromString: '{C2E42069-D534-4F0C-ACB8-1C11EC0EB3F7}')!
Gcirw62 comment: ''!
!Gcirw62 categoriesForClass!Unclassified! !
!Gcirw62 class methodsFor!

displayName

	^'32-bit 6.2.x'.
! !
!Gcirw62 class categoriesFor: #displayName!public! !

LibGciRpc64_20 guid: (GUID fromString: '{092F7B3C-B2E2-4238-8ACC-F7A381FE596D}')!
LibGciRpc64_20 comment: ''!
!LibGciRpc64_20 categoriesForClass!Unclassified! !
!LibGciRpc64_20 class methodsFor!

displayName

	^'64-bit 2.0.x'.
! !
!LibGciRpc64_20 class categoriesFor: #displayName!public! !

LibGciRpc64_21 guid: (GUID fromString: '{95100075-9A56-4116-AD62-415290B2BA64}')!
LibGciRpc64_21 comment: ''!
!LibGciRpc64_21 categoriesForClass!Unclassified! !
!LibGciRpc64_21 class methodsFor!

displayName

	^'64-bit 2.1.x'.
! !
!LibGciRpc64_21 class categoriesFor: #displayName!public! !

LibGciRpc64_22 guid: (GUID fromString: '{5D1A9DC4-2B15-4DA9-93B0-851D62109E88}')!
LibGciRpc64_22 comment: ''!
!LibGciRpc64_22 categoriesForClass!Unclassified! !
!LibGciRpc64_22 class methodsFor!

displayName

	^'64-bit 2.2.x'.
! !
!LibGciRpc64_22 class categoriesFor: #displayName!public! !

LibGciRpc64_23 guid: (GUID fromString: '{D51C9D71-CC8F-4BC9-B4B5-946F772BFC1B}')!
LibGciRpc64_23 comment: ''!
!LibGciRpc64_23 categoriesForClass!Unclassified! !
!LibGciRpc64_23 class methodsFor!

displayName

	^'64-bit 2.3.x'.
! !
!LibGciRpc64_23 class categoriesFor: #displayName!public! !

LibGciRpc64_24 guid: (GUID fromString: '{4DF6B51F-E61C-4046-9413-2DB70136A4C1}')!
LibGciRpc64_24 comment: ''!
!LibGciRpc64_24 categoriesForClass!Unclassified! !
!LibGciRpc64_24 class methodsFor!

displayName

	^'64-bit 2.4.x'.
! !
!LibGciRpc64_24 class categoriesFor: #displayName!public! !

LibGciRpc64_3_0 guid: (GUID fromString: '{A8A6C6D6-29C1-4E0D-9C00-EC47F1078D4D}')!
LibGciRpc64_3_0 comment: ''!
!LibGciRpc64_3_0 categoriesForClass!Unclassified! !
!LibGciRpc64_3_0 methodsFor!

errorStructureClass

	^GciErrSType64_30.
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType64_30*>
	^self invalidCall
! !
!LibGciRpc64_3_0 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64_3_0 categoriesFor: #gciErr:!private! !

!LibGciRpc64_3_0 class methodsFor!

displayName

	^'64-bit 3.0.x'.
! !
!LibGciRpc64_3_0 class categoriesFor: #displayName!public! !

LibGciRpc64_3_1 guid: (GUID fromString: '{B9F761F8-7AA2-47AB-804A-8A2C76AFDB1D}')!
LibGciRpc64_3_1 comment: ''!
!LibGciRpc64_3_1 categoriesForClass!Unclassified! !
!LibGciRpc64_3_1 methodsFor!

errorStructureClass

	^GciErrSType64_31.
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType64_31*>
	^self invalidCall
! !
!LibGciRpc64_3_1 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64_3_1 categoriesFor: #gciErr:!private! !

!LibGciRpc64_3_1 class methodsFor!

displayName

	^'64-bit 3.1.0'.
!

fileNameSearch

	^'libgcirpc-3.1.0-32.dll'.
! !
!LibGciRpc64_3_1 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_1 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_3_2 guid: (GUID fromString: '{25CBF893-B55F-4C84-B927-F0E27DAF8114}')!
LibGciRpc64_3_2 comment: ''!
!LibGciRpc64_3_2 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2 methodsFor!

errorStructureClass

	^GciErrSType64_31.
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType64_31*>
	^self invalidCall
! !
!LibGciRpc64_3_2 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64_3_2 categoriesFor: #gciErr:!private! !

!LibGciRpc64_3_2 class methodsFor!

displayName

	^'64-bit 3.2'.
!

fileNameSearch

	^'libgcits-3.2.0-32.dll'.
! !
!LibGciRpc64_3_2 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_310x guid: (GUID fromString: '{1A433237-482B-47C8-89DB-541890BB91B8}')!
LibGciRpc64_310x comment: ''!
!LibGciRpc64_310x categoriesForClass!Unclassified! !
!LibGciRpc64_310x class methodsFor!

displayName

	^'64-bit 3.1.0.x'.
!

fileNameSearch

	^'libgcirpc-3.1.0.*-32.dll'.
! !
!LibGciRpc64_310x class categoriesFor: #displayName!public! !
!LibGciRpc64_310x class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_3_2_01 guid: (GUID fromString: '{9B3A8D44-3E44-49AC-90C5-AFDA22E1CD1B}')!
LibGciRpc64_3_2_01 comment: ''!
!LibGciRpc64_3_2_01 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2_01 class methodsFor!

displayName

	^'64-bit 3.2.1'.
!

fileNameSearch

	^'libgcirpc-3.2.1-32.dll'.
! !
!LibGciRpc64_3_2_01 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2_01 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_3_2_02 guid: (GUID fromString: '{BD5DC8E2-7DCC-48E3-B0B8-9F73D810E6E6}')!
LibGciRpc64_3_2_02 comment: ''!
!LibGciRpc64_3_2_02 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2_02 class methodsFor!

displayName

	^'64-bit 3.2.2'.
!

fileNameSearch

	^'libgcirpc-3.2.2-32.dll'.
! !
!LibGciRpc64_3_2_02 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2_02 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_3_2_03 guid: (GUID fromString: '{7CD3D48A-C954-4465-8DBC-90511A44E6E3}')!
LibGciRpc64_3_2_03 comment: ''!
!LibGciRpc64_3_2_03 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2_03 class methodsFor!

displayName

	^'64-bit 3.2.3'.
!

fileNameSearch

	^'libgcirpc-3.2.3-32.dll'.
! !
!LibGciRpc64_3_2_03 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2_03 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_3_2_04 guid: (GUID fromString: '{AE6862DF-767F-45A7-8CF0-2520FDD79C28}')!
LibGciRpc64_3_2_04 comment: ''!
!LibGciRpc64_3_2_04 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2_04 class methodsFor!

displayName

	^'64-bit 3.2.4'.
!

fileNameSearch

	^'libgcirpc-3.2.4-32.dll'.
! !
!LibGciRpc64_3_2_04 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2_04 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_3_2_05 guid: (GUID fromString: '{24C3DFCA-1974-4647-BE98-B8A0AA33801A}')!
LibGciRpc64_3_2_05 comment: ''!
!LibGciRpc64_3_2_05 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2_05 class methodsFor!

displayName

	^'64-bit 3.2.5'.
!

fileNameSearch

	^'libgcirpc-3.2.5-32.dll'.
! !
!LibGciRpc64_3_2_05 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2_05 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_3_2_06 guid: (GUID fromString: '{81EB5050-FDDC-4907-9B98-69F01284C900}')!
LibGciRpc64_3_2_06 comment: ''!
!LibGciRpc64_3_2_06 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2_06 class methodsFor!

displayName

	^'64-bit 3.2.6'.
!

fileNameSearch

	^'libgcirpc-3.2.6-32.dll'.
! !
!LibGciRpc64_3_2_06 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2_06 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_3_2_07 guid: (GUID fromString: '{ED635505-59E8-42E5-87A5-47C0E655E5FC}')!
LibGciRpc64_3_2_07 comment: ''!
!LibGciRpc64_3_2_07 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2_07 class methodsFor!

displayName

	^'64-bit 3.2.7'.
!

fileNameSearch

	^'libgcirpc-3.2.7-32.dll'.
! !
!LibGciRpc64_3_2_07 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2_07 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_3_2_08 guid: (GUID fromString: '{E31F62FC-C1EE-40C6-90D4-A59DDA3C831B}')!
LibGciRpc64_3_2_08 comment: ''!
!LibGciRpc64_3_2_08 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2_08 class methodsFor!

displayName

	^'64-bit 3.2.8'.
!

fileNameSearch

	^'libgcirpc-3.2.8-32.dll'.
! !
!LibGciRpc64_3_2_08 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2_08 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_3_2_09 guid: (GUID fromString: '{12081231-C77F-4BC8-8F36-DD11BDBB44CD}')!
LibGciRpc64_3_2_09 comment: ''!
!LibGciRpc64_3_2_09 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2_09 class methodsFor!

displayName

	^'64-bit 3.2.9'.
!

fileNameSearch

	^'libgcirpc-3.2.9-32.dll'.
! !
!LibGciRpc64_3_2_09 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2_09 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_3_2_10 guid: (GUID fromString: '{4EF14B8F-3BF8-4484-9A0C-4B2468F46491}')!
LibGciRpc64_3_2_10 comment: ''!
!LibGciRpc64_3_2_10 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2_10 class methodsFor!

displayName

	^'64-bit 3.2.10'.
!

fileNameSearch

	^'libgcirpc-3.2.10-32.dll'.
! !
!LibGciRpc64_3_2_10 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2_10 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_3_2_11 guid: (GUID fromString: '{B4E236BE-BE04-48DD-AC35-D4CE1BB763C5}')!
LibGciRpc64_3_2_11 comment: ''!
!LibGciRpc64_3_2_11 categoriesForClass!Unclassified! !
!LibGciRpc64_3_2_11 class methodsFor!

displayName

	^'64-bit 3.2.11'.
!

fileNameSearch

	^'libgcirpc-3.2.11-32.dll'.
! !
!LibGciRpc64_3_2_11 class categoriesFor: #displayName!public! !
!LibGciRpc64_3_2_11 class categoriesFor: #fileNameSearch!public! !

GciErrSType guid: (GUID fromString: '{9932DE27-ACC2-4E27-AD64-9A3FF8B6ECF9}')!
GciErrSType comment: ''!
!GciErrSType categoriesForClass!Unclassified! !
!GciErrSType methodsFor!

args: anArray

	| list |
	list := anArray asOrderedCollection.
	[list size < 10] whileTrue: [list add: (self oopTypeClass fromInteger: 1)].
	args := list asArray.
!

isApplicationErrorInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 2318].
!

isClientForwarderSendInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 2336].
!

isCompileErrorInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number > 1000 and: [
		self number < 2000]].
!

isDoesNotUnderstandInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 2010].
!

isEventErrorInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number > 6000 and: [
		self number < 7000]].
!

isGemStoneErrorCategoryInSession: aGciSession

	^self categoryOop = aGciSession oopGemStoneError.
!

isHardBreakInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 6004].
!

isInvalidSessionInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 4100].
!

isOverlappingGCIErrorInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 2203].
!

isPauseInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 6001].
!

isRuntimeErrorInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number > 2000 and: [
		self number < 3000]].
!

isSoftBreakInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 6003].
!

isStackBreakpointInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 6006].
!

number

	self subclassResponsibility.
!

oopTypeClass

	self subclassResponsibility.
!

stack
	^stack.
!

stack: anObject
	stack := anObject.
! !
!GciErrSType categoriesFor: #args:!public! !
!GciErrSType categoriesFor: #isApplicationErrorInSession:!public! !
!GciErrSType categoriesFor: #isClientForwarderSendInSession:!public! !
!GciErrSType categoriesFor: #isCompileErrorInSession:!public! !
!GciErrSType categoriesFor: #isDoesNotUnderstandInSession:!public! !
!GciErrSType categoriesFor: #isEventErrorInSession:!public! !
!GciErrSType categoriesFor: #isGemStoneErrorCategoryInSession:!public! !
!GciErrSType categoriesFor: #isHardBreakInSession:!public! !
!GciErrSType categoriesFor: #isInvalidSessionInSession:!public! !
!GciErrSType categoriesFor: #isOverlappingGCIErrorInSession:!public! !
!GciErrSType categoriesFor: #isPauseInSession:!public! !
!GciErrSType categoriesFor: #isRuntimeErrorInSession:!public! !
!GciErrSType categoriesFor: #isSoftBreakInSession:!public! !
!GciErrSType categoriesFor: #isStackBreakpointInSession:!public! !
!GciErrSType categoriesFor: #number!public! !
!GciErrSType categoriesFor: #oopTypeClass!public! !
!GciErrSType categoriesFor: #stack!public! !
!GciErrSType categoriesFor: #stack:!public! !

GciTsObjInfo guid: (GUID fromString: '{6A0671AC-78FF-43D1-B841-525B61598094}')!
GciTsObjInfo comment: '
class GciTsObjInfo {
	public:
		OopType			objId;
		OopType			objClass;							/* OOP of the class of the obj						*/
		int64					objSize;								/* obj''s total size, in bytes or OOPs					*/
		int					namedSize;						/* num of named inst vars in the obj				*/
		unsigned short	objectSecurityPolicyId;		/* previously named segmentId						*/
		unsigned short	_bits;
		unsigned short	access;								/* 0 no auth, 1 read allowed, 2 write allowed	*/

		enum { 
			AUTH_NONE = 0, AUTH_READ = 1, AUTH_WRITE = 2 
		};
		enum {  // definitions of _bits
			implem_mask		= GC_IMPLEMENTATION_MASK,	// 0x03
			indexable_mask	= GC_INDEXABLE_MASK,				// 0x04
			invariant_mask		= GC_INVARIANT_MASK,				// 0x08
			partial_mask			= 0x10,
			overlay_mask		= 0x20,
			is_placeholder		= 0x40,		// object is place holder for unsatisfied forward reference
			swiz_kind_mask	= 0x300,
			swiz_kind_shift		= 8
		};

		inline unsigned char isInvariant()	{ return _bits & invariant_mask;	}
		inline unsigned char isIndexable()	{ return _bits & indexable_mask;	}
		inline unsigned char isPartial()		{ return _bits & partial_mask;		}
		inline unsigned char isOverlayed()	{ return _bits & overlay_mask;		}

		inline GciByteSwizEType byteSwizKind() const {
			return (GciByteSwizEType)((_bits & swiz_kind_mask) >> swiz_kind_shift) ;
		}

		inline unsigned char objImpl() {
			/* implementation format 0..3 , one of GC_FORMAT_OOP..GC_FORMAT_SPECIAL */
			return _bits & GC_IMPLEMENTATION_MASK;
		}
};'!
!GciTsObjInfo categoriesForClass!Unclassified! !
!GciTsObjInfo methodsFor!

_bits
	"Answer the receiver's _bits field as a Smalltalk object."

	^(bytes swordAtOffset: 30)!

_bits: anObject
	"Set the receiver's _bits field to the value of anObject."

	bytes swordAtOffset: 30 put: anObject!

access
	"Answer the receiver's access field as a Smalltalk object."

	^(bytes swordAtOffset: 32)!

access: anObject
	"Set the receiver's access field to the value of anObject."

	bytes swordAtOffset: 32 put: anObject!

data
	^data!

data: anObject
	data := anObject!

implementation
"
#define GC_FORMAT_OOP			0
#define GC_FORMAT_BYTE			1
#define GC_FORMAT_NSC			2
#define GC_FORMAT_SPECIAL		3
"
	^self _bits bitAnd: 16r03	"implem_mask    = GC_IMPLEMENTATION_MASK, // 0x03"
!

isIndexable

	^0 < (self _bits bitAnd: 16r04)	"indexable_mask = GC_INDEXABLE_MASK,      // 0x04"
!

isInvariant

	^0 < (self _bits bitAnd: 16r08)	"invariant_mask = GC_INVARIANT_MASK,      // 0x08"
!

isReadable

	^0 < self access.
!

isWriteable

	^1 < self access.
!

namedSize
	"Answer the receiver's namedSize field as a Smalltalk object."

	^(bytes sdwordAtOffset: 24)!

namedSize: anObject
	"Set the receiver's namedSize field to the value of anObject."

	bytes sdwordAtOffset: 24 put: anObject!

objClass
	"Answer the receiver's objClass field as a Smalltalk object."

	^OopType64 fromInteger: (bytes qwordAtOffset: 8)!

objClass: anObject
	"Set the receiver's objClass field to the value of anObject."

	bytes qwordAtOffset: 8 put: anObject value!

objectSecurityPolicyId
	"Answer the receiver's objectSecurityPolicyId field as a Smalltalk object."

	^(bytes swordAtOffset: 28)!

objectSecurityPolicyId: anObject
	"Set the receiver's objectSecurityPolicyId field to the value of anObject."

	bytes swordAtOffset: 28 put: anObject!

objId
	"Answer the receiver's objId field as a Smalltalk object."

	^(bytes qwordAtOffset: 0)!

objId: anObject
	"Set the receiver's objId field to the value of anObject."

	bytes qwordAtOffset: 0 put: anObject!

objSize
	"Answer the receiver's objSize field as a Smalltalk object."

	^(bytes sqwordAtOffset: 16)!

objSize: anObject
	"Set the receiver's objSize field to the value of anObject."

	bytes sqwordAtOffset: 16 put: anObject! !
!GciTsObjInfo categoriesFor: #_bits!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #_bits:!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #access!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #access:!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #data!accessing!public! !
!GciTsObjInfo categoriesFor: #data:!accessing!public! !
!GciTsObjInfo categoriesFor: #implementation!public!testing! !
!GciTsObjInfo categoriesFor: #isIndexable!public!testing! !
!GciTsObjInfo categoriesFor: #isInvariant!public!testing! !
!GciTsObjInfo categoriesFor: #isReadable!public!testing! !
!GciTsObjInfo categoriesFor: #isWriteable!public!testing! !
!GciTsObjInfo categoriesFor: #namedSize!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #namedSize:!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #objClass!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #objClass:!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #objectSecurityPolicyId!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #objectSecurityPolicyId:!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #objId!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #objId:!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #objSize!**compiled accessors**!public! !
!GciTsObjInfo categoriesFor: #objSize:!**compiled accessors**!public! !

!GciTsObjInfo class methodsFor!

defineFields
"
	GciTsObjInfo compileDefinition.
"
	self
		defineField: #objId 							type: QWORDField 	new	;
		defineField: #objClass						type: QWORDField 	new	;
		defineField: #objSize							type: SQWORDField	new	;
		defineField: #namedSize					type: SDWORDField	new	;
		defineField: #objectSecurityPolicyId	type: SWORDField	new	;
		defineField: #_bits								type: SWORDField	new	;
		defineField: #access							type: SWORDField	new	;
		yourself.
! !
!GciTsObjInfo class categoriesFor: #defineFields!public! !

OopType32Array guid: (GUID fromString: '{73AA2718-0F19-4B5E-AF7F-A3D2D8FBB1C4}')!
OopType32Array comment: ''!
!OopType32Array categoriesForClass!Unclassified! !
!OopType32Array methodsFor!

elementClass

	^OopType32.!

uncheckedAt: index 

	^OopType32 fromInteger: (bytes dwordAtOffset: (index - 1) * 4).
!

uncheckedAt: index  put: anOopType32

	bytes 
		dwordAtOffset: (index - 1) * 4
		put: anOopType32 value.

! !
!OopType32Array categoriesFor: #elementClass!constants!public! !
!OopType32Array categoriesFor: #uncheckedAt:!accessing!private! !
!OopType32Array categoriesFor: #uncheckedAt:put:!accessing!private! !

!OopType32Array class methodsFor!

elementSize

	^4.
! !
!OopType32Array class categoriesFor: #elementSize!instance creation!private! !

OopType64Array guid: (GUID fromString: '{729F7C08-E907-4B03-A8FA-562774423BBA}')!
OopType64Array comment: ''!
!OopType64Array categoriesForClass!Unclassified! !
!OopType64Array methodsFor!

elementClass

	^OopType64.
!

size: anInteger

	bytes := bytes copyFrom: 1 to: self class elementSize * anInteger.
!

uncheckedAt: index 

	^OopType64 fromInteger: (bytes qwordAtOffset: (index - 1) * 8)!

uncheckedAt: index  put: anOopType64

	bytes 
		qwordAtOffset: (index - 1) * 8
		put: anOopType64 value.

! !
!OopType64Array categoriesFor: #elementClass!constants!public! !
!OopType64Array categoriesFor: #size:!public! !
!OopType64Array categoriesFor: #uncheckedAt:!accessing!private! !
!OopType64Array categoriesFor: #uncheckedAt:put:!accessing!private! !

!OopType64Array class methodsFor!

elementSize

	^8.
!

fromBytes: aByteArray

	^(self new: aByteArray size / self elementSize)
		bytes: aByteArray;
		yourself.
! !
!OopType64Array class categoriesFor: #elementSize!instance creation!private! !
!OopType64Array class categoriesFor: #fromBytes:!public! !

GciSessionId guid: (GUID fromString: '{63B7B17D-EE05-4344-8A1F-E3EC5ABC32E8}')!
GciSessionId comment: ''!
!GciSessionId categoriesForClass!Unclassified! !
OopType32 guid: (GUID fromString: '{5E753C2A-33E0-438C-8B48-549E19CCC28E}')!
OopType32 comment: ''!
!OopType32 categoriesForClass!Unclassified! !
!OopType32 methodsFor!

is64BitBoolean

	^(self value bitAnd: 16r3F) = 16r0C.
!

is64BitCharacter

	^(self value bitAnd: 16rFFFFC01F) = 16r1C.

!

is64BitSmallInteger

	^(self value bitAnd: 3) = 2.

!

is6xBoolean

	^(self value bitAnd: 16r1F) = 16r06.
!

is6xCharacter

	^(self value bitAnd: (16rFFFFE000 bitOr: 16r1F)) = 16r0E.

!

is6xSmallInteger

	^(self value bitAnd: 3) = 3.

!

isGsNil

	^self  value = 10.
!

isSmallDouble

	^false.
!

printOn: aStream

	aStream nextPutAll: 'Oop(' , self value printString , ')'.
! !
!OopType32 categoriesFor: #is64BitBoolean!public! !
!OopType32 categoriesFor: #is64BitCharacter!public! !
!OopType32 categoriesFor: #is64BitSmallInteger!public! !
!OopType32 categoriesFor: #is6xBoolean!public! !
!OopType32 categoriesFor: #is6xCharacter!public! !
!OopType32 categoriesFor: #is6xSmallInteger!public! !
!OopType32 categoriesFor: #isGsNil!public! !
!OopType32 categoriesFor: #isSmallDouble!public! !
!OopType32 categoriesFor: #printOn:!public! !

!OopType32 class methodsFor!

elementSize

	^4.
! !
!OopType32 class categoriesFor: #elementSize!public! !

OopType64 guid: (GUID fromString: '{E5D12262-5936-4C83-B2C9-026C8197FBC9}')!
OopType64 comment: '#define OOP_TAG_RAM_OOP     0x0 /* 2r000  memory pointer ObjSType* */
#define OOP_TAG_POM_OOP     0x1 /* 2r001  disk objId */
  // disk objIds have the form 0x0000nnnnnnnnnn01 , with the 
  //  oopNumber shifted left by 8 before adding the pom tag bit 
#define OOP_TAG_SMALLINT    0x2 /* 2r010  SmallInteger */
#define OOP_TAG_SMALLDOUBLE 0x6 /* 2r110  SmallDouble */
#define OOP_TAG_SPECIAL     0x4 /* 2r100  true,false,nil, Char, JISChar */

#define OOP_NUM_TAG_BITS 3 /* number of tag bits on a disk objId */

#define OOP_POM_TAG_MASK    0x1

#define OOP_TAG_SPECIAL_MASK  0x6    /* any special oop */
'!
!OopType64 categoriesForClass!Unclassified! !
!OopType64 methodsFor!

asFraction
	"SmallFractions are special objects that can represent Fractions
	  with   -268435456 <= numerator   <= 268435455
	  and             0 <  denominator <= 134217727 . 

	 A SmallFraction contains the bits
		snnnnnnn|nnnnnnnn|nnnnnnnn|nnnnnddd|dddddddd|dddddddd|dddddddd|tttttttt
	 where bits are shown with least-significant on the right.
	 The bits  sn...n  are a 29 bit signed twos-complement numerator ,
	 The bits  d...d   are a 27 bit unsigned denominator .
	 The 8 tag bits have the constant value  16r2C . "

	| numerator denominator fraction |
	numerator := ((bytes dwordAtOffset: 4) bitAnd: 16rFFFFFFF8) bitShift: -3.
	0 ~~ (numerator bitAnd: 16r10000000) ifTrue: [
		| temp |
		temp := ByteArray new: 4.
		numerator := numerator bitOr: 16rF0000000.
		temp dwordAtOffset: 0 put: numerator.
		numerator := temp sdwordAtOffset: 0.
	].
	denominator := (bytes dwordAtOffset: 1) bitAnd: 16r7FFFFFF.
	fraction := numerator / denominator.
	^fraction
!

asSmallInteger

	| value myBytes |
	value := self value.
	(value anyMask: 16r8000000000000000) ifFalse: [^value >> 3].
	value := value bitOr: 7.
	myBytes := ByteArray new: 8.
	myBytes qwordAtOffset: 0 put: value.
	value := myBytes sqwordAtOffset: 0.
	^value >> 3.
!

isBoolean
"#define OOP_FALSE           ((OopType)0x0C)
#define OOP_TRUE           ((OopType)0x10C) 
"
	^self value = 16r00C or: [self value = 16r10C].!

isCharacter

	^(self value bitAnd: 16rFF) = 16r1C.
!

isGsNil

	^self  value = 20.
!

isImmediate

	^(self value bitAnd: 6) > 0.
!

isOopIllegal

	^self  value = 1.
!

isSmallDouble

	^(self value bitAnd: 7) = 6.
!

isSmallFraction

	^(self value bitAnd: 16rFF) = 16r2C.
!

isSmallInteger

	^(self value bitAnd: 7) = 2.

!

isSpecial
	"true,false,nil, Char, JISChar"

	^(self value bitAnd: 7) = 4.
!

objectInfo
	^objectInfo!

objectInfo: anObject
	objectInfo := anObject!

printOn: aStream

	aStream nextPutAll: 'Oop(' , self value printString , ')'.
!

smallDoubleAsFloat

	| bits sign expn frac |
	bits := (bytes qwordAtOffset: 0) bitAnd: 7 bitInvert.
	bits := bits printStringRadix: 2 showRadix: false.
	[bits size < 64] whileTrue: [bits := '0' , bits].
	expn := ('2r' , (bits copyFrom: 1 to: 8)) asNumber.
	frac := ('2r' , (bits copyFrom: 9 to: 59)) asNumber.
	sign := (bits at: 61) = $1 ifTrue: [-1] ifFalse: [1].
	frac = 0 ifTrue: [
		expn = 0 ifTrue: [^0.0].
		expn = 16rFF ifTrue: [^1.0 / 0.0 * sign].
	].
	^(frac + 16r8000000000000 / 16r8000000000000 * (2 raisedTo: expn - 127) * sign) asFloat.
! !
!OopType64 categoriesFor: #asFraction!public! !
!OopType64 categoriesFor: #asSmallInteger!public! !
!OopType64 categoriesFor: #isBoolean!public! !
!OopType64 categoriesFor: #isCharacter!public! !
!OopType64 categoriesFor: #isGsNil!public! !
!OopType64 categoriesFor: #isImmediate!public! !
!OopType64 categoriesFor: #isOopIllegal!public! !
!OopType64 categoriesFor: #isSmallDouble!public! !
!OopType64 categoriesFor: #isSmallFraction!public! !
!OopType64 categoriesFor: #isSmallInteger!public! !
!OopType64 categoriesFor: #isSpecial!public! !
!OopType64 categoriesFor: #objectInfo!accessing!public! !
!OopType64 categoriesFor: #objectInfo:!accessing!public! !
!OopType64 categoriesFor: #printOn:!public! !
!OopType64 categoriesFor: #smallDoubleAsFloat!public! !

GciErrSType32 guid: (GUID fromString: '{A4559093-6277-4975-A45B-6F8FA8285429}')!
GciErrSType32 comment: 'GciErrSType
	<C: typedef struct {
			OopType category;
			long number;
			OopType context;
			char message[401];
			OopType args[10];
			long argCount;
			BoolType fatal;
		} GciErrSType>'!
!GciErrSType32 categoriesForClass!Unclassified! !
!GciErrSType32 methodsFor!

argCount
	"Answer the receiver's argCount field as a Smalltalk object."

	^(bytes dwordAtOffset: 456)!

argCount: anObject
	"Set the receiver's argCount field to the value of anObject."

	bytes dwordAtOffset: 456 put: anObject!

args
	args ifNotNil: [^args].
	^OopType32Array fromAddress: (bytes yourAddress + 416) length: 10!

category
	"Answer the receiver's category field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

category: anObject
	"Set the receiver's category field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

categoryOop

	^OopType32 fromInteger: self category.
!

context
	"Answer the receiver's context field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

context: anObject
	"Set the receiver's context field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

contextOop

	^OopType32 fromInteger: self context.!

fatal
	"Answer the receiver's fatal field as a Smalltalk object."

	^(bytes dwordAtOffset: 460) asBoolean!

fatal: anObject
	"Set the receiver's fatal field to the value of anObject."

	bytes dwordAtOffset: 460 put: anObject asParameter!

message
	"Answer the receiver's message field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 12)!

message: anObject
	"Set the receiver's message field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (400 * 1).
	anObject replaceBytesOf: bytes from: 13 to: 12 + size startingAt: 1.
	bytes at: size+13 put: 0!

number
	"Answer the receiver's number field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

number: anObject
	"Set the receiver's number field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

oopTypeClass

	^OopType32.
! !
!GciErrSType32 categoriesFor: #argCount!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #argCount:!public! !
!GciErrSType32 categoriesFor: #args!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #category!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #category:!public! !
!GciErrSType32 categoriesFor: #categoryOop!public! !
!GciErrSType32 categoriesFor: #context!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #context:!public! !
!GciErrSType32 categoriesFor: #contextOop!public! !
!GciErrSType32 categoriesFor: #fatal!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #fatal:!public! !
!GciErrSType32 categoriesFor: #message!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #message:!public! !
!GciErrSType32 categoriesFor: #number!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #number:!public! !
!GciErrSType32 categoriesFor: #oopTypeClass!public! !

!GciErrSType32 class methodsFor!

defineFields
	"	typedef struct {
			OopType category;
			long number;
			OopType context;
			char message[401];
			OopType args[10];
			long argCount;
			BoolType fatal;
		} GciErrSType;

		GciErrSType32 compileDefinition
	"

	| arrayField stringField |
	arrayField := ArrayField type: OopType32Array length: 10.
	stringField := StringField length: 401.
	self
		defineField: #category 	type: OopType32Field 	new	;
		defineField: #number		type: DWORDField 		new	;
		defineField: #context		type: OopType32Field	new	;
		defineField: #message	type: stringField						;
		defineField: #args			type: arrayField						;
		defineField: #argCount	type: DWORDField		new	;
		defineField: #fatal			type: BOOLField			new	;
		yourself.
! !
!GciErrSType32 class categoriesFor: #defineFields!public! !

GciErrSType64 guid: (GUID fromString: '{04DEFB15-DFDD-4009-ACB0-548EB82E4AEA}')!
GciErrSType64 comment: ''!
!GciErrSType64 categoriesForClass!Unclassified! !
!GciErrSType64 methodsFor!

argCount
	"Answer the receiver's argCount field as a Smalltalk object."

	^(bytes dwordAtOffset: 100)!

argCount: anObject
	"Set the receiver's argCount field to the value of anObject."

	bytes dwordAtOffset: 100 put: anObject!

args

	args ifNotNil: [^args].
	^OopType64Array fromAddress: (bytes yourAddress + 16) length: 10!

category
	"Answer the receiver's category field as a Smalltalk object."

	^(bytes qwordAtOffset: 0)!

category: anObject
	"Set the receiver's category field to the value of anObject."

	bytes qwordAtOffset: 0 put: anObject!

categoryOop

	^OopType64 fromInteger: self category.!

context
	"Answer the receiver's context field as a Smalltalk object."

	^(bytes qwordAtOffset: 8)!

context: anObject
	"Set the receiver's context field to the value of anObject."

	bytes qwordAtOffset: 8 put: anObject!

contextOop

	^OopType64 fromInteger: self context.
!

fatal
	"Answer the receiver's fatal field as a Smalltalk object."

	^(bytes byteAtOffset: 104)!

fatal: anObject
	"Set the receiver's fatal field to the value of anObject."

	bytes byteAtOffset: 104 put: anObject!

message
	"Answer the receiver's message field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 105)!

message: anObject
	"Set the receiver's message field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (1024 * 1).
	anObject replaceBytesOf: bytes from: 106 to: 105 + size startingAt: 1.
	bytes at: size+106 put: 0!

number
	"Answer the receiver's number field as a Smalltalk object."

	^(bytes dwordAtOffset: 96)!

number: anObject
	"Set the receiver's number field to the value of anObject."

	bytes dwordAtOffset: 96 put: anObject!

oopTypeClass

	^OopType64.
! !
!GciErrSType64 categoriesFor: #argCount!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #argCount:!public! !
!GciErrSType64 categoriesFor: #args!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #category!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #category:!public! !
!GciErrSType64 categoriesFor: #categoryOop!public! !
!GciErrSType64 categoriesFor: #context!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #context:!public! !
!GciErrSType64 categoriesFor: #contextOop!public! !
!GciErrSType64 categoriesFor: #fatal!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #fatal:!public! !
!GciErrSType64 categoriesFor: #message!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #message:!public! !
!GciErrSType64 categoriesFor: #number!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #number:!public! !
!GciErrSType64 categoriesFor: #oopTypeClass!public! !

!GciErrSType64 class methodsFor!

defineFields
	"typedef struct {
		OopType			category;                      /* error dictionary       */
		OopType			context;  /* GemStone Smalltalk execution state , a GsProcess */
		OopType			args[GCI_MAX_ERR_ARGS];        /* arguments to error text */
		int				number;                        /* GemStone error number  */
		int				argCount;                      /* num of arg in the args[]*/
		unsigned char	fatal;                         /* nonzero if err is fatal */
		char			message[GCI_ERR_STR_SIZE + 1]; /* null-term. str of err text */
} GciErrSType;

		GciErrSType64 compileDefinition
	"

	| arrayField stringField |
	arrayField := ArrayField type: OopType64Array length: 10.
	stringField := StringField length: 1025.
	self
		defineField: #category 	type: OopType64Field 	new	;
		defineField: #context		type: OopType64Field	new	;
		defineField: #args			type: arrayField						;
		defineField: #number		type: DWORDField 		new	;
		defineField: #argCount	type: DWORDField		new	;
		defineField: #fatal			type: BYTEField			new	;
		defineField: #message	type: stringField						;
		yourself.
! !
!GciErrSType64 class categoriesFor: #defineFields!public! !

GciErrSType64_30 guid: (GUID fromString: '{2CFF1EDF-058D-44C9-A9AB-9A6E7BF669F1}')!
GciErrSType64_30 comment: ''!
!GciErrSType64_30 categoriesForClass!Unclassified! !
!GciErrSType64_30 methodsFor!

argCount
	"Answer the receiver's argCount field as a Smalltalk object."

	^(bytes dwordAtOffset: 108)!

argCount: anObject
	"Set the receiver's argCount field to the value of anObject."

	bytes dwordAtOffset: 108 put: anObject!

args

	args ifNotNil: [^args].
	^OopType64Array fromAddress: (bytes yourAddress + 24) length: 10!

category
	"Answer the receiver's category field as a Smalltalk object."

	^(bytes qwordAtOffset: 0)!

category: anObject
	"Set the receiver's category field to the value of anObject."

	bytes qwordAtOffset: 0 put: anObject!

categoryOop

	^OopType64 fromInteger: self category.!

context
	"Answer the receiver's context field as a Smalltalk object."

	^(bytes qwordAtOffset: 8)!

context: anObject
	"Set the receiver's context field to the value of anObject."

	bytes qwordAtOffset: 8 put: anObject!

contextOop

	^OopType64 fromInteger: self context.
!

exceptionObj
	"Answer the receiver's exceptionObj field as a Smalltalk object."

	^(bytes qwordAtOffset: 16)!

exceptionObj: anObject
	"Set the receiver's exceptionObj field to the value of anObject."

	bytes qwordAtOffset: 16 put: anObject!

exceptionObjOop

	^OopType64 fromInteger: self exceptionObj.!

fatal
	"Answer the receiver's fatal field as a Smalltalk object."

	^(bytes byteAtOffset: 112)!

fatal: anObject
	"Set the receiver's fatal field to the value of anObject."

	bytes byteAtOffset: 112 put: anObject!

message
	"Answer the receiver's message field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 113)!

message: anObject
	"Set the receiver's message field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (1024 * 1).
	anObject replaceBytesOf: bytes from: 114 to: 113 + size startingAt: 1.
	bytes at: size+114 put: 0!

number
	"Answer the receiver's number field as a Smalltalk object."

	^(bytes dwordAtOffset: 104)!

number: anObject
	"Set the receiver's number field to the value of anObject."

	bytes dwordAtOffset: 104 put: anObject!

oopTypeClass

	^OopType64.
!

reason
	"Answer the receiver's reason field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 1138)!

reason: anObject
	"Set the receiver's reason field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (63 * 1).
	anObject replaceBytesOf: bytes from: 1139 to: 1138 + size startingAt: 1.
	bytes at: size+1139 put: 0! !
!GciErrSType64_30 categoriesFor: #argCount!**compiled accessors**!public! !
!GciErrSType64_30 categoriesFor: #argCount:!public! !
!GciErrSType64_30 categoriesFor: #args!**compiled accessors**!public! !
!GciErrSType64_30 categoriesFor: #category!**compiled accessors**!public! !
!GciErrSType64_30 categoriesFor: #category:!public! !
!GciErrSType64_30 categoriesFor: #categoryOop!public! !
!GciErrSType64_30 categoriesFor: #context!**compiled accessors**!public! !
!GciErrSType64_30 categoriesFor: #context:!public! !
!GciErrSType64_30 categoriesFor: #contextOop!public! !
!GciErrSType64_30 categoriesFor: #exceptionObj!**compiled accessors**!public! !
!GciErrSType64_30 categoriesFor: #exceptionObj:!public! !
!GciErrSType64_30 categoriesFor: #exceptionObjOop!public! !
!GciErrSType64_30 categoriesFor: #fatal!**compiled accessors**!public! !
!GciErrSType64_30 categoriesFor: #fatal:!public! !
!GciErrSType64_30 categoriesFor: #message!**compiled accessors**!public! !
!GciErrSType64_30 categoriesFor: #message:!public! !
!GciErrSType64_30 categoriesFor: #number!**compiled accessors**!public! !
!GciErrSType64_30 categoriesFor: #number:!public! !
!GciErrSType64_30 categoriesFor: #oopTypeClass!public! !
!GciErrSType64_30 categoriesFor: #reason!**compiled accessors**!public! !
!GciErrSType64_30 categoriesFor: #reason:!public! !

!GciErrSType64_30 class methodsFor!

defineFields
	"typedef struct {
		OopType			category;												/* error dictionary       */
		OopType			context;												/* GemStone Smalltalk execution state , a GsProcess */
		OopType			exceptionObj;										/* an instance of Exception, or nil; may be nil if error was not signaled from Smalltalk execution */
		OopType			args[GCI_MAX_ERR_ARGS];			/* arguments to error text */
		int					number;												/* GemStone error number  */
		int					argCount;											/* num of arg in the args[]*/
		unsigned char	fatal;													/* nonzero if err is fatal */
		char					message[GCI_ERR_STR_SIZE + 1];	/* null-term. str of err text */		// GCI_ERR_STR_SIZE = 1024
		char					reason[GCI_ERR_reasonSize + 1];	// GCI_ERR_reasonSize = 63

  // If you change this struct,  Smalltalk client FFI code may
  //  need smalltalk code changes.
} GciErrSType;

		GciErrSType64_30 compileDefinition
"

	| arrayField messageField reasonField |
	arrayField := ArrayField type: OopType64Array length: 10.
	messageField := StringField length: 1025.
	reasonField := StringField length: 64.
	self
		defineField: #category 			type: OopType64Field 	new	;
		defineField: #context				type: OopType64Field	new	;
		defineField: #exceptionObj		type: OopType64Field	new	;
		defineField: #args					type: arrayField						;
		defineField: #number				type: DWORDField 		new	;
		defineField: #argCount			type: DWORDField		new	;
		defineField: #fatal					type: BYTEField			new	;
		defineField: #message			type: messageField				;
		defineField: #reason				type: reasonField					;
		yourself.
! !
!GciErrSType64_30 class categoriesFor: #defineFields!public! !

GciErrSType64_31 guid: (GUID fromString: '{673BB5D8-2105-4A66-BAEB-712876FB246C}')!
GciErrSType64_31 comment: ''!
!GciErrSType64_31 categoriesForClass!Unclassified! !
!GciErrSType64_31 methodsFor!

argCount
	"Answer the receiver's argCount field as a Smalltalk object."

	^(bytes dwordAtOffset: 108)!

argCount: anObject
	"Set the receiver's argCount field to the value of anObject."

	bytes dwordAtOffset: 108 put: anObject!

args

	args ifNotNil: [^args].
	^OopType64Array fromAddress: (bytes yourAddress + 24) length: 10!

category
	"Answer the receiver's category field as a Smalltalk object."

	^(bytes qwordAtOffset: 0)!

category: anObject
	"Set the receiver's category field to the value of anObject."

	bytes qwordAtOffset: 0 put: anObject!

categoryOop

	^OopType64 fromInteger: self category.!

context
	"Answer the receiver's context field as a Smalltalk object."

	^(bytes qwordAtOffset: 8)!

context: anObject
	"Set the receiver's context field to the value of anObject."

	bytes qwordAtOffset: 8 put: anObject!

contextOop

	^OopType64 fromInteger: self context.
!

exceptionObj
	"Answer the receiver's exceptionObj field as a Smalltalk object."

	^(bytes qwordAtOffset: 16)!

exceptionObj: anObject
	"Set the receiver's exceptionObj field to the value of anObject."

	bytes qwordAtOffset: 16 put: anObject!

exceptionObjOop

	^OopType64 fromInteger: self exceptionObj.!

fatal
	"Answer the receiver's fatal field as a Smalltalk object."

	^(bytes byteAtOffset: 112)!

fatal: anObject
	"Set the receiver's fatal field to the value of anObject."

	bytes byteAtOffset: 112 put: anObject!

message
	"Answer the receiver's message field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 113)!

message: anObject
	"Set the receiver's message field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (1024 * 1).
	anObject replaceBytesOf: bytes from: 114 to: 113 + size startingAt: 1.
	bytes at: size+114 put: 0!

number
	"Answer the receiver's number field as a Smalltalk object."

	^(bytes dwordAtOffset: 104)!

number: anObject
	"Set the receiver's number field to the value of anObject."

	bytes dwordAtOffset: 104 put: anObject!

oopTypeClass

	^OopType64.
!

reason
	"Answer the receiver's reason field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 1138)!

reason: anObject
	"Set the receiver's reason field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (1024 * 1).
	anObject replaceBytesOf: bytes from: 1139 to: 1138 + size startingAt: 1.
	bytes at: size+1139 put: 0! !
!GciErrSType64_31 categoriesFor: #argCount!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #argCount:!public! !
!GciErrSType64_31 categoriesFor: #args!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #category!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #category:!public! !
!GciErrSType64_31 categoriesFor: #categoryOop!public! !
!GciErrSType64_31 categoriesFor: #context!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #context:!public! !
!GciErrSType64_31 categoriesFor: #contextOop!public! !
!GciErrSType64_31 categoriesFor: #exceptionObj!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #exceptionObj:!public! !
!GciErrSType64_31 categoriesFor: #exceptionObjOop!public! !
!GciErrSType64_31 categoriesFor: #fatal!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #fatal:!public! !
!GciErrSType64_31 categoriesFor: #message!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #message:!public! !
!GciErrSType64_31 categoriesFor: #number!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #number:!public! !
!GciErrSType64_31 categoriesFor: #oopTypeClass!public! !
!GciErrSType64_31 categoriesFor: #reason!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #reason:!public! !

!GciErrSType64_31 class methodsFor!

defineFields
	"typedef struct {
		OopType			category;												/* error dictionary       */
		OopType			context;												/* GemStone Smalltalk execution state , a GsProcess */
		OopType			exceptionObj;										/* an instance of Exception, or nil; may be nil if error was not signaled from Smalltalk execution */
		OopType			args[GCI_MAX_ERR_ARGS];			/* arguments to error text */
		int					number;												/* GemStone error number  */
		int					argCount;											/* num of arg in the args[]*/
		unsigned char	fatal;													/* nonzero if err is fatal */
		char					message[GCI_ERR_STR_SIZE + 1];	/* null-term. str of err text */		// GCI_ERR_STR_SIZE = 1024
		char					reason[GCI_ERR_reasonSize + 1];	// GCI_ERR_reasonSize = GCI_ERR_STR_SIZE = 1024

  // If you change this struct,  Smalltalk client FFI code may
  //  need smalltalk code changes.
} GciErrSType;

		GciErrSType64_31 compileDefinition
"

	| arrayField messageField reasonField |
	arrayField := ArrayField type: OopType64Array length: 10.
	messageField := StringField length: 1025.
	reasonField := StringField length: 1025.
	self
		defineField: #category 			type: OopType64Field 	new	;
		defineField: #context				type: OopType64Field	new	;
		defineField: #exceptionObj		type: OopType64Field	new	;
		defineField: #args					type: arrayField						;
		defineField: #number				type: DWORDField 		new	;
		defineField: #argCount			type: DWORDField		new	;
		defineField: #fatal					type: BYTEField			new	;
		defineField: #message			type: messageField				;
		defineField: #reason				type: reasonField					;
		yourself.
! !
!GciErrSType64_31 class categoriesFor: #defineFields!public! !

GciMtLibraryTestCase guid: (GUID fromString: '{14F7F08C-7854-44C6-BDAA-E1B5055DC094}')!
GciMtLibraryTestCase comment: ''!
!GciMtLibraryTestCase categoriesForClass!Unclassified! !
!GciMtLibraryTestCase methodsFor!

setUp

	super setUp.
	library := LibGciRpc64_3_3 default.
	session := GciMtLibraryTestResource current session!

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
		assert: object objClass = class;
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
		assert: objects class == library oopTypeArrayClass;
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
! !
!GciMtLibraryTestCase categoriesFor: #setUp!private! !
!GciMtLibraryTestCase categoriesFor: #test_abort!public! !
!GciMtLibraryTestCase categoriesFor: #test_begin!public! !
!GciMtLibraryTestCase categoriesFor: #test_break!public! !
!GciMtLibraryTestCase categoriesFor: #test_charToOop!public! !
!GciMtLibraryTestCase categoriesFor: #test_clearStack!public! !
!GciMtLibraryTestCase categoriesFor: #test_commit!public! !
!GciMtLibraryTestCase categoriesFor: #test_continue!public! !
!GciMtLibraryTestCase categoriesFor: #test_doubleToOop!public! !
!GciMtLibraryTestCase categoriesFor: #test_doubleToSmallDouble!public! !
!GciMtLibraryTestCase categoriesFor: #test_execute!public! !
!GciMtLibraryTestCase categoriesFor: #test_fetchBytes!public! !
!GciMtLibraryTestCase categoriesFor: #test_fetchClass!public! !
!GciMtLibraryTestCase categoriesFor: #test_fetchObject1!public! !
!GciMtLibraryTestCase categoriesFor: #test_fetchObject2!public! !
!GciMtLibraryTestCase categoriesFor: #test_fetchObject3!public! !
!GciMtLibraryTestCase categoriesFor: #test_fetchObject4!public! !
!GciMtLibraryTestCase categoriesFor: #test_fetchObjects!public! !
!GciMtLibraryTestCase categoriesFor: #test_fetchSize!public! !
!GciMtLibraryTestCase categoriesFor: #test_fetchSpecialClass!public! !
!GciMtLibraryTestCase categoriesFor: #test_fetchString!public! !
!GciMtLibraryTestCase categoriesFor: #test_fetchVaryingSize!public! !
!GciMtLibraryTestCase categoriesFor: #test_gemTrace!public! !
!GciMtLibraryTestCase categoriesFor: #test_i64ToOop!public! !
!GciMtLibraryTestCase categoriesFor: #test_login!public! !
!GciMtLibraryTestCase categoriesFor: #test_newString!public! !
!GciMtLibraryTestCase categoriesFor: #test_objExists!public! !
!GciMtLibraryTestCase categoriesFor: #test_oopIsSpecial!public! !
!GciMtLibraryTestCase categoriesFor: #test_oopToChar!public! !
!GciMtLibraryTestCase categoriesFor: #test_oopToDouble!public! !
!GciMtLibraryTestCase categoriesFor: #test_oopToI64!public! !
!GciMtLibraryTestCase categoriesFor: #test_perform!public! !
!GciMtLibraryTestCase categoriesFor: #test_releaseAllObjs!public! !
!GciMtLibraryTestCase categoriesFor: #test_releaseOops!public! !
!GciMtLibraryTestCase categoriesFor: #test_resolveSymbol!public! !
!GciMtLibraryTestCase categoriesFor: #test_resolveSymbolOop!public! !
!GciMtLibraryTestCase categoriesFor: #test_sessionIsRemote!public! !
!GciMtLibraryTestCase categoriesFor: #test_version!public! !

!GciMtLibraryTestCase class methodsFor!

resources

	^IdentitySet with: GciMtLibraryTestResource! !
!GciMtLibraryTestCase class categoriesFor: #resources!public! !

GciMtLibraryTestResource guid: (GUID fromString: '{689B0A0E-E39A-43AB-AE67-060AF181BB24}')!
GciMtLibraryTestResource comment: ''!
!GciMtLibraryTestResource categoriesForClass!Unclassified! !
!GciMtLibraryTestResource methodsFor!

session

	^session!

setUp

	super setUp.
	library := LibGciRpc64_3_3 default.
	session ifNil: [
		session := library 
			loginUser: 'DataCurator'
			password: 'swordfish'
			gemNRS: '!!@portugal#netldi:ldijfoster#task!!gemnetobject'
			stoneNRS: 'jfoster0'.
	]!

tearDown

	session ifNotNil: [
		library logoutSession: session.
		session := nil.
	].
	super tearDown.
! !
!GciMtLibraryTestResource categoriesFor: #session!accessing!public! !
!GciMtLibraryTestResource categoriesFor: #setUp!private! !
!GciMtLibraryTestResource categoriesFor: #tearDown!private! !

"Binary Globals"!

