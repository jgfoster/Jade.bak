| package |
package := Package name: 'GemStone C Interface'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.122'.

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
	add: #Gcirw62;
	add: #LibGciRpc64;
	add: #LibGciRpc64_20;
	add: #LibGciRpc64_21;
	add: #LibGciRpc64_22;
	add: #LibGciRpc64_23;
	add: #LibGciRpc64_24;
	add: #LibGciRpc64_30;
	add: #LibGciRpc64_31;
	add: #LibGciRpc64_310x;
	add: #LibGciRpc64_32;
	add: #LibGciRpc64_321;
	add: #LibGciRpc64_322;
	add: #LibGciRpc64_323;
	add: #LibGciRpc64_324;
	add: #LibGciRpc64_33;
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
	instanceVariableNames: 'semaphore'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciLibrary subclass: #Gcilw6x
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciLibrary subclass: #LibGciRpc64
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
LibGciRpc64 subclass: #LibGciRpc64_30
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_30 subclass: #LibGciRpc64_31
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_31 subclass: #LibGciRpc64_310x
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_31 subclass: #LibGciRpc64_32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_32 subclass: #LibGciRpc64_321
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_32 subclass: #LibGciRpc64_322
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_32 subclass: #LibGciRpc64_323
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_32 subclass: #LibGciRpc64_324
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_32 subclass: #LibGciRpc64_33
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #GciErrSType
	instanceVariableNames: 'args stack'
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
DWORD subclass: #OopType32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ULARGE_INTEGER subclass: #OopType64
	instanceVariableNames: ''
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
GciCommitFailure guid: (GUID fromString: '{0282A2E9-2264-46A8-8391-61EDE92B12C7}')!
GciCommitFailure comment: ''!
!GciCommitFailure categoriesForClass!Unclassified! !
GciLoginFailed guid: (GUID fromString: '{48341733-FBF0-4F01-A585-CCA55E4FF300}')!
GciLoginFailed comment: ''!
!GciLoginFailed categoriesForClass!Unclassified! !
GciLibrary guid: (GUID fromString: '{AFFFAFEB-5777-4D4B-A7D0-931922158468}')!
GciLibrary comment: 'JadeLoginShell show.
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
!GciLibrary categoriesForClass!Unclassified! !
!GciLibrary methodsFor!

_semaphore

	^semaphore.
!

abortSession: anInteger

	self critical: [
		self gciSetSessionId: anInteger.
		self gciAbort.
	].
!

beginSession: anInteger

	self critical: [
		self gciSetSessionId: anInteger.
		self gciBegin.
	].
!

callInProgress

	^self gciCallInProgress.
!

clearStack: anOopType session: anInteger

	self critical: [
		self gciSetSessionId: anInteger.
		self gciClearStack: anOopType.
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

continue: aProcess session: anInteger

	^self
		continue: aProcess 
		with: self oopIllegal 
		session: anInteger.
!

continue: aProcess with: anObject session: anInteger

	self critical: [
		self gciSetSessionId: anInteger.
		self 
			gciNbContinueWith: aProcess
			_: anObject
			_: 1 "GCI_PERFORM_FLAG_ENABLE_DEBUG"
			_: nil.
		^self nbResult.
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

errorStructureClass

	^GciErrSType64.
!

executeString: aString fromContext: anOopType session: anInteger

	self critical: [
		self gciSetSessionId: anInteger.
		self
			gciNbExecuteStrFromContext: aString
			_: anOopType
			_: self oopNil.
		^self nbResult.
	].
!

executeString: aString session: anInteger

	^self
		executeStr: aString
		fromContext: self oopNil
		session: anInteger.
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

fetchBytes: anOopType session: anInteger

	self critical: [
		self gciSetSessionId: anInteger.
		^self fetchBytes: anOopType.
	].
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

fetchObjImpl: anOopType session: anInteger

	self critical: [
		^self fetchObjImpl: anOopType.
	].
!

fetchOops: anOopType session: anInteger

	anOopType isSpecial ifTrue: [^self specialFromOop: anOopType].
	self critical: [
		| type size array result |
		self gciSetSessionId: anInteger.
		(type := self fetchObjImpl:anOopType) = 0 ifFalse: [self error: 'Expected 0 but got ' , type printString].
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

gciLongToOop: anInteger

	<cdecl: OopType32 GciLongToOop sdword>
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

	<cdecl: void GciNbPerformNoDebug OopType64 lpstr OopType64* dword>
	^self invalidCall
!

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
!

is32Bit

	^false.
!

is64Bit

	^false.
!

is64Bit24

	^false.
!

is64Bit32

	^false.
!

is64Bit3x

	^false.
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

logoutSession: anInteger

	self critical: [
		self gciSetSessionId: anInteger.
		self gciLogout.
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

newString: aString session: anInteger

	self critical: [
		self gciSetSessionId: anInteger.
		^self gciNewString: aString.
	].
!

oopAsciiNul

	self subclassResponsibility.
!

oopAt: anExternalAddress

	^OopType32 fromInteger: (anExternalAddress sdwordAtOffset: 0).
!

oopClassArray

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

	| int bytes |
	bytes := ByteArray new: 4.
	bytes 
		dwordAtOffset: 0 
		put: anInteger.
	int := bytes sdwordAtOffset: 0.
	^self gciLongToOop: int.
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

	self subclassResponsibility.
!

pollForSignalSession: anInteger

	self critical: [
		self gciSetSessionId: anInteger.
		self gciPollForSignal ifFalse: [^self].
		self signalLastError.
	].
!

releaseOops: anArray session: anInteger

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

send: aString to: anOopType with: anArray session: anInteger

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

sendInterpreted: aString to: anOopType with: anArray session: anInteger

	^self 
		send: aString 
		to: anOopType 
		with: anArray 
		session: anInteger.
!

signalIfError

	| error |
	(error := self lastError) notNil ifTrue: [GciError signalWith: error].
!

signalLastError

	GciError signal: self lastError.
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

valueOfOop: anOopType

	| type |
	type := self fetchObjImpl: anOopType.
	type = 1 ifTrue: [^self fetchBytes: anOopType].
	type = 3 ifTrue: [^self specialFromOop: anOopType].
	^anOopType.
! !
!GciLibrary categoriesFor: #_semaphore!private! !
!GciLibrary categoriesFor: #abortSession:!public! !
!GciLibrary categoriesFor: #beginSession:!public! !
!GciLibrary categoriesFor: #callInProgress!public! !
!GciLibrary categoriesFor: #clearStack:session:!public! !
!GciLibrary categoriesFor: #close!public! !
!GciLibrary categoriesFor: #commitSession:!public! !
!GciLibrary categoriesFor: #continue:session:!public! !
!GciLibrary categoriesFor: #continue:with:session:!public! !
!GciLibrary categoriesFor: #critical:!private! !
!GciLibrary categoriesFor: #errorStructureClass!private! !
!GciLibrary categoriesFor: #executeString:fromContext:session:!public! !
!GciLibrary categoriesFor: #executeString:session:!public! !
!GciLibrary categoriesFor: #fetchBytes:!private! !
!GciLibrary categoriesFor: #fetchBytes:class:!private! !
!GciLibrary categoriesFor: #fetchBytes:session:!public! !
!GciLibrary categoriesFor: #fetchChars:!private! !
!GciLibrary categoriesFor: #fetchClass:!private! !
!GciLibrary categoriesFor: #fetchObjImpl:!private! !
!GciLibrary categoriesFor: #fetchObjImpl:session:!public! !
!GciLibrary categoriesFor: #fetchOops:session:!public! !
!GciLibrary categoriesFor: #gciAbort!private! !
!GciLibrary categoriesFor: #gciBegin!private! !
!GciLibrary categoriesFor: #gciCallInProgress!private! !
!GciLibrary categoriesFor: #gciClearStack:!private! !
!GciLibrary categoriesFor: #gciCommit!private! !
!GciLibrary categoriesFor: #gciErr:!private! !
!GciLibrary categoriesFor: #gciFetchBytes:_:_:_:!private! !
!GciLibrary categoriesFor: #gciFetchChars:_:_:_:!private! !
!GciLibrary categoriesFor: #gciFetchClass:!private! !
!GciLibrary categoriesFor: #gciFetchObjImpl:!private! !
!GciLibrary categoriesFor: #gciFetchSize:!private! !
!GciLibrary categoriesFor: #gciFetchVaryingOops:_:_:_:!private! !
!GciLibrary categoriesFor: #gciGemTrace:!private! !
!GciLibrary categoriesFor: #gciGetSessionId!private! !
!GciLibrary categoriesFor: #gciHardBreak!private! !
!GciLibrary categoriesFor: #gciInit!private! !
!GciLibrary categoriesFor: #gciLogin:_:!private! !
!GciLibrary categoriesFor: #gciLogout!private! !
!GciLibrary categoriesFor: #gciLongToOop:!private! !
!GciLibrary categoriesFor: #gciNbContinueWith:_:_:_:!private! !
!GciLibrary categoriesFor: #gciNbEnd:!private! !
!GciLibrary categoriesFor: #gciNbExecuteStrFromContext:_:_:!private! !
!GciLibrary categoriesFor: #gciNbPerform:_:_:_:!private! !
!GciLibrary categoriesFor: #gciNbPerformNoDebug:_:_:_:!private! !
!GciLibrary categoriesFor: #gciNewString:!private! !
!GciLibrary categoriesFor: #gciOopToChr:!private! !
!GciLibrary categoriesFor: #gciPollForSignal!private! !
!GciLibrary categoriesFor: #gciReleaseOops:_:!private! !
!GciLibrary categoriesFor: #gciSetNet:_:_:_:!private! !
!GciLibrary categoriesFor: #gciSetSessionId:!private! !
!GciLibrary categoriesFor: #gciShutdown!private! !
!GciLibrary categoriesFor: #gciSoftBreak!private! !
!GciLibrary categoriesFor: #gciVersion!private! !
!GciLibrary categoriesFor: #gemTrace:!public! !
!GciLibrary categoriesFor: #hardBreakSession:!public! !
!GciLibrary categoriesFor: #initialize!private! !
!GciLibrary categoriesFor: #is32Bit!public!Testing! !
!GciLibrary categoriesFor: #is64Bit!public!Testing! !
!GciLibrary categoriesFor: #is64Bit24!public!Testing! !
!GciLibrary categoriesFor: #is64Bit32!public! !
!GciLibrary categoriesFor: #is64Bit3x!public!Testing! !
!GciLibrary categoriesFor: #lastError!private! !
!GciLibrary categoriesFor: #loginAs:password:!public! !
!GciLibrary categoriesFor: #logoutSession:!public! !
!GciLibrary categoriesFor: #nbResult!public! !
!GciLibrary categoriesFor: #newString:session:!public! !
!GciLibrary categoriesFor: #oopAsciiNul!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopAt:!public! !
!GciLibrary categoriesFor: #oopClassArray!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopClassDoubleByteString!public! !
!GciLibrary categoriesFor: #oopClassQuadByteString!public! !
!GciLibrary categoriesFor: #oopClassString!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopClassSymbol!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopClassSystem!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopClassUnicode16!public! !
!GciLibrary categoriesFor: #oopClassUnicode32!public! !
!GciLibrary categoriesFor: #oopClassUnicode7!public! !
!GciLibrary categoriesFor: #oopFalse!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopForInteger:!public! !
!GciLibrary categoriesFor: #oopGemStoneError!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopIllegal!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopMaxSmallInteger!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopMinSmallInteger!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopMinusOne!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopNil!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopOne!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopRemoteNil!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopTrue!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopTwo!public!Reserved OOPs! !
!GciLibrary categoriesFor: #oopTypeArrayClass!public! !
!GciLibrary categoriesFor: #oopTypeClass!public! !
!GciLibrary categoriesFor: #oopTypeWithOop:!public! !
!GciLibrary categoriesFor: #oopZero!public!Reserved OOPs! !
!GciLibrary categoriesFor: #pollForSignalSession:!public! !
!GciLibrary categoriesFor: #releaseOops:session:!public! !
!GciLibrary categoriesFor: #send:to:with:session:!public! !
!GciLibrary categoriesFor: #sendInterpreted:to:with:session:!public! !
!GciLibrary categoriesFor: #signalIfError!private! !
!GciLibrary categoriesFor: #signalLastError!private! !
!GciLibrary categoriesFor: #signalSemaphoreIfNeeded:!private! !
!GciLibrary categoriesFor: #softBreakSession:!public! !
!GciLibrary categoriesFor: #valueOfOop:!private! !

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

fileNameSearch

	^(self name copyReplaceAll: '_' with: '-') , '*.dll'.
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
		result gciInit ifFalse: [self error: 'GciInit() failed!!'].
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
!GciLibrary class categoriesFor: #fileNameSearch!public! !
!GciLibrary class categoriesFor: #onStartup2!public! !
!GciLibrary class categoriesFor: #open:!public! !
!GciLibrary class categoriesFor: #sessionStarted!public! !
!GciLibrary class categoriesFor: #withDisplayName:!public! !

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

is32Bit

	^true.
!

oopAsciiNul

	^OopType32 fromInteger: 14.!

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
!Gcilw6x categoriesFor: #gciNbContinueWith:_:_:_:!private! !
!Gcilw6x categoriesFor: #gciNbExecuteStrFromContext:_:_:!private! !
!Gcilw6x categoriesFor: #gciNbPerform:_:_:_:!private! !
!Gcilw6x categoriesFor: #gciNewString:!private! !
!Gcilw6x categoriesFor: #gciOopToChr:!private! !
!Gcilw6x categoriesFor: #gciReleaseOops:_:!private! !
!Gcilw6x categoriesFor: #is32Bit!public!Testing! !
!Gcilw6x categoriesFor: #oopAsciiNul!public!Reserved OOPs! !
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
!Gcilw6x categoriesFor: #oopZero!public!Reserved OOPs! !
!Gcilw6x categoriesFor: #specialFromOop:!public! !

LibGciRpc64 guid: (GUID fromString: '{CE7FE05A-C8EE-4A13-B4AA-7C8AEF295620}')!
LibGciRpc64 comment: ''!
!LibGciRpc64 categoriesForClass!Unclassified! !
!LibGciRpc64 methodsFor!

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

is64Bit

	^true.
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
!LibGciRpc64 categoriesFor: #is64Bit!public!Testing! !
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
!LibGciRpc64_24 methodsFor!

is64Bit24

	^true.
! !
!LibGciRpc64_24 categoriesFor: #is64Bit24!public!Testing! !

!LibGciRpc64_24 class methodsFor!

displayName

	^'64-bit 2.4.x'.
! !
!LibGciRpc64_24 class categoriesFor: #displayName!public! !

LibGciRpc64_30 guid: (GUID fromString: '{A8A6C6D6-29C1-4E0D-9C00-EC47F1078D4D}')!
LibGciRpc64_30 comment: ''!
!LibGciRpc64_30 categoriesForClass!Unclassified! !
!LibGciRpc64_30 methodsFor!

errorStructureClass

	^GciErrSType64_30.
!

gciErr: errorReport

	<cdecl: bool GciErr GciErrSType64_30*>
	^self invalidCall
!

is64Bit3x

	^true.
! !
!LibGciRpc64_30 categoriesFor: #errorStructureClass!private! !
!LibGciRpc64_30 categoriesFor: #gciErr:!private! !
!LibGciRpc64_30 categoriesFor: #is64Bit3x!public!Testing! !

!LibGciRpc64_30 class methodsFor!

displayName

	^'64-bit 3.0.x'.
! !
!LibGciRpc64_30 class categoriesFor: #displayName!public! !

LibGciRpc64_31 guid: (GUID fromString: '{B9F761F8-7AA2-47AB-804A-8A2C76AFDB1D}')!
LibGciRpc64_31 comment: ''!
!LibGciRpc64_31 categoriesForClass!Unclassified! !
!LibGciRpc64_31 methodsFor!

errorStructureClass

	^GciErrSType64_31.
! !
!LibGciRpc64_31 categoriesFor: #errorStructureClass!private! !

!LibGciRpc64_31 class methodsFor!

displayName

	^'64-bit 3.1.0'.
!

fileNameSearch

	^'libgcirpc-3.1.0-32.dll'.
! !
!LibGciRpc64_31 class categoriesFor: #displayName!public! !
!LibGciRpc64_31 class categoriesFor: #fileNameSearch!public! !

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

LibGciRpc64_32 guid: (GUID fromString: '{25CBF893-B55F-4C84-B927-F0E27DAF8114}')!
LibGciRpc64_32 comment: ''!
!LibGciRpc64_32 categoriesForClass!Unclassified! !
!LibGciRpc64_32 methodsFor!

is64Bit32

	^true.
! !
!LibGciRpc64_32 categoriesFor: #is64Bit32!public! !

!LibGciRpc64_32 class methodsFor!

displayName

	^'64-bit 3.2'.
!

fileNameSearch

	^'libgcirpc-3.2.0-32.dll'.
! !
!LibGciRpc64_32 class categoriesFor: #displayName!public! !
!LibGciRpc64_32 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_321 guid: (GUID fromString: '{9B3A8D44-3E44-49AC-90C5-AFDA22E1CD1B}')!
LibGciRpc64_321 comment: ''!
!LibGciRpc64_321 categoriesForClass!Unclassified! !
!LibGciRpc64_321 class methodsFor!

displayName

	^'64-bit 3.2.1'.
!

fileNameSearch

	^'libgcirpc-3.2.1-32.dll'.
! !
!LibGciRpc64_321 class categoriesFor: #displayName!public! !
!LibGciRpc64_321 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_322 guid: (GUID fromString: '{BD5DC8E2-7DCC-48E3-B0B8-9F73D810E6E6}')!
LibGciRpc64_322 comment: ''!
!LibGciRpc64_322 categoriesForClass!Unclassified! !
!LibGciRpc64_322 class methodsFor!

displayName

	^'64-bit 3.2.2'.
!

fileNameSearch

	^'libgcirpc-3.2.2-32.dll'.
! !
!LibGciRpc64_322 class categoriesFor: #displayName!public! !
!LibGciRpc64_322 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_323 guid: (GUID fromString: '{7CD3D48A-C954-4465-8DBC-90511A44E6E3}')!
LibGciRpc64_323 comment: ''!
!LibGciRpc64_323 categoriesForClass!Unclassified! !
!LibGciRpc64_323 class methodsFor!

displayName

	^'64-bit 3.2.3'.
!

fileNameSearch

	^'libgcirpc-3.2.3-32.dll'.
! !
!LibGciRpc64_323 class categoriesFor: #displayName!public! !
!LibGciRpc64_323 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_324 guid: (GUID fromString: '{AE6862DF-767F-45A7-8CF0-2520FDD79C28}')!
LibGciRpc64_324 comment: ''!
!LibGciRpc64_324 categoriesForClass!Unclassified! !
!LibGciRpc64_324 class methodsFor!

displayName

	^'64-bit 3.2.4'.
!

fileNameSearch

	^'libgcirpc-3.2.4-32.dll'.
! !
!LibGciRpc64_324 class categoriesFor: #displayName!public! !
!LibGciRpc64_324 class categoriesFor: #fileNameSearch!public! !

LibGciRpc64_33 guid: (GUID fromString: '{68DB187D-A739-41BE-AD58-A476218B1B04}')!
LibGciRpc64_33 comment: ''!
!LibGciRpc64_33 categoriesForClass!Unclassified! !
!LibGciRpc64_33 class methodsFor!

displayName

	^'64-bit 3.3'.
!

fileNameSearch

	^'libgcirpc-3.3.0-32.dll'.
! !
!LibGciRpc64_33 class categoriesFor: #displayName!public! !
!LibGciRpc64_33 class categoriesFor: #fileNameSearch!public! !

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

uncheckedAt: index 

	^OopType64 fromInteger: (bytes qwordAtOffset: (index - 1) * 8)!

uncheckedAt: index  put: anOopType64

	bytes 
		qwordAtOffset: (index - 1) * 8
		put: anOopType64 value.

! !
!OopType64Array categoriesFor: #elementClass!constants!public! !
!OopType64Array categoriesFor: #uncheckedAt:!accessing!private! !
!OopType64Array categoriesFor: #uncheckedAt:put:!accessing!private! !

!OopType64Array class methodsFor!

elementSize

	^8.
! !
!OopType64Array class categoriesFor: #elementSize!instance creation!private! !

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

isSmallInteger

	^(self value bitAnd: 7) = 2.

!

isSpecial
	"true,false,nil, Char, JISChar"

	^(self value bitAnd: 7) = 4.
!

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
!OopType64 categoriesFor: #asSmallInteger!public! !
!OopType64 categoriesFor: #isBoolean!public! !
!OopType64 categoriesFor: #isCharacter!public! !
!OopType64 categoriesFor: #isGsNil!public! !
!OopType64 categoriesFor: #isImmediate!public! !
!OopType64 categoriesFor: #isOopIllegal!public! !
!OopType64 categoriesFor: #isSmallDouble!public! !
!OopType64 categoriesFor: #isSmallInteger!public! !
!OopType64 categoriesFor: #isSpecial!public! !
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

"Binary Globals"!

