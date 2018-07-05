"Filed out from Dolphin Smalltalk 7"!

GciLibrary subclass: #GciSingleThreadedLibrary
	instanceVariableNames: 'semaphore'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciSingleThreadedLibrary guid: (GUID fromString: '{afffafeb-5777-4d4b-a7d0-931922158468}')!
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
		SessionManager current pause.
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

gciNbExecuteStrFromContext_: string _: context _: symbolList _: anInteger

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

	(ActiveLibrary isNil or: [ActiveLibrary == self]) ifFalse: [self error: 'GemStone allows only one library per process!!'].
	ActiveLibrary := self.
	super initialize.
	semaphore := Semaphore forMutualExclusion.
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
		[
			self 
				gciSetSessionId: anInteger;
				gciLogout;
				signalIfError;
				yourself.
		] on: GciError do: [:ex | 
			ex tag number == 4100 ifTrue: [ex return].
			ex pass.
		].
	].!

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

session: session execute: stringOrOop context: contextOop environment: anInteger

	anInteger == 0 ifFalse: [self error: 'Only environment 0 is supported in this version!!'].
	self critical: [
		self gciSetSessionId: session.
		self
			gciNbExecuteStrFromContext: stringOrOop
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

session: session valueOfOop: anOopType

	self critical: [
		self gciSetSessionId: session.
		^self valueOfOop: anOopType.
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
!GciSingleThreadedLibrary categoriesFor: #gciNbExecuteStrFromContext_:_:_:_:!private! !
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
!GciSingleThreadedLibrary categoriesFor: #session:execute:context:environment:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:fetchBytes:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:fetchObjects:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:oopForInteger:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:oopForString:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:releaseOops:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:send:to:with:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #session:valueOfOop:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #signalIfError!private! !
!GciSingleThreadedLibrary categoriesFor: #signalLastError!private! !
!GciSingleThreadedLibrary categoriesFor: #signalSemaphoreIfNeeded:!private! !
!GciSingleThreadedLibrary categoriesFor: #softBreakSession:!public!required! !
!GciSingleThreadedLibrary categoriesFor: #specialFromOop:!private! !
!GciSingleThreadedLibrary categoriesFor: #valueOfOop:!private! !
!GciSingleThreadedLibrary categoriesFor: #version!public! !

