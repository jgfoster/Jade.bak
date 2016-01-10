"Filed out from Dolphin Smalltalk 7"!

GciLibrary subclass: #GciMultiThreadedLibrary
	instanceVariableNames: ''
	classVariableNames: 'OOP_ILLEGAL OOP_NIL'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

