"Filed out from Dolphin Smalltalk 7"!

GciLibrary subclass: #GciThreadSafeLibrary
	instanceVariableNames: ''
	classVariableNames: 'OOP_ILLEGAL OOP_NIL'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciThreadSafeLibrary guid: (GUID fromString: '{9edd7a96-1ba3-4be6-a416-2773ac69a324}')!
GciThreadSafeLibrary comment: ''!
!GciThreadSafeLibrary categoriesForClass!Unclassified! !
!GciThreadSafeLibrary methodsFor!

abortSession: session

	| didAbort gciErrSType |
	gciErrSType := self errorStructureClass new.
	didAbort := self GciTsAbort: session _: gciErrSType asParameter.
	didAbort ifFalse: [self gciError: gciErrSType].
!

beginSession: session

	| didBegin gciErrSType |
	gciErrSType := self errorStructureClass new.
	didBegin := self GciTsBegin: session _: gciErrSType asParameter.
	didBegin ifFalse: [self gciError: gciErrSType].
!

characterForOop: anOop

	^Character codePoint: (self GciTsOopToChar: anOop)

!

classForSpecial: anOop

	^self GciTsFetchSpecialClass: anOop

!

commitSession: session

	| didCommit gciErrSType |
	gciErrSType := self errorStructureClass new.
	didCommit := self GciTsCommit: session _: gciErrSType asParameter.
	didCommit ifFalse: [self gciError: gciErrSType].
!

fetchObjImpl: anOopType

	self subclassResponsibility.
!

gciError: aGciErrSType

	GciError signalWith: aGciErrSType.
!

GciTsAbort: session _: error

	<cdecl: bool GciTsAbort GciSessionId lpvoid>
	^self invalidCall: _failureCode
!

GciTsBegin: session _: error

	<cdecl: bool GciTsBegin GciSessionId lpvoid>
	^self invalidCall: _failureCode!

GciTsBreak: session _: isHard _: error

	<cdecl: bool GciTsBreak GciSessionId bool lpvoid>
	^self invalidCall: _failureCode!

GciTsCharToOop: myChar

	<cdecl: OopType64 GciTsCharToOop dword>
	^self invalidCall: _failureCode!

GciTsClearStack: session _: process _: error

	<cdecl: bool GciTsClearStack GciSessionId OopType64 lpvoid>
	^self invalidCall: _failureCode
!

GciTsCommit: session _: error

	<cdecl: bool GciTsCommit GciSessionId lpvoid>
	^self invalidCall: _failureCode
!

GciTsContinueWith: session _: gsProcess _: replaceTopOfStack _: continueWithError _: flags _: error
	"Remember, this doesn't return an OopType64 but an error code on the thread fork attempt.
	The actual result is sent to the callback."

	<cdecl: OopType64 GciTsContinueWith GciSessionId OopType64 OopType64 lpvoid sdword lpvoid>
	^self invalidCall: _failureCode
!

GciTsDoubleToOop: session _: double _: error

	<cdecl: OopType64 GciTsDoubleToOop GciSessionId double lpvoid>
	^self invalidCall: _failureCode!

GciTsDoubleToSmallDouble: aFloat

	<cdecl: OopType64 GciTsDoubleToSmallDouble double>
	^self invalidCall: _failureCode
!

GciTsExecute: session _: sourceString _: sourceOop _: contextOop _: symbolListOop _: flags _: environment _: error
	"Remember, this doesn't return an OopType64 but an error code on the thread fork attempt.
	The actual result is sent to the callback."

	<cdecl: OopType64 GciTsExecute GciSessionId lpvoid OopType64 OopType64 OopType64 sdword word lpvoid>
	^self invalidCall: _failureCode
!

GciTsFetchBytes: session _: theObject _: startIndex _: dest _: numBytes _: error

	<cdecl: sqword GciTsFetchBytes GciSessionId OopType64 sqword lpvoid sqword lpvoid>
	^self invalidCall: _failureCode
!

GciTsFetchChars: session _: theObject _: startIndex _: cString _: maxSize _: error

	<cdecl: sqword GciTsFetchChars GciSessionId OopType64 sqword lpvoid sqword lpvoid>
	^self invalidCall: _failureCode
!

GciTsFetchClass: session _: theObject _: error

	<cdecl: OopType64 GciTsFetchClass GciSessionId OopType64 lpvoid>
	^self invalidCall: _failureCode
!

GciTsFetchObjInfo: session _: objId _: addToExportSet _: result _: buffer _: bufsize _: error

	<cdecl: sqword GciTsFetchObjInfo GciSessionId OopType64 bool lpvoid lpvoid sdword lpvoid>
	^self invalidCall: _failureCode
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

GciTsFetchOops: session _: theObject _: startIndex _: theOops _: numOops _: error

	<cdecl: sdword GciTsFetchOops GciSessionId OopType64 sqword OopType64Array* sdword lpvoid>
	^self invalidCall: _failureCode
!

GciTsFetchSize: session _: theObject _: error

	<cdecl: sqword GciTsFetchSize GciSessionId OopType64 lpvoid>
	^self invalidCall: _failureCode!

GciTsFetchSpecialClass: anOop

	<cdecl: OopType64 GciTsFetchSpecialClass OopType64>
	^self invalidCall: _failureCode
!

GciTsFetchVaryingSize: session _: theObject _: error

	<cdecl: sqword GciTsFetchVaryingSize GciSessionId OopType64 lpvoid>
	^self invalidCall: _failureCode
!

GciTsGemTrace: session _: enable _: error

	<cdecl: sdword GciTsGemTrace GciSessionId sdword lpvoid>
	^self invalidCall: _failureCode
!

GciTsI64ToOop: session _: anInteger _: error

	<cdecl: OopType64 GciTsI64ToOop GciSessionId sqword lpvoid>
	^self invalidCall: _failureCode
!

GciTsLogin: stoneName 
	_:hostUserId 
	_: hostPassword 
	_: hostPwIsEncrypted 
	_: gemServiceNRS 
	_: gsUsername 
	_: gsPassword 
	_: loginFlags 
	_: haltOnErrNum 
	_: gciErrSType 

	<cdecl: GciSessionId GciTsLogin lpvoid lpvoid lpvoid bool lpvoid lpvoid lpvoid dword sdword lpvoid>
	^self invalidCall: _failureCode
!

GciTsLogout: session _: error

	<cdecl: bool GciTsLogout GciSessionId lpvoid>
	^self invalidCall: _failureCode
!

GciTsNewString: session _: string _: error

	<cdecl: OopType64 GciTsNewString GciSessionId lpstr lpvoid>
	^self invalidCall: _failureCode
!

GciTsObjExists: session _: anOop

	<cdecl: bool GciTsObjExists GciSessionId OopType64>
	^self invalidCall: _failureCode!

GciTsOopIsSpecial: anOop

	<cdecl: bool GciTsOopIsSpecial OopType64>
	^self invalidCall: _failureCode
!

GciTsOopToChar: anOop

	<cdecl: sdword GciTsOopToChar OopType64>
	^self invalidCall: _failureCode
!

GciTsOopToDouble: session _: anOop _: result _: error

	<cdecl: bool GciTsOopToDouble GciSessionId OopType64 lpvoid lpvoid>
	^self invalidCall: _failureCode
!

GciTsOopToI64: session _: anOop _: result _: error

	<cdecl: bool GciTsOopToI64 GciSessionId OopType64 lpvoid lpvoid>
	^self invalidCall: _failureCode!

GciTsPerform: session _: reciverOop _: symbolOop _: selectorString _: argumentList _: argumentCount _: flags _: environment _: error
	"Remember, this doesn't return an OopType64 but an error code on the thread fork attempt.
	The actual result is sent to the callback."

	<cdecl: OopType64 GciTsPerform GciSessionId OopType64 OopType64 lpvoid qword* sdword sdword word lpvoid>
	^self invalidCall: _failureCode!

GciTsReleaseAllObjs: session _: error

	<cdecl: bool GciTsReleaseAllObjs GciSessionId lpvoid>
	^self invalidCall: _failureCode!

GciTsReleaseObjs: session _: oopList _: count _: error

	<cdecl: bool GciTsReleaseAllObjs GciSessionId OopType64Array* sdword lpvoid>
	^self invalidCall: _failureCode
!

GciTsResolveSymbol: session _: string _: anOop _: error

	<cdecl: OopType64 GciTsResolveSymbol GciSessionId lpstr OopType64 lpvoid>
	^self invalidCall: _failureCode
!

GciTsResolveSymbolObj: session _: stringOop _: symbolListOop _: error

	<cdecl: OopType64 GciTsResolveSymbolObj GciSessionId OopType64 OopType64 lpvoid>
	^self invalidCall: _failureCode!

GciTsSessionIsRemote: sessionId

	<cdecl: sdword GciTsSessionIsRemote GciSessionId>
	^self invalidCall: _failureCode!

GciTsVersion: buffer _: size

	<cdecl: dword GciTsVersion lpstr dword>
	^self invalidCall: _failureCode!

hardBreakSession: session

	self session: session breakHard: true.
!

initialize
"
	LibGciTs_3_3 new initialize.
"
	OOP_ILLEGAL := OopType64 fromInteger: 1.
	OOP_NIL := OopType64 fromInteger: 20.
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

	| stoneNrsEx hostUserEx hostPasswordEx gemNrsEx gsUserEx gsPasswordEx gciErrSType sessionId |
	stoneNrsEx := ExternalMemory fromString: stoneNRS.
	hostUserEx := ExternalMemory fromString: hostUser.
	hostPasswordEx := ExternalMemory fromString: hostPassword.
	gemNrsEx := ExternalMemory fromString: gemNRS.
	gsUserEx := ExternalMemory fromString: gsUser.
	gsPasswordEx := ExternalMemory fromString: gsPassword.
	gciErrSType := self errorStructureClass new.
	sessionId := self 
		GciTsLogin: stoneNrsEx asParameter
		_: hostUserEx asParameter
		_: hostPasswordEx asParameter
		_: isEncrypted
		_: gemNrsEx asParameter
		_: gsUserEx asParameter
		_: gsPasswordEx asParameter
		_: loginFlags
		_: errNum
		_: gciErrSType asParameter.
	sessionId ifNil: [self gciError: gciErrSType].
	^sessionId

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
	didLogout := self GciTsLogout: session _: gciErrSType asParameter.
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

	^self GciTsCharToOop: aCharacter codePoint

!

oopForSmallDouble: aFloat

	| oop |
	oop := self GciTsDoubleToSmallDouble: aFloat.
	oop == OOP_ILLEGAL ifTrue: [self error: 'Unable to convert ' , aFloat printString , ' to a SmallDouble'].
	^oop!

oopGemStoneError

	^OopType64 fromInteger:  231169.!

oopIllegal

	^OopType64 fromInteger: 1. "16r01"
!

oopIsSpecial: anOop

	^self GciTsOopIsSpecial: anOop

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
	didRelease := self GciTsReleaseAllObjs: session _: gciErrSType asParameter.
	didRelease ifFalse: [self gciError: gciErrSType].
!

session: session breakHard: isHard

	| flag gciErrSType |
	gciErrSType := self errorStructureClass new.
	flag := self GciTsBreak: session _: isHard _: gciErrSType asParameter.
	flag ifFalse: [self gciError: gciErrSType].
!

session: session clearStack: process

	| didAbort gciErrSType |
	gciErrSType := self errorStructureClass new.
	didAbort := self GciTsClearStack: session _: process _: gciErrSType asParameter.
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

	| gciErrSType result |
	gciErrSType := self errorStructureClass new.
	result := self 
		GciTsContinueWith: session
		_: gsProcessOop 
		_: anOop 
		_: aGciErrSType 
		_: flags 
		_: gciErrSType asParameter.
	result = OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
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
	flag := self GciTsOopToDouble: session _: anOop _: bytes asParameter _: gciErrSType asParameter.
	flag ifFalse: [self gciError: gciErrSType].
	^bytes doubleAtOffset: 0!

session: session execute: stringOrOop

	^self session: session execute: stringOrOop context: OOP_ILLEGAL symbolList: OOP_NIL.!

session: session execute: stringOrOop context: contextOop

	^self session: session execute: stringOrOop context: contextOop symbolList: OOP_NIL.!

session: session execute: stringOrOop context: contextOop symbolList: symbolListOop

	| result stringEx stringOop gciErrSType |
	gciErrSType := self errorStructureClass new.
	(stringOrOop isKindOf: String) ifTrue: [
		stringEx := ExternalMemory fromString: stringOrOop.
		stringOop := self oopClassString.
	] ifFalse: [
		stringEx := OOP_NIL.
		stringOop := stringOrOop.
	].
	result := self
		GciTsExecute: session 
		_: stringEx asParameter 
		_: stringOop  
		_: contextOop 
		_: symbolListOop 
		_: 1 "GCI_PERFORM_FLAG_ENABLE_DEBUG"
		_: 0 "environment" 
		_: gciErrSType asParameter.
	result = OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^self session: session valueOfOop: result
!

session: session fetchBytes: anOop

	| bytes gciErrSType result |
	gciErrSType := self errorStructureClass new.
	bytes := ByteArray new: 1000.
	result := self GciTsFetchBytes: session _: anOop _: 1 _: bytes _: bytes size _: gciErrSType.
	result < 0 ifTrue: [self gciError: gciErrSType].
	(0 <= result and: [result < bytes size]) ifTrue: [^bytes copyFrom: 1 to: result].
	bytes := ByteArray new: (self session: session fetchSize: anOop).
	result := self GciTsFetchBytes: session _: anOop _: 1 _: bytes _: bytes size _: gciErrSType.
	^bytes!

session: session fetchClass: anOop

	| gciErrSType result |
	gciErrSType := self errorStructureClass new.
	result := self GciTsFetchClass: session _: anOop _: gciErrSType asParameter.
	result == OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^result!

session: session fetchObject: anOop

	| buffer class gciErrSType gciTsObjInfo implementation result |
	gciErrSType := self errorStructureClass new.
	gciTsObjInfo := GciTsObjInfo new.
	buffer := ByteArray new: 1000.
	result := self 
		GciTsFetchObjInfo: session 
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
	class := gciTsObjInfo objClassOop.
	(class isKindOf: OopType64) ifFalse: [self error: 'class isKindOf: ' , class class name].
	implementation == 3 "Special" ifTrue: [
		anOop = self oopNil 				ifTrue: [gciTsObjInfo data: nil	] ifFalse: [
		anOop = self oopTrue 			ifTrue: [gciTsObjInfo data: true	] ifFalse: [
		anOop = self oopFalse 			ifTrue: [gciTsObjInfo data: false] ifFalse: [
		class = self oopClassSmallInteger 	ifTrue: [gciTsObjInfo data: (self session: session integerForOop: anOop)	] ifFalse: [
		class = self oopClassSmallDouble 	ifTrue: [gciTsObjInfo data: (self session: session doubleForOop: anOop)	] ifFalse: [
		class = self oopClassCharacter		ifTrue: [gciTsObjInfo data: (self characterForOop: anOop)				] ifFalse: [
		class = self oopClassSmallFraction	ifTrue: [gciTsObjInfo data: anOop asFraction						] ifFalse: [
		self error: 'Unrecognized special: ' , anOop printString , ' in class with OOP ' , class printString]]]]]]].
		^gciTsObjInfo
	].
	"bytes"
	result < gciTsObjInfo objSize ifTrue: [
			buffer := ByteArray new: gciTsObjInfo objSize.
			result := self GciTsFetchBytes: session _: anOop _: 1 _: buffer _: buffer size _: gciErrSType.
			result < 0 ifTrue: [self gciError: gciErrSType].
	].
	buffer := buffer copyFrom: 1 to: result.
	class = self oopClassByteArray 			value ifTrue: [gciTsObjInfo data: buffer						] ifFalse: [
	class = self oopClassString 			value ifTrue: [gciTsObjInfo data: buffer asString				] ifFalse: [
	class = self oopClassSymbol 			value ifTrue: [gciTsObjInfo data: buffer asString asSymbol			] ifFalse: [
	class = self oopClassUnicode7 			value ifTrue: [gciTsObjInfo data: (Unicode7 withAll: buffer			)] ifFalse: [
	class = self oopClassUnicode16 			value ifTrue: [gciTsObjInfo data: (Unicode16 withAll: buffer		)] ifFalse: [
	class = self oopClassUnicode32 			value ifTrue: [gciTsObjInfo data: (Unicode32 withAll: buffer		)] ifFalse: [
	class = self oopClassDoubleByteString 	value ifTrue: [gciTsObjInfo data: (DoubleByteString withAll: buffer	)] ifFalse: [
	class = self oopClassQuadByteString 		value ifTrue: [gciTsObjInfo data: (QuadByteString withAll: buffer	)] ifFalse: [
	self error: 'Unrecognized class ' , class printString]]]]]]]].
	^gciTsObjInfo!

session: session fetchObjects: anOop

	| count gciErrSType objects result |
	gciErrSType := self errorStructureClass new.
	count := 100.
	objects := self oopTypeArrayClass new: count.
	result := self GciTsFetchOops: session _: anOop _: 1 _: objects _: count _: gciErrSType.
	result < 0 ifTrue: [self gciError: gciErrSType].
	result == 0 ifTrue: [^#()].
	result == count ifFalse: [
		count := result.
	] ifTrue: [
		count := self session: session fetchSize: anOop.
		objects := self oopTypeArrayClass new: count.
		result := self GciTsFetchOops: session _: anOop _: 1 _: objects _: count _: gciErrSType.
		result < 0 ifTrue: [self gciError: gciErrSType].
	].
	objects := objects copyFrom: 1 to: count.
	^objects!

session: session fetchSize: anOop

	| gciErrSType result |
	gciErrSType := self errorStructureClass new.
	result := self GciTsFetchSize: session _: anOop _: gciErrSType asParameter.
	result < 0 ifTrue: [self gciError: gciErrSType].
	^result!

session: session fetchString: anOop

	| string gciErrSType result |
	gciErrSType := self errorStructureClass new.
	string := String new: 1000.
	result := self GciTsFetchChars: session _: anOop _: 1 _: string _: string size _: gciErrSType.
	result < 0 ifTrue: [self gciError: gciErrSType].
	result == 0 ifTrue: [^''].
	result + 1 < string size ifTrue: [^string copyFrom: 1 to: result].
	string := String new: (self session: session fetchSize: anOop) + 1.
	result := self GciTsFetchChars: session _: anOop _: 1 _: string _: string size _: gciErrSType.
	^string copyFrom: 1 to: result!

session: session fetchVaryingSize: anOop

	| gciErrSType result |
	gciErrSType := self errorStructureClass new.
	result := self GciTsFetchVaryingSize: session _: anOop _: gciErrSType asParameter.
	result < 0 ifTrue: [self gciError: gciErrSType].
	^result!

session: session gemTrace: anInteger

	| oldValue gciErrSType |
	gciErrSType := self errorStructureClass new.
	oldValue := self GciTsGemTrace: session _: anInteger _: gciErrSType asParameter.
	oldValue < 0 ifTrue: [self gciError: gciErrSType].
	^oldValue!

session: session integerForOop: anOop

	| bytes flag gciErrSType  |
	bytes := ByteArray new: 8.
	gciErrSType := self errorStructureClass new.
	flag := self GciTsOopToI64: session _: anOop _: bytes asParameter _: gciErrSType asParameter.
	flag ifFalse: [self gciError: gciErrSType].
	^bytes sqwordAtOffset: 0!

session: session objectExists: anOop

	^self GciTsObjExists: session _: anOop
!

session: session objectNamed: aString

	^self session: session objectNamed: aString inSymbolList: OOP_NIL
!

session: session objectNamed: aString inSymbolList: symbolList

	| oop gciErrSType |
	gciErrSType := self errorStructureClass new.
	oop := self GciTsResolveSymbol: session _: aString asParameter _: symbolList _: gciErrSType asParameter.
	oop == OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^oop
!

session: session objectNamedOop: stringOop inSymbolList: symbolList

	| oop gciErrSType |
	gciErrSType := self errorStructureClass new.
	oop := self GciTsResolveSymbolObj: session _: stringOop _: symbolList _: gciErrSType asParameter.
	oop == OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^oop
!

session: session oopForDouble: aFloat

	| oop gciErrSType |
	gciErrSType := self errorStructureClass new.
	oop := self GciTsDoubleToOop: session _: aFloat _: gciErrSType asParameter.
	oop == OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^oop!

session: session oopForInteger: anInteger

	| oop gciErrSType |
	gciErrSType := self errorStructureClass new.
	oop := self GciTsI64ToOop: session _: anInteger asParameter _: gciErrSType asParameter.
	oop == OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^oop
!

session: session oopForString: aString

	| oop gciErrSType |
	gciErrSType := self errorStructureClass new.
	oop := self GciTsNewString: session _: aString asParameter _: gciErrSType asParameter.
	oop == OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
	^oop
!

session: session releaseOops: anOopType64Array

	| didRelease gciErrSType |
	gciErrSType := self errorStructureClass new.
	didRelease := self GciTsReleaseObjs: session _: anOopType64Array _: anOopType64Array size _: gciErrSType asParameter.
	didRelease ifFalse: [self gciError: gciErrSType].
!

session: session send: stringOrOop to: receiverOop

	^self session: session send: stringOrOop to: receiverOop with: self oopTypeArrayClass new!

session: session send: stringOrOop to: receiverOop with: anOopType64Array

	| argumentsEx result string selectorOop gciErrSType |
	gciErrSType := self errorStructureClass new.
	argumentsEx := ExternalMemory fromString: anOopType64Array bytes asString.
	(stringOrOop isKindOf: String) ifTrue: [
		string := ExternalMemory fromString: stringOrOop.
		selectorOop := OOP_ILLEGAL.
	] ifFalse: [
		string := OOP_NIL.
		selectorOop := stringOrOop.
	].
	result := self 
		GciTsPerform: session 
		_: receiverOop
		_: selectorOop 
		_: string asParameter 
		_: argumentsEx asParameter
		_: anOopType64Array size 
		_: 1 "GCI_PERFORM_FLAG_ENABLE_DEBUG"
		_: 0 "environment" 
		_: gciErrSType asParameter.
	result = OOP_ILLEGAL ifTrue: [self gciError: gciErrSType].
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
	resultCode := self GciTsSessionIsRemote: session.
	resultCode == -1 ifTrue: [self error: 'invalid session'].
	^resultCode == 1!

softBreakSession: session

	self session: session breakHard: false.!

version

	| productCode string |
	string := String new: 1024.
	productCode := self GciTsVersion: string _: string size.
	productCode == 3 ifFalse: [self error: 'Unexpected product code (' , productCode printString , ')'].
	string := string copyFrom: 1 to: (string indexOf: (Character codePoint: 0)) - 1.
	^string! !
!GciThreadSafeLibrary categoriesFor: #abortSession:!public!required! !
!GciThreadSafeLibrary categoriesFor: #beginSession:!public!required! !
!GciThreadSafeLibrary categoriesFor: #characterForOop:!public! !
!GciThreadSafeLibrary categoriesFor: #classForSpecial:!public! !
!GciThreadSafeLibrary categoriesFor: #commitSession:!public!required! !
!GciThreadSafeLibrary categoriesFor: #fetchObjImpl:!private!subclassResponsibility! !
!GciThreadSafeLibrary categoriesFor: #gciError:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsAbort:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsBegin:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsBreak:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsCharToOop:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsClearStack:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsCommit:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsContinueWith:_:_:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsDoubleToOop:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsDoubleToSmallDouble:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsExecute:_:_:_:_:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsFetchBytes:_:_:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsFetchChars:_:_:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsFetchClass:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsFetchObjInfo:_:_:_:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsFetchOops:_:_:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsFetchSize:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsFetchSpecialClass:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsFetchVaryingSize:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsGemTrace:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsI64ToOop:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsLogin:_:_:_:_:_:_:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsLogout:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsNewString:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsObjExists:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsOopIsSpecial:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsOopToChar:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsOopToDouble:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsOopToI64:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsPerform:_:_:_:_:_:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsReleaseAllObjs:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsReleaseObjs:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsResolveSymbol:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsResolveSymbolObj:_:_:_:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsSessionIsRemote:!private! !
!GciThreadSafeLibrary categoriesFor: #GciTsVersion:_:!private! !
!GciThreadSafeLibrary categoriesFor: #hardBreakSession:!public!required! !
!GciThreadSafeLibrary categoriesFor: #initialize!private! !
!GciThreadSafeLibrary categoriesFor: #loginHostUser:hostPassword:gsUser:gsPassword:gemNRS:stoneNRS:!public!required! !
!GciThreadSafeLibrary categoriesFor: #loginToStone:hostUser:hostPassword:hostPasswordIsEncrypted:gemNRS:gsUser:gsPassword:loginFlags:haltOnErrorNumber:!public!threaded! !
!GciThreadSafeLibrary categoriesFor: #loginUser:password:gemNRS:stoneNRS:!public! !
!GciThreadSafeLibrary categoriesFor: #logoutSession:!public!required! !
!GciThreadSafeLibrary categoriesFor: #oopAsciiNul!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassArray!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassByteArray!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassCharacter!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassDoubleByteString!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassQuadByteString!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassSmallDouble!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassSmallFraction!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassSmallInteger!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassString!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassSymbol!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassSystem!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassUnicode16!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassUnicode32!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopClassUnicode7!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopFalse!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopForCharacter:!public! !
!GciThreadSafeLibrary categoriesFor: #oopForSmallDouble:!public! !
!GciThreadSafeLibrary categoriesFor: #oopGemStoneError!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopIllegal!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopIsSpecial:!public! !
!GciThreadSafeLibrary categoriesFor: #oopMaxSmallInteger!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopMinSmallInteger!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopMinusOne!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopNil!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopOne!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopRemoteNil!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopTrue!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopTwo!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopTypeArrayClass!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #oopTypeClass!public! !
!GciThreadSafeLibrary categoriesFor: #oopTypeWithOop:!public! !
!GciThreadSafeLibrary categoriesFor: #oopZero!public!Reserved OOPs! !
!GciThreadSafeLibrary categoriesFor: #releaseAllObjectsInSession:!public!required! !
!GciThreadSafeLibrary categoriesFor: #session:breakHard:!public! !
!GciThreadSafeLibrary categoriesFor: #session:clearStack:!public!required! !
!GciThreadSafeLibrary categoriesFor: #session:continue:with:error:!public! !
!GciThreadSafeLibrary categoriesFor: #session:continue:with:error:flags:!public!threaded! !
!GciThreadSafeLibrary categoriesFor: #session:continue:withError:!public! !
!GciThreadSafeLibrary categoriesFor: #session:continue:withObject:!public!required! !
!GciThreadSafeLibrary categoriesFor: #session:doubleForOop:!public! !
!GciThreadSafeLibrary categoriesFor: #session:execute:!public! !
!GciThreadSafeLibrary categoriesFor: #session:execute:context:!public!required! !
!GciThreadSafeLibrary categoriesFor: #session:execute:context:symbolList:!public!threaded! !
!GciThreadSafeLibrary categoriesFor: #session:fetchBytes:!public!required! !
!GciThreadSafeLibrary categoriesFor: #session:fetchClass:!public! !
!GciThreadSafeLibrary categoriesFor: #session:fetchObject:!public! !
!GciThreadSafeLibrary categoriesFor: #session:fetchObjects:!public!required! !
!GciThreadSafeLibrary categoriesFor: #session:fetchSize:!public! !
!GciThreadSafeLibrary categoriesFor: #session:fetchString:!public! !
!GciThreadSafeLibrary categoriesFor: #session:fetchVaryingSize:!public! !
!GciThreadSafeLibrary categoriesFor: #session:gemTrace:!public! !
!GciThreadSafeLibrary categoriesFor: #session:integerForOop:!public! !
!GciThreadSafeLibrary categoriesFor: #session:objectExists:!public! !
!GciThreadSafeLibrary categoriesFor: #session:objectNamed:!public! !
!GciThreadSafeLibrary categoriesFor: #session:objectNamed:inSymbolList:!public! !
!GciThreadSafeLibrary categoriesFor: #session:objectNamedOop:inSymbolList:!public! !
!GciThreadSafeLibrary categoriesFor: #session:oopForDouble:!public! !
!GciThreadSafeLibrary categoriesFor: #session:oopForInteger:!public! !
!GciThreadSafeLibrary categoriesFor: #session:oopForString:!public!required! !
!GciThreadSafeLibrary categoriesFor: #session:releaseOops:!public!required! !
!GciThreadSafeLibrary categoriesFor: #session:send:to:!public! !
!GciThreadSafeLibrary categoriesFor: #session:send:to:with:!public!required!threaded! !
!GciThreadSafeLibrary categoriesFor: #session:valueOfOop:!public! !
!GciThreadSafeLibrary categoriesFor: #sessionIsRemote:!public! !
!GciThreadSafeLibrary categoriesFor: #softBreakSession:!public!required! !
!GciThreadSafeLibrary categoriesFor: #version!public! !

