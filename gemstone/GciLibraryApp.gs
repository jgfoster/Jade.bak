! ------------------- Class definition for GciLibraryApp
expectvalue /Class
doit
WebApp subclass: 'GciLibraryApp'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: WebGS
  options: #()

%
expectvalue /Class
doit
GciLibraryApp comment:
'No class-specific documentation for GciLibraryApp, hierarchy is:
Object
  WebApp( begin end exception html request response)
    GciLibraryApp( main)
'
%
expectvalue /Class
doit
GciLibraryApp category: 'User Interface'
%
! ------------------- Remove existing behavior from GciLibraryApp
expectvalue /Metaclass3
doit
GciLibraryApp removeAllMethods.
GciLibraryApp class removeAllMethods.
%
! ------------------- Class methods for GciLibraryApp
set compile_env: 0
category: 'required'
classmethod: GciLibraryApp
workerCount
	"Do everything in one Gem"
	^0
%
set compile_env: 0
category: 'startup'
classmethod: GciLibraryApp
httpServerClass

	^HttpsServer
%
! ------------------- Instance methods for GciLibraryApp
set compile_env: 0
category: 'REST API'
method: GciLibraryApp
library

	^SessionTemps current
		at: #'library'
		ifAbsentPut: [
			GciLibrary new
				GciInit;
				GciSetNetEx_: System stoneName
					_: nil
					_: nil
					_: '!tcp@localhost#netldi:gs64ldi#task!gemnetobject'
					_: 0;
				yourself]
%
method: GciLibraryApp
setSessionFromArgs: args

	(args at: 'session' ifAbsent: [nil]) ifNotNil: [:session |
		self library GciSetSessionId_: session.
	].
%
method: GciLibraryApp
abort_gs: args

	self setSessionFromArgs: args.
	self library GciAbort.
	^Dictionary new
%
method: GciLibraryApp
begin_gs: args

	self setSessionFromArgs: args.
	self library GciBegin.
	^Dictionary new
%
method: GciLibraryApp
callInProgress_gs: args

	self setSessionFromArgs: args.
	^Dictionary new
		at: 'result' put: self library GciCallInProgress == 1;
		yourself
%
method: GciLibraryApp
commit_gs: args

	self setSessionFromArgs: args.
	^Dictionary new
		at: 'result' put: self library GciCommit == 1;
		yourself
%
method: GciLibraryApp
getError_gs: args

	| dict flag gciError |
	gciError := GciErrSType new.
	dict := Dictionary new.
	(self library GciErr_: gciError) == 0 ifTrue: [^dict].
	^dict
		at: 'category' put: gciError category;
		at: 'context' put: gciError context;
		at: 'exceptionObj' put: gciError exceptionObj;
		at: 'args' put: gciError args;
		at: 'number' put: gciError number;
		at: 'argCount' put: gciError argCount;
		at: 'fatal' put: gciError fatal;
		at: 'message' put: gciError message;
		at: 'reason' put: gciError reason;
		yourself.
%
method: GciLibraryApp
getSessionId_gs: args

	^Dictionary new
		at: 'result' put: (self library GciGetSessionId);
		yourself.
%
method: GciLibraryApp
hardBreak_gs: args

	self setSessionFromArgs: args.
	self library GciHardBreak.
	^Dictionary new
%
method: GciLibraryApp
login_gs: args

	| result |
	result := self library
		GciLoginEx_: (args at: 'username' ifAbsent: ['username'])
		_: (args at: 'password' ifAbsent: ['password'])
		_: 0	"flags"
		_: 0.	"haltOnErrNum"
	result == 1 ifTrue: [
		^Dictionary new
			at: 'result' put: self library GciGetSessionId;
			yourself
	] ifFalse: [
		^self getError_gs: nil
	].
%
method: GciLibraryApp
logout_gs: args

	GsFile stdoutServer nextPutAll: 'logout - ' , args printString; cr.
	self setSessionFromArgs: args.
	self library GciLogout.
	^Dictionary new
%
method: GciLibraryApp
nbEnd_gs: args

	| progress result |
	result := CByteArray gcMalloc: 8.
	self setSessionFromArgs: args.
	progress := self library GciNbEnd__: result.
	^Dictionary new
		at: 'progress' put: progress;
		at: 'result' put: (result int64At: 0);
		yourself
%
method: GciLibraryApp
nbExecuteStr_gs: args

	self library
		GciNbExecuteStr_: (args at: 'source' ifAbsent: [''])
		_: 16r14. 	"OOP_NIL"
	^Dictionary new
%
method: GciLibraryApp
softBreak_gs: args

	self setSessionFromArgs: args.
	self library GciSoftBreak.
	^Dictionary new
%
method: GciLibraryApp
version_gs: args

	^Dictionary new
		at: 'result' put: self library GciVersion;
		yourself.
%
