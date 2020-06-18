"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GciSession
	instanceVariableNames: 'briefDescription clientForwarders eventCount gciSessionId gemHost gemNRS heartbeatProcess isAutoCommit isAutoMigrate isHandlingClientForwarderSend isNativeCode isPackagePolicyEnabled isShowUnimplementedMessages library netPort netTask server serverClass semaphore stoneHost stoneName stoneNRS stoneSerial stoneSessionID userID'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciSession guid: (GUID fromString: '{c52a3e3c-b7f3-4c3c-ab3d-a925ecb8b114}')!
GciSession comment: 'Permitted calls from within a non-blocking call:

GciCallInProgress
GciErr
GciGetSessionId
GciHardBreak
GciNbEnd
GciSetSessionId
GciShutdown
GciSoftBreak'!
!GciSession categoriesForClass!Unclassified! !
!GciSession methodsFor!

_executeString: aString fromContextOop: anOopType environment: anInteger

	^library 
		session: gciSessionId
		execute: aString
		context: anOopType
		environment: anInteger
!

_send: aSymbol to: anObject withAll: anArray

	| arguments oops result |
	oops := OrderedCollection new.
	arguments := anArray collect: [:each | 
		(each isKindOf: Integer) ifTrue: [library session: gciSessionId oopForInteger: each] ifFalse: [
		(each isKindOf: String) ifTrue: [oops add: (self oopForString: each)] ifFalse: [
		each]].
	].
	[
		[
			result := library 
				session: gciSessionId
				send: aSymbol 
				to: (self oopTypeFor: anObject) 
				with: (self serverArrayFor: arguments).
		] on: GsRuntimeError do: [:ex | 
			ex errorReport number == 2106 ifTrue: [
				Error signal: oops printString , '; ' , ex description.
			] ifFalse: [
				ex pass.
			].
		].
	] ensure: [
		self releaseOops: oops.
	].
	^result.
!

_server

	^server.
!

abort

	self serverPerform: #'abort'.
!

begin

	self serverPerform: #'beginTransaction'.
!

beInvalidSession

	gciSessionId := nil.
	library := nil.
!

briefDescription

	briefDescription ifNil: [
		| stream |
		stream := WriteStream on: String new.
		stream 
			nextPutAll: userID;
			nextPutAll: ' as session ';
			yourself.
		stoneSessionID printOn: stream.
		stream 
			nextPutAll: ' in ';
			nextPutAll: stoneName;
			nextPutAll: ' on ';
			nextPutAll: stoneHost;
			yourself.
		self isRemoteGem ifTrue: [
			stream
				nextPutAll: ' with remote gem on ';
				nextPutAll: gemHost;
				yourself.
		] ifFalse: [
			stream nextPutAll: ' with linked gem'. 
		].
		briefDescription := stream contents.
	].
	^briefDescription.
!

clearStack: anOopType

	library
		session: gciSessionId
		clearStack: anOopType 
!

clientForwardError: gciErrSType

	[
		| receiver selector args |
		isHandlingClientForwarderSend := true.
		receiver := self valueOfOop: (gciErrSType args at: 2).
		selector := self valueOfOop: (gciErrSType args at: 3).
		args := self valueOfArrayOop: (gciErrSType args at: 4).
		(receiver isNil or: [self clientForwarders size < receiver]) ifTrue: [^library oopNil].
		receiver := self clientForwarders at: receiver.
		SessionManager inputState pumpMessages.	"This seems to be necessary to allow #withExplanation:doA: to properly keep in synch."
		^receiver
			perform: selector
			withArguments: args.
	] ensure: [
		isHandlingClientForwarderSend := false.
	].
!

clientForwarders

	clientForwarders isNil ifTrue: [clientForwarders := OrderedCollection with: self].
	^clientForwarders.
!

commit

	^self serverPerform: #'commit'.
!

debugString: aString fromContext: anObject environment: anInteger
	"https://github.com/jgfoster/Jade/issues/122"

	^self
		serverPerformInterpreted: #'debugString:fromContext:environment:' 
		with: (self encode: aString)
		with: anObject 
		with: anInteger.
!

debugToFilePath: aString

	^library gciDbgEstablishToFile: aString
!

decode: aString
	"asAnsiString before 3.3.0; asUtf8String from 3.3.0 on."

	library ifNil: [GsInvalidSessionError signal].
	^library decode: aString!

encode: aString
	"asAnsiString before 3.3.0; asUtf8String from 3.3.0 on."

	library ifNil: [^GsInvalidSessionError signal].
	^library encode: aString!

eventCount

	^eventCount.
!

executeString: aString

	^self
		executeString: aString
		fromContext: nil
		environment: 0.
!

executeString: aString fromContext: anObject environment: anInteger

	^self
		withExplanation: aString 
		do: [
			self
				_executeString: aString
				fromContextOop: (self oopTypeFor: anObject)
				environment: anInteger.
		].
!

fetchBytes: anOopType

	^library 
		session: gciSessionId
		fetchBytes: anOopType 
!

forceLogout

	self
		stopHeartbeat;
		hardBreak;
		logout;
		yourself.

!

gciSessionId

	^gciSessionId
!

gciSessionId: aGciSessionId

	gciSessionId := aGciSessionId.
!

gciVersion

	library ifNil: [^nil].
	^library version.
!

gemHost
	^gemHost!

gsHasPragmas

	^library gsHasPragmas!

handlingClientForwarderSendDo: aBlock

	| block result continueWith |
	block := aBlock.
	[
		result := self returningResultOrErrorDo: block.
		result isKindOf: GsClientForwarderSend.
	] whileTrue: [
		continueWith := self oopTypeFor: result signal.
		block := [
			library
				session: gciSessionId
				continue: result errorReport contextOop
				withObject: continueWith.
		].
	].
	^result.
!

hardBreak

	library ifNil: [^self].
	library hardBreakSession: gciSessionId.
!

hasHeartbeat

	^heartbeatProcess notNil!

hasServer

	^server notNil.
!

heartbeat: receiver arguments: arguments

	| result |
	(Delay forSeconds: 5) wait.
	self isValidSession ifFalse: [^self].
	result := self returningResultOrErrorDo: [
		library 
			session: gciSessionId
			send: 'heartbeat' 
			to: receiver
			with: arguments
	].
	[
		result isKindOf: GciError.
	] whileTrue: [
		| error |
		error := result.
		result := GsError 
			signalGCI: self
			gciErrSType: error tag.
		result = #'resume' ifFalse: [self error: 'Unrecognized response of ' , result printString].
		result := self returningResultOrErrorDo: [
			library
				session: gciSessionId
				continue: error tag contextOop.
		].
	].
	(result isKindOf: Error) ifTrue: [^self].	"We are just going to ignore any errors!!"
	0 < result ifTrue: [self idleWarning].

!

idleWarning

	| response |
	response := ProgressDialog
		showModalWhile: [:progress | 
			1 to: 300 do: [:i | 
				progress 
					caption: 'Idle Session Warning';
					value: i / 3; 
					text: 'To avoid an excessive commit record backlog, ' , 
						'this session will abort in ' , (300 - i) displayString, ' seconds...';
					yourself.
				Processor sleep: 1000.
			].
			true.
		].
	response ifNil: [		"cancel"
		(Delay forSeconds: 60) wait.
	] ifNotNil: [		"timeo0ut"
		self abort.
		[MessageBox warning: 'To avoid an excessive commit record backlog your session has been aborted.' caption: 'Idle session lost data!!'] fork.
	].
!

incrementEventCount
	"Without a GC, we get a very strange stack corruption!!"

	(eventCount := eventCount + 1) \\ 1000 == 0 ifTrue: [MemoryManager current collectGarbage; compact].

"
MemoryManager current collectGarbage; compact.
1 to: 10000 do: [:i |
	| semaphore |
	semaphore := Semaphore new.
	[
		(Delay forMilliseconds: 1) wait.
		semaphore signal.
	] fork.
	semaphore wait.
	i \\ 1000 == 0 ifTrue: [MemoryManager current collectGarbage; compact].
].
"
!

indexOfClientForwarder: anObject
	"Each server ClientForwarder instance has a 'clientObject' instance variable that is a SmallInteger 
	representing the receiver of the server's message. Some hard-coded values:
		1 - self (to handle OBConfirmationRequest)
		2 - Transcript
		3 - GsProfStef
	"

	^self clientForwarders 
		indexOf: anObject
		ifAbsent: [clientForwarders add: anObject. clientForwarders size].
!

initializeLibrary: libraryClass 
stoneNRS: stoneString 
gemNRS: gemString 
userID: gsUserID 
password: gsPassword 
hostUserID: hostUserID 
password: hostPassword 
initials: initials
debugPath: debugPath

	| list |
	semaphore := Semaphore forMutualExclusion.
	isAutoCommit := false.
	isAutoMigrate := false.
	isHandlingClientForwarderSend := false.
	isNativeCode := true.
	isShowUnimplementedMessages := true.
	eventCount := 0.
	stoneNRS := stoneString.
	gemNRS := gemString.
	userID := gsUserID.
	list := stoneNRS subStrings: $!!.
	stoneName := list at: 3.
	list := (list at: 2) subStrings: $#.
	list := (list at: 1) subStrings: $@.
	stoneHost := list at: 2.
	self isLinkedGem ifFalse: [
		list := gemNRS subStrings: $#.
		netPort := ((list at: 2) subStrings: $:) at: 2.
		netTask := ((list at: 3) subStrings: $!!) at: 2.
		gemHost := ((list at: 1) subStrings: $@) at: 2.
	].
	self 
		loadLibrary: libraryClass debugPath: debugPath;
		gciSessionId: (library
			loginHostUser: hostUserID 
			hostPassword: hostPassword 
			gsUser: gsUserID 
			gsPassword: gsPassword 
			gemNRS: gemNRS 
			stoneNRS: stoneNRS) ;
		postLoginAs: initials;
		yourself.
!

initializeServer
"
	GciSession allInstances first initializeServer.
"
	| crlf jadeServerClassOop |
	crlf := Character cr asString , Character lf asString.
	library ifNil: [^self].
	[
		jadeServerClassOop := library session: gciSessionId resolveSymbol: serverClass name.
		jadeServerClassOop isOopIllegal ifFalse: [
			| version |
			server := self send: #'new' to: jadeServerClassOop.
			self send: #'initialize' to: server.
			version := self send: #'version' to: server.
			version = self jadeSessionManagerClass version ifTrue: [^self].
			(MessageBox confirm: 'Use built-in JadeServer version ' , version , '?') ifTrue: [^self].
		].
	] on: Error do: [:ex | 
		MessageBox notify: 'Unable to use built-in JadeServer due to error:' , crlf , ex description.
	].
	[
		| string |
		string := library encode: serverClass gsString.
		server := self executeString: string.
	] on: GsRuntimeError do: [:ex | 
		| number notes |
		number := ex errorReport number.
		notes := ''.
		number = 2151 ifTrue: [notes := ' (code modification?)'].
		self logout.
		GciLoginFailed
			signal: 'JadeServer could not be installed:
' , ex description , notes
			with: self.
	].

	((server isKindOf: String) or: [server isKindOf: ByteArray]) ifTrue: [
		self logout.
		GciLoginFailed
			signal: 'JadeServer compile error:
' , server asAnsiString
			with: self.
	].
	1 ~~ 1 ifTrue: [		"messasges that are sent by a ClientForwarderSend should not be stripped"
		self 
			signalConfirmationRequestUsing: nil;
			signalInformRequestUsing: nil;
			signalMultiLineTextRequestUsing: nil;
			signalTextRequestUsing: nil;
			message: 'messageString' caption: 'captionString' icon: 'alertSymbol' buttons: 'buttonsSymbol';
			prompt: 'promptString' caption: 'captionString';
			yourself.
	].
!

isAutoCommit

	^isAutoCommit
!

isAutoCommit: aBoolean

	isAutoCommit == aBoolean ifTrue: [^self].
	self error: 'Auto-commit is not yet supported (see https://github.com/jgfoster/Jade/issues/8)'.
	isAutoCommit := aBoolean.
!

isAutoMigrate

	^isAutoMigrate
!

isAutoMigrate: aBoolean

	isAutoMigrate == aBoolean ifTrue: [^self].
	self executeString: 'McPlatformSupport autoMigrate: ' , aBoolean printString.
	isAutoMigrate := true.
!

isLinkedGem

	^gemNRS isEmpty.
!

isNativeCode

	^isNativeCode!

isNativeCode: aBoolean

	isNativeCode == aBoolean ifTrue: [^self].
	self executeString: 'System configurationAt: #GemNativeCodeEnabled put: ' , aBoolean printString.
	isNativeCode := aBoolean.
!

isOopType: anObject

	^anObject isKindOf: library oopTypeClass.
!

isPackagePolicyEnabled

	^isPackagePolicyEnabled!

isRemoteGem

	^self isLinkedGem not.
!

isShowUnimplementedMessages

	^isShowUnimplementedMessages
!

isShowUnimplementedMessages: aBoolean

	isShowUnimplementedMessages == aBoolean ifTrue: [^self].
	self serverPerform: #'showUnimplementedMessages:' with: aBoolean.
	isShowUnimplementedMessages := aBoolean.
!

isValidSession

	^gciSessionId notNil
!

libraryVersion

	^library 
		ifNil: [nil]
		ifNotNil: [library class displayName].
!

loadLibrary: aClass debugPath: debugPath

	| others |
	others := self class allInstances collect: [:each | each libraryVersion].
	others := others asSet reject: [:each | each = aClass displayName].
	others isEmpty ifFalse: [self error: 'Unable to login to another version!!'].
	library := aClass open: aClass fileName.
	serverClass := library jadeServerClass.
	debugPath notEmpty ifTrue: [
		(self debugToFilePath: debugPath) ifFalse: [self error: 'Unable to open ' , debugPath printString , ' for GCI debugging'].
		library gciGemTrace: 3.
	].!

logout

	self stopHeartbeat.
	self trigger: #'logoutPending'.
	library ifNotNil: [
		library logoutSession: gciSessionId.
		library := nil.
	].
	self trigger: #'logout'.
	gciSessionId := nil.
!

logoutRequested

	| valueHolder |
	valueHolder := Association key: self value: true.
	self trigger: #'logoutRequested:' with: valueHolder.
	^valueHolder value.
!

message: messageString caption: captionString icon: alertSymbol buttons: buttonsSymbol

	^MessageBox new
		text: messageString;
		caption: captionString;
		iconStyle: alertSymbol asSymbol;
		buttonStyle: buttonsSymbol asSymbol;
		open
!

netPort
	^netPort!

netTask
	^netTask!

oopForString: aString

	| log oop |
	aString isNil ifTrue: [^library oopNil].
	oop := library session: gciSessionId oopForString: aString.
	log := 'Allocated oop ' , oop printString , ' for ' , (aString copyFrom: 1 to: (aString size min: 40)) printString.
	log := log copyReplacing: Character lf withObject: $\.
	JadeLog log: log.
	^oop!

oopGemStoneError

	^library oopGemStoneError.
!

oopIllegal

	^library oopIllegal.
!

oopTypeFor: anObject 

	anObject isNil 					ifTrue: [^library oopNil].
	(anObject isKindOf: ExternalInteger) 	ifTrue: [^anObject].
	(anObject isKindOf: Boolean) 		ifTrue: [^anObject ifTrue: [library oopTrue] ifFalse: [library oopFalse]].
	(anObject isKindOf: SmallInteger) 	ifTrue: [^library session: gciSessionId oopForInteger: anObject].
	(anObject isKindOf: GsObject) 		ifTrue: [^anObject oopType].
	(anObject isKindOf: String)			ifTrue: [^self oopForString: anObject].
	MessageBox notify: 'Sorry, we are not yet prepared to convert ' , anObject printString , ' to an OOP!!'.
	SessionManager current pause.
!

oopTypeWithOop: anInteger

	^library oopTypeWithOop: anInteger.
!

postLoginAs: aString 

	self 
		initializeServer;
		setInitials: aString;		"This gets back the session and serial number and some preferences"
		startHeartbeat;
		yourself.!

printString: anOopType

	^self
		serverPerform: 'printStringOf:'
		with: anOopType.
!

prompt: promptString caption: captionString

	^Prompter 
		prompt: promptString
		caption: captionString
!

releaseOop: anOopType

	self releaseOops: (Array with: anOopType).
!

releaseOops: anArray

	library ifNil: [^self].
	anArray isEmpty ifTrue: [^self].
	library session: gciSessionId releaseOops: anArray.
!

returningResultOrErrorDo: aBlock
	"Here we isolate the error handling so we can unwind the stack.
	We should never return/resume the exception, but instead make
	a new call such as GciContinue()"

	^[
		aBlock value.
	] on: GciError do: [:ex | 
		(SessionManager current class == DevelopmentSessionManager and: [ex tag number == 2106]) ifTrue: [self halt].
		ex return: (GsError forSession: self gciErrSType: ex tag).
	].
!

send: aSymbol to: anObject

	^self
		send: aSymbol
		to: anObject
		withAll: #().
!

send: aSymbol to: anObject withAll: anArray

	| stream |
	stream := WriteStream on: String new.
	stream 
		nextPutAll: (anObject == server ifTrue: ['jadeServer'] ifFalse: [anObject printString]); cr;
		nextPutAll: '	perform: #''';
		nextPutAll: aSymbol;
		nextPut: $'; cr;
		nextPutAll: '	withAll: (Array';
		yourself.
	anArray do: [:each | 
		stream cr; nextPutAll: '		with: '; print: each.
	].
	stream nextPut: $).
	^self
		withExplanation: stream contents 
		do: [
			semaphore critical: [
				self
					_send: aSymbol 
					to: anObject 
					withAll: anArray.
			].
		].
!

sendInterpreted: aSymbol to: anObject withAll: anArray

	| stream arguments |
	stream := WriteStream on: String new.
	stream 
		nextPutAll: (anObject == server ifTrue: ['jadeServer'] ifFalse: [anObject printString]); cr;
		nextPutAll: '	performInterpreted: #''';
		nextPutAll: aSymbol;
		nextPut: $'; cr;
		nextPutAll: '	withAll: (Array';
		yourself.
	anArray do: [:each | 
		stream cr; nextPutAll: '		with: '; print: each.
	].
	stream nextPut: $).
	arguments := self serverArrayFor: anArray.
	^self
		withExplanation: stream contents 
		do: [
			library 
				sendInterpreted: aSymbol 
				to: (self oopTypeFor: anObject) 
				with: arguments
				session: gciSessionId.
		].
!

serverArrayFor: anArray

	| array |
	library ifNil: [GsInvalidSessionError signal].
	array := library oopTypeArrayClass new: anArray size.
	1 to: anArray size do: [:i | 
		array at: i put: (self oopTypeFor: (anArray at: i)).
	].
	^array.
!

serverClass

	^serverClass
!

serverPerform: aSymbol

	^self
		send: aSymbol
		to: server.
!

serverPerform: aSymbol with: anObject

	^self
		serverPerform: aSymbol 
		withArguments: (Array with: anObject).
!

serverPerform: aSymbol with: arg1 with: arg2

	^self
		serverPerform: aSymbol 
		withArguments: (Array
			with: arg1
			with: arg2).
!

serverPerform: aSymbol with: arg1 with: arg2 with: arg3

	^self
		serverPerform: aSymbol 
		withArguments: (Array
			with: arg1
			with: arg2
			with: arg3).
!

serverPerform: aSymbol with: arg1 with: arg2 with: arg3 with: arg4

	^self
		serverPerform: aSymbol 
		withArguments: (Array
			with: arg1
			with: arg2
			with: arg3
			with: arg4).
!

serverPerform: aSymbol withArguments: anArray

	| answer |
	server isNil ifTrue: [
		MessageBox notify: 'Unable to perform action due to server initialization failure.'.
		^self.
	].
	answer := self
		send: aSymbol
		to: server
		withAll: anArray.
	^answer.
!

serverPerformInterpreted: aSymbol with: arg1 with: arg2

	^self
		serverPerformInterpreted: aSymbol 
		withArguments: (Array
			with: arg1
			with: arg2).
!

serverPerformInterpreted: aSymbol with: arg1 with: arg2 with: arg3

	^self
		serverPerformInterpreted: aSymbol 
		withArguments: (Array
			with: arg1
			with: arg2
			with: arg3).
!

serverPerformInterpreted: aSymbol withArguments: anArray

	| answer |
	server isNil ifTrue: [
		MessageBox notify: 'Unable to perform action due to server initialization failure.'.
		^self.
	].
	answer := self
		sendInterpreted: aSymbol
		to: server
		withAll: anArray.
	^answer.
!

setInitials: initials

	| string |
	server isNil ifTrue: [^self].
	string := self
		serverPerform: #'mcInitials:' 
		with: (self encode: initials).
	self updatePreferencesFrom: string.
!

signalConfirmationRequestUsing: anOopType64 

	| string pieces answer |
	string := self 
		serverPerform: #'obConfirmationRequest:' 
		with: anOopType64.
	pieces := string subStrings: Character lf.
	string := (pieces at: 3) , ' (Yes = ' , (pieces at: 2) , '; No = ' , (pieces at: 1) , ')'.
	answer := MessageBox
		confirm: string 
		caption: 'Server Confirmation Request'.
	^answer ifTrue: [library oopTrue] ifFalse: [library oopFalse].
!

signalInformRequestUsing: anOopType64 

	| string |
	string := self 
		serverPerform: #'obInformRequest:' 
		with: anOopType64.
	MessageBox
		notify: string 
		caption: 'Server Inform Request'.
	^library oopNil
!

signalMultiLineTextRequestUsing: anOopType64 

	| string stream size prompt template answer oop |
	string := self 
		serverPerform: #'obTextRequest:' 
		with: anOopType64.
	stream := ReadStream on: string.
	size := stream nextLine asNumber.
	prompt := stream next: size.
	template := stream upToEnd.
	answer := Prompter
		on: template 
		prompt: prompt 
		caption: 'Server Text Request'.
	answer ifNil: [^library oopNil].
	oop := self oopForString: answer.
	[
		self releaseOop: oop.
	] forkAt: Processor userBackgroundPriority.
	^oop.
!

signalTextRequestUsing: anOopType64 

	| string stream size prompt template answer oop |
	string := self 
		serverPerform: #'obTextRequest:' 
		with: anOopType64.
	stream := ReadStream on: string.
	size := stream nextLine asNumber.
	prompt := stream next: size.
	template := stream upToEnd.
	answer := Prompter
		on: template 
		prompt: prompt 
		caption: 'Server Text Request'.
	answer ifNil: [^library oopNil].
	oop := self oopForString: answer.
	[
		self releaseOop: oop.
	] forkAt: Processor userBackgroundPriority.
	^oop.
!

softBreak

	library softBreakSession: gciSessionId.
!

startHeartbeat
		"Private - Every second execute something inexpensive on the server (a Delay) 
		so that background processes on the server get a chance to run."

	heartbeatProcess := [
		| receiver arguments |
		receiver := self oopTypeFor: server.
		arguments := self serverArrayFor: #().
		[self isValidSession] whileTrue: [self heartbeat: receiver arguments: arguments].
		heartbeatProcess := nil.
	] forkAt: Processor userBackgroundPriority.
!

stepInto: aGsProcess inFrame: anInteger

	self
		serverPerformInterpreted: #'step:inFrame:'
		with: aGsProcess
		with: 0.
!

stepOut: aGsProcess inFrame: anInteger

	self
		serverPerformInterpreted: #'step:inFrame:'
		with: aGsProcess
		with: anInteger.
!

stepOver: aGsProcess inFrame: anInteger

	self
		serverPerformInterpreted: #'step:inFrame:'
		with: aGsProcess
		with: anInteger.
!

stoneHost
	^stoneHost!

stoneName
	^stoneName!

stoneSerial

	^stoneSerial.
!

stoneSessionID

	^stoneSessionID.
!

stopHeartbeat

	heartbeatProcess notNil ifTrue: [
		heartbeatProcess terminate.
		heartbeatProcess := nil.
	].
!

terminate: anOopType

	self serverPerform: #'terminate:' with: anOopType.
	TerminateProcess signal.!

titleBarFor: aString
"'Jade ' , gciSession gciSessionID printString , ' (' , gciSession userID , ') - "
	| stream list |
	list := stoneNRS subStrings: $!!.
	stoneName := list at: 3.
	list := (list at: 2) subStrings: $#.
	list := (list at: 1) subStrings: $@.
	stoneHost := list at: 2.
	stream := WriteStream on: String new.
	stream 
		nextPutAll: 'Jade ';
		print: gciSessionId;
		nextPutAll: ' (';
		nextPutAll: userID asAnsiString;
		nextPutAll: ') - ';
		nextPutAll: aString asAnsiString;
		nextPutAll: ' on ';
		nextPutAll: stoneName asAnsiString;
		nextPutAll: ' session ';
		print: stoneSessionID;
		nextPutAll: ' on ';
		nextPutAll: stoneHost asAnsiString;
		yourself.
	self isRemoteGem ifTrue: [
		list := gemNRS subStrings: $#.
		netPort := ((list at: 2) subStrings: $:) at: 2.
		netTask := ((list at: 3) subStrings: $!!) at: 2.
		gemHost := ((list at: 1) subStrings: $@) at: 2.
		stream
			nextPutAll: ' with remote gem on ';
			nextPutAll: gemHost asAnsiString;
			yourself.
	] ifFalse: [
		stream nextPutAll: ' with linked gem'. 
	].
	^stream contents.
!

updatePreferences

	| string |
	string := self serverPerform: #'preferences'.
	self updatePreferencesFrom: string.
!

updatePreferencesFrom: aString
	"See JadeServer>>_preferences for the generation of this string"

	| pieces x |
	pieces := (self decode: aString) subStrings: Character tab.
	stoneSessionID := (pieces at: 1) asNumber.
	stoneSerial := (pieces at: 2) asNumber.
	isPackagePolicyEnabled 			:= (pieces at: 3) = 'true'.
	isNativeCode				 	:= (x := pieces at: 4) = 'nil' ifTrue: [nil] ifFalse: [x = 'true'].
	isAutoMigrate					:= (x := pieces at: 5) = 'nil' ifTrue: [nil] ifFalse: [x = 'true'].
	isAutoCommit 					:= (x := pieces at: 6) = 'nil' ifTrue: [nil] ifFalse: [x = 'true'].
	isShowUnimplementedMessages	:= (x := pieces at: 7) = 'nil' ifTrue: [nil] ifFalse: [x = 'true'].!

userID

	^userID.
!

valueOfArrayOop: anOopType

	| array |
	anOopType isGsNil ifTrue: [^#()].
	array := library session: gciSessionId fetchObjects: anOopType.
	array := array collect: [:each | self valueOfOop: each].
	^array.
!

valueOfOop: anOopType

	^library session: gciSessionId valueOfOop: anOopType.!

withExplanation: aString do: aBlock
	"Here we make the call and handle any error."

	| result error |
	result := self
		withExplanation: aString 
		doA: aBlock.
	self incrementEventCount.
	(result isKindOf: GsError) ifFalse: [^result].

	error := result.
	JadeLog log: error description.
	result := error signal.
	result = #'resume' ifFalse: [self error: 'Unrecognized response of ' , result printString].
	^self
		withExplanation: aString 
		do: [library
			session: gciSessionId
			continue: error errorReport contextOop].
!

withExplanation: aString doA: aBlock
	"Here we manage the UI feedback"

	| result haveResult shouldRunEventLoop dialog |
	haveResult := false.
	"The next block does the actual work. It is forked to a separate process so we can monitor it from the main process and handle the delay feedback."
	[
		[
			[[
				result := self handlingClientForwarderSendDo: aBlock.
			] on: GsInvalidSessionError do: [:ex | ex return: ex].	"Ignore invalid session (logout!!)"
			] on: Error do: [:ex | 							"Close WaitOnGemStoneDialog"
				result := ex.
				haveResult := true.
				shouldRunEventLoop := false.
				SessionManager inputState prod.
				ex pass.
			].
		] ensure: [
			haveResult := true.
			shouldRunEventLoop := false.
			SessionManager inputState prod.
		].
	] newProcess name: 'GciSession>>withExplanation:doA: - 1'; resume.

	"The following loop handles removing the wait dialog if we are in a ClientForwarderSend, then restoring it when we go back to the server."
	[
		shouldRunEventLoop := true.
		"The next block shows the busy cursor for up to one second. The one second restarts if something happens on the client (ClientForwarderSend)."
		[
			| proc |
			proc := [
				(Delay forSeconds: 1) wait.
				shouldRunEventLoop := false.
				SessionManager inputState prod.
			] newProcess name: 'GciSession>>withExplanation:doA: - 2'; resume.
			isHandlingClientForwarderSend ifTrue: [
				SessionManager inputState loopWhile: [shouldRunEventLoop].
				shouldRunEventLoop := true.
			] ifFalse: [
				Cursor wait showWhile: [
					SessionManager inputState loopWhile: [shouldRunEventLoop].
					shouldRunEventLoop := true.
				].
			].
			proc terminate.
			haveResult ifTrue: [
				((result isKindOf: Error) and: [(result isKindOf: GciError) not and: [(result isKindOf: GsError) not]]) ifTrue: [self error: result description].
				^result
			].
			isHandlingClientForwarderSend.
		] whileTrue: [].

		"Now we have been on the server for at least a second and it is time to create a dialog for more detailed feedback (but it doesn't get shown quite yet)."
		dialog := WaitOnGemStoneDialog
			gciSession: self 
			message: aString
			havingWaited: 1.

		"Watch to see when the main process (above) finishes. Once the work is done we close the wait dialog."
		[
			SessionManager inputState loopWhile: [shouldRunEventLoop and: [isHandlingClientForwarderSend not]].
			dialog view close.
		] newProcess name: 'GciSession>>withExplanation:doA: - 3'; resume.

		"Show the dialog and wait till the dialog closes."
		dialog showModal.
		"The dialog has been closed (either by the background process since the main task finished, or for some other reason."
		SessionManager inputState pumpMessages.
		haveResult.	"Dialog may have been closed by a ClientForwarderSend"
	] whileFalse.
	((result isKindOf: Error) and: [(result isKindOf: GciError) not and: [(result isKindOf: GsError) not]]) ifTrue: [self error: result description].
	^result.
! !
!GciSession categoriesFor: #_executeString:fromContextOop:environment:!private! !
!GciSession categoriesFor: #_send:to:withAll:!private! !
!GciSession categoriesFor: #_server!private! !
!GciSession categoriesFor: #abort!Jade convenience!public! !
!GciSession categoriesFor: #begin!Jade convenience!public! !
!GciSession categoriesFor: #beInvalidSession!public! !
!GciSession categoriesFor: #briefDescription!public! !
!GciSession categoriesFor: #clearStack:!Jade!public! !
!GciSession categoriesFor: #clientForwardError:!public! !
!GciSession categoriesFor: #clientForwarders!public! !
!GciSession categoriesFor: #commit!Jade convenience!public! !
!GciSession categoriesFor: #debugString:fromContext:environment:!long running!public! !
!GciSession categoriesFor: #debugToFilePath:!Jade convenience!public! !
!GciSession categoriesFor: #decode:!public! !
!GciSession categoriesFor: #encode:!public! !
!GciSession categoriesFor: #eventCount!public! !
!GciSession categoriesFor: #executeString:!Jade convenience!public! !
!GciSession categoriesFor: #executeString:fromContext:environment:!long running!public! !
!GciSession categoriesFor: #fetchBytes:!private! !
!GciSession categoriesFor: #forceLogout!Jade!public! !
!GciSession categoriesFor: #gciSessionId!public! !
!GciSession categoriesFor: #gciSessionId:!private! !
!GciSession categoriesFor: #gciVersion!public! !
!GciSession categoriesFor: #gemHost!accessing!public! !
!GciSession categoriesFor: #gsHasPragmas!public! !
!GciSession categoriesFor: #handlingClientForwarderSendDo:!long running!private! !
!GciSession categoriesFor: #hardBreak!Jade!public! !
!GciSession categoriesFor: #hasHeartbeat!heartbeat!public! !
!GciSession categoriesFor: #hasServer!public! !
!GciSession categoriesFor: #heartbeat:arguments:!heartbeat!private! !
!GciSession categoriesFor: #idleWarning!heartbeat!private! !
!GciSession categoriesFor: #incrementEventCount!long running!private! !
!GciSession categoriesFor: #indexOfClientForwarder:!public! !
!GciSession categoriesFor: #initializeLibrary:stoneNRS:gemNRS:userID:password:hostUserID:password:initials:debugPath:!private! !
!GciSession categoriesFor: #initializeServer!private! !
!GciSession categoriesFor: #isAutoCommit!accessing!public! !
!GciSession categoriesFor: #isAutoCommit:!accessing!public! !
!GciSession categoriesFor: #isAutoMigrate!accessing!public! !
!GciSession categoriesFor: #isAutoMigrate:!accessing!public! !
!GciSession categoriesFor: #isLinkedGem!public! !
!GciSession categoriesFor: #isNativeCode!accessing!public! !
!GciSession categoriesFor: #isNativeCode:!accessing!public! !
!GciSession categoriesFor: #isOopType:!public! !
!GciSession categoriesFor: #isPackagePolicyEnabled!public! !
!GciSession categoriesFor: #isRemoteGem!public! !
!GciSession categoriesFor: #isShowUnimplementedMessages!accessing!public! !
!GciSession categoriesFor: #isShowUnimplementedMessages:!accessing!public! !
!GciSession categoriesFor: #isValidSession!public! !
!GciSession categoriesFor: #libraryVersion!public! !
!GciSession categoriesFor: #loadLibrary:debugPath:!public! !
!GciSession categoriesFor: #logout!Jade!public! !
!GciSession categoriesFor: #logoutRequested!Jade!public! !
!GciSession categoriesFor: #message:caption:icon:buttons:!OmniBrowser!public! !
!GciSession categoriesFor: #netPort!accessing!public! !
!GciSession categoriesFor: #netTask!accessing!public! !
!GciSession categoriesFor: #oopForString:!public! !
!GciSession categoriesFor: #oopGemStoneError!public! !
!GciSession categoriesFor: #oopIllegal!public! !
!GciSession categoriesFor: #oopTypeFor:!public! !
!GciSession categoriesFor: #oopTypeWithOop:!public! !
!GciSession categoriesFor: #postLoginAs:!private! !
!GciSession categoriesFor: #printString:!Jade convenience!public! !
!GciSession categoriesFor: #prompt:caption:!OmniBrowser!public! !
!GciSession categoriesFor: #releaseOop:!Jade convenience!public! !
!GciSession categoriesFor: #releaseOops:!Jade!public! !
!GciSession categoriesFor: #returningResultOrErrorDo:!long running!private! !
!GciSession categoriesFor: #send:to:!Jade convenience!public! !
!GciSession categoriesFor: #send:to:withAll:!Jade convenience!long running!public! !
!GciSession categoriesFor: #sendInterpreted:to:withAll:!Jade convenience!long running!public! !
!GciSession categoriesFor: #serverArrayFor:!Jade convenience!public! !
!GciSession categoriesFor: #serverClass!accessing!public! !
!GciSession categoriesFor: #serverPerform:!Jade convenience!public! !
!GciSession categoriesFor: #serverPerform:with:!Jade convenience!public! !
!GciSession categoriesFor: #serverPerform:with:with:!Jade convenience!public! !
!GciSession categoriesFor: #serverPerform:with:with:with:!Jade convenience!public! !
!GciSession categoriesFor: #serverPerform:with:with:with:with:!Jade convenience!public! !
!GciSession categoriesFor: #serverPerform:withArguments:!Jade convenience!public! !
!GciSession categoriesFor: #serverPerformInterpreted:with:with:!Jade convenience!public! !
!GciSession categoriesFor: #serverPerformInterpreted:with:with:with:!Jade convenience!public! !
!GciSession categoriesFor: #serverPerformInterpreted:withArguments:!Jade convenience!public! !
!GciSession categoriesFor: #setInitials:!private! !
!GciSession categoriesFor: #signalConfirmationRequestUsing:!OmniBrowser!public! !
!GciSession categoriesFor: #signalInformRequestUsing:!OmniBrowser!public! !
!GciSession categoriesFor: #signalMultiLineTextRequestUsing:!OmniBrowser!public! !
!GciSession categoriesFor: #signalTextRequestUsing:!OmniBrowser!public! !
!GciSession categoriesFor: #softBreak!Jade!public! !
!GciSession categoriesFor: #startHeartbeat!heartbeat!private! !
!GciSession categoriesFor: #stepInto:inFrame:!public! !
!GciSession categoriesFor: #stepOut:inFrame:!public! !
!GciSession categoriesFor: #stepOver:inFrame:!public! !
!GciSession categoriesFor: #stoneHost!accessing!public! !
!GciSession categoriesFor: #stoneName!accessing!public! !
!GciSession categoriesFor: #stoneSerial!private! !
!GciSession categoriesFor: #stoneSessionID!public! !
!GciSession categoriesFor: #stopHeartbeat!heartbeat!private! !
!GciSession categoriesFor: #terminate:!public! !
!GciSession categoriesFor: #titleBarFor:!public! !
!GciSession categoriesFor: #updatePreferences!private! !
!GciSession categoriesFor: #updatePreferencesFrom:!private! !
!GciSession categoriesFor: #userID!public! !
!GciSession categoriesFor: #valueOfArrayOop:!Jade convenience!public! !
!GciSession categoriesFor: #valueOfOop:!Jade convenience!public! !
!GciSession categoriesFor: #withExplanation:do:!long running!private! !
!GciSession categoriesFor: #withExplanation:doA:!long running!private! !

!GciSession class methodsFor!

libraryClass: libraryClass 
stoneNRS: stoneNRS 
gemNRS: gemNRS 
userID: gsUserID 
password: gsPassword 
hostUserID: hostUserID 
password: hostPassword 
initials: initials
debugPath: debugPath

	^super new
		initializeLibrary: libraryClass
		stoneNRS: stoneNRS 
		gemNRS: gemNRS 
		userID: gsUserID 
		password: gsPassword
		hostUserID: hostUserID 
		password: hostPassword
		initials: initials
		debugPath: debugPath

!

new

	self error: 'use other instance creation protocol'.
!

publishedEventsOfInstances
    	"Answer a Set of Symbols that describe the published events triggered
    	by instances of the receiver."
    
    	^super publishedEventsOfInstances
			add: #'logoutPending';
			add: #'logoutRequested:';
    		add: #'logout';
    		yourself.
    
! !
!GciSession class categoriesFor: #libraryClass:stoneNRS:gemNRS:userID:password:hostUserID:password:initials:debugPath:!public! !
!GciSession class categoriesFor: #new!public! !
!GciSession class categoriesFor: #publishedEventsOfInstances!public! !

