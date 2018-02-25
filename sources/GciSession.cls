"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GciSession
	instanceVariableNames: 'briefDescription clientForwarders eventCount gciSessionId gemHost gemNRS heartbeatProcess isHandlingClientForwarderSend isPackagePolicyEnabled library netPort netTask server stoneHost stoneName stoneNRS stoneSerial stoneSessionID userID'
	classVariableNames: 'GemCursor'
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
		execute: (aString copyWithout: Character cr)
		context: anOopType
		environment: anInteger
!

_library

	^library.
!

_send: aSymbol to: anObject withAll: anArray

	| arguments oops result |
	oops := OrderedCollection new.
	arguments := anArray collect: [:each | 
		(each isKindOf: Integer) ifTrue: [library session: gciSessionId oopForInteger: each] ifFalse: [
		(each isKindOf: String) ifTrue: [oops add: (self oopForString: each)] ifFalse: [
		each]].
	].
	result := library 
		session: gciSessionId
		send: aSymbol 
		to: (self oopTypeFor: anObject) 
		with: (self serverArrayFor: arguments).
	self releaseOops: oops.
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
		args := self valueOfArrayOop: (gciErrSType args at: 4).		(receiver isNil or: [self clientForwarders size < receiver]) ifTrue: [^library oopNil].
		receiver := self clientForwarders at: receiver.
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
		with: aString 
		with: anObject 
		with: anInteger.
!

debugToFilePath: aString

	^library gciDbgEstablishToFile: aString
!

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
		continueWith := result signal.
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

	library hardBreakSession: gciSessionId.
!

hasServer

	^server notNil.
!

heartbeat: receiver arguments: arguments

	| result |
	(Delay forSeconds: 1) wait.
	result := self returningResultOrErrorDo: [
		library 
			session: gciSessionId
			send: 'delay' 
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
	isHandlingClientForwarderSend := false.
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
	library ifNil: [^self].
	[
		server := self executeString: library jadeServerClass gsString.
	] on: GsRuntimeError do: [:ex | 
		| number notes |
		number := ex errorReport number.
		notes := ''.
		number = 2151 ifTrue: [notes := ' (code modification?)'].
		MessageBox warning: ex description , notes , '
Server initialization failed. Most functionality will be broken!!'.
		SessionManager current pause.
		ex return: nil.
	].

	(server isKindOf: String) ifTrue: [
		self logout.
		GciLoginFailed
			signal: 'JadeServer compile error:
' , server
			with: self.
	].

	1 ~~ 1 ifTrue: [		"messasges that are sent by a ClientForwarderSend should not be stripped"
		self 
			signalConfirmationRequestUsing: nil;
			signalTextRequestUsing: nil;
			yourself.
	].
!

isLinkedGem

	^gemNRS isEmpty.
!

isOopType: anObject

	^anObject isKindOf: library oopTypeClass.
!

isPackagePolicyEnabled

	^isPackagePolicyEnabled!

isRemoteGem

	^self isLinkedGem not.
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
	debugPath notEmpty ifTrue: [
		(self debugToFilePath: debugPath) ifFalse: [self error: 'Unable to open ' , debugPath printString , ' for GCI debugging'].
		library gemTrace: 2.
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

netPort
	^netPort!

netTask
	^netTask!

oopForString: aString

	aString isNil ifTrue: [^library oopNil].
	^library session: gciSessionId oopForString: aString
!

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
	(anObject isKindOf: String)		ifTrue: [^self oopForString: anObject].
	MessageBox notify: 'Sorry, we are not yet prepared to convert ' , anObject printString , ' to an OOP!!'.
	SessionManager current pause.
!

oopTypeWithOop: anInteger

	^library oopTypeWithOop: anInteger.
!

postLoginAs: aString 

	self initializeServer.
	self setInitials: aString.		"This gets back the session and serial number"
!

printString: anOopType

	^self
		serverPerform: 'printStringOf:'
		with: anOopType.
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
			self
				_send: aSymbol 
				to: anObject 
				withAll: anArray.
		].
!

sendInterpreted: aString to: anObject withAll: anArray

	| stream arguments |
	stream := WriteStream on: String new.
	stream 
		nextPutAll: (anObject == server ifTrue: ['jadeServer'] ifFalse: [anObject printString]); cr;
		nextPutAll: '	performInterpreted: #''';
		nextPutAll: aString;
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
				sendInterpreted: aString 
				to: (self oopTypeFor: anObject) 
				with: arguments
				session: gciSessionId.
		].
!

serverArrayFor: anArray

	| array |
	array := library oopTypeArrayClass new: anArray size.
	1 to: anArray size do: [:i | 
		array at: i put: (self oopTypeFor: (anArray at: i)).
	].
	^array.
!

serverPerform: aSymbol

	^self
		send: aSymbol
		to: server.
!

serverPerform: aSymbol with: arg1

	^self
		serverPerform: aSymbol 
		withArguments: (Array with: arg1).
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

	| pieces string |
	server isNil ifTrue: [^self].
	string := self
		serverPerform: #'mcInitials:' 
		with: initials.
	pieces := string subStrings collect: [:each | each asNumber].
	stoneSessionID := pieces at: 1.
	stoneSerial := pieces at: 2.
	isPackagePolicyEnabled := (pieces at: 3) = 1.!

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
	^library oopNil!

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
		so that background processes on the server get a chance to run.

		Could be called from #'postLogin'"

	1 = 1 ifTrue: [^self].
	heartbeatProcess := [
		| receiver arguments |
		receiver := self oopTypeFor: server.
		arguments := self serverArrayFor: #().
		[true] whileTrue: [self heartbeat: receiver arguments: arguments].
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

userID

	^userID.
!

valueOfArrayOop: anOopType

	| array |
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

	| result haveResult shouldRunEventLoop dialog proc |
	shouldRunEventLoop := true.
	haveResult := false.
	[
		result := self handlingClientForwarderSendDo: aBlock.
		haveResult := true.
		shouldRunEventLoop := false.
		SessionManager inputState prod.
	] newProcess name: 'GciSession>>withExplanation:doA: - 1'; resume.
	[
		proc := [
			(Delay forSeconds: 1) wait.
			shouldRunEventLoop := false.
			SessionManager inputState prod.
		] newProcess name: 'GciSession>>withExplanation:doA: - 2'; resume.
		isHandlingClientForwarderSend ifTrue: [
			SessionManager inputState loopWhile: [shouldRunEventLoop].
			shouldRunEventLoop := true.
		] ifFalse: [
			self class cursor showWhile: [
				SessionManager inputState loopWhile: [shouldRunEventLoop].
				shouldRunEventLoop := true.
			].
		].
		haveResult ifTrue: [^result].
		isHandlingClientForwarderSend.
	] whileTrue: [].
	dialog := WaitOnGemStoneDialog
		gciSession: self 
		message: aString
		havingWaited: 1.
	[
		SessionManager inputState loopWhile: [shouldRunEventLoop].
		shouldRunEventLoop := true.
		dialog view close.
	] newProcess name: 'GciSession>>withExplanation:doA: - 3'; resume.
	dialog showModal.
	SessionManager inputState pumpMessages.
	^result.
! !
!GciSession categoriesFor: #_executeString:fromContextOop:environment:!private! !
!GciSession categoriesFor: #_library!accessing!private! !
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
!GciSession categoriesFor: #hasServer!public! !
!GciSession categoriesFor: #heartbeat:arguments:!heartbeat!private! !
!GciSession categoriesFor: #incrementEventCount!long running!private! !
!GciSession categoriesFor: #indexOfClientForwarder:!public! !
!GciSession categoriesFor: #initializeLibrary:stoneNRS:gemNRS:userID:password:hostUserID:password:initials:debugPath:!private! !
!GciSession categoriesFor: #initializeServer!private! !
!GciSession categoriesFor: #isLinkedGem!public! !
!GciSession categoriesFor: #isOopType:!public! !
!GciSession categoriesFor: #isPackagePolicyEnabled!public! !
!GciSession categoriesFor: #isRemoteGem!public! !
!GciSession categoriesFor: #isValidSession!public! !
!GciSession categoriesFor: #libraryVersion!public! !
!GciSession categoriesFor: #loadLibrary:debugPath:!public! !
!GciSession categoriesFor: #logout!Jade!public! !
!GciSession categoriesFor: #logoutRequested!Jade!public! !
!GciSession categoriesFor: #netPort!accessing!public! !
!GciSession categoriesFor: #netTask!accessing!public! !
!GciSession categoriesFor: #oopForString:!public! !
!GciSession categoriesFor: #oopGemStoneError!public! !
!GciSession categoriesFor: #oopIllegal!public! !
!GciSession categoriesFor: #oopTypeFor:!public! !
!GciSession categoriesFor: #oopTypeWithOop:!public! !
!GciSession categoriesFor: #postLoginAs:!private! !
!GciSession categoriesFor: #printString:!Jade convenience!public! !
!GciSession categoriesFor: #releaseOop:!Jade convenience!public! !
!GciSession categoriesFor: #releaseOops:!Jade!public! !
!GciSession categoriesFor: #returningResultOrErrorDo:!long running!private! !
!GciSession categoriesFor: #send:to:!Jade convenience!public! !
!GciSession categoriesFor: #send:to:withAll:!Jade convenience!long running!public! !
!GciSession categoriesFor: #sendInterpreted:to:withAll:!Jade convenience!long running!public! !
!GciSession categoriesFor: #serverArrayFor:!Jade convenience!public! !
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
!GciSession categoriesFor: #stopHeartbeat!Jade!public! !
!GciSession categoriesFor: #terminate:!public! !
!GciSession categoriesFor: #titleBarFor:!public! !
!GciSession categoriesFor: #userID!public! !
!GciSession categoriesFor: #valueOfArrayOop:!Jade convenience!public! !
!GciSession categoriesFor: #valueOfOop:!Jade convenience!public! !
!GciSession categoriesFor: #withExplanation:do:!long running!private! !
!GciSession categoriesFor: #withExplanation:doA:!long running!private! !

!GciSession class methodsFor!

cursor

	GemCursor isNil ifTrue: [
		GemCursor := Cursor fromFile: 'icons\GS16.ico'.
	].
	^GemCursor.
!

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
!GciSession class categoriesFor: #cursor!public! !
!GciSession class categoriesFor: #libraryClass:stoneNRS:gemNRS:userID:password:hostUserID:password:initials:debugPath:!public! !
!GciSession class categoriesFor: #new!public! !
!GciSession class categoriesFor: #publishedEventsOfInstances!public! !

