"Filed out from Dolphin Smalltalk 7"!

JadeServer64bit32 subclass: #JadeServer64bit33
	instanceVariableNames: 'socket'
	classVariableNames: 'Breakpoint GsNMethod GsSocket Utf8'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer64bit33 guid: (GUID fromString: '{4100a18b-fbc2-4ea5-86a8-b8a8c6194269}')!
JadeServer64bit33 comment: ''!
!JadeServer64bit33 categoriesForClass!Unclassified! !
!JadeServer64bit33 methodsFor!

_decodeString: aString

	^aString asUnicodeString!

_encodeString: aString

	^aString encodeAsUTF8!

_gemLogPath

	^System gemLogPath
!

_string

	^Unicode16 new!

acceptConnection

	| listener time flag |
	listener := socket.
	socket := nil.
	time := Time millisecondsElapsedTime: [flag := listener readWillNotBlockWithin: 2000].
	flag ifFalse: [
		listener close.
		^nil
	].
	socket := listener accept.
	listener close.
	socket isNil ifTrue: [^nil].
	^time.

!

add: anObject toByteStream: aStream

	anObject == nil		ifTrue: [aStream nextPut: 0. ^self].
	anObject == true 	ifTrue: [aStream nextPut: 1. ^self].
	anObject == false 	ifTrue: [aStream nextPut: 2. ^self].
	(ExternalInteger notNil and: [anObject isKindOf: ExternalInteger]) ifTrue: [self addExternalInteger: anObject toByteStream: aStream. ^self].		"3 & 4"	(self isInDolphin and: [anObject isKindOf: GsObject]) ifTrue: [self addExternalInteger: anObject oopType toByteStream: aStream. ^self].			"3 & 4"
	(anObject isKindOf: Symbol	) ifTrue: [self addSymbol: anObject toByteStream: aStream. ^self].	"5 & 6"
	(anObject isKindOf: Integer	) ifTrue: [self addInteger: anObject toByteStream: aStream. ^self].	"7, 8, 9, & 10"
	(anObject isKindOf: String		) ifTrue: [self addString: anObject toByteStream: aStream. ^self].	"11 & 12"
	(anObject isKindOf: Array		) ifTrue: [self addArray: anObject toByteStream: aStream. ^self].	"13"
	self isInDolphin ifTrue: [self error: 'Attempt to encode an unsupported object'].
	self addPositiveInteger: anObject toByteStream: aStream.
!

addArray: anArray toByteStream: aStream

	16rFF < anArray size ifTrue: [self error: 'Array is too large!!'].
	aStream nextPut: 14; nextPut: anArray size.
	anArray do: [:each | self add: each toByteStream: aStream].
!

addExternalInteger: anObject toByteStream: aStream

	self 
		addPositiveInteger: anObject value
		toByteStream: aStream 
		code: (anObject byteSize == 4 ifTrue: [3] ifFalse: [4]) 
		size: anObject byteSize.
!

addInteger: anObject toByteStream: aStream

	| integer stream |
	(0 <= anObject and: [anObject < 16r100]) ifTrue: [
		aStream nextPut: 7; nextPut: anObject.
		^self.
	].
	(-16r100 <= anObject and: [anObject < 0]) ifTrue: [
		aStream nextPut: 8; nextPut: anObject negated - 1.
		^self.
	].
	aStream nextPut: (anObject positive ifTrue: [9] ifFalse: [10]).
	integer := anObject abs.
	stream := WriteStream on: ByteArray new.
	[
		0 < integer.
	] whileTrue: [
		stream nextPut: (integer bitAnd: 16rFF).
		integer := integer // 16r100.
	].
	aStream
		nextPut: stream contents size;
		nextPutAll: stream contents;
		yourself.
!

addPositiveInteger: anInteger toByteStream: aStream

		self
			addPositiveInteger: anInteger asOop
			toByteStream: aStream 
			code: 4 
			size: 8.
!

addPositiveInteger: anInteger toByteStream: aStream code: codeInteger size: sizeInteger

	| x |
	x := anInteger.
	aStream nextPut: codeInteger.
	sizeInteger timesRepeat: [
		aStream nextPut: (x bitAnd: 16rFF).
		x := x // 16r100.
	].
!

addString: aString toByteStream: aStream

	| size |
	(size := aString size) < 16r100 ifTrue: [
		aStream nextPut: 11; nextPut: size.
		aString do: [:each | aStream nextPut: each asciiValue].
		^self.
	].
	size < 16r1000000 ifTrue: [
		aStream nextPut: 12; nextPut: (size bitAnd: 16rFF); nextPut: (size // 16r100 bitAnd: 16rFF); nextPut: (size // 16r10000 bitAnd: 16rFF).
		aString do: [:each | aStream nextPut: each asciiValue].
		^self.
	].
	self error: 'Object cannot be encoded'.
!

addSymbol: aSymbol toByteStream: aStream

	aSymbol size <= 16rFF ifTrue: [
		aStream nextPut: 5; nextPut: aSymbol size.
		aSymbol do: [:each | aStream nextPut: each asciiValue].
		^self.
	].
	aSymbol size <= 16rFFFF ifTrue: [
		aStream nextPut: 6; nextPut: (aSymbol size bitAnd: 16rFF); nextPut: (aSymbol size // 16r100 bitAnd: 16rFF).
		aSymbol do: [:each | aStream nextPut: each asciiValue].
		^self.
	].
	self error: 'Object cannot be encoded'.
!

doSocketRequest
	"We are in a forked process.
	We provide the result or a description of an Exception on the stream."

	| arguments process receiver selector semaphore stream |
	Breakpoint trappable: true.
	process			:= Processor activeProcess.
	receiver 		:= process environmentAt: #'receiver'.
	selector 		:= process environmentAt: #'selector'.
	arguments 	:= process environmentAt: #'arguments'.
	semaphore 	:= process environmentAt: #'semaphore'.
	stream 			:= process environmentAt: #'stream'.
	[
		self add: (receiver perform: selector withArguments: arguments) toByteStream: stream.
		semaphore signal.		"we are finished and the GCI process may resume"
	] on: Exception do: [:ex | 
		GsNMethod _clearAllStepBreaks.		"We are headed into a debugger so we either hit a breakpoint or we have an unrelated error"
		stream nextPut: Character cr codePoint.
		self
			add: GemStoneError 			toByteStream: stream;
			add: ex gsNumber 			toByteStream: stream;
			add: GsProcess _current 	toByteStream: stream;
			add: ex 							toByteStream: stream;
			add: ex class name 			toByteStream: stream;
			add: ex description 			toByteStream: stream;
			add: ex gsArguments 		toByteStream: stream;
			add: (GsProcess stackReportToLevel: 100) toByteStream: stream;
			yourself.
		semaphore signal.		"GCI process may resume"
		semaphore wait.		"wait till we are told it is okay to resume"
		stream reset.
		ex resume.
	].
!

environmentAt: key

	self error: 'method included to avoid unimplemented selector warning in Dolphin'.
!

environmentAt: key put: value

	self error: 'method included to avoid unimplemented selector warning in Dolphin'.
!

isInDolphin
	"Most JadeServer code is in GemStone but the socket wire protocol encode/decode is in Dolphin as well"

	^System isNil.
!

isInGemStone
	"Most JadeServer code is in GemStone but the socket wire protocol encode/decode is in Dolphin as well"

	^System notNil.
!

makeListener

	socket := GsSocket new makeServer: 1.
	socket isNil ifTrue: [^nil].
	^socket port.
!

processAction: arguments

	| process request semaphore stream |
	process := arguments at: 1.
	semaphore := process environmentAt: #'semaphore'.
	stream := process environmentAt: #'stream'.
	stream reset.
	semaphore signal.		"okay for process to continue"
	request := arguments at: 2.
	request == #'terminate' 	ifTrue: [process terminate. self add: #'terminate' toByteStream: stream. 			^self].
	request == #'resume' 	ifTrue: [												semaphore wait. 	^self].	"this should resume from the last line of #'doSocketRequest'."
	request == #'stepInto' 	ifTrue: [process stepIntoFromLevel: 		(arguments at: 3). 	semaphore wait. 	^self].
	request == #'stepOut' 	ifTrue: [process stepOverFromLevel: 		(arguments at: 3). 	semaphore wait. 	^self].	"1 added by JadeDebugger>>#'stepOut'"
	request == #'stepOver'	ifTrue: [process stepThroughFromLevel: 	(arguments at: 3). 	semaphore wait. 	^self].	"thru gives us more natural behavior"
	self error: 'Unrecognized command!!'.
!

readExceptionFrom: aStream errorClass: gciErrorSTypeClass

	| category number context exception name message arguments stack gciErrSType |
	category		:= self readObjectFrom: aStream.	number		:= self readObjectFrom: aStream.
	context 		:= self readObjectFrom: aStream.
	exception 		:= self readObjectFrom: aStream.
	name 		:= self readObjectFrom: aStream.	name yourself.
	message 		:= self readObjectFrom: aStream.
	arguments 	:= self readObjectFrom: aStream.
	stack 		:= self readObjectFrom: aStream.
	(gciErrSType	:= gciErrorSTypeClass new)		category: category value;
		number: number;
		context: context value;
		exceptionObj: exception value;		message: (message isNil ifTrue: [''] ifFalse: [message]);
		argCount: arguments size;
		args: arguments;
		stack: stack;
		yourself.
	GciError signalWith:  gciErrSType.
!

readObjectFrom: aStream

	^self readObjectFrom: aStream errorClass: nil.!

readObjectFrom: aStream errorClass: gciErrorSTypeClass

	| type byteSize bytes oid x |
	type := aStream next.
	type == 0 ifTrue: [^nil].
	(type == 1 or: [type == 2]) ifTrue: [
		^type == 1.
	].
	(type == 3 or: [type == 4]) ifTrue: [
		byteSize := type == 3 ifTrue: [4] ifFalse: [8].
		bytes := aStream next: byteSize.
		oid := 0.
		bytes reverseDo: [:each |
			oid := oid * 16r100 + each.
		].
		^System notNil
			ifTrue: [self _objectForOop: oid]	"Usually this will execute in GemStone"
			ifFalse: [type == 3			"This can execute in Dolphin to support testing"
				ifTrue: [OopType32 fromInteger: oid]
				ifFalse: [OopType64 fromInteger: oid]
			].
	].
	type == 5 ifTrue: [		"Symbol with size < 256"
		bytes := aStream next: aStream next.
		x := String new: bytes size.
		1 to: bytes size do: [:i | 
			x at: i put: (Character codePoint: (bytes at: i)).
		].
		^x asSymbol.
	].
	type == 6 ifTrue: [		"Symbol with size >= 256"
		x := aStream next + (aStream next * 16r100).
		bytes := aStream next: x.
		x := String new: bytes size.
		1 to: bytes size do: [:i | 
			x at: i put: (Character codePoint: (bytes at: i)).
		].
		^x asSymbol.
	].
	type == 7 ifTrue: [		"SmallInteger 0 <= x < 256"
		^aStream next.
	].
	type == 8 ifTrue: [		"SmallInteger: -256 <= x < 0"
		^(aStream next + 1) negated
	].
	(type == 9 or: [type == 10]) ifTrue: [
		| integer |
		byteSize := aStream next.
		bytes := aStream next: byteSize.
		integer := 0.
		bytes reverseDo: [:each |
			integer := integer * 16r100 + each.
		].
		^integer * (type == 9 ifTrue: [1] ifFalse: [-1]).
	].
	type == 11 ifTrue: [		"String with size < 256"
		bytes := aStream next: aStream next.
		x := String new: bytes size.
		1 to: bytes size do: [:i | 
			x at: i put: (Character codePoint: (bytes at: i)).
		].
		^x.
	].
	type == 12 ifTrue: [		"String with size >= 256"
		x := aStream next + (aStream next * 16r100) + (aStream next * 16r10000).
		bytes := aStream next: x.
		x := String new: bytes size.
		1 to: bytes size do: [:i | 
			x at: i put: (Character codePoint: (bytes at: i)).
		].
		^x.
	].
	type == 13 ifTrue: [		"GciErrSType"
		^self readExceptionFrom: aStream errorClass: gciErrorSTypeClass.
	].
	type == 14 ifTrue: [		"Array"
		x := Array new: aStream next.
		1 to: x size do: [:i | x at: i put: (self readObjectFrom: aStream)].
		^x.
	].
!

readSocket: anInteger

	| arguments bytes process receiver selector semaphore stream |	bytes := ByteArray new.	[		bytes size < anInteger.
	] whileTrue: [
		socket read: anInteger - bytes size into: bytes startingAt: bytes size + 1.
	].
	stream := ReadStream on: bytes.
	receiver := self readObjectFrom: stream.
	selector := self readObjectFrom: stream.
	arguments := self readObjectFrom: stream.
	(receiver == self and: [selector == #'processAction:']) ifTrue: [	"We already have a process so should not fork again!!"
		process := arguments at: 1.
		process environmentAt: #'parent' put: Processor activeProcess.		"ignore me when returning a list of processes"
		stream := process environmentAt: #'stream'.
		self processAction: arguments.
	] ifFalse: [		"This is the first time with this command so create a new process to handle request"
		process := [self doSocketRequest] newProcess.
		process 
			environmentAt: #'receiver'		put: receiver;
			environmentAt: #'selector'		put: selector;
			environmentAt: #'arguments'	put: arguments;
			environmentAt: #'semaphore'	put: (semaphore := Semaphore new);
			environmentAt: #'stream'		put: (stream := WriteStream on: ByteArray new);
			environmentAt: #'parent' 		put: Processor activeProcess;		"ignore me when returning a list of processes"
			resume.
		semaphore wait.	"we wait while the process runs"
	].
	bytes := stream contents.
	socket write: bytes.
	^bytes size.
! !
!JadeServer64bit33 categoriesFor: #_decodeString:!private! !
!JadeServer64bit33 categoriesFor: #_encodeString:!private! !
!JadeServer64bit33 categoriesFor: #_gemLogPath!private!Transcript! !
!JadeServer64bit33 categoriesFor: #_string!private! !
!JadeServer64bit33 categoriesFor: #acceptConnection!public! !
!JadeServer64bit33 categoriesFor: #add:toByteStream:!public!streaming! !
!JadeServer64bit33 categoriesFor: #addArray:toByteStream:!public!streaming! !
!JadeServer64bit33 categoriesFor: #addExternalInteger:toByteStream:!public!streaming! !
!JadeServer64bit33 categoriesFor: #addInteger:toByteStream:!public!streaming! !
!JadeServer64bit33 categoriesFor: #addPositiveInteger:toByteStream:!public!streaming! !
!JadeServer64bit33 categoriesFor: #addPositiveInteger:toByteStream:code:size:!public!streaming! !
!JadeServer64bit33 categoriesFor: #addString:toByteStream:!public!streaming! !
!JadeServer64bit33 categoriesFor: #addSymbol:toByteStream:!public!streaming! !
!JadeServer64bit33 categoriesFor: #doSocketRequest!public!Socket! !
!JadeServer64bit33 categoriesFor: #environmentAt:!public!Socket! !
!JadeServer64bit33 categoriesFor: #environmentAt:put:!public!Socket! !
!JadeServer64bit33 categoriesFor: #isInDolphin!public!testing! !
!JadeServer64bit33 categoriesFor: #isInGemStone!public!testing! !
!JadeServer64bit33 categoriesFor: #makeListener!public! !
!JadeServer64bit33 categoriesFor: #processAction:!public!Socket! !
!JadeServer64bit33 categoriesFor: #readExceptionFrom:errorClass:!public!Socket! !
!JadeServer64bit33 categoriesFor: #readObjectFrom:!public!streaming! !
!JadeServer64bit33 categoriesFor: #readObjectFrom:errorClass:!public!streaming! !
!JadeServer64bit33 categoriesFor: #readSocket:!public!Socket! !

