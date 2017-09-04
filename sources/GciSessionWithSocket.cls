"Filed out from Dolphin Smalltalk 7"!

GciSession subclass: #GciSessionWithSocket
	instanceVariableNames: 'socket'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciSessionWithSocket guid: (GUID fromString: '{604cbf33-d6be-45ff-b250-4654b42aa062}')!
GciSessionWithSocket comment: ''!
!GciSessionWithSocket categoriesForClass!Unclassified! !
!GciSessionWithSocket methodsFor!

_continueProcess: aContextOop

	^self
		_usingSocketSend: #'processAction:' 
		to: server
		withAll: (Array with: aContextOop with: #'resume').
!

_send: aSymbol to: anObject withAll: anArray

	^self
		_usingSocketSend: aSymbol 
		to: anObject 
		withAll: anArray.
!

_usingSocketSend: aSymbol to: anObject withAll: anArray

	| stream  process resultSize result encoder |
	stream := WriteStream on: ByteArray new.
	encoder := JadeServer64bit33 new
		add: anObject toByteStream: stream;
		add: aSymbol toByteStream: stream;
		add: anArray toByteStream: stream;
		yourself.
	process := [:bytes | socket sendByteArray: bytes] newProcessWithArguments: (Array with: stream contents).
	process resume.
	[
		resultSize := library 
				session: gciSessionId
				send: #'readSocket:' 
				to: server
				with: (self serverArrayFor: (Array with: stream contents size)).
		result := socket receiveByteArray: resultSize.
	] on: Exception do: [:ex | 
	].	stream := ReadStream on: result.
	result := encoder 
		readObjectFrom: stream
		errorClass: library errorStructureClass.
	^result.
!

initializeFrom: aGciSession withSocket: aSocket

	1 to: aGciSession class allInstVarNames size do: [:i | 
		self instVarAt: i put: (aGciSession instVarAt: i).
	].
	socket := aSocket.
!

stepInto: aGsProcess inFrame: anInteger

	[
		self
			_usingSocketSend: #'processAction:' 
			to: server
			withAll: (Array with: aGsProcess with: #'stepInto' with: anInteger).
	] on: GciError do: [:ex | 
		ex tag number == 6002 ifTrue: [
			ex return.	"We were expecting a single-step breakpoint!!"
		].
		ex pass.
	].!

stepOut: aGsProcess inFrame: anInteger

	[
		self
			_usingSocketSend: #'processAction:' 
			to: server
			withAll: (Array with: aGsProcess with: #'stepOut' with: anInteger).
	] on: GciError do: [:ex | 
		ex tag number == 6002 ifTrue: [
			ex return.	"We were expecting a single-step breakpoint!!"
		].
		ex pass.
	].!

stepOver: aGsProcess inFrame: anInteger

	[
		self
			_usingSocketSend: #'processAction:' 
			to: server
			withAll: (Array with: aGsProcess with: #'stepOver' with: anInteger).
	] on: GciError do: [:ex | 
		ex tag number == 6002 ifTrue: [
			ex return.	"We were expecting a single-step breakpoint!!"
		].
		ex pass.
	].!

terminate: anOopType

	self
		_usingSocketSend: #'processAction:' 
		to: server
		withAll: (Array with: anOopType with: #'terminate').
	TerminateProcess signal.
! !
!GciSessionWithSocket categoriesFor: #_continueProcess:!long running!private! !
!GciSessionWithSocket categoriesFor: #_send:to:withAll:!private! !
!GciSessionWithSocket categoriesFor: #_usingSocketSend:to:withAll:!private! !
!GciSessionWithSocket categoriesFor: #initializeFrom:withSocket:!public! !
!GciSessionWithSocket categoriesFor: #stepInto:inFrame:!public! !
!GciSessionWithSocket categoriesFor: #stepOut:inFrame:!public! !
!GciSessionWithSocket categoriesFor: #stepOver:inFrame:!public! !
!GciSessionWithSocket categoriesFor: #terminate:!public! !

!GciSessionWithSocket class methodsFor!

from: aGciSession withSocket: aSocket

	^self basicNew
		initializeFrom: aGciSession withSocket: aSocket;
		yourself.
!

new

	self error: 'use #''from:withSocket:'''.! !
!GciSessionWithSocket class categoriesFor: #from:withSocket:!public! !
!GciSessionWithSocket class categoriesFor: #new!public! !

