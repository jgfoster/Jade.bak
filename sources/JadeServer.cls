"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadeServer
	instanceVariableNames: 'classList classOrganizer readStream writeStream selectedClass methodFilterType methodFilters selections'
	classVariableNames: 'AllGroups AllUsers ClassOrganizer ClientForwarder GemStoneError Globals GsMethodDictionary ProcessorScheduler SymbolDictionary System UserGlobals UserProfile'
	poolDictionaries: ''
	classInstanceVariableNames: 'gsString'!
JadeServer guid: (GUID fromString: '{FC038152-9707-4C5F-8977-A1F8D02EB005}')!
JadeServer comment: '(System _sessionStateAt: 3).	"pre-3.2"
(System __sessionStateAt: 3).	"3.2 and on"
GciSession allInstances , GciSessionWithSocket allInstances do: [:each | each initializeServer].
GciSession allInstances do: [:each | each logout].'!
!JadeServer categoriesForClass!Unclassified! !
!JadeServer methodsFor!

_addToPureExportSet: anObject

	System 
		_add: anObject 
		toGciSet: 39.  "PureExportSet"
!

abort

	classOrganizer := nil.
	System abortTransaction.
	self refreshSymbolList.
!

asString: anObject

	(anObject isKindOf: String) ifTrue: [^anObject].
	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | 
			^'<<printString error: ' , ex printString , '>>'.
		].
	^anObject printString.
!

beginTransaction

	classOrganizer := nil.
	System beginTransaction.
!

commit

	classOrganizer := nil.
	^System commitTransaction.
!

contents
	"WriteStream method to identify things that have not yet been flushed to the output. We have flushed everything!!"

	^''.
!

cr

	self nextPut: Character cr.
!

delay

	(Delay forMilliseconds: 10) wait.
!

errorListFor: aCollection

	| stream |
	aCollection class name == #'ErrorDescription' ifTrue: [^''].
	stream := WriteStream on: String new.
	aCollection do: [:each | 
		stream
			nextPutAll: (each at: 1) printString; tab;
			nextPutAll: (each at: 2) printString; tab;
			nextPutAll: ((2 < each size and: [(each at: 3) notNil]) ifTrue: [(each at: 3)] ifFalse: [(GemStoneError at: #English) at: (each at: 1)]); tab;
			lf.
	].
	^stream contents.
!

executeString: aString fromContext: anObject

	^aString
		evaluateInContext: anObject 
		symbolList: GsSession currentSession symbolList. !

initialize

	self 
		installTranscript;
		registerOBNotifications;
		yourself.!

installTranscript

	| transcript |
	transcript := self objectNamed: #'Transcript'.

	"If no Transcript object, then install me!!"
	(transcript == nil or: [transcript class name == self class name]) ifTrue: [
		UserGlobals at: #'Transcript' put: self.
		System commitTransaction.
		^self.
	].
	
	"Transcript object from Seaside"
	transcript class name = 'TranscriptProxy class' ifTrue: [
		| clientForwarder |
		clientForwarder := ClientForwarder new.
		clientForwarder	clientObject: 2.
		transcript registerTranscriptClientForwarder: clientForwarder.
		^self.
	].
!

is32Bit

	^false.
!

isClientForwarder: anObject

	^anObject _class name == #'ClientForwarder'.
!

makeListener

	^nil.
!

nextPut: aCharacter

	self nextPutAll: aCharacter asString.
!

nextPutAll: anObject

	| string args |
	string := self asString: anObject.
	args := Array
		with: self
		with: 1
		with: #'nextPutAll:'
		with: (Array with: string).
	System
		signal: 2336
		args: args
		signalDictionary: GemStoneError.
!

obConfirmationRequest: anOBConfirmationRequest

	^String new 
		addAll: anOBConfirmationRequest cancelChoice;
		add: Character lf;
		addAll: anOBConfirmationRequest okChoice;
		add: Character lf;
		addAll: anOBConfirmationRequest prompt;
		yourself.
!

obInformRequest: anOBInformRequest

	^anOBInformRequest message!

objectForOop: anInteger

	self subclassResponsibility.!

objectNamed: aString

	^System myUserProfile objectNamed: aString asSymbol.
!

obTextRequest: anOBTextRequest

	| prompt template |
	prompt := anOBTextRequest prompt.
	template := anOBTextRequest template.
	^String new 
		addAll: prompt size printString;
		add: Character lf;
		addAll: prompt;
		addAll: template;
		yourself.
!

oopOf: anObject

	^anObject asOop.
!

refreshSymbolList

	GsSession currentSession symbolList replaceElementsFrom: self symbolList.	"replace the transcient one with the persistent one"
!

registerOBNotifications

	| platform clientForwarder |
	(platform := self objectNamed: #'OBGemStonePlatform') isNil ifTrue: [^self].
	clientForwarder := ClientForwarder new.
	clientForwarder	clientObject: 1.
	self
		registerOBNotificationsForPlatform: platform 
		clientForwarder: clientForwarder.
!

registerOBNotificationsForPlatform: platform clientForwarder: clientForwarder

	platform 
		registerBrowseClientForwarder: clientForwarder;
		registerChoiceClientForwarder: clientForwarder;
		registerCloseClientForwarder: clientForwarder;
		registerConfirmationClientForwarder: clientForwarder;
		registerInformClientForwarder: clientForwarder;
		registerMultiLineTextClientForwarder: clientForwarder;
		registerTextClientForwarder: clientForwarder;
		yourself.
!

reset
	"WriteStream protocol"!

show: anObject

	self nextPutAll: anObject printString.
!

stackForProcess: aGsProcess

	| array stream |
	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | nil].
	array := aGsProcess _reportOfSize: 5000.
	stream := WriteStream on: String new.
	array do: [:each | 
		stream nextPutAll: each; lf.
	].
	^stream contents.
!

terminate: aGsProcess

	aGsProcess isNil ifTrue: [^self].
	aGsProcess terminate.
	(Delay forMilliseconds: 10) wait.	"allow forked processes to finish"
! !
!JadeServer categoriesFor: #_addToPureExportSet:!private! !
!JadeServer categoriesFor: #abort!public! !
!JadeServer categoriesFor: #asString:!public!Transcript! !
!JadeServer categoriesFor: #beginTransaction!public! !
!JadeServer categoriesFor: #commit!public! !
!JadeServer categoriesFor: #contents!public! !
!JadeServer categoriesFor: #cr!public!Transcript! !
!JadeServer categoriesFor: #delay!public! !
!JadeServer categoriesFor: #errorListFor:!public! !
!JadeServer categoriesFor: #executeString:fromContext:!public! !
!JadeServer categoriesFor: #initialize!public! !
!JadeServer categoriesFor: #installTranscript!public!Transcript! !
!JadeServer categoriesFor: #is32Bit!public! !
!JadeServer categoriesFor: #isClientForwarder:!Debugger!public! !
!JadeServer categoriesFor: #makeListener!private! !
!JadeServer categoriesFor: #nextPut:!public!Transcript! !
!JadeServer categoriesFor: #nextPutAll:!public!Transcript! !
!JadeServer categoriesFor: #obConfirmationRequest:!OmniBrowser!public! !
!JadeServer categoriesFor: #obInformRequest:!OmniBrowser!public! !
!JadeServer categoriesFor: #objectForOop:!private! !
!JadeServer categoriesFor: #objectNamed:!private! !
!JadeServer categoriesFor: #obTextRequest:!OmniBrowser!public! !
!JadeServer categoriesFor: #oopOf:!private! !
!JadeServer categoriesFor: #refreshSymbolList!public! !
!JadeServer categoriesFor: #registerOBNotifications!public! !
!JadeServer categoriesFor: #registerOBNotificationsForPlatform:clientForwarder:!public! !
!JadeServer categoriesFor: #reset!public! !
!JadeServer categoriesFor: #show:!public!Transcript! !
!JadeServer categoriesFor: #stackForProcess:!public! !
!JadeServer categoriesFor: #terminate:!Processes!public! !

!JadeServer class methodsFor!

addGsStringTo: aStream definingClassBlock: aBlock

	aStream
		nextPutAll: 'class := ', (aBlock value: self); lf;
		yourself.
	self selectors do: [:each | 
		aStream nextPutAll: 'source := '.
		((self sourceCodeAt: each) reject: [:char | char == Character cr]) printOn: aStream.
		aStream
			nextPutAll: '.'; lf;
			nextPutAll: 'result := class'; lf;
			tab; nextPutAll: 'compileMethod: source'; lf;
			tab; nextPutAll: 'dictionaries: symbolList'; lf;
			tab; nextPutAll: 'category: ''category''.'; lf;
			nextPutAll: 'result ~~ nil ifTrue: [^GsMethod _sourceWithErrors: result fromString: source].'; lf;
			yourself.
	].
!

classVarsForGemStone

	^(self == JadeServer ifTrue: ['ExternalInteger GciError GsObject OopType32 OopType64'] ifFalse: ['']).
!

gsClassDefinitionBlock

	self subclassResponsibility.
!

gsString
"
	JadeServer64bit3x gsString.
"
	| stream |
	SessionManager current isRuntime ifTrue: [^gsString].
	(stream := WriteStream on: String new)
		nextPutAll: '| class symbolList source server result mcPlatformSupport |'; lf;
		nextPutAll: 'symbolList := System myUserProfile symbolList.'; lf;
		nextPutAll: 'class := Object.'; lf;
		yourself.
	(self withAllSuperclasses remove: Object; yourself) reverseDo: [:eachClass | eachClass addGsStringTo: stream definingClassBlock: self gsClassDefinitionBlock].
	stream 
		nextPutAll: '(mcPlatformSupport := System myUserProfile objectNamed: #''MCPlatformSupport'') notNil ifTrue: ['; lf;
		nextPutAll: '	mcPlatformSupport autoCommit: false; autoMigrate: false].'; lf;
		nextPutAll: 'server := class new initialize; yourself.'; lf;
		nextPutAll: self sessionStateCode; lf;
		nextPutAll: 'server';
		yourself.
	gsString := stream contents.
	^gsString.
!

sessionStateCode

	^'System _sessionStateAt: 3 put: server.'! !
!JadeServer class categoriesFor: #addGsStringTo:definingClassBlock:!public! !
!JadeServer class categoriesFor: #classVarsForGemStone!public! !
!JadeServer class categoriesFor: #gsClassDefinitionBlock!public! !
!JadeServer class categoriesFor: #gsString!public! !
!JadeServer class categoriesFor: #sessionStateCode!public! !

