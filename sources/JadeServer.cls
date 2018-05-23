"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadeServer
	instanceVariableNames: 'classList classOrganizers readStream writeStream selectedClass methodFilterType methodFilters selections'
	classVariableNames: 'AllGroups AllUsers ClassOrganizer ClientForwarder GemStoneError Globals GsMethodDictionary ProcessorScheduler SymbolDictionary SymbolList System UserGlobals UserProfile'
	poolDictionaries: ''
	classInstanceVariableNames: 'gsString'!
JadeServer guid: (GUID fromString: '{fc038152-9707-4c5f-8977-a1f8d02eb005}')!
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

_allSelectors

	| allSelectors |
	allSelectors := IdentitySet new.
	self classOrganizer classes do: [:each | 
		allSelectors addAll: each selectors; addAll: each class selectors.
	].
	^allSelectors!

abort

	classOrganizers := Array new: 4.
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

	classOrganizers := Array new: 4.
	System beginTransaction.
!

clearBreakAtStepPoint: anInteger inMethod: aGsMethod

	aGsMethod clearBreakAtStepPoint: anInteger.
!

commit

	classOrganizers := Array new: 4.
	^System commitTransaction.
!

contents
	"WriteStream method to identify things that have not yet been flushed to the output. We have flushed everything!!"

	^''.
!

cr

	self nextPut: Character cr.
!

debugString: aString fromContext: anObject environment: anInteger

	anInteger == 0 ifFalse: [self error: 'Only environment 0 is supported in this version!!'].
	^'nil halt. ' , aString
		evaluateInContext: anObject 
		symbolList: GsSession currentSession symbolList. !

delay

	(Delay forMilliseconds: 10) wait.
!

describeMethod: aMethod
	"Provide info needed to create a GsMethod in Jade client"

	writeStream := WriteStream on: String new.
	self _describeMethod: (self homeMethodFor: aMethod).
	^writeStream contents
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

executeString: aString fromContext: anObject environment: anInteger

	anInteger == 0 ifFalse: [self error: 'Only environment 0 is supported in this version!!'].
	^aString
		evaluateInContext: anObject 
		symbolList: GsSession currentSession symbolList. !

homeMethodFor: aGsMethod

	^aGsMethod!

initialize

	classOrganizers := Array new: 4.
	self 
		installTranscript;
		registerOBNotifications;
		yourself.!

installTranscript

	| transcript |
	transcript := self objectInBaseNamed: #'Transcript'.

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

objectInBaseNamed: aString

	^[(SymbolList withAll: self class sharedPools) objectNamed: aString asSymbol] on: Error do: [:ex | ex return: nil].
!

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
	(platform := self objectInBaseNamed: #'OBGemStonePlatform') isNil ifTrue: [^self].
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

setBreakAtStepPoint: anInteger inMethod: aGsMethod

	aGsMethod setBreakAtStepPoint: anInteger.
!

show: anObject

	self nextPutAll: anObject printString.
!

sourceForProcess: gsProcess frame: level

	self subclassResponsibility!

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
!JadeServer categoriesFor: #_allSelectors!Debugger!GsObject!public!System Browser! !
!JadeServer categoriesFor: #abort!public! !
!JadeServer categoriesFor: #asString:!public!Transcript! !
!JadeServer categoriesFor: #beginTransaction!public! !
!JadeServer categoriesFor: #clearBreakAtStepPoint:inMethod:!Methods!public! !
!JadeServer categoriesFor: #commit!public! !
!JadeServer categoriesFor: #contents!public! !
!JadeServer categoriesFor: #cr!public!Transcript! !
!JadeServer categoriesFor: #debugString:fromContext:environment:!public! !
!JadeServer categoriesFor: #delay!public! !
!JadeServer categoriesFor: #describeMethod:!Debugger!GsObject!public!System Browser! !
!JadeServer categoriesFor: #errorListFor:!public! !
!JadeServer categoriesFor: #executeString:fromContext:environment:!public! !
!JadeServer categoriesFor: #homeMethodFor:!public! !
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
!JadeServer categoriesFor: #objectInBaseNamed:!private! !
!JadeServer categoriesFor: #objectNamed:!private! !
!JadeServer categoriesFor: #obTextRequest:!OmniBrowser!public! !
!JadeServer categoriesFor: #oopOf:!private! !
!JadeServer categoriesFor: #refreshSymbolList!public! !
!JadeServer categoriesFor: #registerOBNotifications!public! !
!JadeServer categoriesFor: #registerOBNotificationsForPlatform:clientForwarder:!public! !
!JadeServer categoriesFor: #reset!public! !
!JadeServer categoriesFor: #setBreakAtStepPoint:inMethod:!Methods!public! !
!JadeServer categoriesFor: #show:!public!Transcript! !
!JadeServer categoriesFor: #sourceForProcess:frame:!public! !
!JadeServer categoriesFor: #stackForProcess:!public! !
!JadeServer categoriesFor: #terminate:!Processes!public! !

!JadeServer class methodsFor!

addGsStringForClass: aClass to: aStream

	aStream
		nextPutAll: 'class := ', (self gsClassDefinitionStringFor: aClass); lf;
		nextPutAll: 'readStream := ReadStream on: ''';
		yourself.
	aClass selectors do: [:each | 
		| string |
		string := aClass sourceCodeAt: each.
		string := string reject: [:char | char == Character cr].
		string := string printString.
		aStream 
			nextPutAll: (string copyFrom: 2 to: string size - 1);
			lf; nextPut: $%; lf.
	].
	aStream nextPutAll: '''.
[readStream atEnd not] whileTrue: [
	| line |
	writeStream := WriteStream on: String new.
	[
		line := readStream nextLine.
		line ~= ''%''
	] whileTrue: [
		writeStream nextPutAll: line; lf.
	].
	source := writeStream contents.
	result := class
		compileMethod: source
		dictionaries: symbolList
		category: ''category''.
	result ~~ nil ifTrue: [^methodClass _sourceWithErrors: result fromString: source].
].
'.
!

classVarsForGemStone

	^(self == JadeServer ifTrue: ['ExternalInteger GciError GsObject OopType32 OopType64'] ifFalse: ['']).
!

gsClassDefinitionStringFor: aClass

	self subclassResponsibility.
!

gsMethodClassName

	^'GsMethod'
!

sessionStateCode

	^'System _sessionStateAt: 3 put: server.'! !
!JadeServer class categoriesFor: #addGsStringForClass:to:!public! !
!JadeServer class categoriesFor: #classVarsForGemStone!public! !
!JadeServer class categoriesFor: #gsClassDefinitionStringFor:!public! !
!JadeServer class categoriesFor: #gsMethodClassName!public! !
!JadeServer class categoriesFor: #sessionStateCode!public! !

