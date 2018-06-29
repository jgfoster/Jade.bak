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

_asString: anObject

	^anObject printString
!

_categoryOfMethod: aMethod

	| selector |
	(selector := aMethod selector) isNil ifTrue: [^''].
	^self _behavior: aMethod inClass categoryOfSelector: selector.
!

_currentUserMayEditMethod: aMethod

	| objectSecurityPolicy |
	objectSecurityPolicy := self _objectSecurityPolicyFor: aMethod inClass.
	^self _isPackagePolicyEnabled or: [objectSecurityPolicy isNil or: [objectSecurityPolicy currentUserCanWrite]].
!

_decodeString: aString

	^aString!

_defectiveTestsIn: aClass

	| testClass results stream |
	testClass := aClass thisClass.
	results := testClass suite run.
	stream := self _writeStreamOnString.
	stream nextPutAll: results printString; lf.
	results defects asSet do: [:each | 
		| selector class |
		selector := each selector asSymbol.
		class := each class whichClassIncludesSelector: selector.
		stream nextPutAll: class name , ' debug: ' , each selector printString; lf.
	].
	^self _encodeString: stream contents.
!

_describeMethod: aMethod
	"Private - Provide info needed to create a GsMethod in Jade client"
	"Nice to add packageName and mcTimestamp"

	| allSelectors class list oldGsMethod string x |
	self environment: (self _environmentForMethod: aMethod).
	writeStream 	"Line 1 for GsMethod (line 3 for JadeSystemBrowserPresenter)"
"1"		nextPutAll: (class := aMethod inClass) asOop printString; tab;
"2"		nextPutAll: class printString; tab;
"3"		nextPutAll: aMethod asOop printString; tab;
"4"		nextPutAll: ((x := aMethod selector) isNil ifFalse: [x] ifTrue: ['']); tab;
"5"		nextPutAll: (self _categoryOfMethod: aMethod); tab;
"6"		nextPutAll: (self _currentUserMayEditMethod: aMethod) asString; tab;
		lf.

	"Method source"
	writeStream nextPutAll: (string := aMethod sourceString).
	string last = Character lf ifFalse: [writeStream lf].
	writeStream nextPut: $%; lf.	"Lines 2-N"

	"unimplemented selectors"			"https://github.com/jgfoster/Jade/issues/117"
	((aMethod class includesSelector: #'_selectorPool') and: [aMethod class includesSelector: #'_sourceOffsetOfFirstSendOf:']) ifTrue: [
		allSelectors := self _allSelectors.
		(aMethod _selectorPool reject: [:each | allSelectors includes: each]) do: [:each | 
			(aMethod _sourceOffsetOfFirstSendOf: each) printOn: writeStream.
			writeStream space; nextPutAll: each; tab.
		].
	].
	writeStream lf.	"Line N+1"

	"Array of Associations (offset -> selector) indexed by step points"
	list := self sbUpdateMethodStepPointsFor: aMethod.
	list := list collect: [:each | each key printString , ' ' , each value].
	self writeList: list.	"Line N+2"

	"breaks"
	list := self sbUpdateMethodBreakPointsFor: aMethod.
	self writeList: (list collect: [:each | each printString]).	"Line N+3"

	"original method"
	oldGsMethod := (aMethod inClass class canUnderstand: #'persistentMethodDictForEnv:')
		ifTrue: [(aMethod inClass persistentMethodDictForEnv: 0) at: aMethod selector ifAbsent: [aMethod]]
		ifFalse: [(aMethod inClass class canUnderstand: #'_rawMethodDict')
			ifTrue: [aMethod inClass _rawMethodDict at: aMethod selector ifAbsent: [aMethod]]
			ifFalse: [aMethod]].
	aMethod ~~ oldGsMethod ifTrue: [
		string := oldGsMethod sourceString.
		writeStream nextPutAll: string.
		(string notEmpty and: [string last = Character lf]) ifFalse: [writeStream lf].
	].
	writeStream nextPut: $%; lf.

	"method compile warnings"
	string := selections isNil 
		ifTrue: ['']
		ifFalse: [selections at: #'methodWarnings' ifAbsent: ['']].
	string isNil ifTrue: [string := ''].
	writeStream nextPutAll: string; nextPut: $%; lf.
 !

_encodeString: aString

	^String withAll: (aString collect: [:each | 
		| codePoint |
		codePoint := each codePoint.
		((32 <= codePoint and: [codePoint <= 255]) or: [each == Character tab or: [each == Character lf]]) 
			ifTrue: [each] 
			ifFalse: [$?]
	]).
!

_environmentForMethod: aGsNMethod

	^0!

_environmentSuperClassFor: aBehavior

	^aBehavior superclass!

_gemLogPath

	^''
!

_gsPackagePolicy

	| class |
	class := self _gsPackagePolicyClass.
	class isNil ifTrue: [^nil].
	^class current.
!

_gsPackagePolicyClass

	^self objectInBaseNamed: #'GsPackagePolicy'.
!

_inspectDictionary: aDictionary on: aStream

	| keys keyDict |
	keys := self _keysForDictionary: aDictionary.
	keyDict := Dictionary new.
	keys do: [:each | 
		| key |
		key := each printString , '~' , (self oopOf: each) printString.
		key := key collect: [:char | char asciiValue < 32 ifTrue: [$?] ifFalse: [char]].
		keyDict
			at: key
			put: each.
	].
	keys size printOn: aStream.
	aStream lf.
	keyDict keys asSortedCollection do: [:each | 
		| index keyString key value valueString |
		index := each findLast: [:char | char = $~].
		keyString := each copyFrom: 1 to: index - 1.
		key := keyDict at: each.
		value := aDictionary at: key. 
		valueString := (self printStringOf: value to: 10).
		valueString := self _decodeString: valueString.
		aStream nextPutAll: keyString , '->' , valueString; tab.
		self print: (self oopOf: value) on: aStream.
		aStream lf.
	].
	^aStream 
		lf; 
		contents.
!

_inspectNamedInstanceVariablesOf: anObject on: aStream

	| list size |
	list := anObject class allInstVarNames.
	size := list size.
	anObject class format > 0 ifTrue: [
		size := size + (anObject _basicSize min: 200).
	].
	size printOn: aStream.
	aStream lf.
	1 to: list size do: [:i | 
		aStream nextPutAll: (list at: i); tab.
		self print: (self oopOf: (anObject instVarAt: i)) on: aStream.
		aStream lf.
	].
!

_installTranscript

	| baseName className transcript |
	transcript := self objectNamed: #'Transcript'.
	baseName := 'JadeServer'.
	className := transcript class name.
	"If no Transcript object, then install me!!"
	(transcript == nil or: [className size >= baseName size and: [(className copyFrom: 1 to: baseName size) = baseName]]) ifTrue: [
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

_isClientForwarder: anObject

	^anObject _class name == #'ClientForwarder'.
!

_keysForDictionary: aDictionary 

	^aDictionary keys.
!

_mcHttpRepositoryClass

	^self objectInBaseNamed: #'MCHttpRepository'.
!

_mcInitialsA: aString
	"Private - Subclasses provide error handling, typically means string is too long"

	System _cacheName: aString.
!

_mcPackageClass

	^self objectInBaseNamed: #'MCPackage'.
!

_methodSignatureForSelector: aSymbol

	^aSymbol.
!

_millisecondsElapsedTime: aBlock

	^Time millisecondsElapsedTime: aBlock.
!

_moveMethod: aGsMethod toCategory: aString

	aGsMethod inClass
		moveMethod: aGsMethod selector
		toCategory: aString.
!

_nameOfFirstDictionaryReferencing: aGlobal

	| list |
	list := self symbolList dictionaryAndSymbolOf: aGlobal.
	list isNil ifTrue: [^''].
	^list first name!

_nextLine

	^readStream upTo: Character lf.
!

_nextLineAsList

	^(self _nextLine subStrings: Character tab) reject: [:each | each isEmpty].
!

_objectForOop: anInteger

	self subclassResponsibility.!

_refreshSymbolList

	GsSession currentSession symbolList replaceElementsFrom: self symbolList.	"replace the transcient one with the persistent one"
!

_registerOBNotifications

	| platform clientForwarder |
	(platform := self objectInBaseNamed: #'OBGemStonePlatform') isNil ifTrue: [^self].
	clientForwarder := ClientForwarder new.
	clientForwarder	clientObject: 1.
	self
		_registerOBNotificationsForPlatform: platform 
		clientForwarder: clientForwarder.
!

_registerOBNotificationsForPlatform: platform clientForwarder: clientForwarder

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

_string

	^String new!

_writeStreamOnString

	^WriteStream on: self _string!

abort

	classOrganizers := Array new: 4.
	System abortTransaction.
	self _refreshSymbolList.
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
	"Transcript protocol"

	self nextPut: Character cr.
!

debugString: aString fromContext: anObject environment: anInteger

	anInteger == 0 ifFalse: [self error: 'Only environment 0 is supported in this version!!'].
	^'nil halt. ' , (self _decodeString: aString)
		evaluateInContext: anObject 
		symbolList: GsSession currentSession symbolList. !

describeMethod: aMethod
	"Provide info needed to create a GsMethod in Jade client"

	writeStream := self _writeStreamOnString.
	self _describeMethod: (self homeMethodFor: aMethod).
	^self _encodeString: writeStream contents
!

errorListFor: aCollection

	| stream |
	aCollection class name == #'ErrorDescription' ifTrue: [^''].
	stream := self _writeStreamOnString.
	aCollection do: [:each | 
		stream
			nextPutAll: (each at: 1) printString; tab;
			nextPutAll: (each at: 2) printString; tab;
			nextPutAll: ((2 < each size and: [(each at: 3) notNil]) ifTrue: [(each at: 3)] ifFalse: [(GemStoneError at: #English) at: (each at: 1)]); tab;
			lf.
	].
	^self _encodeString: stream contents.
!

executeString: aString fromContext: anObject environment: anInteger

	anInteger == 0 ifFalse: [self error: 'Only environment 0 is supported in this version!!'].
	^(self _decodeString: aString)
		evaluateInContext: anObject 
		symbolList: GsSession currentSession symbolList. !

homeMethodFor: aGsMethod

	^aGsMethod!

initialize

UserGlobals at: #'James' put: Time now printString , ' - initialize'.
	classOrganizers := Array new: 4.
	self 
		_installTranscript;
		_registerOBNotifications;
		yourself.!

makeListener

	^nil.
!

mcRemoveRepository: aRepository fromPackage: aMCWorkingCopy

	aMCWorkingCopy repositoryGroup removeRepository: aRepository.
!

nextPut: aCharacter
	"Transcript protocol"

	self nextPutAll: aCharacter asString.
!

nextPutAll: anObject
	"Transcript protocol"

	| string args |
	string := self _asString: anObject.
	string := self _encodeString: string.
	args := Array
		with: self
		with: 2
		with: #'nextPutAll:'
		with: (Array with: string).
	System
		signal: 2336
		args: args
		signalDictionary: GemStoneError.
!

obConfirmationRequest: anOBConfirmationRequest

	| string |
	string := String new 
		addAll: anOBConfirmationRequest cancelChoice;
		add: Character lf;
		addAll: anOBConfirmationRequest okChoice;
		add: Character lf;
		addAll: anOBConfirmationRequest prompt;
		yourself.
	^self _encodeString: string!

obInformRequest: anOBInformRequest

	^self _encodeString: anOBInformRequest message!

objectInBaseNamed: aString

	| object |
	object := [(SymbolList withAll: self class sharedPools) objectNamed: aString asSymbol] on: Error do: [:ex | ex return: nil].
	object isNil ifTrue: [object := self objectNamed: aString].	"If not found in DataCurator, then check local user (https://github.com/jgfoster/Jade/issues/150)"
	^object
!

objectNamed: aString

	^System myUserProfile objectNamed: aString asSymbol.
!

obTextRequest: anOBTextRequest

	| prompt string template |
	prompt := anOBTextRequest prompt.
	template := anOBTextRequest template.
	string := String new 
		addAll: prompt size printString;
		add: Character lf;
		addAll: prompt;
		addAll: template;
		yourself.
	^self _encodeString: string!

oopOf: anObject

	^anObject asOop.
!

reset
	"WriteStream protocol used by Transcript"!

setBreakAtStepPoint: anInteger inMethod: aGsMethod

	aGsMethod setBreakAtStepPoint: anInteger.
!

show: anObject
	"Transcript protocol"

	self nextPutAll: anObject printString.
!

stackForProcess: aGsProcess

	| array stream |
	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | nil].
	array := aGsProcess _reportOfSize: 5000.
	stream := self _writeStreamOnString.
	array do: [:each | 
		stream nextPutAll: each; lf.
	].
	^self _encodeString: stream contents.
!

terminate: aGsProcess

	aGsProcess isNil ifTrue: [^self].
	aGsProcess terminate.
	(Delay forMilliseconds: 10) wait.	"allow forked processes to finish"
! !
!JadeServer categoriesFor: #_addToPureExportSet:!private! !
!JadeServer categoriesFor: #_allSelectors!Debugger!GsObject!private!System Browser! !
!JadeServer categoriesFor: #_asString:!private!Transcript! !
!JadeServer categoriesFor: #_categoryOfMethod:!private!System Browser! !
!JadeServer categoriesFor: #_currentUserMayEditMethod:!private!System Browser! !
!JadeServer categoriesFor: #_decodeString:!private! !
!JadeServer categoriesFor: #_defectiveTestsIn:!private! !
!JadeServer categoriesFor: #_describeMethod:!Debugger!GsObject!private!System Browser! !
!JadeServer categoriesFor: #_encodeString:!private! !
!JadeServer categoriesFor: #_environmentForMethod:!private! !
!JadeServer categoriesFor: #_environmentSuperClassFor:!private!System Browser! !
!JadeServer categoriesFor: #_gemLogPath!private!Transcript! !
!JadeServer categoriesFor: #_gsPackagePolicy!private! !
!JadeServer categoriesFor: #_gsPackagePolicyClass!private! !
!JadeServer categoriesFor: #_inspectDictionary:on:!private! !
!JadeServer categoriesFor: #_inspectNamedInstanceVariablesOf:on:!private! !
!JadeServer categoriesFor: #_installTranscript!private!Transcript! !
!JadeServer categoriesFor: #_isClientForwarder:!Debugger!private! !
!JadeServer categoriesFor: #_keysForDictionary:!private! !
!JadeServer categoriesFor: #_mcHttpRepositoryClass!private! !
!JadeServer categoriesFor: #_mcInitialsA:!private! !
!JadeServer categoriesFor: #_mcPackageClass!private! !
!JadeServer categoriesFor: #_methodSignatureForSelector:!private!System Browser! !
!JadeServer categoriesFor: #_millisecondsElapsedTime:!private!System Browser! !
!JadeServer categoriesFor: #_moveMethod:toCategory:!private! !
!JadeServer categoriesFor: #_nameOfFirstDictionaryReferencing:!private! !
!JadeServer categoriesFor: #_nextLine!private!System Browser! !
!JadeServer categoriesFor: #_nextLineAsList!private!System Browser! !
!JadeServer categoriesFor: #_objectForOop:!private! !
!JadeServer categoriesFor: #_refreshSymbolList!private! !
!JadeServer categoriesFor: #_registerOBNotifications!private! !
!JadeServer categoriesFor: #_registerOBNotificationsForPlatform:clientForwarder:!private! !
!JadeServer categoriesFor: #_string!private! !
!JadeServer categoriesFor: #_writeStreamOnString!private! !
!JadeServer categoriesFor: #abort!public! !
!JadeServer categoriesFor: #beginTransaction!public! !
!JadeServer categoriesFor: #clearBreakAtStepPoint:inMethod:!Methods!public! !
!JadeServer categoriesFor: #commit!public! !
!JadeServer categoriesFor: #contents!public! !
!JadeServer categoriesFor: #cr!public!Transcript! !
!JadeServer categoriesFor: #debugString:fromContext:environment:!public! !
!JadeServer categoriesFor: #describeMethod:!Debugger!GsObject!public!System Browser! !
!JadeServer categoriesFor: #errorListFor:!public! !
!JadeServer categoriesFor: #executeString:fromContext:environment:!public! !
!JadeServer categoriesFor: #homeMethodFor:!private! !
!JadeServer categoriesFor: #initialize!public! !
!JadeServer categoriesFor: #makeListener!private! !
!JadeServer categoriesFor: #mcRemoveRepository:fromPackage:!public! !
!JadeServer categoriesFor: #nextPut:!public!Transcript! !
!JadeServer categoriesFor: #nextPutAll:!public!Transcript! !
!JadeServer categoriesFor: #obConfirmationRequest:!OmniBrowser!public! !
!JadeServer categoriesFor: #obInformRequest:!OmniBrowser!public! !
!JadeServer categoriesFor: #objectInBaseNamed:!private! !
!JadeServer categoriesFor: #objectNamed:!private! !
!JadeServer categoriesFor: #obTextRequest:!OmniBrowser!public! !
!JadeServer categoriesFor: #oopOf:!private! !
!JadeServer categoriesFor: #reset!public!Transcript! !
!JadeServer categoriesFor: #setBreakAtStepPoint:inMethod:!Methods!public! !
!JadeServer categoriesFor: #show:!public!Transcript! !
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

gsString
"
	JadeServer64bit3x gsString.
"
	| stream |
	SessionManager current isRuntime ifTrue: [^gsString].
	(stream := WriteStream on: String new)
		nextPutAll: '| class mcPlatformSupport methodClass readStream result server source symbolList writeStream |'; lf;
		nextPutAll: 'methodClass := ';
		nextPutAll: self gsMethodClassName;
		nextPut: $.; lf;
		nextPutAll: 'symbolList := System myUserProfile symbolList.'; lf;
		nextPutAll: 'symbolList := (AllUsers userWithId: ''DataCurator'') symbolList.'; lf;
		nextPutAll: 'symbolList := symbolList class new add: (symbolList detect: [:each | each name == #UserGlobals]); add: (symbolList detect: [:each | each name == #Globals]); yourself.'; lf;
		nextPutAll: '[class := symbolList objectNamed: #Object.'; lf;
		yourself.
	(self withAllSuperclasses remove: Object; yourself) reverseDo: [:eachClass | self addGsStringForClass: eachClass to: stream].
	stream 
		nextPutAll: '(mcPlatformSupport := symbolList objectNamed: #''MCPlatformSupport'') notNil ifTrue: ['; lf;
		nextPutAll: '	mcPlatformSupport autoCommit: false; autoMigrate: false].'; lf;
		nextPutAll: 'class := ((symbolList objectNamed: #System) myUserProfile symbolList objectNamed: #UserGlobals) at: #' , self name , ' ifAbsent: [class].'; lf;
		nextPutAll: 'server := class new initialize; yourself.'; lf;
		nextPutAll: self sessionStateCode; lf;
		nextPutAll: 'server] on: (symbolList objectNamed: #Error) do: [:ex | ex return: ''ERROR: '' , (GsProcess stackReportToLevel: 100)]';
		yourself.
	gsString := stream contents.
	^gsString.
!

new

	^self basicNew 
		initialize;
		yourself!

sessionStateCode

	^'System _sessionStateAt: 3 put: server.'! !
!JadeServer class categoriesFor: #addGsStringForClass:to:!public! !
!JadeServer class categoriesFor: #classVarsForGemStone!public! !
!JadeServer class categoriesFor: #gsClassDefinitionStringFor:!public! !
!JadeServer class categoriesFor: #gsMethodClassName!public! !
!JadeServer class categoriesFor: #gsString!public! !
!JadeServer class categoriesFor: #new!public! !
!JadeServer class categoriesFor: #sessionStateCode!public! !

