"Filed out from Dolphin Smalltalk 7"!

JadeServer64bit24 subclass: #JadeServer64bit3x
	instanceVariableNames: 'environment'
	classVariableNames: 'Admonition ClientForwarderSend CompileError CompileWarning RubyHash SessionTemps Transcript'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer64bit3x guid: (GUID fromString: '{1dc3debb-81ec-4b7b-872e-82229e88781b}')!
JadeServer64bit3x comment: '(System _sessionStateAt: 3).
GciSession allInstances do: [:each | each initializeServer].'!
!JadeServer64bit3x categoriesForClass!Unclassified! !
!JadeServer64bit3x methodsFor!

_allSelectors

	| allSelectors |
	allSelectors := IdentitySet new.
	self classOrganizer classes do: [:each | 
		allSelectors addAll: (each selectorsForEnvironment: environment); addAll: (each class selectorsForEnvironment: environment).
	].
	^allSelectors!

_asString: anObject

	(anObject isKindOf: String) ifTrue: [^anObject].
	(anObject _class name == #'ClientForwarder') ifTrue: [^'aClientForwarder(' , (self _asString: anObject clientObject) , ')'].
	^[
		super _asString: anObject.
	] on: Error , Admonition do: [:ex | 
		ex return: '<<#printString error: ' , ex description , '>>'.
	].
!

_categoryOfMethod: aMethod

	| category selector |
	(selector := aMethod selector) isNil ifTrue: [^''].
	category := self _behavior: aMethod inClass categoryOfSelector: aMethod selector.
	category ifNil: [category := #'other'].
	^category.
!

_describeMethod: aMethod

	(aMethod class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , aMethod class name].
	^super _describeMethod: aMethod!

_environmentForMethod: aGsNMethod

	^aGsNMethod environmentId!

_inspectNamedInstanceVariablesOf: anObject on: aStream

	| list dynamic size |
	list := anObject class allInstVarNames.
	dynamic := anObject dynamicInstanceVariables.
	size := list size + dynamic size.
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
	1 to: dynamic size do: [:i | 
		aStream nextPutAll: (dynamic at: i); tab.
		self print: (self oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) on: aStream.
		aStream lf.
	].
!

_installTranscript

	| sessionTemps |
	Transcript class name == #'TranscriptStreamPortable' ifFalse: [^super _installTranscript].
	sessionTemps := SessionTemps current.
	(sessionTemps at: #'TranscriptStream_SessionStream' ifAbsent: [nil]) notNil ifTrue: [^super _installTranscript].
	sessionTemps at: #'TranscriptStream_SessionStream' put: self.
!

_keysForDictionary: aDictionary 
	"Private - RubyHash does not implement #'keys' or #'keysDo:'!!"

	| keys |
	(aDictionary isKindOf: RubyHash) ifFalse: [^super _keysForDictionary: aDictionary].
	keys := Set new.
	aDictionary keysAndValuesDo: [:eachKey :eachValue | keys add: eachKey].
	^keys.
!

_methodSignatureForSelector: aSymbol
	"Private - Ruby bridge methods can have some strange selectors!!"

	| class comma i j method source |
	environment ~~ 1 ifTrue: [^aSymbol].
	class := selectedClass whichClassIncludesSelector: aSymbol environmentId: environment.
	method := class compiledMethodAt: aSymbol environmentId: environment.
	source := (method sourceString subStrings: Character lf) first trimBlanks.
	(4 < source size and: [(source copyFrom: 1 to: 4) = 'def ']) ifTrue: [
		source := source copyFrom: 5 to: source size.
		(source includes: $#) ifTrue: [source := (source copyFrom: 1 to: (source indexOf: $#) - 1) trimBlanks].
		^source.
	].
	(i := aSymbol indexOf: $#) == 0 ifTrue: [^aSymbol].
	source := aSymbol copyFrom: 1 to: i - 1.
	(aSymbol copyFrom: i to: aSymbol size) = '#0__' ifTrue: [^source].
	comma := ''.
	source add: $(.
	j := (aSymbol at: i + 1) asString asNumber.
	1 to: j do: [:k | 
		source 
			add: comma;
			add: 'arg'.
		1 < j ifTrue: [source add: k printString].
		comma := $,.
	].
	(aSymbol at: i + 2) == $* ifTrue: [
		source 
			add: comma;
			add: (0 == j ifTrue: ['args'] ifFalse: ['rest']).
		comma := $,.
	].
	aSymbol last == $& ifTrue: [
		source
			add: comma;
			add: '&block'.
	].
	source add: $).
	^source.
!

debugString: aString fromContext: anObject environment: anInteger

	[
		^super debugString: aString fromContext: anObject environment: anInteger.
	] on: CompileWarning do: [:ex | 
		ex resume.
	].
!

executeString: aString fromContext: anObject environment: anInteger

	[
		^super 
			executeString: aString 
			fromContext: anObject 
			environment: anInteger.
	] on: CompileWarning do: [:ex | 
		ex resume.
	].
!

gemCacheStatisticsForSessionId: anInteger

^System gemCacheStatisticsForSessionId: anInteger!

homeMethodFor: aGsMethod

	| result |
	result := super homeMethodFor: aGsMethod.
	(result class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , result class name].
	^result!

nextPutAll: anObject

	| exception |
	exception := ClientForwarderSend new 
		receiver: self 
		clientObj: 2
		selector:#'nextPutAll:'
		args: (Array with: (self _asString: anObject)).
	exception defaultAction.  "return error direct to GCI"! !
!JadeServer64bit3x categoriesFor: #_allSelectors!Debugger!GsObject!private!System Browser! !
!JadeServer64bit3x categoriesFor: #_asString:!private!Transcript! !
!JadeServer64bit3x categoriesFor: #_categoryOfMethod:!private!System Browser! !
!JadeServer64bit3x categoriesFor: #_describeMethod:!private! !
!JadeServer64bit3x categoriesFor: #_environmentForMethod:!private! !
!JadeServer64bit3x categoriesFor: #_inspectNamedInstanceVariablesOf:on:!private!Transcript! !
!JadeServer64bit3x categoriesFor: #_installTranscript!private!Transcript! !
!JadeServer64bit3x categoriesFor: #_keysForDictionary:!private! !
!JadeServer64bit3x categoriesFor: #_methodSignatureForSelector:!private!System Browser! !
!JadeServer64bit3x categoriesFor: #debugString:fromContext:environment:!public! !
!JadeServer64bit3x categoriesFor: #executeString:fromContext:environment:!public! !
!JadeServer64bit3x categoriesFor: #gemCacheStatisticsForSessionId:!public!Sessions! !
!JadeServer64bit3x categoriesFor: #homeMethodFor:!private! !
!JadeServer64bit3x categoriesFor: #nextPutAll:!public!Transcript! !

!JadeServer64bit3x class methodsFor!

addRangeStopTo: stepPoints withSource: source
 1 to: stepPoints size do: [:stepPoint |
		| range start char length |
		range := (stepPoints at: stepPoint) key.
		start := range start.
		char := source at: start.
		length := (char isAlphaNumeric or: [char = $_])
			ifTrue: [(source copyFrom: start + 1 to: source size) findFirst: [:eachChar | (eachChar isAlphaNumeric or: [eachChar = $_ or: [eachChar = $:]]) not]]
			ifFalse: [2].
		length = 0 ifTrue: [length := source size - start].
		[
			2 < length and: [(source at: start) = $_].
		] whileTrue: [
			start := start + 1.
			length := length - 1.
		].
		range
			start: start;
			stop: start + length - 1;
			yourself.
	].!

gsMethodClassName

	^'GsNMethod'
! !
!JadeServer64bit3x class categoriesFor: #addRangeStopTo:withSource:!public! !
!JadeServer64bit3x class categoriesFor: #gsMethodClassName!public! !

