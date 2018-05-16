"Filed out from Dolphin Smalltalk 7"!

Object subclass: #STONReader
	instanceVariableNames: 'readStream objects classes unresolvedReferences stringStream allowComplexMapKeys stack'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STONReader guid: (GUID fromString: '{b5e7f683-a071-41ac-b501-1335cad0e600}')!
STONReader comment: ''!
!STONReader categoriesForClass!Unclassified! !
!STONReader methodsFor!

allowComplexMapKeys: boolean
	allowComplexMapKeys := boolean!

atEnd
	^readStream atEnd!

close
	readStream
		ifNotNil: 
			[readStream close.
			readStream := nil]!

consumeWhitespace
	"Strip whitespaces from the input stream."

	[readStream atEnd not and: [readStream peek isSeparator]] whileTrue: [readStream next]!

error: aString
	| streamPosition |
	"Remain compatible with streams that don't understand #position"
	streamPosition := [readStream position] on: MessageNotUnderstood do: [nil].
	^STONReaderError signal: aString streamPosition: streamPosition!

expectChar: character
	"Expect character and consume input and optional whitespace at the end,
	 throw an error otherwise."

	(self matchChar: character) ifFalse: [self error: character asString , ' expected']!

initialize
	objects := IdentityDictionary new.
	classes := IdentityDictionary new.
	allowComplexMapKeys := false.
	stack := OrderedCollection new.
	unresolvedReferences := 0!

isClassChar: char
	^'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' includes: char!

isClassStartChar: char
	^'ABCDEFGHIJKLMNOPQRSTUVWXYZ' includes: char!

isSimpleSymbolChar: char
	^'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./' includes: char!

lookupClass: name
	"seems there ought to be a more robust way to lookup a class"

	^Smalltalk at: name ifAbsent: [self error: 'Class not found - ' , name]!

match: string do: block
	"Try to read and consume string and execute block if successful.
	Else do nothing (but do not back up)"

	(string allSatisfy: [:each | readStream peekFor: each])
		ifTrue: 
			[self consumeWhitespace.
			block value]!

matchChar: character
	"Tries to match character, consume input and 
	answer true if successful and consumes whitespace at the end."

	^(readStream peekFor: character)
		ifTrue: 
			[self consumeWhitespace.
			true]
		ifFalse: [false]!

newReference
	| index reference |
	index := objects size + 1.
	reference := STONReference index: index.
	objects at: index put: reference.
	^reference!

next
	| object |
	self consumeWhitespace.
	object := self parseValue.
	unresolvedReferences > 0 ifTrue: [self processSubObjectsOf: object].
	^object!

on: aReadStream
	readStream := aReadStream!

optimizeForLargeStructures
	"nothing special for GemStone"

	!

parseCharacter
	| char |
	(char := readStream next) = $\ ifFalse: [^char].
	(#($' $" $/ $\) includes: (char := readStream next)) ifTrue: [^char].
	char = $b ifTrue: [^Character backspace].
	char = $f ifTrue: [^Character newPage].
	char = $n ifTrue: [^Character lf].
	char = $r ifTrue: [^Character cr].
	char = $t ifTrue: [^Character tab].
	char = $u ifTrue: [^self parseCharacterHex].
	self error: 'invalid escape character \' , (String with: char)!

parseCharacterHex
	| value |
	value := self parseCharacterHexDigit.
	3 timesRepeat: [value := (value bitShift: 4) + self parseCharacterHexDigit].
	^Character codePoint: value!

parseCharacterHexDigit
	| digit |
	readStream atEnd
		ifFalse: 
			[digit := readStream next asInteger.
			(digit between: 48 and: 57) ifTrue: [^digit - 48].	"$0"	"$9"
			(digit between: 65 and: 70) ifTrue: [^digit - 55].	"$A"	"$F"
			(digit between: 97 and: 102) ifTrue: [^digit - 87]	"$a"	"$f"].
	self error: 'hex-digit expected'!

parseClass
	| className |
	className := self stringStreamContents: 
					[:stream |
					[readStream atEnd not and: [self isClassChar: readStream peek]]
						whileTrue: [stream nextPut: readStream next]].
	self consumeWhitespace.
	^self lookupClass: className asSymbol!

parseConstantDo: block
	"Parse and consume either true|false|nil|null and execute block 
	or else do nothing (but do not back up).
	Hand written implementation to avoid the use of #position:"

	readStream peek = $t ifTrue: [^self match: 'true' do: [block value: true]].
	readStream peek = $f ifTrue: [^self match: 'false' do: [block value: false]].
	readStream peek = $n
		ifTrue: 
			[readStream next.
			readStream peek = $i ifTrue: [self match: 'il' do: [block value: nil]].
			readStream peek = $u ifTrue: [self match: 'ull' do: [block value: nil]]]!

parseList
	| reference array |
	reference := self newReference.
	array := STON listClass streamContents: [:stream | self parseListDo: [:each | stream nextPut: each]].
	self setReference: reference to: array.
	^array!

parseListDo: block
	| index |
	self expectChar: $[.
	(self matchChar: $]) ifTrue: [^self].
	index := 1.
	[readStream atEnd] whileFalse: 
			[block cull: self parseValue cull: index.
			(self matchChar: $]) ifTrue: [^self].
			index := index + 1.
			self expectChar: $,].
	self error: 'end of list expected'!

parseListSingleton
	| value |
	value := nil.
	self parseListDo: [:each :index | index = 1 ifTrue: [value := each]].
	^value!

parseMap
	| map |
	map := STON mapClass new.
	self storeReference: map.
	self parseMapDo: [:key :value | map at: key put: value].
	^map!

parseMapDo: block
	self expectChar: ${.
	(self matchChar: $}) ifTrue: [^self].
	[readStream atEnd] whileFalse: 
			[| name value |
			name := self parseValue.
			(allowComplexMapKeys or: [name isString or: [name isNumber]])
				ifFalse: [self error: 'unexpected property name type'].
			self expectChar: $:.
			value := self parseValue.
			block value: name value: value.
			(self matchChar: $}) ifTrue: [^self].
			self expectChar: $,].
	self error: 'end of map expected'!

parseNumber
	| negated number |
	negated := readStream peekFor: $-.
	number := self parseNumberInteger.
	(readStream peekFor: $.) ifTrue: [number := number + self parseNumberFraction].
	((readStream peekFor: $e) or: [readStream peekFor: $E])
		ifTrue: [number := number * self parseNumberExponent].
	negated ifTrue: [number := number negated].
	self consumeWhitespace.
	^number!

parseNumberExponent
	| number negated |
	number := 0.
	(negated := readStream peekFor: $-) ifFalse: [readStream peekFor: $+].
	[readStream atEnd not and: [readStream peek isDigit]]
		whileTrue: [number := 10 * number + readStream next digitValue].
	negated ifTrue: [number := number negated].
	^10 raisedTo: number!

parseNumberFraction
	| number power |
	number := 0.
	power := 1.0.
	[readStream atEnd not and: [readStream peek isDigit]] whileTrue: 
			[number := 10 * number + readStream next digitValue.
			power := power * 10.0].
	^number / power!

parseNumberInteger
	| number |
	number := 0.
	[readStream atEnd not and: [readStream peek isDigit]]
		whileTrue: [number := 10 * number + readStream next digitValue].
	^number!

parseObject
	| targetClass reference object |
	targetClass := self parseClass.
	reference := self newReference.
	object := targetClass fromSton: self.
	self setReference: reference to: object.
	^object!

parseReference
	| index |
	self expectChar: $@.
	index := self parseNumberInteger.
	self consumeWhitespace.
	unresolvedReferences := unresolvedReferences + 1.
	^STONReference index: index!

parseString
	^self parseStringInternal!

parseStringInternal
	| result delimiter |
	delimiter := readStream next.
	(delimiter = $' or: [delimiter = $"]) ifFalse: [self error: ''' or " expected'].
	result := self stringStreamContents: 
					[:stream |
					[readStream atEnd or: [readStream peek = delimiter]]
						whileFalse: [stream nextPut: self parseCharacter]].
	self expectChar: delimiter.
	^result!

parseSymbol
	| string |
	self expectChar: $#.
	readStream peek = $' ifTrue: [^self parseStringInternal asSymbol].
	string := self stringStreamContents: 
					[:stream |
					[readStream atEnd not and: [self isSimpleSymbolChar: readStream peek]]
						whileTrue: [stream nextPut: readStream next]].
	string isEmpty
		ifFalse: 
			[self consumeWhitespace.
			^string asSymbol].
	self error: 'unexpected input'!

parseValue
	| char |
	readStream atEnd
		ifFalse: 
			[(self isClassStartChar: (char := readStream peek)) ifTrue: [^self parseObject].
			char = ${ ifTrue: [^self parseMap].
			char = $[ ifTrue: [^self parseList].
			(char = $' or: [char = $"]) ifTrue: [^self parseString].
			char = $# ifTrue: [^self parseSymbol].
			char = $@ ifTrue: [^self parseReference].
			(char = $- or: [char isDigit]) ifTrue: [^self parseNumber].
			self parseConstantDo: [:value | ^value]].
	self error: 'invalid input'!

processSubObjectsOf: object
	stack addFirst: object.
	[stack isEmpty] whileFalse: 
			[stack removeFirst stonProcessSubObjects: 
					[:each |
					each isStonReference
						ifTrue: [self resolveReference: each]
						ifFalse: [each stonContainSubObjects ifTrue: [stack addFirst: each] ifFalse: [each]]]]!

reset
	unresolvedReferences := 0.
	objects removeAll!

resolveReference: reference
	^self resolveReferenceIndex: reference index!

resolveReferenceIndex: index
	^objects at: index!

setReference: reference to: object
	objects at: reference index put: object!

storeReference: object
	| index |
	index := objects size + 1.
	objects at: index put: object.
	^index!

stringStreamContents: block
	stringStream ifNil: [stringStream := WriteStream on: String new].
	stringStream reset.
	block value: stringStream.
	^stringStream contents! !
!STONReader categoriesFor: #allowComplexMapKeys:!public! !
!STONReader categoriesFor: #atEnd!public! !
!STONReader categoriesFor: #close!public! !
!STONReader categoriesFor: #consumeWhitespace!public! !
!STONReader categoriesFor: #error:!public! !
!STONReader categoriesFor: #expectChar:!public! !
!STONReader categoriesFor: #initialize!public! !
!STONReader categoriesFor: #isClassChar:!public! !
!STONReader categoriesFor: #isClassStartChar:!public! !
!STONReader categoriesFor: #isSimpleSymbolChar:!public! !
!STONReader categoriesFor: #lookupClass:!public! !
!STONReader categoriesFor: #match:do:!public! !
!STONReader categoriesFor: #matchChar:!public! !
!STONReader categoriesFor: #newReference!public! !
!STONReader categoriesFor: #next!public! !
!STONReader categoriesFor: #on:!public! !
!STONReader categoriesFor: #optimizeForLargeStructures!public! !
!STONReader categoriesFor: #parseCharacter!public! !
!STONReader categoriesFor: #parseCharacterHex!public! !
!STONReader categoriesFor: #parseCharacterHexDigit!public! !
!STONReader categoriesFor: #parseClass!public! !
!STONReader categoriesFor: #parseConstantDo:!public! !
!STONReader categoriesFor: #parseList!public! !
!STONReader categoriesFor: #parseListDo:!public! !
!STONReader categoriesFor: #parseListSingleton!public! !
!STONReader categoriesFor: #parseMap!public! !
!STONReader categoriesFor: #parseMapDo:!public! !
!STONReader categoriesFor: #parseNumber!public! !
!STONReader categoriesFor: #parseNumberExponent!public! !
!STONReader categoriesFor: #parseNumberFraction!public! !
!STONReader categoriesFor: #parseNumberInteger!public! !
!STONReader categoriesFor: #parseObject!public! !
!STONReader categoriesFor: #parseReference!public! !
!STONReader categoriesFor: #parseString!public! !
!STONReader categoriesFor: #parseStringInternal!public! !
!STONReader categoriesFor: #parseSymbol!public! !
!STONReader categoriesFor: #parseValue!public! !
!STONReader categoriesFor: #processSubObjectsOf:!public! !
!STONReader categoriesFor: #reset!public! !
!STONReader categoriesFor: #resolveReference:!public! !
!STONReader categoriesFor: #resolveReferenceIndex:!public! !
!STONReader categoriesFor: #setReference:to:!public! !
!STONReader categoriesFor: #storeReference:!public! !
!STONReader categoriesFor: #stringStreamContents:!public! !

!STONReader class methodsFor!

new
	^self basicNew
		initialize;
		yourself!

on: readStream
	^self new
		on: readStream;
		yourself! !
!STONReader class categoriesFor: #new!public! !
!STONReader class categoriesFor: #on:!public! !

