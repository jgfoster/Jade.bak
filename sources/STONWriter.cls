"Filed out from Dolphin Smalltalk 7"!

Object subclass: #STONWriter
	instanceVariableNames: 'writeStream prettyPrint newLine jsonMode referencePolicy level objects'
	classVariableNames: 'STONCharacters STONSimpleSymbolCharacters'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STONWriter guid: (GUID fromString: '{bcc77339-01c8-4655-b759-4ef550a5b65f}')!
STONWriter comment: ''!
!STONWriter categoriesForClass!Unclassified! !
!STONWriter methodsFor!

close
	writeStream
		ifNotNil: 
			[writeStream close.
			writeStream := nil]!

encodeCharacter: char
	| code encoding |
	((code := char codePoint) < 127 and: [(encoding := STONCharacters at: code + 1) notNil])
		ifTrue: 
			[encoding = #pass ifTrue: [writeStream nextPut: char] ifFalse: [writeStream nextPutAll: encoding]]
		ifFalse: 
			[| paddedStream padding digits |
			paddedStream := WriteStream on: String new.
			code
				printOn: paddedStream
				base: 16
				showRadix: false.
			digits := paddedStream contents.
			padding := 4 - digits size.
			writeStream nextPutAll: '\u'.
			encoding := padding > 0
						ifTrue: [((String new: padding)
								atAllPut: $0;
								yourself) , digits]
						ifFalse: [digits].
			writeStream nextPutAll: encoding]!

encodeKey: key value: value
	self nextPut: key.
	self prettyPrintSpace.
	writeStream nextPut: $:.
	self prettyPrintSpace.
	self nextPut: value!

encodeList: elements
	writeStream nextPut: $[.
	elements isEmpty
		ifTrue: [self prettyPrintSpace]
		ifFalse: 
			[self indentedDo: 
					[self newlineIndent.
					elements do: [:each | self nextPut: each] separatedBy: [self listElementSeparator]].
			self newlineIndent].
	writeStream nextPut: $]!

encodeMap: pairs
	| first |
	first := true.
	writeStream nextPut: ${.
	pairs isEmpty
		ifTrue: [self prettyPrintSpace]
		ifFalse: 
			[self indentedDo: 
					[self newlineIndent.
					pairs keysAndValuesDo: 
							[:key :value |
							first ifTrue: [first := false] ifFalse: [self mapElementSeparator].
							self encodeKey: key value: value]].
			self newlineIndent].
	writeStream nextPut: $}!

encodeString: string
	| encodedString |
	encodedString := string.
	writeStream nextPut: (jsonMode ifTrue: [$"] ifFalse: [$']).
	encodedString do: [:each | self encodeCharacter: each].
	writeStream nextPut: (jsonMode ifTrue: [$"] ifFalse: [$'])!

indentedDo: block
	level := level + 1.
	block value.
	level := level - 1!

initialize
	prettyPrint := false.
	newLine := String with: Character cr.
	level := 0.
	referencePolicy := #normal.
	jsonMode := false.
	objects := IdentityDictionary new!

isSimpleSymbol: symbol
	symbol isEmpty ifTrue: [^false].
	^(self class
		findFirstInString: symbol
		inSet: STONSimpleSymbolCharacters
		startingAt: 1) = 0!

jsonMode: boolean
	jsonMode := boolean!

listElementSeparator
	writeStream nextPut: $,.
	self newlineIndent!

mapElementSeparator
	writeStream nextPut: $,.
	self newlineIndent!

newLine: string
	newLine := string!

newlineIndent
	prettyPrint
		ifTrue: 
			[writeStream nextPutAll: newLine.
			level timesRepeat: [writeStream tab]]!

nextPut: anObject
	anObject stonOn: self !

on: aWriteStream
	writeStream := aWriteStream!

optimizeForLargeStructures
	"nothing special for GemStone"

	!

prettyPrint: boolean
	prettyPrint := boolean!

prettyPrintSpace
	prettyPrint ifTrue: [writeStream space]!

referencePolicy: policy
	(#(#normal #ignore #error) includes: policy)
		ifFalse: [self error: 'Unknown reference policy: ' , policy printString].
	referencePolicy := policy!

reset
	objects removeAll!

shortListElementSeparator
	writeStream nextPut: $,.
	self prettyPrintSpace!

with: object do: block
	| index |
	referencePolicy = #ignore ifTrue: [^block value].
	(index := objects at: object ifAbsent: [nil]) notNil
		ifTrue: 
			[referencePolicy = #error ifTrue: [^STONWriterError signal: 'Shared reference detected'].
			self writeReference: index]
		ifFalse: 
			[index := objects size + 1.
			objects at: object put: index.
			block value]!

writeBoolean: boolean
	writeStream print: boolean!

writeFloat: float
	writeStream nextPutAll: float asString!

writeInteger: integer
	writeStream print: integer!

writeList: collection
	self with: collection do: [self encodeList: collection]!

writeMap: hashedCollection
	self with: hashedCollection do: [self encodeMap: hashedCollection]!

writeNull
	jsonMode ifTrue: [writeStream nextPutAll: 'null'] ifFalse: [writeStream print: nil]!

writeObject: anObject
	| instanceVariableNames |
	(instanceVariableNames := anObject class allInstVarNames) isEmpty
		ifTrue: [self writeObject: anObject do: [self encodeMap: #()]]
		ifFalse: 
			[self writeObject: anObject
				streamMap: 
					[:dictionary |
					instanceVariableNames do: 
							[:each |
							(anObject instVarAt: (instanceVariableNames indexOf: each))
								ifNotNil: [:value | dictionary at: each asSymbol put: value]
								ifNil: [anObject stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]]!

writeObject: anObject do: block
	(jsonMode and: [anObject class ~= STON listClass and: [anObject class ~= STON mapClass]])
		ifTrue: [STONWriterError signal: 'Wrong object class for JSON mode'].
	self with: anObject
		do: 
			[writeStream nextPutAll: anObject class stonName.
			self prettyPrintSpace.
			block value]!

writeObject: object listSingleton: element
	self writeObject: object
		do: 
			[writeStream nextPut: $[.
			self
				prettyPrintSpace;
				nextPut: element;
				prettyPrintSpace.
			writeStream nextPut: $]]!

writeObject: object streamList: block
	self writeObject: object
		do: 
			[| listWriter |
			listWriter := STONListWriter on: self.
			writeStream nextPut: $[.
			self indentedDo: 
					[self newlineIndent.
					block value: listWriter].
			self newlineIndent.
			writeStream nextPut: $]]!

writeObject: object streamMap: block
	self writeObject: object
		do: 
			[| mapWriter |
			mapWriter := STONMapWriter on: self.
			writeStream nextPut: ${.
			self indentedDo: 
					[self newlineIndent.
					block value: mapWriter].
			self newlineIndent.
			writeStream nextPut: $}]!

writeObject: object streamShortList: block
	self writeObject: object
		do: 
			[| listWriter |
			listWriter := STONShortListWriter on: self.
			writeStream nextPut: $[.
			self indentedDo: 
					[self prettyPrintSpace.
					block value: listWriter].
			self prettyPrintSpace.
			writeStream nextPut: $]]!

writeReference: index
	writeStream
		nextPut: $@;
		print: index!

writeString: string
	self encodeString: string!

writeSymbol: symbol
	jsonMode
		ifTrue: [self writeString: symbol]
		ifFalse: 
			[writeStream nextPut: $#.
			(self isSimpleSymbol: symbol)
				ifTrue: [writeStream nextPutAll: symbol]
				ifFalse: [self encodeString: symbol]]! !
!STONWriter categoriesFor: #close!public! !
!STONWriter categoriesFor: #encodeCharacter:!public! !
!STONWriter categoriesFor: #encodeKey:value:!public! !
!STONWriter categoriesFor: #encodeList:!public! !
!STONWriter categoriesFor: #encodeMap:!public! !
!STONWriter categoriesFor: #encodeString:!public! !
!STONWriter categoriesFor: #indentedDo:!public! !
!STONWriter categoriesFor: #initialize!public! !
!STONWriter categoriesFor: #isSimpleSymbol:!public! !
!STONWriter categoriesFor: #jsonMode:!public! !
!STONWriter categoriesFor: #listElementSeparator!public! !
!STONWriter categoriesFor: #mapElementSeparator!public! !
!STONWriter categoriesFor: #newLine:!public! !
!STONWriter categoriesFor: #newlineIndent!public! !
!STONWriter categoriesFor: #nextPut:!public! !
!STONWriter categoriesFor: #on:!public! !
!STONWriter categoriesFor: #optimizeForLargeStructures!public! !
!STONWriter categoriesFor: #prettyPrint:!public! !
!STONWriter categoriesFor: #prettyPrintSpace!public! !
!STONWriter categoriesFor: #referencePolicy:!public! !
!STONWriter categoriesFor: #reset!public! !
!STONWriter categoriesFor: #shortListElementSeparator!public! !
!STONWriter categoriesFor: #with:do:!public! !
!STONWriter categoriesFor: #writeBoolean:!public! !
!STONWriter categoriesFor: #writeFloat:!public! !
!STONWriter categoriesFor: #writeInteger:!public! !
!STONWriter categoriesFor: #writeList:!public! !
!STONWriter categoriesFor: #writeMap:!public! !
!STONWriter categoriesFor: #writeNull!public! !
!STONWriter categoriesFor: #writeObject:!public! !
!STONWriter categoriesFor: #writeObject:do:!public! !
!STONWriter categoriesFor: #writeObject:listSingleton:!public! !
!STONWriter categoriesFor: #writeObject:streamList:!public! !
!STONWriter categoriesFor: #writeObject:streamMap:!public! !
!STONWriter categoriesFor: #writeObject:streamShortList:!public! !
!STONWriter categoriesFor: #writeReference:!public! !
!STONWriter categoriesFor: #writeString:!public! !
!STONWriter categoriesFor: #writeSymbol:!public! !

!STONWriter class methodsFor!

findFirstInString: aString inSet: inclusionMap startingAt: start
	"Trivial, non-primitive version"

	| i stringSize ascii |
	inclusionMap size ~= 256 ifTrue: [^0].
	i := start.
	stringSize := aString size.
	
	[i <= stringSize and: 
			[ascii := (aString at: i) asciiValue.
			ascii < 256 ifTrue: [(inclusionMap at: ascii + 1) = 0] ifFalse: [true]]]
			whileTrue: [i := i + 1].
	i > stringSize ifTrue: [^0].
	^i!

initialize
	self initializeSTONCharacters.
	self initializeSTONSimpleSymbolCharacters!

initializeSTONCharacters
	| escapes |
	STONCharacters := Array new: 127.
	32 to: 126 do: [:each | STONCharacters at: each + 1 put: #pass].
	escapes := #(8 '\b' 9 '\t' 10 '\n' 12 '\f' 13 '\r' 34 '\"' 39 '\''' 92 '\\').
	1 to: escapes size - 1
		by: 2
		do: [:index | STONCharacters at: (escapes at: index) + 1 put: (escapes at: index + 1)]!

initializeSTONSimpleSymbolCharacters
	"STONSimpleSymbolCharacters asArray collectWithIndex: [ :each :index |
		each isZero ifTrue: [ (index - 1) asCharacter ] ]."

	STONSimpleSymbolCharacters := (ByteArray new: 256)
				atAllPut: 1;
				yourself.
	1 to: 256
		do: 
			[:each |
			| char |
			char := (each - 1) asCharacter.
			(self isSimpleSymbolChar: char) ifTrue: [STONSimpleSymbolCharacters at: each put: 0]]!

isSimpleSymbolChar: char
	^'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./' includes: char!

new
	^self basicNew
		initialize;
		yourself!

on: writeStream
	^self new
		on: writeStream;
		yourself! !
!STONWriter class categoriesFor: #findFirstInString:inSet:startingAt:!public! !
!STONWriter class categoriesFor: #initialize!public! !
!STONWriter class categoriesFor: #initializeSTONCharacters!public! !
!STONWriter class categoriesFor: #initializeSTONSimpleSymbolCharacters!public! !
!STONWriter class categoriesFor: #isSimpleSymbolChar:!public! !
!STONWriter class categoriesFor: #new!public! !
!STONWriter class categoriesFor: #on:!public! !

