"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #GsMethod
	instanceVariableNames: 'behavior behaviorName breakPoints category isReadOnly originalSource source stepPoints unimplementedSelectors warnings'
	classVariableNames: 'GsNMethodClassOop'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsMethod guid: (GUID fromString: '{b1e7fc81-607c-4011-8820-b81aaa95a565}')!
GsMethod comment: ''!
!GsMethod categoriesForClass!Unclassified! !
!GsMethod methodsFor!

<= aGsMethod

	^self printString <= aGsMethod printString.
!

= aGsMethod

	^self class == aGsMethod class and: [
		self gsBehavior = aGsMethod gsBehavior and: [
		self name = aGsMethod name]].
!

behavior
	^behavior!

breakPoints
	^breakPoints!

category
	^category!

clearBreakAtStepPoint: anInteger

	gciSession
		serverPerform: #'clearBreakAtStepPoint:inMethod:'
		with: anInteger
		with: oopType.
	breakPoints := breakPoints copyWithout: anInteger.
!

hash

	^self name hash.
!

initialize: readStream session: aGciSession

	gciSession := aGciSession.
	behavior := gciSession oopTypeWithOop: readStream upToTab asNumber.
	behaviorName := readStream upToTab.
	oopType := gciSession oopTypeWithOop: readStream upToTab asNumber.
	name := readStream upToTab.
	category := readStream upToTab.
	isReadOnly := readStream upToTab = 'false'.	"current user has write permission for the class"
	readStream nextLine.	"should be empty"
	(source := readStream nextParagraph) isEmpty ifTrue: [
		source := 
'newMethod: argument
		"Method comment"

	^self yourself.
'.
	].
	"unimplemented selectors"
	unimplementedSelectors := Dictionary new.
	readStream nextLineAsList do: [:eachPair | 
		| pieces |
		pieces := eachPair subStrings.
		unimplementedSelectors at: pieces first asNumber put: pieces last.
	].
	"step points"
	stepPoints := readStream nextLineAsList collect: [:each |
		| pieces offset selector | 
		pieces := each subStrings.
		offset := pieces first asNumber.
		selector := (2 <= pieces size ifTrue: [pieces at: 2] ifFalse: ['']).
		(offset to: 0) -> selector.
	].
	"breaks"
	breakPoints := readStream nextLineAsList collect: [:each | each asNumber].
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
	].
	originalSource := readStream nextParagraph.
	warnings := readStream nextParagraph.
!

isReadOnly
	^isReadOnly!

originalSource
	^originalSource!

printOn: aStream

	aStream
		nextPutAll: behaviorName;
		nextPutAll: '>>#''';
		nextPutAll: name;
		nextPut: $';
		yourself.
!

selector

	^name asSymbol
!

setBreakAtStepPoint: anInteger

	gciSession
		serverPerform: #'setBreakAtStepPoint:inMethod:'
		with: anInteger
		with: oopType.
	breakPoints := breakPoints asSortedCollection add: anInteger; asArray.
!

source

	| gsCode result |
	source isNil ifTrue: [
		gsCode := self codeForObject , ' sourceString'.
		(result := gciSession executeString: gsCode) isNil ifTrue: [^self].
		source := result.
	].
	^source.
!

stepPoints
	^stepPoints!

unimplementedSelectors
	^unimplementedSelectors!

warnings
	^warnings! !
!GsMethod categoriesFor: #<=!public! !
!GsMethod categoriesFor: #=!public! !
!GsMethod categoriesFor: #behavior!accessing!public! !
!GsMethod categoriesFor: #breakPoints!accessing!public! !
!GsMethod categoriesFor: #category!accessing!public! !
!GsMethod categoriesFor: #clearBreakAtStepPoint:!accessing!public! !
!GsMethod categoriesFor: #hash!public! !
!GsMethod categoriesFor: #initialize:session:!public! !
!GsMethod categoriesFor: #isReadOnly!accessing!public! !
!GsMethod categoriesFor: #originalSource!accessing!public! !
!GsMethod categoriesFor: #printOn:!public! !
!GsMethod categoriesFor: #selector!public! !
!GsMethod categoriesFor: #setBreakAtStepPoint:!accessing!public! !
!GsMethod categoriesFor: #source!accessing!public! !
!GsMethod categoriesFor: #stepPoints!accessing!public! !
!GsMethod categoriesFor: #unimplementedSelectors!accessing!public! !
!GsMethod categoriesFor: #warnings!accessing!public! !

!GsMethod class methodsFor!

fromOopType: anOopType session: aGciSession

	| string oopClass |
	(anOopType isKindOf: ExternalInteger) ifFalse: [self error: 'Expected an OopType but got ' , anOopType printString].

	"https://github.com/jgfoster/Jade/issues/178"
	oopClass := aGciSession _library fetchClass: anOopType.
	GsNMethodClassOop ifNil: [GsNMethodClassOop := aGciSession executeString: 'GsNMethod' ].
	oopClass ~= GsNMethodClassOop ifTrue: [
		| actualClass |
		actualClass := aGciSession send: #'name' to: oopClass.
		self error: 'Expected ' , anOopType printString , ' to be a GsNMethod but it is a(n) ' , actualClass.
	].

	string := aGciSession 
		serverPerform: #'describeMethod:'
		with: anOopType.
	string := aGciSession decode: string.
	^self new
		initialize: (JadeServerReadStream on: string)
		session: aGciSession
! !
!GsMethod class categoriesFor: #fromOopType:session:!public! !

