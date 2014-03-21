| package |
package := Package name: 'Jade from Dolphin'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.009'.


package classNames
	add: #JadeExpressionStyler;
	add: #JadeMethodStyler;
	add: #JadeNumberStylingToken;
	add: #JadeStyler;
	add: #JadeStylingScanner;
	yourself.

package methodNames
	add: #Boolean -> #literalTextStyle;
	add: #Character -> #literalTextStyle;
	add: #JadeTextPresenter -> #onViewOpened;
	add: #Number -> #literalTextStyle;
	add: #SmalltalkScanner -> #recoverFromExpectChar;
	add: #SmalltalkScanner -> #recoverFromUnterminatedLiteral;
	add: #StLiteralToken -> #textStyle;
	add: #StNumberLiteralToken -> #textStyle;
	add: #String -> #literalTextStyle;
	add: #StToken -> #postComments;
	add: #StToken -> #preComments;
	add: #Symbol -> #literalTextStyle;
	add: #UndefinedObject -> #literalTextStyle;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Views\Scintilla\Dolphin Scintilla View';
	add: 'Jade UI Base';
	add: '..\Object Arts\Dolphin\System\Compiler\Smalltalk Parser';
	yourself).

package!

"Class Definitions"!

ScintillaStyler subclass: #JadeStyler
	instanceVariableNames: 'arrayDepth view last inTag stack'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeStyler subclass: #JadeExpressionStyler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeStyler subclass: #JadeMethodStyler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SmalltalkScanner subclass: #JadeStylingScanner
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StNumberLiteralToken subclass: #JadeNumberStylingToken
	instanceVariableNames: 'numberClass'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Boolean methodsFor!

literalTextStyle
	^#literalPseudo! !
!Boolean categoriesFor: #literalTextStyle!constants!private! !

!Character methodsFor!

literalTextStyle
	^#literalCharacter! !
!Character categoriesFor: #literalTextStyle!constants!private! !

!JadeTextPresenter methodsFor!

onViewOpened

	super onViewOpened.
	view lexer: #container.
	view stylerClass: JadeMethodStyler.
	view textStyles: self class textStyles.
! !
!JadeTextPresenter categoriesFor: #onViewOpened!public! !

!Number methodsFor!

literalTextStyle
	^#literalNumber! !
!Number categoriesFor: #literalTextStyle!constants!private! !

!SmalltalkScanner methodsFor!

recoverFromExpectChar
	^self literalErrorToken: nil start: stream position!

recoverFromUnterminatedLiteral
	^self literalErrorToken: buffer contents start: self previousStepPosition! !
!SmalltalkScanner categoriesFor: #recoverFromExpectChar!error handling!private! !
!SmalltalkScanner categoriesFor: #recoverFromUnterminatedLiteral!error handling!private! !

!StLiteralToken methodsFor!

textStyle
	^self value literalTextStyle! !
!StLiteralToken categoriesFor: #textStyle!public!visitor/double dispatching! !

!StNumberLiteralToken methodsFor!

textStyle
	^#literalNumber! !
!StNumberLiteralToken categoriesFor: #textStyle!public!visitor/double dispatching! !

!String methodsFor!

literalTextStyle
	^#literalString! !
!String categoriesFor: #literalTextStyle!constants!private! !

!StToken methodsFor!

postComments
	"Answer the set of comments associated with the receiver that are after the token"

	^self comments 
		ifNil: [#()]
		ifNotNil: 
			[:all | 
			| stop |
			stop := self stop.
			all select: [:each | each start > stop]]!

preComments
	"Answer the set of comments associated with the receiver that are before the token"

	^self comments 
		ifNil: [#()]
		ifNotNil: 
			[:all | 
			| start |
			start := self start.
			all select: [:each | each stop < start]]! !
!StToken categoriesFor: #postComments!accessing!public! !
!StToken categoriesFor: #preComments!accessing!public! !

!Symbol methodsFor!

literalTextStyle
	^#literalSymbol! !
!Symbol categoriesFor: #literalTextStyle!constants!private! !

!UndefinedObject methodsFor!

literalTextStyle
	^#literalPseudo! !
!UndefinedObject categoriesFor: #literalTextStyle!constants!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

JadeStyler guid: (GUID fromString: '{10CB88FD-A0F2-4CCC-8019-8072F263ACA1}')!
JadeStyler comment: 'JadeStyler is a <ScintillaStyler> that is capable of colouring Smalltalk source text according to the established Dolphin syntax colouring convention; in fact it is somewhat more sophisticated than the earlier compiler based colouring, and can differentiate a larger set of syntactic elements. The text styles emitted are:

#argDecl			Formal parameter name in method signature
#assignment			Assignment token, i.e. :=
#binaryMessage		Selector of binary message send, e.g. the + in 1+2.
#binarySelector		Binary selector in a method signature.
#blockArgDecl		Formal parameter name of a block argument
#comment			Comments
#identifier			Variable references
#illegal				Illegal characters that are not valid in ANSI Smalltalk
#keywordMessage	Each part of a keyword selector in a message send
#keywordSelector	Each part of a keyword selector in a method signature
#literalArray			Opening and closing brackets of a literal array, including the initial #.
#literalBytes			Literal byte array (brackets and values).
#literalCharacter		Character constants, e.g. $A
#literalNumber		Numeric constants, e.g. -1, or 1.2e10
#literalPseudo		The constants nil, true, and false.
#literalString			String constants, e.g. ''abc''
#literalSymbol		Symbolic constants, e.g. #abc
#normal 			Used for whitespace if a #whitespace style is not defined
#specialCharacter	Other characters with syntactice relevance, such as the period used for statement termination, parentheses, block brackets, etc
#tag				Primitive or external call declaration
#tempCloseBar		Closing bar of temporary declarations
#tempDecl			Temporary variable declaration in a method or block.
#tempOpenBar		Opening bar of temporary declarations
#unaryMessage 		Selector of a unary message send.
#unarySelector		Selector of a unary method, in the signature.
#whitespace			Whitespace between tokens (but not in comments or literal strings).

Note that the #whitespace style is optional. If it is not set up in the View, then whitespace is set to the #normal style, but will use the whitespace foreground and background colours configured in the view (if any), although these will only be used when the whitespaceVisibility mode is other than #invisible.

The actual visual styles associated with these classifications can be configured by modifying the view configuration. This is exposed as the #textStyles published aspect of SmalltalkWorkspace.

The colouring is largely based on identification of tokens through lexical analysis. This has three significant advantages over colouring based on syntax analysis: i) it is faster, ii) it can be initiated in the middle of some source text much more easily, iii) it is tolerant of syntax errors. These features allow the colouring to be performed in "real-time" as the user types, and also allow reasonable colouring of very large workspaces. Large amounts of text are coloured in chunks. 

Pure lexical analysis has the drawback that it isn''t able to distinguish between different uses of the same token type. In Smalltalk there are a number of such ambiguities, for example identifiers that might be variable names or unary selectors. Another example is the use of the vertical bar symbol as a temporary declaration delimiter and also as a binary selector. In order to handle these cases efficiently a number of extra style names are introduced to record an informal parse state as the colouring progresses. Since the style set up for the last token (mostly) encodes the parse state, it is possible to style the current token without looking any further back. This does, however, mean that there are more styles than are strictly needed for visual purposes - for example there are styles for opening and closing temp bars, but it is unlikely one would want these to have a different appearance. There is one case, that of literal arrays, where it is necessary to look back further when styling. This is because literal arrays may contain unquoted literal symbols and nester arrays, at least in Smalltalk-80 syntax.
'!
!JadeStyler categoriesForClass!Unclassified! !
!JadeStyler methodsFor!

acceptAssignmentToken: aStAssignmentToken 
	^#assignment!

acceptBinarySelectorToken: aStBinarySelectorToken 
	| lastStyle selector |
	self atStart ifTrue: [^self initialBinaryStyle: aStBinarySelectorToken].
	self isInArray ifTrue: [^#literalSymbol].
	selector := aStBinarySelectorToken value.
	selector == #| ifTrue: [^self styleForBar].
	lastStyle := self lastStyle.
	selector == #< 
		ifTrue: 
			[^(lastStyle == #unarySelector or: [lastStyle == #argDecl or: [lastStyle == #tempCloseBar]]) 
				ifTrue: 
					[inTag := true.
					#tag]
				ifFalse: [#binaryMessage]].
	"The scanner may have wrongly interpreted adjacent temp open/close bars as a binary
	selector. If it has we must emit appropriate bar styling, and then hack the token to include
	only the last bar."
	selector == #'||' 
		ifTrue: 
			[lastStyle == #blockArgDecl 
				ifTrue: 
					[self applyStyle: #tempCloseBar toNext: 1.
					aStBinarySelectorToken start: aStBinarySelectorToken start + 1.
					^#tempOpenBar].
			(lastStyle == #tempCloseBar or: 
					[lastStyle == #argDecl 
						or: [lastStyle == #unarySelector or: [lastStyle == #specialCharacter and: [self lastToken = '##(']]]]) 
				ifTrue: 
					[self applyStyle: #tempOpenBar toNext: 1.
					aStBinarySelectorToken start: aStBinarySelectorToken start + 1.
					^#tempCloseBar]].
	(selector == #||| and: [lastStyle == #blockArgDecl]) 
		ifTrue: 
			[self
				applyStyle: #tempCloseBar toNext: 1;
				applyStyle: #tempOpenBar toNext: 1.
			aStBinarySelectorToken start: aStBinarySelectorToken stop.
			^#tempCloseBar].
	^#binaryMessage!

acceptIdentifierToken: aStIdentifierToken 
	| lastStyle |
	self atStart ifTrue: [^self initialUnaryStyle].
	self isInArray ifTrue: [^#literalSymbol].
	lastStyle := self lastStyle.
	lastStyle == #specialCharacter 
		ifTrue: 
			[| char |
			char := self lastCharacter.
			char == $: ifTrue: [^#blockArgDecl].
			(char == $) or: [char == $; or: [char = $]]]) ifTrue: [^#unaryMessage].
			^#identifier].
	^##((IdentityDictionary new)
		at: #identifier put: #unaryMessage;
		at: #unaryMessage put: #unaryMessage;
		at: #literalPseudo put: #unaryMessage;
		at: #literalString put: #unaryMessage;
		at: #literalNumber put: #unaryMessage;
		at: #literalCharacter put: #unaryMessage;
		at: #literalSymbol put: #unaryMessage;
		at: #literalBytes put: #unaryMessage;
		at: #tempOpenBar put: #tempDecl;
		at: #tempDecl put: #tempDecl;
		at: #keywordSelector put: #argDecl;
		at: #binarySelector put: #argDecl;
		at: #literalArray put: #unaryMessage;
		shrink;
		yourself) at: lastStyle ifAbsent: [#identifier]!

acceptIllegalCharacterToken: aStIllegalCharacterToken 
	^#illegal!

acceptKeywordToken: aStKeywordToken 
	self atStart ifTrue: [^self initialKeywordStyle].
	self isInArray ifTrue: [^#literalSymbol].
	^(self lastStyle == #argDecl or: [self lastIsSpecial: $!!]) 
		ifTrue: [#keywordSelector]
		ifFalse: [#keywordMessage]!

acceptLiteralArrayToken: aStLiteralArrayToken 
	^aStLiteralArrayToken isForByteArray 
		ifTrue: [#literalBytes]
		ifFalse: 
			[arrayDepth := arrayDepth + 1.
			stack addLast: aStLiteralArrayToken.
			#literalArray]!

acceptLiteralToken: aStLiteralToken 
	self atStart ifTrue: [^self initialLiteralStyle: aStLiteralToken].
	^aStLiteralToken textStyle!

acceptNumberLiteralToken: aStNumberLiteralToken 
	self atStart ifTrue: [^#literalNumber].
	self lastIsLiteralByte ifTrue: [^#literalBytes].
	self isInArray ifTrue: [^#literalNumber].
	"Handle 'x-1' ambiguity: invalid: binary selector, block arg decl, keyword selector, literal
	array start, static start, temp decl, temp open must be a literal if previous was keyword
	message, period, hat, assignment, binary message, arg decl, close temp, unary selector can
	be a message if previous was: literal (of any sort), identifier, special: paren, special:
	block paren, unaryMessage"
	(aStNumberLiteralToken isNegative and: 
			[(#(#identifier #literalPseudo #literalNumber #literalString #literalCharacter #literalArray #literalBytes #literalSymbol #unaryMessage) 
				identityIncludes: self lastStyle) or: [self lastIsStyle: #specialCharacter chars: '])']]) 
		ifTrue: 
			[self applyStyle: #binaryMessage toNext: 1.
			aStNumberLiteralToken start: aStNumberLiteralToken start + 1].
	^#literalNumber!

acceptOptimizedToken: aStOptimizedToken 
	stack addLast: aStOptimizedToken.
	^#specialCharacter!

acceptSpecialCharacterToken: aStSpecialCharacterToken 
	"Special case for Smalltalk-80 literal syntax, which allows nested arrays in literal arrays to be unquoted."

	| char |
	char := aStSpecialCharacterToken value.
	char == $( 
		ifTrue: 
			[^self isInArray 
				ifTrue: 
					[stack addLast: (StLiteralArrayToken value: '(' start: aStSpecialCharacterToken start).
					arrayDepth := arrayDepth + 1.
					#literalArray]
				ifFalse: 
					[stack addLast: aStSpecialCharacterToken.
					#specialCharacter]].
	char == $) 
		ifTrue: 
			[(stack removeLastIfAbsent: []) 
				ifNotNil: 
					[:token | 
					token isLiteralArrayToken 
						ifTrue: 
							[arrayDepth := arrayDepth - 1.
							^#literalArray]].
			^#specialCharacter].
	(char == $] and: [self lastIsLiteralByte]) ifTrue: [^#literalBytes].
	^#specialCharacter!

applyStyle: aSymbol toNext: anInteger 
	anInteger < 1 ifTrue: [^self].
	(arrayDepth > 0 or: [aSymbol == #literalArray]) 
		ifTrue: 
			[view 
				setIndicator: self literalArrayIndicatorId
				from: view stylingPosition
				length: anInteger].
	view styleNext: anInteger
		mask: ((view styleNamed: aSymbol) ifNil: [0] ifNotNil: [:style | style id])!

atStart
	^last isNil!

colorComments: aCollection startingAt: anInteger 
	"Note that the anInteger is the start position in the text of the token stream, not the text
	of the view."

	| currentPos |
	currentPos := anInteger.
	aCollection do: 
			[:each | 
			self applyStyle: #whitespace toNext: each start - currentPos.
			currentPos := each stop + 1.
			self applyStyle: #comment toNext: currentPos - each start].
	^currentPos!

colorText: aString in: aScintillaView startingAt: anInteger 
	| scanner currentPos |
	view := aScintillaView.
	arrayDepth := 0.
	stack := OrderedCollection new.
	scanner := self scannerClass on: aString readStream.
	currentPos := self colourTokens: scanner startingAt: anInteger.
	"Colour any remaining comments"
	scanner getComments 
		ifNotNil: [:comments | currentPos := self colorComments: comments startingAt: currentPos].
	"And finally any terminal whitespace up to EOF"
	self applyStyle: #whitespace toNext: aString size - currentPos + 1!

colourBefore: aStToken startingAt: anInteger 
	| position |
	position := self colorComments: aStToken preComments startingAt: anInteger.
	self applyStyle: #whitespace toNext: aStToken start - position!

colourTokens: aSmalltalkScanner startingAt: anInteger 
	| position offset |
	position := 1.
	offset := anInteger - 1.
	last := self tokenBefore: offset.
	[aSmalltalkScanner atEnd] whileFalse: 
			[| nextAfter aStToken styleName |
			aStToken := aSmalltalkScanner next.
			self colourBefore: aStToken startingAt: position.
			nextAfter := aStToken stop + 1.
			styleName := self styleForToken: aStToken.
			self applyStyle: styleName toNext: nextAfter - aStToken start.
			position := self colorComments: aStToken postComments startingAt: nextAfter.
			last := aStToken sourceInterval + offset -> styleName].
	^position!

initialBinaryStyle: aStBinarySelectorToken 
	^self subclassResponsibility!

initialize
	arrayDepth := 0.
	inTag := false.
	stack := OrderedCollection new!

initialKeywordStyle
	^self subclassResponsibility!

initialLiteralStyle: aStLiteralToken 
	^aStLiteralToken value literalTextStyle!

initialUnaryStyle
	^self subclassResponsibility!

isInArray
	^arrayDepth > 0 and: [stack last isLiteralArrayToken]!

isLiteralArrayAt: anInteger in: aScintillaView 
	^aScintillaView isIndicator: self literalArrayIndicatorId setAt: anInteger!

lastCharacter
	"Private - Answer the last styled character."

	^view characterAt: last key stop!

lastIsLiteralByte
	^self lastStyle == #literalBytes and: [self lastCharacter ~~ $]]!

lastIsSpecial: aCharacter 
	^self lastStyle == #specialCharacter and: [self lastCharacter == aCharacter]!

lastIsStyle: aSymbol char: aCharacter 
	^self lastStyle == aSymbol and: [self lastCharacter == aCharacter]!

lastIsStyle: aSymbol chars: aString 
	^self lastStyle == aSymbol and: [aString identityIncludes: self lastCharacter]!

lastStyle
	^last ifNotNil: [last value]!

lastToken
	^view plainTextRange: last key!

literalArrayIndicatorId
	^8 "INDIC_CONTAINER" - 1!

prepareToStyleView: aScintillaView 
	"The receiver has been set up as the styler for the specified <ScintillaView>. This is an
	opportunity to initialise that view appropriately for this styler."

	"Set up the Smalltalk brace chars grouped into the various styles used by the receiver."

	aScintillaView braceChars: ##((IdentityDictionary new)
				at: #specialCharacter put: '()[]<>';
				at: #literalBytes put: '[]';
				at: #literalArray put: '()';
				yourself)!

resetStylingIn: aScintillaView from: startInteger to: stopInteger 
	super 
		resetStylingIn: aScintillaView
		from: startInteger
		to: stopInteger.
	aScintillaView 
		clearIndicator: self literalArrayIndicatorId
		from: startInteger
		to: stopInteger!

scannerClass
	^JadeStylingScanner!

styleForBar
	| lastStyle |
	lastStyle := self lastStyle.
	lastStyle == #specialCharacter 
		ifTrue: 
			[^(self lastCharacter == $[ or: [self lastToken = '##(']) 
				ifTrue: [#tempOpenBar]
				ifFalse: [#binaryMessage]].
	^##((IdentityDictionary new)
		at: #tempOpenBar put: #tempCloseBar;
		at: #tempDecl put: #tempCloseBar;
		at: #blockArgDecl put: #tempCloseBar;
		at: #argDecl put: #tempOpenBar;
		at: #unarySelector put: #tempOpenBar;
		at: #tempCloseBar put: #tempOpenBar;
		shrink;
		yourself) at: lastStyle ifAbsent: [#binaryMessage]!

styleForToken: aStToken 
	inTag 
		ifTrue: 
			[(aStToken isBinary and: [#(#> #*> #**>) includes: aStToken value]) ifTrue: [inTag := false].
			^#tag].
	^aStToken acceptVisitor: self!

stylingStartBefore: startInteger in: aScintillaView 
	"Private - Locate the position before the <integer>, startInteger, from which to start styling in the
	<ScintillaView>, aScintillaView. This needs to be a position from which we can safely start
	the scanner from its start state."

	| start arrayRange |
	start := super stylingStartBefore: startInteger in: aScintillaView.
	arrayRange := aScintillaView rangeOfIndicator: self literalArrayIndicatorId at: start.
	^arrayRange isEmpty 
		ifTrue: 
			[| styleMask |
			styleMask := aScintillaView styleMaskAt: start.
			inTag := (aScintillaView styleNamed: #tag) 
						ifNil: [false]
						ifNotNil: [:style | style id = (styleMask bitAnd: aScintillaView maxStyle)].
			start]
		ifFalse: 
			["If the start token is inside a literal array, we must start styling from
			 the start of that array. This is the one case where significant look back
			 might be required (because of literal array nesting)."
			arrayRange start]!

tokenBefore: anInteger 
	| start id white comment stop |
	anInteger < 1 ifTrue: [^nil].
	start := anInteger.
	id := nil.
	white := view idOfStyleNamed: #whitespace.
	comment := view idOfStyleNamed: #comment.
	
	[id := view styleIdAt: start.
	id == comment or: [id == white and: [(view characterAt: start) isSeparator]]] 
			whileTrue: 
				[start := start - 1.
				start == 0 ifTrue: [^nil]].
	stop := start.
	[(start := start - 1) == 0 or: [(view styleIdAt: start) ~~ id]] whileFalse.
	^(start + 1 to: stop) -> (view styleWithId: id) name! !
!JadeStyler categoriesFor: #acceptAssignmentToken:!public!visitor/double dispatching! !
!JadeStyler categoriesFor: #acceptBinarySelectorToken:!public!visitor/double dispatching! !
!JadeStyler categoriesFor: #acceptIdentifierToken:!public!visitor/double dispatching! !
!JadeStyler categoriesFor: #acceptIllegalCharacterToken:!public!visitor/double dispatching! !
!JadeStyler categoriesFor: #acceptKeywordToken:!public!visitor/double dispatching! !
!JadeStyler categoriesFor: #acceptLiteralArrayToken:!public!visitor/double dispatching! !
!JadeStyler categoriesFor: #acceptLiteralToken:!public!visitor/double dispatching! !
!JadeStyler categoriesFor: #acceptNumberLiteralToken:!public!visitor/double dispatching! !
!JadeStyler categoriesFor: #acceptOptimizedToken:!public!visitor/double dispatching! !
!JadeStyler categoriesFor: #acceptSpecialCharacterToken:!public!visitor/double dispatching! !
!JadeStyler categoriesFor: #applyStyle:toNext:!helpers!private! !
!JadeStyler categoriesFor: #atStart!private!testing! !
!JadeStyler categoriesFor: #colorComments:startingAt:!operations!private! !
!JadeStyler categoriesFor: #colorText:in:startingAt:!operations!private! !
!JadeStyler categoriesFor: #colourBefore:startingAt:!operations!private! !
!JadeStyler categoriesFor: #colourTokens:startingAt:!operations!private! !
!JadeStyler categoriesFor: #initialBinaryStyle:!constants!private! !
!JadeStyler categoriesFor: #initialize!initializing!private! !
!JadeStyler categoriesFor: #initialKeywordStyle!constants!private! !
!JadeStyler categoriesFor: #initialLiteralStyle:!helpers!private! !
!JadeStyler categoriesFor: #initialUnaryStyle!constants!private! !
!JadeStyler categoriesFor: #isInArray!private!testing! !
!JadeStyler categoriesFor: #isLiteralArrayAt:in:!helpers!private! !
!JadeStyler categoriesFor: #lastCharacter!accessing!private! !
!JadeStyler categoriesFor: #lastIsLiteralByte!private!testing! !
!JadeStyler categoriesFor: #lastIsSpecial:!helpers!private! !
!JadeStyler categoriesFor: #lastIsStyle:char:!helpers!private! !
!JadeStyler categoriesFor: #lastIsStyle:chars:!helpers!private! !
!JadeStyler categoriesFor: #lastStyle!accessing!private! !
!JadeStyler categoriesFor: #lastToken!accessing!private! !
!JadeStyler categoriesFor: #literalArrayIndicatorId!constants!private! !
!JadeStyler categoriesFor: #prepareToStyleView:!initializing!public! !
!JadeStyler categoriesFor: #resetStylingIn:from:to:!helpers!private! !
!JadeStyler categoriesFor: #scannerClass!constants!private! !
!JadeStyler categoriesFor: #styleForBar!helpers!private! !
!JadeStyler categoriesFor: #styleForToken:!helpers!private! !
!JadeStyler categoriesFor: #stylingStartBefore:in:!helpers!private! !
!JadeStyler categoriesFor: #tokenBefore:!helpers!private! !

!JadeStyler class methodsFor!

language
	^'Smalltalk'! !
!JadeStyler class categoriesFor: #language!constants!public! !

JadeExpressionStyler guid: (GUID fromString: '{055C9EA2-7ED5-4798-A27E-E6A87B195F64}')!
JadeExpressionStyler comment: 'JadeExpressionStyler is a <JadeStyler> specialised for colouring Smalltalk expressions (as opposed to methods) in workspaces.'!
!JadeExpressionStyler categoriesForClass!Unclassified! !
!JadeExpressionStyler methodsFor!

initialBinaryStyle: aStBinarySelectorToken 
	aStBinarySelectorToken value = #'||' 
		ifTrue: 
			[self applyStyle: #tempOpenBar toNext: 1.
			aStBinarySelectorToken start: aStBinarySelectorToken start + 1.
			^#tempCloseBar].
	aStBinarySelectorToken value = #| ifTrue: [^#tempOpenBar].
	^#binaryMessage!

initialKeywordStyle
	^#keywordMessage!

initialUnaryStyle
	^#identifier! !
!JadeExpressionStyler categoriesFor: #initialBinaryStyle:!constants!private! !
!JadeExpressionStyler categoriesFor: #initialKeywordStyle!constants!private! !
!JadeExpressionStyler categoriesFor: #initialUnaryStyle!constants!private! !

JadeMethodStyler guid: (GUID fromString: '{446D7673-3AE1-40EB-8D06-81127700D496}')!
JadeMethodStyler comment: 'JadeMethodStyler is a <JadeStyler> specialised to colour method definitions. It differs in recognising the method signature, which is not itself a valid Smalltalk expression.'!
!JadeMethodStyler categoriesForClass!Unclassified! !
!JadeMethodStyler methodsFor!

initialBinaryStyle: aStBinarySelectorToken 
	^#binarySelector!

initialKeywordStyle
	^#keywordSelector!

initialLiteralStyle: aStLiteralToken 
	| style |
	style := aStLiteralToken value literalTextStyle.
	^style == #literalPseudo ifTrue: [#unarySelector] ifFalse: [style]!

initialUnaryStyle
	^#unarySelector! !
!JadeMethodStyler categoriesFor: #initialBinaryStyle:!constants!private! !
!JadeMethodStyler categoriesFor: #initialKeywordStyle!constants!private! !
!JadeMethodStyler categoriesFor: #initialLiteralStyle:!helpers!private! !
!JadeMethodStyler categoriesFor: #initialUnaryStyle!constants!private! !

JadeStylingScanner guid: (GUID fromString: '{1627292B-38B8-4FB4-B5DC-0426A0CFD9EF}')!
JadeStylingScanner comment: 'JadeStylingScanner is a <SmalltalkScanner> specialized for the purpose of real-time syntax colouring. It differs from its superclass only in that it is tolerant of lexical errors, and returns error tokens rather than raising exceptions. This is appropriate for scanning source text that is being edited, since at any time it may be in an incomplete state.'!
!JadeStylingScanner categoriesForClass!Unclassified! !
!JadeStylingScanner methodsFor!

constantExpected
	^self literalErrorToken: #'' start: self previousStepPosition!

lexicalError: anInteger range: anInterval 
	"Private - Evaluate the error block passing it an appropriately initialised <exception>.
	If the block returns, signal the exception."

	"Implementation Note: The error is ignored, and recovered from."

	!

newNumberToken: numberClass 
	| stop |
	stop := self previousStepPosition.
	^(self numberTokenClass 
		value: nil
		start: tokenStart
		stop: stop
		source: (stream copyFrom: tokenStart to: stop))
		numberClass: numberClass;
		yourself!

numberTokenClass
	^JadeNumberStylingToken!

scanExponentInteger
	"Allow plus prefix on exponent, e.g. 2e+16, although not strictly Smalltalk syntax."

	| pos isNegative |
	pos := stream position.
	self step.
	((isNegative := currentCharacter == $-) or: [currentCharacter == $+]) ifTrue: [self step].
	(self skipIntegerOfRadix: 10) 
		ifTrue: 
			[isNegative 
				ifTrue: 
					[| exp |
					exp := stream copyFrom: pos+2 to: self previousStepPosition.
					(exp allSatisfy: [:each | each == $0]) ifFalse: [^Fraction]]]
		ifFalse: [self stepBackTo: pos].
	^Integer!

scanNumber
	| numberClass start |
	currentCharacter == $- 
		ifTrue: 
			["isNegative := true."
			self step].
	start := stream position.
	(self skipIntegerOfRadix: 10) 
		ifFalse: 
			["This should not happen since we have previously detected a digit"
			^self error: 'internal error'].
	self atEnd ifTrue: [^self newNumberToken: Integer].
	"The type of number is determined by what we find next"
	numberClass := Integer.
	currentCharacter == $. 
		ifTrue: [numberClass := self scanSmalltalkReal]
		ifFalse: 
			[currentCharacter == $r 
				ifTrue: 
					[| radix |
					radix := Integer 
								readPositiveFrom: (stream copyFrom: start to: self previousStepPosition) readStream
								radix: 10.
					(radix between: 2 and: 36) 
						ifTrue: 
							[| pos |
							pos := stream position.
							self step.
							(self skipIntegerOfRadix: radix) 
								ifTrue: [currentCharacter == $e ifTrue: [numberClass := self scanExponentInteger]]
								ifFalse: [self stepBackTo: pos]]]
				ifFalse: 
					[currentCharacter == $s 
						ifTrue: 
							[self step.	"s will always be part of token, regardless if there is a scale value"
							self skipIntegerOfRadix: 10.
							numberClass := ScaledDecimal]
						ifFalse: 
							["Also support St-80 format integer of the form 1e5, not valid ANSI syntax though"
							currentCharacter == $e ifTrue: [numberClass := self scanExponentInteger]]]].
	^self newNumberToken: numberClass!

scanSmalltalkReal
	(self classify: stream peek) == #digit 
		ifFalse: 
			["Trailing full stop on integer"
			^Integer].
	self step.
	self skipIntegerOfRadix: 10.

	"Process any exponent..."
	(currentCharacter == $e or: [currentCharacter == $d or: [currentCharacter == $q]]) 
		ifTrue: 
			[| exponentStart |
			exponentStart := stream position.
			self step.	"Skip the exponent character"
			"Allow plus prefix on the exponent, although not ANSI Smalltalk syntax"
			(currentCharacter == $- or: [currentCharacter == $+]) ifTrue: [self step].
			(self skipIntegerOfRadix: 10) 
				ifFalse: 
					["Found Float with trailing exponent character which is not
					 part of the number, e.g. 1.5e From ANSI standard p 28: 'An
					 exponentLetter must be followed by an explicit exponent'"
					self stepBackTo: exponentStart]]
		ifFalse: 
			[currentCharacter == $s 
				ifTrue: 
					[self step.	"Skip the $s"
					self skipIntegerOfRadix: 10.
					^ScaledDecimal]].
	^Float!

skipIntegerOfRadix: radix 
	| found |
	found := false.
	[self atEnd] whileFalse: 
			[| digit |
			((digit := currentCharacter digitValue) < radix and: [digit >= 0]) 
				ifTrue: 
					[self step.
					found := true]
				ifFalse: [^found]].
	^found!

stepBackTo: exponentStart 
	stream position: exponentStart - 1.
	self step! !
!JadeStylingScanner categoriesFor: #constantExpected!public! !
!JadeStylingScanner categoriesFor: #lexicalError:range:!error handling!private! !
!JadeStylingScanner categoriesFor: #newNumberToken:!private! !
!JadeStylingScanner categoriesFor: #numberTokenClass!public! !
!JadeStylingScanner categoriesFor: #scanExponentInteger!public!scanning! !
!JadeStylingScanner categoriesFor: #scanNumber!public!scanning! !
!JadeStylingScanner categoriesFor: #scanSmalltalkReal!public!scanning! !
!JadeStylingScanner categoriesFor: #skipIntegerOfRadix:!public!scanning! !
!JadeStylingScanner categoriesFor: #stepBackTo:!private! !

JadeNumberStylingToken guid: (GUID fromString: '{C7C638B9-53CD-4316-803A-A5AC217D7F73}')!
JadeNumberStylingToken comment: ''!
!JadeNumberStylingToken categoriesForClass!Unclassified! !
!JadeNumberStylingToken methodsFor!

isNegative
	^self source first == $-!

numberClass
	^numberClass ifNil: [self value class]!

numberClass: aClass 
	numberClass := aClass!

value
	value isNil ifTrue: [value := [Number readFrom: self source readStream] on: Error do: [0]].
	^value! !
!JadeNumberStylingToken categoriesFor: #isNegative!accessing!public! !
!JadeNumberStylingToken categoriesFor: #numberClass!public! !
!JadeNumberStylingToken categoriesFor: #numberClass:!accessing!private! !
!JadeNumberStylingToken categoriesFor: #value!accessing!public! !

"Binary Globals"!

