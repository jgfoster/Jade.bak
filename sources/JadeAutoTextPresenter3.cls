"Filed out from Dolphin Smalltalk 7"!

JadeTextPresenter subclass: #JadeAutoTextPresenter3
	instanceVariableNames: 'currentGsShape currentText previousText currentStyleName previousStyleName lastTimeKey wordRightSideIsEmpty lastParsedResult lastTextSelection'
	classVariableNames: 'ReplacementDictionary'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeAutoTextPresenter3 guid: (GUID fromString: '{40c6fc5c-2470-4eb8-8841-fd5ce72a4c7a}')!
JadeAutoTextPresenter3 comment: ''!
!JadeAutoTextPresenter3 categoriesForClass!Unclassified! !
!JadeAutoTextPresenter3 methodsFor!

addParenthesisOrBracketsToSelection
	| openParenthesisOrBrackets closeParenthesisOrBrackets|

	openParenthesisOrBrackets := (view characterAt: view caretPosition - 1) asString.

	closeParenthesisOrBrackets := (openParenthesisOrBrackets = '(') ifTrue: [')'] ifFalse: [']'].

	view undo.
	view undo.
	view invalidate.
[	view caretPosition: (model value indexOfSubCollection: lastTextSelection).
	view insertText: openParenthesisOrBrackets at: view caretPosition.
	view caretPosition: (view caretPosition + lastTextSelection size).
	view insertText: closeParenthesisOrBrackets at: view caretPosition + 1.] forkAt: 4.!

autocompletationConfiguration
	^JadeAutocompletationConfiguration default!

autocompleteForIdentifier

	^self showAutoCompletionList: self getNamesForIdentifier prefixLength: 0
	!

autocompleteForNormal

	currentText isEmpty ifTrue: [^self].
	previousStyleName isEmpty ifTrue: [^self].

	currentText first isUpperCase ifTrue: [^self showAutoCompletionList: self getNamesForClassUnaryMessage prefixLength: 0].

!

autocompleteForUnaryMessage

	^self showAutoCompletionList: self getNamesForUnaryMessage prefixLength: 0
	!

autocompleteForUpperIdentifier

	^self showAutoCompletionList: self getNameUpperIdentifier prefixLength: 0
	!

codePresenterIsMethod

	^parentPresenter codePresenterIsMethod!

configuration

	^JadeAutocompletationConfiguration default!

createSchematicWiring

	super createSchematicWiring.

	self when: #autoComplete:startingAt:accept: send: #onAutoComplete:startingAt:accept: to: self;
		when: #leftButtonPressed: send: #onLeftButtonPressed: to: self";
		when: #painted send: #onPainted to: self".

!

currentGsShape
	^currentGsShape!

currentGsShape: anObject
	currentGsShape := anObject!

currentStyleName
	^currentStyleName!

currentStyleName: anObject
	currentStyleName := anObject!

currentText
	^currentText!

currentText: anObject
	currentText := anObject!

getArgumentsNames
   "Get and set receiver's inst var <arguments>"

	lastParsedResult ifNil: [^SortedCollection new].

	self codePresenterIsMethod ifFalse: [^SortedCollection new].

	^lastParsedResult allArgumentVariables asSet asSortedCollection
!

getBreakCharacters

	^OrderedCollection new 
		add: Character space; 
		add: Character lf; 
		add: Character nl; 
		add: Character cr; 
		add: Character tab; 
		add: Character null; 
		add: $[; 
		add: $]; 
		add: ${; 
		add: $}; 
		add: $(; 
		add: $); 
		add: $.; 
		add: $^; 
		yourself
!

getClassInstancesVariables
	| isClassSide |

	(parentPresenter parentPresenter isKindOf: JadeMethodListPresenter) ifTrue: [
		| listBrowser className |
		listBrowser := parentPresenter parentPresenter.
		className := listBrowser selectedClass name subStrings: ' '.
		 isClassSide := className last = 'class'.
		currentGsShape := self registry getClass: className first.
		currentGsShape isNil ifTrue: [^#()].
		^(isClassSide ifTrue: [currentGsShape getClassInstanceVariableLike: currentText] ifFalse: [currentGsShape getInstanceVariableLike: currentText]) asSortedCollection.
	].

	currentGsShape ifNil: [^#()].

	^(parentPresenter hasClassSideSelected 
		ifTrue: [currentGsShape getClassInstanceVariableLike: currentText] ifFalse: [currentGsShape getInstanceVariableLike: currentText]) asSortedCollection!

getClassName
	currentGsShape ifNil: [^''].

	^currentGsShape name!

getCurrentWord
	""
	| breakCharacters leftPosition rightPosition leftStream rightStream currentCharacter |
	(view class == ScintillaView) ifFalse: [^self].
	wordRightSideIsEmpty := true.
	(view value size <= 1) ifTrue: [^self].
	currentText := ''.
	breakCharacters := self getBreakCharacters.
	leftStream := WriteStream on: String new.
	rightStream := WriteStream on: String new.
	leftPosition := view caretPosition.
	rightPosition := view caretPosition + 1.

	[(leftPosition > 0) and: [breakCharacters includes: (view characterAt: leftPosition)]]  whileTrue: [leftPosition := leftPosition - 1].

	"move left"
	currentCharacter := view characterAt: leftPosition.
	[(leftPosition >= 1) and:[(breakCharacters includes: currentCharacter) not]] 
	whileTrue: [((currentCharacter = Character null) and: [currentCharacter = Character lf]) ifFalse: [leftStream nextPut: (view characterAt: leftPosition)].
			leftPosition := leftPosition - 1. 
			currentCharacter := view characterAt: leftPosition.
			].
	((view characterAt: view caretPosition) = Character space) "if the current is Space --> the is no right part"
	ifFalse: [
		currentCharacter := view characterAt: rightPosition.
		[(rightPosition < self value size) and:[(breakCharacters includes: currentCharacter) not]] 
		whileTrue: [((currentCharacter = Character null) and: [currentCharacter = Character lf]) 
				ifFalse: [	wordRightSideIsEmpty := false. "the cursor is in the middle of a word --> disable popup menu"
						rightStream nextPut: (view characterAt: rightPosition)].
				rightPosition := rightPosition + 1. 
				currentCharacter := view characterAt: rightPosition.
				].
	].
	currentText trimBlanks notEmpty ifTrue: [previousText := currentText].
	currentText := (leftStream contents reverse, rightStream contents) trimBlanks.

	
	!

getNamesForClassUnaryMessage

	^(self registry getClass: currentText) ifNotNil: [:value | value getAllClassMethods asSortedCollection ] ifNil: [#()]!

getNamesForIdentifier

	currentText first isUpperCase ifTrue: [^self getNameUpperIdentifier].

	^(#('self' 'super' 'nil' 'true' 'false'),
		self getArgumentsNames, 
		self getTemporaries, 
		self getClassInstancesVariables,
		(self getSelectedGsShape ifNil: [#()] ifNotNil: [:value | value getAllInstVarNames])) asSortedCollection 
			select: [:each | (currentText, '*') match: each]!

getNamesForUnaryMessage

	currentText isEmpty ifTrue: [^self registry getAllGlobalMethods].

	currentText first isUpperCase ifTrue: [^self getNamesForClassUnaryMessage].

	^self registry getAllGlobalMethods select: [:each | (currentText, '*') match: each]!

getNameUpperIdentifier

	(parentPresenter parentPresenter isKindOf: JadeMethodListPresenter) ifTrue: [
		| listBrowser |
		listBrowser := parentPresenter parentPresenter.
		currentGsShape := self registry getClass: listBrowser getSelectedClassName.
	].
	^(parentPresenter hasClassSideSelected 
		ifFalse: [#()] 
		ifTrue: [currentGsShape 
			ifNotNil: [currentGsShape getClassVariableAndPoolDictionaryLike: currentText] 
			ifNil: [#()]]), 
	(self registry getClassesNamedWith: currentText) asSortedCollection!

getSelectedGsShape

	(self topShell class name == #'JadeTranscript') ifTrue: [^nil].
	(self topShell class name == #'JadeMethodListBrowser') ifTrue: [^nil].
	(self topShell isKindOf: JadeInspector) ifTrue: [^nil "self registry getClass: inspected object class"].
	(self topShell class name == #'JadeDebugger') ifTrue: [^nil].
	(self topShell class == JadeWorkspace) ifTrue: [^nil].

	^self registry getClass: parentPresenter getClassName!

getTemporaries
   "The receiver get the temporaries of the current method being edited"

	lastParsedResult ifNil: [^SortedCollection new].

	^self codePresenterIsMethod
               ifTrue: [lastParsedResult body allTemporaryVariables asSet asSortedCollection]
               ifFalse: [lastParsedResult allTemporaryVariables asSet asSortedCollection]

	




!

initialize

	super initialize.

	currentText := ''.
	currentStyleName := ''.
	previousStyleName := ''.
	previousText := ''.
	lastTimeKey := Time now asMilliseconds.
	wordRightSideIsEmpty := false.
!

insertBasicText: aString at: anInteger

	view insertText: aString at: view caretPosition!

insertKeywordText: aString at: anInteger
	"The receiver insert <aString> at the position <anInteger>. This method is called when the user select an option in the autocompletation menu.
	<anIntegrer> is the caret position at the START of the word (even if the user has typed some character)"
	| signatureStream |

	signatureStream := WriteStream on: String new.

	(aString subStrings: ':') keysAndValuesDo: [:index :keywordElement | | paramString keywordToInsert |
		keywordToInsert := ((index = 1)  and: [(anInteger ~= view caretPosition) or: [(view characterAt: view caretPosition-1) isAlphaNumeric] ])
			ifTrue: [keywordElement allButFirst: currentText size] 
			ifFalse: [keywordElement].
		paramString := self registry paramFor: aString index: index.
		(paramString = 'param') ifTrue: [paramString := paramString, index printString].
		signatureStream nextPutAll: (keywordToInsert, ': ', paramString, ' ')].	

	self insertBasicText: signatureStream contents "allButLast"  at: view caretPosition.

	((view characterAt: view caretPosition) = $:) ifFalse: [view moveToEndOfWord]."if the user type the entire first part of the keyword ?"

	view moveToEndOfWord. "the user has NOT type any key of the selected autocompletation"

	!

insertKeywordText2: aString at: anInteger
	"The receiver insert <aString> at the position <anInteger>. This method is called when the user select an option in the autocompletation menu.
	<anIntegrer> is the caret position at the START of the word (even if the user has typed some character)"
	| signatureStream |

	signatureStream := WriteStream on: String new.

	(aString subStrings: ':') keysAndValuesDo: [:index :keywordElement | | paramString |
		paramString := self registry paramFor: aString index: index.
		(paramString = 'param') ifTrue: [paramString := paramString, index printString].
		signatureStream nextPutAll: (keywordElement, ': ', paramString, ' ')].

	"false: the user has not typed any part of the autocompletion" "is at end of the text pane --> has type some part of the autocompletion"
	((view caretPosition = anInteger) and: [(self value size < anInteger) not])
	ifFalse: [view deleteToStartOfWord.
		self insertBasicText: signatureStream contents at: (anInteger-currentText size).
		^view moveToEndOfWord; moveToEndOfWord].

	self insertBasicText: signatureStream contents "allButLast"  at: view caretPosition.

	view moveToEndOfWord;moveToEndOfWord. "the user has NOT type any key of the selected autocompletation"

	!

insertSimpleText: aString at: anInteger
	"The receiver insert <aString> at the position <anInteger>. This method is called when the user select an option in the autocompletation menu"

	self lastCharacterIsSpace ifFalse: [view deleteToStartOfWord].
	self insertBasicText: aString at: anInteger.
	view moveToEndOfWord.!

insertText: aString at: anInteger
	"The receiver insert <aString> at the position <anInteger>. This method is called when the user select an option in the autocompletation menu"

	(aString includes: $:) 
		ifFalse: [self insertSimpleText: aString at: anInteger]
		ifTrue: [self insertKeywordText: aString at: anInteger.
				view extendToEndOfNextWord.
				"arguments with [,{,( should have text inside to be selected after autocompletion hit"
				(#('[' '(' '{') includes: view  selection) 
				ifTrue: [view moveRight."if the block if empty --> does nothing, if block has a string --> extend selection of the string"
					^(#($] $} $)) includes: (view characterAt: view caretPosition))  
					ifFalse: [view extendToEndOfWord]
					ifTrue: [ view basicSelectionStart: view caretPosition end: view caretPosition]
				].
				(#('[]' '()' '{}') includes: view  selection) 
				ifTrue: [^view moveLeft; basicSelectionStart: view caretPosition end: view caretPosition].

				(#('[:' '{:') includes: view selection trimBlanks) 
				ifTrue: [^view basicSelectionStart: view caretPosition end: view caretPosition; moveLeft; extendToEndOfWord].
		]


	!

isAddingParenthesisOrBrackets

	self getCurrentWord.

	^(currentText = '-') and: [((view characterAt: view caretPosition - 1) = $() or: [(view characterAt: view caretPosition - 1) = $[] ]!

isAtEndOfWord

	^(Array with: Character space with: Character tab with: Character cr) includes: (view characterAt: view caretPosition)!

isKeyboardShorcut

	"self getCurrentWord.

	Transcript show: 'previous text [', previousText printString, '] - current text [', currentText printString,']'.
	Transcript cr."

	^self class replacementDictionaryKeys includes: currentText!

lastCharacterIsSpace

	^(view characterAt: view caretPosition - 1) = Character space!

lastGsShape: gsShape!

onAutoComplete: aString startingAt: anInteger accept: aValueHolder 
	"This method is executed when an autocompletation has been entered. The user has selected an option in the autocompletation menu"

	aValueHolder value: false.
 "
	view performUndoableAction: [
		self insertText: aString at: anInteger. 
	].
"

	
	[self insertText: aString at: anInteger.] forkAt: 4!

onKeyPressed: aKeyEvent
	"This is the main method for autocompletation, each key stroke is registered and analyzed here"

	"super onKeyPressed: aKeyEvent. "

	((aKeyEvent wParam = 27) and: [view isAutoCompletionActive]) ifTrue: [^view cancelAutoCompletion]."ESC pressed -> hide autocompletion"
	"((aKeyEvent wParam = 13) and: [view isAutoCompletionActive]) ifTrue: [previousText := currentText. ^currentText := '']." "Enter pressed - autocompletion accepted" 

	(Keyboard default isCtrlDown) ifTrue: [^self]. "the user is pressing ctrl--> not interested in autocompletion"

	(#(37 38 39 40) includes: aKeyEvent wParam) ifTrue: [^[self getCurrentWord] forkAt: 4]. "user using cursor keys left = 37, up = 38, right = 39, down = 40"

	self autocompletationConfiguration isEnabled ifFalse: [^self]. "autocompletation is disable"

	[self processKeyPressed: aKeyEvent. 
	self isKeyboardShorcut ifTrue: [self replaceKeyboardShortcut].
	self isAddingParenthesisOrBrackets ifTrue: [self addParenthesisOrBracketsToSelection]] forkAt: 4.!

onLeftButtonPressed: aKeyEvent
	"This is the main method for autocompletation, each key stroke is registered and analyzed here"

	self getCurrentWord!

onPainted

	view selection notEmpty ifTrue: [lastTextSelection := view selection].!

previousStyleName
	^previousStyleName!

previousStyleName: anObject
	previousStyleName := anObject!

previousText
	^previousText!

previousText: anObject
	^previousText := anObject!

processKeyPressed: aKeyEvent
   "This is the main method for autocompletation, each key stroke is registered and analyzed here"

	self updateLastParsedResult. "to get temporaries & arguments names"

	view isAutoCompletionActive ifTrue: [^self].
	"update scintilla style name"
	currentStyleName := (view styleAt: view caretPosition - 1) name. 
	(currentStyleName = #normal) ifFalse: [previousStyleName := currentStyleName.].

	((Character value: aKeyEvent wParam) = $.) ifTrue: [currentText := ''. ^previousText := ''.].

	self getCurrentWord.

	(aKeyEvent wParam = 8) ifTrue: [^self]. "pressing -backspace-"
	(currentText size > 0) ifFalse: [^self]. "start to pop up autocompletion for words larger than 1 "
	wordRightSideIsEmpty ifFalse: [^self]. "to avoid pop up autocompletion while editing a word"

	(currentStyleName = #identifier) ifTrue: [^self autocompleteForIdentifier].

	(currentStyleName = #unaryMessage) ifTrue: [^self autocompleteForUnaryMessage].

	 (currentStyleName = #normal) ifTrue: [^self autocompleteForNormal]. 

	 (currentStyleName = #unarySelector) ifTrue: [^self autocompleteForUnaryMessage]. 

	(currentStyleName = #literalSymbol) ifTrue: [currentText := currentText allButFirst. 
		(currentText notEmpty and: [currentText first isUpperCase])
			ifTrue: [self autocompleteForUpperIdentifier] 
			ifFalse: [self autocompleteForUnaryMessage]]. !

registry

	^JadeAutocompletationRegistry default!

replaceKeyboardShortcut
	| replacementDictionary replacementText caretPositionShift bracketsOccurrences |

	replacementDictionary := self class replacementDictionary.

	currentText do: [:each | view backspace].

	replacementText := replacementDictionary at: currentText ifAbsent: [^self].
	view insertText: replacementText at: view caretPosition.

	bracketsOccurrences := replacementText occurrencesOf: $].

	(bracketsOccurrences = 0) ifTrue: [caretPositionShift := replacementText size].
	"for ifTrue: [], ifFalse: [] the caret must be inside the brackets --> (-1 to the regular shift)"
	(bracketsOccurrences = 1) ifTrue: [caretPositionShift := replacementText size -1].
	"ifTrue: [] ifFalse: [] inserted --> caretPosition in first bracket"
	(bracketsOccurrences = 2) ifTrue: [caretPositionShift := (replacementText indexOfSubCollection: ']' startingAt: 1) - 1].

	view caretPosition: view caretPosition + caretPositionShift.
!

showAutoCompletionList: anOrdedCollection prefixLength: anInteger

	((anOrdedCollection size = 1) and: [anOrdedCollection last = currentText]) ifTrue: [^self] .

	"^view showAutoCompletionList: ((anOrdedCollection size > 100) ifTrue:[anOrdedCollection first: 100] ifFalse:[anOrdedCollection]) 
			prefixLength: anInteger"

	^view showAutoCompletionList: anOrdedCollection prefixLength: anInteger!

updateLastParsedResult
   "The receiver get the temporaries of the current method being edited"

	^lastParsedResult :=  
		self codePresenterIsMethod
		       ifTrue: [SmalltalkParser parseMethod: self value onError: [:err | lastParsedResult]]
		       ifFalse: [SmalltalkParser parseExpression: self value onError: [:err | lastParsedResult]]
	




!

wordRightSideIsEmpty
	^wordRightSideIsEmpty!

wordRightSideIsEmpty: anObject
	wordRightSideIsEmpty := anObject! !
!JadeAutoTextPresenter3 categoriesFor: #addParenthesisOrBracketsToSelection!keyboard shortcut!public! !
!JadeAutoTextPresenter3 categoriesFor: #autocompletationConfiguration!public! !
!JadeAutoTextPresenter3 categoriesFor: #autocompleteForIdentifier!autocompletation cycle!public! !
!JadeAutoTextPresenter3 categoriesFor: #autocompleteForNormal!autocompletation cycle!public! !
!JadeAutoTextPresenter3 categoriesFor: #autocompleteForUnaryMessage!autocompletation cycle!public! !
!JadeAutoTextPresenter3 categoriesFor: #autocompleteForUpperIdentifier!autocompletation cycle!public! !
!JadeAutoTextPresenter3 categoriesFor: #codePresenterIsMethod!public! !
!JadeAutoTextPresenter3 categoriesFor: #configuration!public! !
!JadeAutoTextPresenter3 categoriesFor: #createSchematicWiring!key event!public! !
!JadeAutoTextPresenter3 categoriesFor: #currentGsShape!accessing!private! !
!JadeAutoTextPresenter3 categoriesFor: #currentGsShape:!accessing!private! !
!JadeAutoTextPresenter3 categoriesFor: #currentStyleName!accessing!private! !
!JadeAutoTextPresenter3 categoriesFor: #currentStyleName:!accessing!private! !
!JadeAutoTextPresenter3 categoriesFor: #currentText!accessing!private! !
!JadeAutoTextPresenter3 categoriesFor: #currentText:!accessing!private! !
!JadeAutoTextPresenter3 categoriesFor: #getArgumentsNames!autocompletion list!public! !
!JadeAutoTextPresenter3 categoriesFor: #getBreakCharacters!public! !
!JadeAutoTextPresenter3 categoriesFor: #getClassInstancesVariables!autocompletion list!public! !
!JadeAutoTextPresenter3 categoriesFor: #getClassName!public! !
!JadeAutoTextPresenter3 categoriesFor: #getCurrentWord!public! !
!JadeAutoTextPresenter3 categoriesFor: #getNamesForClassUnaryMessage!autocompletion list!public! !
!JadeAutoTextPresenter3 categoriesFor: #getNamesForIdentifier!autocompletion list!public! !
!JadeAutoTextPresenter3 categoriesFor: #getNamesForUnaryMessage!autocompletion list!public! !
!JadeAutoTextPresenter3 categoriesFor: #getNameUpperIdentifier!autocompletion list!public! !
!JadeAutoTextPresenter3 categoriesFor: #getSelectedGsShape!public! !
!JadeAutoTextPresenter3 categoriesFor: #getTemporaries!autocompletion list!public! !
!JadeAutoTextPresenter3 categoriesFor: #initialize!public! !
!JadeAutoTextPresenter3 categoriesFor: #insertBasicText:at:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter3 categoriesFor: #insertKeywordText:at:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter3 categoriesFor: #insertKeywordText2:at:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter3 categoriesFor: #insertSimpleText:at:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter3 categoriesFor: #insertText:at:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter3 categoriesFor: #isAddingParenthesisOrBrackets!keyboard shortcut!public! !
!JadeAutoTextPresenter3 categoriesFor: #isAtEndOfWord!public! !
!JadeAutoTextPresenter3 categoriesFor: #isKeyboardShorcut!keyboard shortcut!public! !
!JadeAutoTextPresenter3 categoriesFor: #lastCharacterIsSpace!public! !
!JadeAutoTextPresenter3 categoriesFor: #lastGsShape:!public! !
!JadeAutoTextPresenter3 categoriesFor: #onAutoComplete:startingAt:accept:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter3 categoriesFor: #onKeyPressed:!key event!public! !
!JadeAutoTextPresenter3 categoriesFor: #onLeftButtonPressed:!key event!public! !
!JadeAutoTextPresenter3 categoriesFor: #onPainted!keyboard shortcut!public! !
!JadeAutoTextPresenter3 categoriesFor: #previousStyleName!accessing!private! !
!JadeAutoTextPresenter3 categoriesFor: #previousStyleName:!accessing!private! !
!JadeAutoTextPresenter3 categoriesFor: #previousText!accessing!private! !
!JadeAutoTextPresenter3 categoriesFor: #previousText:!accessing!private! !
!JadeAutoTextPresenter3 categoriesFor: #processKeyPressed:!key event!public! !
!JadeAutoTextPresenter3 categoriesFor: #registry!autocompletion!public! !
!JadeAutoTextPresenter3 categoriesFor: #replaceKeyboardShortcut!keyboard shortcut!public! !
!JadeAutoTextPresenter3 categoriesFor: #showAutoCompletionList:prefixLength:!public! !
!JadeAutoTextPresenter3 categoriesFor: #updateLastParsedResult!autocompletion list!public! !
!JadeAutoTextPresenter3 categoriesFor: #wordRightSideIsEmpty!accessing!private! !
!JadeAutoTextPresenter3 categoriesFor: #wordRightSideIsEmpty:!accessing!private! !

!JadeAutoTextPresenter3 class methodsFor!

initializeReplacementDictionary

	^ReplacementDictionary := Dictionary new
							at: '-g' put: ':= ';
							at: '-t' put: 'ifTrue: []';
							at: '-f' put: 'ifFalse: []';
							at: '-b' put: 'ifTrue: [] ifFalse: []';
							at: '-v' put: 'ifFalse: [] ifTrue: []';
							yourself
	
	!

replacementDictionary

	ReplacementDictionary ifNil: [self initializeReplacementDictionary].

	^ReplacementDictionary!

replacementDictionaryKeys

	ReplacementDictionary ifNil: [self initializeReplacementDictionary].

	^ReplacementDictionary keys! !
!JadeAutoTextPresenter3 class categoriesFor: #initializeReplacementDictionary!public! !
!JadeAutoTextPresenter3 class categoriesFor: #replacementDictionary!public! !
!JadeAutoTextPresenter3 class categoriesFor: #replacementDictionaryKeys!public! !

