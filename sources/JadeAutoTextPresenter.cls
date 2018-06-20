"Filed out from Dolphin Smalltalk 7"!

JadeTextPresenter subclass: #JadeAutoTextPresenter
	instanceVariableNames: 'lastGsShape lastWord lastCharacter currentWord temporaries arguments altMode'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeAutoTextPresenter guid: (GUID fromString: '{f845e86e-2c61-4193-a936-e7c4d7078136}')!
JadeAutoTextPresenter comment: 'This class use the registry (JadeAutocompletationRegistry)  to get information about the GsClasses and methods.

It uses this information to show autocompletation options for the current text being typed.


see:
#onKeyPressed:'!
!JadeAutoTextPresenter categoriesForClass!Unclassified! !
!JadeAutoTextPresenter methodsFor!

autocompletationConfiguration
	^JadeAutocompletationConfiguration default!

autocompleteForAlphaNumeric
	"This method is triggered when an alpha numeric key is pressed.
	If ALT Mode is enabled then show the autocompletation for the Right Alt key.
	If the current typed word <currentWord> start with an uppercase character --> it is a Class name, a Class Variable name, a Class Inst Var name or a Pool dictionary.
	If NOT (lowercase) --> shows all possible names "

	currentWord trimBlanks isEmpty ifTrue: [^self].

	altMode ifTrue: [^self autocompleteForRightAlt].

	currentWord trimBlanks first isUppercase ifTrue: [	self logMethodName: 'autocompleteForAlphaNumeric - uppercase Classes names' info: #().
		^self showAutoCompletionList: self getClassesName prefixLength: 0 ]. "we have to add class var and class inst var ?"

self logMethodName: 'autocompleteForAlphaNumeric - All Names' info: #().
	^self showAutoCompletionList: self getNamesForAlphaNumeric prefixLength: 0!

autocompleteForEnter
	"This method is triggered when Enter key is pressed.
	The receiver store the current typed word <currentWord> into <lastWord>, reset the ALT Mode"

	self logMethodName: 'autocompleteForEnter' info: #().

	lastWord := currentWord.

	altMode := false.

	self clearAutocompletation.!

autocompleteForRightAlt
	"This method is triggered when Right Alt key is pressed.
	It shows all possible names after an assignment: method arguments, method temporaries and object's inst var, "

	self logMethodName: 'autocompleteForRightAlt' info: #().

	self showAutoCompletionList: self getNamesAfterAssigment prefixLength: 0.

	altMode := true.!

autocompleteForSpace
	"This method is triggered when <space> key is pressed.
	Update the last typed word <lastWord> and clear the autocompleation.
	Check the last typed word <lastWord> to guess what autocompletation list it will fit better for <lastWord>"

	self logMethodName: 'autocompleteForSpace' info: #().
	altMode := false.
	lastWord := currentWord isEmpty ifFalse: [currentWord] ifTrue: [lastWord].
	self clearAutocompletation.
	" -self- typed --> show methods for -self-"
	((lastWord trimBlanks asLowercase = 'self') and: [lastGsShape notNil]) 
		ifTrue: [^self showAutoCompletionList: self getMethodsForSelf prefixLength: 0. ].
	"-uppercase- typed and is a class name --> show class names"
	(lastWord notEmpty and: [((lastWord first isUppercase) and: [self registry includesClassNamed: lastWord trimBlanks])]) 
		ifTrue: [^self showAutoCompletionList: (self getClassMethodsFor: lastWord trimBlanks) prefixLength: 0.].
	" -registered var name- typed --> show the methods of the class related to the map (check JadeAutocompletationRegistry variableMap)"
	(self registry hasMapFor: lastWord trimBlanks) 
		ifTrue: [^self showAutoCompletionList: self getMappedStringToClass prefixLength: 0. ].
	"-assignment- typed --> show possible names after assignment"
	(lastWord trimBlanks notEmpty and: [lastWord trimBlanks = ';=' ]) 
		ifTrue: [^self showAutoCompletionList: self getNamesAfterAssigment prefixLength: 0.].	

	"otherwise it return all possible methods in the system"
	self showAutoCompletionList: self getMethodsForAnyone prefixLength: 0.

!

autocompleteForTab

	self showAutoCompletionList: self getNamesAfterAssigment prefixLength: 0.

!

clearAutocompletation
	"The receiver after character <space> or <enter> reset the current registered word <currentWord>.
	If <currentWord> is not empty --> store it in <lastWord>.
	The clear <currentWord>"

self logMethodName: 'clearAutocompletation ' info: #().

	currentWord notEmpty ifTrue: [lastWord := currentWord].
	currentWord := ''!

codePresenterIsMethod

	^parentPresenter codePresenterIsMethod!

configuration

	^JadeAutocompletationConfiguration default!

controlKeys
	
	^#(
		'ALT'
		'CTRL' 
		'DOWN' 
		'END' 			'ENTER' 			'ESC' 
		'HOME' 
		'LEFT' 			'LEFT ALT'		'LEFT CTRL'		'LEFT SHIFT' 
		'PGDOWN' 	'PGUP' 
		'RIGHT' 		'RIGHT ALT' 	'RIGHT CTRL'		'RIGHT SHIFT' 
		'SHIFT'		'SPACE' 
		'TAB' 
		'UP' 
	).
!

createSchematicWiring

	super createSchematicWiring.

	
	self when: #autoComplete:startingAt:accept: send: #onAutoComplete:startingAt:accept:  to: self.!

currentWord
	^currentWord!

currentWord: anObject
	currentWord := anObject!

getArgumentsNames
   "Get and set receiver's inst var <arguments>"
   | parseResult |

   self codePresenterIsMethod ifFalse: [^SortedCollection new].

   parseResult := SmalltalkParser parseMethod: self value onError: [:err | ^arguments := SortedCollection new].

   ^arguments := parseResult argumentNames asSortedCollection
!

getClassesName
	"The receiver (from registry) get all classes which includes <aString> in it's name"

	^self registry getClassesNamedWith: currentWord!

getClassMethodsFor: className
	"The receiver get all method from class named <className>"

	^(self registry getClass: className) gsClassMethods!

getMappedStringToClass
	"The receiver get and set the class related to var text name <lastWord> in the mapping of the registry.
	JadeAutocompletationRegistry <variableMap>"

	lastGsShape := self registry mapFor: lastWord.

	^self getMethodsForSelf!

getMethodsForAnyone
	"The receiver get all methods stored in the registry"

	^self registry getAllGlobalMethods!

getMethodsForSelf

	^lastGsShape getAllMethods!

getNamesAfterAssigment
   "The receiver get all possible text names after an assigment.
   Class name, method temporary, method argument, inst var, class var, inst class var or pool dictionary"
   | gsNames afterAssignment |

	afterAssignment := lastGsShape ifNotNil: [:value | value getNamesAfterAssigment] ifNil: [SortedCollection new].
   gsNames := afterAssignment, self getTemporaries, self getArgumentsNames. "method args, inst var"

   currentWord isEmpty ifTrue: [^gsNames].

   ^gsNames select: [:each | (each indexOfSubCollection: currentWord) ~=  0]!

getNamesForAlphaNumeric
	"The receiver get all possible names for the current word being typed <currentWord>.
	- All method arguments.
	- All possible methods.
	- All method tempories that match with <currentWord>"

	^(self getArgumentsNames, self getMethodsForAnyone, self getTemporaries) select: [:each | 
		(currentWord size >= each size) ifFalse: [(each first: currentWord size) = currentWord] ifTrue: [false]]!

getTemporaries
   "The receiver get the temporaries of the current method being edited"

   temporaries := self codePresenterIsMethod
               ifTrue: [(SmalltalkParser parseMethod: self value onError: [:err | ^temporaries := SortedCollection new]) body temporaryVariables asSortedCollection]
               ifFalse: [(SmalltalkParser parseExpression: self value onError: [:err | ^temporaries := SortedCollection new]) temporaryVariables asSortedCollection].

   ^temporaries
!

initialize
	super initialize.
	lastWord := ''.
	currentWord := ''.
	altMode := false.
	temporaries := OrderedCollection new.
	arguments := OrderedCollection new.

"	lastGsShape 		- current JadeGsClassShape to get information for autocompletation process.
	lastWord 			- last typed word	
	lastCharacter 		- last typed character
	currentWord 		- current typed word
	temporaries 		- temporaries of the current method being typed
	arguments 		- arguments of the current method being typed
	altMode			- wheter the receiver is in Right Alt mode
" !

insertText: aString at: anInteger
	"The receiver insert <aString> at the position <anInteger>. This method is called when the user select an option in the autocompletation menu"
	| stringWithArgs |

	self logMethodName: 'insert:Text: (string to insert at: )' info: (Array with: aString with: anInteger printString).

	(aString includes: $:) ifFalse: [ | text | "is a string without : --> a var name or a unary selector or a class name"
						text := (aString size >= lastWord size) ifTrue: [aString allButFirst: lastWord size] ifFalse: [aString].
						self logMethodName: 'insert:Text: (text typed + to insert)' info: (Array with: text).
						view insertText: text at: view caretPosition; moveToEndOfWord.
						lastWord := aString.
						^currentWord := ''.].
	stringWithArgs := ''.
	"inserting a binary selector (with :)"
	((aString allButFirst: lastWord size) subStrings: ':') keysAndValuesDo: [:key :value | stringWithArgs := stringWithArgs, value, ': ', (self registry paramFor: aString index: key), ' '].

	currentWord trimBlanks notEmpty ifTrue: [self logMethodName: 'insert:Text: (binary selector1)' info: (Array with: stringWithArgs).
		^view insertText: stringWithArgs  at: anInteger]. "the user has typed some keys of the autocomplation"

	view insertText: stringWithArgs allButLast at: view caretPosition; moveToEndOfWord. "the user has NOT type any key of the selected autocompletation"
	self logMethodName: 'insert:Text: (binary selector2)' info: (Array with: stringWithArgs).
	lastWord := aString.
	currentWord := ''.
	!

lastCharacter
	^lastCharacter!

lastCharacter: anObject
	lastCharacter := anObject!

lastGsShape
	^lastGsShape!

lastGsShape: anObject
	lastGsShape := anObject!

lastWord
	^lastWord!

lastWord: anObject 
	lastWord := anObject!

logMethodName: methodName info: anArray
	| fileStream |

	self configuration logEnabled ifFalse: [^self].

	fileStream := FileStream write: 'autocompletion.log' mode: #'append'.
	fileStream
		nextPutAll: 'time: ', Time now asMilliseconds printString; cr;
		nextPutAll: methodName ; cr; 
		nextPutAll: 'lastCharacter: ', lastCharacter printString; cr;
		nextPutAll: 'currentWord: ', currentWord; cr;
		nextPutAll: 'lastWord: ', lastWord; cr.
	anArray do: [:each | fileStream nextPutAll: ('info: ', each); cr].
	fileStream
		nextPutAll: '----------------------------------------------------------------------'; cr;
		close.!

onAutoComplete: aString startingAt: anInteger accept: aValueHolder 
	"This method is executed when an autocompletation has been entered. The user has selected an option in the autocompletation menu"

	self logMethodName: 'onAutoComplete: (before insertText:)' info: (Array with: aString with: anInteger printString).
	[self insertText: aString at: anInteger. "insert selected option"
	view moveToEndOfLine] forkAt: 4 . 

	aValueHolder value: false.

	lastWord := aString. "we store the selected autocompletation in order to guess the new autocompletation menu for the <lastWord>"

	altMode := false.

	"self transcriptOutput."!

onKeyPressed: aKeyEvent
	"This is the main method for autocompletation, each key stroke is registered and analyzed here"

	"super onKeyPressed: aKeyEvent."

	self autocompletationConfiguration isEnabled ifFalse: [^self]. "autocompletation is disable"

	[self processKeyPressed: aKeyEvent] forkAt: 4!

processKeyPressed: aKeyEvent
   "This is the main method for autocompletation, each key stroke is registered and analyzed here"
   | keyName |

   keyName := Keyboard keyNameFromLParam: aKeyEvent lParam. "get the name of the pressed Key "
   (keyName = 'F2') ifTrue: [^JadeAutocompletationConfigurationShell show].

   self logMethodName: 'processKeyPressed: ' info: (Array with: keyName).

   (keyName size = 1) ifTrue: [self updateAutocompletationTextWith: aKeyEvent]. "we update the <currentWord> with the key pressed if is not a control key"

   aKeyEvent wParam = 32 "space" ifTrue: [self autocompleteForSpace].

   aKeyEvent wParam = 13 "enter" ifTrue: [self autocompleteForEnter].

   (Keyboard default isKeyDown: 165) "right alt" ifTrue: [self autocompleteForRightAlt].

   (Character value: aKeyEvent wParam) isAlphaNumeric ifTrue: [self autocompleteForAlphaNumeric]!

registry

	^JadeAutocompletationRegistry default!

showAutoCompletionList: anOrdedCollection prefixLength: anInteger

	^view showAutoCompletionList: anOrdedCollection prefixLength: anInteger!

transcriptOutput

	Transcript nextPutAll: String lineDelimiter.
	Transcript nextPutAll: lastCharacter asString, ' - CW: ', currentWord , ' - LW: ', lastWord.
	Transcript flush.!

updateAutocompletationTextWith: aKeyEvent 
	"The receiver register <aKeyEvent> and update <currentWord> by adding the pressed key"
	| key |

	key := Keyboard keyNameFromLParam: aKeyEvent lParam. "get the key value"
	key := Keyboard default isShiftDown ifTrue: [key] ifFalse: [key asLowercase]. 
	(key = '-') ifTrue: [key := '_']. "- is the character for _ , so we simply change it"

	aKeyEvent wParam = 16 "shift" ifTrue: [^self].
	lastCharacter := Character value: aKeyEvent wParam. "get Character from <aKeyEvent>"

	Keyboard default isShiftDown ifFalse: [lastCharacter := lastCharacter asLowercase].
	"update the <currentWord> that is being typed"
	self currentWord:  ((lastCharacter == Character backspace) 
					ifTrue: [currentWord notEmpty ifTrue: [currentWord allButLast] ifFalse: ['']]
					ifFalse: [currentWord , key]).
	"self transcriptOutput."! !
!JadeAutoTextPresenter categoriesFor: #autocompletationConfiguration!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #autocompleteForAlphaNumeric!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #autocompleteForEnter!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #autocompleteForRightAlt!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #autocompleteForSpace!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #autocompleteForTab!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #clearAutocompletation!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #codePresenterIsMethod!public! !
!JadeAutoTextPresenter categoriesFor: #configuration!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #controlKeys!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #createSchematicWiring!public! !
!JadeAutoTextPresenter categoriesFor: #currentWord!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #currentWord:!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #getArgumentsNames!autocompletion!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #getClassesName!public! !
!JadeAutoTextPresenter categoriesFor: #getClassMethodsFor:!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #getMappedStringToClass!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #getMethodsForAnyone!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #getMethodsForSelf!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #getNamesAfterAssigment!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #getNamesForAlphaNumeric!autocompletion!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #getTemporaries!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #initialize!public! !
!JadeAutoTextPresenter categoriesFor: #insertText:at:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #lastCharacter!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #lastCharacter:!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #lastGsShape!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #lastGsShape:!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #lastWord!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #lastWord:!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #logMethodName:info:!public! !
!JadeAutoTextPresenter categoriesFor: #onAutoComplete:startingAt:accept:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #onKeyPressed:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #processKeyPressed:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #registry!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #showAutoCompletionList:prefixLength:!autocompletation cycle!public! !
!JadeAutoTextPresenter categoriesFor: #transcriptOutput!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #updateAutocompletationTextWith:!autocompletation cycle!autocompletion!public! !

