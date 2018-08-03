"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadeTextStyles
	instanceVariableNames: 'colorForCompileError colorForNoEdits colorForUnsavedEdits textStyles'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'default'!
JadeTextStyles guid: (GUID fromString: '{2dadb98c-2aaa-4ce2-8457-7737a2104ac4}')!
JadeTextStyles comment: ''!
!JadeTextStyles categoriesForClass!Kernel-Objects! !
!JadeTextStyles methodsFor!

colorForCompileError
	^colorForCompileError!

colorForCompileError: anObject
	colorForCompileError := anObject!

colorForNoEdits
	^colorForNoEdits!

colorForNoEdits: anObject
	colorForNoEdits := anObject!

colorForUnsavedEdits
	^colorForUnsavedEdits!

colorForUnsavedEdits: anObject
	colorForUnsavedEdits := anObject!

defaultTextStyles
	"Copied from SmalltalkWorkspace class>>#'defaultTextStyles' with permission from Object-Arts."

	| answer |
	answer := Set new.
	answer
		add: ((ScintillaTextStyle name: #illegal)
					description: 'Illegal characters, e.g. £';
					forecolor: Color red;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #comment)
					description: 'Comments in method source';
					forecolor: Color darkGreen;
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: #identifier)
					description: 'Variables references (instance, temporary and class)';
					yourself);
		add: ((ScintillaTextStyle name: #argDecl)
					description: 'Argument declaration in method signature';
					isItalic: true;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #blockArgDecl)
					description: 'Block argument declaration';
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: #tempDecl)
					description: 'Temporary variable declaration';
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: #unarySelector)
					description: 'Unary selectors (method signature)';
					forecolor: Color blue;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #unaryMessage)
					description: 'Unary (no argument) messages';
					forecolor: Color blue;
					yourself);
		add: ((ScintillaTextStyle name: #binarySelector)
					description: 'Binary in-fix selectors such as + and - (method signature)';
					forecolor: Color blue;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #binaryMessage)
					description: 'Binary in-fix messages such as + and -';
					forecolor: Color blue;
					yourself);
		add: ((ScintillaTextStyle name: #keywordSelector)
					description: 'Components of multi-keyword message selectors';
					forecolor: Color blue;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #keywordMessage)
					description: 'Components of multi-keyword message selectors';
					forecolor: Color blue;
					yourself);
		add: ((ScintillaTextStyle name: #literalPseudo)
					description: 'The literal constants true, false and nil';
					forecolor: Color darkCyan;
					yourself);
		add: ((ScintillaTextStyle name: #literalNumber)
					description: 'Numeric literal constants, e.g. 1.2e6';
					forecolor: Color darkRed;
					yourself);
		add: ((ScintillaTextStyle name: #literalString)
					description: 'Literal string constants, e.g. ''abc''';
					forecolor: Color darkMagenta;
					yourself);
		add: ((ScintillaTextStyle name: #literalSymbol)
					description: 'Literal symbol constants, e.g. #abc';
					forecolor: Color darkBlue;
					yourself);
		add: ((ScintillaTextStyle name: #literalCharacter)
					description: 'Literal character constants, e.g. $A';
					forecolor: Color darkBlue;
					yourself);
		add: ((ScintillaTextStyle name: #literalBytes)
					description: 'Literal byte arrays, e.g. #[0 1 2]';
					forecolor: Color brown;
					yourself);
		add: ((ScintillaTextStyle name: #assignment)
					description: 'Assignment operation, i.e. :=';
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: #tempOpenBar)
					description: 'Temporary declarations opening bar';
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: #tempCloseBar)
					description: 'Temporary declarations closing bar';
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: #specialCharacter)
					description: 'Special characters, e.g. normal and block parentheses';
					yourself);
		add: ((ScintillaTextStyle name: #literalArray)
					description: 'Opening/closing token of literal array, i.e. #()';
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #tag)
					description: 'Primitive or external call tag, e.g. <primitive: 1>';
					forecolor: Color darkGray;
					isItalic: true;
					yourself);
		yourself.
	self assert: [answer size < 32].
	"Scintilla pre-defined styles - note how #normal style inherits font of the view (which in turn should be the default system font)"
	answer
		add: ScintillaTextStyle normal yourself;
		add: ((ScintillaTextStyle name: #indentGuide)
					description: 'Indentation guides, when visible';
					forecolor: Color gray;
					yourself);
		add: ((ScintillaTextStyle name: #braceHighlight)
					description: 'Matching brace, when brace highlighting enabled';
					forecolor: Color blue;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #braceMismatch)
					description: 'Mismatched brace, when brace highlighting enabled';
					forecolor: Color red;
					isBold: true;
					yourself);
		yourself.
	^answer!

resetColors
"
	JadeTextStyles default resetColors.
	ColorDialog showModalOn: JadeTextStyles default colorForNoEdits.
"
	colorForCompileError 	:= RGB red: 250 green: 220 blue: 200.
	colorForNoEdits 		:= RGB red: 250 green: 240 blue: 200.
	colorForUnsavedEdits 	:= RGB red: 240 green: 250 blue: 200.
!

textStyles

	textStyles isNil ifTrue: [self textStyles: self defaultTextStyles].
	^textStyles!

textStyles: anObject
	textStyles := anObject! !
!JadeTextStyles categoriesFor: #colorForCompileError!accessing!public! !
!JadeTextStyles categoriesFor: #colorForCompileError:!accessing!public! !
!JadeTextStyles categoriesFor: #colorForNoEdits!accessing!public! !
!JadeTextStyles categoriesFor: #colorForNoEdits:!accessing!public! !
!JadeTextStyles categoriesFor: #colorForUnsavedEdits!accessing!public! !
!JadeTextStyles categoriesFor: #colorForUnsavedEdits:!accessing!public! !
!JadeTextStyles categoriesFor: #defaultTextStyles!public! !
!JadeTextStyles categoriesFor: #resetColors!public! !
!JadeTextStyles categoriesFor: #textStyles!accessing!public! !
!JadeTextStyles categoriesFor: #textStyles:!accessing!public! !

!JadeTextStyles class methodsFor!

default

	^default ifNil: [default := self new].
! !
!JadeTextStyles class categoriesFor: #default!public! !

