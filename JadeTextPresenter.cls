"Filed out from Dolphin Smalltalk 7"!

TextPresenter subclass: #JadeTextPresenter
	instanceVariableNames: ''
	classVariableNames: 'ColorForCompileError ColorForNoEdits ColorForUnsavedEdits JadeTextStyles'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeTextPresenter guid: (GUID fromString: '{483A5019-CE38-4F6A-B12D-D39804FC00F4}')!
JadeTextPresenter comment: ''!
!JadeTextPresenter categoriesForClass!Unclassified! !
!JadeTextPresenter methodsFor!

selection

	^view selection.
! !
!JadeTextPresenter categoriesFor: #selection!public! !

!JadeTextPresenter class methodsFor!

colorForCompileError

	ColorForCompileError ifNil: [ColorForCompileError := Color red: 255 green: 220 blue: 255].
	^ColorForCompileError.!

colorForCompileError: aColor

	ColorForCompileError := aColor.
!

colorForNoEdits

	ColorForNoEdits ifNil: [ColorForNoEdits := Color red: 253 green: 222 blue: 181].
	^ColorForNoEdits.
!

colorForNoEdits: aColor

	ColorForNoEdits := aColor.
!

colorForUnsavedEdits

	ColorForUnsavedEdits ifNil: [ColorForUnsavedEdits := Color red: 255 green: 255 blue: 180].
	^ColorForUnsavedEdits.!

colorForUnsavedEdits: aColor

	ColorForUnsavedEdits := aColor.!

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
	JadeTextPresenter resetColors.
	ColorDialog showModalOn: JadeTextPresenter colorForNoEdits.
"
	ColorForCompileError := nil.
	ColorForNoEdits := nil.
	ColorForUnsavedEdits := nil.
!

textStyles

	JadeTextStyles ifNil: [JadeTextStyles := self defaultTextStyles].
	^JadeTextStyles.


!

textStyles: aSortedCollection

	JadeTextStyles := aSortedCollection.
! !
!JadeTextPresenter class categoriesFor: #colorForCompileError!public! !
!JadeTextPresenter class categoriesFor: #colorForCompileError:!public! !
!JadeTextPresenter class categoriesFor: #colorForNoEdits!public! !
!JadeTextPresenter class categoriesFor: #colorForNoEdits:!public! !
!JadeTextPresenter class categoriesFor: #colorForUnsavedEdits!public! !
!JadeTextPresenter class categoriesFor: #colorForUnsavedEdits:!public! !
!JadeTextPresenter class categoriesFor: #defaultTextStyles!public! !
!JadeTextPresenter class categoriesFor: #resetColors!public! !
!JadeTextPresenter class categoriesFor: #textStyles!public! !
!JadeTextPresenter class categoriesFor: #textStyles:!public! !

