| package |
package := Package name: 'SSW Widget Enhancements'.
package paxVersion: 1;
	basicComment: '©2006 Solutions Software Limited'.

package basicPackageVersion: '0.001'.


package classNames
	add: #EmulatedCheckBox;
	add: #FormattedTextEdit;
	yourself.

package methodNames
	add: #Canvas -> #erase:;
	add: #Canvas -> #erase:color:;
	add: #Date -> #shortString;
	add: #KeyEvent -> #resendTo:;
	add: #PointEvent -> #resendTo:;
	add: 'Color class' -> #grayText;
	add: 'Color class' -> #highlightText;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	yourself).

package!

"Class Definitions"!

View subclass: #EmulatedCheckBox
	instanceVariableNames: 'isTransition isChecked hasCursor isThemed'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TextEdit subclass: #FormattedTextEdit
	instanceVariableNames: 'isFormatting isErroring format separatorChars placeholderChar'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Canvas methodsFor!

erase: aRectangle
	"Erase the receiver to the current background colour"

	self erase: aRectangle color: self backcolor!

erase: aRectangle color: aColor
	"Erase the receiver to the current background colour"

	self fillRectangle: aRectangle brush: (Brush color: aColor)! !
!Canvas categoriesFor: #erase:!drawing!public! !
!Canvas categoriesFor: #erase:color:!drawing!public! !

!Color class methodsFor!

grayText
	"Answer the Gray Text system color."

	^self systemColor: COLOR_GRAYTEXT!

highlightText
	"Answer the Highlight Text system color."

	^self systemColor: COLOR_HIGHLIGHTTEXT! !
!Color class categoriesFor: #grayText!constants!public! !
!Color class categoriesFor: #highlightText!constants!public! !

!Date methodsFor!

shortString

	| stream |

	stream := WriteStream on: (String new: 10).

	self printOn: stream longPicture: false.

	^stream contents! !
!Date categoriesFor: #shortString!printing!public! !

!KeyEvent methodsFor!

resendTo: aView

	"Send this message onto aView"

	aView 
		sendMessage: message 
		wParam: wParam 
		lParam: lParam! !
!KeyEvent categoriesFor: #resendTo:!public!testing! !

!PointEvent methodsFor!

resendTo: aView

	"Send this message onto aView, translating the position"

	| newPoint newLParam |

	newLParam := DWORD fromInteger: lParam.
	newPoint := self window mapPoint: self position to: aView.
	newLParam 
		lowSWord: newPoint x;
		highSWord: newPoint y.

	aView 
		sendMessage: message 
		wParam: wParam 
		lParam: newLParam asInteger! !
!PointEvent categoriesFor: #resendTo:!accessing!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

EmulatedCheckBox guid: (GUID fromString: '{0C2F590C-7A32-472F-8BDA-78BEC35B9903}')!
EmulatedCheckBox comment: ''!
!EmulatedCheckBox categoriesForClass!Unclassified! !
!EmulatedCheckBox methodsFor!

defaultWindowStyle

	"Private - Answer a default style to use when creating an instance of the receiver.
	All controls have the WS_TABSTOP style by default."

	^super defaultWindowStyle bitOr: WS_TABSTOP
!

draw: aValue transition: aBoolean 
	in: aRectangle on: aCanvas 
	forecolor: fcolor backcolor: bcolor 
	focus: focusBool highlight: highlightBool

	^self isThemed
	ifTrue: 
		[self 
			drawXP: aValue 
			transition: aBoolean 
			in: aRectangle 
			on: aCanvas 
			forecolor: fcolor
			backcolor: bcolor
			focus: focusBool
			highlight: highlightBool]
	ifFalse: 
		[self 
			drawNonXP: aValue 
			transition: aBoolean 
			in: aRectangle 
			on: aCanvas 
			forecolor: fcolor
			backcolor: bcolor
			focus: focusBool
			highlight: highlightBool]!

drawNonXP: aValue transition: aBoolean 
	in: aRectangle on: aCanvas 
	forecolor: fcolor backcolor: bcolor 
	focus: isFocus highlight: isHighlighted

	| size origin box uState | 

	size := 15.
	origin := ((aRectangle width - size) //2)@((aRectangle height - size) //2).
	box := (aRectangle origin + origin) extent: (size@size).
	
	aValue
		ifTrue: [uState := 16r0400 "DFCS_CHECKED"]
		ifFalse: [uState := 0].
	aBoolean ifTrue: [uState := uState | 16r0200 "DFCS_PUSHED"].

	UserLibrary default 
		drawFrameControl: aCanvas handle
		lprc: (RECT fromRectangle: box) 
		uType: 4 "DFC_BUTTON" 
		uState: uState.

	isFocus ifTrue: [self drawFocusRect: (box insetBy: 2)]
!

drawOn: aCanvas

	self 
		draw: self isChecked 
		transition: self isTransition 
		in: (0@0 extent: self extent) 
		on: aCanvas
		forecolor: self forecolor
		backcolor: self backcolor
		focus: self hasFocus
		highlight: self isHighlighted!

drawXP: aValue transition: aBoolean 
	in: aRectangle on: aHdc 
	forecolor: fcolor backcolor: bcolor 
	focus: isFocus highlight: isHighlighted

	| size origin box canvas htheme iState |

	size := 15.

	origin := ((aRectangle width - size) //2)@((aRectangle height - size) //2).

	box := (aRectangle origin + origin) extent: (size@size).

	canvas := Canvas withNonOwnedDC: aHdc.

	htheme := ThemeLibrary default openThemeData: self handle pszClassList: 'BUTTON' asUnicodeString.

	aValue
		ifTrue: [iState := 5 "CBS_CHECKEDNORMAL"]
		ifFalse: [iState := 1 "CBS_UNCHECKEDNORMAL"].
	aBoolean 
		ifTrue: [iState := iState + 2 "CBS_xxxxPRESSED"]
		ifFalse: [isHighlighted ifTrue: [iState := iState + 1]].

	ThemeLibrary default 
		drawThemeBackground: htheme 
		hdc: canvas asParameter 
		iPartId: 3 "BP_CHECKBOX"
		iStateId: iState 
		pRect: box asParameter 
		pClipRect: nil.

	ThemeLibrary default closeThemeData: htheme.

	canvas free
!

hasCursor
	^hasCursor ifNil: [false]!

hasCursor: anObject
	hasCursor := anObject!

isChecked
	^isChecked ifNil: [isChecked := false"self model value"]!

isChecked: anObject
	isChecked := anObject!

isHighlighted

	^self hasCursor!

isThemed

	^isThemed ifNil:
		[isThemed := 
			OSVERSIONINFO current isWinXP 
			and: [ThemeLibrary default isThemeActive 
			and: [ThemeLibrary default isAppThemed]]]
!

isTransition
	^isTransition ifNil: [isTransition := false]!

isTransition: anObject
	isTransition := anObject!

onKeyPressed: aKeyEvent

	aKeyEvent code = Character space codePoint ifTrue:
		[self isTransition: true.
		self invalidate].

	^super onKeyPressed: aKeyEvent!

onKeyReleased: aKeyEvent

	(self isTransition and: [aKeyEvent code = Character space codePoint]) ifTrue:
		[self isChecked: self isChecked not.
		self updateModel].

	self isTransition: false.
	self invalidate.

	^super onKeyReleased: aKeyEvent!

onKillFocus

	self isTransition: false.

	self invalidate.

	^super onKillFocus!

onLeftButtonDoubleClicked: aMouseEvent
	"The event source received a mouse left button double-click event
	Treat as a single click"

	^self onLeftButtonPressed: aMouseEvent!

onLeftButtonPressed: aMouseEvent

	self hasFocus ifFalse: [self setFocus].
	self isTransition: true.
	self invalidate.

	^super onLeftButtonPressed: aMouseEvent!

onLeftButtonReleased: aMouseEvent

	self isTransition ifTrue:
		[self isChecked: self isChecked not.
		self updateModel].

	self isTransition: false.
	self invalidate.

	^super onLeftButtonReleased: aMouseEvent!

onMouseMoved: aMouseEvent

	| wasTransition hadCursor point |

	(aMouseEvent isLButtonDown and: [self isTransition not]) ifTrue: [self isTransition: true].

	hadCursor := self hasCursor ifNil: [false].
	wasTransition := self isTransition.
	point := aMouseEvent position.

	self hasCursor: (self clientRectangle containsPoint: point). 
	self isTransition: (wasTransition and: [self hasCursor]). 

	(self hasCursor = hadCursor and: [self isTransition = wasTransition]) ifFalse: 
		[self invalidate.
		(self hasCursor and: [hadCursor not]) ifTrue: [self trackMouseLeave]]!

onPaintRequired: aPaintEvent

	self drawOn: aPaintEvent canvas
!

onSetFocus

	self invalidate.

	^super onSetFocus!

refreshContents

	self isChecked: self model value.
	self invalidate!

updateModel

	self model value: self isChecked!

wmMouseLeave: message wParam: wParam lParam: lParam

	self hasCursor: false.
	self isTransition: false.
	self invalidate! !
!EmulatedCheckBox categoriesFor: #defaultWindowStyle!constants!private! !
!EmulatedCheckBox categoriesFor: #draw:transition:in:on:forecolor:backcolor:focus:highlight:!displaying!public! !
!EmulatedCheckBox categoriesFor: #drawNonXP:transition:in:on:forecolor:backcolor:focus:highlight:!displaying!private! !
!EmulatedCheckBox categoriesFor: #drawOn:!displaying!public! !
!EmulatedCheckBox categoriesFor: #drawXP:transition:in:on:forecolor:backcolor:focus:highlight:!displaying!private! !
!EmulatedCheckBox categoriesFor: #hasCursor!accessing!private! !
!EmulatedCheckBox categoriesFor: #hasCursor:!accessing!private! !
!EmulatedCheckBox categoriesFor: #isChecked!accessing!private! !
!EmulatedCheckBox categoriesFor: #isChecked:!accessing!private! !
!EmulatedCheckBox categoriesFor: #isHighlighted!displaying!public! !
!EmulatedCheckBox categoriesFor: #isThemed!accessing!private! !
!EmulatedCheckBox categoriesFor: #isTransition!accessing!private! !
!EmulatedCheckBox categoriesFor: #isTransition:!accessing!private! !
!EmulatedCheckBox categoriesFor: #onKeyPressed:!event handling!public! !
!EmulatedCheckBox categoriesFor: #onKeyReleased:!event handling!public! !
!EmulatedCheckBox categoriesFor: #onKillFocus!event handling!public! !
!EmulatedCheckBox categoriesFor: #onLeftButtonDoubleClicked:!event handling!public! !
!EmulatedCheckBox categoriesFor: #onLeftButtonPressed:!event handling!public! !
!EmulatedCheckBox categoriesFor: #onLeftButtonReleased:!event handling!public! !
!EmulatedCheckBox categoriesFor: #onMouseMoved:!event handling!public! !
!EmulatedCheckBox categoriesFor: #onPaintRequired:!event handling!public! !
!EmulatedCheckBox categoriesFor: #onSetFocus!event handling!public! !
!EmulatedCheckBox categoriesFor: #refreshContents!public!updating! !
!EmulatedCheckBox categoriesFor: #updateModel!public!updating! !
!EmulatedCheckBox categoriesFor: #wmMouseLeave:wParam:lParam:!event handling!public! !

!EmulatedCheckBox class methodsFor!

defaultModel
	"Answer a default model to be assigned to the receiver when it is initialized."

	^false asValue!

initialize

	"self initialize"

	MessageMap 
		isImmutable: false;
		at: (16r02A3 + 1) put: #wmMouseLeave:wParam:lParam:;
		isImmutable: true!

uninitialize

	"self uninitialize"

	MessageMap
		at: (16r02A3 + 1) put: nil! !
!EmulatedCheckBox class categoriesFor: #defaultModel!models!public! !
!EmulatedCheckBox class categoriesFor: #initialize!models!public! !
!EmulatedCheckBox class categoriesFor: #uninitialize!models!public! !

FormattedTextEdit guid: (GUID fromString: '{DA7E43DA-5D5C-4332-A6D6-756996F820EE}')!
FormattedTextEdit comment: ''!
!FormattedTextEdit categoriesForClass!Unclassified! !
!FormattedTextEdit methodsFor!

applyFormat

	| inStream formatStream outStream text |

	inStream := ReadStream on: (self text reject: [ :char | self isSeparator: char]).
	formatStream := ReadStream on: self format.
	outStream := WriteStream on: (String new: self format size).

	[formatStream atEnd] whileFalse:
		[| char formatChar |
		formatChar := formatStream next.
		(self isSeparator: formatChar)
		ifTrue: 
			[char := formatChar]
		ifFalse:
			[inStream atEnd 
				ifTrue: [char := self placeholderChar]
				ifFalse: [char := inStream next]].
		outStream nextPut: char].

	text := outStream contents.
	self text: text.
	^text
	
!

enUpdate
	"Private - The receiver is about to display altered text.
	Note that this is fired when selecting text in the edit control, 
	as well as when keys are typed, but not when moving the caret 
	i.e. it indicates that the display has been updated."

	self isFormatting ifTrue: [^self].

	self formatDo: 
		[| pos text |
		pos := self caretPosition.
		text := self applyFormat.
		(pos < text size and: [self isSeparator: (text at: pos)]) ifTrue: [pos := pos + 1].
		self
			selectionRange: (pos to: (pos-1));
			invalidateUserInterface]!

errorTextInvalid

	self isErroring ifTrue: [^self].

	[self isErroring: true.
	MessageBox warning: 'Please enter a valid value'.
	self onModelChanged; tabFocus] ensure: [self isErroring: false]!

format
	^format!

format: anObject
	format := anObject!

formatDo: aBlock

	[self isFormatting: true.
	aBlock value] ensure: [self isFormatting: false]!

initialize

	super initialize.
	self 
		isFormatting: false;
		isErroring: false!

isErroring
	^isErroring ifNil: [false]!

isErroring: anObject
	isErroring := anObject!

isFormatting
	^isFormatting!

isFormatting: anObject
	isFormatting := anObject!

isPlaceholder: aCharacter

	^self placeholderChar == aCharacter!

isSeparator: aCharacter

	^self separatorChars identityIncludes: aCharacter!

isTextValid

	^[self typeconverter convertFromRightToLeft: self displayValue.
	true]
		on: InvalidFormat
		do: [:exception | exception return: false]!

onFullyCreated

	super onFullyCreated.
	self typeconverter rightNullValue: self format!

onKeyPressed: aKeyEvent

	| code pos text |

	code := aKeyEvent code.
	pos := self caretPosition.
	text := self text.

	(code = VK_RIGHT and: [pos < text size and: [self isSeparator: (text at: (pos + 1))]]) ifTrue:
		[self caretPosition: pos + 1].
	
	(code = VK_LEFT and: [pos >1 and: [self isSeparator: (text at: (pos - 1))]]) ifTrue:
		[self caretPosition: pos - 1].

	code = VK_DELETE ifTrue: 
		[self sendMessage: WM_CHAR wParam: (self placeholderChar codePoint).
		^false].

	^super onKeyTyped: aKeyEvent
!

onKeyTyped: aKeyEvent

	| code char pos text range |

	code := aKeyEvent code.
	char := Character value: code.
	pos := self caretPosition.
	text := self text.

	(self isSeparator: char) ifTrue: 
		[| nextPos |
		nextPos := text nextIndexOf: char from: ((pos-1) max: 1) to: text size.
		nextPos > 0 ifTrue: 
			[self caretPosition: nextPos + 1.
			^false]].

	range := self selectionRange.
	code >= 32 ifTrue: [range isEmpty ifTrue: [self selectionRange: (range start to: range start)]].

	(char = Character backspace and: [pos > 1 and: [self isSeparator: (text at: (pos-1))]]) ifTrue: 
		[self caretPosition: (range start - 1)].

	^super onKeyTyped: aKeyEvent
!

onKillFocus

	SessionManager inputState queueDeferredAction: [self invalidate].
	^super onKillFocus!

onPaintRequired: aPaintEvent

	"Only sent if we don't want to display contents - so just erase"

	self isEnabled ifFalse: [aPaintEvent canvas backcolor: Color face3d].
	aPaintEvent canvas erase; free!

onSetFocus

	self enUpdate; selectAll.
	^super onSetFocus!

placeholderChar
	^placeholderChar!

placeholderChar: anObject
	placeholderChar := anObject!

separatorChars
	^separatorChars!

separatorChars: anObject
	separatorChars := anObject!

updateModel

	| newValue |

	self displayValue = self format
	ifTrue:
		[newValue := nil]
	ifFalse:
		[newValue := [self typeconverter convertFromRightToLeft: self displayValue]
					on: InvalidFormat
					do: [:exception | exception return: #failed]].

	newValue == #failed 
		ifTrue: [self errorTextInvalid]
		ifFalse: [self model value: newValue].

	self invalidateUserInterface!

wmPaint: message wParam: wParam lParam: lParam
	"Private - Controls do their own painting so only allow the default."

	self ensureLayoutValid ifTrue: [^1].
	^(self hasFocus or: [self text ~= self typeconverter rightNullValue])
		ifTrue: [self defaultWindowProcessing: message wParam: wParam lParam: lParam]
		ifFalse: [self basicPaint: message wParam: wParam lParam: lParam]
! !
!FormattedTextEdit categoriesFor: #applyFormat!helpers!private! !
!FormattedTextEdit categoriesFor: #enUpdate!event handling-win32!private! !
!FormattedTextEdit categoriesFor: #errorTextInvalid!helpers!private! !
!FormattedTextEdit categoriesFor: #format!accessing!private! !
!FormattedTextEdit categoriesFor: #format:!accessing!private! !
!FormattedTextEdit categoriesFor: #formatDo:!helpers!private! !
!FormattedTextEdit categoriesFor: #initialize!initialize/release!public! !
!FormattedTextEdit categoriesFor: #isErroring!accessing!private! !
!FormattedTextEdit categoriesFor: #isErroring:!accessing!private! !
!FormattedTextEdit categoriesFor: #isFormatting!accessing!private! !
!FormattedTextEdit categoriesFor: #isFormatting:!accessing!private! !
!FormattedTextEdit categoriesFor: #isPlaceholder:!public!testing! !
!FormattedTextEdit categoriesFor: #isSeparator:!public!testing! !
!FormattedTextEdit categoriesFor: #isTextValid!private!testing! !
!FormattedTextEdit categoriesFor: #onFullyCreated!event handling!private! !
!FormattedTextEdit categoriesFor: #onKeyPressed:!event handling!public! !
!FormattedTextEdit categoriesFor: #onKeyTyped:!event handling!public! !
!FormattedTextEdit categoriesFor: #onKillFocus!event handling!private! !
!FormattedTextEdit categoriesFor: #onPaintRequired:!event handling!public! !
!FormattedTextEdit categoriesFor: #onSetFocus!event handling!public! !
!FormattedTextEdit categoriesFor: #placeholderChar!accessing!private! !
!FormattedTextEdit categoriesFor: #placeholderChar:!accessing!private! !
!FormattedTextEdit categoriesFor: #separatorChars!accessing!private! !
!FormattedTextEdit categoriesFor: #separatorChars:!accessing!private! !
!FormattedTextEdit categoriesFor: #updateModel!helpers!private! !
!FormattedTextEdit categoriesFor: #wmPaint:wParam:lParam:!event handling-win32!public! !

"Binary Globals"!

