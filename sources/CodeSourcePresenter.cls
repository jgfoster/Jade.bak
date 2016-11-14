"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #CodeSourcePresenter
	instanceVariableNames: 'documentPresenter menuTitle'
	classVariableNames: 'CodeFont'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CodeSourcePresenter guid: (GUID fromString: '{549A5009-CDD1-42B3-8907-C3C5C1C9E532}')!
CodeSourcePresenter comment: 'model - the context in which code can be executed'!
!CodeSourcePresenter categoriesForClass!Unclassified! !
!CodeSourcePresenter methodsFor!

abortTransaction

	gciSession abort.
	Sound informationBeep.
!

addMenu

	| menuBar |
	menuBar := self view topShell view menuBar.
	self updateMenuBar: menuBar.
	self view topShell view menuBar: menuBar.
!

addMenuTo: aMenuBar

	self updateMenuBar: aMenuBar.
!

addQuotesToSelection

	documentPresenter view replaceSelection: documentPresenter selection printString.
!

browseImplementors

	self browseImplementorsOf: self currentSelector.!

browseSenders

	self browseSendersOf: self currentSelector.!

clearBreakAtStepPoint: anInteger

	| method |
	(method := self trigger: #'needMethod') isNil ifTrue: [^self].
	method clearBreakAtStepPoint: anInteger.
!

codeFont: aFont

	documentPresenter view font: aFont.
!

codePresenterIsMethod

	^false!

currentSelectionOrLine

	| range |
	documentPresenter hasSelection ifFalse: [documentPresenter view selectCurrentLine].
	range := documentPresenter view selectionRange.
	^(documentPresenter value copyFrom: range start to: range stop) replaceCrLfWithLf.
!

currentSelector

	| selection |
	selection := documentPresenter view selection.
	^[
		(SmalltalkParser parseMethod: selection) selector.
	] on: Error do: [:ex | 
		selection.
	]
!

documentPresenter

	^documentPresenter!

editCopy

	documentPresenter view
		copySelection;
		updateModel;
		yourself.
!

editCut

	documentPresenter view
		cutSelection;
		updateModel;
		yourself.
!

editDelete

	self selectLfIfEndingOnCr.
	documentPresenter view
		clearSelection;
		updateModel;
		yourself.
!

editFind

	documentPresenter view editFind.
	self showSelection.
!

editFindNext

	documentPresenter view findNext.
	self showSelection.

!

editPaste

	documentPresenter view
		pasteClipboard;
		updateModel;
		yourself.
!

editReplace

	documentPresenter view
		findReplace;
		updateModel;
		yourself.
!

editSelectAll

	documentPresenter view selectAll.
!

executeSelectionOrLine

	[
		^true -> (gciSession executeString: self currentSelectionOrLine fromContext: model).
	] on: GsCompileError do: [:ex | 
		^false -> ex list.
	].
	self error: 'How did we get here?'.
	^false -> #(nil).
!

fileSave
		"Private - Answer whether the save succeeded (false means to stay on the window and cancel any attempt to leave)"

	^parentPresenter fileSave!

jadeDisplay

	self jadeExecuteAndDisplay: true.
!

jadeExecute

	self jadeExecuteAndDisplay: false.
!

jadeExecuteAndDisplay: showResult 

	| result |
	result := self executeSelectionOrLine.
	result key ifTrue: [
		showResult ifTrue: [
			self showResult: result value.
		] ifFalse: [
			self setCaretToEndOfSelection.
		].
		^result value.
	] ifFalse: [
		self showCompileError: result value first.
	].
!

menuTitle: aString

	menuTitle := aString.
!

mySave
!

queryCommand: query

	(#(#'fileSave') includes: query commandSymbol) ifTrue: [
		query isEnabled: documentPresenter isModified.
		^true.
	].
	(#(#editCut #editCopy) includes: query commandSymbol) ifTrue: [
		query isEnabled: documentPresenter hasSelection.
		^true.
	].
	(query commandSymbol = #editPaste) ifTrue: [
		query isEnabled: documentPresenter view canPaste.
		^true.
	].
	^super queryCommand: query.
!

removeMenu

	| menuBar item |
	(menuBar := self view topShell view menuBar) isNil ifTrue: [^self].
	item := menuBar items 
		detect: [:each | each text = self subMenuName]
		ifNone: [^self].
	menuBar removeItem: item.
	self view topShell view menuBar: menuBar.
!

removeQuotesFromSelection

	| string |
	string := documentPresenter view selection trimBlanks.
	(string size >= 2 and: [string first = $' and: [string last = $']]) ifFalse: [
		^MessageBox notify: 'Selection must begin and end with quote'.
	].
	string := string copyFrom: 2 to: string size - 1.
	string := string 
		copyReplaceAll: ''''''
		with: ''''.
	documentPresenter view replaceSelection: string.
!

selectionChanging: aSelectionChangingEvent 

	(documentPresenter view isKindOf: DeafObject) ifTrue: [^self].
	documentPresenter view isModified ifFalse: [^self].
	documentPresenter view ensureVisible.
	MessageBox 
		confirm: 'Save changes?' 
		onYes: 			[aSelectionChangingEvent value: self fileSave] 
		onNo: 			[documentPresenter view isModified: false] 
		onCancel: 	[aSelectionChangingEvent value: false].
!

selectLfIfEndingOnCr
	"deleting a CR without the subsequent LF can leave things somewhat confused"

	| text selectionRange |
	selectionRange := documentPresenter view selectionRange.
	text := documentPresenter view "hide; show;" value.			"somehow the value gets out of synch"
	selectionRange stop < selectionRange start 			ifFalse: [^self ].
	selectionRange start < documentPresenter view value size 	ifFalse: [^self ].
	(text at: selectionRange start) = Character cr 			ifFalse: [^self ].
	(text at: selectionRange start + 1) = Character lf 		ifFalse: [^self ].
	documentPresenter view selectionRange: (selectionRange start to: selectionRange start + 1).
!

setBreakAtStepPoint: anInteger

	| method |
	(method := self trigger: #'needMethod') isNil ifTrue: [^self].
	method setBreakAtStepPoint: anInteger.
	self trigger: #'breaksChanged'.
!

setCaretToEndOfSelection

	| textView |
	textView := documentPresenter view.
	textView caretPosition: textView selectionRange stop + 1.
!

setDocumentPresenterWith: aJadeGsClassShape

	documentPresenter lastGsShape: aJadeGsClassShape!

setFont

	| font |
	font := CodeFont notNil
		ifTrue: [FontDialog showModalOn: CodeFont]
		ifFalse: [FontDialog showModal].
	font notNil ifTrue: [
		self class codeFont: font.
	]
!

showCompileError: anArray

	| result string count textView selectionRange offset |
	textView := documentPresenter view.
	offset := anArray at: 2.
	result := anArray at: 3.
	selectionRange := textView selectionRange.
	string := textView value.
	string := string copyFrom: selectionRange start to: selectionRange stop.
	string := string replaceCrLfWithLf copyFrom: 1 to: offset - 1.
	count := (string select: [:each | each = Character lf]) size.
	offset := offset + count.
	textView
		caretPosition: selectionRange start + offset - 1;
		replaceSelection: result;
		selectionStart: textView caretPosition - result size length: result size.
!

showResult: anObject

	| result textView |
	(gciSession isOopType: anObject) ifFalse: [
		result := ' ' , anObject printString.
	] ifTrue: [
		result := ' ' , (gciSession printString: anObject).
	].
	"result := result replaceLfWithCrLf."
	self setCaretToEndOfSelection.
	(textView := documentPresenter view)
		replaceSelection: result;
		selectionStart: textView caretPosition - result size length: result size.
!

showSelection

	| range lineNumber |
	(range := documentPresenter view selectionRange) isEmpty ifTrue: [^self].
	lineNumber := documentPresenter view lineFromPosition: range first.
	lineNumber := lineNumber - 4 max: 1.
	documentPresenter view lineScroll: lineNumber.
!

subMenuName

	^menuTitle.
!

subMenuPresenter

	^documentPresenter.
!

update

	self subclassResponsibility.
!

updateCodeFont

	CodeFont notNil ifTrue: [
		documentPresenter view font: CodeFont.
	].
! !
!CodeSourcePresenter categoriesFor: #abortTransaction!Jade!public! !
!CodeSourcePresenter categoriesFor: #addMenu!menus!public! !
!CodeSourcePresenter categoriesFor: #addMenuTo:!menus!public! !
!CodeSourcePresenter categoriesFor: #addQuotesToSelection!edit!private! !
!CodeSourcePresenter categoriesFor: #browseImplementors!public! !
!CodeSourcePresenter categoriesFor: #browseSenders!public! !
!CodeSourcePresenter categoriesFor: #clearBreakAtStepPoint:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #codeFont:!public! !
!CodeSourcePresenter categoriesFor: #codePresenterIsMethod!public! !
!CodeSourcePresenter categoriesFor: #currentSelectionOrLine!Jade!private! !
!CodeSourcePresenter categoriesFor: #currentSelector!public! !
!CodeSourcePresenter categoriesFor: #documentPresenter!public! !
!CodeSourcePresenter categoriesFor: #editCopy!edit!private! !
!CodeSourcePresenter categoriesFor: #editCut!edit!private! !
!CodeSourcePresenter categoriesFor: #editDelete!edit!private! !
!CodeSourcePresenter categoriesFor: #editFind!edit!private! !
!CodeSourcePresenter categoriesFor: #editFindNext!edit!private! !
!CodeSourcePresenter categoriesFor: #editPaste!edit!private! !
!CodeSourcePresenter categoriesFor: #editReplace!edit!private! !
!CodeSourcePresenter categoriesFor: #editSelectAll!edit!private! !
!CodeSourcePresenter categoriesFor: #executeSelectionOrLine!Jade!private! !
!CodeSourcePresenter categoriesFor: #fileSave!private! !
!CodeSourcePresenter categoriesFor: #jadeDisplay!Jade!private! !
!CodeSourcePresenter categoriesFor: #jadeExecute!Jade!private! !
!CodeSourcePresenter categoriesFor: #jadeExecuteAndDisplay:!Jade!private! !
!CodeSourcePresenter categoriesFor: #menuTitle:!menus!public! !
!CodeSourcePresenter categoriesFor: #mySave!private! !
!CodeSourcePresenter categoriesFor: #queryCommand:!public! !
!CodeSourcePresenter categoriesFor: #removeMenu!menus!public! !
!CodeSourcePresenter categoriesFor: #removeQuotesFromSelection!edit!private! !
!CodeSourcePresenter categoriesFor: #selectionChanging:!public! !
!CodeSourcePresenter categoriesFor: #selectLfIfEndingOnCr!edit!private! !
!CodeSourcePresenter categoriesFor: #setBreakAtStepPoint:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #setCaretToEndOfSelection!Jade!private! !
!CodeSourcePresenter categoriesFor: #setDocumentPresenterWith:!public! !
!CodeSourcePresenter categoriesFor: #setFont!private! !
!CodeSourcePresenter categoriesFor: #showCompileError:!Jade!private! !
!CodeSourcePresenter categoriesFor: #showResult:!Jade!private! !
!CodeSourcePresenter categoriesFor: #showSelection!edit!private! !
!CodeSourcePresenter categoriesFor: #subMenuName!menus!public! !
!CodeSourcePresenter categoriesFor: #subMenuPresenter!menus!public! !
!CodeSourcePresenter categoriesFor: #update!public! !
!CodeSourcePresenter categoriesFor: #updateCodeFont!public! !

!CodeSourcePresenter class methodsFor!

codeFont

	^CodeFont.
!

codeFont: aFont

	CodeFont := aFont.
	self withAllSubclassesDo: [:eachClass | 
		eachClass allInstances do: [:each | 
			each codeFont: aFont.
		].
	].
	JadeTextDocument withAllSubclassesDo: [:eachClass | 
		eachClass allInstances do: [:each | 
			each updateCodeFont.
		].
	].
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ContainerView) 98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 234 240 98 4 410 8 ##(Smalltalk.ScintillaView) 98 50 0 416 98 2 8 1445007428 1025 560 721990 2 ##(Smalltalk.ValueHolder) 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 5 0 0 0 560 0 8 4294902941 852486 ##(Smalltalk.NullConverter) 0 0 9 0 234 256 98 4 8 #callTip 1182790 1 ##(Smalltalk.ScintillaTextStyle) 77 786694 ##(Smalltalk.IndexedColor) 33554457 882 33554471 1 0 0 0 0 832 0 0 0 8 #normal 850 1 0 0 1 0 0 0 0 928 0 0 0 98 256 944 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 864 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 0 0 32 202 208 98 0 234 256 98 2 8 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 0 0 0 560 8 #circle 0 0 0 0 0 0 202 8 ##(Smalltalk.IdentitySet) 1024 0 234 240 1024 9215 0 0 0 0 882 33554447 0 0 65 0 0 234 256 98 6 8 #literalBytes 8 '[]' 8 #literalArray 8 '()' 8 #specialCharacter 8 '()[]<>' 8 '' 3 234 256 98 2 8 #container 234 256 98 4 832 864 928 944 0 0 0 0 1 0 234 256 98 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 560 65025 3 32 1 0 0 0 0 0 0 3 1458 3 560 33423361 5 32 3 0 0 0 0 0 0 5 1458 5 560 511 1 32 5 0 0 0 0 0 0 0 0 234 256 98 6 8 #Notification 850 1029 0 196934 1 ##(Smalltalk.RGB) 31185883 1 0 0 0 0 1552 0 0 0 8 #Error 850 1031 1586 335 1586 31317503 1 0 0 0 0 1616 0 0 0 8 #Warning 850 1027 1586 60239 1586 31326207 1 0 0 0 0 1680 0 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 5 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point) 1 51 1874 1001 551 560 1810 8 #modificationEventMask: 98 1 9215 560 1810 8 #caretPeriod: 98 1 1061 560 1810 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin) 1 560 1 3 32 1 15 2082 3 560 33 1 16 67108863 15 2082 5 560 1 1 16 -67108863 15 560 1810 8 #tabWidth: 98 1 9 560 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 244 1 0 0 44 1 0 0] 98 0 1874 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) 8 #fixedParentLeft 1 2306 8 #fixedParentRight 1 2306 8 #fixedPreviousBottom 1 2306 8 #fixedParentBottom 1 410 8 ##(Smalltalk.Toolbar) 98 25 0 416 98 2 8 1140851500 131137 2448 0 706 8 4278190080 0 517 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1874 193 193 0 2448 706 2544 8 4294902931 234 256 1024 234 256 98 24 13249 1246982 ##(Smalltalk.ToolbarSystemButton) 13249 0 2448 1 1180998 4 ##(Smalltalk.CommandDescription) 8 #fileSave 8 'Save' 1 1 0 1 17 13251 2738 13251 0 2448 1 2770 8 #editCut 8 'Cut' 1 1 0 1 1 13253 2738 13253 0 2448 1 2770 8 #editCopy 8 'Copy' 1 1 0 1 3 13255 2738 13255 0 2448 1 2770 8 #editPaste 8 'Paste' 1 1 0 1 5 13257 2738 13257 0 2448 1 2770 8 #editDelete 8 'Delete' 1 1 0 1 11 13259 2738 13259 0 2448 1 2770 8 #undo 8 'Undo' 1 1 0 1 7 13261 2738 13261 0 2448 1 2770 8 #redo 8 'Redo' 1 1 0 1 9 13263 2738 13263 0 2448 1 2770 8 #editFind 8 'Find' 1 1 0 1 25 13265 2738 13265 0 2448 1 2770 8 #editReplace 8 'Replace' 1 1 0 1 27 13243 853766 ##(Smalltalk.ToolbarButton) 13243 0 2448 1 2770 8 #jadeDisplay 8 'Print Result of Selection or Line' 1 1 0 395334 3 ##(Smalltalk.Bitmap) 0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 0 7 1874 1857 33 55 13245 3346 13245 0 2448 1 2770 8 #jadeExecute 8 'Evaluate Selection or Line' 1 1 0 3440 57 13247 3346 13247 0 2448 1 2770 8 #jadeInspect 8 'Inspect Selection or Line' 1 1 0 3440 59 98 15 3360 3568 3632 1050118 ##(Smalltalk.ToolbarSeparator) 0 0 2448 3 0 1 2752 3714 0 0 2448 3 0 1 2832 2896 2960 3024 3088 3152 3714 0 0 2448 3 0 1 3216 3280 234 240 98 4 1 117 3440 1 0 1 0 1874 33 33 1874 45 45 0 0 1746 202 208 98 2 1810 1840 98 2 1874 1 1 1874 1001 51 2448 1810 8 #updateSizePosted 1024 2448 2194 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 244 1 0 0 25 0 0 0] 98 0 2256 0 27 2274 2320 1 2352 1 2306 8 #fixedParentTop 1 2306 8 #fixedViewTop 51 234 256 98 2 560 8 'document' 0 1746 202 208 98 1 1810 1840 98 2 1874 2559 21 1874 1001 601 416 2194 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 243 6 0 0 54 1 0 0] 98 2 2448 560 2256 0 27 )! !
!CodeSourcePresenter class categoriesFor: #codeFont!public! !
!CodeSourcePresenter class categoriesFor: #codeFont:!public! !
!CodeSourcePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

