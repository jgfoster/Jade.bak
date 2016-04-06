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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ContainerView) 98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 234 240 98 4 410 8 ##(Smalltalk.ScintillaView) 98 50 0 416 98 2 8 1176571972 1025 560 721990 2 ##(Smalltalk.ValueHolder) 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 196934 1 ##(Smalltalk.RGB) 27387381 0 5 265030 4 ##(Smalltalk.Menu) 0 16 98 23 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) 8 #fileSave 8 '&Save' 9383 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 786 1 818 8 #undo 8 '&Undo' 9397 1 0 0 0 786 1 818 8 #redo 8 'R&edo' 9395 1 0 0 0 882 4097 786 1 818 8 #editCut 8 'Cu&t' 9393 1 0 0 0 786 1 818 8 #editCopy 8 '&Copy' 9351 1 0 0 0 786 1 818 8 #editPaste 8 '&Paste' 9389 1 0 0 0 786 1 818 8 #editDelete 8 'De&lete' 1629 1 0 0 0 786 1 818 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 882 4097 786 1 818 8 #editFind 8 '&Find...' 9357 1 0 0 0 786 1 818 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 786 1 818 8 #editReplace 8 '&Replace...' 9361 1 0 0 0 882 4097 786 1 818 8 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 786 1 818 8 #jadeDisplay 8 '&Display' 9353 1 0 0 0 786 1 818 8 #jadeExecute 8 'Execute' 9355 1 0 0 0 882 4097 786 1 818 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 786 1 818 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 882 4097 786 1 818 8 #fileIn 8 'File In' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 560 0 8 4294903335 852486 ##(Smalltalk.NullConverter) 0 0 13 0 234 256 98 44 8 #braceMismatch 1182790 1 ##(Smalltalk.ScintillaTextStyle) 71 786694 ##(Smalltalk.IndexedColor) 33554459 0 3 0 0 0 0 2224 0 1401 0 8 #normal 2242 1 0 0 1 0 0 0 0 2304 0 0 0 8 #specialSelector 2242 33 706 16646145 0 3 0 0 0 0 2336 0 1401 0 8 #global 2242 21 0 0 3 0 0 0 0 2384 0 1401 0 8 #symbol 2242 9 2274 33554443 0 1 0 0 0 0 2416 0 0 0 8 #binary 2242 11 2274 33554433 0 1 0 0 0 0 2464 0 0 0 8 #string 2242 3 706 16646399 0 129 0 0 0 0 2512 0 0 0 8 #nil 2242 19 2368 0 3 0 0 0 0 2560 0 1401 0 8 #assignment 2242 29 0 0 3 0 0 0 0 2592 0 1401 0 8 #callTip 2242 77 2274 33554457 2274 33554471 1 0 0 0 0 2624 0 0 0 8 #self 2242 15 2368 0 3 0 0 0 0 2688 0 1401 0 8 #character 2242 31 706 16646399 0 3 0 0 0 0 2720 0 1401 0 8 #super 2242 17 2368 0 3 0 0 0 0 2768 0 1401 0 8 #keywordSend 2242 27 2274 33554437 0 3 0 0 0 0 2800 0 1401 0 8 #braceHighlight 2242 69 2274 33554465 0 3 0 0 0 0 2848 0 1401 0 8 #special 2242 25 0 0 3 0 0 0 0 2896 0 1401 0 8 #lineNumber 2242 67 0 0 1 0 0 0 0 2928 0 0 0 8 #indentGuide 2242 75 2274 33554447 0 1 0 0 0 0 2960 0 0 0 8 #boolean 2242 13 2368 0 3 0 0 0 0 3008 0 1401 0 8 #comment 2242 7 706 65025 0 1 0 0 0 0 3040 0 0 0 8 #number 2242 5 706 16711169 0 1 0 0 0 0 3088 0 0 0 8 #return 2242 23 706 321 0 3 0 0 0 0 3136 0 1401 0 98 40 2320 2528 3104 3056 2432 2480 3024 2704 2784 2576 2400 3152 2912 2816 2608 2736 2352 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2944 2864 2256 0 2976 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler) 2304 234 256 98 16 8 #folderOpen 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 63 13 0 0 560 3264 0 0 0 0 0 0 8 #folderOpenMid 3282 53 11 33554471 33554471 560 3312 0 0 0 0 0 0 8 #folderTail 3282 57 11 0 0 560 3344 0 0 0 0 0 0 8 #circle 3282 1 0 0 0 560 3376 0 0 0 0 0 0 8 #folderEnd 3282 51 11 33554471 33554471 560 3408 0 0 0 0 0 0 8 #folderSub 3282 59 11 0 0 560 3440 0 0 0 0 0 0 8 #folder 3282 61 5 0 0 560 3472 0 0 0 0 0 0 8 #folderMidTail 3282 55 11 33554471 33554471 560 3504 0 0 0 0 0 0 202 208 98 0 0 63 9215 0 0 0 0 2992 0 0 65 0 0 0 8 '' 7 234 256 98 2 8 #container 234 256 98 2 2304 2242 1 0 0 1 0 0 0 0 2304 0 0 0 0 0 8 #arrows 0 1 0 234 256 98 12 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 560 65025 3 32 1 0 0 0 0 0 0 3 3730 3 560 33423361 5 32 3 0 0 0 0 0 0 5 3730 5 560 511 1 32 5 0 0 0 0 0 0 8 'indicator8' 3730 17 560 33554447 1 32 0 0 0 0 0 0 0 8 'indicator9' 3730 19 560 33554459 13 32 0 0 0 0 0 0 0 8 'indicator10' 3730 21 560 511 3 32 0 0 0 0 0 0 0 0 0 234 256 98 6 8 #Notification 2242 1029 0 706 31185883 1 0 0 0 0 3920 0 0 0 8 #Error 2242 1031 706 335 706 31317503 1 0 0 0 0 3968 0 0 0 8 #Warning 2242 1027 706 60239 706 31326207 1 0 0 0 0 4032 0 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 15 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 2114 1 51 2114 1001 551 560 4162 8 #contextMenu: 98 1 752 560 4162 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval) 3 1 3 560 4162 8 #isTextModified: 98 1 32 560 4162 8 #modificationEventMask: 98 1 9215 560 4162 8 #hoverTime: 98 1 401 560 4162 8 #secondaryCaretForecolor: 98 1 706 16711423 560 4162 8 #caretPeriod: 98 1 1061 560 4162 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin) 1 560 61 3 32 1 15 4706 3 560 1 1 16 67108863 15 4706 5 560 1 1 16 -67108863 15 560 4162 8 #sciSetIndentationGuides: 98 1 3 560 4162 8 #tabIndents: 98 1 16 560 4162 8 #tabWidth: 98 1 9 560 4162 8 #setLexerLanguage: 98 1 8 #smalltalk 560 4162 8 #positionCacheSize: 98 1 1 560 4162 8 #areAdditionalCaretsVisible: 98 1 16 560 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 244 1 0 0 44 1 0 0] 98 0 2114 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) 8 #fixedParentLeft 1 5186 8 #fixedParentRight 1 5186 8 #fixedParentTop 51 5186 8 #fixedParentBottom 1 410 8 ##(Smalltalk.Toolbar) 98 25 0 416 98 2 8 1140851500 131137 5328 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 517 0 2034 0 16 2066 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2114 193 193 0 5328 5410 5440 8 4294905905 234 256 3552 234 256 98 24 24971 853766 ##(Smalltalk.ToolbarButton) 24971 0 5328 1 818 1696 8 'Print Result of Selection or Line' 1 1 0 395334 3 ##(Smalltalk.Bitmap) 0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 0 7 2114 1857 33 55 24973 5602 24973 0 5328 1 818 1760 8 'Evaluate Selection or Line' 1 1 0 5680 57 24975 5602 24975 0 5328 1 818 1632 8 'Inspect Selection or Line' 1 1 0 5680 59 24981 1246982 ##(Smalltalk.ToolbarSystemButton) 24981 0 5328 1 818 848 8 'Save' 1 1 0 1 17 24983 5906 24983 0 5328 1 818 1088 8 'Cut' 1 1 0 1 1 24985 5906 24985 0 5328 1 818 1152 8 'Copy' 1 1 0 1 3 24987 5906 24987 0 5328 1 818 1216 8 'Paste' 1 1 0 1 5 24989 5906 24989 0 5328 1 818 1280 8 'Delete' 1 1 0 1 11 24991 5906 24991 0 5328 1 818 944 8 'Undo' 1 1 0 1 7 24993 5906 24993 0 5328 1 818 1008 8 'Redo' 1 1 0 1 9 24995 5906 24995 0 5328 1 818 1424 8 'Find' 1 1 0 1 25 24997 5906 24997 0 5328 1 818 1552 8 'Replace' 1 1 0 1 27 98 15 5616 5808 5856 1050118 ##(Smalltalk.ToolbarSeparator) 0 0 5328 3 0 1 5920 6370 0 0 5328 3 0 1 5968 6016 6064 6112 6160 6208 6370 0 0 5328 3 0 1 6256 6304 234 240 98 4 1 117 5680 1 0 1 0 2114 33 33 2114 45 45 0 0 4098 202 208 98 2 4162 4192 98 2 2114 1 1 2114 1001 51 5328 4162 8 #updateSizePosted 3552 5328 5074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 244 1 0 0 25 0 0 0] 98 0 5136 0 27 5154 5200 1 5232 1 5264 1 5186 8 #fixedViewTop 51 234 256 98 2 560 8 'document' 0 4098 202 208 98 1 4162 4192 98 2 2114 5119 21 2114 1001 601 416 5074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 243 11 0 0 54 1 0 0] 98 2 5328 560 5136 0 27 )! !
!CodeSourcePresenter class categoriesFor: #codeFont!public! !
!CodeSourcePresenter class categoriesFor: #codeFont:!public! !
!CodeSourcePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

