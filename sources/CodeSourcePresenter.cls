"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #CodeSourcePresenter
	instanceVariableNames: 'documentPresenter menuTitle'
	classVariableNames: 'CodeFont'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CodeSourcePresenter guid: (GUID fromString: '{549A5009-CDD1-42B3-8907-C3C5C1C9E532}')!
CodeSourcePresenter comment: ''!
!CodeSourcePresenter categoriesForClass!Unclassified! !
!CodeSourcePresenter methodsFor!

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
	"I'm not sure how it works, but this method isn't called!! 
	Somehow, the command is sent directly to the text widget."

self error: 'Do we ever see this?'.
	"self activeTextEdit editFind."
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
	selectionRange stop < selectionRange start 						ifFalse: [^self ].
	selectionRange start < documentPresenter view value size 	ifFalse: [^self ].
	(text at: selectionRange start) = Character cr 						ifFalse: [^self ].
	(text at: selectionRange start + 1) = Character lf 				ifFalse: [^self ].
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
	(self gciSession isOopType: anObject) ifFalse: [
		result := ' ' , anObject printString.
	] ifTrue: [
		result := ' ' , (self gciSession printString: anObject).
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.ScintillaView)  98 46 0 416 98 2 8 1176571972 1025 560 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 196934 1 ##(Smalltalk.RGB)  27387381 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 23 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #fileSave 8 '&Save' 9383 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 786 1 818 8 #undo 8 '&Undo' 9397 1 0 0 0 786 1 818 8 #redo 8 'R&edo' 9395 1 0 0 0 882 4097 786 1 818 8 #editCut 8 'Cu&t' 9393 1 0 0 0 786 1 818 8 #editCopy 8 '&Copy' 9351 1 0 0 0 786 1 818 8 #editPaste 8 '&Paste' 9389 1 0 0 0 786 1 818 8 #editDelete 8 'De&lete' 1629 1 0 0 0 786 1 818 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 882 4097 786 1 818 8 #editFind 8 '&Find...' 9357 1 0 0 0 786 1 818 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 786 1 818 8 #editReplace 8 '&Replace...' 9361 1 0 0 0 882 4097 786 1 818 8 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 786 1 818 8 #jadeDisplay 8 '&Display' 9353 1 0 0 0 786 1 818 8 #jadeExecute 8 'Execute' 9355 1 0 0 0 882 4097 786 1 818 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 786 1 818 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 882 4097 786 1 818 8 #fileIn 8 'File In' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point)  193 193 0 560 0 8 4294902863 852486 ##(Smalltalk.NullConverter)  0 0 13 0 234 256 98 42 8 #lineNumber 1182726 ##(Smalltalk.ScintillaTextStyle)  67 0 0 1 0 0 0 0 2224 0 0 0 8 #specialSelector 2242 33 706 16646145 0 3 0 0 0 0 2272 0 0 0 8 #global 2242 21 0 0 3 0 0 0 0 2320 0 0 0 8 #normal 2242 1 0 0 1 0 0 0 0 2352 0 0 0 8 #boolean 2242 13 2304 0 3 0 0 0 0 2384 0 0 0 8 #special 2242 25 0 0 3 0 0 0 0 2416 0 0 0 8 #number 2242 5 706 16711169 0 1 0 0 0 0 2448 0 0 0 8 #nil 2242 19 2304 0 3 0 0 0 0 2496 0 0 0 8 #character 2242 31 706 16646399 0 3 0 0 0 0 2528 0 0 0 8 #braceHighlight 2242 69 786694 ##(Smalltalk.IndexedColor)  33554465 0 3 0 0 0 0 2576 0 0 0 8 #indentGuide 2242 75 2610 33554447 0 1 0 0 0 0 2640 0 0 0 8 #string 2242 3 706 16646399 0 129 0 0 0 0 2688 0 0 0 8 #symbol 2242 9 2610 33554443 0 1 0 0 0 0 2736 0 0 0 8 #super 2242 17 2304 0 3 0 0 0 0 2784 0 0 0 8 #comment 2242 7 706 65025 0 1 0 0 0 0 2816 0 0 0 8 #binary 2242 11 2610 33554433 0 1 0 0 0 0 2864 0 0 0 8 #assignment 2242 29 0 0 3 0 0 0 0 2912 0 0 0 8 #keywordSend 2242 27 2610 33554437 0 3 0 0 0 0 2944 0 0 0 8 #return 2242 23 706 321 0 3 0 0 0 0 2992 0 0 0 8 #braceMismatch 2242 71 2610 33554459 0 3 0 0 0 0 3040 0 0 0 8 #self 2242 15 2304 0 3 0 0 0 0 3088 0 0 0 98 40 2368 2704 2464 2832 2752 2880 2400 3104 2800 2512 2336 3008 2432 2960 2928 2544 2288 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2256 2592 3056 0 2656 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  2352 234 256 98 16 8 #folderTail 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  57 11 2896 2896 560 3200 8 #folderSub 3218 59 11 2896 2896 560 3248 8 #folderOpenMid 3218 53 11 2610 33554471 2896 560 3280 8 #folderMidTail 3218 55 11 3312 2896 560 3328 8 #folder 3218 61 5 2896 2896 560 3360 8 #folderOpen 3218 63 13 2896 2896 560 3392 8 #circle 3218 1 1 2896 3312 560 3424 8 #folderEnd 3218 51 11 3312 2896 560 3456 202 208 98 0 0 63 9215 0 0 0 0 2672 0 0 0 0 0 0 8 '' 7 234 256 98 2 8 #container 234 256 98 2 2352 2242 1 0 0 1 0 0 0 0 2352 0 0 0 0 0 8 #arrows 0 1 0 234 256 98 12 1 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  1 560 65025 3 32 1 0 3 3682 3 560 33423361 5 32 3 0 5 3682 5 560 511 1 32 5 0 8 'indicator8' 3682 17 560 33554447 1 32 0 0 8 'indicator9' 3682 19 560 33554459 13 32 0 0 8 'indicator10' 3682 21 560 511 3 32 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 12 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 2114 1 51 2114 1001 551 560 3906 8 #contextMenu: 98 1 752 560 3906 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 560 3906 8 #isTextModified: 98 1 32 560 3906 8 #modificationEventMask: 98 1 9215 560 3906 8 #hoverTime: 98 1 401 560 3906 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 560 61 3 32 1 4338 3 560 1 1 16 67108863 4338 5 560 1 1 16 -67108863 560 3906 8 #indentationGuides: 98 1 8 #real 560 3906 8 #tabIndents: 98 1 16 560 3906 8 #tabWidth: 98 1 9 560 3906 8 #setLexerLanguage: 98 1 8 #smalltalk 560 3906 8 #positionCacheSize: 98 1 1 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 244 1 0 0 44 1 0 0] 98 0 2114 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 4786 8 #fixedParentRight 1 4786 8 #fixedParentTop 51 4786 8 #fixedParentBottom 1 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1140851500 131137 4928 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 517 0 2034 0 16 2066 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2114 193 193 0 4928 5010 5040 8 4294903291 234 256 3504 234 256 98 38 31205 853766 ##(Smalltalk.ToolbarButton)  31205 0 4928 1 818 1760 8 'Evaluate Selection or Line' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 2114 1857 33 57 31207 5202 31207 0 4928 1 818 1632 8 'Inspect Selection or Line' 1 1 0 5280 59 31209 1246982 ##(Smalltalk.ToolbarSystemButton)  31209 0 4928 1 818 8 #fileNew 8 'New Workspace' 1 1 0 1 13 31211 5458 31211 0 4928 1 818 8 #fileOpen 8 'Open Workspace' 1 1 0 1 15 31213 5458 31213 0 4928 1 818 848 8 'Save' 1 1 0 1 17 31215 5458 31215 0 4928 1 818 1088 8 'Cut' 1 1 0 1 1 31217 5458 31217 0 4928 1 818 1152 8 'Copy' 1 1 0 1 3 31219 5458 31219 0 4928 1 818 1216 8 'Paste' 1 1 0 1 5 31221 5458 31221 0 4928 1 818 1280 8 'Delete' 1 1 0 1 11 31223 5458 31223 0 4928 1 818 944 8 'Undo' 1 1 0 1 7 31225 5458 31225 0 4928 1 818 1008 8 'Redo' 1 1 0 1 9 31227 5458 31227 0 4928 1 818 1424 8 'Find' 1 1 0 1 25 31229 5458 31229 0 4928 1 818 1552 8 'Replace' 1 1 0 1 27 31193 5202 31193 0 4928 1 818 8 #abortTransaction 8 'Abort Transaction' 1 1 0 5280 1 31195 5202 31195 0 4928 1 818 8 #commitTransaction 8 'Commit Transaction' 1 1 0 5280 27 31197 5202 31197 0 4928 1 818 8 #jadeBrowseUsers 8 'Browse Users' 1 1 0 5280 75 31199 5202 31199 0 4928 1 818 8 #jadeBrowseClasses 8 'Open System Browser' 1 1 0 5280 17 31201 5202 31201 0 4928 1 818 8 #jadeBrowseMonticello 8 'Open Monticello Browser' 1 1 0 5280 3 31203 5202 31203 0 4928 1 818 1696 8 'Print Result of Selection or Line' 1 1 0 5280 55 98 24 6032 6096 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 4928 3 0 1 6160 6224 6288 6418 0 0 4928 3 0 1 6352 5216 5408 6418 0 0 4928 3 0 1 5472 5536 5600 6418 0 0 4928 3 0 1 5648 5696 5744 5792 5840 5888 6418 0 0 4928 3 0 1 5936 5984 234 240 98 4 1 117 5280 1 0 1 0 2114 33 33 2114 45 45 0 0 3842 202 208 98 2 3906 3936 98 2 2114 1 1 2114 1001 51 4928 3906 8 #updateSize 3504 4928 4674 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 244 1 0 0 25 0 0 0] 98 0 4736 0 27 4754 4800 1 4832 1 4864 1 4786 8 #fixedViewTop 51 234 256 98 2 560 8 'document' 0 3842 202 208 98 1 3906 3936 98 2 2114 2879 21 2114 1001 601 416 4674 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 147 7 0 0 54 1 0 0] 98 2 4928 560 4736 0 27 )! !
!CodeSourcePresenter class categoriesFor: #codeFont!public! !
!CodeSourcePresenter class categoriesFor: #codeFont:!public! !
!CodeSourcePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

