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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 0 0 34 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.Toolbar) 34 25 0 416 34 2 8 1140851500 131137 544 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 517 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 544 610 640 8 4294903021 170 192 8 #() 170 192 34 24 55707 1246982 ##(Smalltalk.ToolbarSystemButton) 55707 0 544 1 1180998 4 ##(Smalltalk.CommandDescription) #undo 8 'Undo' 1 1 0 1 7 55709 866 55709 0 544 1 898 #redo 8 'Redo' 1 1 0 1 9 55711 866 55711 0 544 1 898 #editFind 8 'Find' 1 1 0 1 25 55713 866 55713 0 544 1 898 #editReplace 8 'Replace' 1 1 0 1 27 55691 853766 ##(Smalltalk.ToolbarButton) 55691 0 544 1 898 #jadeDisplay 8 'Print Result of Selection or Line' 1 1 0 395334 3 ##(Smalltalk.Bitmap) 0 true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 0 7 738 1857 33 55 55693 1090 55693 0 544 1 898 #jadeExecute 8 'Evaluate Selection or Line' 1 1 0 1168 57 55695 1090 55695 0 544 1 898 #jadeInspect 8 'Inspect Selection or Line' 1 1 0 1168 59 55697 866 55697 0 544 1 898 #fileSave 8 'Save' 1 1 0 1 17 55699 866 55699 0 544 1 898 #editCut 8 'Cut' 1 1 0 1 1 55701 866 55701 0 544 1 898 #editCopy 8 'Copy' 1 1 0 1 3 55703 866 55703 0 544 1 898 #editPaste 8 'Paste' 1 1 0 1 5 55705 866 55705 0 544 1 898 #editDelete 8 'Delete' 1 1 0 1 11 34 15 1104 1296 1344 1050118 ##(Smalltalk.ToolbarSeparator) 0 0 544 3 0 1 1392 1650 0 0 544 3 0 1 1440 1488 1536 1584 880 944 1650 0 0 544 3 0 1 992 1040 170 176 34 4 1 117 1168 1 0 1 0 738 33 33 738 45 45 0 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 738 1 1 738 1001 51 544 1842 #updateSizePosted 816 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 244 1 0 0 25 0 0 0] 8 #() 738 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2050 #fixedParentRight 1 2050 #fixedParentTop 1 2050 #fixedViewTop 51 410 ##(Smalltalk.ScintillaView) 34 50 0 416 34 2 8 1445007428 1025 2128 721990 2 ##(Smalltalk.ValueHolder) 0 false 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 610 8 4278190080 0 5 0 0 0 2128 0 8 4294902843 852486 ##(Smalltalk.NullConverter) 0 0 9 0 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 0 0 1 0 0 0 0 #normal 0 0 0 #callTip 2370 77 786694 ##(Smalltalk.IndexedColor) 33554457 2418 33554471 1 0 0 0 0 #callTip 0 0 0 34 256 2384 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2400 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 0 0 false 138 144 816 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 0 0 0 2128 #circle 0 0 0 0 0 0 138 ##(Smalltalk.IdentitySet) 816 0 170 176 816 9215 0 0 0 0 2418 33554447 0 0 65 0 0 170 192 34 6 #literalBytes 8 '[]' #literalArray 8 '()' #specialCharacter 8 '()[]<>' 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 2384 #callTip 2400 0 0 0 0 1 0 170 192 34 12 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 2128 65025 3 false 1 0 0 0 0 0 0 3 2834 3 2128 33423361 5 false 3 0 0 0 0 0 0 5 2834 5 2128 511 1 false 5 0 0 0 0 0 0 8 'indicator9' 2834 19 2128 33554459 13 0 0 0 0 0 0 0 0 8 'indicator10' 2834 21 2128 33554459 3 0 0 0 0 0 0 0 0 8 'indicator8' 2834 17 2128 33554447 1 0 0 0 0 0 0 0 0 0 0 170 192 34 6 #Notification 2370 1029 0 196934 1 ##(Smalltalk.RGB) 31185883 1 0 0 0 0 #Notification 0 0 0 #Error 2370 1031 3042 335 3042 31317503 1 0 0 0 0 #Error 0 0 0 #Warning 2370 1027 3042 60239 3042 31326207 1 0 0 0 0 #Warning 0 0 0 0 1778 138 144 34 6 1842 #createAt:extent: 34 2 738 1 51 738 1001 551 2128 1842 #modificationEventMask: 8 #(4607) 2128 1842 #hoverTime: 8 #(200) 2128 1842 #caretPeriod: 8 #(530) 2128 1842 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 2128 61 3 0 0 0 0 3426 3 2128 1 0 3 67108863 0 0 3426 5 2128 1 0 3 -67108863 0 0 2128 1842 #tabWidth: 8 #(4) 2128 1938 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 244 1 0 0 44 1 0 0] 8 #() 2000 0 29 2018 2064 1 2080 1 2050 #fixedPreviousBottom 1 2050 #fixedParentBottom 1 170 192 34 2 2128 8 'document' 0 1778 138 144 34 1 1842 #createAt:extent: 34 2 738 5119 21 738 1001 601 416 1938 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 243 11 0 0 54 1 0 0] 34 2 544 2128 2000 0 27 )! !
!CodeSourcePresenter class categoriesFor: #codeFont!public! !
!CodeSourcePresenter class categoriesFor: #codeFont:!public! !
!CodeSourcePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

