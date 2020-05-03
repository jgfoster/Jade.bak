"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #CodeSourcePresenter
	instanceVariableNames: 'documentPresenter menuTitle environment'
	classVariableNames: 'CodeFont'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CodeSourcePresenter guid: (GUID fromString: '{549a5009-cdd1-42b3-8907-c3c5c1c9e532}')!
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

browseSelectedClass

	| range string list assoc |
	range := documentPresenter view selectionRange.
	string := (documentPresenter value copyFrom: range start to: range stop) asCharacterCollection.
	list := self findClassList.
	assoc := list 
		detect: [:each | each key asCharacterCollection = string]
		ifNone: [^MessageBox warning: 'Class ' , string printString , ' not found!!!!' caption: 'Jade'].
	parentPresenter parentPresenter parentPresenter addSystemBrowserForClass: assoc value.
!

browseSenders

	self browseSendersOf: self currentSelector.!

clearBreakAtStepPoint: anInteger

	| method |
	(method := self trigger: #'needMethod') isNil ifTrue: [^self].
	method clearBreakAtStepPoint: anInteger.
!

clearSelection
	^self editDelete!

codeFont: aFont

	documentPresenter view font: aFont.
!

codePresenterIsMethod

	^false!

copySelection

	documentPresenter view
		copySelection;
		updateModel;
		yourself.
!

currentSelectionOrLine
	"answers CharacterCollection"
 
	| range |
	documentPresenter hasSelection ifFalse: [documentPresenter view selectCurrentLine].
	range := documentPresenter view selectionRange.
	^(documentPresenter value copyFrom: range start to: range stop) asCharacterCollection.
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

cutSelection
	^self editCut!

documentPresenter

	^documentPresenter!

editCut

	documentPresenter view
		cutSelection;
		updateModel;
		yourself.
!

editDelete

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

environment: anInteger

	environment := anInteger.
!

executeSelectionOrLine

	| contextObject |
	contextObject := self topShell isJadeInspector 
		ifTrue: [self topShell contextObject] 
		ifFalse: [model].
	[
		^true -> (gciSession executeString: self currentSelectionOrLine fromContext: contextObject environment: environment).
	] on: GsCompileError do: [:ex | 
		^false -> ex list.
	].
	self error: 'How did we get here?'.
	^false -> #(nil).
!

fileSave
		"Private - Answer whether the save succeeded (false means to stay on the window and cancel any attempt to leave)"

	^parentPresenter fileSave!

findClassList
"
	Array with: className with: dictionaryName with: catetory with: packageName.
"
	| string list |
	string := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: (self gciSession encode: '0 findClass').
	string := self gciSession decode: string.
	list := (string nonEmptySubStrings: Character lf) collect: [:each | each subStrings: Character tab].
	list := list copyFrom: 2 to: list size.
	list := list collect: [:each | each size < 3 ifTrue: [each , #('' '' '')] ifFalse: [each]].
	list := list collect: [:each | (each at: 1) -> each].
	^list
!

handleInvalidSession

	documentPresenter view isModified ifFalse: [^false].
	self ensureVisible.
	documentPresenter ensureVisible.
	documentPresenter view isModified: false.
	(MessageBox confirm: 'Copy unsaved method to clipboard?' caption: 'Invalid Session!!') ifTrue: [
		documentPresenter value copyToClipboard.
	].
	^true
!

initialize

	super initialize.
	environment := 0.
!

isClassSelectedInEditor

	| range string |
	(range := documentPresenter view selectionRange) isEmpty ifTrue: [^false].
	string := (documentPresenter value copyFrom: range start to: range stop) asCharacterCollection.
	^(string allSatisfy: [:each | each isAlphaNumeric]) and: [string first isLetter and: [string first isUppercase]]
!

isModified

	^documentPresenter isModified!

isModified: aBoolean

	^documentPresenter isModified: aBoolean!

jadeDebug

	[
		gciSession 
			debugString: (gciSession encode: self currentSelectionOrLine) 
			fromContext: model 
			environment: environment.
	] on: GsCompileError do: [:ex | 
		self jadeExecute.
	].
!

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
		^self		"flag for compile error"
	].
!

menuTitle: aString

	menuTitle := aString.
!

mySave
!

pasteClipboard
	^self editPaste!

queryCommand: query

	(#(#'fileSave') includes: query commandSymbol) ifTrue: [
		query isEnabled: documentPresenter isModified.
		^true.
	].
	(#(#cutSelection #copySelection) includes: query commandSymbol) ifTrue: [
		query isEnabled: documentPresenter hasSelection.
		^true.
	].
	(query commandSymbol = #pasteClipboard) ifTrue: [
		query isEnabled: documentPresenter view canPaste.
		^true.
	].
	(query commandSymbol == #'browseSelectedClass') ifTrue: [
		query isEnabled: self isClassSelectedInEditor.
		^true
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
	string := documentPresenter view selection trimBlanks asCharacterCollection.
	(string size >= 2 and: [string first = $' and: [string last = $']]) ifFalse: [
		^MessageBox notify: 'Selection must begin and end with quote'.
	].
	string := string copyFrom: 2 to: string size - 1.
	string := string 
		copyReplaceAll: ''''''
		with: ''''.
	documentPresenter view replaceSelection: string asUtf8String.
!

selectAll
	^self editSelectAll!

selectionChanging: aSelectionChangingEvent 

	(documentPresenter view isKindOf: DeafObject) ifTrue: [^self].
	documentPresenter view isModified ifFalse: [^self].
	documentPresenter view ensureVisible.
	MessageBox 
		confirm: 'Save changes?' 
		onYes: 	[aSelectionChangingEvent value: self fileSave] 
		onNo: 	[documentPresenter view isModified: false] 
		onCancel: 	[aSelectionChangingEvent value: false].
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
	string := string copyFrom: 1 to: offset - 1.
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
	self setCaretToEndOfSelection.
	(textView := documentPresenter view)
		replaceSelection: result asUtf8String;
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

text: aString

	documentPresenter value: aString.
!

textStyles

	^documentPresenter view editStyles textStyles!

undo

	documentPresenter view undo!

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
!CodeSourcePresenter categoriesFor: #browseSelectedClass!public! !
!CodeSourcePresenter categoriesFor: #browseSenders!public! !
!CodeSourcePresenter categoriesFor: #clearBreakAtStepPoint:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #clearSelection!public! !
!CodeSourcePresenter categoriesFor: #codeFont:!public! !
!CodeSourcePresenter categoriesFor: #codePresenterIsMethod!public! !
!CodeSourcePresenter categoriesFor: #copySelection!edit!private! !
!CodeSourcePresenter categoriesFor: #currentSelectionOrLine!Jade!private! !
!CodeSourcePresenter categoriesFor: #currentSelector!public! !
!CodeSourcePresenter categoriesFor: #cutSelection!public! !
!CodeSourcePresenter categoriesFor: #documentPresenter!public! !
!CodeSourcePresenter categoriesFor: #editCut!edit!private! !
!CodeSourcePresenter categoriesFor: #editDelete!edit!private! !
!CodeSourcePresenter categoriesFor: #editFind!edit!private! !
!CodeSourcePresenter categoriesFor: #editFindNext!edit!private! !
!CodeSourcePresenter categoriesFor: #editPaste!edit!private! !
!CodeSourcePresenter categoriesFor: #editReplace!edit!private! !
!CodeSourcePresenter categoriesFor: #editSelectAll!edit!private! !
!CodeSourcePresenter categoriesFor: #environment:!public! !
!CodeSourcePresenter categoriesFor: #executeSelectionOrLine!Jade!private! !
!CodeSourcePresenter categoriesFor: #fileSave!private! !
!CodeSourcePresenter categoriesFor: #findClassList!public! !
!CodeSourcePresenter categoriesFor: #handleInvalidSession!public! !
!CodeSourcePresenter categoriesFor: #initialize!public! !
!CodeSourcePresenter categoriesFor: #isClassSelectedInEditor!public! !
!CodeSourcePresenter categoriesFor: #isModified!public! !
!CodeSourcePresenter categoriesFor: #isModified:!public! !
!CodeSourcePresenter categoriesFor: #jadeDebug!Jade!private! !
!CodeSourcePresenter categoriesFor: #jadeDisplay!Jade!private! !
!CodeSourcePresenter categoriesFor: #jadeExecute!Jade!private! !
!CodeSourcePresenter categoriesFor: #jadeExecuteAndDisplay:!Jade!private! !
!CodeSourcePresenter categoriesFor: #menuTitle:!menus!public! !
!CodeSourcePresenter categoriesFor: #mySave!private! !
!CodeSourcePresenter categoriesFor: #pasteClipboard!public! !
!CodeSourcePresenter categoriesFor: #queryCommand:!public! !
!CodeSourcePresenter categoriesFor: #removeMenu!menus!public! !
!CodeSourcePresenter categoriesFor: #removeQuotesFromSelection!edit!private! !
!CodeSourcePresenter categoriesFor: #selectAll!public! !
!CodeSourcePresenter categoriesFor: #selectionChanging:!public! !
!CodeSourcePresenter categoriesFor: #setBreakAtStepPoint:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #setCaretToEndOfSelection!Jade!private! !
!CodeSourcePresenter categoriesFor: #setDocumentPresenterWith:!public! !
!CodeSourcePresenter categoriesFor: #setFont!private! !
!CodeSourcePresenter categoriesFor: #showCompileError:!Jade!private! !
!CodeSourcePresenter categoriesFor: #showResult:!Jade!private! !
!CodeSourcePresenter categoriesFor: #showSelection!edit!private! !
!CodeSourcePresenter categoriesFor: #subMenuName!menus!public! !
!CodeSourcePresenter categoriesFor: #subMenuPresenter!menus!public! !
!CodeSourcePresenter categoriesFor: #text:!public! !
!CodeSourcePresenter categoriesFor: #textStyles!public! !
!CodeSourcePresenter categoriesFor: #undo!Jade!private! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1140851500 131137 544 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 517 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 nil 544 624 8 1939804337 170 192 8 #() 170 192 34 26 47705 1115206 1 ##(Smalltalk.ToolbarIconButton) 544 1 1180998 4 ##(Smalltalk.CommandDescription) #jadeDisplay 8 'Print Result of Selection or Line' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'DisplayIt.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil 47705 47707 850 544 1 882 #jadeExecute 8 'Evaluate Selection or Line' 1 1 930 nil true 976 8 'EvaluateIt.ico' 1024 47707 47709 850 544 1 882 #jadeInspect 8 'Inspect Selection or Line' 1 1 930 nil true 976 8 'InspectIt.ico' 1024 47709 47711 850 544 1 882 #fileSave 8 'Save' 1 1 930 nil true 976 8 'FileSave.ico' 1024 47711 47713 850 544 1 882 #editCut 8 'a CommandDescription(Cut)' 1 1 930 nil true 976 8 'EditCut.ico' 1024 47713 47847 850 544 1 882 #jadeDebug 8 'Debug Selection or Line' 1 1 930 nil true 976 8 'Debugger.ico' 1024 47847 47717 850 544 1 882 #editPaste 8 'Paste' 1 1 930 nil true 976 8 'EditPaste.ico' 1024 47717 47715 850 544 1 882 #copySelection 8 'Copy' 1 1 930 nil true 976 8 'EditCopy.ico' 1024 47715 47719 850 544 1 882 #editDelete 8 'Delete' 1 1 930 nil true 976 8 'EditClear.ico' 1024 47719 47721 850 544 1 882 #undo 8 'Undo' 1 1 930 nil true 976 8 'EditUndo.ico' 1024 47721 47723 850 544 1 882 #redo 8 'Redo' 1 1 930 nil true 976 8 'EditRedo.ico' 1024 47723 47725 850 544 1 882 #editFind 8 'Find' 1 1 930 nil true 976 8 'Find.ico' 1024 47725 47727 850 544 1 882 #editReplace 8 'Replace' 1 1 930 nil true 976 8 'FindNext.ico' 1024 47727 34 16 864 1056 1136 1376 1049158 1 ##(Smalltalk.ToolbarSeparator) 544 1 1216 2034 544 1 1296 1536 1456 1616 1696 1776 2034 544 1 1856 1936 nil nil 1 nil 738 33 33 738 45 45 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 738 1 1 738 1001 51 544 2194 #updateSizePosted 800 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 244 1 0 0 25 0 0 0] 8 #() 738 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2402 #fixedParentRight 1 2402 #fixedParentTop 1 2402 #fixedViewTop 51 410 ##(Smalltalk.ScintillaView) 34 50 nil 416 34 2 8 1445007428 1025 2480 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 624 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 24 984134 2 ##(Smalltalk.CommandMenuItem) 1 882 #fileSave 8 '&Save' 9383 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 2658 1 882 #undo 8 '&Undo' 9397 1 nil nil nil 2658 1 882 #redo 8 'R&edo' 9395 1 nil nil nil 2722 4097 2658 1 882 #editCut 8 'Cu&t' 9393 1 nil nil nil 2658 1 882 #editCopy 8 '&Copy' 9351 1 nil nil nil 2658 1 882 #editPaste 8 '&Paste' 9389 1 nil nil nil 2658 1 882 #editDelete 8 'De&lete' 1629 1 nil nil nil 2658 1 882 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 2722 4097 2658 1 882 #editFind 8 '&Find...' 9357 1 nil nil nil 2658 1 882 #editFindNext 8 'Find &Next' 9359 1 nil nil nil 2658 1 882 #editReplace 8 '&Replace...' 9361 1 nil nil nil 2722 4097 2658 1 882 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 2658 1 882 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 2658 1 882 #jadeExecute 8 'Execute' 9355 1 nil nil nil 2658 1 882 #jadeDebug 8 'De&bug' 9349 1 nil nil nil 2722 4097 2658 1 882 #addQuotesToSelection 8 'Add &Quotes' 1 1 nil nil nil 2658 1 882 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 nil nil nil 2722 4097 2658 1 882 #fileIn 8 'File In' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil 658 nil true 690 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 738 193 193 nil 2480 nil 8 1660210793 852486 ##(Smalltalk.NullConverter) nil nil 9 nil 170 192 34 4 #callTip 1182790 1 ##(Smalltalk.ScintillaTextStyle) 77 786694 ##(Smalltalk.IndexedColor) 33554457 3842 33554471 1 nil nil nil nil #callTip nil nil nil #normal 3810 1 nil nil 1 nil nil nil nil #normal nil nil nil 34 256 3888 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 3824 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 800 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 2480 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 800 nil 170 176 800 9215 nil nil nil nil 3842 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #literalArray 8 '()' #specialCharacter 8 '()[]<>' 8 '' 3 170 192 34 2 #container 170 192 34 4 #callTip 3824 #normal 3888 nil nil nil nil 130003 nil 170 192 34 14 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 2480 65025 3 false 1 nil nil nil nil nil nil 3 4274 3 2480 33423361 5 false 3 nil nil nil nil nil nil 5 4274 5 2480 511 1 false 5 nil nil nil nil nil nil #secondarySelection 4274 23 2480 nil 15 nil #secondarySelection 31 nil nil nil nil nil 8 'indicator9' 4274 19 2480 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator10' 4274 21 2480 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator8' 4274 17 2480 33554447 1 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Warning 3810 1027 196934 1 ##(Smalltalk.RGB) 60239 4498 31326207 1 nil nil nil nil #Warning nil nil nil #Notification 3810 1029 nil 4498 31185883 1 nil nil nil nil #Notification nil nil nil #Error 3810 1031 4498 335 4498 31317503 1 nil nil nil nil #Error nil nil nil nil 2130 138 144 34 10 2194 #createAt:extent: 34 2 738 1 51 738 1001 551 2480 2194 #contextMenu: 34 1 2624 2480 2194 #textLimit: 8 #(1073741823) 2480 2194 #modificationEventMask: 8 #(4607) 2480 2194 #hoverTime: 8 #(200) 2480 2194 #caretPeriod: 8 #(530) 2480 2194 #sciSetEOLMode: 8 #(2) 2480 2194 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 2480 61 3 nil nil nil nil 4978 3 2480 1 nil 3 67108863 nil nil 4978 5 2480 1 nil 3 -67108863 nil nil 2480 2194 #tabWidth: 8 #(4) 2480 2194 #maxCompletionListHeight: 8 #(9) 2480 2290 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 244 1 0 0 44 1 0 0] 8 #() 2352 nil 29 2370 2416 1 2432 1 2402 #fixedPreviousBottom 1 2402 #fixedParentBottom 1 170 192 34 2 2480 8 'document' nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 738 1721 21 738 1001 601 416 2290 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 92 3 0 0 10 0 0 0 80 5 0 0 54 1 0 0] 34 2 544 2480 2352 nil 27 )! !
!CodeSourcePresenter class categoriesFor: #codeFont!public! !
!CodeSourcePresenter class categoriesFor: #codeFont:!public! !
!CodeSourcePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

