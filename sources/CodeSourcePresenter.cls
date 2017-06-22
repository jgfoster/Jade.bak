"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #CodeSourcePresenter
	instanceVariableNames: 'documentPresenter menuTitle'
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
	string := documentPresenter value copyFrom: range start to: range stop.
	list := self findClassList.
	assoc := list 
		detect: [:each | each key = string]
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

findClassList
"
	Array with: className with: dictionaryName with: catetory with: packageName.
"
	| string list |
	string := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: 'findClass'.
	list := (string subStrings: Character lf) collect: [:each | each subStrings: Character tab].
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
		documentPresenter view selectAll; copySelection.
	].
	^true
!

isClassSelectedInEditor

	| range string |
	(range := documentPresenter view selectionRange) isEmpty ifTrue: [^false].
	string := documentPresenter value copyFrom: range start to: range stop.
	^(string allSatisfy: [:each | each isAlphaNumeric]) and: [string first isLetter and: [string first isUppercase]]
!

isModified

	^documentPresenter isModified!

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

selectAll
	^self editSelectAll!

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
!CodeSourcePresenter categoriesFor: #executeSelectionOrLine!Jade!private! !
!CodeSourcePresenter categoriesFor: #fileSave!private! !
!CodeSourcePresenter categoriesFor: #findClassList!public! !
!CodeSourcePresenter categoriesFor: #handleInvalidSession!public! !
!CodeSourcePresenter categoriesFor: #isClassSelectedInEditor!public! !
!CodeSourcePresenter categoriesFor: #isModified!public! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 0 0 34 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ScintillaView) 34 50 0 416 34 2 8 1445007428 1025 544 721990 2 ##(Smalltalk.ValueHolder) 0 false 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 5 265030 4 ##(Smalltalk.Menu) 0 true 34 23 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #fileSave 8 '&Save' 9383 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 770 1 802 #undo 8 '&Undo' 9397 1 0 0 0 770 1 802 #redo 8 'R&edo' 9395 1 0 0 0 850 4097 770 1 802 #editCut 8 'Cu&t' 9393 1 0 0 0 770 1 802 #editCopy 8 '&Copy' 9351 1 0 0 0 770 1 802 #editPaste 8 '&Paste' 9389 1 0 0 0 770 1 802 #editDelete 8 'De&lete' 1629 1 0 0 0 770 1 802 #editSelectAll 8 'Select &All' 9347 1 0 0 0 850 4097 770 1 802 #editFind 8 '&Find...' 9357 1 0 0 0 770 1 802 #editFindNext 8 'Find &Next' 9359 1 0 0 0 770 1 802 #editReplace 8 '&Replace...' 9361 1 0 0 0 850 4097 770 1 802 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 770 1 802 #jadeDisplay 8 '&Display' 9353 1 0 0 0 770 1 802 #jadeExecute 8 'Execute' 9355 1 0 0 0 850 4097 770 1 802 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 770 1 802 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 850 4097 770 1 802 #fileIn 8 'File In' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 544 0 8 4294905729 852486 ##(Smalltalk.NullConverter) 0 0 9 0 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 0 0 1 0 0 0 0 #normal 0 0 0 #callTip 1938 77 786694 ##(Smalltalk.IndexedColor) 33554457 1986 33554471 1 0 0 0 0 #callTip 0 0 0 34 256 1952 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1968 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 0 0 false 138 144 8 #() 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 0 0 0 544 #circle 0 0 0 0 0 0 138 ##(Smalltalk.IdentitySet) 2096 0 170 176 2096 9215 0 0 0 0 1986 33554447 0 0 65 0 0 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 1952 #callTip 1968 0 0 0 0 1 0 170 192 34 12 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 544 65025 3 false 1 0 0 0 0 0 0 3 2418 3 544 33423361 5 false 3 0 0 0 0 0 0 5 2418 5 544 511 1 false 5 0 0 0 0 0 0 8 'indicator10' 2418 21 544 33554459 3 0 0 0 0 0 0 0 0 8 'indicator9' 2418 19 544 33554459 13 0 0 0 0 0 0 0 0 8 'indicator8' 2418 17 544 33554447 1 0 0 0 0 0 0 0 0 0 0 170 192 34 6 #Notification 1938 1029 0 196934 1 ##(Smalltalk.RGB) 31185883 1 0 0 0 0 #Notification 0 0 0 #Warning 1938 1027 2626 60239 2626 31326207 1 0 0 0 0 #Warning 0 0 0 #Error 1938 1031 2626 335 2626 31317503 1 0 0 0 0 #Error 0 0 0 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 7 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 1826 1 51 1826 1001 551 544 2818 #contextMenu: 34 1 736 544 2818 #modificationEventMask: 8 #(4607) 544 2818 #hoverTime: 8 #(200) 544 2818 #caretPeriod: 8 #(530) 544 2818 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 544 61 3 0 0 0 0 3074 3 544 1 0 3 67108863 0 0 3074 5 544 1 0 3 -67108863 0 0 544 2818 #tabWidth: 8 #(4) 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 244 1 0 0 44 1 0 0] 8 #() 1826 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 3282 #fixedParentRight 1 3282 #fixedPreviousBottom 1 3282 #fixedParentBottom 1 410 ##(Smalltalk.Toolbar) 34 25 0 416 34 2 8 1140851500 131137 3360 0 674 8 4278190080 0 517 0 1746 0 true 1778 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1826 193 193 0 3360 674 3440 8 4294904659 170 192 2096 170 192 34 24 9909 853766 ##(Smalltalk.ToolbarButton) 9909 0 3360 1 802 #jadeDisplay 8 'Print Result of Selection or Line' 1 1 0 395334 3 ##(Smalltalk.Bitmap) 0 true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 0 7 1826 1857 33 55 9911 3602 9911 0 3360 1 802 #jadeExecute 8 'Evaluate Selection or Line' 1 1 0 3680 57 9913 3602 9913 0 3360 1 802 #jadeInspect 8 'Inspect Selection or Line' 1 1 0 3680 59 9915 1246982 ##(Smalltalk.ToolbarSystemButton) 9915 0 3360 1 802 #fileSave 8 'Save' 1 1 0 1 17 9917 3906 9917 0 3360 1 802 #editCut 8 'Cut' 1 1 0 1 1 9919 3906 9919 0 3360 1 802 #copySelection 8 'Copy' 1 1 0 1 3 9921 3906 9921 0 3360 1 802 #editPaste 8 'Paste' 1 1 0 1 5 9923 3906 9923 0 3360 1 802 #editDelete 8 'Delete' 1 1 0 1 11 9925 3906 9925 0 3360 1 802 #undo 8 'Undo' 1 1 0 1 7 9927 3906 9927 0 3360 1 802 #redo 8 'Redo' 1 1 0 1 9 9929 3906 9929 0 3360 1 802 #editFind 8 'Find' 1 1 0 1 25 9931 3906 9931 0 3360 1 802 #editReplace 8 'Replace' 1 1 0 1 27 34 15 3616 3808 3856 1050118 ##(Smalltalk.ToolbarSeparator) 0 0 3360 3 0 1 3920 4370 0 0 3360 3 0 1 3968 4016 4064 4112 4160 4208 4370 0 0 3360 3 0 1 4256 4304 170 176 34 4 1 117 3680 1 0 1 0 1826 33 33 1826 45 45 0 0 2754 138 144 34 2 2818 #createAt:extent: 34 2 1826 1 1 1826 1001 51 3360 2818 #updateSizePosted 2096 3360 3170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 244 1 0 0 25 0 0 0] 8 #() 3232 0 27 3250 3296 1 3312 1 3282 #fixedParentTop 1 3282 #fixedViewTop 51 170 192 34 2 544 8 'document' 0 2754 138 144 34 1 2818 #createAt:extent: 34 2 1826 5119 21 1826 1001 601 416 3170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 243 11 0 0 54 1 0 0] 34 2 3360 544 3232 0 27 )! !
!CodeSourcePresenter class categoriesFor: #codeFont!public! !
!CodeSourcePresenter class categoriesFor: #codeFont:!public! !
!CodeSourcePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

