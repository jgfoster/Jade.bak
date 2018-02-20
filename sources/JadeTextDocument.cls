"Filed out from Dolphin Smalltalk 7"!

TextDocument subclass: #JadeTextDocument
	instanceVariableNames: 'gciSession codePane environment'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeTextDocument guid: (GUID fromString: '{b31562b7-43a8-4f81-ae6b-add2de2f1c0b}')!
JadeTextDocument comment: 'This class probably should not have any subclasses except Workspace (and Transcript). The CodeBrowser subclasses should be elsewhere since we don''t want to treat the code as an external text file that can be loaded and saved. '!
!JadeTextDocument categoriesForClass!Unclassified! !
!JadeTextDocument methodsFor!

abortTransaction

	gciSession abort.
	Sound informationBeep.
!

aboutJade

	| stream version |
	version := [
		SessionManager current isRuntime 
		ifTrue: [SessionManager current version] 
		ifFalse: ['<Development>']
	] on: Error do: [:ex | 
		ex return: ex description printString.
	].
	stream := WriteStream on: String new.
	stream
		nextPutAll: 'Jade for GemStone/S ('; 
		nextPutAll: version;
		nextPutAll: ')';
		cr;
		nextPutAll: 'GCI Version: ' , gciSession gciVersion;
		yourself.
	MessageBox 
		notify: stream contents
		caption: 'About Jade'.
!

activeTextEdit 

	^View focus.
!

addQuotesToSelection

	self activeTextEdit replaceSelection: self activeTextEdit selection printString.
!

codePresenterIsMethod

	^false!

codeSourcePresenterClass

	^CodeSourcePresenter
!

commitTransaction

	gciSession commit ifTrue: [
		Sound informationBeep.
	] ifFalse: [
		MessageBox warning: 'Commit failed!!'.
	].
!

contextObject

	^nil.
!

continueTransaction

	MessageBox notify: 'Sorry, we are not yet prepared to handle this feature!!'.
	SessionManager current pause.
!

createComponents

	super createComponents.
	view viewNamed: 'codePane' ifNone: [^self].
	codePane := self add: self codeSourcePresenterClass new name: 'codePane'.
	documentPresenter := codePane documentPresenter.
	self updateCodeFont.
!

currentSelectionOrLine

	self activeTextEdit hasSelection ifFalse: [self activeTextEdit selectCurrentLine].
	^self activeTextEdit selection replaceCrLfWithLf.
!

defineClass: aString inPackageNamed: anUndefinedObject 

	[
		gciSession 
			serverPerform: #'fileInClass:' 
			with: aString.
	] on: GsCompileError do: [:ex | 
		(JadeWorkspace showOnSession: gciSession)
			caption: 'Jade Workspace - Compile Error';
			showError: ex list on: aString.
	].
!

editCopy

	(self activeTextEdit selections size > 1) 
	ifTrue: [Clipboard current setText: self activeTextEdit selections last format: #String.
			^self activeTextEdit updateModel].

	self activeTextEdit 
		copySelection;
		updateModel;
		yourself.
!

editCut

	self activeTextEdit 
		cutSelection;
		updateModel;
		yourself.
!

editDelete

	self selectLfIfEndingOnCr.
	self activeTextEdit
		clearSelection;
		updateModel;
		yourself.
!

editFind
	"I'm not sure how it works, but this method isn't called!! 
	Somehow, the command is sent directly to the text widget."

self error: 'Do we ever get here!!?'.
	"self activeTextEdit editFind."
!

editFindNext

	self activeTextEdit findNext.
	self showSelection.

!

editPaste

	self activeTextEdit 
		pasteClipboard;
		updateModel;
		yourself.
!

editReplace

	self activeTextEdit 
		findReplace;
		updateModel;
		yourself.
!

editSelectAll

	self activeTextEdit selectAll.
!

environment: anInteger

	environment := anInteger.
!

executeSelectionOrLine
	"Returns anAssociation
		key:	true if compiled and returned a result
			false if we have a compile error
			nil if compile error with no details
		value: result, compile errors, or general description of error"
	[
		^true -> (gciSession 
			executeString: self currentSelectionOrLine 
			fromContext: self contextObject
			 environment: environment).
	] on: GsCompileError do: [:ex | 
		ex list notEmpty ifTrue: [^false -> ex list].
		^nil -> ex description.
	].
	self error: 'How did we get here!!?'.
!

fileIn: aString

	self setDocumentData: aString.
	self activeTextEdit selectAll.
	self 
		fileIn;
		isModified: false;
		yourself.
!

fileInEnd: aPackage
!

fileInStart: aPackage
!

fileNew

	JadeWorkspace showOnSession: gciSession.
!

fileOpen
	"Prompts for a file to open into the receiver"

	| openFilename |
	openFilename := self class getFilename.
	openFilename isNil ifTrue: [^self].
	(JadeWorkspace showOnSession: gciSession)
		openOn: openFilename;
		yourself.
!

gciSession
	^gciSession!

gciSession: aGciSession

	gciSession := aGciSession.
	gciSession
		when: #'logoutRequested:'	send: #'logoutRequested:'	to: self;
		when: #'logoutPending'	send: #'exit'			to: self;
		yourself.
!

getDocumentData

	^documentPresenter value ifNil: [''] ifNotNil: [:x | x asString].
!

help

	self aboutJade.
!

initialize

	super initialize.
	environment := 0.
!

jadeDebug
	"Intercepted by CodeSourcePresenter"!

jadeDisplay

	self jadeExecuteAndDisplay: true.

!

jadeExecute

	self jadeExecuteAndDisplay: false.
!

jadeExecuteAndDisplay: showResult 

	[
		| result |
		result := self executeSelectionOrLine.
		[self jadeExecuteAndDisplay: showResult result: result] postToInputQueue.
	] newProcess 
		name: 'GciSession';
		resume.!

jadeExecuteAndDisplay: showResult result: anAssociation

	| textView result value selectionRange offset |
	(result := anAssociation) key isNil ifTrue: [
		MessageBox notify: result value.
		^self.
	].
	textView := self activeTextEdit.
	result key ifTrue: [
		value := result value.
		result := ''.
		showResult ifTrue: [
			(gciSession isOopType: value) ifFalse: [
				result := ' ' , value printString.
			] ifTrue: [
				result := ' ' , (gciSession printString: value).
			].
		].
		selectionRange := textView selectionRange.
		result := result "replaceLfWithCrLf".
		textView
			caretPosition: selectionRange stop + 1;
			replaceSelection: result;
			selectionStart: textView caretPosition - result size length: result size.
	] ifFalse: [
		| string count |
		(result := result value first) isNil ifTrue: [^self].
		offset := result at: 2.
		result := result at: 3.
		selectionRange := textView selectionRange.
		(string := textView text) size < selectionRange stop ifTrue: [
			self error: 'Selection range is beyond text size!!?'.
			string := documentPresenter value.
		].
		string := string copyFrom: selectionRange start to: selectionRange stop.
		string := string replaceCrLfWithLf copyFrom: 1 to: offset - 1.
		count := (string select: [:each | each = Character lf]) size.
		offset := offset + count.
		textView
			caretPosition: selectionRange start + offset - 1;
			replaceSelection: result;
			selectionStart: textView caretPosition - result size length: result size.
	].
	^value.
!

logoutRequested: aValueHolder
	"Opportunity to save changes."

	aValueHolder value: true.
!

onCloseRequested: boolValueHolder

	super onCloseRequested: boolValueHolder.
"
	boolValueHolder value ifTrue: [
		| start |
		self view hide.
		start := Time millisecondClockValue.
		SessionManager inputState loopWhile: [(Time millisecondClockValue - start) abs < 100].
	].
"!

onPromptToSaveChanges: aBooleanValue 

	self getDocumentData isEmpty ifTrue: [
		self isModified: false.
	].
	^super onPromptToSaveChanges: aBooleanValue.
!

onViewClosed

	gciSession notNil ifTrue: [
		| temp |
		temp := gciSession.
		gciSession := nil.
		temp removeEventsTriggeredFor: self.
	].
	super onViewClosed.


!

onViewOpened

	codePane ifNotNil: [codePane gciSession: gciSession].
	super onViewOpened.
	self updateStatusBar.
	(documentPresenter view isKindOf: ScintillaView) ifFalse: [^self].
	documentPresenter view 
		restyleAll;
		backcolor: (RGB red: 250 green: 242 blue: 208);
		yourself.
	self updateCodeFont.
!

print!

queryCommand: query

	| textEdit |
	textEdit := self activeTextEdit.
	(#(#editCut #copySelection #fileIn) includes: query commandSymbol) ifTrue: [
		query isEnabled: (textEdit notNil and: [(textEdit isKindOf: TextEdit) and: [textEdit hasSelection]]).
		^true.
	].
	(query commandSymbol = #editPaste) ifTrue: [
		query isEnabled: (textEdit notNil and: [(textEdit isKindOf: TextEdit) and: [textEdit canPaste]]).
		^true.
	].
	^super queryCommand: query.
!

removeQuotesFromSelection

	| string |
	string := self activeTextEdit selection trimBlanks.
	(string size >= 2 and: [string first = $' and: [string last = $']]) ifFalse: [
		^MessageBox notify: 'Selection must begin and end with quote'.
	].
	string := string copyFrom: 2 to: string size - 1.
	string := string 
		copyReplaceAll: ''''''
		with: ''''.
	self activeTextEdit replaceSelection: string.
!

saveDocument

	| result |
	result := super saveDocument.
	result ifTrue: [self isModified: false].
	^result.
!

selectLfIfEndingOnCr
	"deleting a CR without the subsequent LF can leave things somewhat confused"

	| textEdit text text1 text2 selectionRange |
	textEdit := self activeTextEdit.
	selectionRange := textEdit selectionRange.
	text := textEdit view "hide; show;" value.			"somehow the value gets out of synch"
	selectionRange stop < selectionRange start 				ifFalse: [^self ].
	selectionRange start < textEdit value size 					ifFalse: [^self ].
	(text at: selectionRange start) = Character cr 			ifFalse: [^self ].
	(text at: selectionRange start + 1) = Character lf 		ifFalse: [^self ].
	textEdit selectionRange: (selectionRange start to: selectionRange start + 1).
!

setDocumentData: aString

	documentPresenter value: aString.
!

showError: aList on: aString

	self setDocumentData: aString.
	aList do: [:each | 
		| offset error |
		offset := (each at: 2).
		error := (each at: 3).
		self activeTextEdit 
			caretPosition: offset;
			replaceSelection: error;
			selectionStart: self activeTextEdit caretPosition - error size length: error size.
	].
	self isModified: false.
	(MessageBox confirm: 'Continue?' caption: 'Compile Error Found') ifTrue: [^self].
	TerminateProcess signal.
!

showSelection

	| myView range lineNumber |
	myView := self activeTextEdit.
	(range := myView selectionRange) isEmpty ifTrue: [^self].
	lineNumber := myView lineFromPosition: range first.
	lineNumber := lineNumber - 4 max: 1.
	myView lineScroll: lineNumber.
!

showText: aString

	self setDocumentData: aString.
	self isModified: false.
!

updateCodeFont

	codePane updateCodeFont.
!

updateStatusBar
!

updateStatusBarItem: aString with: anObject

	| item |
	item := self view 
		viewNamed: aString
		ifNone: [self error: 'statusBar item ' , aString printString , ' not found'].
	item model: (ValueHolder with: anObject).
!

validateUserInterface

	[
		super validateUserInterface.
	] on: Error do: [:ex | 
		view == DeafObject current ifTrue: [ex return: nil].
		ex pass.
	].! !
!JadeTextDocument categoriesFor: #abortTransaction!Jade!private! !
!JadeTextDocument categoriesFor: #aboutJade!private! !
!JadeTextDocument categoriesFor: #activeTextEdit!private! !
!JadeTextDocument categoriesFor: #addQuotesToSelection!edit!private! !
!JadeTextDocument categoriesFor: #codePresenterIsMethod!private! !
!JadeTextDocument categoriesFor: #codeSourcePresenterClass!private! !
!JadeTextDocument categoriesFor: #commitTransaction!Jade!private! !
!JadeTextDocument categoriesFor: #contextObject!Jade!private! !
!JadeTextDocument categoriesFor: #continueTransaction!Jade!private! !
!JadeTextDocument categoriesFor: #createComponents!private! !
!JadeTextDocument categoriesFor: #currentSelectionOrLine!Jade!private! !
!JadeTextDocument categoriesFor: #defineClass:inPackageNamed:!private! !
!JadeTextDocument categoriesFor: #editCopy!edit!private! !
!JadeTextDocument categoriesFor: #editCut!edit!private! !
!JadeTextDocument categoriesFor: #editDelete!edit!private! !
!JadeTextDocument categoriesFor: #editFind!edit!private! !
!JadeTextDocument categoriesFor: #editFindNext!edit!private! !
!JadeTextDocument categoriesFor: #editPaste!edit!private! !
!JadeTextDocument categoriesFor: #editReplace!edit!private! !
!JadeTextDocument categoriesFor: #editSelectAll!edit!private! !
!JadeTextDocument categoriesFor: #environment:!accessing!public! !
!JadeTextDocument categoriesFor: #executeSelectionOrLine!Jade!private! !
!JadeTextDocument categoriesFor: #fileIn:!private! !
!JadeTextDocument categoriesFor: #fileInEnd:!private! !
!JadeTextDocument categoriesFor: #fileInStart:!private! !
!JadeTextDocument categoriesFor: #fileNew!private! !
!JadeTextDocument categoriesFor: #fileOpen!private! !
!JadeTextDocument categoriesFor: #gciSession!private! !
!JadeTextDocument categoriesFor: #gciSession:!private! !
!JadeTextDocument categoriesFor: #getDocumentData!accessing!private! !
!JadeTextDocument categoriesFor: #help!private! !
!JadeTextDocument categoriesFor: #initialize!overrides!public! !
!JadeTextDocument categoriesFor: #jadeDebug!Jade!private! !
!JadeTextDocument categoriesFor: #jadeDisplay!Jade!private! !
!JadeTextDocument categoriesFor: #jadeExecute!Jade!private! !
!JadeTextDocument categoriesFor: #jadeExecuteAndDisplay:!Jade!private! !
!JadeTextDocument categoriesFor: #jadeExecuteAndDisplay:result:!Jade!private! !
!JadeTextDocument categoriesFor: #logoutRequested:!private! !
!JadeTextDocument categoriesFor: #onCloseRequested:!private! !
!JadeTextDocument categoriesFor: #onPromptToSaveChanges:!private! !
!JadeTextDocument categoriesFor: #onViewClosed!private! !
!JadeTextDocument categoriesFor: #onViewOpened!private! !
!JadeTextDocument categoriesFor: #print!private! !
!JadeTextDocument categoriesFor: #queryCommand:!commands!private! !
!JadeTextDocument categoriesFor: #removeQuotesFromSelection!edit!private! !
!JadeTextDocument categoriesFor: #saveDocument!private! !
!JadeTextDocument categoriesFor: #selectLfIfEndingOnCr!edit!private! !
!JadeTextDocument categoriesFor: #setDocumentData:!accessing!private! !
!JadeTextDocument categoriesFor: #showError:on:!private! !
!JadeTextDocument categoriesFor: #showSelection!edit!private! !
!JadeTextDocument categoriesFor: #showText:!private! !
!JadeTextDocument categoriesFor: #updateCodeFont!private! !
!JadeTextDocument categoriesFor: #updateStatusBar!private! !
!JadeTextDocument categoriesFor: #updateStatusBarItem:with:!private! !
!JadeTextDocument categoriesFor: #validateUserInterface!overrides!private! !

!JadeTextDocument class methodsFor!

defaultFileExtension

	^'gs'.
!

fileTypes

	^Array
		with: #('GemStone Files (*.gs)' '*.gs')
		with: #('Smalltalk Files (*.st)' '*.st')
		with: FileDialog allFilesType.
!

icon

	^Icon fromFile: 'icons\GS32.ico'.
!

openOnSession: aGciSession

	| openFilename |
	openFilename := self getFilename.
	openFilename isNil ifTrue: [^nil].
	^(self showOnSession: aGciSession)
		openOn: openFilename;
		yourself.
!

showOnSession: aGciSession

	^self new 
		gciSession: aGciSession;
		createView: self defaultView;
		showShell;
		yourself.

!

shutdownOnExit

	^false.
! !
!JadeTextDocument class categoriesFor: #defaultFileExtension!public! !
!JadeTextDocument class categoriesFor: #fileTypes!public! !
!JadeTextDocument class categoriesFor: #icon!public! !
!JadeTextDocument class categoriesFor: #openOnSession:!public! !
!JadeTextDocument class categoriesFor: #showOnSession:!public! !
!JadeTextDocument class categoriesFor: #shutdownOnExit!public! !

