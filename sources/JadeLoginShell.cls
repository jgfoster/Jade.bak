"Filed out from Dolphin Smalltalk 7"!

DocumentShell subclass: #JadeLoginShell
	instanceVariableNames: 'debugPathPresenter gemHostPresenter gemServicePresenter gemTaskPresenter gemTypePresenter hostPasswordPresenter hostUserIDPresenter initialsPresenter isModified loginTypePresenter passwordPresenter stoneHostPresenter stoneNamePresenter userIDPresenter versionListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeLoginShell guid: (GUID fromString: '{0fa658ce-d1b0-40d0-9305-5830b29aed32}')!
JadeLoginShell comment: 'model - an instance of JadeLogin (treated as a document)
'!
!JadeLoginShell categoriesForClass!Unclassified! !
!JadeLoginShell methodsFor!

aboutJade
	"See also JadeTextDocument>>#'aboutJade'"

	| version |
	version := [
		SessionManager current isRuntime 
		ifTrue: [SessionManager current version] 
		ifFalse: ['<Development>']
	] on: Error do: [:ex | 
		ex return: ex description printString.
	].
	MessageBox 
		notify: 
'Jade is an Alternative Development Environment (JADE) for
GemStone/Smalltalk that provides developer tools (Browsers 
and Workspaces) as an alternative to Topaz or GBS.

Jade was inspired by, built in, and includes components from 
Dolphin Smalltalk at http://www.object-arts.com.

Version ' , version , ' built by James Foster
with contributions from Bruno Buzzi Brassesco.'
		caption: 'About Jade'.
!

basicCaption

	^'Jade Login'.
!

checkScintillaVersion

	| expected actual |
	expected := ScintillaLibrary versionString.
	actual := ScintillaLibrary versionInfo productVersionString.
	expected = actual ifTrue: [^self].
	MessageBox warning: 'SciLexer.dll is version ' , actual , ' and we expected ' , expected , '
Please consider downloading the latest product package!!'
!

createComponents

	super createComponents.
	versionListPresenter		:= self add: ListPresenter			new name: 'versionList'.
	stoneHostPresenter 		:= self add: TextPresenter 		new name: 'stoneHost'.
	stoneNamePresenter 		:= self add: TextPresenter 		new name: 'stoneName'.
	gemTypePresenter 		:= self add: RadioButtonSetPresenter 	new name: 'gemType'.
	gemHostPresenter 		:= self add: TextPresenter 		new name: 'gemHost'.
	gemServicePresenter 	:= self add: TextPresenter 		new name: 'gemService'.
	gemTaskPresenter 		:= self add: TextPresenter 		new name: 'gemTask'.
	userIDPresenter 		:= self add: TextPresenter 		new name: 'userID'.
	passwordPresenter 		:= self add: TextPresenter 		new name: 'password'.
	loginTypePresenter 		:= self add: RadioButtonSetPresenter 	new name: 'loginType'.
	hostUserIDPresenter		:= self add: TextPresenter			new name: 'hostUserID'.
	hostPasswordPresenter	:= self add: TextPresenter			new name: 'hostPassword'.
	initialsPresenter		:= self add: TextPresenter			new name: 'initials'.
	debugPathPresenter		:= self add: TextPresenter			new name: 'debugPath'.
!

createSchematicWiring

	super createSchematicWiring.
	versionListPresenter		when: #selectionChanged 	send: #isModified: 	to: self with: true.
	stoneHostPresenter 		when: #valueChanged 	send: #isModified: 	to: self with: true.
	stoneNamePresenter 		when: #valueChanged 	send: #isModified: 	to: self with: true.
	gemTypePresenter		when: #valueChanged 	send: #isModified: 	to: self with: true.
	gemHostPresenter 		when: #valueChanged 	send: #isModified: 	to: self with: true.
	gemServicePresenter 	when: #valueChanged 	send: #isModified: 	to: self with: true.
	gemTaskPresenter 		when: #valueChanged 	send: #isModified: 	to: self with: true.
	userIDPresenter 		when: #valueChanged 	send: #isModified: 	to: self with: true.
	passwordPresenter 		when: #valueChanged 	send: #isModified: 	to: self with: true.
	loginTypePresenter		when: #valueChanged 	send: #isModified: 	to: self with: true.
	hostUserIDPresenter		when: #valueChanged 	send: #isModified: 	to: self with: true.
	hostPasswordPresenter	when: #valueChanged 	send: #isModified: 	to: self with: true.
	initialsPresenter		when: #valueChanged	send: #isModified:	to: self with: true.
	debugPathPresenter		when: #valueChanged	send: #isModified:	to: self with: true.!

debugGCI

	| default fileTypes path |
	default := debugPathPresenter value.
	default isEmpty ifTrue: [default := File workingDirectory , 'gciDebug.log'].
	fileTypes := 	#( 
		('Text Files (*.txt)' '*.txt')
		('Log Files (*.log)' '*.log')
		('All Files (*.*)' '*.*')).
	path := FileSaveDialog new
		caption: 'Add GCI debug information to log file';
		fileTypes: fileTypes;
		defaultExtension: 'log';
		value: default;
		showModal.
	debugPathPresenter value: (path ifNil: ['']).
!

editCopy

	| textView |
	textView := View focus.
	(textView isKindOf: TextEdit) ifFalse: [^Sound beep].
	textView hasSelection ifFalse: [^Sound beep].
	textView copySelection.
!

editCut

	| textView |
	textView := View focus.
	(textView isKindOf: TextEdit) ifFalse: [^Sound beep].
	textView hasSelection ifFalse: [^Sound beep].
	textView cutSelection.
!

editDelete

	| textView |
	textView := View focus.
	(textView isKindOf: TextEdit) ifFalse: [^Sound beep].
	textView clearSelection.
!

editPaste

	| textView |
	textView := View focus.
	(textView isKindOf: TextEdit) ifFalse: [^Sound beep].
	textView canPaste ifFalse: [^Sound beep].
	textView pasteClipboard.
!

editRedo

	View focus redo.
!

editSelectAll

	| textView |
	textView := View focus.
	(textView isKindOf: TextEdit) ifFalse: [^Sound beep].
	textView selectAll.
!

editUndo

	View focus undo.
!

fileNew

	self promptToSaveChanges ifFalse: [^self].
	filename := nil.
	self 
		model: self class defaultModel;
		updateView;
		updateCaption;
		yourself.
!

fileOpen
	"Prompts for a file to open into the receiver"

	| openFilename |
	self promptToSaveChanges ifFalse: [^self].
	openFilename := FileOpenDialog new
		fileTypes: self class fileTypes;
		defaultExtension: self class defaultFileExtension;
		value: SessionManager current imageBase , 'connections\*.GSS';
		showModal.
	openFilename isNil ifTrue: [^self].
	self filename: openFilename.
	self fileLoad.
!

fillVersionList
	| list |
	list := GciLibrary allSubclasses collect: [:each | each displayName].
	list := list reject: [:each | each isNil].
	list := list asSortedCollection: [:a :b | self isVersion: a lessThanOrEqualTo: b].
	versionListPresenter list: list!

getDocumentData

	self updateModel.
	^self model asXML.
!

isModified

	^isModified.
!

isModified: aBoolean

	isModified := aBoolean.
	self updateCaption.
!

isText

	^true.
!

isVersion: a lessThanOrEqualTo: b

	| aList bList |
	aList := a subStrings: $. .
	bList := b subStrings: $. .
	1 to: aList size do: [:i | 
		| x y |
		bList size < i ifTrue: [^false].
		x := aList at: i.
		y := bList at: i.
		((x allSatisfy: [:each | each isDigit]) and: [y allSatisfy: [:each | each isDigit]]) ifTrue: [
			x := x asNumber.
			y := y asNumber.
		].
		x < y ifTrue: [^true].
		x = y ifFalse: [^false].
	].
	^true.
!

login

	| session |
	self updateModel.
	[
		session := self model login.
	] on: Error do: [:ex | 
		MessageBox 
			errorMsg: ex description
			caption: 'Jade Login Error'.
		SessionManager current pause.
		^self.
	]. 
	self postLogin: session.
!

model: anObject

	super model: anObject.
	self isModified: false.
!

onCloseRequested: boolValueHolder

	self isModified: false.
	^super onCloseRequested: boolValueHolder.
!

onViewClosed

	self updateModel.
	model saveDefault.
	super onViewClosed.
!

onViewOpened

	super onViewOpened.
	self view position: 80 @ 5.
	self 
		fillVersionList;
		updateView;
		sendRequiredMessages;
		yourself.
	[self checkScintillaVersion] postToMessageQueue.!

openHomePage

	ShellLibrary default 
		shellOpen: 'http://www.gemtalksystems.com/index.php/products/gemstones/' 
		directory: ''.
!

postLogin: aGciSession

	versionListPresenter view disable.
	self setWorkingDirectory.
	JadeTranscript showOnSession: aGciSession.
!

queryCommand: query

	(#(#editCopy #editCut #editDelete ) includes: query commandSymbol) ifTrue: [
		query isEnabled: (View focus isKindOf: TextEdit).
		^true.
	].
	(#(#editSelectAll) includes: query commandSymbol) ifTrue: [
		query isEnabled: ((View focus isKindOf: TextEdit) and: [View focus value notEmpty]).
		^true.
	].
	(#(#editPaste) includes: query commandSymbol) ifTrue: [
		query isEnabled: ((View focus isKindOf: TextEdit) and: [View focus canPaste]).
		^true.
	].
	(#(#editUndo) includes: query commandSymbol) ifTrue: [
		query isEnabled: ((View focus isKindOf: TextEdit) and: [View focus canUndo]).
		^true.
	].
	(#(#editRedo) includes: query commandSymbol) ifTrue: [
		query isEnabled: ((View focus isKindOf: TextEdit) and: [View focus canRedo]).
		^true.
	].
	(#(#fileSave) includes: query commandSymbol) ifTrue: [
		query isEnabled: self isModified.
		^true.
	].
	^super queryCommand: query.
!

saveDocument

	| result |
	result := super saveDocument.
	result ifTrue: [self isModified: false].
	^result.
!

sendRequiredMessages
	"The ImageStripper seems to think these messages aren't needed, but we die during runtime packaging with a MessageNotUnderstood."

	'string1' equals: 'string2'.
	1 isImmediate.
!

setDocumentData: aString

	self model updateFrom: (JadeLogin fromXML: aString).
	self isModified: false.
	self updateView.
!

setWorkingDirectory

	File workingDirectory: SessionManager current imageBase.
!

showGemNRS

	Prompter
		on: (model generateGemNRS; gemNRS)
		prompt: 'Gem NRS'
		caption: 'Jade Login Info'.!

showStoneNRS

	Prompter
		on: (model generateStoneNRS; stoneNRS)
		prompt: 'Stone NRS'
		caption: 'Jade Login Info'.!

updateCaption
	"The receiver has changed in such a way that the caption may need to be refreshed.
	Do this here"

	| stream fileName suffix |
	stream := String writeStream.
	fileName := self hasFilename ifTrue: [self filename] ifFalse: ['Untitled'].
	fileName := File splitFilenameFrom: fileName.
	stream nextPutAll: fileName.
	suffix := self basicCaption.
	suffix notEmpty 
		ifTrue: 
			[stream
				nextPutAll: ' - ';
				nextPutAll: suffix].
	self isModified ifTrue: [stream nextPut: $*].
	self caption: stream contents!

updateModel

	self model
		gemVersion:	versionListPresenter 		selection;
		stoneHost: 	stoneHostPresenter 		value trimBlanks;
		stoneName: 	stoneNamePresenter 		value trimBlanks;
		gemType: 	gemTypePresenter 		value;
		gemHost: 		gemHostPresenter 		value trimBlanks;
		gemService: 	gemServicePresenter 	value trimBlanks;
		gemTask: 	gemTaskPresenter 		value trimBlanks;
		gsUserID: 		userIDPresenter 		value trimBlanks;
		gsPassword: 	passwordPresenter 		value trimBlanks;
		loginType: 	loginTypePresenter 		value;
		hostUserID:	hostUserIDPresenter		value trimBlanks;
		hostPassword:	hostPasswordPresenter	value trimBlanks;
		initials:		initialsPresenter		value trimBlanks;
		debugPath: 	debugPathPresenter		value trimBlanks;
		yourself.
!

updateView

	| selection |
	selection := versionListPresenter list 
		detect: [:each | each = self model gemVersion] 
		ifNone: [versionListPresenter list last].
	versionListPresenter		selection: selection.
	stoneHostPresenter 		value: self model stoneHost.
	stoneNamePresenter 		value: self model stoneName.
	gemTypePresenter 		value: self model gemType.
	gemHostPresenter 		value: self model gemHost.
	gemServicePresenter 	value: self model gemService.
	gemTaskPresenter 		value: self model gemTask.
	userIDPresenter 		value: self model gsUserID.
	passwordPresenter 		value: self model gsPassword.
	loginTypePresenter		value: self model loginType.
	hostUserIDPresenter		value: self model hostUserID.
	hostPasswordPresenter	value: self model hostPassword.
	initialsPresenter		value: self model initials.
	debugPathPresenter		value: self model debugPath.
	self isModified: false.
! !
!JadeLoginShell categoriesFor: #aboutJade!public! !
!JadeLoginShell categoriesFor: #basicCaption!accessing!public! !
!JadeLoginShell categoriesFor: #checkScintillaVersion!public! !
!JadeLoginShell categoriesFor: #createComponents!public! !
!JadeLoginShell categoriesFor: #createSchematicWiring!public! !
!JadeLoginShell categoriesFor: #debugGCI!commands!public! !
!JadeLoginShell categoriesFor: #editCopy!public! !
!JadeLoginShell categoriesFor: #editCut!public! !
!JadeLoginShell categoriesFor: #editDelete!public! !
!JadeLoginShell categoriesFor: #editPaste!public! !
!JadeLoginShell categoriesFor: #editRedo!public! !
!JadeLoginShell categoriesFor: #editSelectAll!public! !
!JadeLoginShell categoriesFor: #editUndo!public! !
!JadeLoginShell categoriesFor: #fileNew!commands!public! !
!JadeLoginShell categoriesFor: #fileOpen!commands!public! !
!JadeLoginShell categoriesFor: #fillVersionList!public! !
!JadeLoginShell categoriesFor: #getDocumentData!accessing!public! !
!JadeLoginShell categoriesFor: #isModified!public! !
!JadeLoginShell categoriesFor: #isModified:!public! !
!JadeLoginShell categoriesFor: #isText!public!testing! !
!JadeLoginShell categoriesFor: #isVersion:lessThanOrEqualTo:!public! !
!JadeLoginShell categoriesFor: #login!public! !
!JadeLoginShell categoriesFor: #model:!accessing!public! !
!JadeLoginShell categoriesFor: #onCloseRequested:!public! !
!JadeLoginShell categoriesFor: #onViewClosed!public! !
!JadeLoginShell categoriesFor: #onViewOpened!public! !
!JadeLoginShell categoriesFor: #openHomePage!public! !
!JadeLoginShell categoriesFor: #postLogin:!public! !
!JadeLoginShell categoriesFor: #queryCommand:!public! !
!JadeLoginShell categoriesFor: #saveDocument!public! !
!JadeLoginShell categoriesFor: #sendRequiredMessages!public! !
!JadeLoginShell categoriesFor: #setDocumentData:!accessing!public! !
!JadeLoginShell categoriesFor: #setWorkingDirectory!public! !
!JadeLoginShell categoriesFor: #showGemNRS!commands!public! !
!JadeLoginShell categoriesFor: #showStoneNRS!commands!public! !
!JadeLoginShell categoriesFor: #updateCaption!public! !
!JadeLoginShell categoriesFor: #updateModel!public! !
!JadeLoginShell categoriesFor: #updateView!public! !

!JadeLoginShell class methodsFor!

defaultFileExtension

	^'gss'!

defaultModel

	^JadeLogin new.
!

fileTypes

	^Array 
		with: #('GemStone/Smalltalk Files (*.GSS)' '*.GSS' )
		with: #('All Files (*.*)' '*.*' ).
!

icon

	^Icon fromFile: 'icons\GS32.ico'.
!

resource_Compact_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 26476545 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  701 471 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 52 410 8 ##(Smalltalk.GroupBox)  98 14 0 416 98 2 8 1140850695 65 624 0 482 8 4294967295 0 7 0 0 0 624 0 8 4294903245 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 5 211 530 681 91 624 818 8 #text: 98 1 8 'Gem' 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 105 0 0 0 86 1 0 0 150 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 5 1090 8 #fixedViewLeft 681 1090 8 #fixedParentTop 211 1090 8 #fixedViewTop 91 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1140853580 131137 1232 0 482 8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 1232 482 1328 8 4294903147 234 256 98 0 234 256 98 4 21567 1246982 ##(Smalltalk.ToolbarSystemButton)  21567 0 1232 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #getHelpOn 8 'Get help on widget' 1 1 0 1 23 21569 853766 ##(Smalltalk.ToolbarButton)  21569 0 1232 1 1570 8 #openHelp 8 'Jade Help' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 530 1857 33 71 98 2 1552 1648 234 240 98 4 1 1 1728 31 0 1 0 530 33 33 530 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 754 202 208 98 2 818 848 98 2 530 391 1 530 299 51 1232 818 8 #updateSize 1488 1232 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 0 0 0 0 88 1 0 0 25 0 0 0] 98 0 1040 0 27 1058 1090 8 #fixedPreviousRight 1 1090 8 #fixedParentRight 1 1168 1 1200 51 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2240 0 0 0 7 0 0 0 2240 0 8 4294903255 852486 ##(Smalltalk.NullConverter)  0 0 0 754 202 208 98 2 818 848 98 2 530 611 651 530 151 41 2240 818 928 98 1 8 'Password:' 2240 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 69 1 0 0 124 1 0 0 89 1 0 0] 98 0 1040 0 27 1058 1104 611 1136 151 1168 651 1200 41 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 2592 0 482 8 4278190080 0 7 0 0 0 2592 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 471 241 530 201 41 2592 818 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 2592 818 8 #isTextModified: 98 1 32 2592 818 8 #setMarginWidths: 98 1 98 2 7 7 2592 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 235 0 0 0 120 0 0 0 79 1 0 0 140 0 0 0] 98 0 1040 0 27 1058 1104 471 1136 201 1168 241 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 3104 0 0 0 7 0 0 0 3104 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 611 611 530 151 41 3104 818 928 98 1 8 'User ID:' 3104 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 49 1 0 0 124 1 0 0 69 1 0 0] 98 0 1040 0 27 1058 1104 611 1136 151 1168 611 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 3424 0 0 0 7 0 0 0 3424 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 21 247 530 91 31 3424 818 928 98 1 8 'Host/IP' 3424 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 123 0 0 0 55 0 0 0 138 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 247 1200 31 410 2256 98 16 0 416 98 2 8 1140850944 1 3744 0 0 0 7 0 0 0 3744 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 341 127 530 121 31 3744 818 928 98 1 8 'Database' 3744 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 170 0 0 0 63 0 0 0 230 0 0 0 78 0 0 0] 98 0 1040 0 27 1058 1104 341 1136 121 1168 127 1200 31 410 2608 98 16 0 416 98 2 8 1140916352 1025 4064 0 482 8 4278190080 0 7 0 0 0 4064 0 8 4294903261 2338 0 0 1 754 202 208 98 3 818 848 98 2 530 135 321 530 321 41 4064 818 2864 98 1 2898 3 1 3 4064 818 2944 98 1 32 4064 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 67 0 0 0 160 0 0 0 227 0 0 0 180 0 0 0] 98 0 1040 0 27 1058 1104 135 1136 321 1168 321 1200 41 410 1248 98 25 0 416 98 2 8 1409289036 131137 4448 0 482 8 4278190080 0 7 0 0 0 4448 482 4528 8 4294903147 234 256 1488 234 256 98 10 21561 1634 21561 0 4448 1 1570 8 #editPaste 8 'Paste' 1 1 0 1714 0 16 1760 8 'EditBar.bmp' 1808 0 7 530 193 33 5 21563 1634 21563 0 4448 1 1570 8 #editDelete 8 'Delete' 1 1 0 4688 11 21565 1634 21565 0 4448 1 1570 8 #editUndo 8 'Undo' 1 1 0 4688 7 21557 1634 21557 0 4448 1 1570 8 #editCut 8 'Cut' 1 1 0 4688 1 21559 1634 21559 0 4448 1 1570 8 #editCopy 8 'Copy' 1 1 0 4688 3 98 6 4864 4928 4624 4736 4800 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 4448 3 0 1 234 240 98 2 4688 1 0 1 0 530 33 33 530 45 45 0 0 754 202 208 98 2 818 848 98 2 530 151 1 530 241 51 4448 818 2096 1488 4448 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 0 0 0 0 195 0 0 0 25 0 0 0] 98 0 1040 0 27 1058 2176 1 1136 241 1168 1 1200 51 410 2608 98 16 0 416 98 2 8 1140916352 1025 5296 0 482 2688 0 7 0 0 0 5296 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 121 241 530 201 41 5296 818 2864 98 1 2898 3 1 3 5296 818 2944 98 1 32 5296 818 2992 98 1 98 2 7 7 5296 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 120 0 0 0 160 0 0 0 140 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 241 1200 41 410 2608 98 16 0 416 98 2 8 1140916352 1025 5712 0 482 8 4278190080 0 7 0 0 0 5712 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 81 581 530 201 41 5712 818 2864 98 1 2898 3 1 3 5712 818 2944 98 1 32 5712 818 2992 98 1 98 2 7 7 5712 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 40 0 0 0 34 1 0 0 140 0 0 0 54 1 0 0] 98 0 1040 0 27 1058 1104 81 1136 201 1168 581 1200 41 410 8 ##(Smalltalk.ComboBox)  98 17 0 416 98 2 8 1144063491 1025 6144 590662 2 ##(Smalltalk.ListModel)  202 208 1488 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 6144 0 8 4294903259 459270 ##(Smalltalk.Message)  8 #displayString 98 0 1488 401 754 202 208 98 1 818 848 98 2 530 131 51 530 371 47 6144 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 65 0 0 0 25 0 0 0 250 0 0 0 48 0 0 0] 98 0 1040 0 27 1058 1104 131 1136 371 1168 51 1200 47 410 2256 98 16 0 416 98 2 8 1140850944 1 6592 0 0 0 7 0 0 0 6592 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 21 167 530 71 31 6592 818 928 98 1 8 'User' 6592 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 83 0 0 0 45 0 0 0 98 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 71 1168 167 1200 31 410 2256 98 16 0 416 98 2 8 1140850944 1 6912 0 0 0 7 0 0 0 6912 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 341 247 530 91 31 6912 818 928 98 1 8 'NetLDI	' 6912 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 170 0 0 0 123 0 0 0 215 0 0 0 138 0 0 0] 98 0 1040 0 27 1058 1104 341 1136 91 1168 247 1200 31 410 2608 98 16 0 416 98 2 8 1140916384 1025 7232 0 482 8 4278190080 0 7 0 0 0 7232 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 361 711 530 201 41 7232 818 2864 98 1 2898 3 1 3 7232 818 2944 98 1 32 7232 818 2992 98 1 98 2 7 7 7232 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 99 1 0 0 24 1 0 0 119 1 0 0] 98 0 1040 0 27 1058 1104 361 1136 201 1168 711 1200 41 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 7664 0 0 0 7 0 0 0 7664 0 8 4294903245 1570 8 #login 8 'Login' 1 1 0 0 16 0 0 0 754 202 208 98 3 818 848 98 2 530 481 311 530 201 61 7664 818 8 #isEnabled: 98 1 32 7664 818 928 98 1 8 'Login' 7664 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 0 0 0 155 0 0 0 84 1 0 0 185 0 0 0] 98 0 1040 0 29 1058 1104 481 1136 201 1168 311 1200 61 410 2608 98 16 0 416 98 2 8 1140916352 1025 8080 0 482 2688 0 7 0 0 0 8080 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 121 121 530 201 41 8080 818 2864 98 1 2898 3 1 3 8080 818 2944 98 1 32 8080 818 2992 98 1 98 2 7 7 8080 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 60 0 0 0 160 0 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 121 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 8496 0 0 0 7 0 0 0 8496 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 11 321 530 121 31 8496 818 928 98 1 8 'Developer' 8496 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 160 0 0 0 65 0 0 0 175 0 0 0] 98 0 1040 0 27 1058 1104 11 1136 121 1168 321 1200 31 410 2256 98 16 0 416 98 2 8 1140850944 1 8816 0 0 0 7 0 0 0 8816 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 341 167 530 121 31 8816 818 928 98 1 8 'Password' 8816 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 170 0 0 0 83 0 0 0 230 0 0 0 98 0 0 0] 98 0 1040 0 27 1058 1104 341 1136 121 1168 167 1200 31 410 2608 98 16 0 416 98 2 8 1140916384 1025 9136 0 482 8 4278190080 0 7 0 0 0 9136 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 471 161 530 201 41 9136 818 2864 98 1 2898 3 1 3 9136 818 2944 98 1 32 9136 818 2992 98 1 98 2 7 7 9136 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 235 0 0 0 80 0 0 0 79 1 0 0 100 0 0 0] 98 0 1040 0 27 1058 1104 471 1136 201 1168 161 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 9568 0 0 0 7 0 0 0 9568 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 21 129 530 91 31 9568 818 928 98 1 8 'Host/IP' 9568 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 64 0 0 0 55 0 0 0 79 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 129 1200 31 410 1248 98 25 0 416 98 2 8 1409289036 131137 9888 0 482 8 4278190080 0 7 0 0 0 9888 482 9968 8 4294903147 234 256 1488 234 256 98 6 21551 1115910 ##(Smalltalk.ToolbarIconButton)  21551 0 9888 1 1570 8 #fileNew 8 'New' 1 1 263494 3 ##(Smalltalk.Icon)  0 16 1760 8 'DocumentShell.ico' 0 1714 0 16 0 0 0 0 3 530 33 33 1 21553 10066 21553 0 9888 1 1570 8 #fileOpen 8 'Open' 1 1 10146 0 16 1760 8 'FileOpen.ico' 1808 1714 0 16 0 0 0 0 3 530 33 33 1 21555 10066 21555 0 9888 1 1570 8 #fileSave 8 'Save' 1 1 10146 0 16 1760 8 'FileSave.ico' 1808 1714 0 16 0 0 0 0 3 530 33 33 1 98 4 10080 10224 10352 5010 0 0 9888 3 0 1 234 240 98 6 10448 5 10192 1 10320 3 0 1 0 530 33 33 530 45 45 0 0 754 202 208 98 2 818 848 98 2 530 1 1 530 151 51 9888 818 2096 1488 9888 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 75 0 0 0 25 0 0 0] 98 0 1040 0 27 1058 1104 1 1136 151 1168 1 1200 51 410 2608 98 16 0 416 98 2 8 1140916352 1025 10768 0 482 2688 0 7 0 0 0 10768 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 471 121 530 201 41 10768 818 2864 98 1 2898 3 1 3 10768 818 2944 98 1 32 10768 818 2992 98 1 98 2 7 7 10768 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 235 0 0 0 60 0 0 0 79 1 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 471 1136 201 1168 121 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 11184 0 0 0 7 0 0 0 11184 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 21 61 530 91 31 11184 818 928 98 1 8 'Version' 11184 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 30 0 0 0 55 0 0 0 45 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 61 1200 31 410 640 98 14 0 416 98 2 8 1140850695 65 11504 0 482 720 0 7 0 0 0 11504 0 8 4294903245 754 202 208 98 2 818 848 98 2 530 5 91 530 681 121 11504 818 928 98 1 8 'Stone' 11504 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 45 0 0 0 86 1 0 0 105 0 0 0] 98 0 1040 0 27 1058 1104 5 1136 681 1168 91 1200 121 410 2608 98 16 0 416 98 2 8 1140916352 1025 11824 0 482 2688 0 7 0 0 0 11824 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 121 161 530 201 41 11824 818 2864 98 1 2898 3 1 3 11824 818 2944 98 1 32 11824 818 2992 98 1 98 2 7 7 11824 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 80 0 0 0 160 0 0 0 100 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 161 1200 41 234 256 98 26 1232 8 'Other Tools' 2592 8 'gemService' 5296 8 'gemHost' 4064 8 'initials' 4448 8 'Edit Tools' 5712 8 'hostUserID' 6144 8 'versionList' 7232 8 'hostPassword' 8080 8 'stoneHost' 9136 8 'password' 9888 8 'File Tools' 10768 8 'stoneName' 11824 8 'userID' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 1570 10112 8 '&New' 9373 1 0 0 0 12578 1 1570 10256 8 '&Open...' 9375 1 0 0 0 12578 1 1570 10384 8 '&Save' 9383 1 0 0 0 12578 1 1570 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 12578 1 1570 8 #fileRevert 8 '&Revert' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 12578 1 1570 8 #exit 8 'E&xit' 17639 1 0 0 0 8 '&File' 0 134217729 0 0 21515 0 0 12530 0 16 98 9 12578 1 1570 4832 8 '&Undo' 9397 1 0 0 0 12578 1 1570 8 #editRedo 8 '&Redo' 9395 1 0 0 0 12866 4097 12578 1 1570 4896 8 'Cu&t' 9393 1 0 0 0 12578 1 1570 4960 8 '&Copy' 9351 1 0 0 0 12578 1 1570 4656 8 '&Paste' 9389 1 0 0 0 12578 1 1570 4768 8 'De&lete' 1629 1 0 0 0 12866 4097 12578 1 1570 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 8 '&Edit' 0 134217729 0 0 21531 0 0 12530 0 16 98 2 12578 1 1570 8 #setDefaultFont 8 'Set Default &Font' 1 1 0 0 0 12578 1 1570 8 #setCodeFont 8 'Set &Code Font' 1 1 0 0 0 8 '&Preferences' 0 134217729 0 0 21537 0 0 12530 0 16 98 7 12578 1 1570 8 #helpContents 8 '&Contents' 1 1 0 0 0 12578 1 1570 8 #helpOnThisTool 8 '&On this Tool' 1249 1 0 0 0 12578 1 1570 8 #helpWhatsThis 8 '&What''s This?' 5345 1 0 0 0 12866 4097 12578 1 1570 8 #openHomePage 8 '&GemStone Home Page' 1025 1 0 0 0 12866 4097 12578 1 1570 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 134217729 0 0 21549 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 10323 10146 0 16 1760 8 'icons\GS32.ico' 0 10146 0 16 1760 8 'icons\GS16.ico' 0 0 0 1 0 0 754 202 208 98 3 818 848 98 2 530 2879 21 530 701 471 416 818 928 98 1 8 'Jade Login' 416 818 8 #updateMenuBar 1488 416 978 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 253 6 0 0 245 0 0 0] 98 26 6144 8080 10768 11824 9136 5296 2592 4064 9888 4448 1232 5712 7232 7664 9568 3744 6912 3104 2240 3424 11504 624 11184 6592 8816 8496 1040 0 27 )!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13238272 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 801 651 551 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 10 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1140853580 131137 608 nil 466 8 4278190080 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 608 466 688 8 4294903609 170 192 8 #() 170 192 34 4 10521 853766 ##(Smalltalk.ToolbarButton) 10521 nil 608 1 1180998 4 ##(Smalltalk.CommandDescription) #openHelp 8 'Jade Help' 1 1 nil 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 514 1857 33 71 10519 1246982 ##(Smalltalk.ToolbarSystemButton) 10519 nil 608 1 930 #getHelpOn 8 'Get help on widget' 1 1 nil 1 23 34 2 1136 912 170 176 34 4 1 1 992 31 nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 391 1 514 379 51 608 1362 #updateSizePosted 848 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 0 0 0 0 128 1 0 0 25 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedPreviousRight 1 1570 #fixedParentRight 1 1570 #fixedParentTop 1 1570 #fixedViewTop 51 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409289036 131137 1648 nil 466 8 4278190080 nil 7 nil nil nil 1648 466 1728 8 4294903609 170 192 848 170 192 34 10 10517 898 10517 nil 1648 1 930 #editUndo 8 'Undo' 1 1 nil 978 nil true 1024 8 'EditBar.bmp' 1072 nil 7 514 193 33 7 10509 898 10509 nil 1648 1 930 #editCut 8 'Cut' 1 1 nil 1872 1 10511 898 10511 nil 1648 1 930 #editCopy 8 'Copy' 1 1 nil 1872 3 10513 898 10513 nil 1648 1 930 #editPaste 8 'Paste' 1 1 nil 1872 5 10515 898 10515 nil 1648 1 930 #editDelete 8 'Delete' 1 1 nil 1872 11 34 6 1920 1968 2016 2064 1824 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 1648 3 nil 1 170 176 34 2 1872 1 nil 1 nil 514 33 33 514 45 45 nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 151 1 514 241 51 1648 1362 #updateSizePosted 848 1648 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 0 0 0 0 195 0 0 0 25 0 0 0] 8 #() 1520 nil 27 1538 1584 1 1570 #fixedViewLeft 241 1616 1 1632 51 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 2432 nil 466 8 4278190080 nil 7 nil nil nil 2432 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 8 'Basic' 410 ##(Smalltalk.ContainerView) 34 15 nil 2432 34 2 8 1140850688 131073 2640 nil nil nil 7 nil nil nil 2640 546 170 176 34 28 410 ##(Smalltalk.StaticText) 34 16 nil 2640 34 2 8 1140850944 1 2752 nil nil nil 7 nil nil nil 2752 nil 8 1761240368 852486 ##(Smalltalk.NullConverter) nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 11 67 514 371 31 2752 1362 #text: 34 1 8 'RPC Gem Host Name or IP:' 2752 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 190 0 0 0 48 0 0 0] 8 #() 1520 nil 27 1538 1570 #fixedParentLeft 11 2416 371 1570 #fixedPreviousBottom 11 1632 31 410 ##(Smalltalk.TextEdit) 34 16 nil 2640 34 2 8 1140916352 1025 3120 nil 466 8 4278190080 nil 7 nil nil nil 3120 nil 8 4294903337 2834 nil nil 1 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 391 313 514 353 45 3120 1362 #setMarginWidths: 34 1 8 #(3 3) 3120 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 156 0 0 0 115 1 0 0 178 0 0 0] 8 #() 1520 nil 27 1538 1584 11 1600 -9 1570 #fixedPreviousTop 1 1632 45 410 ##(Smalltalk.StaticText) 34 16 nil 2640 34 2 8 1140850944 1 3488 nil nil nil 7 nil nil nil 3488 nil 8 1761240368 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 11 313 514 371 41 3488 1362 #text: 34 1 8 'Monticello User Name:' 3488 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 156 0 0 0 190 0 0 0 176 0 0 0] 8 #() 1520 nil 27 1538 3088 11 2416 371 3104 11 1632 41 410 ##(Smalltalk.StaticText) 34 16 nil 2640 34 2 8 1140850944 1 3808 nil nil nil 7 nil nil nil 3808 nil 8 1761240368 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 11 213 514 371 41 3808 1362 #text: 34 1 8 'GemStone User ID:' 3808 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 106 0 0 0 190 0 0 0 126 0 0 0] 8 #() 1520 nil 27 1538 3088 11 2416 371 3104 3 1632 41 410 ##(Smalltalk.StaticText) 34 16 nil 2640 34 2 8 1140850944 1 4128 nil nil nil 7 nil nil nil 4128 nil 8 1761240368 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 11 259 514 371 41 4128 1362 #text: 34 1 8 'GemStone Password:' 4128 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 129 0 0 0 190 0 0 0 149 0 0 0] 8 #() 1520 nil 27 1538 3088 11 2416 371 3104 3 1632 41 410 ##(Smalltalk.ComboBox) 34 17 nil 2640 34 2 8 1144063491 1025 4448 590662 2 ##(Smalltalk.ListModel) 138 144 848 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 nil 7 nil nil nil 4448 nil 8 4294902905 459270 ##(Smalltalk.Message) #displayString 8 #() 848 401 1298 138 144 34 1 1362 #createAt:extent: 34 2 514 391 11 514 353 53 4448 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 5 0 0 0 115 1 0 0 31 0 0 0] 8 #() 1520 nil 27 1538 1584 11 1600 -9 1616 11 1632 47 410 ##(Smalltalk.TextEdit) 34 16 nil 2640 34 2 8 1140916352 1025 4864 nil 466 8 4278190080 nil 7 nil nil nil 4864 nil 8 4294903337 2834 nil nil 1 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 391 213 514 353 45 4864 1362 #setMarginWidths: 34 1 8 #(3 3) 4864 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 106 0 0 0 115 1 0 0 128 0 0 0] 8 #() 1520 nil 27 1538 1584 11 1600 -9 3472 1 1632 45 410 ##(Smalltalk.TextEdit) 34 16 nil 2640 34 2 8 1140916352 1025 5216 nil 466 4944 nil 7 nil nil nil 5216 nil 8 4294903337 2834 nil nil 1 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 391 167 514 353 45 5216 1362 #setMarginWidths: 34 1 8 #(3 3) 5216 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 83 0 0 0 115 1 0 0 105 0 0 0] 8 #() 1520 nil 27 1538 1584 11 1600 -9 3472 1 1632 45 410 ##(Smalltalk.TextEdit) 34 16 nil 2640 34 2 8 1140916352 1025 5552 nil 466 4944 nil 7 nil nil nil 5552 nil 8 4294903337 2834 nil nil 1 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 391 67 514 353 45 5552 1362 #setMarginWidths: 34 1 8 #(3 3) 5552 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 33 0 0 0 115 1 0 0 55 0 0 0] 8 #() 1520 nil 27 1538 1584 11 1600 -9 3472 1 1632 45 410 ##(Smalltalk.StaticText) 34 16 nil 2640 34 2 8 1140850944 1 5888 nil nil nil 7 nil nil nil 5888 nil 8 1761240368 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 11 21 514 371 41 5888 1362 #text: 34 1 8 'GemStone/Smalltalk Version:' 5888 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 190 0 0 0 30 0 0 0] 8 #() 1520 nil 27 1538 3088 11 2416 371 1616 21 1632 41 410 ##(Smalltalk.TextEdit) 34 16 nil 2640 34 2 8 1140916352 1025 6208 nil 466 4944 nil 7 nil nil nil 6208 nil 8 4294903337 2834 nil nil 1 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 391 113 514 353 45 6208 1362 #setMarginWidths: 34 1 8 #(3 3) 6208 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 56 0 0 0 115 1 0 0 78 0 0 0] 8 #() 1520 nil 27 1538 1584 11 1600 -9 3472 1 1632 45 410 ##(Smalltalk.TextEdit) 34 16 nil 2640 34 2 8 1140916384 1025 6544 nil 466 8 4278190080 nil 7 nil nil nil 6544 nil 8 4294903337 2834 nil nil 1 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 391 259 514 353 45 6544 1362 #setMarginWidths: 34 1 8 #(3 3) 6544 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 129 0 0 0 115 1 0 0 151 0 0 0] 8 #() 1520 nil 27 1538 1584 11 1600 -9 3472 1 1632 45 410 ##(Smalltalk.StaticText) 34 16 nil 2640 34 2 8 1140850944 1 6896 nil nil nil 7 nil nil nil 6896 nil 8 1761240368 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 11 167 514 371 39 6896 1362 #text: 34 1 8 'Stone Name:' 6896 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 83 0 0 0 190 0 0 0 102 0 0 0] 8 #() 1520 nil 27 1538 3088 11 2416 371 3104 11 1632 39 410 ##(Smalltalk.StaticText) 34 16 nil 2640 34 2 8 1140850944 1 7216 nil nil nil 7 nil nil nil 7216 nil 8 1761240368 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 11 113 514 371 31 7216 1362 #text: 34 1 8 'NetLDI Service Name or Port:' 7216 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 56 0 0 0 190 0 0 0 71 0 0 0] 8 #() 1520 nil 27 1538 3088 11 2416 371 3104 3 1632 31 170 192 34 14 5216 8 'stoneName' 6208 8 'gemService' 3120 8 'initials' 4864 8 'userID' 6544 8 'password' 4448 8 'versionList' 5552 8 'gemHost' nil 1298 138 144 34 1 1362 #createAt:extent: 34 2 514 9 55 514 753 361 2640 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 124 1 0 0 207 0 0 0] 34 14 5888 4448 2752 5552 7216 6208 6896 5216 3808 4864 4128 6544 3488 3120 1520 nil 27 2594 8 'Advanced' 410 ##(Smalltalk.ContainerView) 34 15 nil 2432 34 2 8 1140850688 131073 7872 nil nil nil 5 nil nil nil 7872 546 170 176 34 24 410 ##(Smalltalk.TextEdit) 34 16 nil 7872 34 2 8 1140916352 1025 7984 nil 466 8 4278190080 nil 5 nil nil nil 7984 nil 8 4294903337 2834 nil nil 1 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 491 163 514 253 45 7984 1362 #setMarginWidths: 34 1 8 #(3 3) 7984 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 81 0 0 0 115 1 0 0 103 0 0 0] 8 #() 1520 nil 27 1538 1584 11 1600 -9 3472 1 1632 45 410 ##(Smalltalk.StaticText) 34 16 nil 7872 34 2 8 1140850944 1 8336 nil nil nil 5 nil nil nil 8336 nil 8 1761240368 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 11 69 514 301 41 8336 1362 #text: 34 1 8 'Stone Host (from Gem):' 8336 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 34 0 0 0 155 0 0 0 54 0 0 0] 8 #() 1520 nil 27 1538 3088 11 2416 301 3104 11 1632 41 410 ##(Smalltalk.ContainerView) 34 15 nil 7872 34 2 8 1140850688 393217 8656 nil 466 8 4278190080 nil 5 nil nil nil 8656 656390 ##(Smalltalk.GridLayout) 3 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 8656 34 2 8 1141055497 1 8816 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 4642 #= 8 #() 4642 #hash 8 #() false 466 8736 nil 5 nil nil nil 8816 nil 8 4294903359 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 365 1 514 365 45 8816 1362 #text: 34 1 8 'Linked Gem (32-bit only)' 8816 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 182 0 0 0 0 0 0 0 108 1 0 0 22 0 0 0] 8 #() 1520 nil 27 8 'linked' 410 ##(Smalltalk.RadioButton) 34 16 nil 8656 34 2 8 1140924425 1 9280 8882 nil nil 8914 4642 #= 8 #() 4642 #hash 8 #() false 466 8736 nil 5 nil nil nil 9280 nil 8 4294903359 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 1 1 514 365 45 9280 1362 #text: 34 1 8 'RPC Gem' 9280 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 182 0 0 0 22 0 0 0] 8 #() 1520 nil 27 8 'remote' nil 1298 138 144 34 1 1362 #createAt:extent: 34 2 514 11 257 514 733 49 8656 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 128 0 0 0 115 1 0 0 152 0 0 0] 34 2 9280 8816 1520 nil 27 1538 3088 11 1600 -9 3104 5 1632 49 410 ##(Smalltalk.StaticText) 34 16 nil 7872 34 2 8 1140850944 1 9888 nil nil nil 5 nil nil nil 9888 nil 8 1761240368 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 11 115 514 301 31 9888 1362 #text: 34 1 8 'NetLDI Task Name:' 9888 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 57 0 0 0 155 0 0 0 72 0 0 0] 8 #() 1520 nil 27 1538 3088 11 2416 301 3104 3 1632 31 410 ##(Smalltalk.ContainerView) 34 15 nil 7872 34 2 8 1409286144 393217 10208 nil 466 8 4278190080 nil 5 nil nil nil 10208 8754 1 3 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 10208 34 2 8 1141055497 1 10352 8882 nil nil 8914 4642 #= 8 #() 4642 #hash 8 #() false 466 10288 nil 5 nil nil nil 10352 nil 8 4294903359 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 1 1 514 297 39 10352 1362 #text: 34 1 8 'NetLDI Guest' 10352 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 148 0 0 0 19 0 0 0] 8 #() 1520 nil 27 8 'guest' 410 ##(Smalltalk.RadioButton) 34 16 nil 10208 34 2 8 1140924425 1 10784 8882 nil nil 8914 4642 #= 8 #() 4642 #hash 8 #() false 466 10288 nil 5 nil nil nil 10784 nil 8 4294903359 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 1 39 514 297 39 10784 1362 #text: 34 1 8 'Native OS User' 10784 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 148 0 0 0 38 0 0 0] 8 #() 1520 nil 27 8 'member' nil 1298 138 144 34 1 1362 #createAt:extent: 34 2 514 11 163 514 301 81 10208 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 81 0 0 0 155 0 0 0 121 0 0 0] 34 2 10352 10784 1520 nil 27 1538 3088 11 2416 301 3104 5 1632 81 410 ##(Smalltalk.StaticText) 34 16 nil 7872 34 2 8 1140850944 1 11392 nil nil nil 5 nil nil nil 11392 nil 8 1761240368 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 321 209 514 161 41 11392 1362 #text: 34 1 8 'Password:' 11392 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 104 0 0 0 240 0 0 0 124 0 0 0] 8 #() 1520 nil 27 1538 1570 #fixedPreviousLeft -169 2416 161 3104 3 1632 41 410 ##(Smalltalk.PushButton) 34 20 nil 7872 34 2 8 1140924416 1 11728 nil nil nil 5 nil nil nil 11728 nil 8 4294903359 930 #debugGCI 8 'Debug GCI' 1 1 nil nil false nil nil nil 1298 138 144 34 3 1362 #createAt:extent: 34 2 514 11 5 514 141 51 11728 1362 #isEnabled: 8 #(false) 11728 1362 #text: 34 1 8 'Debug GCI' 11728 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 75 0 0 0 27 0 0 0] 8 #() 1520 nil 29 1538 3088 11 2416 141 3104 5 1632 51 410 ##(Smalltalk.TextEdit) 34 16 nil 7872 34 2 8 1140916352 1025 12096 nil 466 4944 nil 5 nil nil nil 12096 nil 8 4294903337 2834 nil nil 1 1298 138 144 34 3 1362 #createAt:extent: 34 2 514 321 69 514 423 45 12096 1362 #text: 34 1 8 'localhost' 12096 1362 #setMarginWidths: 34 1 8 #(3 3) 12096 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 34 0 0 0 115 1 0 0 56 0 0 0] 8 #() 1520 nil 27 1538 1584 11 1600 -9 3472 1 1632 45 410 ##(Smalltalk.TextEdit) 34 16 nil 7872 34 2 8 1140916352 1025 12480 nil 466 4944 nil 5 nil nil nil 12480 nil 8 4294903337 2834 nil nil 1 1298 138 144 34 3 1362 #createAt:extent: 34 2 514 321 115 514 423 45 12480 1362 #text: 34 1 8 'gemnetobject' 12480 1362 #setMarginWidths: 34 1 8 #(3 3) 12480 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 57 0 0 0 115 1 0 0 79 0 0 0] 8 #() 1520 nil 27 1538 1584 11 1600 -9 3472 1 1632 45 410 ##(Smalltalk.StaticText) 34 16 nil 7872 34 2 8 1140850944 1 12864 nil nil nil 5 nil nil nil 12864 nil 8 1761240368 2834 nil nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 321 163 514 161 41 12864 1362 #text: 34 1 8 'OS User ID:' 12864 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 81 0 0 0 240 0 0 0 101 0 0 0] 8 #() 1520 nil 27 1538 1584 11 2416 161 3472 1 1632 41 410 ##(Smalltalk.StaticPath) 34 17 nil 7872 34 2 8 1140850944 1 13184 nil nil nil 5 nil nil nil 13184 nil 8 1761240368 2834 nil nil nil 8 'XXX' 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 161 15 514 583 45 13184 1362 #text: 34 1 13280 13184 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 7 0 0 0 115 1 0 0 29 0 0 0] 8 #() 1520 nil 27 1538 3088 161 1600 -9 3472 11 1632 45 410 ##(Smalltalk.TextEdit) 34 16 nil 7872 34 2 8 1140916384 1025 13504 nil 466 8 4278190080 nil 5 nil nil nil 13504 nil 8 4294903337 2834 nil nil 1 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 491 209 514 253 45 13504 1362 #setMarginWidths: 34 1 8 #(3 3) 13504 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 104 0 0 0 115 1 0 0 126 0 0 0] 8 #() 1520 nil 27 1538 1584 11 1600 -9 3472 1 1632 45 170 192 34 14 8656 8 'gemType' 13184 8 'debugPath' 10208 8 'loginType' 12480 8 'gemTask' 12096 8 'stoneHost' 13504 8 'hostPassword' 7984 8 'hostUserID' nil 1298 138 144 34 1 1362 #createAt:extent: 34 2 514 9 55 514 753 361 7872 1458 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 124 1 0 0 207 0 0 0] 34 12 11728 13184 8336 12096 9888 12480 10208 12864 7984 11392 13504 8656 1520 nil 27 2640 170 192 34 4 2640 8 'Basic' 7872 8 'Advanced' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 2432 34 2 8 1140916736 1 14224 4514 138 144 34 2 2624 7856 nil 4576 nil nil 1 nil nil nil 14224 nil 8 4294902085 787814 3 ##(Smalltalk.BlockClosure) 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 14368 7 257 nil 14354 0 nil 14386 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 14432 7 257 nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #noIcons nil nil nil nil nil 1298 138 144 34 3 1362 #createAt:extent: 34 2 514 1 1 514 769 423 14224 1362 #basicSelectionsByIndex: 34 1 8 #(1) 14224 1362 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 14224 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 211 0 0 0] 8 #() 1520 nil 27 1298 138 144 34 1 1362 #createAt:extent: 34 2 514 1 51 514 769 423 2432 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 128 1 0 0 236 0 0 0] 34 3 2640 7872 14224 1520 nil 27 1538 3088 1 1600 1 1616 51 1570 #fixedParentBottom -59 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 14944 nil nil nil 7 nil nil nil 14944 nil 8 4294903359 930 #login 8 'Login' 1 1 nil nil true nil nil nil 1298 138 144 34 3 1362 #createAt:extent: 34 2 514 1 473 514 769 61 14944 1362 #isEnabled: 8 #(false) 14944 1362 #text: 34 1 8 'Login' 14944 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 236 0 0 0 128 1 0 0 10 1 0 0] 8 #() 1520 nil 29 1538 3088 1 1600 1 14928 -59 1632 61 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409289036 131137 15312 nil 466 8 4278190080 nil 7 nil nil nil 15312 466 15392 8 4294903609 170 192 848 170 192 34 6 10503 1115910 ##(Smalltalk.ToolbarIconButton) 10503 nil 15312 1 930 #fileNew 8 'New' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1024 8 'DocumentShell.ico' nil 978 nil true nil nil nil nil 3 514 33 33 1 10505 15490 10505 nil 15312 1 930 #fileOpen 8 'Open' 1 1 15554 nil true 1024 8 'FileOpen.ico' 1072 978 nil true nil nil nil nil 3 514 33 33 1 10507 15490 10507 nil 15312 1 930 #fileSave 8 'Save' 1 1 15554 nil true 1024 8 'FileSave.ico' 1072 978 nil true nil nil nil nil 3 514 33 33 1 34 4 15504 15632 15744 2130 nil nil 15312 3 nil 1 170 176 34 6 15712 3 15824 5 15600 1 nil 1 nil 514 33 33 514 45 45 nil nil 1298 138 144 34 2 1362 #createAt:extent: 34 2 514 1 1 514 151 51 15312 1362 #updateSizePosted 848 15312 1458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 75 0 0 0 25 0 0 0] 8 #() 1520 nil 27 1538 3088 1 2416 151 1616 1 1632 51 170 192 34 8 608 8 'Other Tools' 1648 8 'Edit Tools' 2432 8 'Tabs' 15312 8 'File Tools' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 265030 4 ##(Smalltalk.Menu) nil true 34 7 984134 2 ##(Smalltalk.CommandMenuItem) 1 930 #fileNew 8 '&New' 9373 1 nil nil nil 16338 1 930 #fileOpen 8 '&Open...' 9375 1 nil nil nil 16338 1 930 #fileSave 8 '&Save' 9383 1 nil nil nil 16338 1 930 #fileSaveAs 8 'Save &As...' 1 1 nil nil nil 16338 1 930 #fileRevert 8 '&Revert' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 16338 1 930 #exit 8 'E&xit' 17639 1 nil nil nil 8 '&File' nil 134217729 nil nil 10577 nil nil 16290 nil true 34 9 16338 1 930 #editUndo 8 '&Undo' 9397 1 nil nil nil 16338 1 930 #editRedo 8 '&Redo' 9395 1 nil nil nil 16594 4097 16338 1 930 #editCut 8 'Cu&t' 9393 1 nil nil nil 16338 1 930 #editCopy 8 '&Copy' 9351 1 nil nil nil 16338 1 930 #editPaste 8 '&Paste' 9389 1 nil nil nil 16338 1 930 #editDelete 8 'De&lete' 1629 1 nil nil nil 16594 4097 16338 1 930 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '&Edit' nil 134217729 nil nil 10593 nil nil 16290 nil true 34 9 16338 1 930 #helpContents 8 '&Contents' 1 1 nil nil nil 16338 1 930 #helpOnThisTool 8 '&On this Tool' 1249 1 nil nil nil 16338 1 930 #helpWhatsThis 8 '&What''s This?' 5345 1 nil nil nil 16594 4097 16338 1 930 #openHomePage 8 '&GemStone Home Page' 1025 1 nil nil nil 16594 4097 16338 1 930 #showGemNRS 8 'Show Gem &NRS' 1 1 nil nil nil 16338 1 930 #showStoneNRS 8 'Show Stone N&RS' 1 1 nil nil nil 16338 1 930 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 10609 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 9197 15554 nil true 1024 8 'icons\GS32.ico' nil 15554 nil true 1024 8 'icons\GS16.ico' nil nil nil 1 nil nil 1298 138 144 34 3 1362 #createAt:extent: 34 2 514 3839 21 514 801 651 416 1362 #text: 34 1 8 'Jade Login' 416 1362 #updateMenuBar 848 416 1458 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 15 9 0 0 79 1 0 0] 34 5 15312 1648 608 14944 2432 1520 nil 27 )!

showOnSample

	^self showOn: JadeLogin sample.
! !
!JadeLoginShell class categoriesFor: #defaultFileExtension!constants!public! !
!JadeLoginShell class categoriesFor: #defaultModel!public! !
!JadeLoginShell class categoriesFor: #fileTypes!constants!public! !
!JadeLoginShell class categoriesFor: #icon!constants!public! !
!JadeLoginShell class categoriesFor: #resource_Compact_view!public!resources-views! !
!JadeLoginShell class categoriesFor: #resource_Default_view!public!resources-views! !
!JadeLoginShell class categoriesFor: #showOnSample!public! !

