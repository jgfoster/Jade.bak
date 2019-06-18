"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #JadeLoginShell
	instanceVariableNames: 'debugPathPresenter gemHostPresenter gemServicePresenter gemTaskPresenter gemTypePresenter hostPasswordPresenter hostUserIDPresenter initialsPresenter inUpdateView loginListPresenter loginTypePresenter passwordPresenter stoneHostPresenter stoneNamePresenter userIDPresenter versionListPresenter'
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

addLogin

	| login |
	login := loginListPresenter selectionOrNil 
		ifNil: [JadeLogin default]
		ifNotNil: [:value | value copy].
	loginListPresenter 
		list: (loginListPresenter list copyWith: login);
		selection: login;
		yourself.
!

checkScintillaVersion

	| expected actual |
	expected := ScintillaLibrary versionString.
	actual := ScintillaLibrary versionInfo productVersionString.
	expected = actual ifTrue: [^self].
	MessageBox warning: 'SciLexer.dll is version ' , actual , ' and we expected ' , expected , '
Please consider downloading the latest product package!!'
!

copyXML

	Clipboard current setText: loginListPresenter selection asXML.
	Sound informationBeep.
!

createComponents

	super createComponents.
	versionListPresenter		:= self add: ListPresenter			new name: 'versionList'.
	stoneHostPresenter 		:= self add: TextPresenter 			new name: 'stoneHost'.
	stoneNamePresenter 	:= self add: TextPresenter 			new name: 'stoneName'.
	gemTypePresenter 		:= self add: RadioButtonSetPresenter 	new name: 'gemType'.
	gemHostPresenter 		:= self add: TextPresenter 			new name: 'gemHost'.
	gemServicePresenter 	:= self add: TextPresenter 			new name: 'gemService'.
	gemTaskPresenter 		:= self add: TextPresenter 			new name: 'gemTask'.
	userIDPresenter 		:= self add: TextPresenter 			new name: 'userID'.
	passwordPresenter 		:= self add: TextPresenter 			new name: 'password'.
	loginListPresenter		:= self add: ListPresenter			new name: 'loginList'.
	loginTypePresenter 		:= self add: RadioButtonSetPresenter 	new name: 'loginType'.
	hostUserIDPresenter		:= self add: TextPresenter			new name: 'hostUserID'.
	hostPasswordPresenter	:= self add: TextPresenter			new name: 'hostPassword'.
	initialsPresenter			:= self add: TextPresenter			new name: 'initials'.
	debugPathPresenter		:= self add: TextPresenter			new name: 'debugPath'.
!

createSchematicWiring

	super createSchematicWiring.
	loginListPresenter	
		when: #'selectionChanged'	send: #'selectionChanged' 	to: self;
		when: #'actionPerformed'	send: #'openLogin'		to: self;
		yourself.
	versionListPresenter		when: #selectionChanged 	send: #modified 	to: self.
	stoneHostPresenter 		when: #valueChanged 	send: #modified 	to: self.
	stoneNamePresenter 	when: #valueChanged 	send: #modified 	to: self.
	gemTypePresenter		when: #valueChanged 	send: #modified 	to: self.
	gemHostPresenter 		when: #valueChanged 	send: #modified 	to: self.
	gemServicePresenter 	when: #valueChanged 	send: #modified 	to: self.
	gemTaskPresenter 		when: #valueChanged 	send: #modified 	to: self.
	userIDPresenter 		when: #valueChanged 	send: #modified 	to: self.
	passwordPresenter 		when: #valueChanged 	send: #modified 	to: self.
	loginTypePresenter		when: #valueChanged 	send: #modified 	to: self.
	hostUserIDPresenter		when: #valueChanged 	send: #modified 	to: self.
	hostPasswordPresenter	when: #valueChanged 	send: #modified 	to: self.
	initialsPresenter			when: #valueChanged	send: #modified 	to: self.
	debugPathPresenter		when: #valueChanged	send: #modified 	to: self.
	self					when: #closeRequested:	send: #saveData	to: self.
!

dataPath

	| regKey appData |
	regKey := RegKey userRoot at: 'Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders' ifAbsent: [nil].
	appData := regKey valueAt: 'AppData'.
	(appData endsWith: ' Dat') ifTrue: [appData := appData , 'a'].		"Wine seems to cut off a character"
	^appData , '\JadeData.gss'.
!

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

fileOpen
	"Prompts for a file to open into the receiver"

	| login path stream string |
	path := FileOpenDialog new
		fileTypes: (Array with: #('GemStone/Smalltalk Files (*.GSS)' '*.GSS' ) with: #('All Files (*.*)' '*.*' ));
		defaultExtension: 'gss';
		value: SessionManager current imageBase , 'connections\*.GSS';
		showModal.
	path isNil ifTrue: [^self].
	stream := FileStream read: path type: #'ansi'.
	string := stream upToEnd.
	stream close.
	login := JadeLogin fromXML: string.
	loginListPresenter 
		list: (loginListPresenter list copyWith: login);
		selection: login;
		yourself.
!

fillVersionList

	| list |
	list := GciLibrary allSubclasses collect: [:each | each displayName].
	list := list reject: [:each | each isNil].
	list := list asSortedCollection: [:a :b | self isVersion: a lessThanOrEqualTo: b].
	versionListPresenter list: list!

initialize

	super initialize.
	inUpdateView := false.
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

loadData

	| data |
	[
		| fileStream |
		fileStream := FileStream read: self dataPath type: #'binary'.
		data := Object binaryReadFrom: fileStream.
		fileStream close.
	] on: Error do: [:ex | 
		data := OrderedCollection new.
	].
	data size < 1 ifTrue: [data add: (OrderedCollection with: JadeLogin default)].
	data size < 2 ifTrue: [data add: data first first].
	data size < 3 ifTrue: [data add: JadePreferencesPresenter defaultFont].
	data size < 4 ifTrue: [data add: JadePreferencesPresenter codeFont].
	loginListPresenter 
		list: (data at: 1);
		selectionOrNil: (data at: 2);
		yourself.
	JadePreferencesPresenter defaultFont: (data at: 3).
	JadePreferencesPresenter codeFont: (data at: 4).
!

login

	| session |
	self updateModel.
	self model isAbleToLoadLibrary ifFalse: [
		Sound beep.
		^self
	].
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

logout: aLogin

	aLogin
		session: nil;
		transcript: nil;
		yourself.
	loginListPresenter view updateAll.
!

modified

	inUpdateView ifTrue: [^self].
	self updateModel.
	loginListPresenter view updateAll.!

onCloseRequested

	GciSession log: 'JadeLoginShell>>onCloseRequested at ' , DateAndTime now printString.
	^super onCloseRequested!

onViewOpened

	super onViewOpened.
	self view position: 80 @ 5.
	self 
		fillVersionList;
		loadData;
		updateView;
		sendRequiredMessages;
		yourself.
	[self checkScintillaVersion] postToMessageQueue.!

openExeDir

	[
		ShellLibrary default shellOpen: (SessionManager current getenv: 'LOCALAPPDATA') , '\VirtualStore\Program Files\Jade'.
	] on: Error do: [:ex | 
		ShellLibrary default shellOpen: (File splitPathFrom: SessionManager current imagePath).
	].!

openGemTalkHomePage

	ShellLibrary default shellOpen: 'https://gemtalksystems.com/'.
!

openJadeHomePage

	ShellLibrary default shellOpen: 'https://github.com/jgfoster/Jade'.
!

openLogin

	self model transcript
		ifNil: [self login] 
		ifNotNil: [:value | value view setFocus]
!

postLogin: aGciSession

	loginListPresenter view updateAll.
	self setWorkingDirectory.
	model 
		session: aGciSession;
		transcript: (JadeTranscript showOnSession: aGciSession);
		yourself.
	aGciSession 
		when: #logout
		send: #logout:
		to: self
		with: self model.

!

predefinedLogin

	| login string |
	string := Prompter 
		prompt: 'Enter the GemStone/S login data:' 
		caption: 'Predefined Login'.
	string ifNil: [^self].
	login := JadeLogin fromXML: string.
	loginListPresenter 
		list: (loginListPresenter list copyWith: login);
		selection: login;
		yourself.
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
	(#(#login) includes: query commandSymbol) ifTrue: [
		query isEnabled: (loginListPresenter hasSelection and: [loginListPresenter selection isAbleToLoadLibrary]).
		^true
	].
	(#(#copyXML #removeLogin) includes: query commandSymbol) ifTrue: [
		query isEnabled: loginListPresenter hasSelection.
		^true
	].
	^super queryCommand: query.
!

removeLogin

	self updateModel.
	loginListPresenter 
		list: (loginListPresenter list copyWithout: loginListPresenter selection);
		selectionOrNil: nil;
		yourself.
!

saveData
	"Version is essentially the number of elements"

	| data fileStream |
	data := OrderedCollection new
		add: loginListPresenter list;
		add: loginListPresenter selectionOrNil;
		add: JadePreferencesPresenter defaultFont;
		add: JadePreferencesPresenter codeFont;
		yourself.
	[
		fileStream := FileStream 
			write: self dataPath 
			mode: #'truncate' 
			check: false 
			type: #'binary'.
		data binaryStoreOn: fileStream.
		fileStream close.
	] on: Error do: [:ex | 
		MessageBox warning: ex description.
		fileStream ifNotNil: [fileStream close].
		ex return.
	].
!

selectionChanged

	self 
		updateModel;
		model: loginListPresenter selectionOrNil;
		updateView;
		yourself.
	!

sendRequiredMessages
	"The ImageStripper seems to think these messages aren't needed, but we die during runtime packaging with a MessageNotUnderstood."
"
	'string1' equals: 'string2'.
	1 isImmediate.
"!

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

updateModel

	inUpdateView ifTrue: [^self].
	self model ifNil: [^self].
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

	[
		inUpdateView := true.
		self model ifNil: [
			(self view viewNamed: 'bottom') hide.
		] ifNotNil: [
			| selection |
			(self view viewNamed: 'bottom') show.
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
		].
	] ensure: [
		inUpdateView := false.
	].! !
!JadeLoginShell categoriesFor: #aboutJade!public! !
!JadeLoginShell categoriesFor: #addLogin!commands!public! !
!JadeLoginShell categoriesFor: #checkScintillaVersion!public! !
!JadeLoginShell categoriesFor: #copyXML!commands!public! !
!JadeLoginShell categoriesFor: #createComponents!public! !
!JadeLoginShell categoriesFor: #createSchematicWiring!public! !
!JadeLoginShell categoriesFor: #dataPath!public! !
!JadeLoginShell categoriesFor: #debugGCI!commands!public! !
!JadeLoginShell categoriesFor: #editCopy!public! !
!JadeLoginShell categoriesFor: #editCut!public! !
!JadeLoginShell categoriesFor: #editDelete!public! !
!JadeLoginShell categoriesFor: #editPaste!public! !
!JadeLoginShell categoriesFor: #editRedo!public! !
!JadeLoginShell categoriesFor: #editSelectAll!public! !
!JadeLoginShell categoriesFor: #editUndo!public! !
!JadeLoginShell categoriesFor: #fileOpen!commands!public! !
!JadeLoginShell categoriesFor: #fillVersionList!public! !
!JadeLoginShell categoriesFor: #initialize!public! !
!JadeLoginShell categoriesFor: #isVersion:lessThanOrEqualTo:!public! !
!JadeLoginShell categoriesFor: #loadData!public! !
!JadeLoginShell categoriesFor: #login!public! !
!JadeLoginShell categoriesFor: #logout:!public! !
!JadeLoginShell categoriesFor: #modified!public! !
!JadeLoginShell categoriesFor: #onCloseRequested!public! !
!JadeLoginShell categoriesFor: #onViewOpened!public! !
!JadeLoginShell categoriesFor: #openExeDir!commands!public! !
!JadeLoginShell categoriesFor: #openGemTalkHomePage!public! !
!JadeLoginShell categoriesFor: #openJadeHomePage!public! !
!JadeLoginShell categoriesFor: #openLogin!public! !
!JadeLoginShell categoriesFor: #postLogin:!public! !
!JadeLoginShell categoriesFor: #predefinedLogin!commands!public! !
!JadeLoginShell categoriesFor: #queryCommand:!public! !
!JadeLoginShell categoriesFor: #removeLogin!commands!public! !
!JadeLoginShell categoriesFor: #saveData!public! !
!JadeLoginShell categoriesFor: #selectionChanged!public! !
!JadeLoginShell categoriesFor: #sendRequiredMessages!public! !
!JadeLoginShell categoriesFor: #setWorkingDirectory!public! !
!JadeLoginShell categoriesFor: #showGemNRS!commands!public! !
!JadeLoginShell categoriesFor: #showStoneNRS!commands!public! !
!JadeLoginShell categoriesFor: #updateModel!public! !
!JadeLoginShell categoriesFor: #updateView!public! !

!JadeLoginShell class methodsFor!

icon

	^Icon fromFile: 'icons\GS32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13238272 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1401 841 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 608 nil nil nil 7 nil nil nil 608 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 608 34 2 8 1140850688 131073 736 nil nil nil 7 nil nil nil 736 674 170 176 34 28 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 848 nil nil nil 7 nil nil nil 848 nil 8 1717865120 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 11 167 514 371 39 848 1026 #text: 34 1 8 'Stone Name:' 848 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 83 0 0 0 190 0 0 0 102 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 11 1266 #fixedViewLeft 371 1266 #fixedPreviousBottom 11 1266 #fixedViewTop 39 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916352 1025 1344 nil 480 nil 7 nil nil nil 1344 nil 8 1717706240 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 113 514 285 45 1344 1026 #setMarginWidths: 34 1 8 #(3 3) 1344 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 56 0 0 0 81 1 0 0 78 0 0 0] 8 #() 1216 nil 27 1234 1266 #fixedPreviousRight 11 1266 #fixedParentRight -9 1266 #fixedPreviousTop 1 1328 45 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916384 1025 1712 nil 480 nil 7 nil nil nil 1712 nil 8 1717706240 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 259 514 285 45 1712 1026 #setMarginWidths: 34 1 8 #(3 3) 1712 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 129 0 0 0 81 1 0 0 151 0 0 0] 8 #() 1216 nil 27 1234 1664 11 1680 -9 1696 1 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 2032 nil nil nil 7 nil nil nil 2032 nil 8 1717865120 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 113 514 371 31 2032 1026 #text: 34 1 8 'NetLDI Service Name or Port:' 2032 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 56 0 0 0 190 0 0 0 71 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 3 1328 31 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916352 1025 2352 nil 480 nil 7 nil nil nil 2352 nil 8 1717706240 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 167 514 285 45 2352 1026 #setMarginWidths: 34 1 8 #(3 3) 2352 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 83 0 0 0 81 1 0 0 105 0 0 0] 8 #() 1216 nil 27 1234 1664 11 1680 -9 1696 1 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 2672 nil nil nil 7 nil nil nil 2672 nil 8 1717865120 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 213 514 371 41 2672 1026 #text: 34 1 8 'GemStone User ID:' 2672 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 106 0 0 0 190 0 0 0 126 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 3 1328 41 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 2992 nil nil nil 7 nil nil nil 2992 nil 8 1717865120 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 21 514 371 41 2992 1026 #text: 34 1 8 'GemStone/Smalltalk Version:' 2992 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 190 0 0 0 30 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1266 #fixedParentTop 21 1328 41 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916352 1025 3328 nil 480 nil 7 nil nil nil 3328 nil 8 1717706240 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 67 514 285 45 3328 1026 #setMarginWidths: 34 1 8 #(3 3) 3328 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 33 0 0 0 81 1 0 0 55 0 0 0] 8 #() 1216 nil 27 1234 1664 11 1680 -9 1696 1 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 3648 nil nil nil 7 nil nil nil 3648 nil 8 1717865120 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 313 514 371 41 3648 1026 #text: 34 1 8 'Monticello User Name:' 3648 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 156 0 0 0 190 0 0 0 176 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 11 1328 41 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 3968 nil nil nil 7 nil nil nil 3968 nil 8 1717865120 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 259 514 371 41 3968 1026 #text: 34 1 8 'GemStone Password:' 3968 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 129 0 0 0 190 0 0 0 149 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 3 1328 41 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916352 1025 4288 nil 480 nil 7 nil nil nil 4288 nil 8 1717706240 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 313 514 285 45 4288 1026 #setMarginWidths: 34 1 8 #(3 3) 4288 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 156 0 0 0 81 1 0 0 178 0 0 0] 8 #() 1216 nil 27 1234 1664 11 1680 -9 1696 1 1328 45 410 ##(Smalltalk.ComboBox) 34 17 nil 736 34 2 8 1144063491 1025 4608 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 480 nil 7 nil nil nil 4608 nil 8 1718381632 459270 ##(Smalltalk.Message) #displayString 8 #() 4720 401 962 138 144 34 1 1026 #createAt:extent: 34 2 514 391 11 514 285 51 4608 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 5 0 0 0 81 1 0 0 30 0 0 0] 8 #() 1216 nil 27 1234 1664 11 1680 -9 3312 11 1328 47 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916352 1025 5008 nil 480 nil 7 nil nil nil 5008 nil 8 1717706240 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 213 514 285 45 5008 1026 #setMarginWidths: 34 1 8 #(3 3) 5008 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 106 0 0 0 81 1 0 0 128 0 0 0] 8 #() 1216 nil 27 1234 1664 11 1680 -9 1696 1 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 5328 nil nil nil 7 nil nil nil 5328 nil 8 1717865120 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 67 514 371 31 5328 1026 #text: 34 1 8 'RPC Gem Host Name or IP:' 5328 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 190 0 0 0 48 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 11 1328 31 170 192 34 14 4288 8 'initials' 1344 8 'gemService' 3328 8 'gemHost' 1712 8 'password' 5008 8 'userID' 2352 8 'stoneName' 4608 8 'versionList' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 1 1 514 685 367 736 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 1 0 0 183 0 0 0] 34 14 2992 4608 5328 3328 2032 1344 848 2352 2672 5008 3968 1712 3648 4288 1216 nil 27 1234 1280 1 1266 #relativeParentWidth 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 3312 1 1266 #fixedParentBottom 1 410 ##(Smalltalk.ContainerView) 34 15 nil 608 34 2 8 1140850688 131073 6032 nil nil nil 7 nil nil nil 6032 674 170 176 34 26 410 ##(Smalltalk.StaticText) 34 16 nil 6032 34 2 8 1140850944 1 6144 nil nil nil 7 nil nil nil 6144 nil 8 1717865120 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 115 514 301 31 6144 1026 #text: 34 1 8 'NetLDI Task Name:' 6144 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 57 0 0 0 155 0 0 0 72 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 301 1312 3 1328 31 410 ##(Smalltalk.PushButton) 34 20 nil 6032 34 2 8 1140924416 1 6464 nil nil nil 7 nil nil nil 6464 nil 8 1717867584 1180998 4 ##(Smalltalk.CommandDescription) #login 8 'Login' 1 1 nil nil true nil nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 11 305 514 665 61 6464 1026 #isEnabled: 8 #(false) 6464 1026 #text: 34 1 8 'Login' 6464 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 152 0 0 0 81 1 0 0 182 0 0 0] 8 #() 1216 nil 29 1234 1280 11 1680 -9 1312 1 1328 61 410 ##(Smalltalk.TextEdit) 34 16 nil 6032 34 2 8 1140916352 1025 6848 nil 480 nil 7 nil nil nil 6848 nil 8 1717706240 930 nil nil 1 962 138 144 34 3 1026 #createAt:extent: 34 2 514 321 115 514 355 45 6848 1026 #text: 34 1 8 'gemnetobject' 6848 1026 #setMarginWidths: 34 1 8 #(3 3) 6848 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 57 0 0 0 81 1 0 0 79 0 0 0] 8 #() 1216 nil 27 1234 1664 11 1680 -9 1696 1 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 6032 34 2 8 1140850944 1 7216 nil nil nil 7 nil nil nil 7216 nil 8 1717865120 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 69 514 301 41 7216 1026 #text: 34 1 8 'Stone Host (from Gem):' 7216 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 34 0 0 0 155 0 0 0 54 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 301 1312 11 1328 41 410 ##(Smalltalk.ContainerView) 34 15 nil 6032 34 2 8 1140850688 393217 7536 nil 480 nil 7 nil nil nil 7536 656390 ##(Smalltalk.GridLayout) 3 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 7536 34 2 8 1140924425 1 7664 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 4786 #= 8 #() 4786 #hash 8 #() false 480 nil 7 nil nil nil 7664 nil 8 1717867584 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 1 1 514 331 45 7664 1026 #text: 34 1 8 'RPC Gem' 7664 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 165 0 0 0 22 0 0 0] 8 #() 1216 nil 27 8 'remote' 410 ##(Smalltalk.RadioButton) 34 16 nil 7536 34 2 8 1141055497 1 8112 7730 nil nil 7762 4786 #= 8 #() 4786 #hash 8 #() false 480 nil 7 nil nil nil 8112 nil 8 1717867584 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 331 1 514 331 45 8112 1026 #text: 34 1 8 'Linked Gem (32-bit only)' 8112 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 74 1 0 0 22 0 0 0] 8 #() 1216 nil 27 8 'linked' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 11 257 514 665 49 7536 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 128 0 0 0 81 1 0 0 152 0 0 0] 34 2 7664 8112 1216 nil 27 1234 1280 11 1680 -9 1312 5 1328 49 410 ##(Smalltalk.TextEdit) 34 16 nil 6032 34 2 8 1140916352 1025 8704 nil 480 nil 7 nil nil nil 8704 nil 8 1717706240 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 491 163 514 185 45 8704 1026 #setMarginWidths: 34 1 8 #(3 3) 8704 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 81 0 0 0 81 1 0 0 103 0 0 0] 8 #() 1216 nil 27 1234 1664 11 1680 -9 1696 1 1328 45 410 ##(Smalltalk.ContainerView) 34 15 nil 6032 34 2 8 1409286144 393217 9024 nil 480 nil 7 nil nil nil 9024 7602 1 3 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 9024 34 2 8 1141055497 1 9136 7730 nil nil 7762 4786 #= 8 #() 4786 #hash 8 #() false 480 nil 7 nil nil nil 9136 nil 8 1717867584 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 1 1 514 297 39 9136 1026 #text: 34 1 8 'NetLDI Guest' 9136 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 148 0 0 0 19 0 0 0] 8 #() 1216 nil 27 8 'guest' 410 ##(Smalltalk.RadioButton) 34 16 nil 9024 34 2 8 1140924425 1 9552 7730 nil nil 7762 4786 #= 8 #() 4786 #hash 8 #() false 480 nil 7 nil nil nil 9552 nil 8 1717867584 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 1 39 514 297 39 9552 1026 #text: 34 1 8 'Native OS User' 9552 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 148 0 0 0 38 0 0 0] 8 #() 1216 nil 27 8 'member' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 11 163 514 301 81 9024 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 81 0 0 0 155 0 0 0 121 0 0 0] 34 2 9136 9552 1216 nil 27 1234 1280 11 1296 301 1312 5 1328 81 410 ##(Smalltalk.PushButton) 34 20 nil 6032 34 2 8 1140924416 1 10144 nil nil nil 7 nil nil nil 10144 nil 8 1717867584 6546 #debugGCI 8 'Debug GCI' 1 1 nil nil false nil nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 11 5 514 141 51 10144 1026 #isEnabled: 8 #(false) 10144 1026 #text: 34 1 8 'Debug GCI' 10144 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 75 0 0 0 27 0 0 0] 8 #() 1216 nil 29 1234 1280 11 1296 141 1312 5 1328 51 410 ##(Smalltalk.StaticText) 34 16 nil 6032 34 2 8 1140850944 1 10512 nil nil nil 7 nil nil nil 10512 nil 8 1717865120 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 321 163 514 161 41 10512 1026 #text: 34 1 8 'OS User ID:' 10512 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 81 0 0 0 240 0 0 0 101 0 0 0] 8 #() 1216 nil 27 1234 1664 11 1296 161 1696 1 1328 41 410 ##(Smalltalk.TextEdit) 34 16 nil 6032 34 2 8 1140916384 1025 10832 nil 480 nil 7 nil nil nil 10832 nil 8 1717706240 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 491 209 514 185 45 10832 1026 #setMarginWidths: 34 1 8 #(3 3) 10832 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 104 0 0 0 81 1 0 0 126 0 0 0] 8 #() 1216 nil 27 1234 1664 11 1680 -9 1696 1 1328 45 410 ##(Smalltalk.StaticPath) 34 17 nil 6032 34 2 8 1140850944 1 11152 nil nil nil 7 nil nil nil 11152 nil 8 1717865120 930 nil nil nil 8 'XXX' 962 138 144 34 2 1026 #createAt:extent: 34 2 514 161 15 514 515 45 11152 1026 #text: 34 1 11248 11152 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 7 0 0 0 81 1 0 0 29 0 0 0] 8 #() 1216 nil 27 1234 1280 161 1680 -9 1696 11 1328 45 410 ##(Smalltalk.TextEdit) 34 16 nil 6032 34 2 8 1140916352 1025 11472 nil 480 nil 7 nil nil nil 11472 nil 8 1717706240 930 nil nil 1 962 138 144 34 3 1026 #createAt:extent: 34 2 514 321 69 514 355 45 11472 1026 #text: 34 1 8 'localhost' 11472 1026 #setMarginWidths: 34 1 8 #(3 3) 11472 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 34 0 0 0 81 1 0 0 56 0 0 0] 8 #() 1216 nil 27 1234 1664 11 1680 -9 1696 1 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 6032 34 2 8 1140850944 1 11840 nil nil nil 7 nil nil nil 11840 nil 8 1717865120 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 321 209 514 161 41 11840 1026 #text: 34 1 8 'Password:' 11840 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 104 0 0 0 240 0 0 0 124 0 0 0] 8 #() 1216 nil 27 1234 1266 #fixedPreviousLeft -169 1296 161 1312 3 1328 41 170 192 34 14 6848 8 'gemTask' 9024 8 'loginType' 11472 8 'stoneHost' 8704 8 'hostUserID' 11152 8 'debugPath' 10832 8 'hostPassword' 7536 8 'gemType' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 685 1 514 685 367 6032 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 86 1 0 0 0 0 0 0 172 2 0 0 183 0 0 0] 34 13 10144 11152 7216 11472 6144 6848 9024 10512 8704 11840 10832 7536 6464 1216 nil 27 1234 5968 5986 8 0 0 0 0 0 0 224 63 1680 1 3312 1 6016 1 170 192 34 4 736 8 'Basic' 6032 8 'Advanced' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 1 357 514 1369 367 608 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 178 0 0 0 172 2 0 0 105 1 0 0] 34 2 736 6032 1216 nil 27 41 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 12736 nil nil nil 7 nil nil nil 12736 674 170 176 34 10 410 ##(Smalltalk.Toolbar) 34 25 nil 12736 34 2 8 1140853580 131137 12848 nil 480 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 12848 480 8 1717808416 170 192 4720 170 192 34 4 10673 1246790 1 ##(Smalltalk.ToolbarSystemButton) 12848 1 6546 #getHelpOn 8 'Get help on widget' 1 1 nil 10673 1 23 10675 1246790 1 ##(Smalltalk.ToolbarBitmapButton) 12848 1 6546 #openHelp 8 'Jade Help' 1 1 nil 10675 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 514 1857 33 71 34 2 13088 13152 nil nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 241 1 514 1129 51 12848 1026 #updateSizePosted 4720 12848 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 120 0 0 0 0 0 0 0 172 2 0 0 25 0 0 0] 8 #() 1216 nil 27 1234 1664 1 1680 1 3312 1 1328 51 410 ##(Smalltalk.Toolbar) 34 25 nil 12736 34 2 8 1409289036 131137 13616 nil 480 nil 7 nil nil nil 13616 480 8 1717808416 170 192 4720 170 192 34 10 10671 1115206 1 ##(Smalltalk.ToolbarIconButton) 13616 1 6546 #editUndo 8 'Undo' 1 1 263494 3 ##(Smalltalk.Icon) nil true 13248 8 'icons\EditUndo.ico' nil 10671 10663 13746 13616 1 6546 #editCut 8 'Cut' 1 1 13810 nil true 13248 8 'icons\EditCut.ico' nil 10663 10665 13746 13616 1 6546 #editCopy 8 'Copy' 1 1 13810 nil true 13248 8 'icons\EditCopy.ico' nil 10665 10667 13746 13616 1 6546 #editPaste 8 'Paste' 1 1 13810 nil true 13248 8 'icons\EditPasteLink.ico' nil 10667 10669 13746 13616 1 6546 #editDelete 8 'Delete' 1 1 13810 nil true 13248 8 'icons\EditDelete.ico' nil 10669 34 6 13856 13936 14016 14096 13760 1049158 1 ##(Smalltalk.ToolbarSeparator) 13616 1 nil nil 1 nil 514 33 33 514 45 45 nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 1 1 514 241 51 13616 1026 #updateSizePosted 4720 13616 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 120 0 0 0 25 0 0 0] 8 #() 1216 nil 27 1234 1280 1 1296 241 3312 1 1328 51 410 ##(Smalltalk.PushButton) 34 20 nil 12736 34 2 8 1140924416 1 14448 nil nil nil 7 nil nil nil 14448 nil 8 1717867584 6546 #addLogin 8 '+' 1 1 nil nil false nil nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 1319 61 514 51 51 14448 1026 #isEnabled: 8 #(false) 14448 1026 #text: 34 1 8 '+' 14448 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 30 0 0 0 172 2 0 0 55 0 0 0] 8 #() 1216 nil 29 1234 1680 -49 1296 51 3312 61 1328 51 410 ##(Smalltalk.PushButton) 34 20 nil 12736 34 2 8 1140924416 1 14816 nil nil nil 7 nil nil nil 14816 nil 8 1717867584 6546 #removeLogin 8 '-' 1 1 nil nil false nil nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 1319 121 514 51 51 14816 1026 #isEnabled: 8 #(false) 14816 1026 #text: 34 1 8 '-' 14816 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 60 0 0 0 172 2 0 0 85 0 0 0] 8 #() 1216 nil 29 1234 1680 -49 1296 51 1312 11 1328 51 410 ##(Smalltalk.ListView) 34 30 nil 12736 34 2 8 1409355853 1025 15184 4674 138 144 4720 nil 4752 480 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 4 984134 2 ##(Smalltalk.CommandMenuItem) 1 6546 #copyXML 8 '&Copy XML' 1 1 nil nil nil 15330 1 6546 #addLogin 8 '&Add' 1 1 nil nil nil 15330 1 6546 #removeLogin 8 '&Remove' 1 1 nil nil nil 15330 1 6546 #login 8 '&Login' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 15184 nil 8 1717758064 4786 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 6 920646 5 ##(Smalltalk.ListViewColumn) 8 'Version' 241 #left 4786 #gemVersion 8 #() 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a gemVersion <= b gemVersion]' 8 #[30 105 226 0 226 32 130 106] #gemVersion 15760 7 513 nil nil nil 15184 nil 1 nil nil 15666 8 'Gem Host' 201 #left 4786 #gemHost 15728 15746 0 nil 15778 2 1 4784 8 'doIt' 8 '[:a :b | a gemHost <= b gemHost]' 8 #[30 105 226 0 226 32 130 106] #gemHost 15952 7 513 nil nil nil 15184 nil 1 nil nil 15666 8 'NetLDI' 201 #left 4786 #gemService 15728 15746 0 nil 15778 2 1 4784 8 'doIt' 8 '[:a :b | a gemService <= b gemService]' 8 #[30 105 226 0 226 32 130 106] #gemService 16080 7 513 nil nil nil 15184 nil 1 nil nil 15666 8 'Stone' 201 #left 4786 #stoneName 15728 15746 0 nil 15778 2 1 4784 8 'doIt' 8 '[:a :b | a stoneName <= b stoneName]' 8 #[30 105 226 0 226 32 130 106] #stoneName 16208 7 513 nil nil nil 15184 nil 1 nil nil 15666 8 'User' 201 #left 4786 #gsUserID 15728 15746 0 nil 15778 2 1 4784 8 'doIt' 8 '[:a :b | a gsUserID <= b gsUserID]' 8 #[30 105 226 0 226 32 130 106] #gsUserID 16336 7 513 nil nil nil 15184 nil 1 nil nil 15666 8 'Last Login' 441 #left 4786 #lastLoginString 8 #() 15746 0 nil 15778 2 1 4784 8 'doIt' 8 '[:a :b | a lastLogin <= b lastLogin]' 8 #[30 105 226 0 226 32 130 106] #lastLogin 16480 7 513 nil nil nil 15184 nil 1 nil nil #report 4720 nil 131169 nil 34 4 nil nil 514 1 1 nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 1 51 514 1319 297 15184 1026 #contextMenu: 34 1 15296 15184 1026 #text: 34 1 8 'Version' 15184 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 147 2 0 0 173 0 0 0] 8 #() 1216 nil 27 1234 1280 1 1680 -49 1312 1 6016 1 170 192 34 6 12848 8 'Other Tools' 13616 8 'Edit Tools' 15184 8 'loginList' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 1 1 514 1369 347 12736 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 173 0 0 0] 34 5 13616 12848 15184 14448 14816 1216 nil 27 39 true 170 192 34 4 608 8 'bottom' 12736 8 'top' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 15282 nil true 34 6 15330 1 6546 #addLogin 8 '&New' 9373 1 nil nil nil 15330 1 6546 #predefinedLogin 8 '&Predefined Login' 1 1 nil nil nil 15330 1 6546 #fileOpen 8 '&Open...' 9375 1 nil nil nil 15330 1 6546 #removeLogin 8 '&Remove' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 15330 1 6546 #exit 8 'E&xit' 17639 1 nil nil nil 8 '&Login' nil 134217729 nil nil 10733 nil nil 15282 nil true 34 9 15330 1 6546 #editUndo 8 '&Undo' 9397 1 nil nil nil 15330 1 6546 #editRedo 8 '&Redo' 9395 1 nil nil nil 17426 4097 15330 1 6546 #editCut 8 'Cu&t' 9393 1 nil nil nil 15330 1 6546 #editCopy 8 '&Copy' 9351 1 nil nil nil 15330 1 6546 #editPaste 8 '&Paste' 9389 1 nil nil nil 15330 1 6546 #editDelete 8 'De&lete' 1629 1 nil nil nil 17426 4097 15330 1 6546 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '&Edit' nil 134217729 nil nil 10749 nil nil 15282 nil true 34 7 15330 1 6546 #openGemTalkHomePage 8 '&GemTalk Home Page' 1025 1 nil nil nil 15330 1 6546 #openJadeHomePage 8 '&Jade Home Page' 1 1 nil nil nil 17426 4097 15330 1 6546 #showGemNRS 8 'Show Gem &NRS' 1 1 nil nil nil 15330 1 6546 #showStoneNRS 8 'Show Stone N&RS' 1 1 nil nil nil 15330 1 6546 #openExeDir 8 'Open Executable &Directory' 1 1 nil nil nil 15330 1 6546 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 10763 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 9517 13810 nil true 13248 8 'icons\GS32.ico' nil 13810 nil true 13248 8 'icons\GS16.ico' nil nil nil 1 nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 5119 21 514 1401 841 416 1026 #text: 34 1 8 'Jade Login' 416 1026 #updateMenuBar 4720 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 187 12 0 0 174 1 0 0] 34 3 12736 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 18592 nil 480 nil 519 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 18592 nil 1 #left nil nil nil 514 1 1 514 9 9 nil 18688 nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 1 347 514 1369 11 18592 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 173 0 0 0 172 2 0 0 178 0 0 0] 8 #() 1216 nil 27 608 1216 nil 27 )! !
!JadeLoginShell class categoriesFor: #icon!constants!public! !
!JadeLoginShell class categoriesFor: #resource_Default_view!public!resources-views! !

