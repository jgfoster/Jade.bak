"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #JadeLoginShell
	instanceVariableNames: 'activeLibraryPresenter activityListPresenter debugPathPresenter gemHostPresenter gemServicePresenter gemTaskPresenter gemTypePresenter hostPasswordPresenter hostUserIDPresenter initialsPresenter inUpdateView loginListPresenter loginTypePresenter passwordPresenter stoneHostPresenter stoneNamePresenter userIDPresenter versionListPresenter'
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
	versionListPresenter	:= self add: ListPresenter				new name: 'versionList'.
	stoneHostPresenter 	:= self add: TextPresenter 			new name: 'stoneHost'.
	stoneNamePresenter 	:= self add: TextPresenter 			new name: 'stoneName'.
	gemTypePresenter 		:= self add: RadioButtonSetPresenter 	new name: 'gemType'.
	gemHostPresenter 		:= self add: TextPresenter 			new name: 'gemHost'.
	gemServicePresenter 	:= self add: TextPresenter 			new name: 'gemService'.
	gemTaskPresenter 		:= self add: TextPresenter 			new name: 'gemTask'.
	userIDPresenter 		:= self add: TextPresenter 			new name: 'userID'.
	passwordPresenter 	:= self add: TextPresenter 			new name: 'password'.
	loginListPresenter		:= self add: ListPresenter				new name: 'loginList'.
	loginTypePresenter 	:= self add: RadioButtonSetPresenter 	new name: 'loginType'.
	hostUserIDPresenter	:= self add: TextPresenter			new name: 'hostUserID'.
	hostPasswordPresenter	:= self add: TextPresenter			new name: 'hostPassword'.
	initialsPresenter		:= self add: TextPresenter			new name: 'initials'.
	debugPathPresenter	:= self add: TextPresenter			new name: 'debugPath'.

	activeLibraryPresenter	:= self add: TextPresenter			new name: 'activeLibrary'.
	activityListPresenter	:= self add: ListPresenter				new name: 'activityList'.

!

createSchematicWiring

	super createSchematicWiring.
	loginListPresenter	
		when: #'selectionChanged'	send: #'selectionChanged' 	to: self;
		when: #'actionPerformed'	send: #'openLogin'			to: self;
		yourself.
	versionListPresenter	when: #selectionChanged 	send: #modified 	to: self.
	stoneHostPresenter 	when: #valueChanged 		send: #modified 	to: self.
	stoneNamePresenter 	when: #valueChanged 		send: #modified 	to: self.
	gemTypePresenter		when: #valueChanged 		send: #modified 	to: self.
	gemHostPresenter 		when: #valueChanged 		send: #modified 	to: self.
	gemServicePresenter 	when: #valueChanged 		send: #modified 	to: self.
	gemTaskPresenter 		when: #valueChanged 		send: #modified 	to: self.
	userIDPresenter 		when: #valueChanged 		send: #modified 	to: self.
	passwordPresenter 	when: #valueChanged 		send: #modified 	to: self.
	loginTypePresenter		when: #valueChanged 		send: #modified 	to: self.
	hostUserIDPresenter	when: #valueChanged 		send: #modified 	to: self.
	hostPasswordPresenter	when: #valueChanged 		send: #modified 	to: self.
	initialsPresenter		when: #valueChanged		send: #modified 	to: self.
	debugPathPresenter	when: #valueChanged		send: #modified 	to: self.
	self					when: #closeRequested:		send: #saveData	to: self.
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
		| path tempPath fileStream |
		path := self dataPath.
		tempPath := self tempDataPath.
		((File exists: path) not and: [File exists: tempPath]) ifTrue: [
			File rename: tempPath to: path.
		].
		fileStream := FileStream read: path type: #'binary'.
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

	JadeLog log: 'JadeLoginShell>>onCloseRequested at ' , DateAndTime now printString.
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

openConsole

	KernelLibrary default 
		allocConsole; 
		setConsoleTitle: 'Jade Console';
		setConsoleOutputCP: 65001; "CP_UTF8"
		setConsoleCP: 65001; "CP_UTF8"
		yourself.
!

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
		| path tempPath |
		path := self dataPath.
		tempPath := self tempDataPath.
		fileStream := FileStream 
			write: tempPath
			mode: #'truncate' 
			check: false 
			type: #'binary'.
		data binaryStoreOn: fileStream.
		fileStream close.
		(File exists: path) ifTrue: [File delete: path].
		File rename: tempPath to: path.
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

tempDataPath

	^self dataPath , '.new'
!

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
!JadeLoginShell categoriesFor: #dataPath!public!user data! !
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
!JadeLoginShell categoriesFor: #loadData!public!user data! !
!JadeLoginShell categoriesFor: #login!public! !
!JadeLoginShell categoriesFor: #logout:!public! !
!JadeLoginShell categoriesFor: #modified!public! !
!JadeLoginShell categoriesFor: #onCloseRequested!public! !
!JadeLoginShell categoriesFor: #onViewOpened!public! !
!JadeLoginShell categoriesFor: #openConsole!commands!public! !
!JadeLoginShell categoriesFor: #openExeDir!commands!public! !
!JadeLoginShell categoriesFor: #openGemTalkHomePage!public! !
!JadeLoginShell categoriesFor: #openJadeHomePage!public! !
!JadeLoginShell categoriesFor: #openLogin!public! !
!JadeLoginShell categoriesFor: #postLogin:!public! !
!JadeLoginShell categoriesFor: #predefinedLogin!commands!public! !
!JadeLoginShell categoriesFor: #queryCommand:!public! !
!JadeLoginShell categoriesFor: #removeLogin!commands!public! !
!JadeLoginShell categoriesFor: #saveData!public!user data! !
!JadeLoginShell categoriesFor: #selectionChanged!public! !
!JadeLoginShell categoriesFor: #sendRequiredMessages!public! !
!JadeLoginShell categoriesFor: #setWorkingDirectory!public! !
!JadeLoginShell categoriesFor: #showGemNRS!commands!public! !
!JadeLoginShell categoriesFor: #showStoneNRS!commands!public! !
!JadeLoginShell categoriesFor: #tempDataPath!public!user data! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13238272 65536) 416 nil 327686 ##(Smalltalk.Color) #default 328198 ##(Smalltalk.Point) 1421 901 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 576 nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 265030 4 ##(Smalltalk.Menu) nil true 34 7 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #openConsole 8 'Open &Console' 9369 1 nil nil nil 706 1 738 #addLogin 8 '&New' 9373 1 nil nil nil 706 1 738 #predefinedLogin 8 '&Predefined Login' 1 1 nil nil nil 706 1 738 #fileOpen 8 '&Open...' 9375 1 nil nil nil 706 1 738 #removeLogin 8 '&Remove' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 706 1 738 #exit 8 'E&xit' 17639 1 nil nil nil 8 '&File' nil 134217729 nil nil 11241 nil nil 658 nil true 34 9 706 1 738 #editUndo 8 '&Undo' 9397 1 nil nil nil 706 1 738 #editRedo 8 '&Redo' 9395 1 nil nil nil 978 4097 706 1 738 #editCut 8 'Cu&t' 9393 1 nil nil nil 706 1 738 #editCopy 8 '&Copy' 9351 1 nil nil nil 706 1 738 #editPaste 8 '&Paste' 9389 1 nil nil nil 706 1 738 #editDelete 8 'De&lete' 1629 1 nil nil nil 978 4097 706 1 738 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '&Edit' nil 134217729 nil nil 11257 nil nil 658 nil true 34 7 706 1 738 #openGemTalkHomePage 8 '&GemTalk Home Page' 1025 1 nil nil nil 706 1 738 #openJadeHomePage 8 '&Jade Home Page' 1 1 nil nil nil 978 4097 706 1 738 #showGemNRS 8 'Show Gem &NRS' 1 1 nil nil nil 706 1 738 #showStoneNRS 8 'Show Stone N&RS' 1 1 nil nil nil 706 1 738 #openExeDir 8 'Open Executable &Directory' 1 1 nil nil nil 706 1 738 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 11271 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 9879 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'icons\GS32.ico' nil 1858 nil true 1904 8 'icons\GS16.ico' nil nil nil 1 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 498 5119 21 498 1421 901 416 2034 #text: 34 1 8 'Jade Login' 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 197 12 0 0 204 1 0 0] 34 1 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 2224 nil 480 nil 7 nil nil nil 2224 656710 1 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 8 'Logins' 410 ##(Smalltalk.ContainerView) 34 15 nil 2224 34 2 8 1140850688 131073 2400 nil nil nil 5 nil nil nil 2400 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 170 192 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 2400 34 2 8 1140850688 131073 2528 nil nil nil 5 nil nil nil 2528 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 2528 34 2 8 1140850688 131073 2656 nil nil nil 5 nil nil nil 2656 2594 170 176 34 28 410 ##(Smalltalk.TextEdit) 34 16 nil 2656 34 2 8 1140916384 1025 2768 nil 480 nil 5 nil nil nil 2768 nil 8 1943850864 852486 ##(Smalltalk.NullConverter) nil nil 1 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 391 259 498 285 45 2768 2034 #setMarginWidths: 34 1 8 #(3 3) 2768 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 129 0 0 0 81 1 0 0 151 0 0 0] 8 #() 498 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedPreviousRight 11 3138 #fixedParentRight -9 3138 #fixedPreviousTop 1 3138 #fixedViewTop 45 410 ##(Smalltalk.StaticText) 34 16 nil 2656 34 2 8 1140850944 1 3216 nil nil nil 5 nil nil nil 3216 nil 8 1943828432 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 11 113 498 371 31 3216 2034 #text: 34 1 8 'NetLDI Service Name or Port:' 3216 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 56 0 0 0 190 0 0 0 71 0 0 0] 8 #() 3088 nil 27 3106 3138 #fixedParentLeft 11 3138 #fixedViewLeft 371 3138 #fixedPreviousBottom 3 3200 31 410 ##(Smalltalk.TextEdit) 34 16 nil 2656 34 2 8 1140916352 1025 3584 nil 480 nil 5 nil nil nil 3584 nil 8 1943850864 2850 nil nil 1 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 391 213 498 285 45 3584 2034 #setMarginWidths: 34 1 8 #(3 3) 3584 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 106 0 0 0 81 1 0 0 128 0 0 0] 8 #() 3088 nil 27 3106 3152 11 3168 -9 3184 1 3200 45 410 ##(Smalltalk.TextEdit) 34 16 nil 2656 34 2 8 1140916352 1025 3904 nil 480 nil 5 nil nil nil 3904 nil 8 1943850864 2850 nil nil 1 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 391 113 498 285 45 3904 2034 #setMarginWidths: 34 1 8 #(3 3) 3904 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 56 0 0 0 81 1 0 0 78 0 0 0] 8 #() 3088 nil 27 3106 3152 11 3168 -9 3184 1 3200 45 410 ##(Smalltalk.TextEdit) 34 16 nil 2656 34 2 8 1140916352 1025 4224 nil 480 nil 5 nil nil nil 4224 nil 8 1943850864 2850 nil nil 1 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 391 67 498 285 45 4224 2034 #setMarginWidths: 34 1 8 #(3 3) 4224 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 33 0 0 0 81 1 0 0 55 0 0 0] 8 #() 3088 nil 27 3106 3152 11 3168 -9 3184 1 3200 45 410 ##(Smalltalk.StaticText) 34 16 nil 2656 34 2 8 1140850944 1 4544 nil nil nil 5 nil nil nil 4544 nil 8 1943828432 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 11 21 498 371 41 4544 2034 #text: 34 1 8 'GemStone/Smalltalk Version:' 4544 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 190 0 0 0 30 0 0 0] 8 #() 3088 nil 27 3106 3536 11 3552 371 3138 #fixedParentTop 21 3200 41 410 ##(Smalltalk.StaticText) 34 16 nil 2656 34 2 8 1140850944 1 4880 nil nil nil 5 nil nil nil 4880 nil 8 1943828432 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 11 67 498 371 31 4880 2034 #text: 34 1 8 'RPC Gem Host Name or IP:' 4880 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 190 0 0 0 48 0 0 0] 8 #() 3088 nil 27 3106 3536 11 3552 371 3568 11 3200 31 410 ##(Smalltalk.StaticText) 34 16 nil 2656 34 2 8 1140850944 1 5200 nil nil nil 5 nil nil nil 5200 nil 8 1943828432 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 11 259 498 371 41 5200 2034 #text: 34 1 8 'GemStone Password:' 5200 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 129 0 0 0 190 0 0 0 149 0 0 0] 8 #() 3088 nil 27 3106 3536 11 3552 371 3568 3 3200 41 410 ##(Smalltalk.StaticText) 34 16 nil 2656 34 2 8 1140850944 1 5520 nil nil nil 5 nil nil nil 5520 nil 8 1943828432 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 11 167 498 371 39 5520 2034 #text: 34 1 8 'Stone Name:' 5520 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 83 0 0 0 190 0 0 0 102 0 0 0] 8 #() 3088 nil 27 3106 3536 11 3552 371 3568 11 3200 39 410 ##(Smalltalk.StaticText) 34 16 nil 2656 34 2 8 1140850944 1 5840 nil nil nil 5 nil nil nil 5840 nil 8 1943828432 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 11 213 498 371 41 5840 2034 #text: 34 1 8 'GemStone User ID:' 5840 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 106 0 0 0 190 0 0 0 126 0 0 0] 8 #() 3088 nil 27 3106 3536 11 3552 371 3568 3 3200 41 410 ##(Smalltalk.ComboBox) 34 17 nil 2656 34 2 8 1144063491 1025 6160 590662 2 ##(Smalltalk.ListModel) 138 144 576 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 480 nil 5 nil nil nil 6160 nil 8 1943716480 459270 ##(Smalltalk.Message) #displayString 8 #() 576 401 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 391 11 498 285 51 6160 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 5 0 0 0 81 1 0 0 30 0 0 0] 8 #() 3088 nil 27 3106 3152 11 3168 -9 4864 11 3200 47 410 ##(Smalltalk.TextEdit) 34 16 nil 2656 34 2 8 1140916352 1025 6544 nil 480 nil 5 nil nil nil 6544 nil 8 1943850864 2850 nil nil 1 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 391 167 498 285 45 6544 2034 #setMarginWidths: 34 1 8 #(3 3) 6544 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 83 0 0 0 81 1 0 0 105 0 0 0] 8 #() 3088 nil 27 3106 3152 11 3168 -9 3184 1 3200 45 410 ##(Smalltalk.StaticText) 34 16 nil 2656 34 2 8 1140850944 1 6864 nil nil nil 5 nil nil nil 6864 nil 8 1943828432 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 11 313 498 371 41 6864 2034 #text: 34 1 8 'Monticello User Name:' 6864 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 156 0 0 0 190 0 0 0 176 0 0 0] 8 #() 3088 nil 27 3106 3536 11 3552 371 3568 11 3200 41 410 ##(Smalltalk.TextEdit) 34 16 nil 2656 34 2 8 1140916352 1025 7184 nil 480 nil 5 nil nil nil 7184 nil 8 1943850864 2850 nil nil 1 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 391 313 498 285 45 7184 2034 #setMarginWidths: 34 1 8 #(3 3) 7184 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 156 0 0 0 81 1 0 0 178 0 0 0] 8 #() 3088 nil 27 3106 3152 11 3168 -9 3184 1 3200 45 170 192 34 14 6544 8 'stoneName' 2768 8 'password' 6160 8 'versionList' 7184 8 'initials' 3584 8 'userID' 3904 8 'gemService' 4224 8 'gemHost' nil 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 1 1 498 685 687 2656 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 1 0 0 87 1 0 0] 34 14 4544 6160 4880 4224 3216 3904 5520 6544 5840 3584 5200 2768 6864 7184 3088 nil 27 3106 3536 1 3138 #relativeParentWidth 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 4864 1 3138 #fixedParentBottom 1 410 ##(Smalltalk.ContainerView) 34 15 nil 2528 34 2 8 1140850688 131073 7888 nil nil nil 5 nil nil nil 7888 2594 170 176 34 26 410 ##(Smalltalk.StaticText) 34 16 nil 7888 34 2 8 1140850944 1 8000 nil nil nil 5 nil nil nil 8000 nil 8 1943828432 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 11 115 498 301 31 8000 2034 #text: 34 1 8 'NetLDI Task Name:' 8000 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 57 0 0 0 155 0 0 0 72 0 0 0] 8 #() 3088 nil 27 3106 3536 11 3552 301 3568 3 3200 31 410 ##(Smalltalk.StaticText) 34 16 nil 7888 34 2 8 1140850944 1 8320 nil nil nil 5 nil nil nil 8320 nil 8 1943828432 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 321 209 498 161 41 8320 2034 #text: 34 1 8 'Password:' 8320 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 104 0 0 0 240 0 0 0 124 0 0 0] 8 #() 3088 nil 27 3106 3138 #fixedPreviousLeft -169 3552 161 3568 3 3200 41 410 ##(Smalltalk.TextEdit) 34 16 nil 7888 34 2 8 1140916384 1025 8656 nil 480 nil 5 nil nil nil 8656 nil 8 1943850864 2850 nil nil 1 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 491 209 498 185 45 8656 2034 #setMarginWidths: 34 1 8 #(3 3) 8656 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 104 0 0 0 81 1 0 0 126 0 0 0] 8 #() 3088 nil 27 3106 3152 11 3168 -9 3184 1 3200 45 410 ##(Smalltalk.ContainerView) 34 15 nil 7888 34 2 8 1140850688 393217 8976 nil 480 nil 5 nil nil nil 8976 656390 ##(Smalltalk.GridLayout) 3 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 8976 34 2 8 1140924425 1 9104 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 6322 #= 8 #() 6322 #hash 8 #() false 480 nil 5 nil nil nil 9104 nil 8 1943863552 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 1 1 498 331 45 9104 2034 #text: 34 1 8 'RPC Gem' 9104 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 165 0 0 0 22 0 0 0] 8 #() 3088 nil 27 8 'remote' 410 ##(Smalltalk.RadioButton) 34 16 nil 8976 34 2 8 1141055497 1 9552 9170 nil nil 9202 6322 #= 8 #() 6322 #hash 8 #() false 480 nil 5 nil nil nil 9552 nil 8 1943863552 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 331 1 498 331 45 9552 2034 #text: 34 1 8 'Linked Gem (32-bit only)' 9552 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 74 1 0 0 22 0 0 0] 8 #() 3088 nil 27 8 'linked' nil 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 11 257 498 665 49 8976 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 128 0 0 0 81 1 0 0 152 0 0 0] 34 2 9104 9552 3088 nil 27 3106 3536 11 3168 -9 3568 5 3200 49 410 ##(Smalltalk.StaticText) 34 16 nil 7888 34 2 8 1140850944 1 10144 nil nil nil 5 nil nil nil 10144 nil 8 1943828432 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 11 69 498 301 41 10144 2034 #text: 34 1 8 'Stone Host (from Gem):' 10144 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 34 0 0 0 155 0 0 0 54 0 0 0] 8 #() 3088 nil 27 3106 3536 11 3552 301 3568 11 3200 41 410 ##(Smalltalk.ContainerView) 34 15 nil 7888 34 2 8 1409286144 393217 10464 nil 480 nil 5 nil nil nil 10464 9042 1 3 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 10464 34 2 8 1141055497 1 10576 9170 nil nil 9202 6322 #= 8 #() 6322 #hash 8 #() false 480 nil 5 nil nil nil 10576 nil 8 1943863552 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 1 1 498 297 39 10576 2034 #text: 34 1 8 'NetLDI Guest' 10576 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 148 0 0 0 19 0 0 0] 8 #() 3088 nil 27 8 'guest' 410 ##(Smalltalk.RadioButton) 34 16 nil 10464 34 2 8 1140924425 1 10992 9170 nil nil 9202 6322 #= 8 #() 6322 #hash 8 #() false 480 nil 5 nil nil nil 10992 nil 8 1943863552 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 1 39 498 297 39 10992 2034 #text: 34 1 8 'Native OS User' 10992 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 148 0 0 0 38 0 0 0] 8 #() 3088 nil 27 8 'member' nil 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 11 163 498 301 81 10464 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 81 0 0 0 155 0 0 0 121 0 0 0] 34 2 10576 10992 3088 nil 27 3106 3536 11 3552 301 3568 5 3200 81 410 ##(Smalltalk.TextEdit) 34 16 nil 7888 34 2 8 1140916352 1025 11584 nil 480 nil 5 nil nil nil 11584 nil 8 1943850864 2850 nil nil 1 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 491 163 498 185 45 11584 2034 #setMarginWidths: 34 1 8 #(3 3) 11584 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 81 0 0 0 81 1 0 0 103 0 0 0] 8 #() 3088 nil 27 3106 3152 11 3168 -9 3184 1 3200 45 410 ##(Smalltalk.TextEdit) 34 16 nil 7888 34 2 8 1140916352 1025 11904 nil 480 nil 5 nil nil nil 11904 nil 8 1943850864 2850 nil nil 1 1970 138 144 34 3 2034 #createAt:extent: 34 2 498 321 69 498 355 45 11904 2034 #text: 34 1 8 'localhost' 11904 2034 #setMarginWidths: 34 1 8 #(3 3) 11904 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 34 0 0 0 81 1 0 0 56 0 0 0] 8 #() 3088 nil 27 3106 3152 11 3168 -9 3184 1 3200 45 410 ##(Smalltalk.PushButton) 34 20 nil 7888 34 2 8 1140924416 1 12272 nil nil nil 5 nil nil nil 12272 nil 8 1943863552 738 #debugGCI 8 'Debug GCI' 1 1 nil nil false nil nil nil 1970 138 144 34 3 2034 #createAt:extent: 34 2 498 11 5 498 141 51 12272 2034 #isEnabled: 8 #(false) 12272 2034 #text: 34 1 8 'Debug GCI' 12272 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 75 0 0 0 27 0 0 0] 8 #() 3088 nil 29 3106 3536 11 3552 141 3568 5 3200 51 410 ##(Smalltalk.PushButton) 34 20 nil 7888 34 2 8 1140924416 1 12640 nil nil nil 5 nil nil nil 12640 nil 8 1943863552 738 #login 8 'Login' 1 1 nil nil true nil nil nil 1970 138 144 34 3 2034 #createAt:extent: 34 2 498 11 305 498 665 61 12640 2034 #isEnabled: 8 #(false) 12640 2034 #text: 34 1 8 'Login' 12640 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 152 0 0 0 81 1 0 0 182 0 0 0] 8 #() 3088 nil 29 3106 3536 11 3168 -9 3568 1 3200 61 410 ##(Smalltalk.StaticPath) 34 17 nil 7888 34 2 8 1140850944 1 13008 nil nil nil 5 nil nil nil 13008 nil 8 1943828432 2850 nil nil nil 8 'XXX' 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 161 15 498 515 45 13008 2034 #text: 34 1 13104 13008 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 7 0 0 0 81 1 0 0 29 0 0 0] 8 #() 3088 nil 27 3106 3536 161 3168 -9 3184 11 3200 45 410 ##(Smalltalk.TextEdit) 34 16 nil 7888 34 2 8 1140916352 1025 13328 nil 480 nil 5 nil nil nil 13328 nil 8 1943850864 2850 nil nil 1 1970 138 144 34 3 2034 #createAt:extent: 34 2 498 321 115 498 355 45 13328 2034 #text: 34 1 8 'gemnetobject' 13328 2034 #setMarginWidths: 34 1 8 #(3 3) 13328 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 57 0 0 0 81 1 0 0 79 0 0 0] 8 #() 3088 nil 27 3106 3152 11 3168 -9 3184 1 3200 45 410 ##(Smalltalk.StaticText) 34 16 nil 7888 34 2 8 1140850944 1 13696 nil nil nil 5 nil nil nil 13696 nil 8 1943828432 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 321 163 498 161 41 13696 2034 #text: 34 1 8 'OS User ID:' 13696 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 81 0 0 0 240 0 0 0 101 0 0 0] 8 #() 3088 nil 27 3106 3152 11 3552 161 3184 1 3200 41 170 192 34 14 13328 8 'gemTask' 11584 8 'hostUserID' 13008 8 'debugPath' 11904 8 'stoneHost' 10464 8 'loginType' 8976 8 'gemType' 8656 8 'hostPassword' nil 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 685 1 498 685 687 7888 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 86 1 0 0 0 0 0 0 172 2 0 0 87 1 0 0] 34 13 12272 13008 10144 11904 8000 13328 10464 13696 11584 8320 8656 8976 12640 3088 nil 27 3106 7824 7842 8 0 0 0 0 0 0 224 63 3168 1 4864 1 7872 1 170 192 34 4 2656 8 'Basic' 7888 8 'Advanced' nil 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 1 349 498 1369 687 2528 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 174 0 0 0 172 2 0 0 5 2 0 0] 34 2 2656 7888 3088 nil 27 8 'bottom' 410 ##(Smalltalk.ContainerView) 34 15 nil 2400 34 2 8 1140850688 131073 14592 nil nil nil 5 nil nil nil 14592 2594 170 176 34 10 410 ##(Smalltalk.Toolbar) 34 25 nil 14592 34 2 8 1140853580 131137 14704 nil 480 nil 517 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 498 193 193 nil 14704 480 8 1943876592 170 192 576 170 192 34 4 11293 1246790 1 ##(Smalltalk.ToolbarSystemButton) 14704 1 738 #getHelpOn 8 'Get help on widget' 1 1 nil 11293 1 23 11295 1246790 1 ##(Smalltalk.ToolbarBitmapButton) 14704 1 738 #openHelp 8 'Jade Help' 1 1 nil 11295 395334 3 ##(Smalltalk.Bitmap) nil true 1904 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 498 1857 33 71 34 2 14944 15008 nil nil 1 nil 498 33 33 498 45 45 nil 2466 1 1 1 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 241 1 498 1129 51 14704 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 120 0 0 0 0 0 0 0 172 2 0 0 25 0 0 0] 8 #() 3088 nil 27 3106 3152 1 3168 1 4864 1 3200 51 410 ##(Smalltalk.PushButton) 34 20 nil 14592 34 2 8 1140924416 1 15408 nil nil nil 5 nil nil nil 15408 nil 8 1943863552 738 #addLogin 8 '+' 1 1 nil nil false nil nil nil 1970 138 144 34 3 2034 #createAt:extent: 34 2 498 1319 61 498 51 51 15408 2034 #isEnabled: 8 #(false) 15408 2034 #text: 34 1 8 '+' 15408 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 30 0 0 0 172 2 0 0 55 0 0 0] 8 #() 3088 nil 29 3106 3168 -49 3552 51 4864 61 3200 51 410 ##(Smalltalk.Toolbar) 34 25 nil 14592 34 2 8 1409289036 131137 15776 nil 480 nil 5 nil nil nil 15776 480 8 1943876592 170 192 576 170 192 34 10 11289 1115206 1 ##(Smalltalk.ToolbarIconButton) 15776 1 738 #editDelete 8 'Delete' 1 1 1858 nil true 1904 8 'icons\EditDelete.ico' nil 11289 11285 15906 15776 1 738 #editCopy 8 'Copy' 1 1 1858 nil true 1904 8 'icons\EditCopy.ico' nil 11285 11291 15906 15776 1 738 #editUndo 8 'Undo' 1 1 1858 nil true 1904 8 'icons\EditUndo.ico' nil 11291 11287 15906 15776 1 738 #editPaste 8 'Paste' 1 1 1858 nil true 1904 8 'icons\EditPasteLink.ico' nil 11287 11283 15906 15776 1 738 #editCut 8 'Cut' 1 1 1858 nil true 1904 8 'icons\EditCut.ico' nil 11283 34 6 16240 16000 16160 15920 16080 1049158 1 ##(Smalltalk.ToolbarSeparator) 15776 1 nil nil 1 nil 498 33 33 498 45 45 nil nil 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 1 1 498 241 51 15776 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 120 0 0 0 25 0 0 0] 8 #() 3088 nil 27 3106 3536 1 3552 241 4864 1 3200 51 410 ##(Smalltalk.PushButton) 34 20 nil 14592 34 2 8 1140924416 1 16576 nil nil nil 5 nil nil nil 16576 nil 8 1943863552 738 #removeLogin 8 '-' 1 1 nil nil false nil nil nil 1970 138 144 34 3 2034 #createAt:extent: 34 2 498 1319 121 498 51 51 16576 2034 #isEnabled: 8 #(false) 16576 2034 #text: 34 1 8 '-' 16576 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 60 0 0 0 172 2 0 0 85 0 0 0] 8 #() 3088 nil 29 3106 3168 -49 3552 51 3568 11 3200 51 410 ##(Smalltalk.ListView) 34 45 nil 14592 34 2 8 1409355853 1025 16944 6226 138 144 576 nil 6288 480 nil 5 658 nil true 34 4 706 1 738 #copyXML 8 '&Copy XML' 1 1 nil nil nil 706 1 738 #addLogin 8 '&Add' 1 1 nil nil nil 706 1 738 #removeLogin 8 '&Remove' 1 1 nil nil nil 706 1 738 #login 8 '&Login' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 16944 nil 8 1943883040 6322 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 6 920646 5 ##(Smalltalk.ListViewColumn) 8 'Version' 241 #left 6322 #gemVersion 8 #() 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a gemVersion <= b gemVersion]' 8 #[30 105 226 0 226 32 130 106] #gemVersion 17488 7 513 nil nil nil 16944 nil 1 nil nil 17394 8 'Gem Host' 201 #left 6322 #gemHost 17456 17474 0 nil 17506 2 1 6320 8 'doIt' 8 '[:a :b | a gemHost <= b gemHost]' 8 #[30 105 226 0 226 32 130 106] #gemHost 17680 7 513 nil nil nil 16944 nil 1 nil nil 17394 8 'NetLDI' 201 #left 6322 #gemService 17456 17474 0 nil 17506 2 1 6320 8 'doIt' 8 '[:a :b | a gemService <= b gemService]' 8 #[30 105 226 0 226 32 130 106] #gemService 17808 7 513 nil nil nil 16944 nil 1 nil nil 17394 8 'Stone' 201 #left 6322 #stoneName 17456 17474 0 nil 17506 2 1 6320 8 'doIt' 8 '[:a :b | a stoneName <= b stoneName]' 8 #[30 105 226 0 226 32 130 106] #stoneName 17936 7 513 nil nil nil 16944 nil 1 nil nil 17394 8 'User' 201 #left 6322 #gsUserID 17456 17474 0 nil 17506 2 1 6320 8 'doIt' 8 '[:a :b | a gsUserID <= b gsUserID]' 8 #[30 105 226 0 226 32 130 106] #gsUserID 18064 7 513 nil nil nil 16944 nil 1 nil nil 17394 8 'Last Login' 441 #left 6322 #lastLoginString 8 #() 17474 0 nil 17506 2 1 6320 8 'doIt' 8 '[:a :b | a lastLogin <= b lastLogin]' 8 #[30 105 226 0 226 32 130 106] #lastLogin 18208 7 513 nil nil nil 16944 nil 1 nil nil #report 576 nil 131169 nil 1 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1970 138 144 34 3 2034 #createAt:extent: 34 2 498 1 51 498 1319 289 16944 2034 #contextMenu: 34 1 17040 16944 2034 #text: 34 1 8 'Version' 16944 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 147 2 0 0 169 0 0 0] 8 #() 3088 nil 35 3106 3536 1 3168 -49 3568 1 7872 1 170 192 34 6 14704 8 'Other Tools' 16944 8 'loginList' 15776 8 'Edit Tools' nil 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 1 1 498 1369 339 14592 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 169 0 0 0] 34 5 15776 14704 16944 15408 16576 3088 nil 27 8 'top' nil 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 9 53 498 1373 723 2400 2162 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 178 2 0 0 131 1 0 0] 34 3 14592 410 ##(Smalltalk.Splitter) 34 12 nil 2400 34 2 8 1140850688 1 18960 nil 480 nil 517 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 18960 nil 1 #left nil nil nil 498 1 1 498 9 9 nil 19056 nil 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 1 339 498 11 11 18960 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 169 0 0 0 5 0 0 0 174 0 0 0] 8 #() 3088 nil 27 2528 3088 nil 27 2354 8 'GCI Activity' 410 ##(Smalltalk.ContainerView) 34 15 nil 2224 34 2 8 1140850688 131073 19280 nil nil nil 7 nil nil nil 19280 2594 170 176 34 6 410 ##(Smalltalk.StaticText) 34 16 nil 19280 34 2 8 1140850944 65 19392 nil nil nil 7 nil nil nil 19392 nil 8 1943828432 2850 nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 11 11 498 201 41 19392 2034 #text: 34 1 8 'Active Library:' 19392 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 105 0 0 0 25 0 0 0] 8 #() 3088 nil 27 3106 3536 11 3552 201 4864 11 3200 41 410 ##(Smalltalk.TextEdit) 34 16 nil 19280 34 2 8 1140916352 1025 19712 nil 480 nil 7 nil nil nil 19712 nil 8 1943850864 2850 nil nil 3 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 201 11 498 301 39 19712 2034 #setMarginWidths: 34 1 8 #(3 3) 19712 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 100 0 0 0 5 0 0 0 250 0 0 0 24 0 0 0] 8 #() 3088 nil 27 3106 3536 201 3552 301 4864 11 3200 39 410 ##(Smalltalk.ListView) 34 45 nil 19280 34 2 8 1409355853 1025 20032 6226 138 144 576 nil 6288 480 nil 7 nil nil nil 20032 nil 8 1943883040 6322 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 17344 nil nil nil nil nil nil 138 144 34 6 17394 8 'Session' 121 #right 6322 #displayString 20160 6322 #<= 8 #() nil nil 20032 nil 1 nil nil 17394 8 'Time' 201 #left 6322 #displayString 576 6322 #<= 576 nil nil 20032 nil 1 nil nil 17394 8 'Description' 401 #left 6322 #displayString 576 6322 #<= 576 nil nil 20032 nil 1 nil nil 17394 8 'Semaphore' 161 #right 6322 #displayString 576 6322 #<= 576 nil nil 20032 nil 1 nil nil 17394 8 'Server' 161 #right 6322 #displayString 576 6322 #<= 576 nil nil 20032 nil 1 nil nil 17394 8 'Result' 261 #left 6322 #displayString 576 6322 #<= 576 nil nil 20032 nil 1 nil nil #report 576 nil 131169 nil 1 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1970 138 144 34 2 2034 #createAt:extent: 34 2 498 11 51 498 1353 663 20032 2034 #text: 34 1 8 'Session' 20032 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 25 0 0 0 169 2 0 0 100 1 0 0] 8 #() 3088 nil 35 3106 3536 11 3168 -9 4864 51 7872 -9 170 192 34 4 20032 8 'activityList' 19712 8 'activeLibrary' nil 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 9 53 498 1373 723 19280 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 178 2 0 0 131 1 0 0] 34 3 19392 19712 20032 3088 nil 27 19280 nil nil nil 170 192 34 4 2400 8 'logins' 19280 8 'gciActivity' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 2224 34 2 8 1140916736 1 21120 6226 138 144 34 2 2384 19264 nil 6288 nil nil 1 nil nil nil 21120 nil 8 1943988896 17474 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 21248 7 257 nil 17474 0 nil 21266 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 21312 7 257 nil 17344 nil nil nil nil nil #noIcons nil nil nil nil nil 1970 138 144 34 3 2034 #createAt:extent: 34 2 498 1 1 498 1389 783 21120 2034 #basicSelectionsByIndex: 34 1 8 #(2) 21120 2034 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 21120 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 182 2 0 0 135 1 0 0] 8 #() 3088 nil 27 1970 138 144 34 1 2034 #createAt:extent: 34 2 498 1 1 498 1389 783 2224 2162 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 182 2 0 0 135 1 0 0] 34 3 2400 19280 21120 3088 nil 27 3088 nil 27 )! !
!JadeLoginShell class categoriesFor: #icon!constants!public! !
!JadeLoginShell class categoriesFor: #resource_Default_view!public!resources-views! !

