"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #JadeLoginShell
	instanceVariableNames: 'activeLibraryPresenter activityListMutex activityListPresenter debugPathPresenter gemHostPresenter gemServicePresenter gemTaskPresenter gemTypePresenter hostPasswordPresenter hostUserIDPresenter initialsPresenter inUpdateView loginListPresenter loginTypePresenter passwordPresenter recentActivity resetButtonPresenter stoneHostPresenter stoneNamePresenter userIDPresenter versionListPresenter'
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

breakBlock: aBlock

	activityListPresenter selectionOrNil ifNil: [
		Sound beep.
	] ifNotNil: [:activity | 
		activity canBreak ifTrue: [
			aBlock value: GciLibrary activeLibrary value: activity sessionId.
		] ifFalse: [
			Sound beep.
		].
	].
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

	GciActivity activity 		when:  #changed			send: #'updateActivity'  to: self.!

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

enableWindow

	activityListPresenter selectionOrNil ifNil: [
		Sound beep.
	] ifNotNil: [:activity | 
		activity enableWindow.
	].
!

exportActivity

	| default fileTypes path file |
	default := File workingDirectory , 'gcActivity.log'.
	fileTypes := #( 
		('Text Files (*.txt)' '*.txt')
		('Log Files (*.log)' '*.log')
		('All Files (*.*)' '*.*')).
	path := FileSaveDialog new
		caption: 'Export GCI Activity to File';
		fileTypes: fileTypes;
		defaultExtension: 'log';
		value: default;
		overwritePrompt;
		showModal.
	file := FileStream write: path mode: #'truncate' check: false type: #'utf8'.
	[
		GciActivity activity do: [:each | each writeOn: file].
	] ensure: [
		file close.
	].
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

hardBreak

	self breakBlock: [:library :sessionId | library hardBreakSession: sessionId].
!

initialize

	super initialize.
	inUpdateView := false.
	activityListMutex := Mutex new.!

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

	| cardContainerView session |
	cardContainerView := self view viewNamed: 'cardContainer'.
	self updateModel.
	self model isAbleToLoadLibrary ifFalse: [
		Sound beep.
		^self
	].
	[
		cardContainerView lastCard.
		SessionManager inputState pumpMessages.
		session := self model login.
	] on: Error do: [:ex | 
		MessageBox 
			errorMsg: ex description
			caption: 'Jade Login Error'.
		SessionManager current pause.
		^self.
	]. 
	self postLogin: session.
	session
		when: #logout
		send: #firstCard
		to: cardContainerView.!

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
	[self checkScintillaVersion] postToMessageQueue.
	activityListPresenter list: GciActivity activity.
	(self view viewNamed: 'cardContainer') firstCard.

!

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

resetActivity

	GciActivity activity resize: 0.
	self updateActivity.

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

softBreak

	self breakBlock: [:library :sessionId | library softBreakSession: sessionId].
!

tempDataPath

	^self dataPath , '.new'
!

updateActivity
	"If we update in real time it seems to slow down other processing (like changing tabs in the System Browser.
	The mutex is to ensure that only one process manipulates the UI at a time (otherwise we can hang)."

	[
		[
			(Delay forMilliseconds: 100) wait.
			activityListMutex critical: [
				| list |
				list := GciActivity activity.
				((list isEmpty and: [recentActivity notNil]) or: [recentActivity ~~ list last]) ifTrue: [
					activeLibraryPresenter value: GciLibrary activeLibrary class name asString.
					[list size > 99] whileTrue: [list removeFirst].
					list := list copy.
					activityListPresenter list: list.
					list isEmpty ifTrue: [
						recentActivity := nil.
					] ifFalse: [
						recentActivity := list last.
						activityListPresenter selection: recentActivity.
					].
				].
			].
		] on: Error do: [:ex | ex halt return].
	] forkAt: Processor userBackgroundPriority.!

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
!JadeLoginShell categoriesFor: #breakBlock:!private! !
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
!JadeLoginShell categoriesFor: #enableWindow!private! !
!JadeLoginShell categoriesFor: #exportActivity!private! !
!JadeLoginShell categoriesFor: #fileOpen!commands!public! !
!JadeLoginShell categoriesFor: #fillVersionList!public! !
!JadeLoginShell categoriesFor: #hardBreak!private! !
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
!JadeLoginShell categoriesFor: #resetActivity!private! !
!JadeLoginShell categoriesFor: #saveData!public!user data! !
!JadeLoginShell categoriesFor: #selectionChanged!public! !
!JadeLoginShell categoriesFor: #sendRequiredMessages!public! !
!JadeLoginShell categoriesFor: #setWorkingDirectory!public! !
!JadeLoginShell categoriesFor: #showGemNRS!commands!public! !
!JadeLoginShell categoriesFor: #showStoneNRS!commands!public! !
!JadeLoginShell categoriesFor: #softBreak!private! !
!JadeLoginShell categoriesFor: #tempDataPath!public!user data! !
!JadeLoginShell categoriesFor: #updateActivity!private! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13238272 65536) 416 nil 327686 ##(Smalltalk.Color) #default 328198 ##(Smalltalk.Point) 1421 901 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 34 2 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 624 nil 480 nil 7 nil nil nil 624 656710 1 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 8 'Logins' 410 ##(Smalltalk.ContainerView) 34 15 nil 624 34 2 8 1140850688 131073 800 nil nil nil 5 nil nil nil 800 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 170 192 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 800 34 2 8 1140850688 131073 928 nil nil nil 5 nil nil nil 928 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 928 34 2 8 1140850688 131073 1056 nil nil nil 5 nil nil nil 1056 994 170 176 34 26 410 ##(Smalltalk.StaticText) 34 16 nil 1056 34 2 8 1140850944 1 1168 nil nil nil 5 nil nil nil 1168 nil 8 1943828432 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 498 11 115 498 301 31 1168 1346 #text: 34 1 8 'NetLDI Task Name:' 1168 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 57 0 0 0 155 0 0 0 72 0 0 0] 8 #() 498 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 11 1586 #fixedViewLeft 301 1586 #fixedPreviousBottom 3 1586 #fixedViewTop 31 410 ##(Smalltalk.ContainerView) 34 15 nil 1056 34 2 8 1140850688 393217 1664 nil 480 nil 5 nil nil nil 1664 656390 ##(Smalltalk.GridLayout) 3 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 1664 34 2 8 1140924425 1 1792 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 459270 ##(Smalltalk.Message) #= 8 #() 1922 #hash 8 #() false 480 nil 5 nil nil nil 1792 nil 8 1943863552 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 1 1 498 331 45 1792 1346 #text: 34 1 8 'RPC Gem' 1792 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 165 0 0 0 22 0 0 0] 8 #() 1536 nil 27 8 'remote' 410 ##(Smalltalk.RadioButton) 34 16 nil 1664 34 2 8 1141055497 1 2256 1858 nil nil 1890 1922 #= 8 #() 1922 #hash 8 #() false 480 nil 5 nil nil nil 2256 nil 8 1943863552 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 331 1 498 331 45 2256 1346 #text: 34 1 8 'Linked Gem (32-bit only)' 2256 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 74 1 0 0 22 0 0 0] 8 #() 1536 nil 27 8 'linked' nil 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 11 257 498 665 49 1664 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 128 0 0 0 81 1 0 0 152 0 0 0] 34 2 1792 2256 1536 nil 27 1554 1600 11 1586 #fixedParentRight -9 1632 5 1648 49 410 ##(Smalltalk.TextEdit) 34 16 nil 1056 34 2 8 1140916352 1025 2864 nil 480 nil 5 nil nil nil 2864 nil 8 1943850864 1250 nil nil 1 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 491 163 498 185 45 2864 1346 #setMarginWidths: 34 1 8 #(3 3) 2864 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 81 0 0 0 81 1 0 0 103 0 0 0] 8 #() 1536 nil 27 1554 1586 #fixedPreviousRight 11 2848 -9 1586 #fixedPreviousTop 1 1648 45 410 ##(Smalltalk.StaticText) 34 16 nil 1056 34 2 8 1140850944 1 3216 nil nil nil 5 nil nil nil 3216 nil 8 1943828432 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 321 209 498 161 41 3216 1346 #text: 34 1 8 'Password:' 3216 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 104 0 0 0 240 0 0 0 124 0 0 0] 8 #() 1536 nil 27 1554 1586 #fixedPreviousLeft -169 1616 161 1632 3 1648 41 410 ##(Smalltalk.StaticText) 34 16 nil 1056 34 2 8 1140850944 1 3552 nil nil nil 5 nil nil nil 3552 nil 8 1943828432 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 321 163 498 161 41 3552 1346 #text: 34 1 8 'OS User ID:' 3552 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 81 0 0 0 240 0 0 0 101 0 0 0] 8 #() 1536 nil 27 1554 3184 11 1616 161 3200 1 1648 41 410 ##(Smalltalk.TextEdit) 34 16 nil 1056 34 2 8 1140916384 1025 3872 nil 480 nil 5 nil nil nil 3872 nil 8 1943850864 1250 nil nil 1 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 491 209 498 185 45 3872 1346 #setMarginWidths: 34 1 8 #(3 3) 3872 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 104 0 0 0 81 1 0 0 126 0 0 0] 8 #() 1536 nil 27 1554 3184 11 2848 -9 3200 1 1648 45 410 ##(Smalltalk.PushButton) 34 20 nil 1056 34 2 8 1140924416 1 4192 nil nil nil 5 nil nil nil 4192 nil 8 1943863552 1180998 4 ##(Smalltalk.CommandDescription) #debugGCI 8 'Debug GCI' 1 1 nil nil false nil nil nil 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 11 5 498 141 51 4192 1346 #isEnabled: 8 #(false) 4192 1346 #text: 34 1 8 'Debug GCI' 4192 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 75 0 0 0 27 0 0 0] 8 #() 1536 nil 29 1554 1600 11 1616 141 1632 5 1648 51 410 ##(Smalltalk.TextEdit) 34 16 nil 1056 34 2 8 1140916352 1025 4576 nil 480 nil 5 nil nil nil 4576 nil 8 1943850864 1250 nil nil 1 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 321 115 498 355 45 4576 1346 #text: 34 1 8 'gemnetobject' 4576 1346 #setMarginWidths: 34 1 8 #(3 3) 4576 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 57 0 0 0 81 1 0 0 79 0 0 0] 8 #() 1536 nil 27 1554 3184 11 2848 -9 3200 1 1648 45 410 ##(Smalltalk.TextEdit) 34 16 nil 1056 34 2 8 1140916352 1025 4944 nil 480 nil 5 nil nil nil 4944 nil 8 1943850864 1250 nil nil 1 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 321 69 498 355 45 4944 1346 #text: 34 1 8 'localhost' 4944 1346 #setMarginWidths: 34 1 8 #(3 3) 4944 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 34 0 0 0 81 1 0 0 56 0 0 0] 8 #() 1536 nil 27 1554 3184 11 2848 -9 3200 1 1648 45 410 ##(Smalltalk.StaticText) 34 16 nil 1056 34 2 8 1140850944 1 5312 nil nil nil 5 nil nil nil 5312 nil 8 1943828432 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 11 69 498 301 41 5312 1346 #text: 34 1 8 'Stone Host (from Gem):' 5312 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 34 0 0 0 155 0 0 0 54 0 0 0] 8 #() 1536 nil 27 1554 1600 11 1616 301 1632 11 1648 41 410 ##(Smalltalk.ContainerView) 34 15 nil 1056 34 2 8 1409286144 393217 5632 nil 480 nil 5 nil nil nil 5632 1730 1 3 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 5632 34 2 8 1140924425 1 5744 1858 nil nil 1890 1922 #= 8 #() 1922 #hash 8 #() false 480 nil 5 nil nil nil 5744 nil 8 1943863552 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 1 39 498 297 39 5744 1346 #text: 34 1 8 'Native OS User' 5744 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 148 0 0 0 38 0 0 0] 8 #() 1536 nil 27 8 'member' 410 ##(Smalltalk.RadioButton) 34 16 nil 5632 34 2 8 1141055497 1 6160 1858 nil nil 1890 1922 #= 8 #() 1922 #hash 8 #() false 480 nil 5 nil nil nil 6160 nil 8 1943863552 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 1 1 498 297 39 6160 1346 #text: 34 1 8 'NetLDI Guest' 6160 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 148 0 0 0 19 0 0 0] 8 #() 1536 nil 27 8 'guest' nil 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 11 163 498 301 81 5632 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 81 0 0 0 155 0 0 0 121 0 0 0] 34 2 6160 5744 1536 nil 27 1554 1600 11 1616 301 1632 5 1648 81 410 ##(Smalltalk.PushButton) 34 20 nil 1056 34 2 8 1140924416 1 6752 nil nil nil 5 nil nil nil 6752 nil 8 1943863552 4274 #login 8 'Login' 1 1 nil nil true nil nil nil 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 11 305 498 665 61 6752 1346 #isEnabled: 8 #(false) 6752 1346 #text: 34 1 8 'Login' 6752 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 152 0 0 0 81 1 0 0 182 0 0 0] 8 #() 1536 nil 29 1554 1600 11 2848 -9 1632 1 1648 61 410 ##(Smalltalk.StaticPath) 34 17 nil 1056 34 2 8 1140850944 1 7120 nil nil nil 5 nil nil nil 7120 nil 8 1943828432 1250 nil nil nil 8 'XXX' 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 161 15 498 515 45 7120 1346 #text: 34 1 7216 7120 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 7 0 0 0 81 1 0 0 29 0 0 0] 8 #() 1536 nil 27 1554 1600 161 2848 -9 3200 11 1648 45 170 192 34 14 2864 8 'hostUserID' 1664 8 'gemType' 7120 8 'debugPath' 4576 8 'gemTask' 4944 8 'stoneHost' 5632 8 'loginType' 3872 8 'hostPassword' nil 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 685 1 498 685 687 1056 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 86 1 0 0 0 0 0 0 172 2 0 0 87 1 0 0] 34 13 4192 7120 5312 4944 1168 4576 5632 3552 2864 3216 3872 1664 6752 1536 nil 27 1554 1586 #relativeParentWidth 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 2848 1 1586 #fixedParentTop 1 1586 #fixedParentBottom 1 410 ##(Smalltalk.ContainerView) 34 15 nil 928 34 2 8 1140850688 131073 7840 nil nil nil 5 nil nil nil 7840 994 170 176 34 28 410 ##(Smalltalk.ComboBox) 34 17 nil 7840 34 2 8 1144063491 1025 7952 590662 2 ##(Smalltalk.ListModel) 138 144 576 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 480 nil 5 nil nil nil 7952 nil 8 1943716480 1922 #displayString 8 #() 576 401 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 391 11 498 285 51 7952 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 5 0 0 0 81 1 0 0 30 0 0 0] 8 #() 1536 nil 27 1554 3184 11 2848 -9 7808 11 1648 47 410 ##(Smalltalk.StaticText) 34 16 nil 7840 34 2 8 1140850944 1 8320 nil nil nil 5 nil nil nil 8320 nil 8 1943828432 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 11 259 498 371 41 8320 1346 #text: 34 1 8 'GemStone Password:' 8320 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 129 0 0 0 190 0 0 0 149 0 0 0] 8 #() 1536 nil 27 1554 1600 11 1616 371 1632 3 1648 41 410 ##(Smalltalk.TextEdit) 34 16 nil 7840 34 2 8 1140916384 1025 8640 nil 480 nil 5 nil nil nil 8640 nil 8 1943850864 1250 nil nil 1 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 391 259 498 285 45 8640 1346 #setMarginWidths: 34 1 8 #(3 3) 8640 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 129 0 0 0 81 1 0 0 151 0 0 0] 8 #() 1536 nil 27 1554 3184 11 2848 -9 3200 1 1648 45 410 ##(Smalltalk.TextEdit) 34 16 nil 7840 34 2 8 1140916352 1025 8960 nil 480 nil 5 nil nil nil 8960 nil 8 1943850864 1250 nil nil 1 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 391 213 498 285 45 8960 1346 #setMarginWidths: 34 1 8 #(3 3) 8960 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 106 0 0 0 81 1 0 0 128 0 0 0] 8 #() 1536 nil 27 1554 3184 11 2848 -9 3200 1 1648 45 410 ##(Smalltalk.StaticText) 34 16 nil 7840 34 2 8 1140850944 1 9280 nil nil nil 5 nil nil nil 9280 nil 8 1943828432 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 11 21 498 371 41 9280 1346 #text: 34 1 8 'GemStone/Smalltalk Version:' 9280 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 190 0 0 0 30 0 0 0] 8 #() 1536 nil 27 1554 1600 11 1616 371 7808 21 1648 41 410 ##(Smalltalk.StaticText) 34 16 nil 7840 34 2 8 1140850944 1 9600 nil nil nil 5 nil nil nil 9600 nil 8 1943828432 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 11 213 498 371 41 9600 1346 #text: 34 1 8 'GemStone User ID:' 9600 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 106 0 0 0 190 0 0 0 126 0 0 0] 8 #() 1536 nil 27 1554 1600 11 1616 371 1632 3 1648 41 410 ##(Smalltalk.StaticText) 34 16 nil 7840 34 2 8 1140850944 1 9920 nil nil nil 5 nil nil nil 9920 nil 8 1943828432 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 11 313 498 371 41 9920 1346 #text: 34 1 8 'Monticello User Name:' 9920 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 156 0 0 0 190 0 0 0 176 0 0 0] 8 #() 1536 nil 27 1554 1600 11 1616 371 1632 11 1648 41 410 ##(Smalltalk.StaticText) 34 16 nil 7840 34 2 8 1140850944 1 10240 nil nil nil 5 nil nil nil 10240 nil 8 1943828432 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 11 167 498 371 39 10240 1346 #text: 34 1 8 'Stone Name:' 10240 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 83 0 0 0 190 0 0 0 102 0 0 0] 8 #() 1536 nil 27 1554 1600 11 1616 371 1632 11 1648 39 410 ##(Smalltalk.StaticText) 34 16 nil 7840 34 2 8 1140850944 1 10560 nil nil nil 5 nil nil nil 10560 nil 8 1943828432 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 11 67 498 371 31 10560 1346 #text: 34 1 8 'RPC Gem Host Name or IP:' 10560 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 190 0 0 0 48 0 0 0] 8 #() 1536 nil 27 1554 1600 11 1616 371 1632 11 1648 31 410 ##(Smalltalk.TextEdit) 34 16 nil 7840 34 2 8 1140916352 1025 10880 nil 480 nil 5 nil nil nil 10880 nil 8 1943850864 1250 nil nil 1 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 391 113 498 285 45 10880 1346 #setMarginWidths: 34 1 8 #(3 3) 10880 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 56 0 0 0 81 1 0 0 78 0 0 0] 8 #() 1536 nil 27 1554 3184 11 2848 -9 3200 1 1648 45 410 ##(Smalltalk.StaticText) 34 16 nil 7840 34 2 8 1140850944 1 11200 nil nil nil 5 nil nil nil 11200 nil 8 1943828432 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 11 113 498 371 31 11200 1346 #text: 34 1 8 'NetLDI Service Name or Port:' 11200 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 56 0 0 0 190 0 0 0 71 0 0 0] 8 #() 1536 nil 27 1554 1600 11 1616 371 1632 3 1648 31 410 ##(Smalltalk.TextEdit) 34 16 nil 7840 34 2 8 1140916352 1025 11520 nil 480 nil 5 nil nil nil 11520 nil 8 1943850864 1250 nil nil 1 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 391 167 498 285 45 11520 1346 #setMarginWidths: 34 1 8 #(3 3) 11520 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 83 0 0 0 81 1 0 0 105 0 0 0] 8 #() 1536 nil 27 1554 3184 11 2848 -9 3200 1 1648 45 410 ##(Smalltalk.TextEdit) 34 16 nil 7840 34 2 8 1140916352 1025 11840 nil 480 nil 5 nil nil nil 11840 nil 8 1943850864 1250 nil nil 1 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 391 313 498 285 45 11840 1346 #setMarginWidths: 34 1 8 #(3 3) 11840 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 156 0 0 0 81 1 0 0 178 0 0 0] 8 #() 1536 nil 27 1554 3184 11 2848 -9 3200 1 1648 45 410 ##(Smalltalk.TextEdit) 34 16 nil 7840 34 2 8 1140916352 1025 12160 nil 480 nil 5 nil nil nil 12160 nil 8 1943850864 1250 nil nil 1 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 391 67 498 285 45 12160 1346 #setMarginWidths: 34 1 8 #(3 3) 12160 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 33 0 0 0 81 1 0 0 55 0 0 0] 8 #() 1536 nil 27 1554 3184 11 2848 -9 3200 1 1648 45 170 192 34 14 11520 8 'stoneName' 10880 8 'gemService' 8960 8 'userID' 8640 8 'password' 11840 8 'initials' 7952 8 'versionList' 12160 8 'gemHost' nil 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 1 1 498 685 687 7840 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 1 0 0 87 1 0 0] 34 14 9280 7952 10560 12160 11200 10880 10240 11520 9600 8960 8320 8640 9920 11840 1536 nil 27 1554 1600 1 7760 7778 8 0 0 0 0 0 0 224 63 7808 1 7824 1 170 192 34 4 1056 8 'Advanced' 7840 8 'Basic' nil 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 1 349 498 1369 687 928 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 174 0 0 0 172 2 0 0 5 2 0 0] 34 2 7840 1056 1536 nil 27 8 'bottom' 410 ##(Smalltalk.ContainerView) 34 15 nil 800 34 2 8 1140850688 131073 13056 nil nil nil 5 nil nil nil 13056 994 170 176 34 10 410 ##(Smalltalk.ListView) 34 45 nil 13056 34 2 8 1409355853 1025 13168 8018 138 144 576 nil 8080 480 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 4 984134 2 ##(Smalltalk.CommandMenuItem) 1 4274 #copyXML 8 '&Copy XML' 1 1 nil nil nil 13314 1 4274 #addLogin 8 '&Add' 1 1 nil nil nil 13314 1 4274 #removeLogin 8 '&Remove' 1 1 nil nil nil 13314 1 4274 #login 8 '&Login' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 13168 nil 8 1943883040 1922 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 6 920646 5 ##(Smalltalk.ListViewColumn) 8 'Version' 241 #left 1922 #gemVersion 8 #() 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a gemVersion <= b gemVersion]' 8 #[30 105 226 0 226 32 130 106] #gemVersion 13744 7 513 nil nil nil 13168 nil 1 nil nil 13650 8 'Gem Host' 201 #left 1922 #gemHost 13712 13730 0 nil 13762 2 1 1920 8 'doIt' 8 '[:a :b | a gemHost <= b gemHost]' 8 #[30 105 226 0 226 32 130 106] #gemHost 13936 7 513 nil nil nil 13168 nil 1 nil nil 13650 8 'NetLDI' 201 #left 1922 #gemService 13712 13730 0 nil 13762 2 1 1920 8 'doIt' 8 '[:a :b | a gemService <= b gemService]' 8 #[30 105 226 0 226 32 130 106] #gemService 14064 7 513 nil nil nil 13168 nil 1 nil nil 13650 8 'Stone' 201 #left 1922 #stoneName 13712 13730 0 nil 13762 2 1 1920 8 'doIt' 8 '[:a :b | a stoneName <= b stoneName]' 8 #[30 105 226 0 226 32 130 106] #stoneName 14192 7 513 nil nil nil 13168 nil 1 nil nil 13650 8 'User' 201 #left 1922 #gsUserID 13712 13730 0 nil 13762 2 1 1920 8 'doIt' 8 '[:a :b | a gsUserID <= b gsUserID]' 8 #[30 105 226 0 226 32 130 106] #gsUserID 14320 7 513 nil nil nil 13168 nil 1 nil nil 13650 8 'Last Login' 441 #left 1922 #lastLoginString 8 #() 13730 0 nil 13762 2 1 1920 8 'doIt' 8 '[:a :b | a lastLogin <= b lastLogin]' 8 #[30 105 226 0 226 32 130 106] #lastLogin 14464 7 513 nil nil nil 13168 nil 1 nil nil #report 576 nil 131169 nil 1 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 1 51 498 1319 289 13168 1346 #contextMenu: 34 1 13280 13168 1346 #text: 34 1 8 'Version' 13168 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 147 2 0 0 169 0 0 0] 8 #() 1536 nil 35 1554 1600 1 2848 -49 1632 1 7824 1 410 ##(Smalltalk.PushButton) 34 20 nil 13056 34 2 8 1140924416 1 14800 nil nil nil 5 nil nil nil 14800 nil 8 1943863552 4274 #removeLogin 8 '-' 1 1 nil nil false nil nil nil 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 1319 121 498 51 51 14800 1346 #isEnabled: 8 #(false) 14800 1346 #text: 34 1 8 '-' 14800 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 60 0 0 0 172 2 0 0 85 0 0 0] 8 #() 1536 nil 29 1554 2848 -49 1616 51 1632 11 1648 51 410 ##(Smalltalk.Toolbar) 34 25 nil 13056 34 2 8 1140853580 131137 15168 nil 480 nil 517 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 498 193 193 nil 15168 480 8 1943876592 170 192 576 170 192 34 4 10177 1246790 1 ##(Smalltalk.ToolbarSystemButton) 15168 1 4274 #getHelpOn 8 'Get help on widget' 1 1 nil 10177 1 23 10179 1246790 1 ##(Smalltalk.ToolbarBitmapButton) 15168 1 4274 #openHelp 8 'Jade Help' 1 1 nil 10179 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 498 1857 33 71 34 2 15408 15472 nil nil 1 nil 498 33 33 498 45 45 nil 866 1 1 1 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 241 1 498 1129 51 15168 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 120 0 0 0 0 0 0 0 172 2 0 0 25 0 0 0] 8 #() 1536 nil 27 1554 3184 1 2848 1 7808 1 1648 51 410 ##(Smalltalk.PushButton) 34 20 nil 13056 34 2 8 1140924416 1 15904 nil nil nil 5 nil nil nil 15904 nil 8 1943863552 4274 #addLogin 8 '+' 1 1 nil nil false nil nil nil 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 1319 61 498 51 51 15904 1346 #isEnabled: 8 #(false) 15904 1346 #text: 34 1 8 '+' 15904 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 30 0 0 0 172 2 0 0 55 0 0 0] 8 #() 1536 nil 29 1554 2848 -49 1616 51 7808 61 1648 51 410 ##(Smalltalk.Toolbar) 34 25 nil 13056 34 2 8 1409289036 131137 16272 nil 480 nil 5 nil nil nil 16272 480 8 1943876592 170 192 576 170 192 34 10 10167 1115206 1 ##(Smalltalk.ToolbarIconButton) 16272 1 4274 #editCut 8 'Cut' 1 1 263494 3 ##(Smalltalk.Icon) nil true 15568 8 'icons\EditCut.ico' nil 10167 10173 16402 16272 1 4274 #editDelete 8 'Delete' 1 1 16466 nil true 15568 8 'icons\EditDelete.ico' nil 10173 10169 16402 16272 1 4274 #editCopy 8 'Copy' 1 1 16466 nil true 15568 8 'icons\EditCopy.ico' nil 10169 10175 16402 16272 1 4274 #editUndo 8 'Undo' 1 1 16466 nil true 15568 8 'icons\EditUndo.ico' nil 10175 10171 16402 16272 1 4274 #editPaste 8 'Paste' 1 1 16466 nil true 15568 8 'icons\EditPasteLink.ico' nil 10171 34 6 16416 16592 16752 16512 16672 1049158 1 ##(Smalltalk.ToolbarSeparator) 16272 1 nil nil 1 nil 498 33 33 498 45 45 nil nil 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 1 1 498 241 51 16272 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 120 0 0 0 25 0 0 0] 8 #() 1536 nil 27 1554 1600 1 1616 241 7808 1 1648 51 170 192 34 6 16272 8 'Edit Tools' 15168 8 'Other Tools' 13168 8 'loginList' nil 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 1 1 498 1369 339 13056 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 169 0 0 0] 34 5 16272 15168 13168 15904 14800 1536 nil 27 8 'top' nil 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 9 53 498 1373 723 800 1474 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 178 2 0 0 131 1 0 0] 34 3 13056 410 ##(Smalltalk.Splitter) 34 12 nil 800 34 2 8 1140850688 1 17504 nil 480 nil 517 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 17504 nil 1 #left nil nil nil 498 1 1 498 9 9 nil 17600 nil 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 1 339 498 11 11 17504 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 169 0 0 0 5 0 0 0 174 0 0 0] 8 #() 1536 nil 27 928 1536 nil 27 754 8 'GCI Activity' 410 ##(Smalltalk.ContainerView) 34 15 nil 624 34 2 8 1140850688 131073 17824 nil nil nil 7 nil nil nil 17824 994 170 176 34 16 410 ##(Smalltalk.PushButton) 34 20 nil 17824 34 2 8 1140924416 1 17936 nil nil nil 7 nil nil nil 17936 nil 8 1943863552 4274 #enableWindow 8 'Enable' 1 1 nil nil false nil nil nil 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 573 5 498 151 51 17936 1346 #isEnabled: 8 #(false) 17936 1346 #text: 34 1 8 'Enable' 17936 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 30 1 0 0 2 0 0 0 105 1 0 0 27 0 0 0] 8 #() 1536 nil 29 1554 2848 -799 1616 151 7808 5 1648 51 410 ##(Smalltalk.ListView) 34 45 nil 17824 34 2 8 1409355853 1025 18304 8018 138 144 576 nil 8080 480 nil 7 nil nil nil 18304 nil 8 1943883040 1922 #displayString 8 #() nil 13600 nil nil nil nil nil nil 138 144 34 7 13650 8 '#' 51 #right 1922 #displayString 18432 1922 #<= 8 #() 13730 0 nil 13762 2 1 13728 8 'doIt' 8 '[:each | each sessionId]' 8 #[30 105 226 0 106] #sessionId 18560 7 257 nil nil 18304 nil 1 nil nil 13650 8 'Process' 131 #right 1922 #displayString 8 #() 1922 #<= 18688 13730 0 nil 13762 2 1 13728 8 'doIt' 8 '[:entry |entry processId]' 8 #[30 105 226 0 106] #processId 18720 7 257 nil nil 18304 nil 1 nil nil 13650 8 'Time' 171 #left 1922 #displayString 8 #() 1922 #<= 18848 13730 0 nil 13762 2 1 13728 8 'doIt' 8 '[:each | each time ]' 8 #[30 105 226 0 106] #time 18880 7 257 nil nil 18304 nil 1 nil nil 13650 8 'Description' 601 #left 1922 #displayString 18848 1922 #<= 18848 13730 0 nil 13762 2 1 13728 8 'doIt' 8 '[:each | each request ]' 8 #[30 105 226 0 106] #request 19024 7 257 nil nil 18304 nil 1 nil nil 13650 8 'Sem' 101 #right 1922 #displayString 18848 1922 #<= 18848 13730 0 nil 13762 2 1 13728 8 'doIt' 8 '[:each | each msForSemaphore]' 8 #[30 105 226 0 106] #msForSemaphore 19168 7 257 nil nil 18304 nil 1 nil nil 13650 8 'GCI' 101 #right 1922 #displayString 18848 1922 #<= 18848 13730 0 nil 13762 2 1 13728 8 'doIt' 8 '[:each | each msForResponse]' 8 #[30 105 226 0 106] #msForResponse 19312 7 257 nil nil 18304 nil 1 nil nil 13650 8 'Result' 161 #left 1922 #displayString 18848 1922 #<= 18848 13730 0 nil 13762 2 1 13728 8 'doIt' 8 '[:each | each response]' 8 #[30 105 226 0 106] #response 19456 7 257 nil nil 18304 nil 1 nil nil #report 576 nil 131169 nil 1 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 11 61 498 1353 653 18304 1346 #text: 34 1 8 '#' 18304 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 30 0 0 0 169 2 0 0 100 1 0 0] 8 #() 1536 nil 35 1554 1600 11 2848 -9 7808 61 7824 -9 410 ##(Smalltalk.StaticText) 34 16 nil 17824 34 2 8 1140850944 65 19760 nil nil nil 7 nil nil nil 19760 nil 8 1943828432 1250 nil nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 11 11 498 201 41 19760 1346 #text: 34 1 8 'Active Library:' 19760 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 105 0 0 0 25 0 0 0] 8 #() 1536 nil 27 1554 1600 11 1616 201 7808 11 1648 41 410 ##(Smalltalk.PushButton) 34 20 nil 17824 34 2 8 1140924416 1 20080 nil nil nil 7 nil nil nil 20080 nil 8 1943863552 4274 #hardBreak 8 'Hard Break' 1 1 nil nil false nil nil nil 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 893 5 498 151 51 20080 1346 #isEnabled: 8 #(false) 20080 1346 #text: 34 1 8 'Hard Break' 20080 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 190 1 0 0 2 0 0 0 9 2 0 0 27 0 0 0] 8 #() 1536 nil 29 1554 2848 -479 1616 151 7808 5 1648 51 410 ##(Smalltalk.TextEdit) 34 16 nil 17824 34 2 8 1140916352 1025 20448 nil 480 nil 7 nil nil nil 20448 nil 8 1943850864 1250 nil nil 1 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 211 11 498 301 39 20448 1346 #isEnabled: 8 #(false) 20448 1346 #setMarginWidths: 34 1 8 #(3 3) 20448 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 5 0 0 0 255 0 0 0 24 0 0 0] 8 #() 1536 nil 27 1554 1600 211 1616 301 7808 11 1648 39 410 ##(Smalltalk.PushButton) 34 20 nil 17824 34 2 8 1140924416 1 20800 nil nil nil 7 nil nil nil 20800 nil 8 1943863552 4274 #softBreak 8 'Soft Break' 1 1 nil nil false nil nil nil 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 1053 5 498 151 51 20800 1346 #isEnabled: 8 #(false) 20800 1346 #text: 34 1 8 'Soft Break' 20800 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 2 0 0 2 0 0 0 89 2 0 0 27 0 0 0] 8 #() 1536 nil 29 1554 2848 -319 1616 151 7808 5 1648 51 410 ##(Smalltalk.PushButton) 34 20 nil 17824 34 2 8 1140924416 1 21168 nil nil nil 7 nil nil nil 21168 nil 8 1943863552 4274 #exportActivity 8 'Export' 1 1 nil nil false nil nil nil 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 733 5 498 151 51 21168 1346 #isEnabled: 8 #(false) 21168 1346 #text: 34 1 8 'Export' 21168 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 110 1 0 0 2 0 0 0 185 1 0 0 27 0 0 0] 8 #() 1536 nil 29 1554 2848 -639 1616 151 7808 5 1648 51 410 ##(Smalltalk.PushButton) 34 20 nil 17824 34 2 8 1140924416 1 21536 nil nil nil 7 nil nil nil 21536 nil 8 1943863552 4274 #resetActivity 8 'Reset' 1 1 nil nil false nil nil nil 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 1213 5 498 151 51 21536 1346 #isEnabled: 8 #(false) 21536 1346 #text: 34 1 8 'Reset' 21536 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 94 2 0 0 2 0 0 0 169 2 0 0 27 0 0 0] 8 #() 1536 nil 29 1554 2848 -159 1616 151 7808 5 1648 51 170 192 34 6 18304 8 'activityList' 21536 8 'resetActivity' 20448 8 'activeLibrary' nil 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 9 53 498 1373 723 17824 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 178 2 0 0 131 1 0 0] 34 8 19760 20448 21536 20800 20080 21168 17936 18304 1536 nil 27 17824 nil nil nil 170 192 34 4 17824 8 'gciActivity' 800 8 'logins' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 624 34 2 8 1140916736 1 22208 8018 138 144 34 2 784 17808 nil 8080 nil nil 1 nil nil nil 22208 nil 8 1943988896 13730 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 22336 7 257 nil 13730 0 nil 22354 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 22400 7 257 nil 13600 nil nil nil nil nil #noIcons nil nil nil nil nil 1282 138 144 34 3 1346 #createAt:extent: 34 2 498 1 1 498 1389 783 22208 1346 #basicSelectionsByIndex: 34 1 8 #(2) 22208 1346 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 22208 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 182 2 0 0 135 1 0 0] 8 #() 1536 nil 27 1282 138 144 34 1 1346 #createAt:extent: 34 2 498 1 1 498 1389 783 624 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 182 2 0 0 135 1 0 0] 34 3 800 17824 22208 1536 nil 27 8 'cardContainer' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 13266 nil true 34 7 13314 1 4274 #openConsole 8 'Open &Console' 9369 1 nil nil nil 13314 1 4274 #addLogin 8 '&New' 9373 1 nil nil nil 13314 1 4274 #predefinedLogin 8 '&Predefined Login' 1 1 nil nil nil 13314 1 4274 #fileOpen 8 '&Open...' 9375 1 nil nil nil 13314 1 4274 #removeLogin 8 '&Remove' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 13314 1 4274 #exit 8 'E&xit' 17639 1 nil nil nil 8 '&File' nil 134217729 nil nil 10193 nil nil 13266 nil true 34 9 13314 1 4274 #editUndo 8 '&Undo' 9397 1 nil nil nil 13314 1 4274 #editRedo 8 '&Redo' 9395 1 nil nil nil 23186 4097 13314 1 4274 #editCut 8 'Cu&t' 9393 1 nil nil nil 13314 1 4274 #editCopy 8 '&Copy' 9351 1 nil nil nil 13314 1 4274 #editPaste 8 '&Paste' 9389 1 nil nil nil 13314 1 4274 #editDelete 8 'De&lete' 1629 1 nil nil nil 23186 4097 13314 1 4274 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '&Edit' nil 134217729 nil nil 10209 nil nil 13266 nil true 34 7 13314 1 4274 #openGemTalkHomePage 8 '&GemTalk Home Page' 1025 1 nil nil nil 13314 1 4274 #openJadeHomePage 8 '&Jade Home Page' 1 1 nil nil nil 23186 4097 13314 1 4274 #showGemNRS 8 'Show Gem &NRS' 1 1 nil nil nil 13314 1 4274 #showStoneNRS 8 'Show Stone N&RS' 1 1 nil nil nil 13314 1 4274 #openExeDir 8 'Open Executable &Directory' 1 1 nil nil nil 13314 1 4274 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 10223 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 9315 16466 nil true 15568 8 'icons\GS32.ico' nil 16466 nil true 15568 8 'icons\GS16.ico' nil nil nil 1 nil nil 1282 138 144 34 2 1346 #createAt:extent: 34 2 498 5119 21 498 1421 901 416 1346 #text: 34 1 8 'Jade Login' 416 1474 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 197 12 0 0 204 1 0 0] 34 1 624 1536 nil 27 )! !
!JadeLoginShell class categoriesFor: #icon!constants!public! !
!JadeLoginShell class categoriesFor: #resource_Default_view!public!resources-views! !

