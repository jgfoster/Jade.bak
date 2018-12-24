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

openGemTalkHomePage

	ShellLibrary default 
		shellOpen: 'https://gemtalksystems.com/' 
		directory: ''.
!

openJadeHomePage

	ShellLibrary default 
		shellOpen: 'https://github.com/jgfoster/Jade' 
		directory: ''.
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

	'string1' equals: 'string2'.
	1 isImmediate.
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13238272 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1401 841 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 608 nil nil nil 7 nil nil nil 608 852230 ##(Smalltalk.FramingLayout) 170 176 34 10 410 ##(Smalltalk.ListView) 34 30 nil 608 34 2 8 1409355853 1025 736 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 480 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 4 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #copyXML 8 '&Copy XML' 1 1 nil nil nil 946 1 978 #addLogin 8 '&Add' 1 1 nil nil nil 946 1 978 #removeLogin 8 '&Remove' 1 1 nil nil nil 946 1 978 #login 8 '&Login' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 736 nil 8 1723715232 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 6 920646 5 ##(Smalltalk.ListViewColumn) 8 'Version' 241 #left 1202 #gemVersion 8 #() 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a gemVersion <= b gemVersion]' 8 #[30 105 226 0 226 32 130 106] #gemVersion 1408 7 513 nil nil nil 736 nil 1 nil nil 1314 8 'Gem Host' 201 #left 1202 #gemHost 1376 1394 0 nil 1426 2 1 1200 8 'doIt' 8 '[:a :b | a gemHost <= b gemHost]' 8 #[30 105 226 0 226 32 130 106] #gemHost 1600 7 513 nil nil nil 736 nil 1 nil nil 1314 8 'NetLDI' 201 #left 1202 #gemService 1376 1394 0 nil 1426 2 1 1200 8 'doIt' 8 '[:a :b | a gemService <= b gemService]' 8 #[30 105 226 0 226 32 130 106] #gemService 1728 7 513 nil nil nil 736 nil 1 nil nil 1314 8 'Stone' 201 #left 1202 #stoneName 1376 1394 0 nil 1426 2 1 1200 8 'doIt' 8 '[:a :b | a stoneName <= b stoneName]' 8 #[30 105 226 0 226 32 130 106] #stoneName 1856 7 513 nil nil nil 736 nil 1 nil nil 1314 8 'User' 201 #left 1202 #gsUserID 1376 1394 0 nil 1426 2 1 1200 8 'doIt' 8 '[:a :b | a gsUserID <= b gsUserID]' 8 #[30 105 226 0 226 32 130 106] #gsUserID 1984 7 513 nil nil nil 736 nil 1 nil nil 1314 8 'Last Login' 441 #left 1202 #lastLoginString 8 #() 1394 0 nil 1426 2 1 1200 8 'doIt' 8 '[:a :b | a lastLogin <= b lastLogin]' 8 #[30 105 226 0 226 32 130 106] #lastLogin 2128 7 513 nil nil nil 736 nil 1 nil nil #report 848 nil 131169 nil 34 4 nil nil 514 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 51 514 1319 297 736 2306 #contextMenu: 34 1 912 736 2306 #text: 34 1 8 'Version' 736 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 147 2 0 0 173 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2578 #fixedParentRight -49 2578 #fixedPreviousBottom 1 2578 #fixedParentBottom 1 410 ##(Smalltalk.Toolbar) 34 25 nil 608 34 2 8 1140853580 131137 2656 nil 480 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 2656 480 8 1723708560 170 192 848 170 192 34 4 20623 1246790 1 ##(Smalltalk.ToolbarSystemButton) 2656 1 978 #getHelpOn 8 'Get help on widget' 1 1 nil 20623 1 23 20625 1246790 1 ##(Smalltalk.ToolbarBitmapButton) 2656 1 978 #openHelp 8 'Jade Help' 1 1 nil 20625 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 514 1857 33 71 34 2 2896 2960 nil nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 241 1 514 1129 51 2656 2306 #updateSizePosted 848 2656 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 120 0 0 0 0 0 0 0 172 2 0 0 25 0 0 0] 8 #() 2528 nil 27 2546 2578 #fixedPreviousRight 1 2608 1 2578 #fixedParentTop 1 2578 #fixedViewTop 51 410 ##(Smalltalk.Toolbar) 34 25 nil 608 34 2 8 1409289036 131137 3472 nil 480 nil 7 nil nil nil 3472 480 8 1723708560 170 192 848 170 192 34 10 20615 1115206 1 ##(Smalltalk.ToolbarIconButton) 3472 1 978 #editCopy 8 'Copy' 1 1 263494 3 ##(Smalltalk.Icon) nil true 3056 8 'icons\EditCopy.ico' nil 20615 20617 3602 3472 1 978 #editPaste 8 'Paste' 1 1 3666 nil true 3056 8 'icons\EditPasteLink.ico' nil 20617 20619 3602 3472 1 978 #editDelete 8 'Delete' 1 1 3666 nil true 3056 8 'icons\EditDelete.ico' nil 20619 20621 3602 3472 1 978 #editUndo 8 'Undo' 1 1 3666 nil true 3056 8 'icons\EditUndo.ico' nil 20621 20613 3602 3472 1 978 #editCut 8 'Cut' 1 1 3666 nil true 3056 8 'icons\EditCut.ico' nil 20613 34 6 3952 3616 3712 3792 3872 1049158 1 ##(Smalltalk.ToolbarSeparator) 3472 1 nil nil 1 nil 514 33 33 514 45 45 nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 1 1 514 241 51 3472 2306 #updateSizePosted 848 3472 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 120 0 0 0 25 0 0 0] 8 #() 2528 nil 27 2546 2592 1 2578 #fixedViewLeft 241 3440 1 3456 51 410 ##(Smalltalk.PushButton) 34 20 nil 608 34 2 8 1140924416 1 4320 nil nil nil 7 nil nil nil 4320 nil 8 1723692192 978 #addLogin 8 '+' 1 1 nil nil false nil nil nil 2242 138 144 34 3 2306 #createAt:extent: 34 2 514 1319 61 514 51 51 4320 2306 #isEnabled: 8 #(false) 4320 2306 #text: 34 1 8 '+' 4320 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 30 0 0 0 172 2 0 0 55 0 0 0] 8 #() 2528 nil 29 2546 2608 -49 4304 51 3440 61 3456 51 410 ##(Smalltalk.PushButton) 34 20 nil 608 34 2 8 1140924416 1 4688 nil nil nil 7 nil nil nil 4688 nil 8 1723692192 978 #removeLogin 8 '-' 1 1 nil nil false nil nil nil 2242 138 144 34 3 2306 #createAt:extent: 34 2 514 1319 121 514 51 51 4688 2306 #isEnabled: 8 #(false) 4688 2306 #text: 34 1 8 '-' 4688 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 60 0 0 0 172 2 0 0 85 0 0 0] 8 #() 2528 nil 29 2546 2608 -49 4304 51 2624 11 3456 51 170 192 34 6 2656 8 'Other Tools' 3472 8 'Edit Tools' 736 8 'loginList' nil 2242 138 144 34 1 2306 #createAt:extent: 34 2 514 1 1 514 1369 347 608 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 173 0 0 0] 34 5 3472 2656 736 4320 4688 2528 nil 27 39 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 5296 nil nil nil 7 nil nil nil 5296 674 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 5296 34 2 8 1140850688 131073 5408 nil nil nil 7 nil nil nil 5408 674 170 176 34 26 410 ##(Smalltalk.TextEdit) 34 16 nil 5408 34 2 8 1140916352 1025 5520 nil 480 nil 7 nil nil nil 5520 nil 8 1723783840 852486 ##(Smalltalk.NullConverter) nil nil 1 2242 138 144 34 3 2306 #createAt:extent: 34 2 514 321 115 514 355 45 5520 2306 #text: 34 1 8 'gemnetobject' 5520 2306 #setMarginWidths: 34 1 8 #(3 3) 5520 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 57 0 0 0 81 1 0 0 79 0 0 0] 8 #() 2528 nil 27 2546 3424 11 2608 -9 2578 #fixedPreviousTop 1 3456 45 410 ##(Smalltalk.ContainerView) 34 15 nil 5408 34 2 8 1140850688 393217 5920 nil 480 nil 7 nil nil nil 5920 656390 ##(Smalltalk.GridLayout) 3 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 5920 34 2 8 1140924425 1 6048 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 1202 #= 8 #() 1202 #hash 8 #() false 480 nil 7 nil nil nil 6048 nil 8 1723692192 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 1 1 514 331 45 6048 2306 #text: 34 1 8 'RPC Gem' 6048 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 165 0 0 0 22 0 0 0] 8 #() 2528 nil 27 8 'remote' 410 ##(Smalltalk.RadioButton) 34 16 nil 5920 34 2 8 1141055497 1 6496 6114 nil nil 6146 1202 #= 8 #() 1202 #hash 8 #() false 480 nil 7 nil nil nil 6496 nil 8 1723692192 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 331 1 514 331 45 6496 2306 #text: 34 1 8 'Linked Gem (32-bit only)' 6496 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 74 1 0 0 22 0 0 0] 8 #() 2528 nil 27 8 'linked' nil 2242 138 144 34 1 2306 #createAt:extent: 34 2 514 11 257 514 665 49 5920 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 128 0 0 0 81 1 0 0 152 0 0 0] 34 2 6048 6496 2528 nil 27 2546 2592 11 2608 -9 2624 5 3456 49 410 ##(Smalltalk.StaticPath) 34 17 nil 5408 34 2 8 1140850944 1 7088 nil nil nil 7 nil nil nil 7088 nil 8 1723628176 5602 nil nil nil 8 'XXX' 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 161 15 514 515 45 7088 2306 #text: 34 1 7184 7088 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 7 0 0 0 81 1 0 0 29 0 0 0] 8 #() 2528 nil 27 2546 2592 161 2608 -9 5904 11 3456 45 410 ##(Smalltalk.TextEdit) 34 16 nil 5408 34 2 8 1140916384 1025 7408 nil 480 nil 7 nil nil nil 7408 nil 8 1723783840 5602 nil nil 1 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 491 209 514 185 45 7408 2306 #setMarginWidths: 34 1 8 #(3 3) 7408 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 104 0 0 0 81 1 0 0 126 0 0 0] 8 #() 2528 nil 27 2546 3424 11 2608 -9 5904 1 3456 45 410 ##(Smalltalk.StaticText) 34 16 nil 5408 34 2 8 1140850944 1 7728 nil nil nil 7 nil nil nil 7728 nil 8 1723628176 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 11 69 514 301 41 7728 2306 #text: 34 1 8 'Stone Host (from Gem):' 7728 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 34 0 0 0 155 0 0 0 54 0 0 0] 8 #() 2528 nil 27 2546 2592 11 4304 301 2624 11 3456 41 410 ##(Smalltalk.StaticText) 34 16 nil 5408 34 2 8 1140850944 1 8048 nil nil nil 7 nil nil nil 8048 nil 8 1723628176 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 11 115 514 301 31 8048 2306 #text: 34 1 8 'NetLDI Task Name:' 8048 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 57 0 0 0 155 0 0 0 72 0 0 0] 8 #() 2528 nil 27 2546 2592 11 4304 301 2624 3 3456 31 410 ##(Smalltalk.TextEdit) 34 16 nil 5408 34 2 8 1140916352 1025 8368 nil 480 nil 7 nil nil nil 8368 nil 8 1723783840 5602 nil nil 1 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 491 163 514 185 45 8368 2306 #setMarginWidths: 34 1 8 #(3 3) 8368 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 81 0 0 0 81 1 0 0 103 0 0 0] 8 #() 2528 nil 27 2546 3424 11 2608 -9 5904 1 3456 45 410 ##(Smalltalk.ContainerView) 34 15 nil 5408 34 2 8 1409286144 393217 8688 nil 480 nil 7 nil nil nil 8688 5986 1 3 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 8688 34 2 8 1141055497 1 8800 6114 nil nil 6146 1202 #= 8 #() 1202 #hash 8 #() false 480 nil 7 nil nil nil 8800 nil 8 1723692192 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 1 1 514 297 39 8800 2306 #text: 34 1 8 'NetLDI Guest' 8800 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 148 0 0 0 19 0 0 0] 8 #() 2528 nil 27 8 'guest' 410 ##(Smalltalk.RadioButton) 34 16 nil 8688 34 2 8 1140924425 1 9216 6114 nil nil 6146 1202 #= 8 #() 1202 #hash 8 #() false 480 nil 7 nil nil nil 9216 nil 8 1723692192 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 1 39 514 297 39 9216 2306 #text: 34 1 8 'Native OS User' 9216 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 148 0 0 0 38 0 0 0] 8 #() 2528 nil 27 8 'member' nil 2242 138 144 34 1 2306 #createAt:extent: 34 2 514 11 163 514 301 81 8688 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 81 0 0 0 155 0 0 0 121 0 0 0] 34 2 8800 9216 2528 nil 27 2546 2592 11 4304 301 2624 5 3456 81 410 ##(Smalltalk.TextEdit) 34 16 nil 5408 34 2 8 1140916352 1025 9808 nil 480 nil 7 nil nil nil 9808 nil 8 1723783840 5602 nil nil 1 2242 138 144 34 3 2306 #createAt:extent: 34 2 514 321 69 514 355 45 9808 2306 #text: 34 1 8 'localhost' 9808 2306 #setMarginWidths: 34 1 8 #(3 3) 9808 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 34 0 0 0 81 1 0 0 56 0 0 0] 8 #() 2528 nil 27 2546 3424 11 2608 -9 5904 1 3456 45 410 ##(Smalltalk.StaticText) 34 16 nil 5408 34 2 8 1140850944 1 10176 nil nil nil 7 nil nil nil 10176 nil 8 1723628176 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 321 209 514 161 41 10176 2306 #text: 34 1 8 'Password:' 10176 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 104 0 0 0 240 0 0 0 124 0 0 0] 8 #() 2528 nil 27 2546 2578 #fixedPreviousLeft -169 4304 161 2624 3 3456 41 410 ##(Smalltalk.StaticText) 34 16 nil 5408 34 2 8 1140850944 1 10512 nil nil nil 7 nil nil nil 10512 nil 8 1723628176 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 321 163 514 161 41 10512 2306 #text: 34 1 8 'OS User ID:' 10512 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 81 0 0 0 240 0 0 0 101 0 0 0] 8 #() 2528 nil 27 2546 3424 11 4304 161 5904 1 3456 41 410 ##(Smalltalk.PushButton) 34 20 nil 5408 34 2 8 1140924416 1 10832 nil nil nil 7 nil nil nil 10832 nil 8 1723692192 978 #login 8 'Login' 1 1 nil nil true nil nil nil 2242 138 144 34 3 2306 #createAt:extent: 34 2 514 11 305 514 665 61 10832 2306 #isEnabled: 8 #(false) 10832 2306 #text: 34 1 8 'Login' 10832 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 152 0 0 0 81 1 0 0 182 0 0 0] 8 #() 2528 nil 29 2546 2592 11 2608 -9 2624 1 3456 61 410 ##(Smalltalk.PushButton) 34 20 nil 5408 34 2 8 1140924416 1 11200 nil nil nil 7 nil nil nil 11200 nil 8 1723692192 978 #debugGCI 8 'Debug GCI' 1 1 nil nil false nil nil nil 2242 138 144 34 3 2306 #createAt:extent: 34 2 514 11 5 514 141 51 11200 2306 #isEnabled: 8 #(false) 11200 2306 #text: 34 1 8 'Debug GCI' 11200 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 75 0 0 0 27 0 0 0] 8 #() 2528 nil 29 2546 2592 11 4304 141 2624 5 3456 51 170 192 34 14 5520 8 'gemTask' 9808 8 'stoneHost' 8368 8 'hostUserID' 8688 8 'loginType' 7088 8 'debugPath' 5920 8 'gemType' 7408 8 'hostPassword' nil 2242 138 144 34 1 2306 #createAt:extent: 34 2 514 685 1 514 685 367 5408 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 86 1 0 0 0 0 0 0 172 2 0 0 183 0 0 0] 34 13 11200 7088 7728 9808 8048 5520 8688 10512 8368 10176 7408 5920 10832 2528 nil 27 2546 2578 #relativeParentWidth 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 2608 1 3440 1 2640 1 410 ##(Smalltalk.ContainerView) 34 15 nil 5296 34 2 8 1140850688 131073 11936 nil nil nil 7 nil nil nil 11936 674 170 176 34 28 410 ##(Smalltalk.TextEdit) 34 16 nil 11936 34 2 8 1140916352 1025 12048 nil 480 nil 7 nil nil nil 12048 nil 8 1723783840 5602 nil nil 1 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 391 67 514 285 45 12048 2306 #setMarginWidths: 34 1 8 #(3 3) 12048 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 33 0 0 0 81 1 0 0 55 0 0 0] 8 #() 2528 nil 27 2546 3424 11 2608 -9 5904 1 3456 45 410 ##(Smalltalk.StaticText) 34 16 nil 11936 34 2 8 1140850944 1 12368 nil nil nil 7 nil nil nil 12368 nil 8 1723628176 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 11 313 514 371 41 12368 2306 #text: 34 1 8 'Monticello User Name:' 12368 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 156 0 0 0 190 0 0 0 176 0 0 0] 8 #() 2528 nil 27 2546 2592 11 4304 371 2624 11 3456 41 410 ##(Smalltalk.StaticText) 34 16 nil 11936 34 2 8 1140850944 1 12688 nil nil nil 7 nil nil nil 12688 nil 8 1723628176 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 11 213 514 371 41 12688 2306 #text: 34 1 8 'GemStone User ID:' 12688 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 106 0 0 0 190 0 0 0 126 0 0 0] 8 #() 2528 nil 27 2546 2592 11 4304 371 2624 3 3456 41 410 ##(Smalltalk.StaticText) 34 16 nil 11936 34 2 8 1140850944 1 13008 nil nil nil 7 nil nil nil 13008 nil 8 1723628176 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 11 21 514 371 41 13008 2306 #text: 34 1 8 'GemStone/Smalltalk Version:' 13008 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 190 0 0 0 30 0 0 0] 8 #() 2528 nil 27 2546 2592 11 4304 371 3440 21 3456 41 410 ##(Smalltalk.TextEdit) 34 16 nil 11936 34 2 8 1140916352 1025 13328 nil 480 nil 7 nil nil nil 13328 nil 8 1723783840 5602 nil nil 1 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 391 313 514 285 45 13328 2306 #setMarginWidths: 34 1 8 #(3 3) 13328 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 156 0 0 0 81 1 0 0 178 0 0 0] 8 #() 2528 nil 27 2546 3424 11 2608 -9 5904 1 3456 45 410 ##(Smalltalk.StaticText) 34 16 nil 11936 34 2 8 1140850944 1 13648 nil nil nil 7 nil nil nil 13648 nil 8 1723628176 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 11 167 514 371 39 13648 2306 #text: 34 1 8 'Stone Name:' 13648 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 83 0 0 0 190 0 0 0 102 0 0 0] 8 #() 2528 nil 27 2546 2592 11 4304 371 2624 11 3456 39 410 ##(Smalltalk.TextEdit) 34 16 nil 11936 34 2 8 1140916384 1025 13968 nil 480 nil 7 nil nil nil 13968 nil 8 1723783840 5602 nil nil 1 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 391 259 514 285 45 13968 2306 #setMarginWidths: 34 1 8 #(3 3) 13968 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 129 0 0 0 81 1 0 0 151 0 0 0] 8 #() 2528 nil 27 2546 3424 11 2608 -9 5904 1 3456 45 410 ##(Smalltalk.TextEdit) 34 16 nil 11936 34 2 8 1140916352 1025 14288 nil 480 nil 7 nil nil nil 14288 nil 8 1723783840 5602 nil nil 1 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 391 113 514 285 45 14288 2306 #setMarginWidths: 34 1 8 #(3 3) 14288 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 56 0 0 0 81 1 0 0 78 0 0 0] 8 #() 2528 nil 27 2546 3424 11 2608 -9 5904 1 3456 45 410 ##(Smalltalk.TextEdit) 34 16 nil 11936 34 2 8 1140916352 1025 14608 nil 480 nil 7 nil nil nil 14608 nil 8 1723783840 5602 nil nil 1 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 391 167 514 285 45 14608 2306 #setMarginWidths: 34 1 8 #(3 3) 14608 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 83 0 0 0 81 1 0 0 105 0 0 0] 8 #() 2528 nil 27 2546 3424 11 2608 -9 5904 1 3456 45 410 ##(Smalltalk.StaticText) 34 16 nil 11936 34 2 8 1140850944 1 14928 nil nil nil 7 nil nil nil 14928 nil 8 1723628176 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 11 259 514 371 41 14928 2306 #text: 34 1 8 'GemStone Password:' 14928 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 129 0 0 0 190 0 0 0 149 0 0 0] 8 #() 2528 nil 27 2546 2592 11 4304 371 2624 3 3456 41 410 ##(Smalltalk.ComboBox) 34 17 nil 11936 34 2 8 1144063491 1025 15248 802 138 144 848 nil 880 480 nil 7 nil nil nil 15248 nil 8 1723506192 1202 #displayString 8 #() 848 401 2242 138 144 34 1 2306 #createAt:extent: 34 2 514 391 11 514 285 51 15248 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 5 0 0 0 81 1 0 0 30 0 0 0] 8 #() 2528 nil 27 2546 3424 11 2608 -9 3440 11 3456 47 410 ##(Smalltalk.TextEdit) 34 16 nil 11936 34 2 8 1140916352 1025 15568 nil 480 nil 7 nil nil nil 15568 nil 8 1723783840 5602 nil nil 1 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 391 213 514 285 45 15568 2306 #setMarginWidths: 34 1 8 #(3 3) 15568 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 106 0 0 0 81 1 0 0 128 0 0 0] 8 #() 2528 nil 27 2546 3424 11 2608 -9 5904 1 3456 45 410 ##(Smalltalk.StaticText) 34 16 nil 11936 34 2 8 1140850944 1 15888 nil nil nil 7 nil nil nil 15888 nil 8 1723628176 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 11 113 514 371 31 15888 2306 #text: 34 1 8 'NetLDI Service Name or Port:' 15888 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 56 0 0 0 190 0 0 0 71 0 0 0] 8 #() 2528 nil 27 2546 2592 11 4304 371 2624 3 3456 31 410 ##(Smalltalk.StaticText) 34 16 nil 11936 34 2 8 1140850944 1 16208 nil nil nil 7 nil nil nil 16208 nil 8 1723628176 5602 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 514 11 67 514 371 31 16208 2306 #text: 34 1 8 'RPC Gem Host Name or IP:' 16208 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 190 0 0 0 48 0 0 0] 8 #() 2528 nil 27 2546 2592 11 4304 371 2624 11 3456 31 170 192 34 14 13328 8 'initials' 14608 8 'stoneName' 14288 8 'gemService' 15568 8 'userID' 12048 8 'gemHost' 13968 8 'password' 15248 8 'versionList' nil 2242 138 144 34 1 2306 #createAt:extent: 34 2 514 1 1 514 685 367 11936 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 1 0 0 183 0 0 0] 34 14 13008 15248 16208 12048 15888 14288 13648 14608 12688 15568 14928 13968 12368 13328 2528 nil 27 2546 2592 1 11888 11906 8 0 0 0 0 0 0 224 63 3440 1 2640 1 170 192 34 4 5408 8 'Advanced' 11936 8 'Basic' nil 2242 138 144 34 1 2306 #createAt:extent: 34 2 514 1 357 514 1369 367 5296 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 178 0 0 0 172 2 0 0 105 1 0 0] 34 2 11936 5408 2528 nil 27 41 true 170 192 34 4 608 8 'top' 5296 8 'bottom' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 898 nil true 34 6 946 1 978 #addLogin 8 '&New' 9373 1 nil nil nil 946 1 978 #predefinedLogin 8 '&Predefined Login' 1 1 nil nil nil 946 1 978 #fileOpen 8 '&Open...' 9375 1 nil nil nil 946 1 978 #removeLogin 8 '&Remove' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 946 1 978 #exit 8 'E&xit' 17639 1 nil nil nil 8 '&Login' nil 134217729 nil nil 20577 nil nil 898 nil true 34 9 946 1 978 #editUndo 8 '&Undo' 9397 1 nil nil nil 946 1 978 #editRedo 8 '&Redo' 9395 1 nil nil nil 17426 4097 946 1 978 #editCut 8 'Cu&t' 9393 1 nil nil nil 946 1 978 #editCopy 8 '&Copy' 9351 1 nil nil nil 946 1 978 #editPaste 8 '&Paste' 9389 1 nil nil nil 946 1 978 #editDelete 8 'De&lete' 1629 1 nil nil nil 17426 4097 946 1 978 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '&Edit' nil 134217729 nil nil 20593 nil nil 898 nil true 34 10 946 1 978 #helpContents 8 '&Contents' 1 1 nil nil nil 946 1 978 #helpOnThisTool 8 '&On this Tool' 1249 1 nil nil nil 946 1 978 #helpWhatsThis 8 '&What''s This?' 5345 1 nil nil nil 17426 4097 946 1 978 #openGemTalkHomePage 8 '&GemTalk Home Page' 1025 1 nil nil nil 946 1 978 #openJadeHomePage 8 '&Jade Home Page' 1 1 nil nil nil 17426 4097 946 1 978 #showGemNRS 8 'Show Gem &NRS' 1 1 nil nil nil 946 1 978 #showStoneNRS 8 'Show Stone N&RS' 1 1 nil nil nil 946 1 978 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 20611 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 12523 3666 nil true 3056 8 'icons\GS32.ico' nil 3666 nil true 3056 8 'icons\GS16.ico' nil nil nil 1 nil nil 2242 138 144 34 3 2306 #createAt:extent: 34 2 514 2879 21 514 1401 841 416 2306 #text: 34 1 8 'Jade Login' 416 2306 #updateMenuBar 848 416 2466 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 91 8 0 0 174 1 0 0] 34 3 608 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 18704 nil 480 nil 519 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 18704 nil 1 #left nil nil nil 514 1 1 514 9 9 nil 18800 nil 2242 138 144 34 1 2306 #createAt:extent: 34 2 514 1 347 514 1369 11 18704 2466 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 173 0 0 0 172 2 0 0 178 0 0 0] 8 #() 2528 nil 27 5296 2528 nil 27 )! !
!JadeLoginShell class categoriesFor: #icon!constants!public! !
!JadeLoginShell class categoriesFor: #resource_Default_view!public!resources-views! !

