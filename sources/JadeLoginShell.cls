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
	(#(#removeLogin) includes: query commandSymbol) ifTrue: [
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
!JadeLoginShell categoriesFor: #onViewOpened!public! !
!JadeLoginShell categoriesFor: #openGemTalkHomePage!public! !
!JadeLoginShell categoriesFor: #openJadeHomePage!public! !
!JadeLoginShell categoriesFor: #openLogin!public! !
!JadeLoginShell categoriesFor: #postLogin:!public! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13238272 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1401 841 551 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 608 nil nil nil 7 nil nil nil 608 852230 ##(Smalltalk.FramingLayout) 170 176 34 10 410 ##(Smalltalk.ListView) 34 30 nil 608 34 2 8 1409355853 1025 736 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 3 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #addLogin 8 '&Add' 1 1 nil nil nil 978 1 1010 #removeLogin 8 '&Remove' 1 1 nil nil nil 978 1 1010 #login 8 '&Login' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 736 nil 8 1730078832 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 6 920646 5 ##(Smalltalk.ListViewColumn) 8 'Version' 241 #left 1186 #gemVersion 8 #() 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a gemVersion <= b gemVersion]' 8 #[30 105 226 0 226 32 130 106] #gemVersion 1392 7 513 nil nil nil 736 nil 1 nil nil 1298 8 'Gem Host' 201 #left 1186 #gemHost 1360 1378 0 nil 1410 2 1 1184 8 'doIt' 8 '[:a :b | a gemHost <= b gemHost]' 8 #[30 105 226 0 226 32 130 106] #gemHost 1584 7 513 nil nil nil 736 nil 1 nil nil 1298 8 'NetLDI' 201 #left 1186 #gemService 1360 1378 0 nil 1410 2 1 1184 8 'doIt' 8 '[:a :b | a gemService <= b gemService]' 8 #[30 105 226 0 226 32 130 106] #gemService 1712 7 513 nil nil nil 736 nil 1 nil nil 1298 8 'Stone' 201 #left 1186 #stoneName 1360 1378 0 nil 1410 2 1 1184 8 'doIt' 8 '[:a :b | a stoneName <= b stoneName]' 8 #[30 105 226 0 226 32 130 106] #stoneName 1840 7 513 nil nil nil 736 nil 1 nil nil 1298 8 'User' 201 #left 1186 #gsUserID 1360 1378 0 nil 1410 2 1 1184 8 'doIt' 8 '[:a :b | a gsUserID <= b gsUserID]' 8 #[30 105 226 0 226 32 130 106] #gsUserID 1968 7 513 nil nil nil 736 nil 1 nil nil 1298 8 'Last Login' 441 #left 1186 #lastLoginString 8 #() 1378 0 nil 1410 2 1 1184 8 'doIt' 8 '[:a :b | a lastLogin <= b lastLogin]' 8 #[30 105 226 0 226 32 130 106] #lastLogin 2112 7 513 nil nil nil 736 nil 1 nil nil #report 848 nil 131169 nil 34 4 nil nil 514 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 51 514 1319 297 736 2290 #contextMenu: 34 1 944 736 2290 #text: 34 1 8 'Version' 736 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 147 2 0 0 173 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2562 #fixedParentRight -49 2562 #fixedPreviousBottom 1 2562 #fixedParentBottom 1 410 ##(Smalltalk.Toolbar) 34 25 nil 608 34 2 8 1409289036 131137 2640 nil 466 8 4278190080 nil 7 nil nil nil 2640 466 2720 8 1730129184 170 192 848 170 192 34 10 15665 853766 ##(Smalltalk.ToolbarButton) 15665 nil 2640 1 1010 #editCopy 8 'Copy' 1 1 nil 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'EditBar.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 514 193 33 3 15667 2818 15667 nil 2640 1 1010 #editPaste 8 'Paste' 1 1 nil 2896 5 15669 2818 15669 nil 2640 1 1010 #editDelete 8 'Delete' 1 1 nil 2896 11 15671 2818 15671 nil 2640 1 1010 #editUndo 8 'Undo' 1 1 nil 2896 7 15663 2818 15663 nil 2640 1 1010 #editCut 8 'Cut' 1 1 nil 2896 1 34 6 3168 2832 3024 3072 3120 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 2640 3 nil 1 170 176 34 2 2896 1 nil 1 nil 514 33 33 514 45 45 nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 1 1 514 241 51 2640 2290 #updateSizePosted 848 2640 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 120 0 0 0 25 0 0 0] 8 #() 2512 nil 27 2530 2576 1 2562 #fixedViewLeft 241 2562 #fixedParentTop 1 2562 #fixedViewTop 51 410 ##(Smalltalk.Toolbar) 34 25 nil 608 34 2 8 1140853580 131137 3568 nil 466 8 4278190080 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 3568 466 3648 8 1730129184 170 192 848 170 192 34 4 15673 1246982 ##(Smalltalk.ToolbarSystemButton) 15673 nil 3568 1 1010 #getHelpOn 8 'Get help on widget' 1 1 nil 1 23 15675 2818 15675 nil 3568 1 1010 #openHelp 8 'Jade Help' 1 1 nil 2882 nil true 2928 8 'Tools.bmp' 2976 nil 7 514 1857 33 71 34 2 3856 3904 170 176 34 4 1 1 3952 31 nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 241 1 514 1129 51 3568 2290 #updateSizePosted 848 3568 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 120 0 0 0 0 0 0 0 172 2 0 0 25 0 0 0] 8 #() 2512 nil 27 2530 2562 #fixedPreviousRight 1 2592 1 3536 1 3552 51 410 ##(Smalltalk.PushButton) 34 20 nil 608 34 2 8 1140924416 1 4320 nil nil nil 7 nil nil nil 4320 nil 8 1730188352 1010 #removeLogin 8 '-' 1 1 nil nil false nil nil nil 2226 138 144 34 3 2290 #createAt:extent: 34 2 514 1319 121 514 51 51 4320 2290 #isEnabled: 8 #(false) 4320 2290 #text: 34 1 8 '-' 4320 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 60 0 0 0 172 2 0 0 85 0 0 0] 8 #() 2512 nil 29 2530 2592 -49 3520 51 2608 11 3552 51 410 ##(Smalltalk.PushButton) 34 20 nil 608 34 2 8 1140924416 1 4688 nil nil nil 7 nil nil nil 4688 nil 8 1730188352 1010 #addLogin 8 '+' 1 1 nil nil false nil nil nil 2226 138 144 34 3 2290 #createAt:extent: 34 2 514 1319 61 514 51 51 4688 2290 #isEnabled: 8 #(false) 4688 2290 #text: 34 1 8 '+' 4688 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 30 0 0 0 172 2 0 0 55 0 0 0] 8 #() 2512 nil 29 2530 2592 -49 3520 51 3536 61 3552 51 170 192 34 6 2640 8 'Edit Tools' 736 8 'loginList' 3568 8 'Other Tools' nil 2226 138 144 34 1 2290 #createAt:extent: 34 2 514 1 1 514 1369 347 608 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 173 0 0 0] 34 5 2640 3568 736 4688 4320 2512 nil 27 39 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 5296 nil nil nil 7 nil nil nil 5296 674 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 5296 34 2 8 1140850688 131073 5408 nil nil nil 7 nil nil nil 5408 674 170 176 34 28 410 ##(Smalltalk.TextEdit) 34 16 nil 5408 34 2 8 1140916384 1025 5520 nil 466 8 4278190080 nil 7 nil nil nil 5520 nil 8 1730027008 852486 ##(Smalltalk.NullConverter) nil nil 1 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 391 259 514 285 45 5520 2290 #setMarginWidths: 34 1 8 #(3 3) 5520 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 129 0 0 0 81 1 0 0 151 0 0 0] 8 #() 2512 nil 27 2530 4304 11 2592 -9 2562 #fixedPreviousTop 1 3552 45 410 ##(Smalltalk.StaticText) 34 16 nil 5408 34 2 8 1140850944 1 5904 nil nil nil 7 nil nil nil 5904 nil 8 1730185888 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 11 213 514 371 41 5904 2290 #text: 34 1 8 'GemStone User ID:' 5904 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 106 0 0 0 190 0 0 0 126 0 0 0] 8 #() 2512 nil 27 2530 2576 11 3520 371 2608 3 3552 41 410 ##(Smalltalk.StaticText) 34 16 nil 5408 34 2 8 1140850944 1 6224 nil nil nil 7 nil nil nil 6224 nil 8 1730185888 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 11 313 514 371 41 6224 2290 #text: 34 1 8 'Monticello User Name:' 6224 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 156 0 0 0 190 0 0 0 176 0 0 0] 8 #() 2512 nil 27 2530 2576 11 3520 371 2608 11 3552 41 410 ##(Smalltalk.TextEdit) 34 16 nil 5408 34 2 8 1140916352 1025 6544 nil 466 8 4278190080 nil 7 nil nil nil 6544 nil 8 1730027008 5634 nil nil 1 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 391 213 514 285 45 6544 2290 #setMarginWidths: 34 1 8 #(3 3) 6544 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 106 0 0 0 81 1 0 0 128 0 0 0] 8 #() 2512 nil 27 2530 4304 11 2592 -9 5888 1 3552 45 410 ##(Smalltalk.StaticText) 34 16 nil 5408 34 2 8 1140850944 1 6896 nil nil nil 7 nil nil nil 6896 nil 8 1730185888 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 11 259 514 371 41 6896 2290 #text: 34 1 8 'GemStone Password:' 6896 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 129 0 0 0 190 0 0 0 149 0 0 0] 8 #() 2512 nil 27 2530 2576 11 3520 371 2608 3 3552 41 410 ##(Smalltalk.ComboBox) 34 17 nil 5408 34 2 8 1144063491 1025 7216 802 138 144 848 nil 880 466 8 4278190080 nil 7 nil nil nil 7216 nil 8 1730702432 1186 #displayString 8 #() 848 401 2226 138 144 34 1 2290 #createAt:extent: 34 2 514 391 11 514 285 51 7216 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 5 0 0 0 81 1 0 0 30 0 0 0] 8 #() 2512 nil 27 2530 4304 11 2592 -9 3536 11 3552 47 410 ##(Smalltalk.StaticText) 34 16 nil 5408 34 2 8 1140850944 1 7568 nil nil nil 7 nil nil nil 7568 nil 8 1730185888 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 11 113 514 371 31 7568 2290 #text: 34 1 8 'NetLDI Service Name or Port:' 7568 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 56 0 0 0 190 0 0 0 71 0 0 0] 8 #() 2512 nil 27 2530 2576 11 3520 371 2608 3 3552 31 410 ##(Smalltalk.StaticText) 34 16 nil 5408 34 2 8 1140850944 1 7888 nil nil nil 7 nil nil nil 7888 nil 8 1730185888 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 11 167 514 371 39 7888 2290 #text: 34 1 8 'Stone Name:' 7888 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 83 0 0 0 190 0 0 0 102 0 0 0] 8 #() 2512 nil 27 2530 2576 11 3520 371 2608 11 3552 39 410 ##(Smalltalk.StaticText) 34 16 nil 5408 34 2 8 1140850944 1 8208 nil nil nil 7 nil nil nil 8208 nil 8 1730185888 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 11 67 514 371 31 8208 2290 #text: 34 1 8 'RPC Gem Host Name or IP:' 8208 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 190 0 0 0 48 0 0 0] 8 #() 2512 nil 27 2530 2576 11 3520 371 2608 11 3552 31 410 ##(Smalltalk.TextEdit) 34 16 nil 5408 34 2 8 1140916352 1025 8528 nil 466 8 4278190080 nil 7 nil nil nil 8528 nil 8 1730027008 5634 nil nil 1 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 391 313 514 285 45 8528 2290 #setMarginWidths: 34 1 8 #(3 3) 8528 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 156 0 0 0 81 1 0 0 178 0 0 0] 8 #() 2512 nil 27 2530 4304 11 2592 -9 5888 1 3552 45 410 ##(Smalltalk.TextEdit) 34 16 nil 5408 34 2 8 1140916352 1025 8880 nil 466 6624 nil 7 nil nil nil 8880 nil 8 1730027008 5634 nil nil 1 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 391 167 514 285 45 8880 2290 #setMarginWidths: 34 1 8 #(3 3) 8880 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 83 0 0 0 81 1 0 0 105 0 0 0] 8 #() 2512 nil 27 2530 4304 11 2592 -9 5888 1 3552 45 410 ##(Smalltalk.StaticText) 34 16 nil 5408 34 2 8 1140850944 1 9216 nil nil nil 7 nil nil nil 9216 nil 8 1730185888 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 11 21 514 371 41 9216 2290 #text: 34 1 8 'GemStone/Smalltalk Version:' 9216 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 190 0 0 0 30 0 0 0] 8 #() 2512 nil 27 2530 2576 11 3520 371 3536 21 3552 41 410 ##(Smalltalk.TextEdit) 34 16 nil 5408 34 2 8 1140916352 1025 9536 nil 466 6624 nil 7 nil nil nil 9536 nil 8 1730027008 5634 nil nil 1 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 391 67 514 285 45 9536 2290 #setMarginWidths: 34 1 8 #(3 3) 9536 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 33 0 0 0 81 1 0 0 55 0 0 0] 8 #() 2512 nil 27 2530 4304 11 2592 -9 5888 1 3552 45 410 ##(Smalltalk.TextEdit) 34 16 nil 5408 34 2 8 1140916352 1025 9872 nil 466 6624 nil 7 nil nil nil 9872 nil 8 1730027008 5634 nil nil 1 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 391 113 514 285 45 9872 2290 #setMarginWidths: 34 1 8 #(3 3) 9872 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 56 0 0 0 81 1 0 0 78 0 0 0] 8 #() 2512 nil 27 2530 4304 11 2592 -9 5888 1 3552 45 170 192 34 14 5520 8 'password' 7216 8 'versionList' 9872 8 'gemService' 6544 8 'userID' 9536 8 'gemHost' 8880 8 'stoneName' 8528 8 'initials' nil 2226 138 144 34 1 2290 #createAt:extent: 34 2 514 1 1 514 685 367 5408 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 1 0 0 183 0 0 0] 34 14 9216 7216 8208 9536 7568 9872 7888 8880 5904 6544 6896 5520 6224 8528 2512 nil 27 2530 2576 1 2562 #relativeParentWidth 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 3536 1 2624 1 410 ##(Smalltalk.ContainerView) 34 15 nil 5296 34 2 8 1140850688 131073 10576 nil nil nil 7 nil nil nil 10576 674 170 176 34 26 410 ##(Smalltalk.StaticText) 34 16 nil 10576 34 2 8 1140850944 1 10688 nil nil nil 7 nil nil nil 10688 nil 8 1730185888 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 11 69 514 301 41 10688 2290 #text: 34 1 8 'Stone Host (from Gem):' 10688 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 34 0 0 0 155 0 0 0 54 0 0 0] 8 #() 2512 nil 27 2530 2576 11 3520 301 2608 11 3552 41 410 ##(Smalltalk.StaticText) 34 16 nil 10576 34 2 8 1140850944 1 11008 nil nil nil 7 nil nil nil 11008 nil 8 1730185888 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 11 115 514 301 31 11008 2290 #text: 34 1 8 'NetLDI Task Name:' 11008 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 57 0 0 0 155 0 0 0 72 0 0 0] 8 #() 2512 nil 27 2530 2576 11 3520 301 2608 3 3552 31 410 ##(Smalltalk.StaticText) 34 16 nil 10576 34 2 8 1140850944 1 11328 nil nil nil 7 nil nil nil 11328 nil 8 1730185888 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 321 163 514 161 41 11328 2290 #text: 34 1 8 'OS User ID:' 11328 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 81 0 0 0 240 0 0 0 101 0 0 0] 8 #() 2512 nil 27 2530 4304 11 3520 161 5888 1 3552 41 410 ##(Smalltalk.StaticText) 34 16 nil 10576 34 2 8 1140850944 1 11648 nil nil nil 7 nil nil nil 11648 nil 8 1730185888 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 321 209 514 161 41 11648 2290 #text: 34 1 8 'Password:' 11648 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 104 0 0 0 240 0 0 0 124 0 0 0] 8 #() 2512 nil 27 2530 2562 #fixedPreviousLeft -169 3520 161 2608 3 3552 41 410 ##(Smalltalk.TextEdit) 34 16 nil 10576 34 2 8 1140916384 1025 11984 nil 466 8 4278190080 nil 7 nil nil nil 11984 nil 8 1730027008 5634 nil nil 1 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 491 209 514 185 45 11984 2290 #setMarginWidths: 34 1 8 #(3 3) 11984 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 104 0 0 0 81 1 0 0 126 0 0 0] 8 #() 2512 nil 27 2530 4304 11 2592 -9 5888 1 3552 45 410 ##(Smalltalk.ContainerView) 34 15 nil 10576 34 2 8 1140850688 393217 12336 nil 466 8 4278190080 nil 7 nil nil nil 12336 656390 ##(Smalltalk.GridLayout) 3 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 12336 34 2 8 1141055497 1 12496 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 1186 #= 8 #() 1186 #hash 8 #() false 466 12416 nil 7 nil nil nil 12496 nil 8 1730188352 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 331 1 514 331 45 12496 2290 #text: 34 1 8 'Linked Gem (32-bit only)' 12496 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 74 1 0 0 22 0 0 0] 8 #() 2512 nil 27 8 'linked' 410 ##(Smalltalk.RadioButton) 34 16 nil 12336 34 2 8 1140924425 1 12960 12562 nil nil 12594 1186 #= 8 #() 1186 #hash 8 #() false 466 12416 nil 7 nil nil nil 12960 nil 8 1730188352 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 1 1 514 331 45 12960 2290 #text: 34 1 8 'RPC Gem' 12960 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 165 0 0 0 22 0 0 0] 8 #() 2512 nil 27 8 'remote' nil 2226 138 144 34 1 2290 #createAt:extent: 34 2 514 11 257 514 665 49 12336 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 128 0 0 0 81 1 0 0 152 0 0 0] 34 2 12960 12496 2512 nil 27 2530 2576 11 2592 -9 2608 5 3552 49 410 ##(Smalltalk.TextEdit) 34 16 nil 10576 34 2 8 1140916352 1025 13568 nil 466 8 4278190080 nil 7 nil nil nil 13568 nil 8 1730027008 5634 nil nil 1 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 491 163 514 185 45 13568 2290 #setMarginWidths: 34 1 8 #(3 3) 13568 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 81 0 0 0 81 1 0 0 103 0 0 0] 8 #() 2512 nil 27 2530 4304 11 2592 -9 5888 1 3552 45 410 ##(Smalltalk.TextEdit) 34 16 nil 10576 34 2 8 1140916352 1025 13920 nil 466 6624 nil 7 nil nil nil 13920 nil 8 1730027008 5634 nil nil 1 2226 138 144 34 3 2290 #createAt:extent: 34 2 514 321 115 514 355 45 13920 2290 #text: 34 1 8 'gemnetobject' 13920 2290 #setMarginWidths: 34 1 8 #(3 3) 13920 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 57 0 0 0 81 1 0 0 79 0 0 0] 8 #() 2512 nil 27 2530 4304 11 2592 -9 5888 1 3552 45 410 ##(Smalltalk.StaticPath) 34 17 nil 10576 34 2 8 1140850944 1 14304 nil nil nil 7 nil nil nil 14304 nil 8 1730185888 5634 nil nil nil 8 'XXX' 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 161 15 514 515 45 14304 2290 #text: 34 1 14400 14304 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 7 0 0 0 81 1 0 0 29 0 0 0] 8 #() 2512 nil 27 2530 2576 161 2592 -9 5888 11 3552 45 410 ##(Smalltalk.ContainerView) 34 15 nil 10576 34 2 8 1409286144 393217 14624 nil 466 8 4278190080 nil 7 nil nil nil 14624 12434 1 3 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 14624 34 2 8 1140924425 1 14768 12562 nil nil 12594 1186 #= 8 #() 1186 #hash 8 #() false 466 14704 nil 7 nil nil nil 14768 nil 8 1730188352 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 1 39 514 297 39 14768 2290 #text: 34 1 8 'Native OS User' 14768 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 148 0 0 0 38 0 0 0] 8 #() 2512 nil 27 8 'member' 410 ##(Smalltalk.RadioButton) 34 16 nil 14624 34 2 8 1141055497 1 15200 12562 nil nil 12594 1186 #= 8 #() 1186 #hash 8 #() false 466 14704 nil 7 nil nil nil 15200 nil 8 1730188352 5634 nil nil nil 2226 138 144 34 2 2290 #createAt:extent: 34 2 514 1 1 514 297 39 15200 2290 #text: 34 1 8 'NetLDI Guest' 15200 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 148 0 0 0 19 0 0 0] 8 #() 2512 nil 27 8 'guest' nil 2226 138 144 34 1 2290 #createAt:extent: 34 2 514 11 163 514 301 81 14624 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 81 0 0 0 155 0 0 0 121 0 0 0] 34 2 15200 14768 2512 nil 27 2530 2576 11 3520 301 2608 5 3552 81 410 ##(Smalltalk.TextEdit) 34 16 nil 10576 34 2 8 1140916352 1025 15808 nil 466 6624 nil 7 nil nil nil 15808 nil 8 1730027008 5634 nil nil 1 2226 138 144 34 3 2290 #createAt:extent: 34 2 514 321 69 514 355 45 15808 2290 #text: 34 1 8 'localhost' 15808 2290 #setMarginWidths: 34 1 8 #(3 3) 15808 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 34 0 0 0 81 1 0 0 56 0 0 0] 8 #() 2512 nil 27 2530 4304 11 2592 -9 5888 1 3552 45 410 ##(Smalltalk.PushButton) 34 20 nil 10576 34 2 8 1140924416 1 16192 nil nil nil 7 nil nil nil 16192 nil 8 1730188352 1010 #debugGCI 8 'Debug GCI' 1 1 nil nil false nil nil nil 2226 138 144 34 3 2290 #createAt:extent: 34 2 514 11 5 514 141 51 16192 2290 #isEnabled: 8 #(false) 16192 2290 #text: 34 1 8 'Debug GCI' 16192 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 75 0 0 0 27 0 0 0] 8 #() 2512 nil 29 2530 2576 11 3520 141 2608 5 3552 51 410 ##(Smalltalk.PushButton) 34 20 nil 10576 34 2 8 1140924416 1 16560 nil nil nil 7 nil nil nil 16560 nil 8 1730188352 1010 #login 8 'Login' 1 1 nil nil true nil nil nil 2226 138 144 34 3 2290 #createAt:extent: 34 2 514 11 305 514 665 61 16560 2290 #isEnabled: 8 #(false) 16560 2290 #text: 34 1 8 'Login' 16560 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 152 0 0 0 81 1 0 0 182 0 0 0] 8 #() 2512 nil 29 2530 2576 11 2592 -9 2608 1 3552 61 170 192 34 14 13568 8 'hostUserID' 15808 8 'stoneHost' 11984 8 'hostPassword' 13920 8 'gemTask' 14304 8 'debugPath' 12336 8 'gemType' 14624 8 'loginType' nil 2226 138 144 34 1 2290 #createAt:extent: 34 2 514 685 1 514 685 367 10576 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 86 1 0 0 0 0 0 0 172 2 0 0 183 0 0 0] 34 13 16192 14304 10688 15808 11008 13920 14624 11328 13568 11648 11984 12336 16560 2512 nil 27 2530 10528 10546 8 0 0 0 0 0 0 224 63 2592 1 3536 1 2624 1 170 192 34 4 5408 8 'Basic' 10576 8 'Advanced' nil 2226 138 144 34 1 2290 #createAt:extent: 34 2 514 1 357 514 1369 367 5296 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 178 0 0 0 172 2 0 0 105 1 0 0] 34 2 5408 10576 2512 nil 27 41 true 170 192 34 4 608 8 'top' 5296 8 'bottom' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 930 nil true 34 5 978 1 1010 #addLogin 8 '&New' 9373 1 nil nil nil 978 1 1010 #fileOpen 8 '&Open...' 9375 1 nil nil nil 978 1 1010 #removeLogin 8 '&Remove' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 978 1 1010 #exit 8 'E&xit' 17639 1 nil nil nil 8 '&Login' nil 134217729 nil nil 15627 nil nil 930 nil true 34 9 978 1 1010 #editUndo 8 '&Undo' 9397 1 nil nil nil 978 1 1010 #editRedo 8 '&Redo' 9395 1 nil nil nil 17778 4097 978 1 1010 #editCut 8 'Cu&t' 9393 1 nil nil nil 978 1 1010 #editCopy 8 '&Copy' 9351 1 nil nil nil 978 1 1010 #editPaste 8 '&Paste' 9389 1 nil nil nil 978 1 1010 #editDelete 8 'De&lete' 1629 1 nil nil nil 17778 4097 978 1 1010 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '&Edit' nil 134217729 nil nil 15643 nil nil 930 nil true 34 10 978 1 1010 #helpContents 8 '&Contents' 1 1 nil nil nil 978 1 1010 #helpOnThisTool 8 '&On this Tool' 1249 1 nil nil nil 978 1 1010 #helpWhatsThis 8 '&What''s This?' 5345 1 nil nil nil 17778 4097 978 1 1010 #openGemTalkHomePage 8 '&GemTalk Home Page' 1025 1 nil nil nil 978 1 1010 #openJadeHomePage 8 '&Jade Home Page' 1 1 nil nil nil 17778 4097 978 1 1010 #showGemNRS 8 'Show Gem &NRS' 1 1 nil nil nil 978 1 1010 #showStoneNRS 8 'Show Stone N&RS' 1 1 nil nil nil 978 1 1010 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 15661 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 10321 263494 3 ##(Smalltalk.Icon) nil true 2928 8 'icons\GS32.ico' nil 18770 nil true 2928 8 'icons\GS16.ico' nil nil nil 1 nil nil 2226 138 144 34 3 2290 #createAt:extent: 34 2 514 5119 21 514 1401 841 416 2290 #text: 34 1 8 'Jade Login' 416 2290 #updateMenuBar 848 416 2450 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 187 12 0 0 174 1 0 0] 34 3 608 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 19072 nil 466 8 4278190080 nil 519 nil nil nil 19072 2226 138 144 34 1 2290 #createAt:extent: 34 2 514 1 347 514 1369 11 19072 2450 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 173 0 0 0 172 2 0 0 178 0 0 0] 8 #() 2512 nil 27 5296 2512 nil 27 )! !
!JadeLoginShell class categoriesFor: #icon!constants!public! !
!JadeLoginShell class categoriesFor: #resource_Default_view!public!resources-views! !

