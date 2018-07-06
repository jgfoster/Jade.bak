"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #JadeLoginShell
	instanceVariableNames: 'debugPathPresenter gemHostPresenter gemServicePresenter gemTaskPresenter gemTypePresenter hostPasswordPresenter hostUserIDPresenter initialsPresenter inUpdateView isModified loginListPresenter loginTypePresenter passwordPresenter stoneHostPresenter stoneNamePresenter userIDPresenter versionListPresenter'
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
	stoneHostPresenter 		:= self add: TextPresenter 		new name: 'stoneHost'.
	stoneNamePresenter 		:= self add: TextPresenter 		new name: 'stoneName'.
	gemTypePresenter 		:= self add: RadioButtonSetPresenter 	new name: 'gemType'.
	gemHostPresenter 		:= self add: TextPresenter 		new name: 'gemHost'.
	gemServicePresenter 	:= self add: TextPresenter 		new name: 'gemService'.
	gemTaskPresenter 		:= self add: TextPresenter 		new name: 'gemTask'.
	userIDPresenter 		:= self add: TextPresenter 		new name: 'userID'.
	passwordPresenter 		:= self add: TextPresenter 		new name: 'password'.
	loginListPresenter		:= self add: ListPresenter			new name: 'loginList'.
	loginTypePresenter 		:= self add: RadioButtonSetPresenter 	new name: 'loginType'.
	hostUserIDPresenter		:= self add: TextPresenter			new name: 'hostUserID'.
	hostPasswordPresenter	:= self add: TextPresenter			new name: 'hostPassword'.
	initialsPresenter		:= self add: TextPresenter			new name: 'initials'.
	debugPathPresenter		:= self add: TextPresenter			new name: 'debugPath'.
!

createSchematicWiring

	super createSchematicWiring.
	loginListPresenter	
		when: #'selectionChanged'	send: #'selectionChanged' 	to: self;
		when: #'actionPerformed'	send: #'login'			to: self;
		yourself.
	versionListPresenter		when: #selectionChanged 	send: #modified 	to: self.
	stoneHostPresenter 		when: #valueChanged 	send: #modified 	to: self.
	stoneNamePresenter 		when: #valueChanged 	send: #modified 	to: self.
	gemTypePresenter		when: #valueChanged 	send: #modified 	to: self.
	gemHostPresenter 		when: #valueChanged 	send: #modified 	to: self.
	gemServicePresenter 	when: #valueChanged 	send: #modified 	to: self.
	gemTaskPresenter 		when: #valueChanged 	send: #modified 	to: self.
	userIDPresenter 		when: #valueChanged 	send: #modified 	to: self.
	passwordPresenter 		when: #valueChanged 	send: #modified 	to: self.
	loginTypePresenter		when: #valueChanged 	send: #modified 	to: self.
	hostUserIDPresenter		when: #valueChanged 	send: #modified 	to: self.
	hostPasswordPresenter	when: #valueChanged 	send: #modified 	to: self.
	initialsPresenter		when: #valueChanged	send: #modified 	to: self.
	debugPathPresenter		when: #valueChanged	send: #modified 	to: self.
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

	| data list |
	[
		| fileStream |
		fileStream := FileStream read: self dataPath type: #'binary'.
		data := Object binaryReadFrom: fileStream.
		fileStream close.
	] on: Error do: [:ex | 
		data := OrderedCollection new
			add: (list := OrderedCollection with: JadeLogin default);
			add: list first;
			yourself.
	].
	loginListPresenter 
		list: (data at: 1);
		selectionOrNil: (data at: 2);
		yourself.
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

modified

	inUpdateView ifTrue: [^self].
	self updateModel.
	loginListPresenter view updateAll.!

onViewClosed

	self saveData.
	super onViewClosed.
!

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

postLogin: aGciSession

	model addSession: aGciSession.
	versionListPresenter view disable.
	loginListPresenter view updateAll.
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
!JadeLoginShell categoriesFor: #modified!public! !
!JadeLoginShell categoriesFor: #onViewClosed!public! !
!JadeLoginShell categoriesFor: #onViewOpened!public! !
!JadeLoginShell categoriesFor: #openGemTalkHomePage!public! !
!JadeLoginShell categoriesFor: #openJadeHomePage!public! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13238272 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1401 841 551 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 608 nil nil nil 7 nil nil nil 608 852230 ##(Smalltalk.FramingLayout) 170 176 34 10 410 ##(Smalltalk.Toolbar) 34 25 nil 608 34 2 8 1140853580 131137 736 nil 466 8 4278190080 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 736 466 816 8 1718856992 170 192 8 #() 170 192 34 4 35091 1246982 ##(Smalltalk.ToolbarSystemButton) 35091 nil 736 1 1180998 4 ##(Smalltalk.CommandDescription) #getHelpOn 8 'Get help on widget' 1 1 nil 1 23 35093 853766 ##(Smalltalk.ToolbarButton) 35093 nil 736 1 1058 #openHelp 8 'Jade Help' 1 1 nil 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 514 1857 33 71 34 2 1040 1120 170 176 34 4 1 1 1184 31 nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 241 1 514 1129 51 736 1490 #updateSizePosted 976 736 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 120 0 0 0 0 0 0 0 172 2 0 0 25 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedPreviousRight 1 1698 #fixedParentRight 1 1698 #fixedParentTop 1 1698 #fixedViewTop 51 410 ##(Smalltalk.ListView) 34 30 nil 608 34 2 8 1409355853 1025 1776 590662 2 ##(Smalltalk.ListModel) 138 144 976 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 3 984134 2 ##(Smalltalk.CommandMenuItem) 1 1058 #addLogin 8 '&Add' 1 1 nil nil nil 2002 1 1058 #removeLogin 8 '&Remove' 1 1 nil nil nil 2002 1 1058 #login 8 '&Login' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 1776 nil 8 1718806640 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 7 920646 5 ##(Smalltalk.ListViewColumn) 8 'Version' 241 #left 2194 #gemVersion 8 #() 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a gemVersion <= b gemVersion]' 8 #[30 105 226 0 226 32 130 106] #gemVersion 2400 7 513 nil nil nil 1776 nil 1 nil nil 2306 8 'Gem Host' 201 #left 2194 #gemHost 2368 2386 0 nil 2418 2 1 2192 8 'doIt' 8 '[:a :b | a gemHost <= b gemHost]' 8 #[30 105 226 0 226 32 130 106] #gemHost 2592 7 513 nil nil nil 1776 nil 1 nil nil 2306 8 'NetLDI' 201 #left 2194 #gemService 2368 2386 0 nil 2418 2 1 2192 8 'doIt' 8 '[:a :b | a gemService <= b gemService]' 8 #[30 105 226 0 226 32 130 106] #gemService 2720 7 513 nil nil nil 1776 nil 1 nil nil 2306 8 'Stone' 201 #left 2194 #stoneName 2368 2386 0 nil 2418 2 1 2192 8 'doIt' 8 '[:a :b | a stoneName <= b stoneName]' 8 #[30 105 226 0 226 32 130 106] #stoneName 2848 7 513 nil nil nil 1776 nil 1 nil nil 2306 8 'User' 201 #left 2194 #gsUserID 2368 2386 0 nil 2418 2 1 2192 8 'doIt' 8 '[:a :b | a gsUserID <= b gsUserID]' 8 #[30 105 226 0 226 32 130 106] #gsUserID 2976 7 513 nil nil nil 1776 nil 1 nil nil 2306 8 'Active' 111 #right 2194 #activeCount 8 #() 2386 0 nil 2418 2 1 2192 8 'doIt' 8 '[:a :b | a activeCount <= b activeCount]' 8 #[30 105 226 0 226 32 130 106] #activeCount 3120 7 513 nil nil nil 1776 nil 1 nil nil 2306 8 'Last Login' 441 #left 2194 #lastLoginString 8 #() 2386 0 nil 2418 2 1 2192 8 'doIt' 8 '[:a :b | a lastLogin <= b lastLogin]' 8 #[30 105 226 0 226 32 130 106] #lastLogin 3264 7 513 nil nil nil 1776 nil 1 nil nil #report 976 nil 131169 nil 34 4 nil nil 514 1 1 nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 514 1 51 514 1319 297 1776 1490 #contextMenu: 34 1 1968 1776 1490 #text: 34 1 8 'Version' 1776 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 147 2 0 0 173 0 0 0] 8 #() 1648 nil 27 1666 1698 #fixedParentLeft 1 1728 -49 1698 #fixedPreviousBottom 1 1698 #fixedParentBottom 1 410 ##(Smalltalk.PushButton) 34 20 nil 608 34 2 8 1140924416 1 3680 nil nil nil 7 nil nil nil 3680 nil 8 1718916160 1058 #removeLogin 8 '-' 1 1 nil nil false nil nil nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 514 1319 121 514 51 51 3680 1490 #isEnabled: 8 #(false) 3680 1490 #text: 34 1 8 '-' 3680 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 60 0 0 0 172 2 0 0 85 0 0 0] 8 #() 1648 nil 29 1666 1728 -49 1698 #fixedViewLeft 51 3648 11 1760 51 410 ##(Smalltalk.PushButton) 34 20 nil 608 34 2 8 1140924416 1 4064 nil nil nil 7 nil nil nil 4064 nil 8 1718916160 1058 #addLogin 8 '+' 1 1 nil nil false nil nil nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 514 1319 61 514 51 51 4064 1490 #isEnabled: 8 #(false) 4064 1490 #text: 34 1 8 '+' 4064 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 30 0 0 0 172 2 0 0 55 0 0 0] 8 #() 1648 nil 29 1666 1728 -49 4048 51 1744 61 1760 51 410 ##(Smalltalk.Toolbar) 34 25 nil 608 34 2 8 1409289036 131137 4432 nil 466 8 4278190080 nil 7 nil nil nil 4432 466 4512 8 1718856992 170 192 976 170 192 34 10 35081 1106 35081 nil 4432 1 1058 #editCut 8 'Cut' 1 1 nil 1170 nil true 1216 8 'EditBar.bmp' 1264 nil 7 514 193 33 1 35083 1106 35083 nil 4432 1 1058 #editCopy 8 'Copy' 1 1 nil 4656 3 35085 1106 35085 nil 4432 1 1058 #editPaste 8 'Paste' 1 1 nil 4656 5 35087 1106 35087 nil 4432 1 1058 #editDelete 8 'Delete' 1 1 nil 4656 11 35089 1106 35089 nil 4432 1 1058 #editUndo 8 'Undo' 1 1 nil 4656 7 34 6 4608 4704 4752 4800 4848 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 4432 3 nil 1 170 176 34 2 4656 1 nil 1 nil 514 33 33 514 45 45 nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 1 1 514 241 51 4432 1490 #updateSizePosted 976 4432 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 120 0 0 0 25 0 0 0] 8 #() 1648 nil 27 1666 3632 1 4048 241 1744 1 1760 51 170 192 34 6 1776 8 'loginList' 4432 8 'Edit Tools' 736 8 'Other Tools' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 514 1 1 514 1369 347 608 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 173 0 0 0] 34 5 4432 736 1776 4064 3680 1648 nil 27 39 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 5440 nil nil nil 7 nil nil nil 5440 674 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 5440 34 2 8 1140850688 131073 5552 nil nil nil 7 nil nil nil 5552 674 170 176 34 28 410 ##(Smalltalk.TextEdit) 34 16 nil 5552 34 2 8 1140916352 1025 5664 nil 466 8 4278190080 nil 7 nil nil nil 5664 nil 8 1718754816 852486 ##(Smalltalk.NullConverter) nil nil 1 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 391 113 514 285 45 5664 1490 #setMarginWidths: 34 1 8 #(3 3) 5664 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 56 0 0 0 81 1 0 0 78 0 0 0] 8 #() 1648 nil 27 1666 1712 11 1728 -9 1698 #fixedPreviousTop 1 1760 45 410 ##(Smalltalk.TextEdit) 34 16 nil 5552 34 2 8 1140916384 1025 6048 nil 466 8 4278190080 nil 7 nil nil nil 6048 nil 8 1718754816 5778 nil nil 1 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 391 259 514 285 45 6048 1490 #setMarginWidths: 34 1 8 #(3 3) 6048 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 129 0 0 0 81 1 0 0 151 0 0 0] 8 #() 1648 nil 27 1666 1712 11 1728 -9 6032 1 1760 45 410 ##(Smalltalk.TextEdit) 34 16 nil 5552 34 2 8 1140916352 1025 6400 nil 466 8 4278190080 nil 7 nil nil nil 6400 nil 8 1718754816 5778 nil nil 1 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 391 313 514 285 45 6400 1490 #setMarginWidths: 34 1 8 #(3 3) 6400 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 156 0 0 0 81 1 0 0 178 0 0 0] 8 #() 1648 nil 27 1666 1712 11 1728 -9 6032 1 1760 45 410 ##(Smalltalk.StaticText) 34 16 nil 5552 34 2 8 1140850944 1 6752 nil nil nil 7 nil nil nil 6752 nil 8 1718913696 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 11 313 514 371 41 6752 1490 #text: 34 1 8 'Monticello User Name:' 6752 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 156 0 0 0 190 0 0 0 176 0 0 0] 8 #() 1648 nil 27 1666 3632 11 4048 371 3648 11 1760 41 410 ##(Smalltalk.TextEdit) 34 16 nil 5552 34 2 8 1140916352 1025 7072 nil 466 5744 nil 7 nil nil nil 7072 nil 8 1718754816 5778 nil nil 1 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 391 167 514 285 45 7072 1490 #setMarginWidths: 34 1 8 #(3 3) 7072 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 83 0 0 0 81 1 0 0 105 0 0 0] 8 #() 1648 nil 27 1666 1712 11 1728 -9 6032 1 1760 45 410 ##(Smalltalk.StaticText) 34 16 nil 5552 34 2 8 1140850944 1 7408 nil nil nil 7 nil nil nil 7408 nil 8 1718913696 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 11 259 514 371 41 7408 1490 #text: 34 1 8 'GemStone Password:' 7408 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 129 0 0 0 190 0 0 0 149 0 0 0] 8 #() 1648 nil 27 1666 3632 11 4048 371 3648 3 1760 41 410 ##(Smalltalk.TextEdit) 34 16 nil 5552 34 2 8 1140916352 1025 7728 nil 466 5744 nil 7 nil nil nil 7728 nil 8 1718754816 5778 nil nil 1 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 391 213 514 285 45 7728 1490 #setMarginWidths: 34 1 8 #(3 3) 7728 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 106 0 0 0 81 1 0 0 128 0 0 0] 8 #() 1648 nil 27 1666 1712 11 1728 -9 6032 1 1760 45 410 ##(Smalltalk.ComboBox) 34 17 nil 5552 34 2 8 1144063491 1025 8064 1842 138 144 976 nil 1904 466 8 4278190080 nil 7 nil nil nil 8064 nil 8 1719430240 2194 #displayString 8 #() 976 401 1426 138 144 34 1 1490 #createAt:extent: 34 2 514 391 11 514 285 53 8064 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 5 0 0 0 81 1 0 0 31 0 0 0] 8 #() 1648 nil 27 1666 1712 11 1728 -9 1744 11 1760 47 410 ##(Smalltalk.StaticText) 34 16 nil 5552 34 2 8 1140850944 1 8416 nil nil nil 7 nil nil nil 8416 nil 8 1718913696 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 11 67 514 371 31 8416 1490 #text: 34 1 8 'RPC Gem Host Name or IP:' 8416 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 190 0 0 0 48 0 0 0] 8 #() 1648 nil 27 1666 3632 11 4048 371 3648 11 1760 31 410 ##(Smalltalk.TextEdit) 34 16 nil 5552 34 2 8 1140916352 1025 8736 nil 466 5744 nil 7 nil nil nil 8736 nil 8 1718754816 5778 nil nil 1 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 391 67 514 285 45 8736 1490 #setMarginWidths: 34 1 8 #(3 3) 8736 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 33 0 0 0 81 1 0 0 55 0 0 0] 8 #() 1648 nil 27 1666 1712 11 1728 -9 6032 1 1760 45 410 ##(Smalltalk.StaticText) 34 16 nil 5552 34 2 8 1140850944 1 9072 nil nil nil 7 nil nil nil 9072 nil 8 1718913696 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 11 167 514 371 39 9072 1490 #text: 34 1 8 'Stone Name:' 9072 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 83 0 0 0 190 0 0 0 102 0 0 0] 8 #() 1648 nil 27 1666 3632 11 4048 371 3648 11 1760 39 410 ##(Smalltalk.StaticText) 34 16 nil 5552 34 2 8 1140850944 1 9392 nil nil nil 7 nil nil nil 9392 nil 8 1718913696 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 11 113 514 371 31 9392 1490 #text: 34 1 8 'NetLDI Service Name or Port:' 9392 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 56 0 0 0 190 0 0 0 71 0 0 0] 8 #() 1648 nil 27 1666 3632 11 4048 371 3648 3 1760 31 410 ##(Smalltalk.StaticText) 34 16 nil 5552 34 2 8 1140850944 1 9712 nil nil nil 7 nil nil nil 9712 nil 8 1718913696 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 11 213 514 371 41 9712 1490 #text: 34 1 8 'GemStone User ID:' 9712 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 106 0 0 0 190 0 0 0 126 0 0 0] 8 #() 1648 nil 27 1666 3632 11 4048 371 3648 3 1760 41 410 ##(Smalltalk.StaticText) 34 16 nil 5552 34 2 8 1140850944 1 10032 nil nil nil 7 nil nil nil 10032 nil 8 1718913696 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 11 21 514 371 41 10032 1490 #text: 34 1 8 'GemStone/Smalltalk Version:' 10032 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 190 0 0 0 30 0 0 0] 8 #() 1648 nil 27 1666 3632 11 4048 371 1744 21 1760 41 170 192 34 14 7072 8 'stoneName' 6400 8 'initials' 6048 8 'password' 5664 8 'gemService' 8736 8 'gemHost' 7728 8 'userID' 8064 8 'versionList' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 514 1 1 514 685 367 5552 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 1 0 0 183 0 0 0] 34 14 10032 8064 8416 8736 9392 5664 9072 7072 9712 7728 7408 6048 6752 6400 1648 nil 27 1666 3632 1 1698 #relativeParentWidth 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 1744 1 3664 1 410 ##(Smalltalk.ContainerView) 34 15 nil 5440 34 2 8 1140850688 131073 10720 nil nil nil 7 nil nil nil 10720 674 170 176 34 26 410 ##(Smalltalk.TextEdit) 34 16 nil 10720 34 2 8 1140916352 1025 10832 nil 466 8 4278190080 nil 7 nil nil nil 10832 nil 8 1718754816 5778 nil nil 1 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 491 163 514 185 45 10832 1490 #setMarginWidths: 34 1 8 #(3 3) 10832 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 81 0 0 0 81 1 0 0 103 0 0 0] 8 #() 1648 nil 27 1666 1712 11 1728 -9 6032 1 1760 45 410 ##(Smalltalk.PushButton) 34 20 nil 10720 34 2 8 1140924416 1 11184 nil nil nil 7 nil nil nil 11184 nil 8 1718916160 1058 #debugGCI 8 'Debug GCI' 1 1 nil nil false nil nil nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 514 11 5 514 141 51 11184 1490 #isEnabled: 8 #(false) 11184 1490 #text: 34 1 8 'Debug GCI' 11184 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 75 0 0 0 27 0 0 0] 8 #() 1648 nil 29 1666 3632 11 4048 141 3648 5 1760 51 410 ##(Smalltalk.TextEdit) 34 16 nil 10720 34 2 8 1140916384 1025 11552 nil 466 8 4278190080 nil 7 nil nil nil 11552 nil 8 1718754816 5778 nil nil 1 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 491 209 514 185 45 11552 1490 #setMarginWidths: 34 1 8 #(3 3) 11552 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 104 0 0 0 81 1 0 0 126 0 0 0] 8 #() 1648 nil 27 1666 1712 11 1728 -9 6032 1 1760 45 410 ##(Smalltalk.ContainerView) 34 15 nil 10720 34 2 8 1140850688 393217 11904 nil 466 8 4278190080 nil 7 nil nil nil 11904 656390 ##(Smalltalk.GridLayout) 3 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 11904 34 2 8 1140924425 1 12064 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 2194 #= 8 #() 2194 #hash 8 #() false 466 11984 nil 7 nil nil nil 12064 nil 8 1718916160 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 1 1 514 331 45 12064 1490 #text: 34 1 8 'RPC Gem' 12064 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 165 0 0 0 22 0 0 0] 8 #() 1648 nil 27 8 'remote' 410 ##(Smalltalk.RadioButton) 34 16 nil 11904 34 2 8 1141055497 1 12528 12130 nil nil 12162 2194 #= 8 #() 2194 #hash 8 #() false 466 11984 nil 7 nil nil nil 12528 nil 8 1718916160 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 331 1 514 331 45 12528 1490 #text: 34 1 8 'Linked Gem (32-bit only)' 12528 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 74 1 0 0 22 0 0 0] 8 #() 1648 nil 27 8 'linked' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 514 11 257 514 665 49 11904 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 128 0 0 0 81 1 0 0 152 0 0 0] 34 2 12064 12528 1648 nil 27 1666 3632 11 1728 -9 3648 5 1760 49 410 ##(Smalltalk.StaticText) 34 16 nil 10720 34 2 8 1140850944 1 13136 nil nil nil 7 nil nil nil 13136 nil 8 1718913696 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 11 115 514 301 31 13136 1490 #text: 34 1 8 'NetLDI Task Name:' 13136 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 57 0 0 0 155 0 0 0 72 0 0 0] 8 #() 1648 nil 27 1666 3632 11 4048 301 3648 3 1760 31 410 ##(Smalltalk.ContainerView) 34 15 nil 10720 34 2 8 1409286144 393217 13456 nil 466 8 4278190080 nil 7 nil nil nil 13456 12002 1 3 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 13456 34 2 8 1141055497 1 13600 12130 nil nil 12162 2194 #= 8 #() 2194 #hash 8 #() false 466 13536 nil 7 nil nil nil 13600 nil 8 1718916160 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 1 1 514 297 39 13600 1490 #text: 34 1 8 'NetLDI Guest' 13600 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 148 0 0 0 19 0 0 0] 8 #() 1648 nil 27 8 'guest' 410 ##(Smalltalk.RadioButton) 34 16 nil 13456 34 2 8 1140924425 1 14032 12130 nil nil 12162 2194 #= 8 #() 2194 #hash 8 #() false 466 13536 nil 7 nil nil nil 14032 nil 8 1718916160 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 1 39 514 297 39 14032 1490 #text: 34 1 8 'Native OS User' 14032 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 148 0 0 0 38 0 0 0] 8 #() 1648 nil 27 8 'member' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 514 11 163 514 301 81 13456 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 81 0 0 0 155 0 0 0 121 0 0 0] 34 2 13600 14032 1648 nil 27 1666 3632 11 4048 301 3648 5 1760 81 410 ##(Smalltalk.TextEdit) 34 16 nil 10720 34 2 8 1140916352 1025 14640 nil 466 5744 nil 7 nil nil nil 14640 nil 8 1718754816 5778 nil nil 1 1426 138 144 34 3 1490 #createAt:extent: 34 2 514 321 69 514 355 45 14640 1490 #text: 34 1 8 'localhost' 14640 1490 #setMarginWidths: 34 1 8 #(3 3) 14640 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 34 0 0 0 81 1 0 0 56 0 0 0] 8 #() 1648 nil 27 1666 1712 11 1728 -9 6032 1 1760 45 410 ##(Smalltalk.StaticText) 34 16 nil 10720 34 2 8 1140850944 1 15024 nil nil nil 7 nil nil nil 15024 nil 8 1718913696 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 321 163 514 161 41 15024 1490 #text: 34 1 8 'OS User ID:' 15024 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 81 0 0 0 240 0 0 0 101 0 0 0] 8 #() 1648 nil 27 1666 1712 11 4048 161 6032 1 1760 41 410 ##(Smalltalk.StaticText) 34 16 nil 10720 34 2 8 1140850944 1 15344 nil nil nil 7 nil nil nil 15344 nil 8 1718913696 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 321 209 514 161 41 15344 1490 #text: 34 1 8 'Password:' 15344 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 104 0 0 0 240 0 0 0 124 0 0 0] 8 #() 1648 nil 27 1666 1698 #fixedPreviousLeft -169 4048 161 3648 3 1760 41 410 ##(Smalltalk.PushButton) 34 20 nil 10720 34 2 8 1140924416 1 15680 nil nil nil 7 nil nil nil 15680 nil 8 1718916160 1058 #login 8 'Login' 1 1 nil nil true nil nil nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 514 11 305 514 665 61 15680 1490 #isEnabled: 8 #(false) 15680 1490 #text: 34 1 8 'Login' 15680 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 152 0 0 0 81 1 0 0 182 0 0 0] 8 #() 1648 nil 29 1666 3632 11 1728 -9 3648 1 1760 61 410 ##(Smalltalk.TextEdit) 34 16 nil 10720 34 2 8 1140916352 1025 16048 nil 466 5744 nil 7 nil nil nil 16048 nil 8 1718754816 5778 nil nil 1 1426 138 144 34 3 1490 #createAt:extent: 34 2 514 321 115 514 355 45 16048 1490 #text: 34 1 8 'gemnetobject' 16048 1490 #setMarginWidths: 34 1 8 #(3 3) 16048 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 57 0 0 0 81 1 0 0 79 0 0 0] 8 #() 1648 nil 27 1666 1712 11 1728 -9 6032 1 1760 45 410 ##(Smalltalk.StaticPath) 34 17 nil 10720 34 2 8 1140850944 1 16432 nil nil nil 7 nil nil nil 16432 nil 8 1718913696 5778 nil nil nil 8 'XXX' 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 161 15 514 515 45 16432 1490 #text: 34 1 16528 16432 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 7 0 0 0 81 1 0 0 29 0 0 0] 8 #() 1648 nil 27 1666 3632 161 1728 -9 6032 11 1760 45 410 ##(Smalltalk.StaticText) 34 16 nil 10720 34 2 8 1140850944 1 16752 nil nil nil 7 nil nil nil 16752 nil 8 1718913696 5778 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 514 11 69 514 301 41 16752 1490 #text: 34 1 8 'Stone Host (from Gem):' 16752 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 34 0 0 0 155 0 0 0 54 0 0 0] 8 #() 1648 nil 27 1666 3632 11 4048 301 3648 11 1760 41 170 192 34 14 10832 8 'hostUserID' 16432 8 'debugPath' 16048 8 'gemTask' 14640 8 'stoneHost' 11904 8 'gemType' 13456 8 'loginType' 11552 8 'hostPassword' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 514 685 1 514 685 367 10720 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 86 1 0 0 0 0 0 0 172 2 0 0 183 0 0 0] 34 13 11184 16432 16752 14640 13136 16048 13456 15024 10832 15344 11552 11904 15680 1648 nil 27 1666 10672 10690 8 0 0 0 0 0 0 224 63 1728 1 1744 1 3664 1 170 192 34 4 5552 8 'Basic' 10720 8 'Advanced' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 514 1 357 514 1369 367 5440 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 178 0 0 0 172 2 0 0 105 1 0 0] 34 2 5552 10720 1648 nil 27 41 true 170 192 34 4 608 8 'top' 5440 8 'bottom' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 1954 nil true 34 5 2002 1 1058 #addLogin 8 '&New' 9373 1 nil nil nil 2002 1 1058 #fileOpen 8 '&Open...' 9375 1 nil nil nil 2002 1 1058 #removeLogin 8 '&Remove' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 2002 1 1058 #exit 8 'E&xit' 17639 1 nil nil nil 8 '&Login' nil 134217729 nil nil 35045 nil nil 1954 nil true 34 9 2002 1 1058 #editUndo 8 '&Undo' 9397 1 nil nil nil 2002 1 1058 #editRedo 8 '&Redo' 9395 1 nil nil nil 17922 4097 2002 1 1058 #editCut 8 'Cu&t' 9393 1 nil nil nil 2002 1 1058 #editCopy 8 '&Copy' 9351 1 nil nil nil 2002 1 1058 #editPaste 8 '&Paste' 9389 1 nil nil nil 2002 1 1058 #editDelete 8 'De&lete' 1629 1 nil nil nil 17922 4097 2002 1 1058 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '&Edit' nil 134217729 nil nil 35061 nil nil 1954 nil true 34 10 2002 1 1058 #helpContents 8 '&Contents' 1 1 nil nil nil 2002 1 1058 #helpOnThisTool 8 '&On this Tool' 1249 1 nil nil nil 2002 1 1058 #helpWhatsThis 8 '&What''s This?' 5345 1 nil nil nil 17922 4097 2002 1 1058 #openGemTalkHomePage 8 '&GemTalk Home Page' 1025 1 nil nil nil 2002 1 1058 #openJadeHomePage 8 '&Jade Home Page' 1 1 nil nil nil 17922 4097 2002 1 1058 #showGemNRS 8 'Show Gem &NRS' 1 1 nil nil nil 2002 1 1058 #showStoneNRS 8 'Show Stone N&RS' 1 1 nil nil nil 2002 1 1058 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 35079 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 9649 263494 3 ##(Smalltalk.Icon) nil true 1216 8 'icons\GS32.ico' nil 18914 nil true 1216 8 'icons\GS16.ico' nil nil nil 1 nil nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 514 5119 21 514 1401 841 416 1490 #text: 34 1 8 'Jade Login' 416 1490 #updateMenuBar 976 416 1586 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 187 12 0 0 174 1 0 0] 34 3 608 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 19216 nil 466 8 4278190080 nil 519 nil nil nil 19216 1426 138 144 34 1 1490 #createAt:extent: 34 2 514 1 347 514 1369 11 19216 1586 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 173 0 0 0 172 2 0 0 178 0 0 0] 8 #() 1648 nil 27 5440 1648 nil 27 )! !
!JadeLoginShell class categoriesFor: #icon!constants!public! !
!JadeLoginShell class categoriesFor: #resource_Default_view!public!resources-views! !

