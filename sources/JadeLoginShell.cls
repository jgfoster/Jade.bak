"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #JadeLoginShell
	instanceVariableNames: 'debugPathPresenter gemHostPresenter gemServicePresenter gemTaskPresenter gemTypePresenter hostPasswordPresenter hostUserIDPresenter initialsPresenter inUpdateView loginListPresenter loginTypePresenter passwordPresenter sessions stoneHostPresenter stoneNamePresenter userIDPresenter versionListPresenter'
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
	sessions := IdentityDictionary new.
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

logout: aLogin

	loginListPresenter view updateAll.
	sessions removeKey: aLogin ifAbsent: [].
!

modified

	inUpdateView ifTrue: [^self].
	self updateModel.
	loginListPresenter view updateAll.!

onCloseRequested

	self saveData.
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

	self model hasActiveSessions
		ifNil: [self login] 
		ifNotNil: [(sessions at: self model) view setFocus]
!

postLogin: aGciSession

	| transcript |
	model addSession: aGciSession.
	versionListPresenter view disable.
	loginListPresenter view updateAll.
	self setWorkingDirectory.
	transcript := JadeTranscript showOnSession: aGciSession.
	sessions at: self model put: transcript.
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
!JadeLoginShell categoriesFor: #onCloseRequested!public! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13238272 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1401 841 551 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 608 nil nil nil 7 nil nil nil 608 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 608 34 2 8 1140850688 131073 736 nil nil nil 7 nil nil nil 736 674 170 176 34 26 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 848 nil nil nil 7 nil nil nil 848 nil 8 1730185888 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 11 69 514 301 41 848 1026 #text: 34 1 8 'Stone Host (from Gem):' 848 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 34 0 0 0 155 0 0 0 54 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 11 1266 #fixedViewLeft 301 1266 #fixedPreviousBottom 11 1266 #fixedViewTop 41 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 1344 nil nil nil 7 nil nil nil 1344 nil 8 1730185888 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 115 514 301 31 1344 1026 #text: 34 1 8 'NetLDI Task Name:' 1344 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 57 0 0 0 155 0 0 0 72 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 301 1312 3 1328 31 410 ##(Smalltalk.ContainerView) 34 15 nil 736 34 2 8 1409286144 393217 1664 nil 466 8 4278190080 nil 7 nil nil nil 1664 656390 ##(Smalltalk.GridLayout) 1 3 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 1664 34 2 8 1140924425 1 1824 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 459270 ##(Smalltalk.Message) #= 8 #() 1954 #hash 8 #() false 466 1744 nil 7 nil nil nil 1824 nil 8 1730188352 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 1 39 514 297 39 1824 1026 #text: 34 1 8 'Native OS User' 1824 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 148 0 0 0 38 0 0 0] 8 #() 1216 nil 27 8 'member' 410 ##(Smalltalk.RadioButton) 34 16 nil 1664 34 2 8 1141055497 1 2304 1890 nil nil 1922 1954 #= 8 #() 1954 #hash 8 #() false 466 1744 nil 7 nil nil nil 2304 nil 8 1730188352 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 1 1 514 297 39 2304 1026 #text: 34 1 8 'NetLDI Guest' 2304 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 148 0 0 0 19 0 0 0] 8 #() 1216 nil 27 8 'guest' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 11 163 514 301 81 1664 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 81 0 0 0 155 0 0 0 121 0 0 0] 34 2 2304 1824 1216 nil 27 1234 1280 11 1296 301 1312 5 1328 81 410 ##(Smalltalk.StaticPath) 34 17 nil 736 34 2 8 1140850944 1 2912 nil nil nil 7 nil nil nil 2912 nil 8 1730185888 930 nil nil nil 8 'XXX' 962 138 144 34 2 1026 #createAt:extent: 34 2 514 161 15 514 515 45 2912 1026 #text: 34 1 3008 2912 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 7 0 0 0 81 1 0 0 29 0 0 0] 8 #() 1216 nil 27 1234 1280 161 1266 #fixedParentRight -9 1266 #fixedPreviousTop 11 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 3264 nil nil nil 7 nil nil nil 3264 nil 8 1730185888 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 321 163 514 161 41 3264 1026 #text: 34 1 8 'OS User ID:' 3264 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 81 0 0 0 240 0 0 0 101 0 0 0] 8 #() 1216 nil 27 1234 1266 #fixedPreviousRight 11 1296 161 3248 1 1328 41 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 3600 nil nil nil 7 nil nil nil 3600 nil 8 1730185888 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 321 209 514 161 41 3600 1026 #text: 34 1 8 'Password:' 3600 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 104 0 0 0 240 0 0 0 124 0 0 0] 8 #() 1216 nil 27 1234 1266 #fixedPreviousLeft -169 1296 161 1312 3 1328 41 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916352 1025 3936 nil 466 8 4278190080 nil 7 nil nil nil 3936 nil 8 1730027008 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 491 163 514 185 45 3936 1026 #setMarginWidths: 34 1 8 #(3 3) 3936 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 81 0 0 0 81 1 0 0 103 0 0 0] 8 #() 1216 nil 27 1234 3584 11 3232 -9 3248 1 1328 45 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916384 1025 4288 nil 466 8 4278190080 nil 7 nil nil nil 4288 nil 8 1730027008 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 491 209 514 185 45 4288 1026 #setMarginWidths: 34 1 8 #(3 3) 4288 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 104 0 0 0 81 1 0 0 126 0 0 0] 8 #() 1216 nil 27 1234 3584 11 3232 -9 3248 1 1328 45 410 ##(Smalltalk.ContainerView) 34 15 nil 736 34 2 8 1140850688 393217 4640 nil 466 8 4278190080 nil 7 nil nil nil 4640 1762 3 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 4640 34 2 8 1141055497 1 4784 1890 nil nil 1922 1954 #= 8 #() 1954 #hash 8 #() false 466 4720 nil 7 nil nil nil 4784 nil 8 1730188352 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 331 1 514 331 45 4784 1026 #text: 34 1 8 'Linked Gem (32-bit only)' 4784 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 74 1 0 0 22 0 0 0] 8 #() 1216 nil 27 8 'linked' 410 ##(Smalltalk.RadioButton) 34 16 nil 4640 34 2 8 1140924425 1 5216 1890 nil nil 1922 1954 #= 8 #() 1954 #hash 8 #() false 466 4720 nil 7 nil nil nil 5216 nil 8 1730188352 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 1 1 514 331 45 5216 1026 #text: 34 1 8 'RPC Gem' 5216 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 165 0 0 0 22 0 0 0] 8 #() 1216 nil 27 8 'remote' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 11 257 514 665 49 4640 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 128 0 0 0 81 1 0 0 152 0 0 0] 34 2 5216 4784 1216 nil 27 1234 1280 11 3232 -9 1312 5 1328 49 410 ##(Smalltalk.PushButton) 34 20 nil 736 34 2 8 1140924416 1 5824 nil nil nil 7 nil nil nil 5824 nil 8 1730188352 1180998 4 ##(Smalltalk.CommandDescription) #debugGCI 8 'Debug GCI' 1 1 nil nil false nil nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 11 5 514 141 51 5824 1026 #isEnabled: 8 #(false) 5824 1026 #text: 34 1 8 'Debug GCI' 5824 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 75 0 0 0 27 0 0 0] 8 #() 1216 nil 29 1234 1280 11 1296 141 1312 5 1328 51 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916352 1025 6208 nil 466 8 4278190080 nil 7 nil nil nil 6208 nil 8 1730027008 930 nil nil 1 962 138 144 34 3 1026 #createAt:extent: 34 2 514 321 69 514 355 45 6208 1026 #text: 34 1 8 'localhost' 6208 1026 #setMarginWidths: 34 1 8 #(3 3) 6208 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 34 0 0 0 81 1 0 0 56 0 0 0] 8 #() 1216 nil 27 1234 3584 11 3232 -9 3248 1 1328 45 410 ##(Smalltalk.PushButton) 34 20 nil 736 34 2 8 1140924416 1 6608 nil nil nil 7 nil nil nil 6608 nil 8 1730188352 5906 #login 8 'Login' 1 1 nil nil true nil nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 11 305 514 665 61 6608 1026 #isEnabled: 8 #(false) 6608 1026 #text: 34 1 8 'Login' 6608 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 152 0 0 0 81 1 0 0 182 0 0 0] 8 #() 1216 nil 29 1234 1280 11 3232 -9 1312 1 1328 61 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916352 1025 6976 nil 466 6288 nil 7 nil nil nil 6976 nil 8 1730027008 930 nil nil 1 962 138 144 34 3 1026 #createAt:extent: 34 2 514 321 115 514 355 45 6976 1026 #text: 34 1 8 'gemnetobject' 6976 1026 #setMarginWidths: 34 1 8 #(3 3) 6976 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 57 0 0 0 81 1 0 0 79 0 0 0] 8 #() 1216 nil 27 1234 3584 11 3232 -9 3248 1 1328 45 170 192 34 14 2912 8 'debugPath' 6208 8 'stoneHost' 3936 8 'hostUserID' 4288 8 'hostPassword' 1664 8 'loginType' 4640 8 'gemType' 6976 8 'gemTask' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 685 1 514 685 367 736 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 86 1 0 0 0 0 0 0 172 2 0 0 183 0 0 0] 34 13 5824 2912 848 6208 1344 6976 1664 3264 3936 3600 4288 4640 6608 1216 nil 27 1234 1266 #relativeParentWidth 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 3232 1 1266 #fixedParentTop 1 1266 #fixedParentBottom 1 410 ##(Smalltalk.ContainerView) 34 15 nil 608 34 2 8 1140850688 131073 7760 nil nil nil 7 nil nil nil 7760 674 170 176 34 28 410 ##(Smalltalk.StaticText) 34 16 nil 7760 34 2 8 1140850944 1 7872 nil nil nil 7 nil nil nil 7872 nil 8 1730185888 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 213 514 371 41 7872 1026 #text: 34 1 8 'GemStone User ID:' 7872 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 106 0 0 0 190 0 0 0 126 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 3 1328 41 410 ##(Smalltalk.TextEdit) 34 16 nil 7760 34 2 8 1140916352 1025 8192 nil 466 6288 nil 7 nil nil nil 8192 nil 8 1730027008 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 167 514 285 45 8192 1026 #setMarginWidths: 34 1 8 #(3 3) 8192 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 83 0 0 0 81 1 0 0 105 0 0 0] 8 #() 1216 nil 27 1234 3584 11 3232 -9 3248 1 1328 45 410 ##(Smalltalk.TextEdit) 34 16 nil 7760 34 2 8 1140916352 1025 8528 nil 466 6288 nil 7 nil nil nil 8528 nil 8 1730027008 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 113 514 285 45 8528 1026 #setMarginWidths: 34 1 8 #(3 3) 8528 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 56 0 0 0 81 1 0 0 78 0 0 0] 8 #() 1216 nil 27 1234 3584 11 3232 -9 3248 1 1328 45 410 ##(Smalltalk.ComboBox) 34 17 nil 7760 34 2 8 1144063491 1025 8864 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 nil 7 nil nil nil 8864 nil 8 1730702432 1954 #displayString 8 #() 8976 401 962 138 144 34 1 1026 #createAt:extent: 34 2 514 391 11 514 285 51 8864 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 5 0 0 0 81 1 0 0 30 0 0 0] 8 #() 1216 nil 27 1234 3584 11 3232 -9 7728 11 1328 47 410 ##(Smalltalk.StaticText) 34 16 nil 7760 34 2 8 1140850944 1 9280 nil nil nil 7 nil nil nil 9280 nil 8 1730185888 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 21 514 371 41 9280 1026 #text: 34 1 8 'GemStone/Smalltalk Version:' 9280 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 190 0 0 0 30 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 7728 21 1328 41 410 ##(Smalltalk.StaticText) 34 16 nil 7760 34 2 8 1140850944 1 9600 nil nil nil 7 nil nil nil 9600 nil 8 1730185888 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 313 514 371 41 9600 1026 #text: 34 1 8 'Monticello User Name:' 9600 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 156 0 0 0 190 0 0 0 176 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 11 1328 41 410 ##(Smalltalk.TextEdit) 34 16 nil 7760 34 2 8 1140916352 1025 9920 nil 466 8 4278190080 nil 7 nil nil nil 9920 nil 8 1730027008 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 313 514 285 45 9920 1026 #setMarginWidths: 34 1 8 #(3 3) 9920 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 156 0 0 0 81 1 0 0 178 0 0 0] 8 #() 1216 nil 27 1234 3584 11 3232 -9 3248 1 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 7760 34 2 8 1140850944 1 10272 nil nil nil 7 nil nil nil 10272 nil 8 1730185888 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 67 514 371 31 10272 1026 #text: 34 1 8 'RPC Gem Host Name or IP:' 10272 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 190 0 0 0 48 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 11 1328 31 410 ##(Smalltalk.TextEdit) 34 16 nil 7760 34 2 8 1140916352 1025 10592 nil 466 6288 nil 7 nil nil nil 10592 nil 8 1730027008 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 213 514 285 45 10592 1026 #setMarginWidths: 34 1 8 #(3 3) 10592 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 106 0 0 0 81 1 0 0 128 0 0 0] 8 #() 1216 nil 27 1234 3584 11 3232 -9 3248 1 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 7760 34 2 8 1140850944 1 10928 nil nil nil 7 nil nil nil 10928 nil 8 1730185888 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 113 514 371 31 10928 1026 #text: 34 1 8 'NetLDI Service Name or Port:' 10928 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 56 0 0 0 190 0 0 0 71 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 3 1328 31 410 ##(Smalltalk.StaticText) 34 16 nil 7760 34 2 8 1140850944 1 11248 nil nil nil 7 nil nil nil 11248 nil 8 1730185888 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 259 514 371 41 11248 1026 #text: 34 1 8 'GemStone Password:' 11248 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 129 0 0 0 190 0 0 0 149 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 3 1328 41 410 ##(Smalltalk.TextEdit) 34 16 nil 7760 34 2 8 1140916352 1025 11568 nil 466 6288 nil 7 nil nil nil 11568 nil 8 1730027008 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 67 514 285 45 11568 1026 #setMarginWidths: 34 1 8 #(3 3) 11568 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 33 0 0 0 81 1 0 0 55 0 0 0] 8 #() 1216 nil 27 1234 3584 11 3232 -9 3248 1 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 7760 34 2 8 1140850944 1 11904 nil nil nil 7 nil nil nil 11904 nil 8 1730185888 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 167 514 371 39 11904 1026 #text: 34 1 8 'Stone Name:' 11904 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 83 0 0 0 190 0 0 0 102 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 11 1328 39 410 ##(Smalltalk.TextEdit) 34 16 nil 7760 34 2 8 1140916384 1025 12224 nil 466 8 4278190080 nil 7 nil nil nil 12224 nil 8 1730027008 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 259 514 285 45 12224 1026 #setMarginWidths: 34 1 8 #(3 3) 12224 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 129 0 0 0 81 1 0 0 151 0 0 0] 8 #() 1216 nil 27 1234 3584 11 3232 -9 3248 1 1328 45 170 192 34 14 10592 8 'userID' 9920 8 'initials' 11568 8 'gemHost' 8864 8 'versionList' 8192 8 'stoneName' 8528 8 'gemService' 12224 8 'password' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 1 1 514 685 367 7760 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 1 0 0 183 0 0 0] 34 14 9280 8864 10272 11568 10928 8528 11904 8192 7872 10592 11248 12224 9600 9920 1216 nil 27 1234 1280 1 7680 7698 8 0 0 0 0 0 0 224 63 7728 1 7744 1 170 192 34 4 736 8 'Advanced' 7760 8 'Basic' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 1 357 514 1369 367 608 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 178 0 0 0 172 2 0 0 105 1 0 0] 34 2 7760 736 1216 nil 27 41 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 13136 nil nil nil 7 nil nil nil 13136 674 170 176 34 10 410 ##(Smalltalk.Toolbar) 34 25 nil 13136 34 2 8 1140853580 131137 13248 nil 466 8 4278190080 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 13248 466 13328 8 1730129184 170 192 8976 170 192 34 4 13091 853766 ##(Smalltalk.ToolbarButton) 13091 nil 13248 1 5906 #openHelp 8 'Jade Help' 1 1 nil 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 514 1857 33 71 13089 1246982 ##(Smalltalk.ToolbarSystemButton) 13089 nil 13248 1 5906 #getHelpOn 8 'Get help on widget' 1 1 nil 1 23 34 2 13744 13536 170 176 34 4 1 1 13600 31 nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 241 1 514 1129 51 13248 1026 #updateSizePosted 8976 13248 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 120 0 0 0 0 0 0 0 172 2 0 0 25 0 0 0] 8 #() 1216 nil 27 1234 3584 1 3232 1 7728 1 1328 51 410 ##(Smalltalk.Toolbar) 34 25 nil 13136 34 2 8 1409289036 131137 14096 nil 466 8 4278190080 nil 7 nil nil nil 14096 466 14176 8 1730129184 170 192 8976 170 192 34 10 13079 13522 13079 nil 14096 1 5906 #editCut 8 'Cut' 1 1 nil 13586 nil true 13632 8 'EditBar.bmp' 13680 nil 7 514 193 33 1 13081 13522 13081 nil 14096 1 5906 #editCopy 8 'Copy' 1 1 nil 14320 3 13083 13522 13083 nil 14096 1 5906 #editPaste 8 'Paste' 1 1 nil 14320 5 13085 13522 13085 nil 14096 1 5906 #editDelete 8 'Delete' 1 1 nil 14320 11 13087 13522 13087 nil 14096 1 5906 #editUndo 8 'Undo' 1 1 nil 14320 7 34 6 14272 14368 14416 14464 14512 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 14096 3 nil 1 170 176 34 2 14320 1 nil 1 nil 514 33 33 514 45 45 nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 1 1 514 241 51 14096 1026 #updateSizePosted 8976 14096 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 120 0 0 0 25 0 0 0] 8 #() 1216 nil 27 1234 1280 1 1296 241 7728 1 1328 51 410 ##(Smalltalk.ListView) 34 30 nil 13136 34 2 8 1409355853 1025 14864 8930 138 144 8976 nil 9008 466 8 4278190080 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 3 984134 2 ##(Smalltalk.CommandMenuItem) 1 5906 #addLogin 8 '&Add' 1 1 nil nil nil 15042 1 5906 #removeLogin 8 '&Remove' 1 1 nil nil nil 15042 1 5906 #login 8 '&Login' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 14864 nil 8 1730078832 1954 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 7 920646 5 ##(Smalltalk.ListViewColumn) 8 'Version' 241 #left 1954 #gemVersion 8 #() 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a gemVersion <= b gemVersion]' 8 #[30 105 226 0 226 32 130 106] #gemVersion 15424 7 513 nil nil nil 14864 nil 1 nil nil 15330 8 'Gem Host' 201 #left 1954 #gemHost 15392 15410 0 nil 15442 2 1 1952 8 'doIt' 8 '[:a :b | a gemHost <= b gemHost]' 8 #[30 105 226 0 226 32 130 106] #gemHost 15616 7 513 nil nil nil 14864 nil 1 nil nil 15330 8 'NetLDI' 201 #left 1954 #gemService 15392 15410 0 nil 15442 2 1 1952 8 'doIt' 8 '[:a :b | a gemService <= b gemService]' 8 #[30 105 226 0 226 32 130 106] #gemService 15744 7 513 nil nil nil 14864 nil 1 nil nil 15330 8 'Stone' 201 #left 1954 #stoneName 15392 15410 0 nil 15442 2 1 1952 8 'doIt' 8 '[:a :b | a stoneName <= b stoneName]' 8 #[30 105 226 0 226 32 130 106] #stoneName 15872 7 513 nil nil nil 14864 nil 1 nil nil 15330 8 'User' 201 #left 1954 #gsUserID 15392 15410 0 nil 15442 2 1 1952 8 'doIt' 8 '[:a :b | a gsUserID <= b gsUserID]' 8 #[30 105 226 0 226 32 130 106] #gsUserID 16000 7 513 nil nil nil 14864 nil 1 nil nil 15330 8 'Active' 111 #right 1954 #activeCount 8 #() 15410 0 nil 15442 2 1 1952 8 'doIt' 8 '[:a :b | a activeCount <= b activeCount]' 8 #[30 105 226 0 226 32 130 106] #activeCount 16144 7 513 nil nil nil 14864 nil 1 nil nil 15330 8 'Last Login' 441 #left 1954 #lastLoginString 8 #() 15410 0 nil 15442 2 1 1952 8 'doIt' 8 '[:a :b | a lastLogin <= b lastLogin]' 8 #[30 105 226 0 226 32 130 106] #lastLogin 16288 7 513 nil nil nil 14864 nil 1 nil nil #report 8976 nil 131169 nil 34 4 nil nil 514 1 1 nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 1 51 514 1319 297 14864 1026 #contextMenu: 34 1 15008 14864 1026 #text: 34 1 8 'Version' 14864 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 147 2 0 0 173 0 0 0] 8 #() 1216 nil 27 1234 1280 1 3232 -49 1312 1 7744 1 410 ##(Smalltalk.PushButton) 34 20 nil 13136 34 2 8 1140924416 1 16656 nil nil nil 7 nil nil nil 16656 nil 8 1730188352 5906 #addLogin 8 '+' 1 1 nil nil false nil nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 1319 61 514 51 51 16656 1026 #isEnabled: 8 #(false) 16656 1026 #text: 34 1 8 '+' 16656 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 30 0 0 0 172 2 0 0 55 0 0 0] 8 #() 1216 nil 29 1234 3232 -49 1296 51 7728 61 1328 51 410 ##(Smalltalk.PushButton) 34 20 nil 13136 34 2 8 1140924416 1 17024 nil nil nil 7 nil nil nil 17024 nil 8 1730188352 5906 #removeLogin 8 '-' 1 1 nil nil false nil nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 1319 121 514 51 51 17024 1026 #isEnabled: 8 #(false) 17024 1026 #text: 34 1 8 '-' 17024 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 60 0 0 0 172 2 0 0 85 0 0 0] 8 #() 1216 nil 29 1234 3232 -49 1296 51 1312 11 1328 51 170 192 34 6 14864 8 'loginList' 14096 8 'Edit Tools' 13248 8 'Other Tools' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 1 1 514 1369 347 13136 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 173 0 0 0] 34 5 14096 13248 14864 16656 17024 1216 nil 27 39 true 170 192 34 4 608 8 'bottom' 13136 8 'top' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 14994 nil true 34 5 15042 1 5906 #addLogin 8 '&New' 9373 1 nil nil nil 15042 1 5906 #fileOpen 8 '&Open...' 9375 1 nil nil nil 15042 1 5906 #removeLogin 8 '&Remove' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 15042 1 5906 #exit 8 'E&xit' 17639 1 nil nil nil 8 '&Login' nil 134217729 nil nil 13043 nil nil 14994 nil true 34 9 15042 1 5906 #editUndo 8 '&Undo' 9397 1 nil nil nil 15042 1 5906 #editRedo 8 '&Redo' 9395 1 nil nil nil 17922 4097 15042 1 5906 #editCut 8 'Cu&t' 9393 1 nil nil nil 15042 1 5906 #editCopy 8 '&Copy' 9351 1 nil nil nil 15042 1 5906 #editPaste 8 '&Paste' 9389 1 nil nil nil 15042 1 5906 #editDelete 8 'De&lete' 1629 1 nil nil nil 17922 4097 15042 1 5906 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '&Edit' nil 134217729 nil nil 13059 nil nil 14994 nil true 34 10 15042 1 5906 #helpContents 8 '&Contents' 1 1 nil nil nil 15042 1 5906 #helpOnThisTool 8 '&On this Tool' 1249 1 nil nil nil 15042 1 5906 #helpWhatsThis 8 '&What''s This?' 5345 1 nil nil nil 17922 4097 15042 1 5906 #openGemTalkHomePage 8 '&GemTalk Home Page' 1025 1 nil nil nil 15042 1 5906 #openJadeHomePage 8 '&Jade Home Page' 1 1 nil nil nil 17922 4097 15042 1 5906 #showGemNRS 8 'Show Gem &NRS' 1 1 nil nil nil 15042 1 5906 #showStoneNRS 8 'Show Stone N&RS' 1 1 nil nil nil 15042 1 5906 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 13077 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 10489 263494 3 ##(Smalltalk.Icon) nil true 13632 8 'icons\GS32.ico' nil 18914 nil true 13632 8 'icons\GS16.ico' nil nil nil 1 nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 5119 21 514 1401 841 416 1026 #text: 34 1 8 'Jade Login' 416 1026 #updateMenuBar 8976 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 187 12 0 0 174 1 0 0] 34 3 13136 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 19216 nil 466 8 4278190080 nil 519 nil nil nil 19216 962 138 144 34 1 1026 #createAt:extent: 34 2 514 1 347 514 1369 11 19216 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 173 0 0 0 172 2 0 0 178 0 0 0] 8 #() 1216 nil 27 608 1216 nil 27 )! !
!JadeLoginShell class categoriesFor: #icon!constants!public! !
!JadeLoginShell class categoriesFor: #resource_Default_view!public!resources-views! !

