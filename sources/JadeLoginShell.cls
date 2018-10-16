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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13238272 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1401 841 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 608 nil nil nil 7 nil nil nil 608 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 608 34 2 8 1140850688 131073 736 nil nil nil 7 nil nil nil 736 674 170 176 34 26 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 848 nil nil nil 7 nil nil nil 848 nil 8 1736477344 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 11 115 514 301 31 848 1026 #text: 34 1 8 'NetLDI Task Name:' 848 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 57 0 0 0 155 0 0 0 72 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 11 1266 #fixedViewLeft 301 1266 #fixedPreviousBottom 3 1266 #fixedViewTop 31 410 ##(Smalltalk.StaticPath) 34 17 nil 736 34 2 8 1140850944 1 1344 nil nil nil 7 nil nil nil 1344 nil 8 1736477344 930 nil nil nil 8 'XXX' 962 138 144 34 2 1026 #createAt:extent: 34 2 514 161 15 514 515 45 1344 1026 #text: 34 1 1440 1344 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 7 0 0 0 81 1 0 0 29 0 0 0] 8 #() 1216 nil 27 1234 1280 161 1266 #fixedParentRight -9 1266 #fixedPreviousTop 11 1328 45 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916352 1025 1696 nil 480 nil 7 nil nil nil 1696 nil 8 1736318464 930 nil nil 1 962 138 144 34 3 1026 #createAt:extent: 34 2 514 321 69 514 355 45 1696 1026 #text: 34 1 8 'localhost' 1696 1026 #setMarginWidths: 34 1 8 #(3 3) 1696 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 34 0 0 0 81 1 0 0 56 0 0 0] 8 #() 1216 nil 27 1234 1266 #fixedPreviousRight 11 1664 -9 1680 1 1328 45 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916352 1025 2080 nil 480 nil 7 nil nil nil 2080 nil 8 1736318464 930 nil nil 1 962 138 144 34 3 1026 #createAt:extent: 34 2 514 321 115 514 355 45 2080 1026 #text: 34 1 8 'gemnetobject' 2080 1026 #setMarginWidths: 34 1 8 #(3 3) 2080 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 57 0 0 0 81 1 0 0 79 0 0 0] 8 #() 1216 nil 27 1234 2064 11 1664 -9 1680 1 1328 45 410 ##(Smalltalk.ContainerView) 34 15 nil 736 34 2 8 1409286144 393217 2448 nil 480 nil 7 nil nil nil 2448 656390 ##(Smalltalk.GridLayout) 1 3 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 2448 34 2 8 1141055497 1 2576 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 459270 ##(Smalltalk.Message) #= 8 #() 2706 #hash 8 #() false 480 nil 7 nil nil nil 2576 nil 8 1736479808 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 1 1 514 297 39 2576 1026 #text: 34 1 8 'NetLDI Guest' 2576 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 148 0 0 0 19 0 0 0] 8 #() 1216 nil 27 8 'guest' 410 ##(Smalltalk.RadioButton) 34 16 nil 2448 34 2 8 1140924425 1 3040 2642 nil nil 2674 2706 #= 8 #() 2706 #hash 8 #() false 480 nil 7 nil nil nil 3040 nil 8 1736479808 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 1 39 514 297 39 3040 1026 #text: 34 1 8 'Native OS User' 3040 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 148 0 0 0 38 0 0 0] 8 #() 1216 nil 27 8 'member' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 11 163 514 301 81 2448 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 81 0 0 0 155 0 0 0 121 0 0 0] 34 2 2576 3040 1216 nil 27 1234 1280 11 1296 301 1312 5 1328 81 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 3632 nil nil nil 7 nil nil nil 3632 nil 8 1736477344 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 321 163 514 161 41 3632 1026 #text: 34 1 8 'OS User ID:' 3632 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 81 0 0 0 240 0 0 0 101 0 0 0] 8 #() 1216 nil 27 1234 2064 11 1296 161 1680 1 1328 41 410 ##(Smalltalk.PushButton) 34 20 nil 736 34 2 8 1140924416 1 3952 nil nil nil 7 nil nil nil 3952 nil 8 1736479808 1180998 4 ##(Smalltalk.CommandDescription) #debugGCI 8 'Debug GCI' 1 1 nil nil false nil nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 11 5 514 141 51 3952 1026 #isEnabled: 8 #(false) 3952 1026 #text: 34 1 8 'Debug GCI' 3952 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 75 0 0 0 27 0 0 0] 8 #() 1216 nil 29 1234 1280 11 1296 141 1312 5 1328 51 410 ##(Smalltalk.PushButton) 34 20 nil 736 34 2 8 1140924416 1 4336 nil nil nil 7 nil nil nil 4336 nil 8 1736479808 4034 #login 8 'Login' 1 1 nil nil true nil nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 11 305 514 665 61 4336 1026 #isEnabled: 8 #(false) 4336 1026 #text: 34 1 8 'Login' 4336 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 152 0 0 0 81 1 0 0 182 0 0 0] 8 #() 1216 nil 29 1234 1280 11 1664 -9 1312 1 1328 61 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 4704 nil nil nil 7 nil nil nil 4704 nil 8 1736477344 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 321 209 514 161 41 4704 1026 #text: 34 1 8 'Password:' 4704 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 104 0 0 0 240 0 0 0 124 0 0 0] 8 #() 1216 nil 27 1234 1266 #fixedPreviousLeft -169 1296 161 1312 3 1328 41 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916352 1025 5040 nil 480 nil 7 nil nil nil 5040 nil 8 1736318464 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 491 163 514 185 45 5040 1026 #setMarginWidths: 34 1 8 #(3 3) 5040 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 81 0 0 0 81 1 0 0 103 0 0 0] 8 #() 1216 nil 27 1234 2064 11 1664 -9 1680 1 1328 45 410 ##(Smalltalk.TextEdit) 34 16 nil 736 34 2 8 1140916384 1025 5360 nil 480 nil 7 nil nil nil 5360 nil 8 1736318464 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 491 209 514 185 45 5360 1026 #setMarginWidths: 34 1 8 #(3 3) 5360 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 104 0 0 0 81 1 0 0 126 0 0 0] 8 #() 1216 nil 27 1234 2064 11 1664 -9 1680 1 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 736 34 2 8 1140850944 1 5680 nil nil nil 7 nil nil nil 5680 nil 8 1736477344 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 69 514 301 41 5680 1026 #text: 34 1 8 'Stone Host (from Gem):' 5680 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 34 0 0 0 155 0 0 0 54 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 301 1312 11 1328 41 410 ##(Smalltalk.ContainerView) 34 15 nil 736 34 2 8 1140850688 393217 6000 nil 480 nil 7 nil nil nil 6000 2514 3 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 6000 34 2 8 1140924425 1 6112 2642 nil nil 2674 2706 #= 8 #() 2706 #hash 8 #() false 480 nil 7 nil nil nil 6112 nil 8 1736479808 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 1 1 514 331 45 6112 1026 #text: 34 1 8 'RPC Gem' 6112 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 165 0 0 0 22 0 0 0] 8 #() 1216 nil 27 8 'remote' 410 ##(Smalltalk.RadioButton) 34 16 nil 6000 34 2 8 1141055497 1 6528 2642 nil nil 2674 2706 #= 8 #() 2706 #hash 8 #() false 480 nil 7 nil nil nil 6528 nil 8 1736479808 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 331 1 514 331 45 6528 1026 #text: 34 1 8 'Linked Gem (32-bit only)' 6528 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 74 1 0 0 22 0 0 0] 8 #() 1216 nil 27 8 'linked' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 11 257 514 665 49 6000 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 128 0 0 0 81 1 0 0 152 0 0 0] 34 2 6112 6528 1216 nil 27 1234 1280 11 1664 -9 1312 5 1328 49 170 192 34 14 5040 8 'hostUserID' 6000 8 'gemType' 2080 8 'gemTask' 2448 8 'loginType' 5360 8 'hostPassword' 1344 8 'debugPath' 1696 8 'stoneHost' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 685 1 514 685 367 736 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 86 1 0 0 0 0 0 0 172 2 0 0 183 0 0 0] 34 13 3952 1344 5680 1696 848 2080 2448 3632 5040 4704 5360 6000 4336 1216 nil 27 1234 1266 #relativeParentWidth 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 1664 1 1266 #fixedParentTop 1 1266 #fixedParentBottom 1 410 ##(Smalltalk.ContainerView) 34 15 nil 608 34 2 8 1140850688 131073 7520 nil nil nil 7 nil nil nil 7520 674 170 176 34 28 410 ##(Smalltalk.StaticText) 34 16 nil 7520 34 2 8 1140850944 1 7632 nil nil nil 7 nil nil nil 7632 nil 8 1736477344 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 313 514 371 41 7632 1026 #text: 34 1 8 'Monticello User Name:' 7632 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 156 0 0 0 190 0 0 0 176 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 11 1328 41 410 ##(Smalltalk.StaticText) 34 16 nil 7520 34 2 8 1140850944 1 7952 nil nil nil 7 nil nil nil 7952 nil 8 1736477344 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 167 514 371 39 7952 1026 #text: 34 1 8 'Stone Name:' 7952 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 83 0 0 0 190 0 0 0 102 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 11 1328 39 410 ##(Smalltalk.StaticText) 34 16 nil 7520 34 2 8 1140850944 1 8272 nil nil nil 7 nil nil nil 8272 nil 8 1736477344 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 113 514 371 31 8272 1026 #text: 34 1 8 'NetLDI Service Name or Port:' 8272 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 56 0 0 0 190 0 0 0 71 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 3 1328 31 410 ##(Smalltalk.TextEdit) 34 16 nil 7520 34 2 8 1140916352 1025 8592 nil 480 nil 7 nil nil nil 8592 nil 8 1736318464 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 313 514 285 45 8592 1026 #setMarginWidths: 34 1 8 #(3 3) 8592 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 156 0 0 0 81 1 0 0 178 0 0 0] 8 #() 1216 nil 27 1234 2064 11 1664 -9 1680 1 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 7520 34 2 8 1140850944 1 8912 nil nil nil 7 nil nil nil 8912 nil 8 1736477344 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 259 514 371 41 8912 1026 #text: 34 1 8 'GemStone Password:' 8912 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 129 0 0 0 190 0 0 0 149 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 3 1328 41 410 ##(Smalltalk.StaticText) 34 16 nil 7520 34 2 8 1140850944 1 9232 nil nil nil 7 nil nil nil 9232 nil 8 1736477344 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 21 514 371 41 9232 1026 #text: 34 1 8 'GemStone/Smalltalk Version:' 9232 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 190 0 0 0 30 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 7488 21 1328 41 410 ##(Smalltalk.ComboBox) 34 17 nil 7520 34 2 8 1144063491 1025 9552 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 480 nil 7 nil nil nil 9552 nil 8 1736993888 2706 #displayString 8 #() 9664 401 962 138 144 34 1 1026 #createAt:extent: 34 2 514 391 11 514 285 51 9552 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 5 0 0 0 81 1 0 0 30 0 0 0] 8 #() 1216 nil 27 1234 2064 11 1664 -9 7488 11 1328 47 410 ##(Smalltalk.TextEdit) 34 16 nil 7520 34 2 8 1140916352 1025 9936 nil 480 nil 7 nil nil nil 9936 nil 8 1736318464 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 213 514 285 45 9936 1026 #setMarginWidths: 34 1 8 #(3 3) 9936 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 106 0 0 0 81 1 0 0 128 0 0 0] 8 #() 1216 nil 27 1234 2064 11 1664 -9 1680 1 1328 45 410 ##(Smalltalk.TextEdit) 34 16 nil 7520 34 2 8 1140916352 1025 10256 nil 480 nil 7 nil nil nil 10256 nil 8 1736318464 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 113 514 285 45 10256 1026 #setMarginWidths: 34 1 8 #(3 3) 10256 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 56 0 0 0 81 1 0 0 78 0 0 0] 8 #() 1216 nil 27 1234 2064 11 1664 -9 1680 1 1328 45 410 ##(Smalltalk.TextEdit) 34 16 nil 7520 34 2 8 1140916352 1025 10576 nil 480 nil 7 nil nil nil 10576 nil 8 1736318464 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 67 514 285 45 10576 1026 #setMarginWidths: 34 1 8 #(3 3) 10576 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 33 0 0 0 81 1 0 0 55 0 0 0] 8 #() 1216 nil 27 1234 2064 11 1664 -9 1680 1 1328 45 410 ##(Smalltalk.TextEdit) 34 16 nil 7520 34 2 8 1140916352 1025 10896 nil 480 nil 7 nil nil nil 10896 nil 8 1736318464 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 167 514 285 45 10896 1026 #setMarginWidths: 34 1 8 #(3 3) 10896 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 83 0 0 0 81 1 0 0 105 0 0 0] 8 #() 1216 nil 27 1234 2064 11 1664 -9 1680 1 1328 45 410 ##(Smalltalk.TextEdit) 34 16 nil 7520 34 2 8 1140916384 1025 11216 nil 480 nil 7 nil nil nil 11216 nil 8 1736318464 930 nil nil 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 391 259 514 285 45 11216 1026 #setMarginWidths: 34 1 8 #(3 3) 11216 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 129 0 0 0 81 1 0 0 151 0 0 0] 8 #() 1216 nil 27 1234 2064 11 1664 -9 1680 1 1328 45 410 ##(Smalltalk.StaticText) 34 16 nil 7520 34 2 8 1140850944 1 11536 nil nil nil 7 nil nil nil 11536 nil 8 1736477344 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 213 514 371 41 11536 1026 #text: 34 1 8 'GemStone User ID:' 11536 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 106 0 0 0 190 0 0 0 126 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 3 1328 41 410 ##(Smalltalk.StaticText) 34 16 nil 7520 34 2 8 1140850944 1 11856 nil nil nil 7 nil nil nil 11856 nil 8 1736477344 930 nil nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 11 67 514 371 31 11856 1026 #text: 34 1 8 'RPC Gem Host Name or IP:' 11856 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 190 0 0 0 48 0 0 0] 8 #() 1216 nil 27 1234 1280 11 1296 371 1312 11 1328 31 170 192 34 14 9552 8 'versionList' 9936 8 'userID' 10256 8 'gemService' 11216 8 'password' 10576 8 'gemHost' 8592 8 'initials' 10896 8 'stoneName' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 1 1 514 685 367 7520 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 1 0 0 183 0 0 0] 34 14 9232 9552 11856 10576 8272 10256 7952 10896 11536 9936 8912 11216 7632 8592 1216 nil 27 1234 1280 1 7440 7458 8 0 0 0 0 0 0 224 63 7488 1 7504 1 170 192 34 4 736 8 'Advanced' 7520 8 'Basic' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 1 357 514 1369 367 608 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 178 0 0 0 172 2 0 0 105 1 0 0] 34 2 7520 736 1216 nil 27 41 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 12736 nil nil nil 7 nil nil nil 12736 674 170 176 34 10 410 ##(Smalltalk.ListView) 34 30 nil 12736 34 2 8 1409355853 1025 12848 9618 138 144 9664 nil 9696 480 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 3 984134 2 ##(Smalltalk.CommandMenuItem) 1 4034 #addLogin 8 '&Add' 1 1 nil nil nil 12994 1 4034 #removeLogin 8 '&Remove' 1 1 nil nil nil 12994 1 4034 #login 8 '&Login' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 12848 nil 8 1736370288 2706 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 6 920646 5 ##(Smalltalk.ListViewColumn) 8 'Version' 241 #left 2706 #gemVersion 8 #() 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a gemVersion <= b gemVersion]' 8 #[30 105 226 0 226 32 130 106] #gemVersion 13376 7 513 nil nil nil 12848 nil 1 nil nil 13282 8 'Gem Host' 201 #left 2706 #gemHost 13344 13362 0 nil 13394 2 1 2704 8 'doIt' 8 '[:a :b | a gemHost <= b gemHost]' 8 #[30 105 226 0 226 32 130 106] #gemHost 13568 7 513 nil nil nil 12848 nil 1 nil nil 13282 8 'NetLDI' 201 #left 2706 #gemService 13344 13362 0 nil 13394 2 1 2704 8 'doIt' 8 '[:a :b | a gemService <= b gemService]' 8 #[30 105 226 0 226 32 130 106] #gemService 13696 7 513 nil nil nil 12848 nil 1 nil nil 13282 8 'Stone' 201 #left 2706 #stoneName 13344 13362 0 nil 13394 2 1 2704 8 'doIt' 8 '[:a :b | a stoneName <= b stoneName]' 8 #[30 105 226 0 226 32 130 106] #stoneName 13824 7 513 nil nil nil 12848 nil 1 nil nil 13282 8 'User' 201 #left 2706 #gsUserID 13344 13362 0 nil 13394 2 1 2704 8 'doIt' 8 '[:a :b | a gsUserID <= b gsUserID]' 8 #[30 105 226 0 226 32 130 106] #gsUserID 13952 7 513 nil nil nil 12848 nil 1 nil nil 13282 8 'Last Login' 441 #left 2706 #lastLoginString 8 #() 13362 0 nil 13394 2 1 2704 8 'doIt' 8 '[:a :b | a lastLogin <= b lastLogin]' 8 #[30 105 226 0 226 32 130 106] #lastLogin 14096 7 513 nil nil nil 12848 nil 1 nil nil #report 9664 nil 131169 nil 34 4 nil nil 514 1 1 nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 1 51 514 1319 297 12848 1026 #contextMenu: 34 1 12960 12848 1026 #text: 34 1 8 'Version' 12848 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 147 2 0 0 173 0 0 0] 8 #() 1216 nil 27 1234 1280 1 1664 -49 1312 1 7504 1 410 ##(Smalltalk.Toolbar) 34 25 nil 12736 34 2 8 1409289036 131137 14464 nil 480 nil 7 nil nil nil 14464 480 8 1736420640 170 192 9664 170 192 34 10 11265 1115206 1 ##(Smalltalk.ToolbarIconButton) 14464 1 4034 #editUndo 8 'Undo' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'icons\EditUndo.ico' nil 11265 11257 14594 14464 1 4034 #editCut 8 'Cut' 1 1 14658 nil true 14704 8 'icons\EditCut.ico' nil 11257 11259 14594 14464 1 4034 #editCopy 8 'Copy' 1 1 14658 nil true 14704 8 'icons\EditCopy.ico' nil 11259 11261 14594 14464 1 4034 #editPaste 8 'Paste' 1 1 14658 nil true 14704 8 'icons\EditPasteLink.ico' nil 11261 11263 14594 14464 1 4034 #editDelete 8 'Delete' 1 1 14658 nil true 14704 8 'icons\EditDelete.ico' nil 11263 34 6 14736 14816 14896 14976 14608 1049158 1 ##(Smalltalk.ToolbarSeparator) 14464 1 nil nil 1 nil 514 33 33 514 45 45 nil nil 962 138 144 34 2 1026 #createAt:extent: 34 2 514 1 1 514 241 51 14464 1026 #updateSizePosted 9664 14464 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 120 0 0 0 25 0 0 0] 8 #() 1216 nil 27 1234 1280 1 1296 241 7488 1 1328 51 410 ##(Smalltalk.PushButton) 34 20 nil 12736 34 2 8 1140924416 1 15328 nil nil nil 7 nil nil nil 15328 nil 8 1736479808 4034 #addLogin 8 '+' 1 1 nil nil false nil nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 1319 61 514 51 51 15328 1026 #isEnabled: 8 #(false) 15328 1026 #text: 34 1 8 '+' 15328 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 30 0 0 0 172 2 0 0 55 0 0 0] 8 #() 1216 nil 29 1234 1664 -49 1296 51 7488 61 1328 51 410 ##(Smalltalk.PushButton) 34 20 nil 12736 34 2 8 1140924416 1 15696 nil nil nil 7 nil nil nil 15696 nil 8 1736479808 4034 #removeLogin 8 '-' 1 1 nil nil false nil nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 1319 121 514 51 51 15696 1026 #isEnabled: 8 #(false) 15696 1026 #text: 34 1 8 '-' 15696 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 60 0 0 0 172 2 0 0 85 0 0 0] 8 #() 1216 nil 29 1234 1664 -49 1296 51 1312 11 1328 51 410 ##(Smalltalk.Toolbar) 34 25 nil 12736 34 2 8 1140853580 131137 16064 nil 480 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 16064 480 8 1736420640 170 192 9664 170 192 34 4 11191 1246790 1 ##(Smalltalk.ToolbarBitmapButton) 16064 1 4034 #openHelp 8 'Jade Help' 1 1 nil 11191 395334 3 ##(Smalltalk.Bitmap) nil true 14704 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 514 1857 33 71 11189 1246790 1 ##(Smalltalk.ToolbarSystemButton) 16064 1 4034 #getHelpOn 8 'Get help on widget' 1 1 nil 11189 1 23 34 2 16480 16304 nil nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 962 138 144 34 2 1026 #createAt:extent: 34 2 514 241 1 514 1129 51 16064 1026 #updateSizePosted 9664 16064 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 120 0 0 0 0 0 0 0 172 2 0 0 25 0 0 0] 8 #() 1216 nil 27 1234 2064 1 1664 1 7488 1 1328 51 170 192 34 6 12848 8 'loginList' 16064 8 'Other Tools' 14464 8 'Edit Tools' nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 1 1 514 1369 347 12736 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 173 0 0 0] 34 5 14464 16064 12848 15328 15696 1216 nil 27 39 true 170 192 34 4 608 8 'bottom' 12736 8 'top' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 12946 nil true 34 5 12994 1 4034 #addLogin 8 '&New' 9373 1 nil nil nil 12994 1 4034 #fileOpen 8 '&Open...' 9375 1 nil nil nil 12994 1 4034 #removeLogin 8 '&Remove' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 12994 1 4034 #exit 8 'E&xit' 17639 1 nil nil nil 8 '&Login' nil 134217729 nil nil 11143 nil nil 12946 nil true 34 9 12994 1 4034 #editUndo 8 '&Undo' 9397 1 nil nil nil 12994 1 4034 #editRedo 8 '&Redo' 9395 1 nil nil nil 17330 4097 12994 1 4034 #editCut 8 'Cu&t' 9393 1 nil nil nil 12994 1 4034 #editCopy 8 '&Copy' 9351 1 nil nil nil 12994 1 4034 #editPaste 8 '&Paste' 9389 1 nil nil nil 12994 1 4034 #editDelete 8 'De&lete' 1629 1 nil nil nil 17330 4097 12994 1 4034 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '&Edit' nil 134217729 nil nil 11159 nil nil 12946 nil true 34 10 12994 1 4034 #helpContents 8 '&Contents' 1 1 nil nil nil 12994 1 4034 #helpOnThisTool 8 '&On this Tool' 1249 1 nil nil nil 12994 1 4034 #helpWhatsThis 8 '&What''s This?' 5345 1 nil nil nil 17330 4097 12994 1 4034 #openGemTalkHomePage 8 '&GemTalk Home Page' 1025 1 nil nil nil 12994 1 4034 #openJadeHomePage 8 '&Jade Home Page' 1 1 nil nil nil 17330 4097 12994 1 4034 #showGemNRS 8 'Show Gem &NRS' 1 1 nil nil nil 12994 1 4034 #showStoneNRS 8 'Show Stone N&RS' 1 1 nil nil nil 12994 1 4034 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 11177 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 9563 14658 nil true 14704 8 'icons\GS32.ico' nil 14658 nil true 14704 8 'icons\GS16.ico' nil nil nil 1 nil nil 962 138 144 34 3 1026 #createAt:extent: 34 2 514 3839 21 514 1401 841 416 1026 #text: 34 1 8 'Jade Login' 416 1026 #updateMenuBar 9664 416 1154 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 59 10 0 0 174 1 0 0] 34 3 12736 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 18608 nil 480 nil 519 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 18608 nil 1 #left nil nil nil 514 1 1 514 9 9 nil 18704 nil 962 138 144 34 1 1026 #createAt:extent: 34 2 514 1 347 514 1369 11 18608 1154 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 173 0 0 0 172 2 0 0 178 0 0 0] 8 #() 1216 nil 27 608 1216 nil 27 )! !
!JadeLoginShell class categoriesFor: #icon!constants!public! !
!JadeLoginShell class categoriesFor: #resource_Default_view!public!resources-views! !

