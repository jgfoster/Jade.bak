"Filed out from Dolphin Smalltalk 7"!

DocumentShell subclass: #JadeLoginShell
	instanceVariableNames: 'debugPathPresenter gemHostPresenter gemServicePresenter gemTaskPresenter gemTypePresenter hostPasswordPresenter hostUserIDPresenter initialsPresenter isModified loginTypePresenter passwordPresenter stoneHostPresenter stoneNamePresenter userIDPresenter useSocketPresenter versionListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeLoginShell guid: (GUID fromString: '{0FA658CE-D1B0-40D0-9305-5830B29AED32}')!
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

createComponents

	super createComponents.
	versionListPresenter		:= self add: ListPresenter						new name: 'versionList'.
	stoneHostPresenter 		:= self add: TextPresenter 					new name: 'stoneHost'.
	stoneNamePresenter 	:= self add: TextPresenter 					new name: 'stoneName'.
	gemTypePresenter 		:= self add: RadioButtonSetPresenter 	new name: 'gemType'.
	gemHostPresenter 		:= self add: TextPresenter 					new name: 'gemHost'.
	gemServicePresenter 	:= self add: TextPresenter 					new name: 'gemService'.
	gemTaskPresenter 		:= self add: TextPresenter 					new name: 'gemTask'.
	userIDPresenter 			:= self add: TextPresenter 					new name: 'userID'.
	passwordPresenter 		:= self add: TextPresenter 					new name: 'password'.
	loginTypePresenter 		:= self add: RadioButtonSetPresenter 	new name: 'loginType'.
	hostUserIDPresenter	:= self add: TextPresenter						new name: 'hostUserID'.
	hostPasswordPresenter	:= self add: TextPresenter						new name: 'hostPassword'.
	initialsPresenter				:= self add: TextPresenter						new name: 'initials'.
	useSocketPresenter		:= self add: BooleanPresenter				new name: 'useSocket'.
	debugPathPresenter		:= self add: TextPresenter						new name: 'debugPath'.
!

createSchematicWiring

	super createSchematicWiring.
	versionListPresenter		when: #selectionChanged 	send: #isModified: 			to: self with: true.
	stoneHostPresenter 		when: #valueChanged 		send: #isModified: 			to: self with: true.
	stoneNamePresenter 	when: #valueChanged 		send: #isModified: 			to: self with: true.
	gemTypePresenter		when: #valueChanged 		send: #isModified: 			to: self with: true.
	gemHostPresenter 		when: #valueChanged 		send: #isModified: 			to: self with: true.
	gemServicePresenter 	when: #valueChanged 		send: #isModified: 			to: self with: true.
	gemTaskPresenter 		when: #valueChanged 		send: #isModified: 			to: self with: true.
	userIDPresenter 			when: #valueChanged 		send: #isModified: 			to: self with: true.
	passwordPresenter 		when: #valueChanged 		send: #isModified: 			to: self with: true.
	loginTypePresenter		when: #valueChanged 		send: #isModified: 			to: self with: true.
	hostUserIDPresenter	when: #valueChanged 		send: #isModified: 			to: self with: true.
	hostPasswordPresenter	when: #valueChanged 		send: #isModified: 			to: self with: true.
	initialsPresenter				when: #valueChanged		send: #isModified:				to: self with: true.
	useSocketPresenter		when: #valueChanged		send: #isModified:				to: self with: true.
	debugPathPresenter		when: #valueChanged		send: #isModified:				to: self with: true.!

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
!

openHomePage

	ShellLibrary default 
		shellOpen: 'http://www.gemtalksystems.com/index.php/products/gemstones/' 
		directory: ''.
!

openScintillaLibrary

	ScintillaLibrary default isNil ifTrue: [
		ScintillaLibrary open: SessionManager current imageBase , 'bin\SciLexer.dll'.
	].
!

postLogin: aGciSession

	self 
		openScintillaLibrary;
		setWorkingDirectory;
		yourself.
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

setCodeFont

	| font |
	font := FontDialog showModalOn: View desktop font.
	font notNil ifTrue: [
		CodeSourcePresenter codeFont: font.
	].
!

setDefaultFont

	| font |
	font := FontDialog showModalOn: View desktop font.
	font notNil ifTrue: [
		View desktop font: font.
	].
!

setDocumentData: aString

	self model updateFrom: (JadeLogin fromXML: aString).
	self isModified: false.
	self updateView.
!

setWorkingDirectory

	File workingDirectory: SessionManager current imageBase.
!

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
		gemVersion:		versionListPresenter 		selection;
		stoneHost: 		stoneHostPresenter 		value;
		stoneName: 		stoneNamePresenter 	value;
		gemType: 		gemTypePresenter 		value;
		gemHost: 			gemHostPresenter 		value;
		gemService: 		gemServicePresenter 	value;
		gemTask: 			gemTaskPresenter 		value;
		gsUserID: 		userIDPresenter 			value;
		gsPassword: 	passwordPresenter 		value;
		loginType: 		loginTypePresenter 		value;
		hostUserID:		hostUserIDPresenter	value;
		hostPassword:	hostPasswordPresenter	value;
		initials:				initialsPresenter				value;
		useSocket: 		useSocketPresenter		value;
		debugPath: 		debugPathPresenter		value;
		yourself.
!

updateView

	| selection |
	selection := versionListPresenter list 
		detect: [:each | each = self model gemVersion] 
		ifNone: [versionListPresenter list last].
	versionListPresenter		selection: selection.
	stoneHostPresenter 		value: self model stoneHost.
	stoneNamePresenter 	value: self model stoneName.
	gemTypePresenter 		value: self model gemType.
	gemHostPresenter 		value: self model gemHost.
	gemServicePresenter 	value: self model gemService.
	gemTaskPresenter 		value: self model gemTask.
	userIDPresenter 			value: self model gsUserID.
	passwordPresenter 		value: self model gsPassword.
	loginTypePresenter		value: self model loginType.
	hostUserIDPresenter	value: self model hostUserID.
	hostPasswordPresenter	value: self model hostPassword.
	initialsPresenter				value: self model initials.
	useSocketPresenter		value: self model useSocket.
	debugPathPresenter		value: self model debugPath.
	self isModified: false.
! !
!JadeLoginShell categoriesFor: #aboutJade!public! !
!JadeLoginShell categoriesFor: #basicCaption!accessing!public! !
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
!JadeLoginShell categoriesFor: #openScintillaLibrary!public! !
!JadeLoginShell categoriesFor: #postLogin:!public! !
!JadeLoginShell categoriesFor: #queryCommand:!public! !
!JadeLoginShell categoriesFor: #saveDocument!public! !
!JadeLoginShell categoriesFor: #sendRequiredMessages!public! !
!JadeLoginShell categoriesFor: #setCodeFont!commands!public! !
!JadeLoginShell categoriesFor: #setDefaultFont!commands!public! !
!JadeLoginShell categoriesFor: #setDocumentData:!accessing!public! !
!JadeLoginShell categoriesFor: #setWorkingDirectory!public! !
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 26476545 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  801 651 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 10 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1409289036 131137 624 0 482 8 4278190080 0 7 0 0 0 624 482 720 8 4294902561 234 256 98 0 234 256 98 6 10109 1115910 ##(Smalltalk.ToolbarIconButton)  10109 0 624 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #fileNew 8 'New' 1 1 263494 3 ##(Smalltalk.Icon)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'DocumentShell.ico' 0 395334 3 ##(Smalltalk.Bitmap)  0 16 0 0 0 0 3 530 33 33 1 10111 834 10111 0 624 1 866 8 #fileOpen 8 'Open' 1 1 930 0 16 976 8 'FileOpen.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr7.dll' 0 1010 0 16 0 0 0 0 3 530 33 33 1 10113 834 10113 0 624 1 866 8 #fileSave 8 'Save' 1 1 930 0 16 976 8 'FileSave.ico' 1168 1010 0 16 0 0 0 0 3 530 33 33 1 98 4 848 1056 1232 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 624 3 0 1 234 240 98 6 1328 5 1024 1 1200 3 0 1 0 530 33 33 530 45 45 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 151 51 624 1538 8 #updateSizePosted 784 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 75 0 0 0 25 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 1778 8 #fixedViewLeft 151 1778 8 #fixedParentTop 1 1778 8 #fixedViewTop 51 410 640 98 25 0 416 98 2 8 1140853580 131137 1920 0 482 8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 1920 482 2000 8 4294902561 234 256 784 234 256 98 4 10125 1246982 ##(Smalltalk.ToolbarSystemButton)  10125 0 1920 1 866 8 #getHelpOn 8 'Get help on widget' 1 1 0 1 23 10127 853766 ##(Smalltalk.ToolbarButton)  10127 0 1920 1 866 8 #openHelp 8 'Jade Help' 1 1 0 1010 0 16 976 8 'Tools.bmp' 1168 0 7 530 1857 33 71 98 2 2208 2288 234 240 98 4 1 1 2352 31 0 1 0 530 33 33 530 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 1474 202 208 98 2 1538 1568 98 2 530 391 1 530 379 51 1920 1538 1648 784 1920 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 0 0 0 0 128 1 0 0 25 0 0 0] 98 0 1728 0 27 1746 1778 8 #fixedPreviousRight 1 1778 8 #fixedParentRight 1 1856 1 1888 51 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1409286144 131073 2768 0 482 8 4278190080 0 7 0 0 0 2768 655878 ##(Smalltalk.CardLayout)  202 208 98 2 721414 ##(Smalltalk.Association)  8 'Basic' 410 8 ##(Smalltalk.ContainerView)  98 15 0 2768 98 2 8 1140850688 131073 2992 0 0 0 7 0 0 0 2992 562 234 240 98 28 410 8 ##(Smalltalk.TextEdit)  98 16 0 2992 98 2 8 1140916352 1025 3120 0 482 8 4278190080 0 7 0 0 0 3120 0 8 4294904703 852486 ##(Smalltalk.NullConverter)  0 0 1 1474 202 208 98 4 1538 1568 98 2 530 391 113 530 353 45 3120 1538 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 3120 1538 8 #isTextModified: 98 1 32 3120 1538 8 #setMarginWidths: 98 1 98 2 7 7 3120 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 56 0 0 0 115 1 0 0 78 0 0 0] 98 0 1728 0 27 1746 2704 11 2736 -9 1778 8 #fixedPreviousTop 1 1888 45 410 8 ##(Smalltalk.StaticText)  98 16 0 2992 98 2 8 1140850944 1 3680 0 0 0 7 0 0 0 3680 0 8 4294902455 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 11 67 530 371 31 3680 1538 8 #text: 98 1 8 'RPC Gem Host Name or IP:' 3680 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 190 0 0 0 48 0 0 0] 98 0 1728 0 27 1746 1792 11 1824 371 1778 8 #fixedPreviousBottom 11 1888 31 410 3696 98 16 0 2992 98 2 8 1140850944 1 4064 0 0 0 7 0 0 0 4064 0 8 4294902455 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 11 113 530 371 31 4064 1538 3920 98 1 8 'NetLDI Service Name or Port:' 4064 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 56 0 0 0 190 0 0 0 71 0 0 0] 98 0 1728 0 27 1746 1792 11 1824 371 4032 3 1888 31 410 3136 98 16 0 2992 98 2 8 1140916384 1025 4384 0 482 8 4278190080 0 7 0 0 0 4384 0 8 4294904703 3250 0 0 1 1474 202 208 98 4 1538 1568 98 2 530 391 259 530 353 45 4384 1538 3408 98 1 3442 3 1 3 4384 1538 3488 98 1 32 4384 1538 3536 98 1 98 2 7 7 4384 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 129 0 0 0 115 1 0 0 151 0 0 0] 98 0 1728 0 27 1746 2704 11 2736 -9 3648 1 1888 45 410 3696 98 16 0 2992 98 2 8 1140850944 1 4816 0 0 0 7 0 0 0 4816 0 8 4294902455 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 11 167 530 371 39 4816 1538 3920 98 1 8 'Stone Name:' 4816 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 83 0 0 0 190 0 0 0 102 0 0 0] 98 0 1728 0 27 1746 1792 11 1824 371 4032 11 1888 39 410 3696 98 16 0 2992 98 2 8 1140850944 1 5136 0 0 0 7 0 0 0 5136 0 8 4294902455 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 11 21 530 371 41 5136 1538 3920 98 1 8 'GemStone/Smalltalk Version:' 5136 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 190 0 0 0 30 0 0 0] 98 0 1728 0 27 1746 1792 11 1824 371 1856 21 1888 41 410 3136 98 16 0 2992 98 2 8 1140916352 1025 5456 0 482 3216 0 7 0 0 0 5456 0 8 4294904703 3250 0 0 1 1474 202 208 98 4 1538 1568 98 2 530 391 67 530 353 45 5456 1538 3408 98 1 3442 3 1 3 5456 1538 3488 98 1 32 5456 1538 3536 98 1 98 2 7 7 5456 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 33 0 0 0 115 1 0 0 55 0 0 0] 98 0 1728 0 27 1746 2704 11 2736 -9 3648 1 1888 45 410 3696 98 16 0 2992 98 2 8 1140850944 1 5872 0 0 0 7 0 0 0 5872 0 8 4294902455 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 11 313 530 371 41 5872 1538 3920 98 1 8 'Monticello User Name:' 5872 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 156 0 0 0 190 0 0 0 176 0 0 0] 98 0 1728 0 27 1746 1792 11 1824 371 4032 11 1888 41 410 3136 98 16 0 2992 98 2 8 1140916352 1025 6192 0 482 3216 0 7 0 0 0 6192 0 8 4294904703 3250 0 0 1 1474 202 208 98 4 1538 1568 98 2 530 391 213 530 353 45 6192 1538 3408 98 1 3442 3 1 3 6192 1538 3488 98 1 32 6192 1538 3536 98 1 98 2 7 7 6192 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 106 0 0 0 115 1 0 0 128 0 0 0] 98 0 1728 0 27 1746 2704 11 2736 -9 3648 1 1888 45 410 3696 98 16 0 2992 98 2 8 1140850944 1 6608 0 0 0 7 0 0 0 6608 0 8 4294902455 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 11 213 530 371 41 6608 1538 3920 98 1 8 'GemStone User ID:' 6608 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 106 0 0 0 190 0 0 0 126 0 0 0] 98 0 1728 0 27 1746 1792 11 1824 371 4032 3 1888 41 410 3136 98 16 0 2992 98 2 8 1140916352 1025 6928 0 482 8 4278190080 0 7 0 0 0 6928 0 8 4294904703 3250 0 0 1 1474 202 208 98 4 1538 1568 98 2 530 391 313 530 353 45 6928 1538 3408 98 1 3442 3 1 3 6928 1538 3488 98 1 32 6928 1538 3536 98 1 98 2 7 7 6928 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 156 0 0 0 115 1 0 0 178 0 0 0] 98 0 1728 0 27 1746 2704 11 2736 -9 3648 1 1888 45 410 3136 98 16 0 2992 98 2 8 1140916352 1025 7360 0 482 3216 0 7 0 0 0 7360 0 8 4294904703 3250 0 0 1 1474 202 208 98 4 1538 1568 98 2 530 391 167 530 353 45 7360 1538 3408 98 1 3442 3 1 3 7360 1538 3488 98 1 32 7360 1538 3536 98 1 98 2 7 7 7360 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 83 0 0 0 115 1 0 0 105 0 0 0] 98 0 1728 0 27 1746 2704 11 2736 -9 3648 1 1888 45 410 8 ##(Smalltalk.ComboBox)  98 17 0 2992 98 2 8 1144063491 1025 7776 590662 2 ##(Smalltalk.ListModel)  202 208 784 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 7776 0 8 4294904685 459270 ##(Smalltalk.Message)  8 #displayString 98 0 784 401 1474 202 208 98 1 1538 1568 98 2 530 391 11 530 353 53 7776 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 5 0 0 0 115 1 0 0 31 0 0 0] 98 0 1728 0 27 1746 2704 11 2736 -9 1856 11 1888 47 410 3696 98 16 0 2992 98 2 8 1140850944 1 8224 0 0 0 7 0 0 0 8224 0 8 4294902455 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 11 259 530 371 41 8224 1538 3920 98 1 8 'GemStone Password:' 8224 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 129 0 0 0 190 0 0 0 149 0 0 0] 98 0 1728 0 27 1746 1792 11 1824 371 4032 3 1888 41 234 256 98 14 7360 8 'stoneName' 5456 8 'gemHost' 6192 8 'userID' 4384 8 'password' 6928 8 'initials' 7776 8 'versionList' 3120 8 'gemService' 0 1474 202 208 98 1 1538 1568 98 2 530 9 55 530 753 361 2992 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 124 1 0 0 207 0 0 0] 98 14 5136 7776 3680 5456 4064 3120 4816 7360 6608 6192 8224 4384 5872 6928 1728 0 27 2946 8 'Advanced' 410 3008 98 15 0 2768 98 2 8 1140850688 131073 8880 0 0 0 5 0 0 0 8880 562 234 240 98 26 410 8 ##(Smalltalk.StaticPath)  98 17 0 8880 98 2 8 1140850944 1 8992 0 0 0 5 0 0 0 8992 0 8 4294902455 3250 0 0 0 8 'XXX' 1474 202 208 98 2 1538 1568 98 2 530 161 59 530 583 45 8992 1538 3920 98 1 9104 8992 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 29 0 0 0 115 1 0 0 51 0 0 0] 98 0 1728 0 27 1746 1792 161 2736 -9 3648 11 1888 45 410 3136 98 16 0 8880 98 2 8 1140916384 1025 9328 0 482 8 4278190080 0 5 0 0 0 9328 0 8 4294904703 3250 0 0 1 1474 202 208 98 4 1538 1568 98 2 530 491 253 530 253 45 9328 1538 3408 98 1 3442 3 1 3 9328 1538 3488 98 1 32 9328 1538 3536 98 1 98 2 7 7 9328 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 126 0 0 0 115 1 0 0 148 0 0 0] 98 0 1728 0 27 1746 2704 11 2736 -9 3648 1 1888 45 410 3136 98 16 0 8880 98 2 8 1140916352 1025 9760 0 482 3216 0 5 0 0 0 9760 0 8 4294904703 3250 0 0 1 1474 202 208 98 5 1538 1568 98 2 530 321 159 530 423 45 9760 1538 3920 98 1 8 'gemnetobject' 9760 1538 3408 98 1 3442 3 1 3 9760 1538 3488 98 1 32 9760 1538 3536 98 1 98 2 7 7 9760 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 79 0 0 0 115 1 0 0 101 0 0 0] 98 0 1728 0 27 1746 2704 11 2736 -9 3648 1 1888 45 410 3008 98 15 0 8880 98 2 8 1409286144 393217 10224 0 482 8 4278190080 0 5 0 0 0 10224 656390 ##(Smalltalk.GridLayout)  1 3 1 1 234 256 98 4 410 8 ##(Smalltalk.RadioButton)  98 16 0 10224 98 2 8 1140924425 1 10384 721990 2 ##(Smalltalk.ValueHolder)  0 0 1376774 ##(Smalltalk.PluggableSearchPolicy)  7986 8 #= 98 0 7986 8 #hash 98 0 32 482 10304 0 5 0 0 0 10384 0 8 4294903405 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 1 39 530 297 39 10384 1538 3920 98 1 8 'Native OS User' 10384 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 148 0 0 0 38 0 0 0] 98 0 1728 0 27 8 'member' 410 10400 98 16 0 10224 98 2 8 1141055497 1 10896 10466 0 0 10498 7986 10544 98 0 7986 10592 98 0 32 482 10304 0 5 0 0 0 10896 0 8 4294903405 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 1 1 530 297 39 10896 1538 3920 98 1 8 'NetLDI Guest' 10896 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 148 0 0 0 19 0 0 0] 98 0 1728 0 27 8 'guest' 0 1474 202 208 98 1 1538 1568 98 2 530 11 207 530 301 81 10224 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 103 0 0 0 155 0 0 0 143 0 0 0] 98 2 10896 10384 1728 0 27 1746 1792 11 1824 301 4032 5 1888 81 410 3136 98 16 0 8880 98 2 8 1140916352 1025 11504 0 482 8 4278190080 0 5 0 0 0 11504 0 8 4294904703 3250 0 0 1 1474 202 208 98 4 1538 1568 98 2 530 491 207 530 253 45 11504 1538 3408 98 1 3442 3 1 3 11504 1538 3488 98 1 32 11504 1538 3536 98 1 98 2 7 7 11504 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 103 0 0 0 115 1 0 0 125 0 0 0] 98 0 1728 0 27 1746 2704 11 2736 -9 3648 1 1888 45 410 3696 98 16 0 8880 98 2 8 1140850944 1 11936 0 0 0 5 0 0 0 11936 0 8 4294902455 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 11 113 530 301 41 11936 1538 3920 98 1 8 'Stone Host (from Gem):' 11936 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 56 0 0 0 155 0 0 0 76 0 0 0] 98 0 1728 0 27 1746 1792 11 1824 301 4032 11 1888 41 410 8 ##(Smalltalk.PushButton)  98 20 0 8880 98 2 8 1140924416 1 12256 0 0 0 5 0 0 0 12256 0 8 4294903405 866 8 #debugGCI 8 'Debug GCI' 1 1 0 0 32 0 0 0 1474 202 208 98 3 1538 1568 98 2 530 11 49 530 141 51 12256 1538 8 #isEnabled: 98 1 32 12256 1538 3920 98 1 8 'Debug GCI' 12256 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 24 0 0 0 75 0 0 0 49 0 0 0] 98 0 1728 0 29 1746 1792 11 1824 141 4032 5 1888 51 410 3008 98 15 0 8880 98 2 8 1140850688 393217 12672 0 482 8 4278190080 0 5 0 0 0 12672 10322 3 5 1 1 234 256 98 4 410 10400 98 16 0 12672 98 2 8 1140924425 1 12816 10466 0 0 10498 7986 10544 98 0 7986 10592 98 0 32 482 12752 0 5 0 0 0 12816 0 8 4294903405 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 1 1 530 365 45 12816 1538 3920 98 1 8 'RPC Gem' 12816 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 182 0 0 0 22 0 0 0] 98 0 1728 0 27 8 'remote' 410 10400 98 16 0 12672 98 2 8 1141055497 1 13248 10466 0 0 10498 7986 10544 98 0 7986 10592 98 0 32 482 12752 0 5 0 0 0 13248 0 8 4294903405 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 365 1 530 365 45 13248 1538 3920 98 1 8 'Linked Gem (32-bit only)' 13248 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 182 0 0 0 0 0 0 0 108 1 0 0 22 0 0 0] 98 0 1728 0 27 8 'linked' 0 1474 202 208 98 1 1538 1568 98 2 530 11 301 530 733 49 12672 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 150 0 0 0 115 1 0 0 174 0 0 0] 98 2 12816 13248 1728 0 27 1746 1792 11 2736 -9 4032 5 1888 49 410 3696 98 16 0 8880 98 2 8 1140850944 1 13856 0 0 0 5 0 0 0 13856 0 8 4294902455 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 321 253 530 161 41 13856 1538 3920 98 1 8 'Password:' 13856 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 126 0 0 0 240 0 0 0 146 0 0 0] 98 0 1728 0 27 1746 1778 8 #fixedPreviousLeft -169 1824 161 4032 3 1888 41 410 3696 98 16 0 8880 98 2 8 1140850944 1 14208 0 0 0 5 0 0 0 14208 0 8 4294902455 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 11 159 530 301 31 14208 1538 3920 98 1 8 'NetLDI Task Name:' 14208 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 79 0 0 0 155 0 0 0 94 0 0 0] 98 0 1728 0 27 1746 1792 11 1824 301 4032 3 1888 31 410 3136 98 16 0 8880 98 2 8 1140916352 1025 14528 0 482 3216 0 5 0 0 0 14528 0 8 4294904703 3250 0 0 1 1474 202 208 98 5 1538 1568 98 2 530 321 113 530 423 45 14528 1538 3920 98 1 8 'localhost' 14528 1538 3408 98 1 3442 3 1 3 14528 1538 3488 98 1 32 14528 1538 3536 98 1 98 2 7 7 14528 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 56 0 0 0 115 1 0 0 78 0 0 0] 98 0 1728 0 27 1746 2704 11 2736 -9 3648 1 1888 45 410 8 ##(Smalltalk.CheckBox)  98 16 0 8880 98 2 8 1409363203 1 14992 10466 0 0 1114118 ##(Smalltalk.NeverSearchPolicy)  32 0 0 5 0 0 0 14992 0 8 4294903405 3250 0 0 0 1474 202 208 98 3 1538 1568 98 2 530 11 5 530 733 41 14992 1538 12528 98 1 32 14992 1538 3920 98 1 8 'Open new socket  to improve performance (experimental)' 14992 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 115 1 0 0 22 0 0 0] 98 0 1728 0 27 1746 1792 11 2736 -9 1856 5 1888 41 410 3696 98 16 0 8880 98 2 8 1140850944 1 15408 0 0 0 5 0 0 0 15408 0 8 4294902455 3250 0 0 0 1474 202 208 98 2 1538 1568 98 2 530 321 207 530 161 41 15408 1538 3920 98 1 8 'OS User ID:' 15408 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 103 0 0 0 240 0 0 0 123 0 0 0] 98 0 1728 0 27 1746 2704 11 1824 161 3648 1 1888 41 234 256 98 16 12672 8 'gemType' 14992 8 'useSocket' 14528 8 'stoneHost' 9760 8 'gemTask' 11504 8 'hostUserID' 10224 8 'loginType' 8992 8 'debugPath' 9328 8 'hostPassword' 0 1474 202 208 98 1 1538 1568 98 2 530 9 55 530 753 361 8880 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 124 1 0 0 207 0 0 0] 98 13 14992 12256 8992 11936 14528 14208 9760 10224 15408 11504 13856 9328 12672 1728 0 27 2992 234 256 98 4 2992 8 'Basic' 8880 8 'Advanced' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 2768 98 2 8 1140916736 1 16112 7858 202 208 98 2 2976 8864 0 7920 0 0 1 0 0 0 16112 0 8 4294902167 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 8016 16272 7 257 0 16258 0 0 16290 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 16368 7 257 0 1049926 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 8 #noIcons 0 0 0 0 0 1474 202 208 98 3 1538 1568 98 2 530 1 1 530 769 423 16112 1538 8 #basicSelectionsByIndex: 98 1 98 1 3 16112 1538 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 16112 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 211 0 0 0] 98 0 1728 0 27 1474 202 208 98 1 1538 1568 98 2 530 1 51 530 769 423 2768 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 128 1 0 0 236 0 0 0] 98 3 2992 8880 16112 1728 0 27 1746 1792 1 2736 1 1856 51 1778 8 #fixedParentBottom -59 410 12272 98 20 0 416 98 2 8 1140924416 1 16992 0 0 0 7 0 0 0 16992 0 8 4294903405 866 8 #login 8 'Login' 1 1 0 0 16 0 0 0 1474 202 208 98 3 1538 1568 98 2 530 1 473 530 769 61 16992 1538 12528 98 1 32 16992 1538 3920 98 1 8 'Login' 16992 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 236 0 0 0 128 1 0 0 10 1 0 0] 98 0 1728 0 29 1746 1792 1 2736 1 16960 -59 1888 61 410 640 98 25 0 416 98 2 8 1409289036 131137 17376 0 482 8 4278190080 0 7 0 0 0 17376 482 17456 8 4294902561 234 256 784 234 256 98 10 10121 2274 10121 0 17376 1 866 8 #editDelete 8 'Delete' 1 1 0 1010 0 16 976 8 'EditBar.bmp' 1168 0 7 530 193 33 11 10123 2274 10123 0 17376 1 866 8 #editUndo 8 'Undo' 1 1 0 17616 7 10115 2274 10115 0 17376 1 866 8 #editCut 8 'Cut' 1 1 0 17616 1 10117 2274 10117 0 17376 1 866 8 #editCopy 8 'Copy' 1 1 0 17616 3 10119 2274 10119 0 17376 1 866 8 #editPaste 8 'Paste' 1 1 0 17616 5 98 6 17728 17792 17856 17552 17664 1378 0 0 17376 3 0 1 234 240 98 2 17616 1 0 1 0 530 33 33 530 45 45 0 0 1474 202 208 98 2 1538 1568 98 2 530 151 1 530 241 51 17376 1538 1648 784 17376 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 0 0 0 0 195 0 0 0 25 0 0 0] 98 0 1728 0 27 1746 2704 1 1824 241 1856 1 1888 51 234 256 98 8 2768 8 'Tabs' 1920 8 'Other Tools' 624 8 'File Tools' 17376 8 'Edit Tools' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 866 896 8 '&New' 9373 1 0 0 0 18402 1 866 1088 8 '&Open...' 9375 1 0 0 0 18402 1 866 1264 8 '&Save' 9383 1 0 0 0 18402 1 866 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 18402 1 866 8 #fileRevert 8 '&Revert' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 18402 1 866 8 #exit 8 'E&xit' 17639 1 0 0 0 8 '&File' 0 134217729 0 0 10073 0 0 18354 0 16 98 9 18402 1 866 17696 8 '&Undo' 9397 1 0 0 0 18402 1 866 8 #editRedo 8 '&Redo' 9395 1 0 0 0 18690 4097 18402 1 866 17760 8 'Cu&t' 9393 1 0 0 0 18402 1 866 17824 8 '&Copy' 9351 1 0 0 0 18402 1 866 17888 8 '&Paste' 9389 1 0 0 0 18402 1 866 17584 8 'De&lete' 1629 1 0 0 0 18690 4097 18402 1 866 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 8 '&Edit' 0 134217729 0 0 10089 0 0 18354 0 16 98 2 18402 1 866 8 #setDefaultFont 8 'Set Default &Font' 1 1 0 0 0 18402 1 866 8 #setCodeFont 8 'Set &Code Font' 1 1 0 0 0 8 '&Preferences' 0 134217729 0 0 10095 0 0 18354 0 16 98 7 18402 1 866 8 #helpContents 8 '&Contents' 1 1 0 0 0 18402 1 866 8 #helpOnThisTool 8 '&On this Tool' 1249 1 0 0 0 18402 1 866 8 #helpWhatsThis 8 '&What''s This?' 5345 1 0 0 0 18690 4097 18402 1 866 8 #openHomePage 8 '&GemStone Home Page' 1025 1 0 0 0 18690 4097 18402 1 866 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 134217729 0 0 10107 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 9343 930 0 16 976 8 'icons\GS32.ico' 0 930 0 16 976 8 'icons\GS16.ico' 0 0 0 1 0 0 1474 202 208 98 3 1538 1568 98 2 530 5119 21 530 801 651 416 1538 3920 98 1 8 'Jade Login' 416 1538 8 #updateMenuBar 784 416 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 143 11 0 0 79 1 0 0] 98 5 624 17376 1920 16992 2768 1728 0 27 )!

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

