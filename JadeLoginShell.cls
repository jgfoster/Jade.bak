"Filed out from Dolphin Smalltalk 7"!

DocumentShell subclass: #JadeLoginShell
	instanceVariableNames: 'debugPathPresenter gemHostPresenter gemServicePresenter gemTaskPresenter gemTypePresenter hostPasswordPresenter hostUserIDPresenter initialsPresenter isModified loginTypePresenter passwordPresenter stoneHostPresenter stoneNamePresenter userIDPresenter useSocketPresenter versionListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeLoginShell guid: (GUID fromString: '{0FA658CE-D1B0-40D0-9305-5830B29AED32}')!
JadeLoginShell comment: ''!
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
		Keyboard default isShiftDown ifTrue: [ex halt].
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
	JadeTranscript showOn: aGciSession.
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

	^Icon fromFile: 'icons\GS32x32.ico'.
!

resource_Compact_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 26476545 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  701 471 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 52 410 8 ##(Smalltalk.GroupBox)  98 14 0 416 98 2 8 1140850695 65 624 0 482 8 4294967295 0 7 0 0 0 624 0 8 4294903245 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 5 211 530 681 91 624 818 8 #text: 98 1 8 'Gem' 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 105 0 0 0 86 1 0 0 150 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 5 1090 8 #fixedViewLeft 681 1090 8 #fixedParentTop 211 1090 8 #fixedViewTop 91 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1140853580 131137 1232 0 482 8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 1232 482 1328 8 4294903147 234 256 98 0 234 256 98 4 21567 1246982 ##(Smalltalk.ToolbarSystemButton)  21567 0 1232 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #getHelpOn 8 'Get help on widget' 1 1 0 1 23 21569 853766 ##(Smalltalk.ToolbarButton)  21569 0 1232 1 1570 8 #openHelp 8 'Jade Help' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 530 1857 33 71 98 2 1552 1648 234 240 98 4 1 1 1728 31 0 1 0 530 33 33 530 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 754 202 208 98 2 818 848 98 2 530 391 1 530 299 51 1232 818 8 #updateSize 1488 1232 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 0 0 0 0 88 1 0 0 25 0 0 0] 98 0 1040 0 27 1058 1090 8 #fixedPreviousRight 1 1090 8 #fixedParentRight 1 1168 1 1200 51 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2240 0 0 0 7 0 0 0 2240 0 8 4294903255 852486 ##(Smalltalk.NullConverter)  0 0 0 754 202 208 98 2 818 848 98 2 530 611 651 530 151 41 2240 818 928 98 1 8 'Password:' 2240 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 69 1 0 0 124 1 0 0 89 1 0 0] 98 0 1040 0 27 1058 1104 611 1136 151 1168 651 1200 41 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 2592 0 482 8 4278190080 0 7 0 0 0 2592 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 471 241 530 201 41 2592 818 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 2592 818 8 #isTextModified: 98 1 32 2592 818 8 #setMarginWidths: 98 1 98 2 7 7 2592 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 235 0 0 0 120 0 0 0 79 1 0 0 140 0 0 0] 98 0 1040 0 27 1058 1104 471 1136 201 1168 241 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 3104 0 0 0 7 0 0 0 3104 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 611 611 530 151 41 3104 818 928 98 1 8 'User ID:' 3104 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 49 1 0 0 124 1 0 0 69 1 0 0] 98 0 1040 0 27 1058 1104 611 1136 151 1168 611 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 3424 0 0 0 7 0 0 0 3424 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 21 247 530 91 31 3424 818 928 98 1 8 'Host/IP' 3424 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 123 0 0 0 55 0 0 0 138 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 247 1200 31 410 2256 98 16 0 416 98 2 8 1140850944 1 3744 0 0 0 7 0 0 0 3744 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 341 127 530 121 31 3744 818 928 98 1 8 'Database' 3744 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 170 0 0 0 63 0 0 0 230 0 0 0 78 0 0 0] 98 0 1040 0 27 1058 1104 341 1136 121 1168 127 1200 31 410 2608 98 16 0 416 98 2 8 1140916352 1025 4064 0 482 8 4278190080 0 7 0 0 0 4064 0 8 4294903261 2338 0 0 1 754 202 208 98 3 818 848 98 2 530 135 321 530 321 41 4064 818 2864 98 1 2898 3 1 3 4064 818 2944 98 1 32 4064 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 67 0 0 0 160 0 0 0 227 0 0 0 180 0 0 0] 98 0 1040 0 27 1058 1104 135 1136 321 1168 321 1200 41 410 1248 98 25 0 416 98 2 8 1409289036 131137 4448 0 482 8 4278190080 0 7 0 0 0 4448 482 4528 8 4294903147 234 256 1488 234 256 98 10 21561 1634 21561 0 4448 1 1570 8 #editPaste 8 'Paste' 1 1 0 1714 0 16 1760 8 'EditBar.bmp' 1808 0 7 530 193 33 5 21563 1634 21563 0 4448 1 1570 8 #editDelete 8 'Delete' 1 1 0 4688 11 21565 1634 21565 0 4448 1 1570 8 #editUndo 8 'Undo' 1 1 0 4688 7 21557 1634 21557 0 4448 1 1570 8 #editCut 8 'Cut' 1 1 0 4688 1 21559 1634 21559 0 4448 1 1570 8 #editCopy 8 'Copy' 1 1 0 4688 3 98 6 4864 4928 4624 4736 4800 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 4448 3 0 1 234 240 98 2 4688 1 0 1 0 530 33 33 530 45 45 0 0 754 202 208 98 2 818 848 98 2 530 151 1 530 241 51 4448 818 2096 1488 4448 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 0 0 0 0 195 0 0 0 25 0 0 0] 98 0 1040 0 27 1058 2176 1 1136 241 1168 1 1200 51 410 2608 98 16 0 416 98 2 8 1140916352 1025 5296 0 482 2688 0 7 0 0 0 5296 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 121 241 530 201 41 5296 818 2864 98 1 2898 3 1 3 5296 818 2944 98 1 32 5296 818 2992 98 1 98 2 7 7 5296 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 120 0 0 0 160 0 0 0 140 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 241 1200 41 410 2608 98 16 0 416 98 2 8 1140916352 1025 5712 0 482 8 4278190080 0 7 0 0 0 5712 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 81 581 530 201 41 5712 818 2864 98 1 2898 3 1 3 5712 818 2944 98 1 32 5712 818 2992 98 1 98 2 7 7 5712 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 40 0 0 0 34 1 0 0 140 0 0 0 54 1 0 0] 98 0 1040 0 27 1058 1104 81 1136 201 1168 581 1200 41 410 8 ##(Smalltalk.ComboBox)  98 17 0 416 98 2 8 1144063491 1025 6144 590662 2 ##(Smalltalk.ListModel)  202 208 1488 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 6144 0 8 4294903259 459270 ##(Smalltalk.Message)  8 #displayString 98 0 1488 401 754 202 208 98 1 818 848 98 2 530 131 51 530 371 47 6144 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 65 0 0 0 25 0 0 0 250 0 0 0 48 0 0 0] 98 0 1040 0 27 1058 1104 131 1136 371 1168 51 1200 47 410 2256 98 16 0 416 98 2 8 1140850944 1 6592 0 0 0 7 0 0 0 6592 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 21 167 530 71 31 6592 818 928 98 1 8 'User' 6592 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 83 0 0 0 45 0 0 0 98 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 71 1168 167 1200 31 410 2256 98 16 0 416 98 2 8 1140850944 1 6912 0 0 0 7 0 0 0 6912 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 341 247 530 91 31 6912 818 928 98 1 8 'NetLDI	' 6912 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 170 0 0 0 123 0 0 0 215 0 0 0 138 0 0 0] 98 0 1040 0 27 1058 1104 341 1136 91 1168 247 1200 31 410 2608 98 16 0 416 98 2 8 1140916384 1025 7232 0 482 8 4278190080 0 7 0 0 0 7232 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 361 711 530 201 41 7232 818 2864 98 1 2898 3 1 3 7232 818 2944 98 1 32 7232 818 2992 98 1 98 2 7 7 7232 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 99 1 0 0 24 1 0 0 119 1 0 0] 98 0 1040 0 27 1058 1104 361 1136 201 1168 711 1200 41 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 7664 0 0 0 7 0 0 0 7664 0 8 4294903245 1570 8 #login 8 'Login' 1 1 0 0 16 0 0 0 754 202 208 98 3 818 848 98 2 530 481 311 530 201 61 7664 818 8 #isEnabled: 98 1 32 7664 818 928 98 1 8 'Login' 7664 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 0 0 0 155 0 0 0 84 1 0 0 185 0 0 0] 98 0 1040 0 29 1058 1104 481 1136 201 1168 311 1200 61 410 2608 98 16 0 416 98 2 8 1140916352 1025 8080 0 482 2688 0 7 0 0 0 8080 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 121 121 530 201 41 8080 818 2864 98 1 2898 3 1 3 8080 818 2944 98 1 32 8080 818 2992 98 1 98 2 7 7 8080 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 60 0 0 0 160 0 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 121 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 8496 0 0 0 7 0 0 0 8496 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 11 321 530 121 31 8496 818 928 98 1 8 'Developer' 8496 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 160 0 0 0 65 0 0 0 175 0 0 0] 98 0 1040 0 27 1058 1104 11 1136 121 1168 321 1200 31 410 2256 98 16 0 416 98 2 8 1140850944 1 8816 0 0 0 7 0 0 0 8816 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 341 167 530 121 31 8816 818 928 98 1 8 'Password' 8816 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 170 0 0 0 83 0 0 0 230 0 0 0 98 0 0 0] 98 0 1040 0 27 1058 1104 341 1136 121 1168 167 1200 31 410 2608 98 16 0 416 98 2 8 1140916384 1025 9136 0 482 8 4278190080 0 7 0 0 0 9136 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 471 161 530 201 41 9136 818 2864 98 1 2898 3 1 3 9136 818 2944 98 1 32 9136 818 2992 98 1 98 2 7 7 9136 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 235 0 0 0 80 0 0 0 79 1 0 0 100 0 0 0] 98 0 1040 0 27 1058 1104 471 1136 201 1168 161 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 9568 0 0 0 7 0 0 0 9568 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 21 129 530 91 31 9568 818 928 98 1 8 'Host/IP' 9568 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 64 0 0 0 55 0 0 0 79 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 129 1200 31 410 1248 98 25 0 416 98 2 8 1409289036 131137 9888 0 482 8 4278190080 0 7 0 0 0 9888 482 9968 8 4294903147 234 256 1488 234 256 98 6 21551 1115910 ##(Smalltalk.ToolbarIconButton)  21551 0 9888 1 1570 8 #fileNew 8 'New' 1 1 263494 3 ##(Smalltalk.Icon)  0 16 1760 8 'DocumentShell.ico' 0 1714 0 16 0 0 0 0 3 530 33 33 1 21553 10066 21553 0 9888 1 1570 8 #fileOpen 8 'Open' 1 1 10146 0 16 1760 8 'FileOpen.ico' 1808 1714 0 16 0 0 0 0 3 530 33 33 1 21555 10066 21555 0 9888 1 1570 8 #fileSave 8 'Save' 1 1 10146 0 16 1760 8 'FileSave.ico' 1808 1714 0 16 0 0 0 0 3 530 33 33 1 98 4 10080 10224 10352 5010 0 0 9888 3 0 1 234 240 98 6 10448 5 10192 1 10320 3 0 1 0 530 33 33 530 45 45 0 0 754 202 208 98 2 818 848 98 2 530 1 1 530 151 51 9888 818 2096 1488 9888 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 75 0 0 0 25 0 0 0] 98 0 1040 0 27 1058 1104 1 1136 151 1168 1 1200 51 410 2608 98 16 0 416 98 2 8 1140916352 1025 10768 0 482 2688 0 7 0 0 0 10768 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 471 121 530 201 41 10768 818 2864 98 1 2898 3 1 3 10768 818 2944 98 1 32 10768 818 2992 98 1 98 2 7 7 10768 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 235 0 0 0 60 0 0 0 79 1 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 471 1136 201 1168 121 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 11184 0 0 0 7 0 0 0 11184 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 21 61 530 91 31 11184 818 928 98 1 8 'Version' 11184 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 30 0 0 0 55 0 0 0 45 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 61 1200 31 410 640 98 14 0 416 98 2 8 1140850695 65 11504 0 482 720 0 7 0 0 0 11504 0 8 4294903245 754 202 208 98 2 818 848 98 2 530 5 91 530 681 121 11504 818 928 98 1 8 'Stone' 11504 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 45 0 0 0 86 1 0 0 105 0 0 0] 98 0 1040 0 27 1058 1104 5 1136 681 1168 91 1200 121 410 2608 98 16 0 416 98 2 8 1140916352 1025 11824 0 482 2688 0 7 0 0 0 11824 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 121 161 530 201 41 11824 818 2864 98 1 2898 3 1 3 11824 818 2944 98 1 32 11824 818 2992 98 1 98 2 7 7 11824 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 80 0 0 0 160 0 0 0 100 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 161 1200 41 234 256 98 26 1232 8 'Other Tools' 2592 8 'gemService' 5296 8 'gemHost' 4064 8 'initials' 4448 8 'Edit Tools' 5712 8 'hostUserID' 6144 8 'versionList' 7232 8 'hostPassword' 8080 8 'stoneHost' 9136 8 'password' 9888 8 'File Tools' 10768 8 'stoneName' 11824 8 'userID' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 1570 10112 8 '&New' 9373 1 0 0 0 12578 1 1570 10256 8 '&Open...' 9375 1 0 0 0 12578 1 1570 10384 8 '&Save' 9383 1 0 0 0 12578 1 1570 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 12578 1 1570 8 #fileRevert 8 '&Revert' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 12578 1 1570 8 #exit 8 'E&xit' 17639 1 0 0 0 8 '&File' 0 134217729 0 0 21515 0 0 12530 0 16 98 9 12578 1 1570 4832 8 '&Undo' 9397 1 0 0 0 12578 1 1570 8 #editRedo 8 '&Redo' 9395 1 0 0 0 12866 4097 12578 1 1570 4896 8 'Cu&t' 9393 1 0 0 0 12578 1 1570 4960 8 '&Copy' 9351 1 0 0 0 12578 1 1570 4656 8 '&Paste' 9389 1 0 0 0 12578 1 1570 4768 8 'De&lete' 1629 1 0 0 0 12866 4097 12578 1 1570 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 8 '&Edit' 0 134217729 0 0 21531 0 0 12530 0 16 98 2 12578 1 1570 8 #setDefaultFont 8 'Set Default &Font' 1 1 0 0 0 12578 1 1570 8 #setCodeFont 8 'Set &Code Font' 1 1 0 0 0 8 '&Preferences' 0 134217729 0 0 21537 0 0 12530 0 16 98 7 12578 1 1570 8 #helpContents 8 '&Contents' 1 1 0 0 0 12578 1 1570 8 #helpOnThisTool 8 '&On this Tool' 1249 1 0 0 0 12578 1 1570 8 #helpWhatsThis 8 '&What''s This?' 5345 1 0 0 0 12866 4097 12578 1 1570 8 #openHomePage 8 '&GemStone Home Page' 1025 1 0 0 0 12866 4097 12578 1 1570 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 134217729 0 0 21549 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 10323 10146 0 16 1760 8 'icons\GS32x32.ico' 0 10146 0 16 1760 8 'icons\GS16x16.ico' 0 0 0 1 0 0 754 202 208 98 3 818 848 98 2 530 2879 21 530 701 471 416 818 928 98 1 8 'Jade Login' 416 818 8 #updateMenuBar 1488 416 978 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 253 6 0 0 245 0 0 0] 98 26 6144 8080 10768 11824 9136 5296 2592 4064 9888 4448 1232 5712 7232 7664 9568 3744 6912 3104 2240 3424 11504 624 11184 6592 8816 8496 1040 0 27 )!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 26476545 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  801 601 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 10 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1409289036 131137 624 0 482 8 4278190080 0 7 0 0 0 624 482 720 8 4294902555 234 256 98 0 234 256 98 10 17535 853766 ##(Smalltalk.ToolbarButton)  17535 0 624 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #editUndo 8 'Undo' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'EditBar.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 530 193 33 7 17527 834 17527 0 624 1 866 8 #editCut 8 'Cut' 1 1 0 944 1 17529 834 17529 0 624 1 866 8 #editCopy 8 'Copy' 1 1 0 944 3 17531 834 17531 0 624 1 866 8 #editPaste 8 'Paste' 1 1 0 944 5 17533 834 17533 0 624 1 866 8 #editDelete 8 'Delete' 1 1 0 944 11 98 6 1072 1136 1200 1264 848 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 624 3 0 1 234 240 98 2 944 1 0 1 0 530 33 33 530 45 45 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 151 1 530 241 51 624 1506 8 #updateSize 784 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 0 0 0 0 195 0 0 0 25 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedPreviousRight 1 1746 8 #fixedViewLeft 241 1746 8 #fixedParentTop 1 1746 8 #fixedViewTop 51 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 1888 0 0 0 7 0 0 0 1888 0 8 4294903135 866 8 #login 8 'Login' 1 1 0 0 16 0 0 0 1442 202 208 98 3 1506 1536 98 2 530 1 445 530 789 61 1888 1506 8 #isEnabled: 98 1 32 1888 1506 8 #text: 98 1 8 'Login' 1888 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 222 0 0 0 138 1 0 0 252 0 0 0] 98 0 1696 0 29 1714 1746 8 #fixedParentLeft 1 1746 8 #fixedParentRight 1 1746 8 #fixedParentBottom -59 1856 61 410 640 98 25 0 416 98 2 8 1409289036 131137 2416 0 482 8 4278190080 0 7 0 0 0 2416 482 2496 8 4294902555 234 256 784 234 256 98 6 17521 1115910 ##(Smalltalk.ToolbarIconButton)  17521 0 2416 1 866 8 #fileNew 8 'New' 1 1 263494 3 ##(Smalltalk.Icon)  0 16 976 8 'DocumentShell.ico' 0 930 0 16 0 0 0 0 3 530 33 33 1 17523 2594 17523 0 2416 1 866 8 #fileOpen 8 'Open' 1 1 2674 0 16 976 8 'FileOpen.ico' 1024 930 0 16 0 0 0 0 3 530 33 33 1 17525 2594 17525 0 2416 1 866 8 #fileSave 8 'Save' 1 1 2674 0 16 976 8 'FileSave.ico' 1024 930 0 16 0 0 0 0 3 530 33 33 1 98 4 2608 2752 2880 1346 0 0 2416 3 0 1 234 240 98 6 2976 5 2720 1 2848 3 0 1 0 530 33 33 530 45 45 0 0 1442 202 208 98 2 1506 1536 98 2 530 1 1 530 151 51 2416 1506 1616 784 2416 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 75 0 0 0 25 0 0 0] 98 0 1696 0 27 1714 2320 1 1792 151 1824 1 1856 51 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1409286144 131073 3296 0 482 8 4278190080 0 7 0 0 0 3296 655878 ##(Smalltalk.CardLayout)  202 208 98 2 721414 ##(Smalltalk.Association)  8 'Basic' 410 8 ##(Smalltalk.ContainerView)  98 15 0 3296 98 2 8 1140850688 131073 3520 0 0 0 7 0 0 0 3520 562 234 240 98 28 410 8 ##(Smalltalk.TextEdit)  98 16 0 3520 98 2 8 1140916352 1025 3648 0 482 8 4278190080 0 7 0 0 0 3648 0 8 4294902953 852486 ##(Smalltalk.NullConverter)  0 0 1 1442 202 208 98 4 1506 1536 98 2 530 391 287 530 373 41 3648 1506 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 3648 1506 8 #isTextModified: 98 1 32 3648 1506 8 #setMarginWidths: 98 1 98 2 7 7 3648 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 143 0 0 0 125 1 0 0 163 0 0 0] 98 0 1696 0 27 1714 1760 11 2352 -9 1746 8 #fixedPreviousTop 1 1856 41 410 8 ##(Smalltalk.StaticText)  98 16 0 3520 98 2 8 1140850944 1 4208 0 0 0 7 0 0 0 4208 0 8 4294903089 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 11 237 530 371 41 4208 1506 2208 98 1 8 'GemStone Password:' 4208 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 118 0 0 0 190 0 0 0 138 0 0 0] 98 0 1696 0 27 1714 2320 11 1792 371 1746 8 #fixedPreviousBottom 1 1856 41 410 3664 98 16 0 3520 98 2 8 1140916352 1025 4576 0 482 8 4278190080 0 7 0 0 0 4576 0 8 4294902953 3778 0 0 1 1442 202 208 98 4 1506 1536 98 2 530 391 157 530 373 41 4576 1506 3936 98 1 3970 3 1 3 4576 1506 4016 98 1 32 4576 1506 4064 98 1 98 2 7 7 4576 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 78 0 0 0 125 1 0 0 98 0 0 0] 98 0 1696 0 27 1714 1760 11 2352 -9 4176 1 1856 41 410 3664 98 16 0 3520 98 2 8 1140916352 1025 5008 0 482 4656 0 7 0 0 0 5008 0 8 4294902953 3778 0 0 1 1442 202 208 98 4 1506 1536 98 2 530 391 197 530 373 41 5008 1506 3936 98 1 3970 3 1 3 5008 1506 4016 98 1 32 5008 1506 4064 98 1 98 2 7 7 5008 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 98 0 0 0 125 1 0 0 118 0 0 0] 98 0 1696 0 27 1714 1760 11 2352 -9 4176 1 1856 41 410 8 ##(Smalltalk.ComboBox)  98 17 0 3520 98 2 8 1144063491 1025 5424 590662 2 ##(Smalltalk.ListModel)  202 208 784 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 5424 0 8 4294902823 459270 ##(Smalltalk.Message)  8 #displayString 98 0 784 401 1442 202 208 98 1 1506 1536 98 2 530 391 11 530 373 49 5424 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 5 0 0 0 125 1 0 0 29 0 0 0] 98 0 1696 0 27 1714 1760 11 2352 -9 1824 11 1856 47 410 4224 98 16 0 3520 98 2 8 1140850944 1 5872 0 0 0 7 0 0 0 5872 0 8 4294903089 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 11 67 530 371 31 5872 1506 2208 98 1 8 'RPC Gem Host Name or IP:' 5872 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 190 0 0 0 48 0 0 0] 98 0 1696 0 27 1714 2320 11 1792 371 4544 11 1856 31 410 4224 98 16 0 3520 98 2 8 1140850944 1 6192 0 0 0 7 0 0 0 6192 0 8 4294903089 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 11 21 530 371 41 6192 1506 2208 98 1 8 'GemStone/Smalltalk Version:' 6192 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 190 0 0 0 30 0 0 0] 98 0 1696 0 27 1714 2320 11 1792 371 1824 21 1856 41 410 4224 98 16 0 3520 98 2 8 1140850944 1 6512 0 0 0 7 0 0 0 6512 0 8 4294903089 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 11 107 530 371 31 6512 1506 2208 98 1 8 'NetLDI Service Name or Port:' 6512 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 53 0 0 0 190 0 0 0 68 0 0 0] 98 0 1696 0 27 1714 2320 11 1792 371 4544 1 1856 31 410 3664 98 16 0 3520 98 2 8 1140916384 1025 6832 0 482 8 4278190080 0 7 0 0 0 6832 0 8 4294902953 3778 0 0 1 1442 202 208 98 4 1506 1536 98 2 530 391 237 530 373 41 6832 1506 3936 98 1 3970 3 1 3 6832 1506 4016 98 1 32 6832 1506 4064 98 1 98 2 7 7 6832 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 118 0 0 0 125 1 0 0 138 0 0 0] 98 0 1696 0 27 1714 1760 11 2352 -9 4176 1 1856 41 410 3664 98 16 0 3520 98 2 8 1140916352 1025 7264 0 482 4656 0 7 0 0 0 7264 0 8 4294902953 3778 0 0 1 1442 202 208 98 4 1506 1536 98 2 530 391 67 530 373 41 7264 1506 3936 98 1 3970 3 1 3 7264 1506 4016 98 1 32 7264 1506 4064 98 1 98 2 7 7 7264 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 33 0 0 0 125 1 0 0 53 0 0 0] 98 0 1696 0 27 1714 1760 11 2352 -9 4176 1 1856 41 410 4224 98 16 0 3520 98 2 8 1140850944 1 7680 0 0 0 7 0 0 0 7680 0 8 4294903089 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 11 157 530 371 39 7680 1506 2208 98 1 8 'Stone Name:' 7680 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 78 0 0 0 190 0 0 0 97 0 0 0] 98 0 1696 0 27 1714 2320 11 1792 371 4544 11 1856 39 410 3664 98 16 0 3520 98 2 8 1140916352 1025 8000 0 482 4656 0 7 0 0 0 8000 0 8 4294902953 3778 0 0 1 1442 202 208 98 4 1506 1536 98 2 530 391 107 530 373 41 8000 1506 3936 98 1 3970 3 1 3 8000 1506 4016 98 1 32 8000 1506 4064 98 1 98 2 7 7 8000 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 53 0 0 0 125 1 0 0 73 0 0 0] 98 0 1696 0 27 1714 1760 11 2352 -9 4176 1 1856 41 410 4224 98 16 0 3520 98 2 8 1140850944 1 8416 0 0 0 7 0 0 0 8416 0 8 4294903089 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 11 197 530 371 41 8416 1506 2208 98 1 8 'GemStone User ID:' 8416 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 98 0 0 0 190 0 0 0 118 0 0 0] 98 0 1696 0 27 1714 2320 11 1792 371 4544 1 1856 41 410 4224 98 16 0 3520 98 2 8 1140850944 1 8736 0 0 0 7 0 0 0 8736 0 8 4294903089 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 11 287 530 371 41 8736 1506 2208 98 1 8 'Monticello User Name:' 8736 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 143 0 0 0 190 0 0 0 163 0 0 0] 98 0 1696 0 27 1714 2320 11 1792 371 4544 11 1856 41 234 256 98 14 8000 8 'gemService' 4576 8 'stoneName' 6832 8 'password' 5008 8 'userID' 7264 8 'gemHost' 3648 8 'initials' 5424 8 'versionList' 0 1442 202 208 98 1 1506 1536 98 2 530 9 51 530 773 337 3520 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 25 0 0 0 134 1 0 0 193 0 0 0] 98 14 6192 5424 5872 7264 6512 8000 7680 4576 8416 5008 4208 6832 8736 3648 1696 0 27 3474 8 'Advanced' 410 3536 98 15 0 3296 98 2 8 1140850688 131073 9392 0 0 0 5 0 0 0 9392 562 234 240 98 26 410 1904 98 20 0 9392 98 2 8 1140924416 1 9504 0 0 0 5 0 0 0 9504 0 8 4294903135 866 8 #debugGCI 8 'Debug GCI' 1 1 0 0 32 0 0 0 1442 202 208 98 3 1506 1536 98 2 530 11 45 530 141 51 9504 1506 2160 98 1 32 9504 1506 2208 98 1 8 'Debug GCI' 9504 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 22 0 0 0 75 0 0 0 47 0 0 0] 98 0 1696 0 29 1714 2320 11 1792 141 4544 1 1856 51 410 8 ##(Smalltalk.StaticPath)  98 17 0 9392 98 2 8 1140850944 1 9888 0 0 0 5 0 0 0 9888 0 8 4294903089 3778 0 0 0 8 'XXX' 1442 202 208 98 2 1506 1536 98 2 530 161 55 530 603 41 9888 1506 2208 98 1 10000 9888 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 27 0 0 0 125 1 0 0 47 0 0 0] 98 0 1696 0 27 1714 2320 161 2352 -9 4176 11 1856 41 410 3536 98 15 0 9392 98 2 8 1409286144 393217 10224 0 482 8 4278190080 0 5 0 0 0 10224 656390 ##(Smalltalk.GridLayout)  1 3 1 1 234 256 98 4 410 8 ##(Smalltalk.RadioButton)  98 16 0 10224 98 2 8 1140924425 1 10384 721990 2 ##(Smalltalk.ValueHolder)  0 0 1376774 ##(Smalltalk.PluggableSearchPolicy)  5634 8 #= 98 0 5634 8 #hash 98 0 32 482 10304 0 5 0 0 0 10384 0 8 4294903135 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 1 39 530 297 39 10384 1506 2208 98 1 8 'Native OS User' 10384 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 148 0 0 0 38 0 0 0] 98 0 1696 0 27 8 'member' 410 10400 98 16 0 10224 98 2 8 1141055497 1 10896 10466 0 0 10498 5634 10544 98 0 5634 10592 98 0 32 482 10304 0 5 0 0 0 10896 0 8 4294903135 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 1 1 530 297 39 10896 1506 2208 98 1 8 'NetLDI Guest' 10896 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 148 0 0 0 19 0 0 0] 98 0 1696 0 27 8 'guest' 0 1442 202 208 98 1 1506 1536 98 2 530 11 185 530 301 81 10224 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 92 0 0 0 155 0 0 0 132 0 0 0] 98 2 10896 10384 1696 0 27 1714 2320 11 1792 301 4544 1 1856 81 410 3664 98 16 0 9392 98 2 8 1140916352 1025 11504 0 482 8 4278190080 0 5 0 0 0 11504 0 8 4294902953 3778 0 0 1 1442 202 208 98 4 1506 1536 98 2 530 491 185 530 273 41 11504 1506 3936 98 1 3970 3 1 3 11504 1506 4016 98 1 32 11504 1506 4064 98 1 98 2 7 7 11504 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 92 0 0 0 125 1 0 0 112 0 0 0] 98 0 1696 0 27 1714 1760 11 2352 -9 4176 1 1856 41 410 4224 98 16 0 9392 98 2 8 1140850944 1 11936 0 0 0 5 0 0 0 11936 0 8 4294903089 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 321 225 530 161 41 11936 1506 2208 98 1 8 'Password:' 11936 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 112 0 0 0 240 0 0 0 132 0 0 0] 98 0 1696 0 27 1714 1746 8 #fixedPreviousLeft -169 1792 161 4544 1 1856 41 410 3664 98 16 0 9392 98 2 8 1140916384 1025 12288 0 482 8 4278190080 0 5 0 0 0 12288 0 8 4294902953 3778 0 0 1 1442 202 208 98 4 1506 1536 98 2 530 491 225 530 273 41 12288 1506 3936 98 1 3970 3 1 3 12288 1506 4016 98 1 32 12288 1506 4064 98 1 98 2 7 7 12288 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 112 0 0 0 125 1 0 0 132 0 0 0] 98 0 1696 0 27 1714 1760 11 2352 -9 4176 1 1856 41 410 8 ##(Smalltalk.CheckBox)  98 16 0 9392 98 2 8 1409363203 1 12720 10466 0 0 1114118 ##(Smalltalk.NeverSearchPolicy)  32 0 0 5 0 0 0 12720 0 8 4294903135 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 11 5 530 753 41 12720 1506 2208 98 1 8 'Open new socket  to improve performance (experimental)' 12720 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 125 1 0 0 22 0 0 0] 98 0 1696 0 27 1714 2320 11 2352 -9 1824 5 1856 41 410 4224 98 16 0 9392 98 2 8 1140850944 1 13104 0 0 0 5 0 0 0 13104 0 8 4294903089 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 11 145 530 301 31 13104 1506 2208 98 1 8 'NetLDI Task Name:' 13104 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 72 0 0 0 155 0 0 0 87 0 0 0] 98 0 1696 0 27 1714 2320 11 1792 301 4544 1 1856 31 410 3664 98 16 0 9392 98 2 8 1140916352 1025 13424 0 482 4656 0 5 0 0 0 13424 0 8 4294902953 3778 0 0 1 1442 202 208 98 5 1506 1536 98 2 530 321 145 530 443 41 13424 1506 2208 98 1 8 'gemnetobject' 13424 1506 3936 98 1 3970 3 1 3 13424 1506 4016 98 1 32 13424 1506 4064 98 1 98 2 7 7 13424 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 72 0 0 0 125 1 0 0 92 0 0 0] 98 0 1696 0 27 1714 1760 11 2352 -9 4176 1 1856 41 410 4224 98 16 0 9392 98 2 8 1140850944 1 13888 0 0 0 5 0 0 0 13888 0 8 4294903089 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 11 105 530 301 41 13888 1506 2208 98 1 8 'Stone Host (from Gem):' 13888 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 52 0 0 0 155 0 0 0 72 0 0 0] 98 0 1696 0 27 1714 2320 11 1792 301 4544 11 1856 41 410 3664 98 16 0 9392 98 2 8 1140916352 1025 14208 0 482 4656 0 5 0 0 0 14208 0 8 4294902953 3778 0 0 1 1442 202 208 98 5 1506 1536 98 2 530 321 105 530 443 41 14208 1506 2208 98 1 8 'localhost' 14208 1506 3936 98 1 3970 3 1 3 14208 1506 4016 98 1 32 14208 1506 4064 98 1 98 2 7 7 14208 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 52 0 0 0 125 1 0 0 72 0 0 0] 98 0 1696 0 27 1714 1760 11 2352 -9 4176 1 1856 41 410 3536 98 15 0 9392 98 2 8 1140850688 393217 14672 0 482 8 4278190080 0 5 0 0 0 14672 10322 3 5 1 1 234 256 98 4 410 10400 98 16 0 14672 98 2 8 1140924425 1 14816 10466 0 0 10498 5634 10544 98 0 5634 10592 98 0 32 482 14752 0 5 0 0 0 14816 0 8 4294903135 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 1 1 530 375 57 14816 1506 2208 98 1 8 'RPC Gem (required for 64-bit)' 14816 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 187 0 0 0 28 0 0 0] 98 0 1696 0 27 8 'remote' 410 10400 98 16 0 14672 98 2 8 1141055497 1 15248 10466 0 0 10498 5634 10544 98 0 5634 10592 98 0 32 482 14752 0 5 0 0 0 15248 0 8 4294903135 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 375 1 530 375 57 15248 1506 2208 98 1 8 'Linked Gem (32-bit only)' 15248 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 187 0 0 0 0 0 0 0 118 1 0 0 28 0 0 0] 98 0 1696 0 27 8 'linked' 0 1442 202 208 98 1 1506 1536 98 2 530 11 275 530 753 61 14672 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 137 0 0 0 125 1 0 0 167 0 0 0] 98 2 14816 15248 1696 0 27 1714 2320 11 2352 -9 4544 11 1856 61 410 4224 98 16 0 9392 98 2 8 1140850944 1 15856 0 0 0 5 0 0 0 15856 0 8 4294903089 3778 0 0 0 1442 202 208 98 2 1506 1536 98 2 530 321 185 530 161 41 15856 1506 2208 98 1 8 'OS User ID:' 15856 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 92 0 0 0 240 0 0 0 112 0 0 0] 98 0 1696 0 27 1714 1760 11 1792 161 4176 1 1856 41 234 256 98 16 13424 8 'gemTask' 14208 8 'stoneHost' 14672 8 'gemType' 12288 8 'hostPassword' 12720 8 'useSocket' 11504 8 'hostUserID' 9888 8 'debugPath' 10224 8 'loginType' 0 1442 202 208 98 1 1506 1536 98 2 530 9 51 530 773 337 9392 1634 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 25 0 0 0 134 1 0 0 193 0 0 0] 98 13 12720 9504 9888 13888 14208 13104 13424 10224 15856 11504 11936 12288 14672 1696 0 27 3520 234 256 98 4 3520 8 'Basic' 9392 8 'Advanced' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 3296 98 2 8 1140916736 1 16560 5506 202 208 98 2 3504 9376 0 5568 0 0 1 0 0 0 16560 0 8 4294903067 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 5664 16720 7 257 0 16706 0 0 16738 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 16816 7 257 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 8 #noIcons 0 0 0 0 0 1442 202 208 98 3 1506 1536 98 2 530 1 1 530 789 395 16560 1506 8 #basicSelectionsByIndex: 98 1 98 1 3 16560 1506 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 16560 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 138 1 0 0 197 0 0 0] 98 0 1696 0 27 1442 202 208 98 1 1506 1536 98 2 530 1 51 530 789 395 3296 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 138 1 0 0 222 0 0 0] 98 3 3520 9392 16560 1696 0 27 1714 2320 1 2352 1 1824 51 2384 -59 410 640 98 25 0 416 98 2 8 1140853580 131137 17408 0 482 8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 17408 482 17488 8 4294902555 234 256 784 234 256 98 4 17539 834 17539 0 17408 1 866 8 #openHelp 8 'Jade Help' 1 1 0 930 0 16 976 8 'Tools.bmp' 1024 0 7 530 1857 33 71 17537 1246982 ##(Smalltalk.ToolbarSystemButton)  17537 0 17408 1 866 8 #getHelpOn 8 'Get help on widget' 1 1 0 1 23 98 2 17808 17680 234 240 98 4 1 1 17744 31 0 1 0 530 33 33 530 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 1442 202 208 98 2 1506 1536 98 2 530 391 1 530 399 51 17408 1506 1616 784 17408 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 0 0 0 0 138 1 0 0 25 0 0 0] 98 0 1696 0 27 1714 1760 1 2352 1 1824 1 1856 51 234 256 98 8 624 8 'Edit Tools' 2416 8 'File Tools' 17408 8 'Other Tools' 3296 8 'Tabs' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 866 2640 8 '&New' 9373 1 0 0 0 18370 1 866 2784 8 '&Open...' 9375 1 0 0 0 18370 1 866 2912 8 '&Save' 9383 1 0 0 0 18370 1 866 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 18370 1 866 8 #fileRevert 8 '&Revert' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 18370 1 866 8 #exit 8 'E&xit' 17639 1 0 0 0 8 '&File' 0 134217729 0 0 17485 0 0 18322 0 16 98 9 18370 1 866 896 8 '&Undo' 9397 1 0 0 0 18370 1 866 8 #editRedo 8 '&Redo' 9395 1 0 0 0 18658 4097 18370 1 866 1104 8 'Cu&t' 9393 1 0 0 0 18370 1 866 1168 8 '&Copy' 9351 1 0 0 0 18370 1 866 1232 8 '&Paste' 9389 1 0 0 0 18370 1 866 1296 8 'De&lete' 1629 1 0 0 0 18658 4097 18370 1 866 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 8 '&Edit' 0 134217729 0 0 17501 0 0 18322 0 16 98 2 18370 1 866 8 #setDefaultFont 8 'Set Default &Font' 1 1 0 0 0 18370 1 866 8 #setCodeFont 8 'Set &Code Font' 1 1 0 0 0 8 '&Preferences' 0 134217729 0 0 17507 0 0 18322 0 16 98 7 18370 1 866 8 #helpContents 8 '&Contents' 1 1 0 0 0 18370 1 866 8 #helpOnThisTool 8 '&On this Tool' 1249 1 0 0 0 18370 1 866 8 #helpWhatsThis 8 '&What''s This?' 5345 1 0 0 0 18658 4097 18370 1 866 8 #openHomePage 8 '&GemStone Home Page' 1025 1 0 0 0 18658 4097 18370 1 866 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 134217729 0 0 17519 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 8671 2674 0 16 976 8 'icons\GS32x32.ico' 0 2674 0 16 976 8 'icons\GS16x16.ico' 0 0 0 1 0 0 1442 202 208 98 3 1506 1536 98 2 530 2879 21 530 801 601 416 1506 2208 98 1 8 'Jade Login' 416 1506 8 #updateMenuBar 784 416 1634 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 47 7 0 0 54 1 0 0] 98 5 2416 624 17408 1888 3296 1696 0 27 )!

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

