"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #JadeLoginShell
	instanceVariableNames: 'loginList debugPathPresenter gemHostPresenter gemServicePresenter gemTaskPresenter gemTypePresenter hostPasswordPresenter hostUserIDPresenter initialsPresenter isModified loginListPresenter loginTypePresenter passwordPresenter stoneHostPresenter stoneNamePresenter userIDPresenter versionListPresenter'
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

	self 
		model: self class defaultModel;
		updateView;
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

	| openFilename |
	openFilename := FileOpenDialog new
		fileTypes: self class fileTypes;
		defaultExtension: self class defaultFileExtension;
		value: SessionManager current imageBase , 'connections\*.GSS';
		showModal.
	openFilename isNil ifTrue: [^self].
	self fileLoad.
!

fillVersionList
	| list |
	list := GciLibrary allSubclasses collect: [:each | each displayName].
	list := list reject: [:each | each isNil].
	list := list asSortedCollection: [:a :b | self isVersion: a lessThanOrEqualTo: b].
	versionListPresenter list: list!

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

	versionListPresenter view disable.
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
	^super queryCommand: query.
!

removeLogin!

sendRequiredMessages
	"The ImageStripper seems to think these messages aren't needed, but we die during runtime packaging with a MessageNotUnderstood."

	'string1' equals: 'string2'.
	1 isImmediate.
!

setDocumentData: aString

	self model updateFrom: (JadeLogin fromXML: aString).
	self updateView.
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

	| selection |
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
! !
!JadeLoginShell categoriesFor: #aboutJade!public! !
!JadeLoginShell categoriesFor: #addLogin!commands!public! !
!JadeLoginShell categoriesFor: #checkScintillaVersion!public! !
!JadeLoginShell categoriesFor: #createComponents!public! !
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
!JadeLoginShell categoriesFor: #isVersion:lessThanOrEqualTo:!public! !
!JadeLoginShell categoriesFor: #login!public! !
!JadeLoginShell categoriesFor: #onViewClosed!public! !
!JadeLoginShell categoriesFor: #onViewOpened!public! !
!JadeLoginShell categoriesFor: #openGemTalkHomePage!public! !
!JadeLoginShell categoriesFor: #openJadeHomePage!public! !
!JadeLoginShell categoriesFor: #postLogin:!public! !
!JadeLoginShell categoriesFor: #queryCommand:!public! !
!JadeLoginShell categoriesFor: #removeLogin!commands!public! !
!JadeLoginShell categoriesFor: #sendRequiredMessages!public! !
!JadeLoginShell categoriesFor: #setDocumentData:!accessing!public! !
!JadeLoginShell categoriesFor: #setWorkingDirectory!public! !
!JadeLoginShell categoriesFor: #showGemNRS!commands!public! !
!JadeLoginShell categoriesFor: #showStoneNRS!commands!public! !
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

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13238272 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1401 841 551 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 608 nil nil nil 7 nil nil nil 608 852230 ##(Smalltalk.FramingLayout) 170 176 34 10 410 ##(Smalltalk.Toolbar) 34 25 nil 608 34 2 8 1409289036 131137 736 nil 466 8 4278190080 nil 7 nil nil nil 736 466 816 8 1795534112 170 192 8 #() 170 192 34 10 58597 853766 ##(Smalltalk.ToolbarButton) 58597 nil 736 1 1180998 4 ##(Smalltalk.CommandDescription) #editCut 8 'Cut' 1 1 nil 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'EditBar.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 514 193 33 1 58599 930 58599 nil 736 1 962 #editCopy 8 'Copy' 1 1 nil 1024 3 58601 930 58601 nil 736 1 962 #editPaste 8 'Paste' 1 1 nil 1024 5 58603 930 58603 nil 736 1 962 #editDelete 8 'Delete' 1 1 nil 1024 11 58605 930 58605 nil 736 1 962 #editUndo 8 'Undo' 1 1 nil 1024 7 34 6 944 1152 1200 1248 1296 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 736 3 nil 1 170 176 34 2 1024 1 nil 1 nil 514 33 33 514 45 45 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 241 51 736 1522 #updateSizePosted 880 736 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 120 0 0 0 25 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 1730 #fixedViewLeft 241 1730 #fixedParentTop 1 1730 #fixedViewTop 51 410 ##(Smalltalk.PushButton) 34 20 nil 608 34 2 8 1140924416 1 1808 nil nil nil 7 nil nil nil 1808 nil 8 1795593280 962 #addLogin 8 '+' 1 1 nil nil false nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 1319 61 514 51 51 1808 1522 #text: 34 1 8 '+' 1808 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 30 0 0 0 172 2 0 0 55 0 0 0] 8 #() 1680 nil 29 1698 1730 #fixedParentRight -49 1760 51 1776 61 1792 51 410 ##(Smalltalk.PushButton) 34 20 nil 608 34 2 8 1140924416 1 2160 nil nil nil 7 nil nil nil 2160 nil 8 1795593280 962 #removeLogin 8 '-' 1 1 nil nil false nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 1319 121 514 51 51 2160 1522 #text: 34 1 8 '-' 2160 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 147 2 0 0 60 0 0 0 172 2 0 0 85 0 0 0] 8 #() 1680 nil 29 1698 2144 -49 1760 51 1730 #fixedPreviousBottom 11 1792 51 410 ##(Smalltalk.ListView) 34 30 nil 608 34 2 8 1409355853 1025 2512 590662 2 ##(Smalltalk.ListModel) 138 144 880 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 nil 7 nil nil nil 2512 nil 8 1795483760 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 6 920646 5 ##(Smalltalk.ListViewColumn) 8 'Version' 201 #left 2706 #gemVersion 8 #() ##(Smalltalk.SortedCollection) nil nil 2512 nil 1 nil nil 2818 8 'Gem Host' 201 #left 2706 #gemHost 2880 2706 #<= 2880 nil nil 2512 nil 1 nil nil 2818 8 'NetLDI' 201 #left 2706 #gemService 2880 2706 #<= 2880 nil nil 2512 nil 1 nil nil 2818 8 'Stone' 201 #left 2706 #stoneName 2880 2706 #<= 2880 nil nil 2512 nil 1 nil nil 2818 8 'User' 201 #left 2706 #gsUserID 2880 2706 #<= 2880 nil nil 2512 nil 1 nil nil 2818 8 'Last Login' 303 #left 2706 #lastLoginString 880 2706 #<= 880 nil nil 2512 nil 3 nil nil #report 880 nil 131169 nil 34 4 nil nil 514 1 1 nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 1 51 514 1319 297 2512 1522 #text: 34 1 8 'Version' 2512 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 147 2 0 0 173 0 0 0] 8 #() 1680 nil 27 1698 1744 1 2144 -49 2496 1 1730 #fixedParentBottom 1 410 ##(Smalltalk.Toolbar) 34 25 nil 608 34 2 8 1140853580 131137 3488 nil 466 8 4278190080 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 3488 466 3568 8 1795534112 170 192 880 170 192 34 4 58607 1246982 ##(Smalltalk.ToolbarSystemButton) 58607 nil 3488 1 962 #getHelpOn 8 'Get help on widget' 1 1 nil 1 23 58609 930 58609 nil 3488 1 962 #openHelp 8 'Jade Help' 1 1 nil 1010 nil true 1056 8 'Tools.bmp' 1104 nil 7 514 1857 33 71 34 2 3776 3824 170 176 34 4 1 1 3872 31 nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 241 1 514 1129 51 3488 1522 #updateSizePosted 880 3488 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 120 0 0 0 0 0 0 0 172 2 0 0 25 0 0 0] 8 #() 1680 nil 27 1698 1730 #fixedPreviousRight 1 2144 1 1776 1 1792 51 170 192 34 6 2512 8 'loginList' 736 8 'Edit Tools' 3488 8 'Other Tools' nil 1458 138 144 34 1 1522 #createAt:extent: 34 2 514 1 1 514 1369 347 608 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 173 0 0 0] 34 5 736 3488 2512 1808 2160 1680 nil 27 39 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 4480 nil nil nil 7 nil nil nil 4480 674 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 4480 34 2 8 1140850688 131073 4592 nil nil nil 7 nil nil nil 4592 674 170 176 34 28 410 ##(Smalltalk.StaticText) 34 16 nil 4592 34 2 8 1140850944 1 4704 nil nil nil 7 nil nil nil 4704 nil 8 1795590816 852486 ##(Smalltalk.NullConverter) nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 11 213 514 371 41 4704 1522 #text: 34 1 8 'GemStone User ID:' 4704 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 106 0 0 0 190 0 0 0 126 0 0 0] 8 #() 1680 nil 27 1698 1744 11 1760 371 2496 3 1792 41 410 ##(Smalltalk.ComboBox) 34 17 nil 4592 34 2 8 1144063491 1025 5040 2578 138 144 880 nil 2640 466 8 4278190080 nil 7 nil nil nil 5040 nil 8 1796107360 2706 #displayString 8 #() 880 401 1458 138 144 34 1 1522 #createAt:extent: 34 2 514 391 11 514 285 53 5040 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 5 0 0 0 81 1 0 0 31 0 0 0] 8 #() 1680 nil 27 1698 4224 11 2144 -9 1776 11 1792 47 410 ##(Smalltalk.StaticText) 34 16 nil 4592 34 2 8 1140850944 1 5392 nil nil nil 7 nil nil nil 5392 nil 8 1795590816 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 11 167 514 371 39 5392 1522 #text: 34 1 8 'Stone Name:' 5392 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 83 0 0 0 190 0 0 0 102 0 0 0] 8 #() 1680 nil 27 1698 1744 11 1760 371 2496 11 1792 39 410 ##(Smalltalk.TextEdit) 34 16 nil 4592 34 2 8 1140916352 1025 5712 nil 466 8 4278190080 nil 7 nil nil nil 5712 nil 8 1795431936 4786 nil nil 1 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 391 167 514 285 45 5712 1522 #setMarginWidths: 34 1 8 #(3 3) 5712 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 83 0 0 0 81 1 0 0 105 0 0 0] 8 #() 1680 nil 27 1698 4224 11 2144 -9 1730 #fixedPreviousTop 1 1792 45 410 ##(Smalltalk.StaticText) 34 16 nil 4592 34 2 8 1140850944 1 6080 nil nil nil 7 nil nil nil 6080 nil 8 1795590816 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 11 67 514 371 31 6080 1522 #text: 34 1 8 'RPC Gem Host Name or IP:' 6080 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 190 0 0 0 48 0 0 0] 8 #() 1680 nil 27 1698 1744 11 1760 371 2496 11 1792 31 410 ##(Smalltalk.StaticText) 34 16 nil 4592 34 2 8 1140850944 1 6400 nil nil nil 7 nil nil nil 6400 nil 8 1795590816 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 11 313 514 371 41 6400 1522 #text: 34 1 8 'Monticello User Name:' 6400 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 156 0 0 0 190 0 0 0 176 0 0 0] 8 #() 1680 nil 27 1698 1744 11 1760 371 2496 11 1792 41 410 ##(Smalltalk.TextEdit) 34 16 nil 4592 34 2 8 1140916352 1025 6720 nil 466 8 4278190080 nil 7 nil nil nil 6720 nil 8 1795431936 4786 nil nil 1 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 391 313 514 285 45 6720 1522 #setMarginWidths: 34 1 8 #(3 3) 6720 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 156 0 0 0 81 1 0 0 178 0 0 0] 8 #() 1680 nil 27 1698 4224 11 2144 -9 6064 1 1792 45 410 ##(Smalltalk.StaticText) 34 16 nil 4592 34 2 8 1140850944 1 7072 nil nil nil 7 nil nil nil 7072 nil 8 1795590816 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 11 21 514 371 41 7072 1522 #text: 34 1 8 'GemStone/Smalltalk Version:' 7072 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 190 0 0 0 30 0 0 0] 8 #() 1680 nil 27 1698 1744 11 1760 371 1776 21 1792 41 410 ##(Smalltalk.StaticText) 34 16 nil 4592 34 2 8 1140850944 1 7392 nil nil nil 7 nil nil nil 7392 nil 8 1795590816 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 11 113 514 371 31 7392 1522 #text: 34 1 8 'NetLDI Service Name or Port:' 7392 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 56 0 0 0 190 0 0 0 71 0 0 0] 8 #() 1680 nil 27 1698 1744 11 1760 371 2496 3 1792 31 410 ##(Smalltalk.TextEdit) 34 16 nil 4592 34 2 8 1140916352 1025 7712 nil 466 5792 nil 7 nil nil nil 7712 nil 8 1795431936 4786 nil nil 1 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 391 67 514 285 45 7712 1522 #setMarginWidths: 34 1 8 #(3 3) 7712 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 33 0 0 0 81 1 0 0 55 0 0 0] 8 #() 1680 nil 27 1698 4224 11 2144 -9 6064 1 1792 45 410 ##(Smalltalk.TextEdit) 34 16 nil 4592 34 2 8 1140916352 1025 8048 nil 466 5792 nil 7 nil nil nil 8048 nil 8 1795431936 4786 nil nil 1 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 391 113 514 285 45 8048 1522 #setMarginWidths: 34 1 8 #(3 3) 8048 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 56 0 0 0 81 1 0 0 78 0 0 0] 8 #() 1680 nil 27 1698 4224 11 2144 -9 6064 1 1792 45 410 ##(Smalltalk.TextEdit) 34 16 nil 4592 34 2 8 1140916352 1025 8384 nil 466 5792 nil 7 nil nil nil 8384 nil 8 1795431936 4786 nil nil 1 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 391 213 514 285 45 8384 1522 #setMarginWidths: 34 1 8 #(3 3) 8384 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 106 0 0 0 81 1 0 0 128 0 0 0] 8 #() 1680 nil 27 1698 4224 11 2144 -9 6064 1 1792 45 410 ##(Smalltalk.TextEdit) 34 16 nil 4592 34 2 8 1140916384 1025 8720 nil 466 8 4278190080 nil 7 nil nil nil 8720 nil 8 1795431936 4786 nil nil 1 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 391 259 514 285 45 8720 1522 #setMarginWidths: 34 1 8 #(3 3) 8720 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 129 0 0 0 81 1 0 0 151 0 0 0] 8 #() 1680 nil 27 1698 4224 11 2144 -9 6064 1 1792 45 410 ##(Smalltalk.StaticText) 34 16 nil 4592 34 2 8 1140850944 1 9072 nil nil nil 7 nil nil nil 9072 nil 8 1795590816 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 11 259 514 371 41 9072 1522 #text: 34 1 8 'GemStone Password:' 9072 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 129 0 0 0 190 0 0 0 149 0 0 0] 8 #() 1680 nil 27 1698 1744 11 1760 371 2496 3 1792 41 170 192 34 14 5712 8 'stoneName' 8720 8 'password' 8384 8 'userID' 5040 8 'versionList' 6720 8 'initials' 7712 8 'gemHost' 8048 8 'gemService' nil 1458 138 144 34 1 1522 #createAt:extent: 34 2 514 1 1 514 685 367 4592 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 1 0 0 183 0 0 0] 34 14 7072 5040 6080 7712 7392 8048 5392 5712 4704 8384 9072 8720 6400 6720 1680 nil 27 1698 1744 1 1730 #relativeParentWidth 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 1776 1 3472 1 410 ##(Smalltalk.ContainerView) 34 15 nil 4480 34 2 8 1140850688 131073 9760 nil nil nil 7 nil nil nil 9760 674 170 176 34 26 410 ##(Smalltalk.TextEdit) 34 16 nil 9760 34 2 8 1140916352 1025 9872 nil 466 5792 nil 7 nil nil nil 9872 nil 8 1795431936 4786 nil nil 1 1458 138 144 34 3 1522 #createAt:extent: 34 2 514 321 115 514 355 45 9872 1522 #text: 34 1 8 'gemnetobject' 9872 1522 #setMarginWidths: 34 1 8 #(3 3) 9872 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 57 0 0 0 81 1 0 0 79 0 0 0] 8 #() 1680 nil 27 1698 4224 11 2144 -9 6064 1 1792 45 410 ##(Smalltalk.ContainerView) 34 15 nil 9760 34 2 8 1409286144 393217 10256 nil 466 8 4278190080 nil 7 nil nil nil 10256 656390 ##(Smalltalk.GridLayout) 1 3 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 10256 34 2 8 1140924425 1 10416 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 2706 #= 8 #() 2706 #hash 8 #() false 466 10336 nil 7 nil nil nil 10416 nil 8 1795593280 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 1 39 514 297 39 10416 1522 #text: 34 1 8 'Native OS User' 10416 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 148 0 0 0 38 0 0 0] 8 #() 1680 nil 27 8 'member' 410 ##(Smalltalk.RadioButton) 34 16 nil 10256 34 2 8 1141055497 1 10880 10482 nil nil 10514 2706 #= 8 #() 2706 #hash 8 #() false 466 10336 nil 7 nil nil nil 10880 nil 8 1795593280 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 1 1 514 297 39 10880 1522 #text: 34 1 8 'NetLDI Guest' 10880 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 148 0 0 0 19 0 0 0] 8 #() 1680 nil 27 8 'guest' nil 1458 138 144 34 1 1522 #createAt:extent: 34 2 514 11 163 514 301 81 10256 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 81 0 0 0 155 0 0 0 121 0 0 0] 34 2 10880 10416 1680 nil 27 1698 1744 11 1760 301 2496 5 1792 81 410 ##(Smalltalk.StaticText) 34 16 nil 9760 34 2 8 1140850944 1 11488 nil nil nil 7 nil nil nil 11488 nil 8 1795590816 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 321 163 514 161 41 11488 1522 #text: 34 1 8 'OS User ID:' 11488 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 81 0 0 0 240 0 0 0 101 0 0 0] 8 #() 1680 nil 27 1698 4224 11 1760 161 6064 1 1792 41 410 ##(Smalltalk.PushButton) 34 20 nil 9760 34 2 8 1140924416 1 11808 nil nil nil 7 nil nil nil 11808 nil 8 1795593280 962 #debugGCI 8 'Debug GCI' 1 1 nil nil false nil nil nil 1458 138 144 34 3 1522 #createAt:extent: 34 2 514 11 5 514 141 51 11808 1522 #isEnabled: 8 #(false) 11808 1522 #text: 34 1 8 'Debug GCI' 11808 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 75 0 0 0 27 0 0 0] 8 #() 1680 nil 29 1698 1744 11 1760 141 2496 5 1792 51 410 ##(Smalltalk.StaticText) 34 16 nil 9760 34 2 8 1140850944 1 12176 nil nil nil 7 nil nil nil 12176 nil 8 1795590816 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 321 209 514 161 41 12176 1522 #text: 34 1 8 'Password:' 12176 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 104 0 0 0 240 0 0 0 124 0 0 0] 8 #() 1680 nil 27 1698 1730 #fixedPreviousLeft -169 1760 161 2496 3 1792 41 410 ##(Smalltalk.StaticText) 34 16 nil 9760 34 2 8 1140850944 1 12512 nil nil nil 7 nil nil nil 12512 nil 8 1795590816 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 11 69 514 301 41 12512 1522 #text: 34 1 8 'Stone Host (from Gem):' 12512 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 34 0 0 0 155 0 0 0 54 0 0 0] 8 #() 1680 nil 27 1698 1744 11 1760 301 2496 11 1792 41 410 ##(Smalltalk.StaticText) 34 16 nil 9760 34 2 8 1140850944 1 12832 nil nil nil 7 nil nil nil 12832 nil 8 1795590816 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 11 115 514 301 31 12832 1522 #text: 34 1 8 'NetLDI Task Name:' 12832 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 57 0 0 0 155 0 0 0 72 0 0 0] 8 #() 1680 nil 27 1698 1744 11 1760 301 2496 3 1792 31 410 ##(Smalltalk.PushButton) 34 20 nil 9760 34 2 8 1140924416 1 13152 nil nil nil 7 nil nil nil 13152 nil 8 1795593280 962 #login 8 'Login' 1 1 nil nil true nil nil nil 1458 138 144 34 3 1522 #createAt:extent: 34 2 514 11 305 514 665 61 13152 1522 #isEnabled: 8 #(false) 13152 1522 #text: 34 1 8 'Login' 13152 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 152 0 0 0 81 1 0 0 182 0 0 0] 8 #() 1680 nil 29 1698 1744 11 2144 -9 2496 1 1792 61 410 ##(Smalltalk.TextEdit) 34 16 nil 9760 34 2 8 1140916352 1025 13520 nil 466 5792 nil 7 nil nil nil 13520 nil 8 1795431936 4786 nil nil 1 1458 138 144 34 3 1522 #createAt:extent: 34 2 514 321 69 514 355 45 13520 1522 #text: 34 1 8 'localhost' 13520 1522 #setMarginWidths: 34 1 8 #(3 3) 13520 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 34 0 0 0 81 1 0 0 56 0 0 0] 8 #() 1680 nil 27 1698 4224 11 2144 -9 6064 1 1792 45 410 ##(Smalltalk.TextEdit) 34 16 nil 9760 34 2 8 1140916384 1025 13904 nil 466 8 4278190080 nil 7 nil nil nil 13904 nil 8 1795431936 4786 nil nil 1 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 491 209 514 185 45 13904 1522 #setMarginWidths: 34 1 8 #(3 3) 13904 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 104 0 0 0 81 1 0 0 126 0 0 0] 8 #() 1680 nil 27 1698 4224 11 2144 -9 6064 1 1792 45 410 ##(Smalltalk.StaticPath) 34 17 nil 9760 34 2 8 1140850944 1 14256 nil nil nil 7 nil nil nil 14256 nil 8 1795590816 4786 nil nil nil 8 'XXX' 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 161 15 514 515 45 14256 1522 #text: 34 1 14352 14256 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 7 0 0 0 81 1 0 0 29 0 0 0] 8 #() 1680 nil 27 1698 1744 161 2144 -9 6064 11 1792 45 410 ##(Smalltalk.ContainerView) 34 15 nil 9760 34 2 8 1140850688 393217 14576 nil 466 8 4278190080 nil 7 nil nil nil 14576 10354 3 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 14576 34 2 8 1140924425 1 14720 10482 nil nil 10514 2706 #= 8 #() 2706 #hash 8 #() false 466 14656 nil 7 nil nil nil 14720 nil 8 1795593280 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 1 1 514 331 45 14720 1522 #text: 34 1 8 'RPC Gem' 14720 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 165 0 0 0 22 0 0 0] 8 #() 1680 nil 27 8 'remote' 410 ##(Smalltalk.RadioButton) 34 16 nil 14576 34 2 8 1141055497 1 15152 10482 nil nil 10514 2706 #= 8 #() 2706 #hash 8 #() false 466 14656 nil 7 nil nil nil 15152 nil 8 1795593280 4786 nil nil nil 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 331 1 514 331 45 15152 1522 #text: 34 1 8 'Linked Gem (32-bit only)' 15152 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 74 1 0 0 22 0 0 0] 8 #() 1680 nil 27 8 'linked' nil 1458 138 144 34 1 1522 #createAt:extent: 34 2 514 11 257 514 665 49 14576 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 128 0 0 0 81 1 0 0 152 0 0 0] 34 2 14720 15152 1680 nil 27 1698 1744 11 2144 -9 2496 5 1792 49 410 ##(Smalltalk.TextEdit) 34 16 nil 9760 34 2 8 1140916352 1025 15760 nil 466 8 4278190080 nil 7 nil nil nil 15760 nil 8 1795431936 4786 nil nil 1 1458 138 144 34 2 1522 #createAt:extent: 34 2 514 491 163 514 185 45 15760 1522 #setMarginWidths: 34 1 8 #(3 3) 15760 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 245 0 0 0 81 0 0 0 81 1 0 0 103 0 0 0] 8 #() 1680 nil 27 1698 4224 11 2144 -9 6064 1 1792 45 170 192 34 14 9872 8 'gemTask' 15760 8 'hostUserID' 14576 8 'gemType' 13520 8 'stoneHost' 10256 8 'loginType' 14256 8 'debugPath' 13904 8 'hostPassword' nil 1458 138 144 34 1 1522 #createAt:extent: 34 2 514 685 1 514 685 367 9760 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 86 1 0 0 0 0 0 0 172 2 0 0 183 0 0 0] 34 13 11808 14256 12512 13520 12832 9872 10256 11488 15760 12176 13904 14576 13152 1680 nil 27 1698 9712 9730 8 0 0 0 0 0 0 224 63 2144 1 1776 1 3472 1 170 192 34 4 4592 8 'Basic' 9760 8 'Advanced' nil 1458 138 144 34 1 1522 #createAt:extent: 34 2 514 1 357 514 1369 367 4480 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 178 0 0 0 172 2 0 0 105 1 0 0] 34 2 4592 9760 1680 nil 27 41 true 170 192 34 4 608 8 'top' 4480 8 'bottom' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 265030 4 ##(Smalltalk.Menu) nil true 34 5 984134 2 ##(Smalltalk.CommandMenuItem) 1 962 #addLogin 8 '&New' 9373 1 nil nil nil 16834 1 962 #fileOpen 8 '&Open...' 9375 1 nil nil nil 16834 1 962 #removeLogin 8 '&Remove' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 16834 1 962 #exit 8 'E&xit' 17639 1 nil nil nil 8 '&File' nil 134217729 nil nil 61491 nil nil 16786 nil true 34 9 16834 1 962 #editUndo 8 '&Undo' 9397 1 nil nil nil 16834 1 962 #editRedo 8 '&Redo' 9395 1 nil nil nil 16994 4097 16834 1 962 #editCut 8 'Cu&t' 9393 1 nil nil nil 16834 1 962 #editCopy 8 '&Copy' 9351 1 nil nil nil 16834 1 962 #editPaste 8 '&Paste' 9389 1 nil nil nil 16834 1 962 #editDelete 8 'De&lete' 1629 1 nil nil nil 16994 4097 16834 1 962 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '&Edit' nil 134217729 nil nil 61507 nil nil 16786 nil true 34 10 16834 1 962 #helpContents 8 '&Contents' 1 1 nil nil nil 16834 1 962 #helpOnThisTool 8 '&On this Tool' 1249 1 nil nil nil 16834 1 962 #helpWhatsThis 8 '&What''s This?' 5345 1 nil nil nil 16994 4097 16834 1 962 #openGemTalkHomePage 8 '&GemTalk Home Page' 1025 1 nil nil nil 16834 1 962 #openJadeHomePage 8 '&Jade Home Page' 1 1 nil nil nil 16994 4097 16834 1 962 #showGemNRS 8 'Show Gem &NRS' 1 1 nil nil nil 16834 1 962 #showStoneNRS 8 'Show Stone N&RS' 1 1 nil nil nil 16834 1 962 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 61525 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 13715 263494 3 ##(Smalltalk.Icon) nil true 1056 8 'icons\GS32.ico' nil 17986 nil true 1056 8 'icons\GS16.ico' nil nil nil 1 nil nil 1458 138 144 34 3 1522 #createAt:extent: 34 2 514 2879 21 514 1401 841 416 1522 #text: 34 1 8 'Jade Login' 416 1522 #updateMenuBar 880 416 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 91 8 0 0 174 1 0 0] 34 3 608 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 18288 nil 466 8 4278190080 nil 519 nil nil nil 18288 1458 138 144 34 1 1522 #createAt:extent: 34 2 514 1 347 514 1369 11 18288 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 173 0 0 0 172 2 0 0 178 0 0 0] 8 #() 1680 nil 27 4480 1680 nil 27 )!

showOnSample

	^self showOn: JadeLogin sample.
! !
!JadeLoginShell class categoriesFor: #defaultFileExtension!constants!public! !
!JadeLoginShell class categoriesFor: #defaultModel!public! !
!JadeLoginShell class categoriesFor: #fileTypes!constants!public! !
!JadeLoginShell class categoriesFor: #icon!constants!public! !
!JadeLoginShell class categoriesFor: #resource_Default_view!public!resources-views! !
!JadeLoginShell class categoriesFor: #showOnSample!public! !

