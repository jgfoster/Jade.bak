| package |
package := Package name: 'Jade Login'.
package paxVersion: 1;
	basicComment: 'Login window redesign'.

package basicPackageVersion: '0.084'.


package classNames
	add: #JadeLogin;
	add: #JadeLoginShell;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Boolean\Dolphin Boolean Presenter';
	add: '..\Object Arts\Dolphin\MVP\Views\Cards\Dolphin Card Containers';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Radio\Dolphin Radio Buttons';
	add: '..\Object Arts\Dolphin\Registry\Dolphin Registry Access';
	add: '..\Object Arts\Dolphin\MVP\Views\Scintilla\Dolphin Scintilla View';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'GemStone C Interface';
	add: 'GemStone Session';
	add: 'Jade Class Browser';
	add: 'Jade Transcript';
	add: '..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	add: '..\Object Arts\Dolphin\ActiveX\Components\XML DOM\XML DOM';
	yourself).

package!

"Class Definitions"!

Model subclass: #JadeLogin
	instanceVariableNames: 'debugPath gemHost gemNRS gemService gemTask gemType gemVersion gsPassword gsUserID hostPassword hostUserID initials loginType stoneHost stoneName stoneNRS useSocket'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DocumentShell subclass: #JadeLoginShell
	instanceVariableNames: 'debugPathPresenter gemHostPresenter gemServicePresenter gemTaskPresenter gemTypePresenter hostPasswordPresenter hostUserIDPresenter initialsPresenter isModified loginTypePresenter passwordPresenter stoneHostPresenter stoneNamePresenter userIDPresenter useSocketPresenter versionListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

JadeLogin guid: (GUID fromString: '{585BD13E-3A03-47F3-B78D-5C29EF797913}')!
JadeLogin comment: ''!
!JadeLogin categoriesForClass!Unclassified! !
!JadeLogin methodsFor!

asXML

	| doc root |
	doc := IXMLDOMDocument new.
	root := (doc createElement: 'JadeLogin')
		setAttribute: 'gemVersion' 		value: gemVersion;
		setAttribute: 'stoneHost' 		value: stoneHost;
		setAttribute: 'stoneName' 		value: stoneName;
		setAttribute: 'stoneNRS' 		value: stoneNRS;
		setAttribute: 'gemType' 			value: gemType;
		setAttribute: 'gemHost' 			value: gemHost;
		setAttribute: 'gemService' 		value: gemService;
		setAttribute: 'gemNRS' 			value: gemNRS;
		setAttribute: 'hostUserID' 		value: hostUserID;
		setAttribute: 'hostPassword' 	value: hostPassword;
		setAttribute: 'gsUserID' 		value: gsUserID;
		setAttribute: 'gsPassword' 		value: gsPassword;
		setAttribute: 'initials'				value: initials;
		setAttribute: 'useSocket'		value: useSocket printString;
		yourself.
	doc setDocumentElement: root.
	^doc xml.
!

debugPath

	^debugPath ifNil: [''].
!

debugPath: aString

	self debugPath = aString ifTrue: [^self].
	debugPath := aString.
	self trigger: #changed.
!

gemHost

	gemHost notNilOrEmpty ifTrue: [^gemHost].
	^'localhost'.
!

gemHost: aString

	self gemHost = aString ifTrue: [^self].
	gemHost := aString.
	self
		generateGemNRS;
		trigger: #changed;
		yourself.
!

gemNRS

	gemType = #linked ifTrue: [^''].
	^gemNRS!

gemNRS: aString

	self gemNRS = aString ifTrue: [^self].
	gemNRS := aString.
	self trigger: #changed.
!

gemService

	^gemService.
!

gemService: aString

	self gemService = aString ifTrue: [^self].
	gemService := aString.
	self
		generateGemNRS;
		trigger: #changed;
		yourself.
!

gemTask

	gemTask notNilOrEmpty ifTrue: [^gemTask].
	^'gemnetobject'.
!

gemTask: aString

	self gemTask = aString ifTrue: [^self].
	gemTask := aString.
	self
		generateGemNRS;
		trigger: #changed;
		yourself.
!

gemType
	^gemType!

gemType: aSymbol

	(#( #linked #remote ) includes: aSymbol) ifFalse: [self error: 'Invalid Gem type'].
	self gemType = aSymbol ifTrue: [^self].
	gemType := aSymbol.
	self trigger: #changed.
!

gemVersion
	gemVersion isNilOrEmpty ifTrue: [^Gcilw6x displayName].
	^gemVersion!

gemVersion: aString 
	gemVersion := aString!

generateGemNRS

	gemNRS := '!!tcp@' , self gemHost , '#netldi:' , self gemService , '#task!!' , self gemTask.
!

generateStoneNRS

	stoneNRS := '!!tcp@' , self stoneHost , '#server!!' , self stoneName.
!

gsPassword

	gsPassword isNilOrEmpty ifTrue: [^'swordfish'].
	^gsPassword.
!

gsPassword: aString

	self gsPassword = aString ifTrue: [^self].
	gsPassword := aString.
	self trigger: #changed.
!

gsUserID

	gsUserID isNilOrEmpty ifTrue: [^'DataCurator'].
	^gsUserID.
!

gsUserID: aString

	self gsUserID = aString ifTrue: [^self].
	gsUserID := aString.
	self trigger: #changed.
!

hostPassword
	^hostPassword!

hostPassword: aString

	self hostPassword = aString ifTrue: [^self].
	hostPassword := aString.
	self trigger: #changed.
!

hostUserID
	^hostUserID!

hostUserID: aString

	self hostUserID = aString ifTrue: [^self].
	hostUserID := aString.
	self trigger: #changed.
!

initializeFromXML: aString

	| element |
	element := (IXMLDOMDocument new loadText: aString) documentElement.
	element baseName = 'JadeLogin' ifFalse: [self error: 'Invalid document!!'].
	gemVersion		:= element getAttribute: 'gemVersion'.
	stoneHost			:= element getAttribute: 'stoneHost'.
	stoneName		:= element getAttribute: 'stoneName'.
	stoneNRS			:= element getAttribute: 'stoneNRS'.
	gemType			:= (element getAttribute: 'gemType') asSymbol.
	gemHost			:= element getAttribute: 'gemHost'.
	gemService		:= element getAttribute: 'gemService'.
	gemNRS			:= element getAttribute: 'gemNRS'.
	loginType			:= element getAttribute: 'loginType'.
	loginType 			:= loginType isNil ifTrue: [#'guest'] ifFalse: [loginType asSymbol].
	hostUserID		:= element getAttribute: 'hostUserID'.
	hostPassword	:= element getAttribute: 'hostPassword'.
	gsUserID			:= element getAttribute: 'gsUserID'.
	gsPassword		:= element getAttribute: 'gsPassword'.
	initials				:= element getAttribute: 'initials'.
	useSocket			:= (element getAttribute: 'useSocket') = 'true'.!

initials

	initials isNilOrEmpty ifTrue: [^'Jade'].
	^initials.
!

initials: aString

	self initials = aString ifTrue: [^self].
	initials := aString.
	self trigger: #changed.
!

login

	^GciSession
		libraryClass: (GciLibrary withDisplayName: gemVersion)
		stoneNRS: self stoneNRS
		gemNRS: self gemNRS
		userID: self gsUserID
		password: self gsPassword
		hostUserID: self hostUserID 
		password: self hostPassword
		initials: self initials
		useSocket: self useSocket
		debugPath: self debugPath.
!

loginType
	^loginType.
!

loginType: anObject

	loginType 			:= anObject.
	hostUserID 		:= ''.
	hostPassword 	:= ''.
!

saveDefault

	(FileStream write: self class defaultPath)
		nextPutAll: self asXML;
		close.
!

stoneHost

	stoneHost notNilOrEmpty ifTrue: [^stoneHost].
	^'localhost'.
!

stoneHost: aString

	self stoneHost = aString ifTrue: [^self].
	stoneHost := aString.
	self
		generateStoneNRS;
		trigger: #changed;
		yourself.

!

stoneName

	^stoneName.
!

stoneName: aString

	self stoneName = aString ifTrue: [^self].
	stoneName := aString.
	self
		generateStoneNRS;
		trigger: #changed;
		yourself.
!

stoneNRS
	^stoneNRS!

stoneNRS: aString

	self stoneNRS = aString ifTrue: [^self].
	stoneNRS := aString.
	self trigger: #changed.
!

updateFrom: aJadeLogin

	aJadeLogin isNil ifTrue: [^self].
	gemVersion 		:= aJadeLogin gemVersion.
	stoneHost 		:= aJadeLogin stoneHost.
	stoneName 		:= aJadeLogin stoneName.
	stoneNRS 		:= aJadeLogin stoneNRS.
	gemType 			:= aJadeLogin gemType.
	gemHost 			:= aJadeLogin gemHost.
	gemService 		:= aJadeLogin gemService.
	gemTask 			:= aJadeLogin gemTask.
	gemNRS 			:= aJadeLogin gemNRS.
	loginType			:= aJadeLogin loginType.
	hostUserID 		:= aJadeLogin hostUserID.
	hostPassword 	:= aJadeLogin hostPassword.
	gsUserID 		:= aJadeLogin gsUserID.
	gsPassword 		:= aJadeLogin gsPassword.
	initials				:= aJadeLogin initials.
	useSocket			:= aJadeLogin useSocket.
	debugPath		:= aJadeLogin debugPath.
	self trigger: #changed.
!

useSocket
	^useSocket == true.
!

useSocket: aBoolean

	self useSocket = aBoolean ifTrue: [^self].
	useSocket := aBoolean.
	self trigger: #changed.
! !
!JadeLogin categoriesFor: #asXML!public! !
!JadeLogin categoriesFor: #debugPath!accessing!public! !
!JadeLogin categoriesFor: #debugPath:!accessing!public! !
!JadeLogin categoriesFor: #gemHost!accessing!public! !
!JadeLogin categoriesFor: #gemHost:!accessing!public! !
!JadeLogin categoriesFor: #gemNRS!accessing!public! !
!JadeLogin categoriesFor: #gemNRS:!accessing!public! !
!JadeLogin categoriesFor: #gemService!accessing!public! !
!JadeLogin categoriesFor: #gemService:!accessing!public! !
!JadeLogin categoriesFor: #gemTask!accessing!public! !
!JadeLogin categoriesFor: #gemTask:!accessing!public! !
!JadeLogin categoriesFor: #gemType!accessing!public! !
!JadeLogin categoriesFor: #gemType:!accessing!public! !
!JadeLogin categoriesFor: #gemVersion!accessing!public! !
!JadeLogin categoriesFor: #gemVersion:!public! !
!JadeLogin categoriesFor: #generateGemNRS!private! !
!JadeLogin categoriesFor: #generateStoneNRS!private! !
!JadeLogin categoriesFor: #gsPassword!accessing!public! !
!JadeLogin categoriesFor: #gsPassword:!accessing!public! !
!JadeLogin categoriesFor: #gsUserID!accessing!public! !
!JadeLogin categoriesFor: #gsUserID:!accessing!public! !
!JadeLogin categoriesFor: #hostPassword!accessing!public! !
!JadeLogin categoriesFor: #hostPassword:!accessing!public! !
!JadeLogin categoriesFor: #hostUserID!accessing!public! !
!JadeLogin categoriesFor: #hostUserID:!accessing!public! !
!JadeLogin categoriesFor: #initializeFromXML:!public! !
!JadeLogin categoriesFor: #initials!accessing!public! !
!JadeLogin categoriesFor: #initials:!accessing!public! !
!JadeLogin categoriesFor: #login!public! !
!JadeLogin categoriesFor: #loginType!accessing!public! !
!JadeLogin categoriesFor: #loginType:!accessing!public! !
!JadeLogin categoriesFor: #saveDefault!public! !
!JadeLogin categoriesFor: #stoneHost!accessing!public! !
!JadeLogin categoriesFor: #stoneHost:!accessing!public! !
!JadeLogin categoriesFor: #stoneName!accessing!public! !
!JadeLogin categoriesFor: #stoneName:!accessing!public! !
!JadeLogin categoriesFor: #stoneNRS!accessing!public! !
!JadeLogin categoriesFor: #stoneNRS:!accessing!public! !
!JadeLogin categoriesFor: #updateFrom:!public! !
!JadeLogin categoriesFor: #useSocket!accessing!public! !
!JadeLogin categoriesFor: #useSocket:!accessing!public! !

!JadeLogin class methodsFor!

defaultPath

	| regKey appData |
	regKey := RegKey userRoot at: 'Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders' ifAbsent: [nil].
	appData := regKey valueAt: 'AppData'.
	^appData , '\JadeDefaultConnection.gss'.
!

defaultXML

^'<?xml version="1.0"?>
<JadeLogin 
	gemVersion="64-bit 3.2" 
	stoneHost="localhost" 
	stoneName="gs64stone" 
	stoneNRS="!!tcp@localhost#server!!gs64stone" 
	gemType="remote" 
	gemHost="localhost" 
	gemService="gs64ldi" 
	gemNRS="!!tcp@localhost#netldi:gs64ldi#task!!gemnetobject" 
	loginType="guest"
	hostUserID="" 
	hostPassword="" 
	gsUserID="DataCurator" 
	gsPassword="swordfish"
	initials="MyName"
	useSocket="false"
	debugPath=""
/>
'.
!

fromXML: aString

	^super new
		initializeFromXML: aString;
		yourself.
!

new

	| path stream string |
	path := self defaultPath.
	(File exists: path) ifTrue: [
		stream := FileStream read: path.
		string := stream upToEnd.
		stream close.
	] ifFalse: [
		string := self defaultXML.
	].
	^self fromXML: string.
!

publishedEventsOfInstances

	^super publishedEventsOfInstances
		add: #changed;
		yourself.
!

sample

	^self fromXML: self defaultXML.
! !
!JadeLogin class categoriesFor: #defaultPath!public! !
!JadeLogin class categoriesFor: #defaultXML!public! !
!JadeLogin class categoriesFor: #fromXML:!public! !
!JadeLogin class categoriesFor: #new!public! !
!JadeLogin class categoriesFor: #publishedEventsOfInstances!public! !
!JadeLogin class categoriesFor: #sample!public! !

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
	versionListPresenter list: list asSortedCollection.
!

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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 26476545 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  801 601 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 10 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1140853580 131137 624 0 482 8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 624 482 720 8 4294903967 234 256 98 0 234 256 98 4 13063 1246982 ##(Smalltalk.ToolbarSystemButton)  13063 0 624 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #getHelpOn 8 'Get help on widget' 1 1 0 1 23 13065 853766 ##(Smalltalk.ToolbarButton)  13065 0 624 1 962 8 #openHelp 8 'Jade Help' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 530 1857 33 71 98 2 944 1040 234 240 98 4 1 1 1120 31 0 1 0 530 33 33 530 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 391 1 530 399 51 624 1426 8 #updateSize 880 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 0 0 0 0 138 1 0 0 25 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedPreviousRight 1 1666 8 #fixedParentRight 1 1666 8 #fixedParentTop 1 1666 8 #fixedViewTop 51 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 1808 0 0 0 7 0 0 0 1808 0 8 4294903549 962 8 #login 8 'Login' 1 1 0 0 16 0 0 0 1362 202 208 98 3 1426 1456 98 2 530 1 445 530 789 61 1808 1426 8 #isEnabled: 98 1 32 1808 1426 8 #text: 98 1 8 'Login' 1808 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 222 0 0 0 138 1 0 0 252 0 0 0] 98 0 1616 0 29 1634 1666 8 #fixedParentLeft 1 1712 1 1666 8 #fixedParentBottom -59 1776 61 410 640 98 25 0 416 98 2 8 1409289036 131137 2304 0 482 8 4278190080 0 7 0 0 0 2304 482 2384 8 4294903967 234 256 880 234 256 98 6 13047 1115910 ##(Smalltalk.ToolbarIconButton)  13047 0 2304 1 962 8 #fileNew 8 'New' 1 1 263494 3 ##(Smalltalk.Icon)  0 16 1152 8 'DocumentShell.ico' 0 1106 0 16 0 0 0 0 3 530 33 33 1 13049 2482 13049 0 2304 1 962 8 #fileOpen 8 'Open' 1 1 2562 0 16 1152 8 'FileOpen.ico' 1200 1106 0 16 0 0 0 0 3 530 33 33 1 13051 2482 13051 0 2304 1 962 8 #fileSave 8 'Save' 1 1 2562 0 16 1152 8 'FileSave.ico' 1200 1106 0 16 0 0 0 0 3 530 33 33 1 98 4 2496 2640 2768 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 2304 3 0 1 234 240 98 6 2864 5 2608 1 2736 3 0 1 0 530 33 33 530 45 45 0 0 1362 202 208 98 2 1426 1456 98 2 530 1 1 530 151 51 2304 1426 1536 880 2304 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 75 0 0 0 25 0 0 0] 98 0 1616 0 27 1634 2240 1 1666 8 #fixedViewLeft 151 1744 1 1776 51 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1409286144 131073 3232 0 482 8 4278190080 0 7 0 0 0 3232 655878 ##(Smalltalk.CardLayout)  202 208 98 2 721414 ##(Smalltalk.Association)  8 'Basic' 410 8 ##(Smalltalk.ContainerView)  98 15 0 3232 98 2 8 1140850688 131073 3456 0 0 0 7 0 0 0 3456 562 234 240 98 28 410 8 ##(Smalltalk.TextEdit)  98 16 0 3456 98 2 8 1140916352 1025 3584 0 482 8 4278190080 0 7 0 0 0 3584 0 8 4294903467 852486 ##(Smalltalk.NullConverter)  0 0 1 1362 202 208 98 3 1426 1456 98 2 530 351 287 530 413 41 3584 1426 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 3584 1426 8 #isTextModified: 98 1 32 3584 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 143 0 0 0 125 1 0 0 163 0 0 0] 98 0 1616 0 27 1634 1680 11 1712 -9 1666 8 #fixedPreviousTop 1 1776 41 410 3600 98 16 0 3456 98 2 8 1140916352 1025 4080 0 482 8 4278190080 0 7 0 0 0 4080 0 8 4294903467 3714 0 0 1 1362 202 208 98 4 1426 1456 98 2 530 351 67 530 413 41 4080 1426 3872 98 1 3906 3 1 3 4080 1426 3952 98 1 32 4080 1426 8 #setMarginWidths: 98 1 98 2 7 7 4080 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 33 0 0 0 125 1 0 0 53 0 0 0] 98 0 1616 0 27 1634 1680 11 1712 -9 4048 1 1776 41 410 8 ##(Smalltalk.StaticText)  98 16 0 3456 98 2 8 1140850944 1 4528 0 0 0 7 0 0 0 4528 0 8 4294903921 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 11 157 530 331 39 4528 1426 2128 98 1 8 'Stone Name:' 4528 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 78 0 0 0 170 0 0 0 97 0 0 0] 98 0 1616 0 27 1634 2240 11 3200 331 1666 8 #fixedPreviousBottom 11 1776 39 410 8 ##(Smalltalk.ComboBox)  98 17 0 3456 98 2 8 1144063491 1025 4896 590662 2 ##(Smalltalk.ListModel)  202 208 880 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 4896 0 8 4294903497 459270 ##(Smalltalk.Message)  8 #displayString 98 0 880 401 1362 202 208 98 1 1426 1456 98 2 530 351 11 530 413 47 4896 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 5 0 0 0 125 1 0 0 28 0 0 0] 98 0 1616 0 27 1634 1680 11 1712 -9 1744 11 1776 47 410 4544 98 16 0 3456 98 2 8 1140850944 1 5344 0 0 0 7 0 0 0 5344 0 8 4294903921 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 11 107 530 331 31 5344 1426 2128 98 1 8 'NetLDI Service Name or Port:' 5344 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 53 0 0 0 170 0 0 0 68 0 0 0] 98 0 1616 0 27 1634 2240 11 3200 331 4864 1 1776 31 410 4544 98 16 0 3456 98 2 8 1140850944 1 5664 0 0 0 7 0 0 0 5664 0 8 4294903921 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 11 67 530 331 31 5664 1426 2128 98 1 8 'RPC Gem Host Name or IP:' 5664 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 0 0 0 170 0 0 0 48 0 0 0] 98 0 1616 0 27 1634 2240 11 3200 331 4864 11 1776 31 410 3600 98 16 0 3456 98 2 8 1140916352 1025 5984 0 482 4160 0 7 0 0 0 5984 0 8 4294903467 3714 0 0 1 1362 202 208 98 4 1426 1456 98 2 530 351 157 530 413 41 5984 1426 3872 98 1 3906 3 1 3 5984 1426 3952 98 1 32 5984 1426 4416 98 1 98 2 7 7 5984 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 78 0 0 0 125 1 0 0 98 0 0 0] 98 0 1616 0 27 1634 1680 11 1712 -9 4048 1 1776 41 410 3600 98 16 0 3456 98 2 8 1140916352 1025 6400 0 482 4160 0 7 0 0 0 6400 0 8 4294903467 3714 0 0 1 1362 202 208 98 4 1426 1456 98 2 530 351 197 530 413 41 6400 1426 3872 98 1 3906 3 1 3 6400 1426 3952 98 1 32 6400 1426 4416 98 1 98 2 7 7 6400 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 98 0 0 0 125 1 0 0 118 0 0 0] 98 0 1616 0 27 1634 1680 11 1712 -9 4048 1 1776 41 410 4544 98 16 0 3456 98 2 8 1140850944 1 6816 0 0 0 7 0 0 0 6816 0 8 4294903921 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 11 237 530 331 41 6816 1426 2128 98 1 8 'GemStone Password:' 6816 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 118 0 0 0 170 0 0 0 138 0 0 0] 98 0 1616 0 27 1634 2240 11 3200 331 4864 1 1776 41 410 3600 98 16 0 3456 98 2 8 1140916384 1025 7136 0 482 8 4278190080 0 7 0 0 0 7136 0 8 4294903467 3714 0 0 1 1362 202 208 98 4 1426 1456 98 2 530 351 237 530 413 41 7136 1426 3872 98 1 3906 3 1 3 7136 1426 3952 98 1 32 7136 1426 4416 98 1 98 2 7 7 7136 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 118 0 0 0 125 1 0 0 138 0 0 0] 98 0 1616 0 27 1634 1680 11 1712 -9 4048 1 1776 41 410 4544 98 16 0 3456 98 2 8 1140850944 1 7568 0 0 0 7 0 0 0 7568 0 8 4294903921 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 11 197 530 331 41 7568 1426 2128 98 1 8 'GemStone User ID:' 7568 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 98 0 0 0 170 0 0 0 118 0 0 0] 98 0 1616 0 27 1634 2240 11 3200 331 4864 1 1776 41 410 4544 98 16 0 3456 98 2 8 1140850944 1 7888 0 0 0 7 0 0 0 7888 0 8 4294903921 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 11 21 530 331 41 7888 1426 2128 98 1 8 'GemStone/Smalltalk Version:' 7888 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 10 0 0 0 170 0 0 0 30 0 0 0] 98 0 1616 0 27 1634 2240 11 3200 331 1744 21 1776 41 410 4544 98 16 0 3456 98 2 8 1140850944 1 8208 0 0 0 7 0 0 0 8208 0 8 4294903921 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 11 287 530 331 41 8208 1426 2128 98 1 8 'Monticello User Name:' 8208 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 143 0 0 0 170 0 0 0 163 0 0 0] 98 0 1616 0 27 1634 2240 11 3200 331 4864 11 1776 41 410 3600 98 16 0 3456 98 2 8 1140916352 1025 8528 0 482 4160 0 7 0 0 0 8528 0 8 4294903467 3714 0 0 1 1362 202 208 98 4 1426 1456 98 2 530 351 107 530 413 41 8528 1426 3872 98 1 3906 3 1 3 8528 1426 3952 98 1 32 8528 1426 4416 98 1 98 2 7 7 8528 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 53 0 0 0 125 1 0 0 73 0 0 0] 98 0 1616 0 27 1634 2240 351 1712 -9 4048 1 1776 41 234 256 98 14 4896 8 'versionList' 5984 8 'stoneName' 8528 8 'gemService' 6400 8 'userID' 3584 8 'initials' 7136 8 'password' 4080 8 'gemHost' 0 1362 202 208 98 1 1426 1456 98 2 530 9 49 530 773 339 3456 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 134 1 0 0 193 0 0 0] 98 14 7888 4896 5664 4080 5344 8528 4528 5984 7568 6400 6816 7136 8208 3584 1616 0 27 3410 8 'Advanced' 410 3472 98 15 0 3232 98 2 8 1140850688 131073 9280 0 0 0 5 0 0 0 9280 562 234 240 98 26 410 3600 98 16 0 9280 98 2 8 1140916352 1025 9392 0 482 8 4278190080 0 5 0 0 0 9392 0 8 4294903467 3714 0 0 1 1362 202 208 98 4 1426 1456 98 2 530 461 185 530 303 41 9392 1426 3872 98 1 3906 3 1 3 9392 1426 3952 98 1 32 9392 1426 4416 98 1 98 2 7 7 9392 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 230 0 0 0 92 0 0 0 125 1 0 0 112 0 0 0] 98 0 1616 0 27 1634 1680 11 1712 -9 4048 1 1776 41 410 4544 98 16 0 9280 98 2 8 1140850944 1 9824 0 0 0 5 0 0 0 9824 0 8 4294903921 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 291 225 530 161 41 9824 1426 2128 98 1 8 'Password:' 9824 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 145 0 0 0 112 0 0 0 225 0 0 0 132 0 0 0] 98 0 1616 0 27 1634 2240 291 3200 161 4864 1 1776 41 410 3600 98 16 0 9280 98 2 8 1140916384 1025 10144 0 482 8 4278190080 0 5 0 0 0 10144 0 8 4294903467 3714 0 0 1 1362 202 208 98 4 1426 1456 98 2 530 461 225 530 303 41 10144 1426 3872 98 1 3906 3 1 3 10144 1426 3952 98 1 32 10144 1426 4416 98 1 98 2 7 7 10144 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 230 0 0 0 112 0 0 0 125 1 0 0 132 0 0 0] 98 0 1616 0 27 1634 1680 11 1712 -9 4048 1 1776 41 410 4544 98 16 0 9280 98 2 8 1140850944 1 10576 0 0 0 5 0 0 0 10576 0 8 4294903921 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 11 105 530 271 41 10576 1426 2128 98 1 8 'Stone Host (from Gem):' 10576 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 52 0 0 0 140 0 0 0 72 0 0 0] 98 0 1616 0 27 1634 2240 11 3200 271 4864 11 1776 41 410 3600 98 16 0 9280 98 2 8 1140916352 1025 10896 0 482 4160 0 5 0 0 0 10896 0 8 4294903467 3714 0 0 1 1362 202 208 98 5 1426 1456 98 2 530 291 105 530 473 41 10896 1426 2128 98 1 8 'localhost' 10896 1426 3872 98 1 3906 3 1 3 10896 1426 3952 98 1 32 10896 1426 4416 98 1 98 2 7 7 10896 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 145 0 0 0 52 0 0 0 125 1 0 0 72 0 0 0] 98 0 1616 0 27 1634 2240 291 1712 -9 4048 1 1776 41 410 3472 98 15 0 9280 98 2 8 1140850688 393217 11360 0 482 8 4278190080 0 5 0 0 0 11360 656390 ##(Smalltalk.GridLayout)  3 5 1 1 234 256 98 4 410 8 ##(Smalltalk.RadioButton)  98 16 0 11360 98 2 8 1141055497 1 11520 721990 2 ##(Smalltalk.ValueHolder)  0 0 1376774 ##(Smalltalk.PluggableSearchPolicy)  5106 8 #= 98 0 5106 8 #hash 98 0 32 482 11440 0 5 0 0 0 11520 0 8 4294903549 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 375 1 530 375 57 11520 1426 2128 98 1 8 'Linked Gem (32-bit only)' 11520 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 187 0 0 0 0 0 0 0 118 1 0 0 28 0 0 0] 98 0 1616 0 27 8 'linked' 410 11536 98 16 0 11360 98 2 8 1140924425 1 12032 11602 0 0 11634 5106 11680 98 0 5106 11728 98 0 32 482 11440 0 5 0 0 0 12032 0 8 4294903549 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 1 1 530 375 57 12032 1426 2128 98 1 8 'RPC Gem (required for 64-bit)' 12032 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 187 0 0 0 28 0 0 0] 98 0 1616 0 27 8 'remote' 0 1362 202 208 98 1 1426 1456 98 2 530 11 275 530 753 61 11360 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 137 0 0 0 125 1 0 0 167 0 0 0] 98 2 12032 11520 1616 0 27 1634 2240 11 1712 -9 4864 11 1776 61 410 8 ##(Smalltalk.StaticPath)  98 17 0 9280 98 2 8 1140850944 1 12640 0 0 0 5 0 0 0 12640 0 8 4294903921 3714 0 0 0 8 'XXX' 1362 202 208 98 2 1426 1456 98 2 530 161 55 530 603 41 12640 1426 2128 98 1 12752 12640 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 27 0 0 0 125 1 0 0 47 0 0 0] 98 0 1616 0 27 1634 2240 161 1712 -9 4048 11 1776 41 410 8 ##(Smalltalk.CheckBox)  98 16 0 9280 98 2 8 1409363203 1 12976 11602 0 0 1114118 ##(Smalltalk.NeverSearchPolicy)  32 0 0 5 0 0 0 12976 0 8 4294903549 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 11 5 530 753 41 12976 1426 2128 98 1 8 'Open additional socket  to improve performance (experimental)' 12976 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 2 0 0 0 125 1 0 0 22 0 0 0] 98 0 1616 0 27 1634 2240 11 1712 -9 1744 5 1776 41 410 4544 98 16 0 9280 98 2 8 1140850944 1 13360 0 0 0 5 0 0 0 13360 0 8 4294903921 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 291 185 530 161 41 13360 1426 2128 98 1 8 'OS User ID:' 13360 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 145 0 0 0 92 0 0 0 225 0 0 0 112 0 0 0] 98 0 1616 0 27 1634 2240 291 3200 161 4048 1 1776 41 410 3600 98 16 0 9280 98 2 8 1140916352 1025 13680 0 482 4160 0 5 0 0 0 13680 0 8 4294903467 3714 0 0 1 1362 202 208 98 5 1426 1456 98 2 530 291 145 530 473 41 13680 1426 2128 98 1 8 'gemnetobject' 13680 1426 3872 98 1 3906 3 1 3 13680 1426 3952 98 1 32 13680 1426 4416 98 1 98 2 7 7 13680 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 145 0 0 0 72 0 0 0 125 1 0 0 92 0 0 0] 98 0 1616 0 27 1634 2240 291 1712 -9 4048 1 1776 41 410 3472 98 15 0 9280 98 2 8 1409286144 393217 14144 0 482 8 4278190080 0 5 0 0 0 14144 11458 1 3 1 1 234 256 98 4 410 11536 98 16 0 14144 98 2 8 1141055497 1 14288 11602 0 0 11634 5106 11680 98 0 5106 11728 98 0 32 482 14224 0 5 0 0 0 14288 0 8 4294903549 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 1 1 530 257 39 14288 1426 2128 98 1 8 'NetLDI Guest' 14288 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 0 0 0 19 0 0 0] 98 0 1616 0 27 8 'guest' 410 11536 98 16 0 14144 98 2 8 1140924425 1 14720 11602 0 0 11634 5106 11680 98 0 5106 11728 98 0 32 482 14224 0 5 0 0 0 14720 0 8 4294903549 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 1 39 530 257 39 14720 1426 2128 98 1 8 'Native OS User' 14720 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 128 0 0 0 38 0 0 0] 98 0 1616 0 27 8 'member' 0 1362 202 208 98 1 1426 1456 98 2 530 11 185 530 261 81 14144 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 92 0 0 0 135 0 0 0 132 0 0 0] 98 2 14288 14720 1616 0 27 1634 2240 11 3200 261 4864 1 1776 81 410 1824 98 20 0 9280 98 2 8 1140924416 1 15328 0 0 0 5 0 0 0 15328 0 8 4294903549 962 8 #debugGCI 8 'Debug GCI' 1 1 0 0 32 0 0 0 1362 202 208 98 3 1426 1456 98 2 530 11 45 530 141 51 15328 1426 2080 98 1 32 15328 1426 2128 98 1 8 'Debug GCI' 15328 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 22 0 0 0 75 0 0 0 47 0 0 0] 98 0 1616 0 29 1634 2240 11 3200 141 4864 1 1776 51 410 4544 98 16 0 9280 98 2 8 1140850944 1 15712 0 0 0 5 0 0 0 15712 0 8 4294903921 3714 0 0 0 1362 202 208 98 2 1426 1456 98 2 530 11 145 530 271 31 15712 1426 2128 98 1 8 'NetLDI Task Name:' 15712 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 72 0 0 0 140 0 0 0 87 0 0 0] 98 0 1616 0 27 1634 2240 11 3200 271 4864 1 1776 31 234 256 98 16 12976 8 'useSocket' 9392 8 'hostUserID' 12640 8 'debugPath' 11360 8 'gemType' 10896 8 'stoneHost' 14144 8 'loginType' 10144 8 'hostPassword' 13680 8 'gemTask' 0 1362 202 208 98 1 1426 1456 98 2 530 9 49 530 773 339 9280 1554 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 134 1 0 0 193 0 0 0] 98 13 12976 15328 12640 10576 10896 15712 13680 14144 13360 9392 9824 10144 11360 1616 0 27 3456 234 256 880 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 3232 98 2 8 1140916736 1 16368 4978 202 208 98 2 3440 9264 0 5040 0 0 1 0 0 0 16368 0 8 4294903577 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 5136 16528 7 257 0 16514 0 0 16546 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 16624 7 257 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 8 #noIcons 0 0 0 0 0 1362 202 208 98 3 1426 1456 98 2 530 1 1 530 789 395 16368 1426 8 #basicSelectionsByIndex: 98 1 98 1 3 16368 1426 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 16368 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 138 1 0 0 197 0 0 0] 98 0 1616 0 27 1362 202 208 98 1 1426 1456 98 2 530 1 51 530 789 395 3232 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 138 1 0 0 222 0 0 0] 98 3 3456 9280 16368 1616 0 27 1634 2240 1 1712 1 1744 51 2272 -59 410 640 98 25 0 416 98 2 8 1409289036 131137 17216 0 482 8 4278190080 0 7 0 0 0 17216 482 17296 8 4294903967 234 256 880 234 256 98 10 13053 1026 13053 0 17216 1 962 8 #editCut 8 'Cut' 1 1 0 1106 0 16 1152 8 'EditBar.bmp' 1200 0 7 530 193 33 1 13055 1026 13055 0 17216 1 962 8 #editCopy 8 'Copy' 1 1 0 17456 3 13057 1026 13057 0 17216 1 962 8 #editPaste 8 'Paste' 1 1 0 17456 5 13059 1026 13059 0 17216 1 962 8 #editDelete 8 'Delete' 1 1 0 17456 11 13061 1026 13061 0 17216 1 962 8 #editUndo 8 'Undo' 1 1 0 17456 7 98 6 17392 17504 17568 17632 17696 2914 0 0 17216 3 0 1 234 240 98 2 17456 1 0 1 0 530 33 33 530 45 45 0 0 1362 202 208 98 2 1426 1456 98 2 530 151 1 530 241 51 17216 1426 1536 880 17216 1554 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 0 0 0 0 195 0 0 0 25 0 0 0] 98 0 1616 0 27 1634 1680 1 3200 241 1744 1 1776 51 234 256 98 6 624 8 'Other Tools' 2304 8 'File Tools' 17216 8 'Edit Tools' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 962 2528 8 '&New' 9373 1 0 0 0 18226 1 962 2672 8 '&Open...' 9375 1 0 0 0 18226 1 962 2800 8 '&Save' 9383 1 0 0 0 18226 1 962 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 18226 1 962 8 #fileRevert 8 '&Revert' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 18226 1 962 8 #exit 8 'E&xit' 17639 1 0 0 0 8 '&File' 0 134217729 0 0 13011 0 0 18178 0 16 98 9 18226 1 962 17728 8 '&Undo' 9397 1 0 0 0 18226 1 962 8 #editRedo 8 '&Redo' 9395 1 0 0 0 18514 4097 18226 1 962 17424 8 'Cu&t' 9393 1 0 0 0 18226 1 962 17536 8 '&Copy' 9351 1 0 0 0 18226 1 962 17600 8 '&Paste' 9389 1 0 0 0 18226 1 962 17664 8 'De&lete' 1629 1 0 0 0 18514 4097 18226 1 962 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 8 '&Edit' 0 134217729 0 0 13027 0 0 18178 0 16 98 2 18226 1 962 8 #setDefaultFont 8 'Set Default &Font' 1 1 0 0 0 18226 1 962 8 #setCodeFont 8 'Set &Code Font' 1 1 0 0 0 8 '&Preferences' 0 134217729 0 0 13033 0 0 18178 0 16 98 7 18226 1 962 8 #helpContents 8 '&Contents' 1 1 0 0 0 18226 1 962 8 #helpOnThisTool 8 '&On this Tool' 1249 1 0 0 0 18226 1 962 8 #helpWhatsThis 8 '&What''s This?' 5345 1 0 0 0 18514 4097 18226 1 962 8 #openHomePage 8 '&GemStone Home Page' 1025 1 0 0 0 18514 4097 18226 1 962 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 134217729 0 0 13045 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 9883 2562 0 16 1152 8 'icons\GS32x32.ico' 0 2562 0 16 1152 8 'icons\GS16x16.ico' 0 0 0 1 0 0 1362 202 208 98 3 1426 1456 98 2 530 2879 21 530 801 601 416 1426 2128 98 1 8 'Jade Login' 416 1426 8 #updateMenuBar 880 416 1554 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 47 7 0 0 54 1 0 0] 98 5 2304 17216 624 1808 3232 1616 0 27 )!

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

"Binary Globals"!

