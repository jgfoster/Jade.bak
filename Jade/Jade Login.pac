| package |
package := Package name: 'Jade Login'.
package paxVersion: 1;
	basicComment: 'Login window redesign'.

package basicPackageVersion: '0.077'.


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
	instanceVariableNames: 'gemVersion stoneHost stoneName stoneNRS gemType gemHost gemService gemTask gemNRS hostUserID hostPassword gsUserID gsPassword loginType initials'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DocumentShell subclass: #JadeLoginShell
	instanceVariableNames: 'versionListPresenter pathPresenter stoneHostPresenter stoneNamePresenter gemTypePresenter gemHostPresenter gemServicePresenter gemTaskPresenter userIDPresenter passwordPresenter linkedTextPresenter loginTypePresenter loginTextPresenter hostUserIDPresenter hostPasswordPresenter initialsPresenter isModified'
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
		yourself.
	doc setDocumentElement: root.
	^doc xml.
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
!

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
		initials: self initials.
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
	self trigger: #changed.
! !
!JadeLogin categoriesFor: #asXML!public! !
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
	gemVersion="64-bit 3.1.0.x" 
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
	linkedTextPresenter		:= self add: TextPresenter 					new name: 'linkedText'.
	loginTypePresenter 		:= self add: RadioButtonSetPresenter 	new name: 'loginType'.
	loginTextPresenter		:= self add: TextPresenter 					new name: 'loginText'.
	hostUserIDPresenter	:= self add: TextPresenter						new name: 'hostUserID'.
	hostPasswordPresenter	:= self add: TextPresenter						new name: 'hostPassword'.
	initialsPresenter				:= self add: TextPresenter						new name: 'initials'.
!

createSchematicWiring

	super createSchematicWiring.
	versionListPresenter		when: #selectionChanged 	send: #isModified: 			to: self with: true.
	stoneHostPresenter 		when: #valueChanged 		send: #isModified: 			to: self with: true.
	stoneNamePresenter 	when: #valueChanged 		send: #isModified: 			to: self with: true.
	gemTypePresenter 		when: #valueChanged 		send: #gemTypeChanged 	to: self.
	gemTypePresenter		when: #valueChanged 		send: #isModified: 			to: self with: true.
	gemHostPresenter 		when: #valueChanged 		send: #isModified: 			to: self with: true.
	gemServicePresenter 	when: #valueChanged 		send: #isModified: 			to: self with: true.
	gemTaskPresenter 		when: #valueChanged 		send: #isModified: 			to: self with: true.
	userIDPresenter 			when: #valueChanged 		send: #isModified: 			to: self with: true.
	passwordPresenter 		when: #valueChanged 		send: #isModified: 			to: self with: true.
	loginTypePresenter		when: #valueChanged 		send: #isModified: 			to: self with: true.
	loginTypePresenter 		when: #valueChanged 		send: #loginTypeChanged 	to: self.
	hostUserIDPresenter	when: #valueChanged 		send: #isModified: 			to: self with: true.
	hostPasswordPresenter	when: #valueChanged 		send: #isModified: 			to: self with: true.
	initialsPresenter				when: #valueChanged		send: #isModified:				to: self with: true.
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

gemTypeChanged

	gemTypePresenter value = #'linked'
		ifTrue:  [linkedTextPresenter view show]
		ifFalse: [linkedTextPresenter view hide].
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

loginTypeChanged

	loginTypePresenter value = #'guest'
		ifTrue:  [loginTextPresenter view show]
		ifFalse: [loginTextPresenter view hide].
	hostUserIDPresenter 		value: ''.
	hostPasswordPresenter 	value: ''.
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
		shellOpen: 'http://www.gemstone.com/products/smalltalk/' 
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
		yourself.
!

updateView

	| selection |
	selection := versionListPresenter list 
		detect: [:each | each = self model gemVersion] 
		ifNone: [versionListPresenter list last].
	versionListPresenter	selection: selection.
	stoneHostPresenter 	value: self model stoneHost.
	stoneNamePresenter value: self model stoneName.
	gemTypePresenter 	value: self model gemType.
	gemHostPresenter 	value: self model gemHost.
	gemServicePresenter value: self model gemService.
	gemTaskPresenter 	value: self model gemTask.
	userIDPresenter 		value: self model gsUserID.
	passwordPresenter 	value: self model gsPassword.
	self model gemType = #'linked'
		ifTrue:  [linkedTextPresenter view show]
		ifFalse: [linkedTextPresenter view hide].
	loginTypePresenter	value: self model loginType.
	self model loginType = #'guest'
		ifTrue:  [loginTextPresenter view show]
		ifFalse: [loginTextPresenter view hide].
	hostUserIDPresenter	value: self model hostUserID.
	hostPasswordPresenter	value: self model hostPassword.
	initialsPresenter				value: self model initials.
	self isModified: false.
! !
!JadeLoginShell categoriesFor: #aboutJade!public! !
!JadeLoginShell categoriesFor: #basicCaption!accessing!public! !
!JadeLoginShell categoriesFor: #createComponents!public! !
!JadeLoginShell categoriesFor: #createSchematicWiring!public! !
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
!JadeLoginShell categoriesFor: #gemTypeChanged!public! !
!JadeLoginShell categoriesFor: #getDocumentData!accessing!public! !
!JadeLoginShell categoriesFor: #isModified!public! !
!JadeLoginShell categoriesFor: #isModified:!public! !
!JadeLoginShell categoriesFor: #isText!public!testing! !
!JadeLoginShell categoriesFor: #login!public! !
!JadeLoginShell categoriesFor: #loginTypeChanged!public! !
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

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 26476545 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  701 541 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 60 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 624 0 0 0 7 0 0 0 624 0 8 4294905631 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 341 171 530 121 31 624 818 8 #text: 98 1 8 'Password' 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 170 0 0 0 85 0 0 0 230 0 0 0 100 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 341 1090 8 #fixedViewLeft 121 1090 8 #fixedParentTop 171 1090 8 #fixedViewTop 31 410 8 ##(Smalltalk.GroupBox)  98 14 0 416 98 2 8 1140850695 65 1232 0 482 8 4294967295 0 7 0 0 0 1232 0 8 4294906439 754 202 208 98 2 818 848 98 2 530 5 221 530 681 161 1232 818 928 98 1 8 'Gem' 1232 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 110 0 0 0 86 1 0 0 190 0 0 0] 98 0 1040 0 27 1058 1104 5 1136 681 1168 221 1200 161 410 640 98 16 0 416 98 2 8 1140850944 1 1584 0 0 0 7 0 0 0 1584 0 8 4294905631 722 0 0 0 754 202 208 98 2 818 848 98 2 530 11 391 530 121 31 1584 818 928 98 1 8 'Developer' 1584 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 195 0 0 0 65 0 0 0 210 0 0 0] 98 0 1040 0 27 1058 1104 11 1136 121 1168 391 1200 31 410 1248 98 14 0 416 98 2 8 1140850695 65 1904 0 482 1328 0 7 0 0 0 1904 0 8 4294906439 754 202 208 98 2 818 848 98 2 530 5 91 530 681 121 1904 818 928 98 1 8 'Stone' 1904 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 45 0 0 0 86 1 0 0 105 0 0 0] 98 0 1040 0 27 1058 1104 5 1136 681 1168 91 1200 121 410 640 98 16 0 416 98 2 8 1140850944 1 2224 0 0 0 7 0 0 0 2224 0 8 4294905631 722 0 0 0 754 202 208 98 2 818 848 98 2 530 21 61 530 91 31 2224 818 928 98 1 8 'Version' 2224 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 30 0 0 0 55 0 0 0 45 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 61 1200 31 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916384 1025 2544 0 482 8 4278190080 0 7 0 0 0 2544 0 8 4294903239 722 0 0 1 754 202 208 98 4 818 848 98 2 530 361 711 530 201 41 2544 818 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 2544 818 8 #isTextModified: 98 1 32 2544 818 8 #setMarginWidths: 98 1 98 2 7 7 2544 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 99 1 0 0 24 1 0 0 119 1 0 0] 98 0 1040 0 27 1058 1104 361 1136 201 1168 711 1200 41 410 2560 98 16 0 416 98 2 8 1140916352 1025 3056 0 482 8 4278190080 0 7 0 0 0 3056 0 8 4294903239 722 0 0 1 754 202 208 98 4 818 848 98 2 530 121 161 530 201 41 3056 818 2816 98 1 2850 3 1 3 3056 818 2896 98 1 32 3056 818 2944 98 1 98 2 7 7 3056 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 80 0 0 0 160 0 0 0 100 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 161 1200 41 410 640 98 16 0 416 98 2 8 1140850944 1 3488 0 0 0 7 0 0 0 3488 0 8 4294905631 722 0 0 0 754 202 208 98 2 818 848 98 2 530 341 131 530 121 31 3488 818 928 98 1 8 'Database' 3488 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 170 0 0 0 65 0 0 0 230 0 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 341 1136 121 1168 131 1200 31 410 2560 98 16 0 416 98 2 8 1140916352 1025 3808 0 482 3136 0 7 0 0 0 3808 0 8 4294903239 722 0 0 1 754 202 208 98 4 818 848 98 2 530 121 331 530 201 41 3808 818 2816 98 1 2850 3 1 3 3808 818 2896 98 1 32 3808 818 2944 98 1 98 2 7 7 3808 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 165 0 0 0 160 0 0 0 185 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 331 1200 41 410 640 98 16 0 416 98 2 8 1140850944 1 4224 0 0 0 7 0 0 0 4224 0 8 4294905631 722 0 0 0 754 202 208 98 2 818 848 98 2 530 611 651 530 151 41 4224 818 928 98 1 8 'Password:' 4224 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 69 1 0 0 124 1 0 0 89 1 0 0] 98 0 1040 0 27 1058 1104 611 1136 151 1168 651 1200 41 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 393217 4544 0 482 8 4278190080 0 7 0 0 0 4544 656390 ##(Smalltalk.GridLayout)  5 3 1 1 234 256 98 4 410 8 ##(Smalltalk.RadioButton)  98 16 0 4544 98 2 8 1140924425 1 4720 721990 2 ##(Smalltalk.ValueHolder)  0 32 1376774 ##(Smalltalk.PluggableSearchPolicy)  459270 ##(Smalltalk.Message)  8 #= 98 0 4866 8 #hash 98 0 16 482 4640 0 7 0 0 0 4720 0 8 4294906439 722 0 0 0 754 202 208 98 2 818 848 98 2 530 1 59 530 137 59 4720 818 928 98 1 8 'Remote' 4720 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 29 0 0 0 68 0 0 0 58 0 0 0] 98 0 1040 0 27 8 'remote' 410 4736 98 16 0 4544 98 2 8 1140924425 1 5248 4802 0 0 4834 4866 4896 98 0 4866 4944 98 0 32 482 4640 0 7 0 0 0 5248 0 8 4294906439 722 0 0 0 754 202 208 98 2 818 848 98 2 530 1 1 530 137 59 5248 818 928 98 1 8 'Linked' 5248 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 68 0 0 0 29 0 0 0] 98 0 1040 0 27 8 'linked' 0 754 202 208 98 1 818 848 98 2 530 341 251 530 141 121 4544 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 170 0 0 0 125 0 0 0 240 0 0 0 185 0 0 0] 98 2 5248 4720 1040 0 27 1058 1104 341 1136 141 1168 251 1200 121 410 2560 98 16 0 416 98 2 8 1140916352 1025 5856 0 482 3136 0 7 0 0 0 5856 0 8 4294903239 722 0 0 1 754 202 208 98 4 818 848 98 2 530 121 121 530 201 41 5856 818 2816 98 1 2850 3 1 3 5856 818 2896 98 1 32 5856 818 2944 98 1 98 2 7 7 5856 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 60 0 0 0 160 0 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 121 1200 41 410 640 98 16 0 416 98 2 8 1140850944 1 6272 0 0 0 7 0 0 0 6272 0 8 4294905631 722 0 0 0 754 202 208 98 2 818 848 98 2 530 21 171 530 71 31 6272 818 928 98 1 8 'User' 6272 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 85 0 0 0 45 0 0 0 100 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 71 1168 171 1200 31 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1140853580 131137 6592 0 482 8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 6592 482 6688 8 4294905755 234 256 98 0 234 256 98 4 21649 853766 ##(Smalltalk.ToolbarButton)  21649 0 6592 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #openHelp 8 'Jade Help' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 530 1857 33 71 21647 1246982 ##(Smalltalk.ToolbarSystemButton)  21647 0 6592 1 6930 8 #getHelpOn 8 'Get help on widget' 1 1 0 1 23 98 2 7152 6912 234 240 98 4 1 1 7008 31 0 1 0 530 33 33 530 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 754 202 208 98 2 818 848 98 2 530 391 1 530 299 51 6592 818 8 #updateSize 6848 6592 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 0 0 0 0 88 1 0 0 25 0 0 0] 98 0 1040 0 27 1058 1090 8 #fixedPreviousRight 1 1090 8 #fixedParentRight 1 1168 1 1200 51 410 2560 98 16 0 416 98 2 8 1140916352 1025 7600 0 482 3136 0 7 0 0 0 7600 0 8 4294903239 722 0 0 1 754 202 208 98 4 818 848 98 2 530 121 251 530 201 41 7600 818 2816 98 1 2850 3 1 3 7600 818 2896 98 1 32 7600 818 2944 98 1 98 2 7 7 7600 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 125 0 0 0 160 0 0 0 145 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 251 1200 41 410 6608 98 25 0 416 98 2 8 1409289036 131137 8016 0 482 8 4278190080 0 7 0 0 0 8016 482 8096 8 4294905755 234 256 6848 234 256 98 10 21637 6898 21637 0 8016 1 6930 8 #editCut 8 'Cut' 1 1 0 6994 0 16 7040 8 'EditBar.bmp' 7088 0 7 530 193 33 1 21639 6898 21639 0 8016 1 6930 8 #editCopy 8 'Copy' 1 1 0 8256 3 21641 6898 21641 0 8016 1 6930 8 #editPaste 8 'Paste' 1 1 0 8256 5 21643 6898 21643 0 8016 1 6930 8 #editDelete 8 'Delete' 1 1 0 8256 11 21645 6898 21645 0 8016 1 6930 8 #editUndo 8 'Undo' 1 1 0 8256 7 98 6 8192 8304 8368 8432 8496 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 8016 3 0 1 234 240 98 2 8256 1 0 1 0 530 33 33 530 45 45 0 0 754 202 208 98 2 818 848 98 2 530 151 1 530 241 51 8016 818 7456 6848 8016 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 0 0 0 0 195 0 0 0 25 0 0 0] 98 0 1040 0 27 1058 7536 1 1136 241 1168 1 1200 51 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 8864 0 0 0 7 0 0 0 8864 0 8 4294906439 6930 8 #login 8 'Login' 1 1 0 0 16 0 0 0 754 202 208 98 3 818 848 98 2 530 481 381 530 201 61 8864 818 8 #isEnabled: 98 1 32 8864 818 928 98 1 8 'Login' 8864 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 0 0 0 190 0 0 0 84 1 0 0 220 0 0 0] 98 0 1040 0 29 1058 1104 481 1136 201 1168 381 1200 61 410 2560 98 16 0 416 98 2 8 1140916352 1025 9280 0 482 3136 0 7 0 0 0 9280 0 8 4294903239 722 0 0 1 754 202 208 98 4 818 848 98 2 530 471 121 530 201 41 9280 818 2816 98 1 2850 3 1 3 9280 818 2896 98 1 32 9280 818 2944 98 1 98 2 7 7 9280 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 235 0 0 0 60 0 0 0 79 1 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 471 1136 201 1168 121 1200 41 410 2560 98 16 0 416 98 2 8 1140916352 1025 9696 0 482 3136 0 7 0 0 0 9696 0 8 4294903239 722 0 0 1 754 202 208 98 4 818 848 98 2 530 121 291 530 201 41 9696 818 2816 98 1 2850 3 1 3 9696 818 2896 98 1 32 9696 818 2944 98 1 98 2 7 7 9696 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 145 0 0 0 160 0 0 0 165 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 291 1200 41 410 2560 98 16 0 416 98 2 8 1140916384 1025 10112 0 482 8 4278190080 0 7 0 0 0 10112 0 8 4294903239 722 0 0 1 754 202 208 98 4 818 848 98 2 530 471 161 530 201 41 10112 818 2816 98 1 2850 3 1 3 10112 818 2896 98 1 32 10112 818 2944 98 1 98 2 7 7 10112 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 235 0 0 0 80 0 0 0 79 1 0 0 100 0 0 0] 98 0 1040 0 27 1058 1104 471 1136 201 1168 161 1200 41 410 640 98 16 0 416 98 2 8 1140850944 1 10544 0 0 0 7 0 0 0 10544 0 8 4294905631 722 0 0 0 754 202 208 98 2 818 848 98 2 530 21 261 530 91 31 10544 818 928 98 1 8 'Host/IP' 10544 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 130 0 0 0 55 0 0 0 145 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 261 1200 31 410 640 98 16 0 416 98 2 8 1140850944 1 10864 0 0 0 7 0 0 0 10864 0 8 4294905631 722 0 0 0 754 202 208 98 2 818 848 98 2 530 611 611 530 151 41 10864 818 928 98 1 8 'User ID:' 10864 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 49 1 0 0 124 1 0 0 69 1 0 0] 98 0 1040 0 27 1058 1104 611 1136 151 1168 611 1200 41 410 640 98 16 0 416 98 2 8 1140850944 1 11184 0 0 0 7 0 0 0 11184 0 8 4294905631 722 0 0 0 754 202 208 98 2 818 848 98 2 530 21 341 530 61 31 11184 818 928 98 1 8 'Task' 11184 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 170 0 0 0 40 0 0 0 185 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 61 1168 341 1200 31 410 4560 98 15 0 416 98 2 8 1409286144 393217 11504 0 482 8 4278190080 0 7 0 0 0 11504 4658 1 3 1 1 234 256 98 4 410 4736 98 16 0 11504 98 2 8 1140924425 1 11648 4802 0 32 4834 4866 4896 98 0 4866 4944 98 0 16 482 11584 0 7 0 0 0 11648 0 8 4294906439 722 0 0 0 754 202 208 98 2 818 848 98 2 530 1 1 530 147 59 11648 818 928 98 1 8 'Guest' 11648 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 73 0 0 0 29 0 0 0] 98 0 1040 0 27 8 'guest' 410 4736 98 16 0 11504 98 2 8 1140924425 1 12080 4802 0 0 4834 4866 4896 98 0 4866 4944 98 0 32 482 11584 0 7 0 0 0 12080 0 8 4294906439 722 0 0 0 754 202 208 98 2 818 848 98 2 530 1 59 530 147 59 12080 818 928 98 1 8 'OS User' 12080 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 29 0 0 0 73 0 0 0 58 0 0 0] 98 0 1040 0 27 8 'member' 0 754 202 208 98 1 818 848 98 2 530 501 251 530 151 121 11504 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 250 0 0 0 125 0 0 0 69 1 0 0 185 0 0 0] 98 2 11648 12080 1040 0 27 1058 1104 501 1136 151 1168 251 1200 121 410 6608 98 25 0 416 98 2 8 1409289036 131137 12688 0 482 8 4278190080 0 7 0 0 0 12688 482 12768 8 4294905755 234 256 6848 234 256 98 6 21631 1115910 ##(Smalltalk.ToolbarIconButton)  21631 0 12688 1 6930 8 #fileNew 8 'New' 1 1 263494 3 ##(Smalltalk.Icon)  0 16 7040 8 'DocumentShell.ico' 0 6994 0 16 0 0 0 0 3 530 33 33 1 21633 12866 21633 0 12688 1 6930 8 #fileOpen 8 'Open' 1 1 12946 0 16 7040 8 'FileOpen.ico' 7088 6994 0 16 0 0 0 0 3 530 33 33 1 21635 12866 21635 0 12688 1 6930 8 #fileSave 8 'Save' 1 1 12946 0 16 7040 8 'FileSave.ico' 7088 6994 0 16 0 0 0 0 3 530 33 33 1 98 4 12880 13024 13152 8578 0 0 12688 3 0 1 234 240 98 6 13248 5 12992 1 13120 3 0 1 0 530 33 33 530 45 45 0 0 754 202 208 98 2 818 848 98 2 530 1 1 530 151 51 12688 818 7456 6848 12688 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 75 0 0 0 25 0 0 0] 98 0 1040 0 27 1058 1104 1 1136 151 1168 1 1200 51 410 2560 98 16 0 416 98 2 8 1140916352 1025 13568 0 482 8 4278190080 0 7 0 0 0 13568 0 8 4294903239 722 0 0 1 754 202 208 98 3 818 848 98 2 530 135 391 530 181 41 13568 818 2816 98 1 2850 3 1 3 13568 818 2896 98 1 32 13568 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 67 0 0 0 195 0 0 0 157 0 0 0 215 0 0 0] 98 0 1040 0 27 1058 1104 135 1136 181 1168 391 1200 41 410 2560 98 16 0 416 98 2 8 1140916352 1025 13952 0 482 8 4278190080 0 7 0 0 0 13952 0 8 4294903239 722 0 0 1 754 202 208 98 4 818 848 98 2 530 81 581 530 201 41 13952 818 2816 98 1 2850 3 1 3 13952 818 2896 98 1 32 13952 818 2944 98 1 98 2 7 7 13952 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 40 0 0 0 34 1 0 0 140 0 0 0 54 1 0 0] 98 0 1040 0 27 1058 1104 81 1136 201 1168 581 1200 41 410 640 98 16 0 416 98 2 8 1140850944 1 14384 0 0 0 7 0 0 0 14384 0 8 4294905631 722 0 0 0 754 202 208 98 2 818 848 98 2 530 21 301 530 91 31 14384 818 928 98 1 8 'Netldi' 14384 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 150 0 0 0 55 0 0 0 165 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 301 1200 31 410 640 98 16 0 416 98 2 8 1140850944 1 14704 0 0 0 7 0 0 0 14704 0 8 4294905631 722 0 0 0 754 202 208 98 2 818 848 98 2 530 21 131 530 91 31 14704 818 928 98 1 8 'Host/IP' 14704 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 65 0 0 0 55 0 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 131 1200 31 410 8 ##(Smalltalk.ComboBox)  98 17 0 416 98 2 8 1144063491 1025 15024 590662 2 ##(Smalltalk.ListModel)  202 208 6848 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 15024 0 8 4294901889 4866 8 #displayString 98 0 6848 401 754 202 208 98 1 818 848 98 2 530 131 51 530 371 47 15024 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 65 0 0 0 25 0 0 0 250 0 0 0 48 0 0 0] 98 0 1040 0 27 1058 1104 131 1136 371 1168 51 1200 47 234 256 98 28 12688 8 'File Tools' 3056 8 'userID' 13952 8 'hostUserID' 6592 8 'Other Tools' 15024 8 'versionList' 10112 8 'password' 3808 8 'gemTask' 9696 8 'gemService' 2544 8 'hostPassword' 7600 8 'gemHost' 9280 8 'stoneName' 13568 8 'initials' 5856 8 'stoneHost' 8016 8 'Edit Tools' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 6930 12912 8 '&New' 9373 1 0 0 0 15810 1 6930 13056 8 '&Open...' 9375 1 0 0 0 15810 1 6930 13184 8 '&Save' 9383 1 0 0 0 15810 1 6930 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 15810 1 6930 8 #fileRevert 8 '&Revert' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 15810 1 6930 8 #exit 8 'E&xit' 17639 1 0 0 0 8 '&File' 0 134217729 0 0 21595 0 0 15762 0 16 98 9 15810 1 6930 8528 8 '&Undo' 9397 1 0 0 0 15810 1 6930 8 #editRedo 8 '&Redo' 9395 1 0 0 0 16098 4097 15810 1 6930 8224 8 'Cu&t' 9393 1 0 0 0 15810 1 6930 8336 8 '&Copy' 9351 1 0 0 0 15810 1 6930 8400 8 '&Paste' 9389 1 0 0 0 15810 1 6930 8464 8 'De&lete' 1629 1 0 0 0 16098 4097 15810 1 6930 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 8 '&Edit' 0 134217729 0 0 21611 0 0 15762 0 16 98 2 15810 1 6930 8 #setDefaultFont 8 'Set Default &Font' 1 1 0 0 0 15810 1 6930 8 #setCodeFont 8 'Set &Code Font' 1 1 0 0 0 8 '&Preferences' 0 134217729 0 0 21617 0 0 15762 0 16 98 7 15810 1 6930 8 #helpContents 8 '&Contents' 1 1 0 0 0 15810 1 6930 8 #helpOnThisTool 8 '&On this Tool' 1249 1 0 0 0 15810 1 6930 8 #helpWhatsThis 8 '&What''s This?' 5345 1 0 0 0 16098 4097 15810 1 6930 8 #openHomePage 8 '&GemStone Home Page' 1025 1 0 0 0 16098 4097 15810 1 6930 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 134217729 0 0 21629 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 15373 12946 0 16 7040 8 'icons\GS32x32.ico' 0 12946 0 16 7040 8 'icons\GS16x16.ico' 0 0 0 1 0 0 754 202 208 98 3 818 848 98 2 530 7679 21 530 701 541 416 818 928 98 1 8 'Jade Login' 416 818 8 #updateMenuBar 6848 416 978 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 93 16 0 0 24 1 0 0] 98 30 15024 5856 9280 3056 10112 7600 9696 3808 13568 12688 8016 6592 13952 2544 8864 14704 3488 14384 11184 10864 4224 10544 1904 1232 2224 6272 624 1584 4544 11504 1040 0 27 )!

showOnSample

	^self showOn: JadeLogin sample.
! !
!JadeLoginShell class categoriesFor: #defaultFileExtension!constants!public! !
!JadeLoginShell class categoriesFor: #defaultModel!public! !
!JadeLoginShell class categoriesFor: #fileTypes!constants!public! !
!JadeLoginShell class categoriesFor: #icon!constants!public! !
!JadeLoginShell class categoriesFor: #resource_Default_view!public!resources-views! !
!JadeLoginShell class categoriesFor: #showOnSample!public! !

"Binary Globals"!

