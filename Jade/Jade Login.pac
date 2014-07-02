| package |
package := Package name: 'Jade Login'.
package paxVersion: 1;
	basicComment: 'Login window redesign'.

package basicPackageVersion: '0.080'.


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

resource_Compact_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 26476545 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  701 471 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 52 410 8 ##(Smalltalk.GroupBox)  98 14 0 416 98 2 8 1140850695 65 624 0 482 8 4294967295 0 7 0 0 0 624 0 8 4294903245 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 5 211 530 681 91 624 818 8 #text: 98 1 8 'Gem' 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 105 0 0 0 86 1 0 0 150 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 5 1090 8 #fixedViewLeft 681 1090 8 #fixedParentTop 211 1090 8 #fixedViewTop 91 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1140853580 131137 1232 0 482 8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 1232 482 1328 8 4294903147 234 256 98 0 234 256 98 4 21567 1246982 ##(Smalltalk.ToolbarSystemButton)  21567 0 1232 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #getHelpOn 8 'Get help on widget' 1 1 0 1 23 21569 853766 ##(Smalltalk.ToolbarButton)  21569 0 1232 1 1570 8 #openHelp 8 'Jade Help' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 530 1857 33 71 98 2 1552 1648 234 240 98 4 1 1 1728 31 0 1 0 530 33 33 530 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 754 202 208 98 2 818 848 98 2 530 391 1 530 299 51 1232 818 8 #updateSize 1488 1232 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 0 0 0 0 88 1 0 0 25 0 0 0] 98 0 1040 0 27 1058 1090 8 #fixedPreviousRight 1 1090 8 #fixedParentRight 1 1168 1 1200 51 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2240 0 0 0 7 0 0 0 2240 0 8 4294903255 852486 ##(Smalltalk.NullConverter)  0 0 0 754 202 208 98 2 818 848 98 2 530 611 651 530 151 41 2240 818 928 98 1 8 'Password:' 2240 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 69 1 0 0 124 1 0 0 89 1 0 0] 98 0 1040 0 27 1058 1104 611 1136 151 1168 651 1200 41 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 2592 0 482 8 4278190080 0 7 0 0 0 2592 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 471 241 530 201 41 2592 818 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 2592 818 8 #isTextModified: 98 1 32 2592 818 8 #setMarginWidths: 98 1 98 2 7 7 2592 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 235 0 0 0 120 0 0 0 79 1 0 0 140 0 0 0] 98 0 1040 0 27 1058 1104 471 1136 201 1168 241 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 3104 0 0 0 7 0 0 0 3104 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 611 611 530 151 41 3104 818 928 98 1 8 'User ID:' 3104 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 49 1 0 0 124 1 0 0 69 1 0 0] 98 0 1040 0 27 1058 1104 611 1136 151 1168 611 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 3424 0 0 0 7 0 0 0 3424 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 21 247 530 91 31 3424 818 928 98 1 8 'Host/IP' 3424 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 123 0 0 0 55 0 0 0 138 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 247 1200 31 410 2256 98 16 0 416 98 2 8 1140850944 1 3744 0 0 0 7 0 0 0 3744 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 341 127 530 121 31 3744 818 928 98 1 8 'Database' 3744 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 170 0 0 0 63 0 0 0 230 0 0 0 78 0 0 0] 98 0 1040 0 27 1058 1104 341 1136 121 1168 127 1200 31 410 2608 98 16 0 416 98 2 8 1140916352 1025 4064 0 482 8 4278190080 0 7 0 0 0 4064 0 8 4294903261 2338 0 0 1 754 202 208 98 3 818 848 98 2 530 135 321 530 321 41 4064 818 2864 98 1 2898 3 1 3 4064 818 2944 98 1 32 4064 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 67 0 0 0 160 0 0 0 227 0 0 0 180 0 0 0] 98 0 1040 0 27 1058 1104 135 1136 321 1168 321 1200 41 410 1248 98 25 0 416 98 2 8 1409289036 131137 4448 0 482 8 4278190080 0 7 0 0 0 4448 482 4528 8 4294903147 234 256 1488 234 256 98 10 21561 1634 21561 0 4448 1 1570 8 #editPaste 8 'Paste' 1 1 0 1714 0 16 1760 8 'EditBar.bmp' 1808 0 7 530 193 33 5 21563 1634 21563 0 4448 1 1570 8 #editDelete 8 'Delete' 1 1 0 4688 11 21565 1634 21565 0 4448 1 1570 8 #editUndo 8 'Undo' 1 1 0 4688 7 21557 1634 21557 0 4448 1 1570 8 #editCut 8 'Cut' 1 1 0 4688 1 21559 1634 21559 0 4448 1 1570 8 #editCopy 8 'Copy' 1 1 0 4688 3 98 6 4864 4928 4624 4736 4800 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 4448 3 0 1 234 240 98 2 4688 1 0 1 0 530 33 33 530 45 45 0 0 754 202 208 98 2 818 848 98 2 530 151 1 530 241 51 4448 818 2096 1488 4448 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 0 0 0 0 195 0 0 0 25 0 0 0] 98 0 1040 0 27 1058 2176 1 1136 241 1168 1 1200 51 410 2608 98 16 0 416 98 2 8 1140916352 1025 5296 0 482 2688 0 7 0 0 0 5296 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 121 241 530 201 41 5296 818 2864 98 1 2898 3 1 3 5296 818 2944 98 1 32 5296 818 2992 98 1 98 2 7 7 5296 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 120 0 0 0 160 0 0 0 140 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 241 1200 41 410 2608 98 16 0 416 98 2 8 1140916352 1025 5712 0 482 8 4278190080 0 7 0 0 0 5712 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 81 581 530 201 41 5712 818 2864 98 1 2898 3 1 3 5712 818 2944 98 1 32 5712 818 2992 98 1 98 2 7 7 5712 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 40 0 0 0 34 1 0 0 140 0 0 0 54 1 0 0] 98 0 1040 0 27 1058 1104 81 1136 201 1168 581 1200 41 410 8 ##(Smalltalk.ComboBox)  98 17 0 416 98 2 8 1144063491 1025 6144 590662 2 ##(Smalltalk.ListModel)  202 208 1488 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 6144 0 8 4294903259 459270 ##(Smalltalk.Message)  8 #displayString 98 0 1488 401 754 202 208 98 1 818 848 98 2 530 131 51 530 371 47 6144 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 65 0 0 0 25 0 0 0 250 0 0 0 48 0 0 0] 98 0 1040 0 27 1058 1104 131 1136 371 1168 51 1200 47 410 2256 98 16 0 416 98 2 8 1140850944 1 6592 0 0 0 7 0 0 0 6592 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 21 167 530 71 31 6592 818 928 98 1 8 'User' 6592 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 83 0 0 0 45 0 0 0 98 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 71 1168 167 1200 31 410 2256 98 16 0 416 98 2 8 1140850944 1 6912 0 0 0 7 0 0 0 6912 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 341 247 530 91 31 6912 818 928 98 1 8 'NetLDI	' 6912 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 170 0 0 0 123 0 0 0 215 0 0 0 138 0 0 0] 98 0 1040 0 27 1058 1104 341 1136 91 1168 247 1200 31 410 2608 98 16 0 416 98 2 8 1140916384 1025 7232 0 482 8 4278190080 0 7 0 0 0 7232 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 361 711 530 201 41 7232 818 2864 98 1 2898 3 1 3 7232 818 2944 98 1 32 7232 818 2992 98 1 98 2 7 7 7232 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 99 1 0 0 24 1 0 0 119 1 0 0] 98 0 1040 0 27 1058 1104 361 1136 201 1168 711 1200 41 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 7664 0 0 0 7 0 0 0 7664 0 8 4294903245 1570 8 #login 8 'Login' 1 1 0 0 16 0 0 0 754 202 208 98 3 818 848 98 2 530 481 311 530 201 61 7664 818 8 #isEnabled: 98 1 32 7664 818 928 98 1 8 'Login' 7664 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 0 0 0 155 0 0 0 84 1 0 0 185 0 0 0] 98 0 1040 0 29 1058 1104 481 1136 201 1168 311 1200 61 410 2608 98 16 0 416 98 2 8 1140916352 1025 8080 0 482 2688 0 7 0 0 0 8080 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 121 121 530 201 41 8080 818 2864 98 1 2898 3 1 3 8080 818 2944 98 1 32 8080 818 2992 98 1 98 2 7 7 8080 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 60 0 0 0 160 0 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 121 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 8496 0 0 0 7 0 0 0 8496 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 11 321 530 121 31 8496 818 928 98 1 8 'Developer' 8496 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 160 0 0 0 65 0 0 0 175 0 0 0] 98 0 1040 0 27 1058 1104 11 1136 121 1168 321 1200 31 410 2256 98 16 0 416 98 2 8 1140850944 1 8816 0 0 0 7 0 0 0 8816 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 341 167 530 121 31 8816 818 928 98 1 8 'Password' 8816 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 170 0 0 0 83 0 0 0 230 0 0 0 98 0 0 0] 98 0 1040 0 27 1058 1104 341 1136 121 1168 167 1200 31 410 2608 98 16 0 416 98 2 8 1140916384 1025 9136 0 482 8 4278190080 0 7 0 0 0 9136 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 471 161 530 201 41 9136 818 2864 98 1 2898 3 1 3 9136 818 2944 98 1 32 9136 818 2992 98 1 98 2 7 7 9136 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 235 0 0 0 80 0 0 0 79 1 0 0 100 0 0 0] 98 0 1040 0 27 1058 1104 471 1136 201 1168 161 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 9568 0 0 0 7 0 0 0 9568 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 21 129 530 91 31 9568 818 928 98 1 8 'Host/IP' 9568 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 64 0 0 0 55 0 0 0 79 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 129 1200 31 410 1248 98 25 0 416 98 2 8 1409289036 131137 9888 0 482 8 4278190080 0 7 0 0 0 9888 482 9968 8 4294903147 234 256 1488 234 256 98 6 21551 1115910 ##(Smalltalk.ToolbarIconButton)  21551 0 9888 1 1570 8 #fileNew 8 'New' 1 1 263494 3 ##(Smalltalk.Icon)  0 16 1760 8 'DocumentShell.ico' 0 1714 0 16 0 0 0 0 3 530 33 33 1 21553 10066 21553 0 9888 1 1570 8 #fileOpen 8 'Open' 1 1 10146 0 16 1760 8 'FileOpen.ico' 1808 1714 0 16 0 0 0 0 3 530 33 33 1 21555 10066 21555 0 9888 1 1570 8 #fileSave 8 'Save' 1 1 10146 0 16 1760 8 'FileSave.ico' 1808 1714 0 16 0 0 0 0 3 530 33 33 1 98 4 10080 10224 10352 5010 0 0 9888 3 0 1 234 240 98 6 10448 5 10192 1 10320 3 0 1 0 530 33 33 530 45 45 0 0 754 202 208 98 2 818 848 98 2 530 1 1 530 151 51 9888 818 2096 1488 9888 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 75 0 0 0 25 0 0 0] 98 0 1040 0 27 1058 1104 1 1136 151 1168 1 1200 51 410 2608 98 16 0 416 98 2 8 1140916352 1025 10768 0 482 2688 0 7 0 0 0 10768 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 471 121 530 201 41 10768 818 2864 98 1 2898 3 1 3 10768 818 2944 98 1 32 10768 818 2992 98 1 98 2 7 7 10768 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 235 0 0 0 60 0 0 0 79 1 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 471 1136 201 1168 121 1200 41 410 2256 98 16 0 416 98 2 8 1140850944 1 11184 0 0 0 7 0 0 0 11184 0 8 4294903255 2338 0 0 0 754 202 208 98 2 818 848 98 2 530 21 61 530 91 31 11184 818 928 98 1 8 'Version' 11184 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 30 0 0 0 55 0 0 0 45 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 61 1200 31 410 640 98 14 0 416 98 2 8 1140850695 65 11504 0 482 720 0 7 0 0 0 11504 0 8 4294903245 754 202 208 98 2 818 848 98 2 530 5 91 530 681 121 11504 818 928 98 1 8 'Stone' 11504 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 45 0 0 0 86 1 0 0 105 0 0 0] 98 0 1040 0 27 1058 1104 5 1136 681 1168 91 1200 121 410 2608 98 16 0 416 98 2 8 1140916352 1025 11824 0 482 2688 0 7 0 0 0 11824 0 8 4294903261 2338 0 0 1 754 202 208 98 4 818 848 98 2 530 121 161 530 201 41 11824 818 2864 98 1 2898 3 1 3 11824 818 2944 98 1 32 11824 818 2992 98 1 98 2 7 7 11824 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 80 0 0 0 160 0 0 0 100 0 0 0] 98 0 1040 0 27 1058 1104 121 1136 201 1168 161 1200 41 234 256 98 26 1232 8 'Other Tools' 2592 8 'gemService' 5296 8 'gemHost' 4064 8 'initials' 4448 8 'Edit Tools' 5712 8 'hostUserID' 6144 8 'versionList' 7232 8 'hostPassword' 8080 8 'stoneHost' 9136 8 'password' 9888 8 'File Tools' 10768 8 'stoneName' 11824 8 'userID' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 1570 10112 8 '&New' 9373 1 0 0 0 12578 1 1570 10256 8 '&Open...' 9375 1 0 0 0 12578 1 1570 10384 8 '&Save' 9383 1 0 0 0 12578 1 1570 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 12578 1 1570 8 #fileRevert 8 '&Revert' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 12578 1 1570 8 #exit 8 'E&xit' 17639 1 0 0 0 8 '&File' 0 134217729 0 0 21515 0 0 12530 0 16 98 9 12578 1 1570 4832 8 '&Undo' 9397 1 0 0 0 12578 1 1570 8 #editRedo 8 '&Redo' 9395 1 0 0 0 12866 4097 12578 1 1570 4896 8 'Cu&t' 9393 1 0 0 0 12578 1 1570 4960 8 '&Copy' 9351 1 0 0 0 12578 1 1570 4656 8 '&Paste' 9389 1 0 0 0 12578 1 1570 4768 8 'De&lete' 1629 1 0 0 0 12866 4097 12578 1 1570 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 8 '&Edit' 0 134217729 0 0 21531 0 0 12530 0 16 98 2 12578 1 1570 8 #setDefaultFont 8 'Set Default &Font' 1 1 0 0 0 12578 1 1570 8 #setCodeFont 8 'Set &Code Font' 1 1 0 0 0 8 '&Preferences' 0 134217729 0 0 21537 0 0 12530 0 16 98 7 12578 1 1570 8 #helpContents 8 '&Contents' 1 1 0 0 0 12578 1 1570 8 #helpOnThisTool 8 '&On this Tool' 1249 1 0 0 0 12578 1 1570 8 #helpWhatsThis 8 '&What''s This?' 5345 1 0 0 0 12866 4097 12578 1 1570 8 #openHomePage 8 '&GemStone Home Page' 1025 1 0 0 0 12866 4097 12578 1 1570 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 134217729 0 0 21549 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 10323 10146 0 16 1760 8 'icons\GS32x32.ico' 0 10146 0 16 1760 8 'icons\GS16x16.ico' 0 0 0 1 0 0 754 202 208 98 3 818 848 98 2 530 2879 21 530 701 471 416 818 928 98 1 8 'Jade Login' 416 818 8 #updateMenuBar 1488 416 978 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 253 6 0 0 245 0 0 0] 98 26 6144 8080 10768 11824 9136 5296 2592 4064 9888 4448 1232 5712 7232 7664 9568 3744 6912 3104 2240 3424 11504 624 11184 6592 8816 8496 1040 0 27 )!

resource_Default_view
	"Answer the literal data from which the 'Full view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Full_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 26476545 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  751 591 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 64 410 8 ##(Smalltalk.GroupBox)  98 14 0 416 98 2 8 1140850695 65 624 0 482 8 4294967295 0 7 0 0 0 624 0 8 4294903309 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 11 91 530 341 121 624 818 8 #text: 98 1 8 'Stone' 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 45 0 0 0 175 0 0 0 105 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 11 1090 8 #fixedViewLeft 341 1090 8 #fixedParentTop 91 1090 8 #fixedViewTop 121 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 1232 0 0 0 7 0 0 0 1232 0 8 4294903713 852486 ##(Smalltalk.NullConverter)  0 0 0 754 202 208 98 2 818 848 98 2 530 191 331 530 121 41 1232 818 928 98 1 8 '#task!!' 1232 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 165 0 0 0 155 0 0 0 185 0 0 0] 98 0 1040 0 27 1058 1104 191 1136 121 1168 331 1200 41 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 1584 0 482 8 4278190080 0 7 0 0 0 1584 0 8 4294903717 1330 0 0 1 754 202 208 98 4 818 848 98 2 530 311 291 530 201 41 1584 818 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1584 818 8 #isTextModified: 98 1 32 1584 818 8 #setMarginWidths: 98 1 98 2 7 7 1584 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 0 0 0 145 0 0 0 255 0 0 0 165 0 0 0] 98 0 1040 0 27 1058 1104 311 1136 201 1168 291 1200 41 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1409289036 131137 2096 0 482 8 4278190080 0 7 0 0 0 2096 482 2192 8 4294902993 234 256 98 0 234 256 98 10 32915 853766 ##(Smalltalk.ToolbarButton)  32915 0 2096 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #editCut 8 'Cut' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'EditBar.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 530 193 33 1 32917 2306 32917 0 2096 1 2338 8 #editCopy 8 'Copy' 1 1 0 2416 3 32919 2306 32919 0 2096 1 2338 8 #editPaste 8 'Paste' 1 1 0 2416 5 32921 2306 32921 0 2096 1 2338 8 #editDelete 8 'Delete' 1 1 0 2416 11 32923 2306 32923 0 2096 1 2338 8 #editUndo 8 'Undo' 1 1 0 2416 7 98 6 2320 2544 2608 2672 2736 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 2096 3 0 1 234 240 98 2 2416 1 0 1 0 530 33 33 530 45 45 0 0 754 202 208 98 2 818 848 98 2 530 151 1 530 241 51 2096 818 8 #updateSize 2256 2096 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 0 0 0 0 195 0 0 0 25 0 0 0] 98 0 1040 0 27 1058 1090 8 #fixedPreviousRight 1 1136 241 1168 1 1200 51 410 1600 98 16 0 416 98 2 8 1140916352 1025 3152 0 482 1680 0 7 0 0 0 3152 0 8 4294903717 1330 0 0 1 754 202 208 98 4 818 848 98 2 530 531 121 530 201 41 3152 818 1856 98 1 1890 3 1 3 3152 818 1936 98 1 32 3152 818 1984 98 1 98 2 7 7 3152 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 1 0 0 60 0 0 0 109 1 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 531 1136 201 1168 121 1200 41 410 1600 98 16 0 416 98 2 8 1140916352 1025 3568 0 482 1680 0 7 0 0 0 3568 0 8 4294903717 1330 0 0 1 754 202 208 98 4 818 848 98 2 530 311 251 530 201 41 3568 818 1856 98 1 1890 3 1 3 3568 818 1936 98 1 32 3568 818 1984 98 1 98 2 7 7 3568 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 0 0 0 125 0 0 0 255 0 0 0 145 0 0 0] 98 0 1040 0 27 1058 1104 311 1136 201 1168 251 1200 41 410 1248 98 16 0 416 98 2 8 1140850944 1 3984 0 0 0 7 0 0 0 3984 0 8 4294903713 1330 0 0 0 754 202 208 98 1 818 848 98 2 530 185 383 530 331 91 3984 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 92 0 0 0 191 0 0 0 1 1 0 0 236 0 0 0] 98 0 1040 0 27 1058 1104 185 1136 331 1168 383 1200 91 410 1600 98 16 0 416 98 2 8 1140916352 1025 4256 0 482 1680 0 7 0 0 0 4256 0 8 4294903717 1330 0 0 1 754 202 208 98 4 818 848 98 2 530 311 331 530 201 41 4256 818 1856 98 1 1890 3 1 3 4256 818 1936 98 1 32 4256 818 1984 98 1 98 2 7 7 4256 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 0 0 0 165 0 0 0 255 0 0 0 185 0 0 0] 98 0 1040 0 27 1058 1104 311 1136 201 1168 331 1200 41 410 1248 98 16 0 416 98 2 8 1140850944 1 4672 0 0 0 7 0 0 0 4672 0 8 4294903713 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 21 121 530 91 41 4672 818 928 98 1 8 '!!tcp@' 4672 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 60 0 0 0 55 0 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 91 1168 121 1200 41 410 1248 98 16 0 416 98 2 8 1140850944 1 4992 0 0 0 7 0 0 0 4992 0 8 4294903713 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 191 391 530 111 41 4992 818 928 98 1 8 'User ID:' 4992 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 195 0 0 0 150 0 0 0 215 0 0 0] 98 0 1040 0 27 1058 1104 191 1136 111 1168 391 1200 41 410 1248 98 16 0 416 98 2 8 1140850944 1 5312 0 0 0 7 0 0 0 5312 0 8 4294903713 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 365 163 530 151 41 5312 818 928 98 1 8 'Password:' 5312 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 182 0 0 0 81 0 0 0 1 1 0 0 101 0 0 0] 98 0 1040 0 27 1058 1104 365 1136 151 1168 163 1200 41 410 1600 98 16 0 416 98 2 8 1140916352 1025 5632 0 482 8 4278190080 0 7 0 0 0 5632 0 8 4294903717 1330 0 0 1 754 202 208 98 4 818 848 98 2 530 311 391 530 201 41 5632 818 1856 98 1 1890 3 1 3 5632 818 1936 98 1 32 5632 818 1984 98 1 98 2 7 7 5632 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 0 0 0 195 0 0 0 255 0 0 0 215 0 0 0] 98 0 1040 0 27 1058 1104 311 1136 201 1168 391 1200 41 410 1248 98 16 0 416 98 2 8 1140850944 1 6064 0 0 0 7 0 0 0 6064 0 8 4294903713 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 11 61 530 331 41 6064 818 928 98 1 8 'GemStone/Smalltalk Version:' 6064 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 30 0 0 0 170 0 0 0 50 0 0 0] 98 0 1040 0 27 1058 1104 11 1136 331 1168 61 1200 41 410 1600 98 16 0 416 98 2 8 1140916352 1025 6384 0 482 1680 0 7 0 0 0 6384 0 8 4294903717 1330 0 0 1 754 202 208 98 4 818 848 98 2 530 141 161 530 201 41 6384 818 1856 98 1 1890 3 1 3 6384 818 1936 98 1 32 6384 818 1984 98 1 98 2 7 7 6384 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 80 0 0 0 170 0 0 0 100 0 0 0] 98 0 1040 0 27 1058 1104 141 1136 201 1168 161 1200 41 410 1248 98 16 0 416 98 2 8 1140850944 1 6800 0 0 0 7 0 0 0 6800 0 8 4294903713 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 191 291 530 121 41 6800 818 928 98 1 8 '#netldi:' 6800 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 145 0 0 0 155 0 0 0 165 0 0 0] 98 0 1040 0 27 1058 1104 191 1136 121 1168 291 1200 41 410 1248 98 16 0 416 98 2 8 1140850944 1 7120 0 0 0 7 0 0 0 7120 0 8 4294903713 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 545 233 530 181 101 7120 818 928 98 1 8 'Developer''s
Name for
Monticello:' 7120 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 16 1 0 0 116 0 0 0 106 1 0 0 166 0 0 0] 98 0 1040 0 27 1058 1104 545 1136 181 1168 233 1200 101 410 1600 98 16 0 416 98 2 8 1140916352 1025 7440 0 482 1680 0 7 0 0 0 7440 0 8 4294903717 1330 0 0 1 754 202 208 98 4 818 848 98 2 530 141 121 530 201 41 7440 818 1856 98 1 1890 3 1 3 7440 818 1936 98 1 32 7440 818 1984 98 1 98 2 7 7 7440 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 60 0 0 0 170 0 0 0 80 0 0 0] 98 0 1040 0 27 1058 1104 141 1136 201 1168 121 1200 41 410 1248 98 16 0 416 98 2 8 1140850944 1 7856 0 0 0 7 0 0 0 7856 0 8 4294903713 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 365 123 530 151 41 7856 818 928 98 1 8 'User ID:' 7856 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 182 0 0 0 61 0 0 0 1 1 0 0 81 0 0 0] 98 0 1040 0 27 1058 1104 365 1136 151 1168 123 1200 41 410 8 ##(Smalltalk.ComboBox)  98 17 0 416 98 2 8 1144063491 1025 8176 590662 2 ##(Smalltalk.ListModel)  202 208 2256 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 8176 0 8 4294903719 459270 ##(Smalltalk.Message)  8 #displayString 98 0 2256 401 754 202 208 98 1 818 848 98 2 530 361 51 530 371 47 8176 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 25 0 0 0 109 1 0 0 48 0 0 0] 98 0 1040 0 27 1058 1104 361 1136 371 1168 51 1200 47 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 8624 0 0 0 7 0 0 0 8624 0 8 4294903309 2338 8 #login 8 'Login' 1 1 0 0 16 0 0 0 754 202 208 98 3 818 848 98 2 530 531 421 530 201 61 8624 818 8 #isEnabled: 98 1 32 8624 818 928 98 1 8 'Login' 8624 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 1 0 0 210 0 0 0 109 1 0 0 240 0 0 0] 98 0 1040 0 29 1058 1104 531 1136 201 1168 421 1200 61 410 1248 98 16 0 416 98 2 8 1140850944 1 9040 0 0 0 7 0 0 0 9040 0 8 4294903713 1330 0 0 0 754 202 208 98 1 818 848 98 2 530 185 243 530 331 131 9040 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 92 0 0 0 121 0 0 0 1 1 0 0 186 0 0 0] 98 0 1040 0 27 1058 1104 185 1136 331 1168 243 1200 131 410 2112 98 25 0 416 98 2 8 1409289036 131137 9312 0 482 8 4278190080 0 7 0 0 0 9312 482 9392 8 4294902993 234 256 2256 234 256 98 6 32913 1115910 ##(Smalltalk.ToolbarIconButton)  32913 0 9312 1 2338 8 #fileSave 8 'Save' 1 1 263494 3 ##(Smalltalk.Icon)  0 16 2448 8 'FileSave.ico' 2496 2402 0 16 0 0 0 0 3 530 33 33 1 32909 9490 32909 0 9312 1 2338 8 #fileNew 8 'New' 1 1 9570 0 16 2448 8 'DocumentShell.ico' 0 2402 0 16 0 0 0 0 3 530 33 33 1 32911 9490 32911 0 9312 1 2338 8 #fileOpen 8 'Open' 1 1 9570 0 16 2448 8 'FileOpen.ico' 2496 2402 0 16 0 0 0 0 3 530 33 33 1 98 4 9648 9776 9504 2818 0 0 9312 3 0 1 234 240 98 6 9616 5 9744 1 9872 3 0 1 0 530 33 33 530 45 45 0 0 754 202 208 98 2 818 848 98 2 530 1 1 530 151 51 9312 818 3040 2256 9312 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 75 0 0 0 25 0 0 0] 98 0 1040 0 27 1058 1104 1 1136 151 1168 1 1200 51 410 1248 98 16 0 416 98 2 8 1140850944 1 10192 0 0 0 7 0 0 0 10192 0 8 4294903713 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 21 161 530 121 41 10192 818 928 98 1 8 '#server!!' 10192 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 80 0 0 0 70 0 0 0 100 0 0 0] 98 0 1040 0 27 1058 1104 21 1136 121 1168 161 1200 41 410 1248 98 16 0 416 98 2 8 1140850944 1 10512 0 0 0 7 0 0 0 10512 0 8 4294903713 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 191 431 530 121 41 10512 818 928 98 1 8 'Password:' 10512 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 215 0 0 0 155 0 0 0 235 0 0 0] 98 0 1040 0 27 1058 1104 191 1136 121 1168 431 1200 41 410 2112 98 25 0 416 98 2 8 1140853580 131137 10832 0 482 8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 10832 482 10912 8 4294902993 234 256 2256 234 256 98 4 32929 1246982 ##(Smalltalk.ToolbarSystemButton)  32929 0 10832 1 2338 8 #getHelpOn 8 'Get help on widget' 1 1 0 1 23 32931 2306 32931 0 10832 1 2338 8 #openHelp 8 'Jade Help' 1 1 0 2402 0 16 2448 8 'Tools.bmp' 2496 0 7 530 1857 33 71 98 2 11120 11184 234 240 98 4 1 1 11248 31 0 1 0 530 33 33 530 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 754 202 208 98 2 818 848 98 2 530 391 1 530 349 51 10832 818 3040 2256 10832 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 195 0 0 0 0 0 0 0 113 1 0 0 25 0 0 0] 98 0 1040 0 27 1058 3120 1 1090 8 #fixedParentRight 1 1168 1 1200 51 410 1248 98 16 0 416 98 2 8 1140850944 1 11632 0 0 0 7 0 0 0 11632 0 8 4294903713 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 191 251 530 111 41 11632 818 928 98 1 8 '!!tcp@' 11632 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 125 0 0 0 150 0 0 0 145 0 0 0] 98 0 1040 0 27 1058 1104 191 1136 111 1168 251 1200 41 410 1600 98 16 0 416 98 2 8 1140916384 1025 11952 0 482 8 4278190080 0 7 0 0 0 11952 0 8 4294903717 1330 0 0 1 754 202 208 98 4 818 848 98 2 530 531 161 530 201 41 11952 818 1856 98 1 1890 3 1 3 11952 818 1936 98 1 32 11952 818 1984 98 1 98 2 7 7 11952 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 1 0 0 80 0 0 0 109 1 0 0 100 0 0 0] 98 0 1040 0 27 1058 1104 531 1136 201 1168 161 1200 41 410 1600 98 16 0 416 98 2 8 1140916352 1025 12384 0 482 8 4278190080 0 7 0 0 0 12384 0 8 4294903717 1330 0 0 1 754 202 208 98 3 818 848 98 2 530 541 341 530 171 41 12384 818 1856 98 1 1890 3 1 3 12384 818 1936 98 1 32 12384 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 1 0 0 170 0 0 0 99 1 0 0 190 0 0 0] 98 0 1040 0 27 1058 1104 541 1136 171 1168 341 1200 41 410 640 98 14 0 416 98 2 8 1140850695 65 12768 0 482 720 0 7 0 0 0 12768 0 8 4294903309 754 202 208 98 2 818 848 98 2 530 15 213 530 511 271 12768 818 928 98 1 8 'Gem' 12768 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 106 0 0 0 6 1 0 0 241 0 0 0] 98 0 1040 0 27 1058 1104 15 1136 511 1168 213 1200 271 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1409286144 393217 13088 0 482 8 4278190080 0 7 0 0 0 13088 656390 ##(Smalltalk.GridLayout)  1 3 1 1 234 256 98 4 410 8 ##(Smalltalk.RadioButton)  98 16 0 13088 98 2 8 1141055497 1 13264 721990 2 ##(Smalltalk.ValueHolder)  0 0 1376774 ##(Smalltalk.PluggableSearchPolicy)  8386 8 #= 98 0 8386 8 #hash 98 0 32 482 13184 0 7 0 0 0 13264 0 8 4294903309 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 1 1 530 157 39 13264 818 928 98 1 8 'Guest' 13264 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 78 0 0 0 19 0 0 0] 98 0 1040 0 27 8 'guest' 410 13280 98 16 0 13088 98 2 8 1140924425 1 13776 13346 0 0 13378 8386 13424 98 0 8386 13472 98 0 32 482 13184 0 7 0 0 0 13776 0 8 4294903309 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 1 39 530 157 39 13776 818 928 98 1 8 'OS User' 13776 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 78 0 0 0 38 0 0 0] 98 0 1040 0 27 8 'member' 0 754 202 208 98 1 818 848 98 2 530 21 391 530 161 81 13088 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 195 0 0 0 90 0 0 0 235 0 0 0] 98 2 13264 13776 1040 0 27 1058 1104 21 1136 161 1168 391 1200 81 410 1600 98 16 0 416 98 2 8 1140916384 1025 14384 0 482 8 4278190080 0 7 0 0 0 14384 0 8 4294903717 1330 0 0 1 754 202 208 98 4 818 848 98 2 530 311 431 530 201 41 14384 818 1856 98 1 1890 3 1 3 14384 818 1936 98 1 32 14384 818 1984 98 1 98 2 7 7 14384 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 0 0 0 215 0 0 0 255 0 0 0 235 0 0 0] 98 0 1040 0 27 1058 1104 311 1136 201 1168 431 1200 41 410 13104 98 15 0 416 98 2 8 1140850688 393217 14816 0 482 8 4278190080 0 7 0 0 0 14816 13202 5 3 1 1 234 256 98 4 410 13280 98 16 0 14816 98 2 8 1140924425 1 14960 13346 0 0 13378 8386 13424 98 0 8386 13472 98 0 32 482 14896 0 7 0 0 0 14960 0 8 4294903309 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 1 59 530 157 59 14960 818 928 98 1 8 'Remote' 14960 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 29 0 0 0 78 0 0 0 58 0 0 0] 98 0 1040 0 27 8 'remote' 410 13280 98 16 0 14816 98 2 8 1141055497 1 15392 13346 0 0 13378 8386 13424 98 0 8386 13472 98 0 32 482 14896 0 7 0 0 0 15392 0 8 4294903309 1330 0 0 0 754 202 208 98 2 818 848 98 2 530 1 1 530 157 59 15392 818 928 98 1 8 'Linked' 15392 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 78 0 0 0 29 0 0 0] 98 0 1040 0 27 8 'linked' 0 754 202 208 98 1 818 848 98 2 530 21 251 530 161 121 14816 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 125 0 0 0 90 0 0 0 185 0 0 0] 98 2 15392 14960 1040 0 27 1058 1104 21 1136 161 1168 251 1200 121 234 256 98 36 6384 8 'stoneName' 3568 8 'gemHost' 12384 8 'initials' 14384 8 'hostPassword' 11952 8 'password' 9040 8 'linkedText' 10832 8 'Other Tools' 4256 8 'gemTask' 14816 8 'gemType' 2096 8 'Edit Tools' 13088 8 'loginType' 1584 8 'gemService' 3152 8 'userID' 9312 8 'File Tools' 5632 8 'hostUserID' 3984 8 'loginText' 8176 8 'versionList' 7440 8 'stoneHost' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 2338 9680 8 '&New' 9373 1 0 0 0 16418 1 2338 9808 8 '&Open...' 9375 1 0 0 0 16418 1 2338 9536 8 '&Save' 9383 1 0 0 0 16418 1 2338 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 16418 1 2338 8 #fileRevert 8 '&Revert' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 16418 1 2338 8 #exit 8 'E&xit' 17639 1 0 0 0 8 '&File' 0 134217729 0 0 32997 0 0 16370 0 16 98 9 16418 1 2338 2768 8 '&Undo' 9397 1 0 0 0 16418 1 2338 8 #editRedo 8 '&Redo' 9395 1 0 0 0 16706 4097 16418 1 2338 2368 8 'Cu&t' 9393 1 0 0 0 16418 1 2338 2576 8 '&Copy' 9351 1 0 0 0 16418 1 2338 2640 8 '&Paste' 9389 1 0 0 0 16418 1 2338 2704 8 'De&lete' 1629 1 0 0 0 16706 4097 16418 1 2338 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 8 '&Edit' 0 134217729 0 0 33013 0 0 16370 0 16 98 2 16418 1 2338 8 #setDefaultFont 8 'Set Default &Font' 1 1 0 0 0 16418 1 2338 8 #setCodeFont 8 'Set &Code Font' 1 1 0 0 0 8 '&Preferences' 0 134217729 0 0 33019 0 0 16370 0 16 98 7 16418 1 2338 8 #helpContents 8 '&Contents' 1 1 0 0 0 16418 1 2338 8 #helpOnThisTool 8 '&On this Tool' 1249 1 0 0 0 16418 1 2338 8 #helpWhatsThis 8 '&What''s This?' 5345 1 0 0 0 16706 4097 16418 1 2338 8 #openHomePage 8 '&GemStone Home Page' 1025 1 0 0 0 16706 4097 16418 1 2338 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 134217729 0 0 33031 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 13859 9570 0 16 2448 8 'icons\GS32x32.ico' 0 9570 0 16 2448 8 'icons\GS16x16.ico' 0 0 0 1 0 0 754 202 208 98 3 818 848 98 2 530 5119 21 530 751 591 416 818 928 98 1 8 'Jade Login' 416 818 8 #updateMenuBar 2256 416 978 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 118 11 0 0 49 1 0 0] 98 32 8176 9312 2096 10832 9040 3984 7440 6384 14816 3568 1584 4256 13088 5632 14384 3152 11952 12384 8624 4672 10192 6800 1232 7856 5312 11632 624 12768 6064 4992 10512 7120 1040 0 27 )!

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

