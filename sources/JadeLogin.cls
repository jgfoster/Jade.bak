"Filed out from Dolphin Smalltalk 7"!

Model subclass: #JadeLogin
	instanceVariableNames: 'debugPath gemHost gemNRS gemService gemTask gemType gemVersion gsPassword gsUserID hostPassword hostUserID initials loginType stoneHost stoneName stoneNRS'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeLogin guid: (GUID fromString: '{585bd13e-3a03-47f3-b78d-5c29ef797913}')!
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
		setAttribute: 'gemType' 		value: gemType;
		setAttribute: 'gemHost' 		value: gemHost;
		setAttribute: 'gemService' 		value: gemService;
		setAttribute: 'gemNRS' 		value: gemNRS;
		setAttribute: 'hostUserID' 		value: hostUserID;
		setAttribute: 'hostPassword' 	value: hostPassword;
		setAttribute: 'gsUserID' 		value: gsUserID;
		setAttribute: 'gsPassword' 		value: gsPassword;
		setAttribute: 'initials'			value: initials;
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

	gemHost ifNil: [gemHost := ''].
	gemHost isEmpty ifTrue: [^'localhost'].
	^gemHost
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

	gemTask ifNil: [gemTask := ''].
	gemTask isEmpty ifTrue: [^'gemnetobject'].
	^gemTask
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

	gemVersion ifNil: [gemVersion := ''].
	gemVersion isEmpty ifTrue: [^Gcilw6x displayName].
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

	gsPassword ifNil: [gsPassword := ''].
	gsPassword isEmpty ifTrue: [^'swordfish'].
	^gsPassword.
!

gsPassword: aString

	self gsPassword = aString ifTrue: [^self].
	gsPassword := aString.
	self trigger: #changed.
!

gsUserID

	gsUserID ifNil: [gsUserID := ''].
	gsUserID isEmpty ifTrue: [^'DataCurator'].
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
	gemVersion	:= (element getAttribute: 'gemVersion'	) ifNil: [''].
	stoneHost		:= (element getAttribute: 'stoneHost'		) ifNil: [''].
	stoneName	:= (element getAttribute: 'stoneName'	) ifNil: [''].
	stoneNRS		:= (element getAttribute: 'stoneNRS'		) ifNil: [''].
	gemType		:= (element getAttribute: 'gemType'		) asSymbol.
	gemHost		:= (element getAttribute: 'gemHost'		) ifNil: [''].
	gemService	:= (element getAttribute: 'gemService'	) ifNil: [''].
	gemNRS		:= (element getAttribute: 'gemNRS'		) ifNil: [''].
	loginType		:= (element getAttribute: 'loginType'		) ifNil: [''].
	loginType 		:= loginType isNil ifTrue: [#'guest'] ifFalse: [loginType asSymbol].
	hostUserID	:= (element getAttribute: 'hostUserID'	) ifNil: [''].
	hostPassword	:= (element getAttribute: 'hostPassword'	) ifNil: [''].
	gsUserID		:= (element getAttribute: 'gsUserID'		) ifNil: [''].
	gsPassword	:= (element getAttribute: 'gsPassword'	) ifNil: [''].
	initials		:= (element getAttribute: 'initials'		) ifNil: [''].
!

initials

	initials ifNil: [initials := ''].
	initials isEmpty ifTrue: [^'Jade'].
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
		debugPath: self debugPath!

loginType
	^loginType.
!

loginType: anObject

	loginType 		:= anObject.
	hostUserID 	:= ''.
	hostPassword 	:= ''.
!

saveDefault

	| path |
	path := self class defaultPath.
	[
		(FileStream write: path)
			nextPutAll: self asXML;
			close.
	] on: Error do: [:ex | 
		MessageBox errorMsg: 'JadeLogin>>#''saveDefault'' resulted in ' , ex description.
		ex return.
	].
!

stoneHost

	stoneHost ifNil: [stoneHost := ''].
	stoneHost isEmpty ifTrue: [^'localhost'].
	^stoneHost!

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
	gemVersion 	:= aJadeLogin gemVersion.
	stoneHost 	:= aJadeLogin stoneHost.
	stoneName 	:= aJadeLogin stoneName.
	stoneNRS 		:= aJadeLogin stoneNRS.
	gemType 		:= aJadeLogin gemType.
	gemHost 		:= aJadeLogin gemHost.
	gemService 	:= aJadeLogin gemService.
	gemTask 		:= aJadeLogin gemTask.
	gemNRS 		:= aJadeLogin gemNRS.
	loginType		:= aJadeLogin loginType.
	hostUserID 	:= aJadeLogin hostUserID.
	hostPassword 	:= aJadeLogin hostPassword.
	gsUserID 		:= aJadeLogin gsUserID.
	gsPassword 	:= aJadeLogin gsPassword.
	initials		:= aJadeLogin initials.
	debugPath		:= aJadeLogin debugPath.
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

!JadeLogin class methodsFor!

defaultPath

	| regKey appData |
	regKey := RegKey userRoot at: 'Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders' ifAbsent: [nil].
	appData := regKey valueAt: 'AppData'.
	(appData endsWith: ' Dat') ifTrue: [appData := appData , 'a'].		"Wine seems to cut off a character"
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

	^[
		super new
			initializeFromXML: aString;
			yourself.
	] on: Error do: [:ex | 
		super new
			initializeFromXML: self defaultXML;
			yourself.
	].!

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
"
	JadeLogin sample.
"
	^self fromXML: self defaultXML.
! !
!JadeLogin class categoriesFor: #defaultPath!public! !
!JadeLogin class categoriesFor: #defaultXML!public! !
!JadeLogin class categoriesFor: #fromXML:!public! !
!JadeLogin class categoriesFor: #new!public! !
!JadeLogin class categoriesFor: #publishedEventsOfInstances!public! !
!JadeLogin class categoriesFor: #sample!public! !

