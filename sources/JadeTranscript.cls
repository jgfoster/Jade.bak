"Filed out from Dolphin Smalltalk 7"!

JadeWorkspace subclass: #JadeTranscript
	instanceVariableNames: 'allSessionsPresenter historyPresenter mySessionPresenter stoneInfoPresenter toolbarPresenter transcriptPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeTranscript guid: (GUID fromString: '{7d871bc4-1338-4ba1-af6a-c1edd936d683}')!
JadeTranscript comment: ''!
!JadeTranscript categoriesForClass!Unclassified! !
!JadeTranscript methodsFor!

autocompletion

	JadeAutocompletationConfigurationShell show.
!

browseMonticelloRepositories

	self jadeBrowseMonticello.
!

createComponents

	super createComponents.
	allSessionsPresenter	:= self add: AllSessionsPresenter		new name: 'All Sessions'.
	historyPresenter	:= self add: TextPresenter				new name: 'history'.
	mySessionPresenter	:= self add: MySessionPresenter			new name: 'My Session'.
	stoneInfoPresenter	:= self add: StoneInfoPresenter			new name: 'Stone'.
	toolbarPresenter	:= self add: JadeToolsToolBarPresenter	new name: 'Toolbar'.
	transcriptPresenter	:= self add: TranscriptPresenter			new name: 'Transcript'.
!

createSchematicWiring

	super createSchematicWiring.
	(self view viewNamed: 'cardContainer') when: #'currentCardChanged' send: #'currentCardChanged' to: self.
!

currentCardChanged

	| currentCard |
	currentCard := (self view viewNamed: 'cardContainer') currentCard.
	(currentCard isKindOf: TextEdit) 	ifTrue: [currentCard setFocus].
	(currentCard name = 'codePane')		ifTrue: [[(currentCard viewNamed: 'document') setFocus] postToInputQueue. ^self].
	(currentCard name = 'history')		ifTrue: [historyPresenter value isEmpty ifTrue: [historyPresenter value: (gciSession executeString: 'DbfHistory')]. ^self].
	(currentCard name = 'My Session')	ifTrue: [mySessionPresenter fillSessionInfo. ^self].
	(currentCard name = 'Stone')		ifTrue: [stoneInfoPresenter fillSessionInfo. ^self].!

defaultFileExtension

	^'gs'.
!

describeErrorNumber

	| string result |
	(string := Prompter prompt: 'Enter GemStoneError number:') isNil ifTrue: [^self].
	(string allSatisfy: [:each | each isDigit]) ifFalse: [
		MessageBox warning: 'Invalid number!!'.
		^self.
	].
	result := gciSession 
		serverPerform: #'descriptionOfErrorNumber:'
		with: string asNumber.
	MessageBox 
		notify: result
		caption: 'GemStoneError ' , string.
!

fileOutJadeServer

	| path string file index version |
	version := self gciSession serverPerform: #'version'.
	path := FileSaveDialog new
		caption: 'File Out JadeServer';
		fileTypes: self fileTypes;
		defaultExtension: self defaultFileExtension;
		value: gciSession _library jadeServerClass name , '-' , version , '.gs';
		overwritePrompt;
		showModal.
	path isNil ifTrue: [^self].
	string := environment printString , ' fileOutJadeServer' , Character tab asString.
	string := self gciSession encode: string.
	string := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: string.
	string := self gciSession decode: string.
	index := string indexOf: Character lf.
	string := string copyFrom: index + 1 to: string size.
	string := string copyReplaceAll: 'poolDictionaries: { UserGlobals . Globals }' with: 'poolDictionaries: { }'.
	string := string copyReplaceAll: '(class not in your dictionaries)' with: 'Globals'.
	file := FileStream write: path mode: #'truncate' check: false type: #'ansi'.
	[
		file nextPutAll: string.
	] ensure: [
		file close.
	].
!

fileTypes

	^Array
		with: #('GemStone Files (*.gs)' '*.gs')
		with: #('Smalltalk Files (*.st)' '*.st')
		with: FileDialog allFilesType.
!

gciSession: aGciSession

	super gciSession: aGciSession.
	mySessionPresenter	gciSession: aGciSession.
	allSessionsPresenter	gciSession: aGciSession.
	stoneInfoPresenter	gciSession: aGciSession.
	toolbarPresenter	gciSession: aGciSession.
	transcriptPresenter	gciSession: aGciSession.
!

hasClassSideSelected
	^false!

inspectOop

	| string result |
	(string := Prompter prompt: 'Enter OOP Number:') isNil ifTrue: [^self].
	(string allSatisfy: [:each | each isDigit]) ifFalse: [
		MessageBox warning: 'Invalid number!!'.
		^self.
	].
	result := gciSession executeString: 'Object _objectForOop: ' , string.
	JadeInspector showOn: result session: gciSession.
!

methodsChanged24

| string result |
	
	result := gciSession executeString: '| result methods|
		result := MethodVersionHistory uniqueInstance methodsNewerThan: (DateTime now subtractDays: 1 ).
		methods := IdentitySet new.
		result do: [:e | e asCompiledMethod ifNotNil: [:m | methods add: m]].
		methods asArray'.
(JadeMethodListBrowser showOnSession: self gciSession) primaryPresenter
		browseMethods: result;
		yourself.
	"JadeInspector showOn: result session: gciSession."
!

methodsChangedDays

| string result |
	(string := Prompter prompt: 'Enter number of days:') isNil ifTrue: [^self].	
	result := gciSession executeString: '| result methods|
		result := MethodVersionHistory uniqueInstance methodsNewerThan: (DateTime now subtractDays: ' , string , ' ).
		methods := IdentitySet new.
		result do: [:e | e asCompiledMethod ifNotNil: [:m | methods add: m]].
		methods asArray'.
	(JadeMethodListBrowser showOnSession: self gciSession) primaryPresenter
		browseMethods: result;
		yourself.

!

onCloseRequested: boolValueHolder
 
	| answer flag |
	super onCloseRequested: boolValueHolder.
	boolValueHolder value ifFalse: [^self].
	gciSession isNil ifTrue: [^self].
	gciSession isValidSession ifFalse: [^self].
	gciSession logoutRequested ifFalse: [
		boolValueHolder value: false.
		^self.
	].
	flag := [
		gciSession serverPerform: #hasDirtyObjects.
	] on: Error do: [:ex | 		"https://github.com/jgfoster/Jade/issues/170"
		ex return: false.
	].
	flag ifTrue: [
		answer := MessageBox new
			caption: 'Close Requested';
			confirmOrCancel: 'Commit current transaction?'.
		boolValueHolder value: (#(#yes #no) includes: answer).
		answer = #yes ifTrue: [gciSession commit].
		boolValueHolder value ifFalse: [^self].
	].
	"We are going to close, so do some things first..."
	"self view hide."
	allSessionsPresenter aboutToClose.
	gciSession notNil ifTrue: [
		| temp |
		temp := gciSession.		"A logout will trigger closing all open windows, including this one."
		gciSession := nil.
		temp forceLogout.
	].
!

onIdleEntered

	GciLibrary activeLibrary gciCallInProgress ifFalse: [
		Cursor current ifNotNil: [
			[Cursor current: nil] forkAt: Processor userBackgroundPriority.
		].
	].
	^super onIdleEntered!

onViewOpened

	super onViewOpened.
	self view position: 95 @ 35.
	codePane view ensureVisible.!

queryCommand: aCommandQuery

	aCommandQuery command == #'stopHeartbeat' ifTrue: [aCommandQuery isEnabled: (self gciSession notNil and: [self gciSession hasHeartbeat]). ^true].
	aCommandQuery command == #'profStef' ifTrue: [aCommandQuery isEnabled: (self gciSession notNil and: [self gciSession isProfStefAvailable]). ^true].
	^super queryCommand: aCommandQuery.
!

resetCursor

	[Cursor current: nil] forkAt: Processor userBackgroundPriority.
!

sleepAndCommit

	[
		gciSession serverPerform: #'sleepAndCommit'.
	] on: GsSoftBreak , GsHardBreak do: [:ex | 
		ex terminateProcess.
	].
!

stopHeartbeat

	self gciSession stopHeartbeat.
!

updateCaption

	gciSession ifNil: [^self].
	self caption: (gciSession titleBarFor: 'Transcript').
! !
!JadeTranscript categoriesFor: #autocompletion!event handling!public! !
!JadeTranscript categoriesFor: #browseMonticelloRepositories!event handling!public! !
!JadeTranscript categoriesFor: #createComponents!public! !
!JadeTranscript categoriesFor: #createSchematicWiring!public! !
!JadeTranscript categoriesFor: #currentCardChanged!public! !
!JadeTranscript categoriesFor: #defaultFileExtension!event handling!public! !
!JadeTranscript categoriesFor: #describeErrorNumber!event handling!public! !
!JadeTranscript categoriesFor: #fileOutJadeServer!event handling!public! !
!JadeTranscript categoriesFor: #fileTypes!event handling!public! !
!JadeTranscript categoriesFor: #gciSession:!private! !
!JadeTranscript categoriesFor: #hasClassSideSelected!public! !
!JadeTranscript categoriesFor: #inspectOop!event handling!public! !
!JadeTranscript categoriesFor: #methodsChanged24!event handling!public! !
!JadeTranscript categoriesFor: #methodsChangedDays!event handling!public! !
!JadeTranscript categoriesFor: #onCloseRequested:!event handling!public! !
!JadeTranscript categoriesFor: #onIdleEntered!event handling!public! !
!JadeTranscript categoriesFor: #onViewOpened!public!updating! !
!JadeTranscript categoriesFor: #queryCommand:!event handling!public! !
!JadeTranscript categoriesFor: #resetCursor!event handling!public! !
!JadeTranscript categoriesFor: #sleepAndCommit!event handling!public! !
!JadeTranscript categoriesFor: #stopHeartbeat!event handling!public! !
!JadeTranscript categoriesFor: #updateCaption!public! !

!JadeTranscript class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 549 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1140850688 131073 608 nil 480 nil 7 nil nil nil 608 655878 ##(Smalltalk.CardLayout) 138 144 34 6 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Transcript' 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 7 1 16 8 'doIt' 8 'CardLabel text: ''Transcript'' iconBlock: [Icon fromId: ''TranscriptShell.ico'']' 8 #[45 30 35 113 47 32 180 106 195 105] 983558 ##(Smalltalk.VariableBinding) #CardLabel 768 800 930 #Icon ##(Smalltalk.Icon) 8 'TranscriptShell.ico' #fromId: #text:iconBlock: 832 11 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140916224 131073 992 nil 480 nil 5 nil nil nil 992 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.TranscriptPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 9 53 514 1153 573 992 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 8 #() 514 193 193 nil 27 738 770 8 'History' 818 0 nil 850 7 1 16 8 'doIt' 8 'CardLabel text: ''History'' iconBlock: [Icon fromId: ''MethodHistory.ico'']' 8 #[45 30 35 113 47 32 180 106 195 105] 944 1344 960 8 'MethodHistory.ico' #fromId: #text:iconBlock: 1360 11 1 nil nil 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 608 34 2 8 1143017796 1025 1456 nil 480 nil 5 nil nil nil 1456 nil 8 1949866667 852486 ##(Smalltalk.NullConverter) nil nil 11 1090 138 144 34 2 1154 #createAt:extent: 34 2 514 9 53 514 1153 573 1456 1154 #setMarginWidths: 34 1 8 #(3 3) 1456 1234 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 8 #() 1296 nil 27 738 770 8 'Stone' 818 0 nil 850 7 1 16 8 'doIt' 8 'CardLabel text: ''Stone'' iconBlock: [Icon fromFile: ''icons\GS32.ico'']' 8 #[45 30 35 113 47 32 180 106 195 105] 944 1808 960 8 'icons\GS32.ico' #fromFile: #text:iconBlock: 1824 11 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 1920 nil nil nil 5 nil nil nil 1920 1058 ##(Smalltalk.StoneInfoPresenter) #resource_Default_view nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 514 9 53 514 1153 573 1920 1234 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 1280 1296 nil 27 738 770 8 'My Session' 818 0 nil 850 7 1 16 8 'doIt' 8 'CardLabel text: ''My Session'' iconBlock: [Icon fromId: ''SMALLTALKOPTIONSFOLDER.ICO'']' 8 #[45 30 35 113 47 32 180 106 195 105] 944 2176 960 8 'SMALLTALKOPTIONSFOLDER.ICO' #fromId: #text:iconBlock: 2192 11 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 2288 nil nil nil 5 nil nil nil 2288 1058 ##(Smalltalk.MySessionPresenter) #resource_Default_view nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 514 9 53 514 1153 573 2288 1234 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 1280 1296 nil 27 738 770 8 'All Sessions' 818 0 nil 850 7 1 16 8 'doIt' 8 'CardLabel text: ''All Sessions'' iconBlock: [Icon fromId: ''ListView.ico'']' 8 #[45 30 35 113 47 32 180 106 195 105] 944 2544 960 8 'ListView.ico' #fromId: #text:iconBlock: 2560 11 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 2656 nil nil nil 5 nil nil nil 2656 1058 ##(Smalltalk.AllSessionsPresenter) #resource_Default_view nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 514 9 53 514 1153 573 2656 1234 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 1280 1296 nil 27 738 770 8 'Workspace' 818 0 nil 850 7 1 16 8 'doIt' 8 'CardLabel text: ''Workspace'' iconBlock: [Icon fromId: ''SmalltalkWorkspace.ico'']' 8 #[45 30 35 113 47 32 180 106 195 105] 944 2912 960 8 'SmalltalkWorkspace.ico' #fromId: #text:iconBlock: 2928 11 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 3024 nil 480 nil 7 nil nil nil 3024 1058 ##(Smalltalk.TranscriptPresenter) #resource_Default_view nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 514 9 53 514 1153 573 3024 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 1280 1296 nil 27 3024 170 192 34 12 3024 8 'codePane' 2656 8 'All Sessions' 1920 8 'Stone' 2288 8 'My Session' 1456 8 'history' 992 8 'Transcript' nil 410 ##(Smalltalk.TabView) 34 23 nil 608 34 2 8 1140916736 1 3376 590662 2 ##(Smalltalk.ListModel) 138 144 34 6 784 1328 1792 2160 2528 2896 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 721158 ##(Smalltalk.SystemColor) 31 nil 1 nil nil nil 3376 nil 8 1949556446 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #smallIcons 1090 138 144 34 3 1154 #createAt:extent: 34 2 514 1 1 514 1169 633 3376 1154 #basicSelectionsByIndex: 34 1 8 #(6) 3376 1154 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 3376 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 60 1 0 0] 8 #() 1296 nil 27 1090 138 144 34 1 1154 #createAt:extent: 34 2 514 1 51 514 1169 633 608 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 72 2 0 0 85 1 0 0] 34 7 992 1456 1920 2288 2656 3024 3376 1296 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 4050 #fixedParentRight 1 4050 #fixedParentTop 51 4050 #fixedParentBottom 1 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 4128 nil nil nil 7 nil nil nil 4128 1058 ##(Smalltalk.JadeToolsToolBarPresenter) #resource_Default_view nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 514 1 1 514 1169 51 4128 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 25 0 0 0] 1280 1296 nil 27 4018 4064 1 4080 1 4096 1 4050 #fixedViewTop 51 170 192 34 4 608 8 'cardContainer' 4128 8 'Toolbar' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 5 265030 4 ##(Smalltalk.Menu) nil true 34 9 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #fileNew 8 '&New Workspace' 9373 1 nil nil nil 4546 1 4578 #fileOpen 8 '&Open Workspace...' 9375 1 nil nil nil 4546 1 4578 #fileSave 8 '&Save' 9383 1 nil nil nil 4546 1 4578 #fileSaveAs 8 'Save &As...' 1 1 nil nil nil 4546 1 4578 #fileRevert 8 '&Revert' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 4546 1 4578 #resetCursor 8 'Reset &Cursor' 1025 1 nil nil nil 4818 4097 4546 1 4578 #exit 8 'E&xit Jade' 17639 1 nil nil nil 8 '&File' nil 1 nil nil 27403 nil nil 4498 nil true 34 15 4546 1 4578 #undo 8 '&Undo' 9397 1 nil nil nil 4546 1 4578 #redo 8 'R&edo' 9395 1 nil nil nil 4818 4097 4546 1 4578 #editCut 8 'Cu&t' 9393 1 nil nil nil 4546 1 4578 #editCopy 8 '&Copy' 9351 1 nil nil nil 4546 1 4578 #editPaste 8 '&Paste' 9389 1 nil nil nil 4546 1 4578 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 4546 1 4578 #editDelete 8 '&Delete' 1629 1 nil nil nil 4818 4097 4546 1 4578 #editFind 8 '&Find...' 9357 1 nil nil nil 4546 1 4578 #editFindNext 8 'Find &Next' 1253 1 nil nil nil 4546 1 4578 #editReplace 8 '&Replace...' 9361 1 nil nil nil 4818 4097 4546 1 4578 #addQuotesToSelection 8 'Add &Quotes' 1 1 nil nil nil 4546 1 4578 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 nil nil nil 8 '&Edit' nil 1 nil nil 27429 nil nil 4498 nil true 34 10 4546 1 4578 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 nil nil nil 4546 1 4578 #browseMonticelloRepositories 8 'Browser &Monticello Repositories' 9371 1 nil nil nil 4546 1 4578 #jadeBrowseObjectLog 8 'Browse &Object Log' 9369 1 nil nil nil 4546 1 4578 #browseProcesses 8 'Browse &Processes' 9377 1 nil nil nil 4546 1 4578 #jadeBrowseTests 8 'Browse SUnit &Tests' 9385 1 nil nil nil 4546 1 4578 #autocompletion 8 'Configure Autocompletion' 1 1 nil nil nil 4546 1 4578 #profStef 8 'Prof&Stef Tutorial' 1 1 nil nil nil 4818 4097 4546 1 4578 #inspectOop 8 'Inspect Oop ...' 1 1 nil nil nil 4546 1 4578 #describeErrorNumber 8 'Description for Error Number ...' 1025 1 nil nil nil 8 '&Browse' nil 134217729 nil nil 27449 nil nil 4498 nil true 34 12 4546 1 4578 #stopHeartbeat 8 'Stop Idle Session Check' 1 1 nil nil nil 4546 1 4578 #sleepAndCommit 8 '&Sleep and Commit' 1 1 nil nil nil 4546 1 4578 #abortTransaction 8 '&Abort Transaction' 1 1 nil nil nil 4546 1 4578 #commitTransaction 8 '&Commit Transaction' 1 1 nil nil nil 4818 4097 4546 1 4578 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 4546 1 4578 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 4546 1 4578 #jadeExecute 8 '&Execute' 9355 1 nil nil nil 4546 1 4578 #jadeDebug 8 'Debu&g' 9359 1 nil nil nil 4546 1 4578 #fileIn 8 'Fi&le In' 1 1 nil nil nil 4818 4097 4546 1 4578 #fileOutJadeServer 8 '&File Out JadeServer' 1 1 nil nil nil 8 '&Jade' nil 1 nil nil 27471 nil nil 4498 nil true 34 1 4546 1 4578 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 1 nil nil 27475 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'icons\GS32.ico' nil 6818 nil true 6864 8 'icons\GS16.ico' nil nil nil 1 nil nil 1090 138 144 34 3 1154 #createAt:extent: 34 2 514 3059 21 514 1201 801 416 1154 #text: 34 1 8 'Jade Session' 416 1154 #updateMenuBar 1280 416 1234 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 5 0 0 10 0 0 0 81 8 0 0 154 1 0 0] 34 2 4128 608 1296 nil 27 )! !
!JadeTranscript class categoriesFor: #resource_Default_view!public!resources-views! !

