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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 549 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1140850688 131073 608 nil 480 nil 7 nil nil nil 608 655878 ##(Smalltalk.CardLayout) 138 144 34 6 721414 ##(Smalltalk.Association) 8 'Workspace' 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 784 nil 480 nil 7 nil nil nil 784 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.TranscriptPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 9 53 514 1153 573 784 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 8 #() 514 193 193 nil 27 738 8 'My Session' 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 1136 nil nil nil 5 nil nil nil 1136 850 ##(Smalltalk.MySessionPresenter) #resource_Default_view nil 882 138 144 34 1 946 #createAt:extent: 34 2 514 9 53 514 1153 573 1136 1026 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 1072 1088 nil 27 738 8 'All Sessions' 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 1392 nil nil nil 5 nil nil nil 1392 850 ##(Smalltalk.AllSessionsPresenter) #resource_Default_view nil 882 138 144 34 1 946 #createAt:extent: 34 2 514 9 53 514 1153 573 1392 1026 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 1072 1088 nil 27 738 8 'Transcript' 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140916224 131073 1648 nil 480 nil 5 nil nil nil 1648 850 ##(Smalltalk.TranscriptPresenter) #resource_Default_view nil 882 138 144 34 1 946 #createAt:extent: 34 2 514 9 53 514 1153 573 1648 1026 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 1072 1088 nil 27 738 8 'History' 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 608 34 2 8 1143017796 1025 1904 nil 480 nil 5 nil nil nil 1904 nil 8 1723783840 852486 ##(Smalltalk.NullConverter) nil nil 11 882 138 144 34 1 946 #createAt:extent: 34 2 514 9 53 514 1153 573 1904 1026 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 8 #() 1088 nil 27 738 8 'Stone' 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 2208 nil nil nil 5 nil nil nil 2208 850 ##(Smalltalk.StoneInfoPresenter) #resource_Default_view nil 882 138 144 34 1 946 #createAt:extent: 34 2 514 9 53 514 1153 573 2208 1026 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 1072 1088 nil 27 784 170 192 34 12 1648 8 'Transcript' 2208 8 'Stone' 1392 8 'All Sessions' 1136 8 'My Session' 1904 8 'history' 784 8 'codePane' nil 410 ##(Smalltalk.TabView) 34 23 nil 608 34 2 8 1140916736 1 2560 590662 2 ##(Smalltalk.ListModel) 138 144 34 6 1632 1888 2192 1120 1376 768 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 721158 ##(Smalltalk.SystemColor) 31 nil 1 nil nil nil 2560 nil 8 1723778448 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #noIcons 882 138 144 34 3 946 #createAt:extent: 34 2 514 1 1 514 1169 633 2560 946 #basicSelectionsByIndex: 34 1 8 #(6) 2560 946 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2560 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 60 1 0 0] 8 #() 1088 nil 27 882 138 144 34 1 946 #createAt:extent: 34 2 514 1 51 514 1169 633 608 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 72 2 0 0 85 1 0 0] 34 7 1648 1904 2208 1136 1392 784 2560 1088 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 3234 #fixedParentRight 1 3234 #fixedParentTop 51 3234 #fixedParentBottom 1 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 3312 nil nil nil 7 nil nil nil 3312 850 ##(Smalltalk.JadeToolsToolBarPresenter) #resource_Default_view nil 882 138 144 34 1 946 #createAt:extent: 34 2 514 1 1 514 1169 51 3312 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 25 0 0 0] 1072 1088 nil 27 3202 3248 1 3264 1 3280 1 3234 #fixedViewTop 51 170 192 34 4 608 8 'cardContainer' 3312 8 'Toolbar' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 5 265030 4 ##(Smalltalk.Menu) nil true 34 9 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #fileNew 8 '&New Workspace' 9373 1 nil nil nil 3730 1 3762 #fileOpen 8 '&Open Workspace...' 9375 1 nil nil nil 3730 1 3762 #fileSave 8 '&Save' 9383 1 nil nil nil 3730 1 3762 #fileSaveAs 8 'Save &As...' 1 1 nil nil nil 3730 1 3762 #fileRevert 8 '&Revert' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 3730 1 3762 #resetCursor 8 'Reset &Cursor' 1025 1 nil nil nil 4002 4097 3730 1 3762 #exit 8 'E&xit Jade' 17639 1 nil nil nil 8 '&File' nil 1 nil nil 10777 nil nil 3682 nil true 34 15 3730 1 3762 #undo 8 '&Undo' 9397 1 nil nil nil 3730 1 3762 #redo 8 'R&edo' 9395 1 nil nil nil 4002 4097 3730 1 3762 #editCut 8 'Cu&t' 9393 1 nil nil nil 3730 1 3762 #editCopy 8 '&Copy' 9351 1 nil nil nil 3730 1 3762 #editPaste 8 '&Paste' 9389 1 nil nil nil 3730 1 3762 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 3730 1 3762 #editDelete 8 '&Delete' 1629 1 nil nil nil 4002 4097 3730 1 3762 #editFind 8 '&Find...' 9357 1 nil nil nil 3730 1 3762 #editFindNext 8 'Find &Next' 1253 1 nil nil nil 3730 1 3762 #editReplace 8 '&Replace...' 9361 1 nil nil nil 4002 4097 3730 1 3762 #addQuotesToSelection 8 'Add &Quotes' 1 1 nil nil nil 3730 1 3762 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 nil nil nil 8 '&Edit' nil 1 nil nil 10803 nil nil 3682 nil true 34 10 3730 1 3762 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 nil nil nil 3730 1 3762 #browseMonticelloRepositories 8 'Browser &Monticello Repositories' 9371 1 nil nil nil 3730 1 3762 #jadeBrowseObjectLog 8 'Browse &Object Log' 9369 1 nil nil nil 3730 1 3762 #browseProcesses 8 'Browse &Processes' 9377 1 nil nil nil 3730 1 3762 #jadeBrowseTests 8 'Browse SUnit &Tests' 9385 1 nil nil nil 3730 1 3762 #autocompletion 8 'Configure Autocompletion' 1 1 nil nil nil 3730 1 3762 #profStef 8 'Prof&Stef Tutorial' 1 1 nil nil nil 4002 4097 3730 1 3762 #inspectOop 8 'Inspect Oop ...' 1 1 nil nil nil 3730 1 3762 #describeErrorNumber 8 'Description for Error Number ...' 1025 1 nil nil nil 8 '&Browse' nil 134217729 nil nil 10823 nil nil 3682 nil true 34 12 3730 1 3762 #stopHeartbeat 8 'Stop Idle Session Check' 1 1 nil nil nil 3730 1 3762 #sleepAndCommit 8 '&Sleep and Commit' 1 1 nil nil nil 3730 1 3762 #abortTransaction 8 '&Abort Transaction' 1 1 nil nil nil 3730 1 3762 #commitTransaction 8 '&Commit Transaction' 1 1 nil nil nil 4002 4097 3730 1 3762 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 3730 1 3762 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 3730 1 3762 #jadeExecute 8 '&Execute' 9355 1 nil nil nil 3730 1 3762 #jadeDebug 8 'Debu&g' 9359 1 nil nil nil 3730 1 3762 #fileIn 8 'Fi&le In' 1 1 nil nil nil 4002 4097 3730 1 3762 #fileOutJadeServer 8 '&File Out JadeServer' 1 1 nil nil nil 8 '&Jade' nil 1 nil nil 10845 nil nil 3682 nil true 34 1 3730 1 3762 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 1 nil nil 10849 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'icons\GS32.ico' nil 6002 nil true 6048 8 'icons\GS16.ico' nil nil nil 1 nil nil 882 138 144 34 3 946 #createAt:extent: 34 2 514 2879 21 514 1201 801 416 946 #text: 34 1 8 'Jade Session' 416 946 #updateMenuBar 1072 416 1026 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 34 2 3312 608 1088 nil 27 )! !
!JadeTranscript class categoriesFor: #resource_Default_view!public!resources-views! !

