"Filed out from Dolphin Smalltalk 7"!

JadeWorkspace subclass: #JadeTranscript
	instanceVariableNames: 'allSessionsPresenter mySessionPresenter toolbarPresenter transcriptPresenter'
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
	mySessionPresenter	:= self add: MySessionPresenter			new name: 'My Session'.
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
	(currentCard isKindOf: TextEdit)  ifTrue: [currentCard setFocus].
	(currentCard name = 'codePane') ifTrue: [
		[
			(currentCard viewNamed: 'document') setFocus.
		] fork.
	].
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

disableNativeCode

	gciSession executeString: 'System configurationAt: #GemNativeCodeEnabled put: false'.
!

gciSession: aGciSession

	super gciSession: aGciSession.
	mySessionPresenter	gciSession: aGciSession.
	allSessionsPresenter	gciSession: aGciSession.
	toolbarPresenter	gciSession: aGciSession.
	transcriptPresenter	gciSession: aGciSession.
!

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
 
	| answer |
	super onCloseRequested: boolValueHolder.
	boolValueHolder value ifFalse: [^self].
	gciSession isNil ifTrue: [^self].
	gciSession isValidSession ifFalse: [^self].
	gciSession logoutRequested ifFalse: [
		boolValueHolder value: false.
		^self.
	].
	answer := MessageBox new
		caption: 'Close Requested';
		confirmOrCancel: 'Commit current transaction?'.
	boolValueHolder value: (#(#yes #no) includes: answer).
	answer = #yes ifTrue: [gciSession commit].
	boolValueHolder value ifFalse: [^self].

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

onViewOpened

	super onViewOpened.
	self view position: 95 @ 35.
!

queryCommand: query

	(#(#'turnAutoMigrateOff' #'turnAutoMigrateOn') includes: query commandSymbol) ifTrue: [
		query isEnabled: true.
		^true.
	].
	^super queryCommand: query.

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

turnAutoMigrateOff

	gciSession executeString: 'MCPlatformSupport autoMigrate: false'.
!

turnAutoMigrateOn

	gciSession executeString: 'MCPlatformSupport autoMigrate: true'.
!

updateCaption

	self caption: (gciSession titleBarFor: 'Transcript').
! !
!JadeTranscript categoriesFor: #autocompletion!event handling!public! !
!JadeTranscript categoriesFor: #browseMonticelloRepositories!event handling!public! !
!JadeTranscript categoriesFor: #createComponents!public! !
!JadeTranscript categoriesFor: #createSchematicWiring!public! !
!JadeTranscript categoriesFor: #currentCardChanged!public! !
!JadeTranscript categoriesFor: #describeErrorNumber!event handling!public! !
!JadeTranscript categoriesFor: #disableNativeCode!event handling!public! !
!JadeTranscript categoriesFor: #gciSession:!private! !
!JadeTranscript categoriesFor: #inspectOop!event handling!public! !
!JadeTranscript categoriesFor: #methodsChanged24!event handling!public! !
!JadeTranscript categoriesFor: #methodsChangedDays!event handling!public! !
!JadeTranscript categoriesFor: #onCloseRequested:!event handling!public! !
!JadeTranscript categoriesFor: #onViewOpened!public!updating! !
!JadeTranscript categoriesFor: #queryCommand:!public!updating! !
!JadeTranscript categoriesFor: #resetCursor!event handling!public! !
!JadeTranscript categoriesFor: #sleepAndCommit!event handling!public! !
!JadeTranscript categoriesFor: #turnAutoMigrateOff!event handling!public! !
!JadeTranscript categoriesFor: #turnAutoMigrateOn!event handling!public! !
!JadeTranscript categoriesFor: #updateCaption!public! !

!JadeTranscript class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13565952 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 0 416 34 2 8 1140850688 131073 608 0 466 496 0 7 0 0 0 608 655878 ##(Smalltalk.CardLayout) 138 144 34 4 721414 ##(Smalltalk.Association) 8 'Workspace' 410 ##(Smalltalk.ReferenceView) 34 14 0 608 34 2 8 1140850688 131073 800 0 466 8 4278190080 0 7 0 0 0 800 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.TranscriptPresenter) #resource_Default_view 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 9 55 514 1153 571 800 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 8 #() 514 193 193 0 27 754 8 'My Session' 410 ##(Smalltalk.ReferenceView) 34 14 0 608 34 2 8 1140850688 131073 1184 0 0 0 5 0 0 0 1184 898 ##(Smalltalk.MySessionPresenter) #resource_Default_view 0 930 138 144 34 1 994 #createAt:extent: 34 2 514 9 55 514 1153 571 1184 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 1120 1136 0 27 754 8 'All Sessions' 410 ##(Smalltalk.ReferenceView) 34 14 0 608 34 2 8 1140850688 131073 1440 0 0 0 5 0 0 0 1440 898 ##(Smalltalk.AllSessionsPresenter) #resource_Default_view 0 930 138 144 34 1 994 #createAt:extent: 34 2 514 9 55 514 1153 571 1440 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 1120 1136 0 27 754 8 'Transcript' 410 ##(Smalltalk.ReferenceView) 34 14 0 608 34 2 8 1140916224 131073 1696 0 466 8 4278190080 0 5 0 0 0 1696 898 ##(Smalltalk.TranscriptPresenter) #resource_Default_view 0 930 138 144 34 1 994 #createAt:extent: 34 2 514 9 55 514 1153 571 1696 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 1120 1136 0 27 800 170 192 34 8 1696 8 'Transcript' 1440 8 'All Sessions' 1184 8 'My Session' 800 8 'codePane' 0 410 ##(Smalltalk.TabView) 34 23 0 608 34 2 8 1140916736 1 2048 590662 2 ##(Smalltalk.ListModel) 138 144 34 4 1680 1168 1424 784 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 721158 ##(Smalltalk.SystemColor) 31 0 1 0 0 0 2048 0 8 4294902215 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 #noIcons 930 138 144 34 3 994 #createAt:extent: 34 2 514 1 1 514 1169 633 2048 994 #basicSelectionsByIndex: 34 1 8 #(4) 2048 994 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2048 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 60 1 0 0] 8 #() 1136 0 27 930 138 144 34 1 994 #createAt:extent: 34 2 514 1 51 514 1169 633 608 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 72 2 0 0 85 1 0 0] 34 5 1696 1184 1440 800 2048 1136 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2722 #fixedParentRight 1 2722 #fixedParentTop 51 2722 #fixedParentBottom 1 410 ##(Smalltalk.ReferenceView) 34 14 0 416 34 2 8 1140850688 131073 2800 0 0 0 7 0 0 0 2800 898 ##(Smalltalk.JadeToolsToolBarPresenter) #resource_Default_view 0 930 138 144 34 1 994 #createAt:extent: 34 2 514 1 1 514 1169 51 2800 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 25 0 0 0] 1120 1136 0 27 2690 2736 1 2752 1 2768 1 2722 #fixedViewTop 51 170 192 34 4 608 8 'cardContainer' 2800 8 'Toolbar' 0 461638 4 ##(Smalltalk.MenuBar) 0 true 34 4 265030 4 ##(Smalltalk.Menu) 0 true 34 9 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #fileNew 8 '&New Workspace' 9373 1 0 0 0 3218 1 3250 #fileOpen 8 '&Open Workspace...' 9375 1 0 0 0 3218 1 3250 #fileSave 8 '&Save' 9383 1 0 0 0 3218 1 3250 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 3218 1 3250 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 3218 1 3250 #resetCursor 8 'Reset &Cursor' 1025 1 0 0 0 3490 4097 3218 1 3250 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 11699 0 0 3170 0 true 34 15 3218 1 3250 #undo 8 '&Undo' 9397 1 0 0 0 3218 1 3250 #redo 8 'R&edo' 9395 1 0 0 0 3490 4097 3218 1 3250 #editCut 8 'Cu&t' 9393 1 0 0 0 3218 1 3250 #editCopy 8 '&Copy' 9351 1 0 0 0 3218 1 3250 #editPaste 8 '&Paste' 9389 1 0 0 0 3218 1 3250 #editSelectAll 8 'Select &All' 9347 1 0 0 0 3218 1 3250 #editDelete 8 '&Delete' 1629 1 0 0 0 3490 4097 3218 1 3250 #editFind 8 '&Find...' 9357 1 0 0 0 3218 1 3250 #editFindNext 8 'Find &Next' 9359 1 0 0 0 3218 1 3250 #editReplace 8 '&Replace...' 9361 1 0 0 0 3490 4097 3218 1 3250 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 3218 1 3250 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 11725 0 0 3170 0 true 34 21 3218 1 3250 #sleepAndCommit 8 '&Sleep and Commit' 1 1 0 0 0 3218 1 3250 #abortTransaction 8 '&Abort Transaction' 1 1 0 0 0 3218 1 3250 #commitTransaction 8 '&Commit Transaction' 1 1 0 0 0 3218 1 3250 #disableNativeCode 8 'Disable Naitive Code' 1 1 0 0 0 3490 4097 3218 1 3250 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 3218 1 3250 #jadeDisplay 8 '&Display' 9353 1 0 0 0 3218 1 3250 #jadeExecute 8 '&Execute' 9355 1 0 0 0 3218 1 3250 #fileIn 8 'Fi&le In' 1 1 0 0 0 3490 4097 3218 1 3250 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 0 0 0 3218 1 3250 #browseMonticelloRepositories 8 'Browser &Monticello Repositories' 9371 1 0 0 0 3218 1 3250 #jadeBrowseObjectLog 8 'Browse &Object Log' 9369 1 0 0 0 3218 1 3250 #browseProcesses 8 'Browse &Processes' 9377 1 0 0 0 3218 1 3250 #autocompletion 8 'Configure Autocompletion' 1 1 0 0 0 3490 4097 3218 1 3250 #inspectOop 8 'Inspect Oop ...' 1 1 0 0 0 3218 1 3250 #describeErrorNumber 8 'Description for Error Number ...' 1025 1 0 0 0 3490 4097 3218 1025 3250 #turnAutoMigrateOff 8 'Turn Auto Migrate Off' 1 1 0 0 0 3218 1025 3250 #turnAutoMigrateOn 8 'Turn Auto Migrate On' 1 1 0 0 0 8 '&Jade' 0 1 0 0 11761 0 0 3170 0 true 34 1 3218 1 3250 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 11765 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 263494 3 ##(Smalltalk.Icon) 0 true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'icons\GS32.ico' 0 5362 0 true 5408 8 'icons\GS16.ico' 0 0 0 1 0 0 930 138 144 34 3 994 #createAt:extent: 34 2 514 5119 21 514 1201 801 416 994 #text: 34 1 8 'Jade Session' 416 994 #updateMenuBar 1120 416 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 154 1 0 0] 34 2 2800 608 1136 0 27 )! !
!JadeTranscript class categoriesFor: #resource_Default_view!public!resources-views! !

