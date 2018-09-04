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

updateCaption

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
!JadeTranscript categoriesFor: #resetCursor!event handling!public! !
!JadeTranscript categoriesFor: #sleepAndCommit!event handling!public! !
!JadeTranscript categoriesFor: #updateCaption!public! !

!JadeTranscript class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 549 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 608 nil nil nil 7 nil nil nil 608 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeToolsToolBarPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 1169 51 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 25 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 962 #fixedParentRight 1 962 #fixedParentTop 1 962 #fixedViewTop 51 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1140850688 131073 1040 nil 466 496 nil 7 nil nil nil 1040 655878 ##(Smalltalk.CardLayout) 138 144 34 6 721414 ##(Smalltalk.Association) 8 'Workspace' 410 ##(Smalltalk.ReferenceView) 34 14 nil 1040 34 2 8 1140850688 131073 1232 nil 466 8 4278190080 nil 7 nil nil nil 1232 674 ##(Smalltalk.TranscriptPresenter) #resource_Default_view nil 706 138 144 34 1 770 #createAt:extent: 34 2 514 9 53 514 1153 573 1232 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 896 912 nil 27 1186 8 'My Session' 410 ##(Smalltalk.ReferenceView) 34 14 nil 1040 34 2 8 1140850688 131073 1520 nil nil nil 5 nil nil nil 1520 674 ##(Smalltalk.MySessionPresenter) #resource_Default_view nil 706 138 144 34 1 770 #createAt:extent: 34 2 514 9 53 514 1153 573 1520 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 896 912 nil 27 1186 8 'All Sessions' 410 ##(Smalltalk.ReferenceView) 34 14 nil 1040 34 2 8 1140850688 131073 1776 nil nil nil 5 nil nil nil 1776 674 ##(Smalltalk.AllSessionsPresenter) #resource_Default_view nil 706 138 144 34 1 770 #createAt:extent: 34 2 514 9 53 514 1153 573 1776 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 896 912 nil 27 1186 8 'Transcript' 410 ##(Smalltalk.ReferenceView) 34 14 nil 1040 34 2 8 1140916224 131073 2032 nil 466 8 4278190080 nil 5 nil nil nil 2032 674 ##(Smalltalk.TranscriptPresenter) #resource_Default_view nil 706 138 144 34 1 770 #createAt:extent: 34 2 514 9 53 514 1153 573 2032 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 896 912 nil 27 1186 8 'History' 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 1040 34 2 8 1143017796 1025 2320 nil 466 8 4278190080 nil 5 nil nil nil 2320 nil 8 1793990144 852486 ##(Smalltalk.NullConverter) nil nil 11 706 138 144 34 1 770 #createAt:extent: 34 2 514 9 53 514 1153 573 2320 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 8 #() 912 nil 27 1186 8 'Stone' 410 ##(Smalltalk.ReferenceView) 34 14 nil 1040 34 2 8 1140850688 131073 2656 nil nil nil 5 nil nil nil 2656 674 ##(Smalltalk.StoneInfoPresenter) #resource_Default_view nil 706 138 144 34 1 770 #createAt:extent: 34 2 514 9 53 514 1153 573 2656 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 896 912 nil 27 1232 170 192 34 12 1520 8 'My Session' 2656 8 'Stone' 2032 8 'Transcript' 1232 8 'codePane' 1776 8 'All Sessions' 2320 8 'history' nil 410 ##(Smalltalk.TabView) 34 23 nil 1040 34 2 8 1140916736 1 3008 590662 2 ##(Smalltalk.ListModel) 138 144 34 6 2016 2304 2640 1504 1760 1216 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 721158 ##(Smalltalk.SystemColor) 31 nil 1 nil nil nil 3008 nil 8 1794379232 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #noIcons 706 138 144 34 3 770 #createAt:extent: 34 2 514 1 1 514 1169 633 3008 770 #basicSelectionsByIndex: 34 1 8 #(6) 3008 770 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 3008 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 60 1 0 0] 8 #() 912 nil 27 706 138 144 34 1 770 #createAt:extent: 34 2 514 1 51 514 1169 633 1040 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 72 2 0 0 85 1 0 0] 34 7 2032 2320 2656 1520 1776 1232 3008 912 nil 27 930 976 1 992 1 1008 51 962 #fixedParentBottom 1 170 192 34 4 608 8 'Toolbar' 1040 8 'cardContainer' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 4 265030 4 ##(Smalltalk.Menu) nil true 34 9 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #fileNew 8 '&New Workspace' 9373 1 nil nil nil 3842 1 3874 #fileOpen 8 '&Open Workspace...' 9375 1 nil nil nil 3842 1 3874 #fileSave 8 '&Save' 9383 1 nil nil nil 3842 1 3874 #fileSaveAs 8 'Save &As...' 1 1 nil nil nil 3842 1 3874 #fileRevert 8 '&Revert' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 3842 1 3874 #resetCursor 8 'Reset &Cursor' 1025 1 nil nil nil 4114 4097 3842 1 3874 #exit 8 'E&xit Jade' 17639 1 nil nil nil 8 '&File' nil 1 nil nil 11011 nil nil 3794 nil true 34 15 3842 1 3874 #undo 8 '&Undo' 9397 1 nil nil nil 3842 1 3874 #redo 8 'R&edo' 9395 1 nil nil nil 4114 4097 3842 1 3874 #editCut 8 'Cu&t' 9393 1 nil nil nil 3842 1 3874 #editCopy 8 '&Copy' 9351 1 nil nil nil 3842 1 3874 #editPaste 8 '&Paste' 9389 1 nil nil nil 3842 1 3874 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 3842 1 3874 #editDelete 8 '&Delete' 1629 1 nil nil nil 4114 4097 3842 1 3874 #editFind 8 '&Find...' 9357 1 nil nil nil 3842 1 3874 #editFindNext 8 'Find &Next' 1253 1 nil nil nil 3842 1 3874 #editReplace 8 '&Replace...' 9361 1 nil nil nil 4114 4097 3842 1 3874 #addQuotesToSelection 8 'Add &Quotes' 1 1 nil nil nil 3842 1 3874 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 nil nil nil 8 '&Edit' nil 1 nil nil 11037 nil nil 3794 nil true 34 20 3842 1 3874 #sleepAndCommit 8 '&Sleep and Commit' 1 1 nil nil nil 3842 1 3874 #abortTransaction 8 '&Abort Transaction' 1 1 nil nil nil 3842 1 3874 #commitTransaction 8 '&Commit Transaction' 1 1 nil nil nil 4114 4097 3842 1 3874 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 3842 1 3874 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 3842 1 3874 #jadeExecute 8 '&Execute' 9355 1 nil nil nil 3842 1 3874 #jadeDebug 8 'Debu&g' 9359 1 nil nil nil 3842 1 3874 #fileIn 8 'Fi&le In' 1 1 nil nil nil 4114 4097 3842 1 3874 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 nil nil nil 3842 1 3874 #browseMonticelloRepositories 8 'Browser &Monticello Repositories' 9371 1 nil nil nil 3842 1 3874 #jadeBrowseObjectLog 8 'Browse &Object Log' 9369 1 nil nil nil 3842 1 3874 #browseProcesses 8 'Browse &Processes' 9377 1 nil nil nil 3842 1 3874 #jadeBrowseTests 8 'Browse SUnit &Tests' 1 1 nil nil nil 3842 1 3874 #autocompletion 8 'Configure Autocompletion' 1 1 nil nil nil 3842 1 3874 #fileOutJadeServer 8 '&File Out JadeServer' 1 1 nil nil nil 4114 4097 3842 1 3874 #inspectOop 8 'Inspect Oop ...' 1 1 nil nil nil 3842 1 3874 #describeErrorNumber 8 'Description for Error Number ...' 1025 1 nil nil nil 8 '&Jade' nil 1 nil nil 11073 nil nil 3794 nil true 34 1 3842 1 3874 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 1 nil nil 11077 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'icons\GS32.ico' nil 5970 nil true 6016 8 'icons\GS16.ico' nil nil nil 1 nil nil 706 138 144 34 3 770 #createAt:extent: 34 2 514 2879 21 514 1201 801 416 770 #text: 34 1 8 'Jade Session' 416 770 #updateMenuBar 896 416 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 34 2 608 1040 912 nil 27 )! !
!JadeTranscript class categoriesFor: #resource_Default_view!public!resources-views! !

