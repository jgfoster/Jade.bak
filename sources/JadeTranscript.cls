"Filed out from Dolphin Smalltalk 7"!

JadeWorkspace subclass: #JadeTranscript
	instanceVariableNames: 'allSessionsPresenter mySessionPresenter toolbarPresenter transcriptPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeTranscript guid: (GUID fromString: '{7D871BC4-1338-4BA1-AF6A-C1EDD936D683}')!
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 234 240 98 4 410 8 ##(Smalltalk.CardContainer) 98 16 0 416 98 2 8 1140850688 131073 624 0 482 512 0 7 0 0 0 624 655878 ##(Smalltalk.CardLayout) 202 208 98 4 721414 ##(Smalltalk.Association) 8 'Workspace' 410 8 ##(Smalltalk.ReferenceView) 98 14 0 624 98 2 8 1140850688 131073 832 0 482 8 4278190080 0 7 0 0 0 832 1180166 ##(Smalltalk.ResourceIdentifier) 8 ##(Smalltalk.CodeSourcePresenter) 8 #resource_Default_view 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 1 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 530 9 55 530 1153 571 832 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 98 0 530 193 193 0 27 786 8 'My Session' 410 848 98 14 0 624 98 2 8 1140850688 131073 1280 0 0 0 5 0 0 0 1280 946 8 ##(Smalltalk.MySessionPresenter) 992 0 1010 202 208 98 1 1074 1104 98 2 530 9 55 530 1153 571 1280 1170 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 1216 1232 0 27 786 8 'All Sessions' 410 848 98 14 0 624 98 2 8 1140850688 131073 1552 0 0 0 5 0 0 0 1552 946 8 ##(Smalltalk.AllSessionsPresenter) 992 0 1010 202 208 98 1 1074 1104 98 2 530 9 55 530 1153 571 1552 1170 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 1216 1232 0 27 786 8 'Transcript' 410 848 98 14 0 624 98 2 8 1140916224 131073 1824 0 482 8 4278190080 0 5 0 0 0 1824 946 8 ##(Smalltalk.TranscriptPresenter) 992 0 1010 202 208 98 1 1074 1104 98 2 530 9 55 530 1153 571 1824 1170 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 1216 1232 0 27 832 234 256 98 8 832 8 'codePane' 1552 8 'All Sessions' 1824 8 'Transcript' 1280 8 'My Session' 0 410 8 ##(Smalltalk.TabView) 98 23 0 624 98 2 8 1140916736 1 2192 590662 2 ##(Smalltalk.ListModel) 202 208 98 4 1808 1264 1536 816 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 721158 ##(Smalltalk.SystemColor) 31 0 1 0 0 0 2192 0 8 4294906105 8 ##(Smalltalk.BasicListAbstract) 8 ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 8 #noIcons 1010 202 208 98 3 1074 1104 98 2 530 1 1 530 1169 633 2192 1074 8 #basicSelectionsByIndex: 98 1 98 1 9 2192 1074 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 2192 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 60 1 0 0] 98 0 1232 0 27 1010 202 208 98 1 1074 1104 98 2 530 1 51 530 1169 633 624 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 72 2 0 0 85 1 0 0] 98 5 1824 1280 1552 832 2192 1232 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) 8 #fixedParentLeft 1 2962 8 #fixedParentRight 1 2962 8 #fixedParentTop 51 2962 8 #fixedParentBottom 1 410 848 98 14 0 416 98 2 8 1140850688 131073 3104 0 0 0 7 0 0 0 3104 946 8 ##(Smalltalk.JadeToolsToolBarPresenter) 992 0 1010 202 208 98 1 1074 1104 98 2 530 1 1 530 1169 51 3104 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 25 0 0 0] 1216 1232 0 27 2930 2976 1 3008 1 3040 1 2962 8 #fixedViewTop 51 234 256 98 4 3104 8 'Toolbar' 624 8 'cardContainer' 0 461638 4 ##(Smalltalk.MenuBar) 0 16 98 4 265030 4 ##(Smalltalk.Menu) 0 16 98 9 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) 8 #fileNew 8 '&New Workspace' 9373 1 0 0 0 3554 1 3586 8 #fileOpen 8 '&Open Workspace...' 9375 1 0 0 0 3554 1 3586 8 #fileSave 8 '&Save' 9383 1 0 0 0 3554 1 3586 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 3554 1 3586 8 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 3554 1 3586 8 #resetCursor 8 'Reset &Cursor' 1025 1 0 0 0 3906 4097 3554 1 3586 8 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 17123 0 0 3506 0 16 98 15 3554 1 3586 8 #undo 8 '&Undo' 9397 1 0 0 0 3554 1 3586 8 #redo 8 'R&edo' 9395 1 0 0 0 3906 4097 3554 1 3586 8 #editCut 8 'Cu&t' 9393 1 0 0 0 3554 1 3586 8 #editCopy 8 '&Copy' 9351 1 0 0 0 3554 1 3586 8 #editPaste 8 '&Paste' 9389 1 0 0 0 3554 1 3586 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 3554 1 3586 8 #editDelete 8 '&Delete' 1629 1 0 0 0 3906 4097 3554 1 3586 8 #editFind 8 '&Find...' 9357 1 0 0 0 3554 1 3586 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 3554 1 3586 8 #editReplace 8 '&Replace...' 9361 1 0 0 0 3906 4097 3554 1 3586 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 3554 1 3586 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 17149 0 0 3506 0 16 98 21 3554 1 3586 8 #sleepAndCommit 8 '&Sleep and Commit' 1 1 0 0 0 3554 1 3586 8 #abortTransaction 8 '&Abort Transaction' 1 1 0 0 0 3554 1 3586 8 #commitTransaction 8 '&Commit Transaction' 1 1 0 0 0 3554 1 3586 8 #disableNativeCode 8 'Disable Naitive Code' 1 1 0 0 0 3906 4097 3554 1 3586 8 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 3554 1 3586 8 #jadeDisplay 8 '&Display' 9353 1 0 0 0 3554 1 3586 8 #jadeExecute 8 '&Execute' 9355 1 0 0 0 3554 1 3586 8 #fileIn 8 'Fi&le In' 1 1 0 0 0 3906 4097 3554 1 3586 8 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 0 0 0 3554 1 3586 8 #browseMonticelloRepositories 8 'Browser &Monticello Repositories' 9371 1 0 0 0 3554 1 3586 8 #jadeBrowseObjectLog 8 'Browse &Object Log' 9369 1 0 0 0 3554 1 3586 8 #browseProcesses 8 'Browse &Processes' 9377 1 0 0 0 3554 1 3586 8 #autocompletion 8 'Configure Autocompletion' 1 1 0 0 0 3906 4097 3554 1 3586 8 #inspectOop 8 'Inspect Oop ...' 1 1 0 0 0 3554 1 3586 8 #describeErrorNumber 8 'Description for Error Number ...' 1025 1 0 0 0 3906 4097 3554 1025 3586 8 #turnAutoMigrateOff 8 'Turn Auto Migrate Off' 1 1 0 0 0 3554 1025 3586 8 #turnAutoMigrateOn 8 'Turn Auto Migrate On' 1 1 0 0 0 8 '&Jade' 0 1 0 0 17185 0 0 3506 0 16 98 1 3554 1 3586 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 17189 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 263494 3 ##(Smalltalk.Icon) 0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'icons\GS32.ico' 0 6290 0 16 6336 8 'icons\GS16.ico' 0 0 0 1 0 0 1010 202 208 98 3 1074 1104 98 2 530 5119 21 530 1201 801 416 1074 8 #text: 98 1 8 'Jade Session' 416 1074 8 #updateMenuBar 1216 416 1170 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 154 1 0 0] 98 2 3104 624 1232 0 27 )! !
!JadeTranscript class categoriesFor: #resource_Default_view!public!resources-views! !

