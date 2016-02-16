"Filed out from Dolphin Smalltalk 7"!

JadeWorkspace subclass: #JadeTranscript
	instanceVariableNames: 'allSessionsPresenter mySessionPresenter'
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

cr

	^self nextPutAll: Character lf asString.
!

createComponents

	super createComponents.
	mySessionPresenter	:= self add: MySessionPresenter		new name: 'My Session'.
	allSessionsPresenter	:= self add: AllSessionsPresenter	new name: 'All Sessions'.
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

editCut

	View focus == self transcriptView ifTrue: [
		View focus isReadOnly: false.
		super editCut.
		View focus isReadOnly: true.
	].
	super editCut.
!

editDelete

	View focus == self transcriptView ifTrue: [
		View focus isReadOnly: false.
		super editDelete.
		View focus isReadOnly: true.
		^self.
	].
	super editDelete.
!

gciSession: aGciSession

	super gciSession: aGciSession.
	mySessionPresenter gciSession: aGciSession.
	allSessionsPresenter gciSession: aGciSession.!

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

invalidateUserInterface

	[
		super invalidateUserInterface.
	] on: Error do: [:ex | 
		ex return.
	].
!

model: anObject

	super model: anObject.
	(anObject isKindOf: GciSession) ifTrue: [
		mySessionPresenter gciSession: anObject.
		allSessionsPresenter gciSession: anObject.
	].!

nextPut: aCharacter

	^self nextPutAll: aCharacter asString.
!

nextPutAll: anObject

	| string transcript |
	string := (anObject isKindOf: String)
		ifTrue: [anObject]
		ifFalse: [anObject printString].
	(transcript := self transcriptView)
		isReadOnly: false;
		goto: transcript textLength + 1;
		insertText: string at: transcript caretPosition;
		goto: transcript caretPosition + string size;
		isReadOnly: true;
		yourself.
	"marker to signal not to replace the top-of-stack"
	^gciSession oopIllegal.
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
	(gciSession indexOfClientForwarder: self) = 2 ifFalse: [self error: 'Transcript should be the second client forwarder!!'].
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

show: aString

	^self nextPutAll: aString.
!

sleepAndCommit

	[
		gciSession serverPerform: #'sleepAndCommit'.
	] on: GsSoftBreak , GsHardBreak do: [:ex | 
		ex terminateProcess.
	].
!

tab

	^self nextPutAll: Character tab asString.
!

transcriptView

	^self view viewNamed: 'Transcript'.
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
!JadeTranscript categoriesFor: #cr!public!Transcript! !
!JadeTranscript categoriesFor: #createComponents!public! !
!JadeTranscript categoriesFor: #createSchematicWiring!public! !
!JadeTranscript categoriesFor: #currentCardChanged!public! !
!JadeTranscript categoriesFor: #describeErrorNumber!event handling!public! !
!JadeTranscript categoriesFor: #disableNativeCode!event handling!public! !
!JadeTranscript categoriesFor: #editCut!public!Transcript! !
!JadeTranscript categoriesFor: #editDelete!public!Transcript! !
!JadeTranscript categoriesFor: #gciSession:!private! !
!JadeTranscript categoriesFor: #inspectOop!event handling!public! !
!JadeTranscript categoriesFor: #invalidateUserInterface!public!updating! !
!JadeTranscript categoriesFor: #model:!public! !
!JadeTranscript categoriesFor: #nextPut:!public!Transcript! !
!JadeTranscript categoriesFor: #nextPutAll:!public!Transcript! !
!JadeTranscript categoriesFor: #onCloseRequested:!event handling!public! !
!JadeTranscript categoriesFor: #onViewOpened!public!updating! !
!JadeTranscript categoriesFor: #queryCommand:!public!updating! !
!JadeTranscript categoriesFor: #resetCursor!event handling!public! !
!JadeTranscript categoriesFor: #show:!public!Transcript! !
!JadeTranscript categoriesFor: #sleepAndCommit!event handling!public! !
!JadeTranscript categoriesFor: #tab!public!Transcript! !
!JadeTranscript categoriesFor: #transcriptView!public! !
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 2 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1140850688 131073 624 0 482 512 0 7 0 0 0 624 655878 ##(Smalltalk.CardLayout)  202 208 98 4 721414 ##(Smalltalk.Association)  8 'Workspace' 410 8 ##(Smalltalk.ReferenceView)  98 14 0 624 98 2 8 1140850688 131073 832 0 482 8 4278190080 0 7 0 0 0 832 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.CodeSourcePresenter)  8 #resource_Default_view 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 9 55 530 1153 621 832 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 81 1 0 0] 98 0 530 193 193 0 27 786 8 'Transcript' 410 8 ##(Smalltalk.ContainerView)  98 15 0 624 98 2 8 1140850688 131073 1280 0 0 0 5 0 0 0 1280 562 234 240 98 4 410 8 ##(Smalltalk.Toolbar)  98 25 0 1280 98 2 8 1140851500 131137 1408 0 482 8 4278190080 0 517 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 1408 482 1504 8 4294902449 234 256 1216 234 256 98 38 12819 1246982 ##(Smalltalk.ToolbarSystemButton)  12819 0 1408 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #editCopy 8 'Copy' 1 1 0 1 3 12821 1698 12821 0 1408 1 1730 8 #editPaste 8 'Paste' 1 1 0 1 5 12823 1698 12823 0 1408 1 1730 8 #editDelete 8 'Delete' 1 1 0 1 11 12825 1698 12825 0 1408 1 1730 8 #undo 8 'Undo' 1 1 0 1 7 12827 1698 12827 0 1408 1 1730 8 #redo 8 'Redo' 1 1 0 1 9 12829 1698 12829 0 1408 1 1730 8 #editFind 8 'Find' 1 1 0 1 25 12831 1698 12831 0 1408 1 1730 8 #editReplace 8 'Replace' 1 1 0 1 27 12795 853766 ##(Smalltalk.ToolbarButton)  12795 0 1408 1 1730 8 #abortTransaction 8 'Abort Transaction' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr7.dll' 0 0 7 530 1857 33 1 12797 2178 12797 0 1408 1 1730 8 #commitTransaction 8 'Commit Transaction' 1 1 0 2272 27 12799 2178 12799 0 1408 1 1730 8 #jadeBrowseUsers 8 'Browse Users' 1 1 0 2272 75 12801 2178 12801 0 1408 1 1730 8 #jadeBrowseClasses 8 'Open System Browser' 1 1 0 2272 17 12803 2178 12803 0 1408 1 1730 8 #jadeBrowseMonticello 8 'Open Monticello Browser' 1 1 0 2272 3 12805 2178 12805 0 1408 1 1730 8 #jadeDisplay 8 'Print Result of Selection or Line' 1 1 0 2272 55 12807 2178 12807 0 1408 1 1730 8 #jadeExecute 8 'Evaluate Selection or Line' 1 1 0 2272 57 12809 2178 12809 0 1408 1 1730 8 #jadeInspect 8 'Inspect Selection or Line' 1 1 0 2272 59 12811 1698 12811 0 1408 1 1730 8 #fileNew 8 'New Workspace' 1 1 0 1 13 12813 1698 12813 0 1408 1 1730 8 #fileOpen 8 'Open Workspace' 1 1 0 1 15 12815 1698 12815 0 1408 1 1730 8 #fileSave 8 'Save' 1 1 0 1 17 12817 1698 12817 0 1408 1 1730 8 #editCut 8 'Cut' 1 1 0 1 1 98 24 2192 2400 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 1408 3 0 1 2464 2528 2592 3122 0 0 1408 3 0 1 2656 2720 2784 3122 0 0 1408 3 0 1 2848 2912 2976 3122 0 0 1408 3 0 1 3040 1712 1792 1856 1920 1984 3122 0 0 1408 3 0 1 2048 2112 234 240 98 4 1 117 2272 1 0 1 0 530 33 33 530 45 45 0 0 1010 202 208 98 2 1074 1104 98 2 530 1 1 530 1153 51 1408 1074 8 #updateSizePosted 1216 1408 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 25 0 0 0] 98 0 1232 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 3506 8 #fixedParentRight 1 3506 8 #fixedParentTop 1 3506 8 #fixedViewTop 51 410 8 ##(Smalltalk.ScintillaView)  98 50 0 1280 98 2 8 1445007428 1025 3648 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 482 8 4278190080 0 5 0 0 0 3648 0 8 4294903515 852486 ##(Smalltalk.NullConverter)  0 0 11 0 234 256 98 4 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 3904 0 0 0 8 #callTip 3922 77 786694 ##(Smalltalk.IndexedColor)  33554457 3986 33554471 1 0 0 0 0 3952 0 0 0 98 40 3936 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  3904 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 3986 33554433 4016 3648 8 #circle 202 208 1216 0 63 9215 0 0 0 0 3986 33554447 0 0 65 0 0 0 8 '' 3 234 256 98 2 8 #container 3872 0 0 0 0 1 0 234 256 98 6 1 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  1 3648 65025 3 32 1 0 3 4322 3 3648 33423361 5 32 3 0 5 4322 5 3648 511 1 32 5 0 0 0 234 256 98 6 8 #Notification 3922 1029 0 196934 1 ##(Smalltalk.RGB)  31185883 1 0 0 0 0 4416 0 0 0 8 #Error 3922 1031 4450 335 4450 31317503 1 0 0 0 0 4480 0 0 0 8 #Warning 3922 1027 4450 60239 4450 31326207 1 0 0 0 0 4544 0 0 0 0 1010 202 208 98 11 1074 1104 98 2 530 1 51 530 1153 571 3648 1074 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 3648 1074 8 #isTextModified: 98 1 32 3648 1074 8 #modificationEventMask: 98 1 9215 3648 1074 8 #secondaryCaretForecolor: 98 1 4450 16711423 3648 1074 8 #caretPeriod: 98 1 1061 3648 1074 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin)  1 3648 1 3 32 1 5 5074 3 3648 33 1 16 67108863 5 5074 5 3648 1 1 16 -67108863 5 3648 1074 8 #indentationGuides: 98 1 0 3648 1074 8 #tabIndents: 98 1 16 3648 1074 8 #tabWidth: 98 1 9 3648 1074 8 #areAdditionalCaretsVisible: 98 1 16 3648 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 64 2 0 0 54 1 0 0] 98 0 1232 0 29 3474 3520 1 3552 1 3584 51 3506 8 #fixedParentBottom 1 234 256 98 2 3648 8 'transcript' 0 1010 202 208 98 1 1074 1104 98 2 530 9 55 530 1153 621 1280 1170 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 81 1 0 0] 98 2 1408 3648 1232 0 27 786 8 'My Session' 410 848 98 14 0 624 98 2 8 1140850688 131073 5664 0 0 0 5 0 0 0 5664 946 8 ##(Smalltalk.MySessionPresenter)  992 0 1010 202 208 98 1 1074 1104 98 2 530 9 55 530 1153 621 5664 1170 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 81 1 0 0] 1216 1232 0 27 786 8 'All Sessions' 410 848 98 14 0 624 98 2 8 1140850688 131073 5936 0 0 0 5 0 0 0 5936 946 8 ##(Smalltalk.AllSessionsPresenter)  992 0 1010 202 208 98 1 1074 1104 98 2 530 9 55 530 1153 621 5936 1170 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 81 1 0 0] 1216 1232 0 27 832 234 256 98 8 832 8 'codePane' 5936 8 'All Sessions' 1280 8 'Transcript' 5664 8 'My Session' 0 410 8 ##(Smalltalk.TabView)  98 23 0 624 98 2 8 1140916736 1 6272 590662 2 ##(Smalltalk.ListModel)  202 208 98 4 1264 5648 5920 816 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  721158 ##(Smalltalk.SystemColor)  31 0 1 0 0 0 6272 0 8 4294903217 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1049926 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 8 #noIcons 1010 202 208 98 3 1074 1104 98 2 530 1 1 530 1169 683 6272 1074 8 #basicSelectionsByIndex: 98 1 98 1 9 6272 1074 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 6272 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 85 1 0 0] 98 0 1232 0 27 1010 202 208 98 1 1074 1104 98 2 530 1 1 530 1169 683 624 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 85 1 0 0] 98 5 1280 5664 5936 832 6272 1232 0 27 3474 3520 1 3552 1 3584 1 5392 1 234 256 98 2 624 8 'cardContainer' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 9 984134 2 ##(Smalltalk.CommandMenuItem)  1 1730 2880 8 '&New Workspace' 9373 1 0 0 0 7170 1 1730 2944 8 '&Open Workspace...' 9375 1 0 0 0 7170 1 1730 3008 8 '&Save' 9383 1 0 0 0 7170 1 1730 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 7170 1 1730 8 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 7170 1 1730 8 #resetCursor 8 'Reset &Cursor' 1025 1 0 0 0 7458 4097 7170 1 1730 8 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 12727 0 0 7122 0 16 98 15 7170 1 1730 1952 8 '&Undo' 9397 1 0 0 0 7170 1 1730 2016 8 'R&edo' 9395 1 0 0 0 7458 4097 7170 1 1730 3072 8 'Cu&t' 9393 1 0 0 0 7170 1 1730 1760 8 '&Copy' 9351 1 0 0 0 7170 1 1730 1824 8 '&Paste' 9389 1 0 0 0 7170 1 1730 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 7170 1 1730 1888 8 '&Delete' 1629 1 0 0 0 7458 4097 7170 1 1730 2080 8 '&Find...' 9357 1 0 0 0 7170 1 1730 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 7170 1 1730 2144 8 '&Replace...' 9361 1 0 0 0 7458 4097 7170 1 1730 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 7170 1 1730 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 12753 0 0 7122 0 16 98 21 7170 1 1730 8 #sleepAndCommit 8 '&Sleep and Commit' 1 1 0 0 0 7170 1 1730 2224 8 '&Abort Transaction' 1 1 0 0 0 7170 1 1730 2432 8 '&Commit Transaction' 1 1 0 0 0 7170 1 1730 8 #disableNativeCode 8 'Disable Naitive Code' 1 1 0 0 0 7458 4097 7170 1 1730 2816 8 '&Inspect' 9379 1 0 0 0 7170 1 1730 2688 8 '&Display' 9353 1 0 0 0 7170 1 1730 2752 8 '&Execute' 9355 1 0 0 0 7170 1 1730 8 #fileIn 8 'Fi&le In' 1 1 0 0 0 7458 4097 7170 1 1730 2560 8 '&Browse Classes' 9349 1 0 0 0 7170 1 1730 8 #browseMonticelloRepositories 8 'Browser &Monticello Repositories' 9371 1 0 0 0 7170 1 1730 8 #jadeBrowseObjectLog 8 'Browse &Object Log' 9369 1 0 0 0 7170 1 1730 8 #browseProcesses 8 'Browse &Processes' 9377 1 0 0 0 7170 1 1730 8 #autocompletion 8 'Configure Autocompletion' 1 1 0 0 0 7458 4097 7170 1 1730 8 #inspectOop 8 'Inspect Oop ...' 1 1 0 0 0 7170 1 1730 8 #describeErrorNumber 8 'Description for Error Number ...' 1025 1 0 0 0 7458 4097 7170 1025 1730 8 #turnAutoMigrateOff 8 'Turn Auto Migrate Off' 1 1 0 0 0 7170 1025 1730 8 #turnAutoMigrateOn 8 'Turn Auto Migrate On' 1 1 0 0 0 8 '&Jade' 0 1 0 0 12789 0 0 7122 0 16 98 1 7170 1 1730 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 12793 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 263494 3 ##(Smalltalk.Icon)  0 16 2304 8 'icons\GS32.ico' 0 9618 0 16 2304 8 'icons\GS16.ico' 0 0 0 1 0 0 1010 202 208 98 3 1074 1104 98 2 530 2879 21 530 1201 801 416 1074 8 #text: 98 1 8 'Jade Session' 416 1074 8 #updateMenuBar 1216 416 1170 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 98 1 624 1232 0 27 )! !
!JadeTranscript class categoriesFor: #resource_Default_view!public!resources-views! !

