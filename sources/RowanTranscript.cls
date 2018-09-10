"Filed out from Dolphin Smalltalk 7"!

JadeTranscript subclass: #RowanTranscript
	instanceVariableNames: 'projectListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanTranscript guid: (GUID fromString: '{93b3c6ce-9a55-45db-bd1e-cc1a44a28e4e}')!
RowanTranscript comment: ''!
!RowanTranscript categoriesForClass!Unclassified! !
!RowanTranscript methodsFor!

createComponents
	super createComponents.
	projectListPresenter := self add: RowanProjectListPresenter new name: 'Projects'!

flipDebugger
	RowanDebugger debuggerClass == RowanDebugger
		ifTrue: [RowanDebugger debuggerClass: JadeDebugger]
		ifFalse: [RowanDebugger debuggerClass: RowanDebugger]!

flipTranscript
	| answeringService |
	answeringService := RowanAnsweringService new.
	answeringService flipTranscriptIn: self gciSession!

isTranscriptInstalled
	| answeringService |
	answeringService := RowanAnsweringService new.
	^answeringService isTranscriptInstalledIn: self gciSession!

onViewOpened
	super onViewOpened.
	projectListPresenter
		gciSession: gciSession;
		browseProjects!

openDictionaryBrowser
	gciSession hasServer ifTrue: [^RowanSystemBrowser showOnSession: gciSession]!

openJadeiteProjectsBrowser
	gciSession hasServer ifTrue: [^JadeiteProjectsSystemBrowser showOnSession: gciSession].
	MessageBox warning: 'Cannot open Jadeite Projects browser'
		caption: 'Cannot open Jadeite Projects browser'!

projectListPresenter
	^projectListPresenter projectListPresenter!

queryCommand: query
	super queryCommand: query.
	query commandSymbol = #flipTranscript
		ifTrue: 
			[query isChecked: self isTranscriptInstalled.
			^true].
	query commandSymbol = #flipDebugger
		ifTrue: 
			[query isChecked: RowanDebugger debuggerClass = RowanDebugger.
			^true]! !
!RowanTranscript categoriesFor: #createComponents!public! !
!RowanTranscript categoriesFor: #flipDebugger!menu handlers!public! !
!RowanTranscript categoriesFor: #flipTranscript!menu handlers!public! !
!RowanTranscript categoriesFor: #isTranscriptInstalled!public! !
!RowanTranscript categoriesFor: #onViewOpened!public!updating! !
!RowanTranscript categoriesFor: #openDictionaryBrowser!menu handlers!public! !
!RowanTranscript categoriesFor: #openJadeiteProjectsBrowser!menu handlers!public! !
!RowanTranscript categoriesFor: #projectListPresenter!accessing!public! !
!RowanTranscript categoriesFor: #queryCommand:!private! !

!RowanTranscript class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 551 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1140850688 131073 608 nil 466 496 nil 7 nil nil nil 608 655878 ##(Smalltalk.CardLayout) 138 144 34 7 721414 ##(Smalltalk.Association) 8 'Workspace' 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 800 nil 466 8 4278190080 nil 5 nil nil nil 800 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.CodeSourcePresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 9 55 514 1153 571 800 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 8 #() 514 193 193 nil 27 754 8 'My Session' 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 1184 nil nil nil 5 nil nil nil 1184 898 ##(Smalltalk.MySessionPresenter) #resource_Default_view nil 930 138 144 34 1 994 #createAt:extent: 34 2 514 9 55 514 1153 571 1184 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 1120 1136 nil 27 754 8 'All Sessions' 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 1440 nil nil nil 5 nil nil nil 1440 898 ##(Smalltalk.AllSessionsPresenter) #resource_Default_view nil 930 138 144 34 1 994 #createAt:extent: 34 2 514 9 55 514 1153 571 1440 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 1120 1136 nil 27 754 8 'Transcript' 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140916224 131073 1696 nil 466 8 4278190080 nil 5 nil nil nil 1696 898 ##(Smalltalk.TranscriptPresenter) #resource_Default_view nil 930 138 144 34 1 994 #createAt:extent: 34 2 514 9 55 514 1153 571 1696 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 1120 1136 nil 27 754 8 'History' 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 608 34 2 8 1143017796 1025 1984 nil 466 8 4278190080 nil 5 nil nil nil 1984 nil 8 4294904231 852486 ##(Smalltalk.NullConverter) nil nil 11 930 138 144 34 1 994 #createAt:extent: 34 2 514 9 55 514 1153 571 1984 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 8 #() 1136 nil 27 754 ##(Smalltalk.Stone) 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 2304 nil nil nil 5 nil nil nil 2304 898 ##(Smalltalk.StoneInfoPresenter) #resource_Default_view nil 930 138 144 34 1 994 #createAt:extent: 34 2 514 9 55 514 1153 571 2304 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 1120 1136 nil 27 754 8 'Projects' 410 ##(Smalltalk.ShellView) 34 27 nil 608 34 2 8 1078919168 131073 2560 nil 466 8 4278190080 514 1201 801 551 265030 4 ##(Smalltalk.Menu) nil true 34 17 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #makeDefault 8 'Make Default' 1 1 nil nil nil 2722 1 2754 #browseProject 8 'Browse' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 2722 1 2754 #cloneProject 8 'Clone Git Project ...' 1 1 nil nil nil 2722 1 2754 #loadProjects 8 'Load ...' 1 1 nil nil nil 2722 1 2754 #unloadProjects 8 'Unload ...' 1 1 nil nil nil 2850 4097 2722 1 2754 #pullFromGit 8 'Pull from Git ...' 1 1 nil nil nil 2722 1 2754 #commit 8 'Commit ...' 1 1 nil nil nil 2722 1 2754 #pushToGit 8 'Push to Git ...' 1 1 nil nil nil 2850 4097 2722 1 2754 #projectChanges 8 'Changes' 1 1 nil nil nil 2722 1 2754 #writeProject 8 'Write ...' 1 1 nil nil nil 2722 1 2754 #checkout 8 'Checkout ...' 1 1 nil nil nil 2850 4097 2722 1 2754 #projectLog 8 'Log' 1 1 nil nil nil 2722 1 2754 #refresh 8 'Refresh' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 2560 1180166 ##(Smalltalk.ProportionalLayout) 170 176 1120 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 2560 34 2 8 1140920649 1025 3552 590662 2 ##(Smalltalk.ListModel) 138 144 1120 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 nil 7 nil nil nil 3552 nil 8 4294906907 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 3 1447750 15 ##(Smalltalk.EditableListViewColumn) 8 'Project Name' 371 #left 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 3904 8 'doIt' 8 '[:project | project displayName]' 8 #[30 105 226 0 106] #displayName 3920 7 257 nil nil 3552 nil 3 nil 3906 0 nil 3938 3 1 3904 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 4016 7 257 nil false nil 1052998 20 ##(Smalltalk.EmbeddedTextEdit) nil nil 8 #(67174528 0) 4112 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil nil nil 5 nil nil nil 4112 nil nil 2098 nil 8 '' 3 nil nil nil nil nil nil nil nil nil 3810 8 'Loaded Sha' 373 #left 3858 #displayString 8 #() 3858 #<= 4288 3906 0 nil 3938 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:project | project sha]' 8 #[30 105 226 0 106] #sha 4320 7 257 nil nil 3552 nil 3 nil nil false nil 4098 nil nil 8 #(67174528 0) 4400 4146 nil false 4192 nil 466 8 4278190080 nil 5 nil nil nil 4400 nil nil 2098 nil 8 '' 3 nil nil nil nil nil nil nil nil nil 3810 8 'Branch' 371 #left 3858 #displayString 4288 3858 #<= 4288 3906 0 nil 3938 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:project | project branch]' 8 #[30 105 226 0 106] #branch 4576 7 257 nil nil 3552 nil 3 nil nil false nil 4098 nil nil 8 #(67174528 0) 4656 4146 nil false 4192 nil 466 4464 nil 5 nil nil nil 4656 nil nil 2098 nil 8 '' 3 nil nil nil nil nil nil nil nil nil #report 1120 nil 131171 514 129 129 34 4 nil nil 514 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 514 1 1 514 1121 539 3552 994 #text: 34 1 8 'Project Name' 3552 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 48 2 0 0 13 1 0 0] 8 #() 1136 nil 27 8 'projectList' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 930 138 144 34 4 994 #createAt:extent: 34 2 514 9 55 514 1153 571 2560 994 #contextMenu: 34 1 2688 2560 994 #text: 34 1 8 'Rowan Project List' 2560 994 #updateMenuBar 1120 2560 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 34 1 3552 1136 nil 27 2560 170 192 34 14 1184 8 'My Session' 800 8 'codePane' 1696 8 'Transcript' 1984 8 'history' 2560 8 'projectsList' 2304 8 'Stone' 1440 8 'All Sessions' nil 410 ##(Smalltalk.TabView) 34 23 nil 608 34 2 8 1140916736 1 5424 3618 138 144 34 7 1680 1968 ##(Smalltalk.Stone) 1168 1424 784 2544 nil 3680 721158 ##(Smalltalk.SystemColor) 31 nil 1 nil nil nil 5424 nil 8 4294905519 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 3760 nil nil nil nil nil #noIcons 930 138 144 34 3 994 #createAt:extent: 34 2 514 1 1 514 1169 633 5424 994 #basicSelectionsByIndex: 34 1 8 #(7) 5424 994 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 5424 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 60 1 0 0] 8 #() 1136 nil 27 930 138 144 34 1 994 #createAt:extent: 34 2 514 1 51 514 1169 633 608 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 72 2 0 0 85 1 0 0] 34 8 1696 1984 2304 1184 1440 800 2560 5424 1136 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 6018 #fixedParentRight 1 6018 #fixedParentTop 51 6018 #fixedParentBottom 1 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 6096 nil nil nil 7 nil nil nil 6096 898 ##(Smalltalk.JadeToolsToolBarPresenter) #resource_Default_view nil 930 138 144 34 1 994 #createAt:extent: 34 2 514 1 1 514 1169 51 6096 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 25 0 0 0] 1120 1136 nil 27 5986 6032 1 6048 1 6064 1 6018 #fixedViewTop 51 170 192 34 4 608 8 'cardContainer' 6096 8 'Toolbar' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 4 2674 nil true 34 9 2722 1 2754 #fileNew 8 '&New Workspace' 9373 1 nil nil nil 2722 1 2754 #fileOpen 8 '&Open Workspace...' 9375 1 nil nil nil 2722 1 2754 #fileSave 8 '&Save' 9383 1 nil nil nil 2722 1 2754 #fileSaveAs 8 'Save &As...' 1 1 nil nil nil 2722 1 2754 #fileRevert 8 '&Revert' 1025 1 nil nil nil 2850 4097 2722 1 2754 #resetCursor 8 'Reset &Cursor' 1025 1 nil nil nil 2850 4097 2722 1 2754 #exit 8 'E&xit Jadeite' 17639 1 nil nil nil 8 '&File' nil 1 nil nil 26369 nil nil 2674 nil true 34 15 2722 1 2754 #undo 8 '&Undo' 9397 1 nil nil nil 2722 1 2754 #redo 8 'R&edo' 9395 1 nil nil nil 2850 4097 2722 1 2754 #editCut 8 'Cu&t' 9393 1 nil nil nil 2722 1 2754 #editCopy 8 '&Copy' 9351 1 nil nil nil 2722 1 2754 #editPaste 8 '&Paste' 9389 1 nil nil nil 2722 1 2754 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 2722 1 2754 #editDelete 8 '&Delete' 1629 1 nil nil nil 2850 4097 2722 1 2754 #editFind 8 '&Find...' 9357 1 nil nil nil 2722 1 2754 #editFindNext 8 'Find &Next' 1253 1 nil nil nil 2722 1 2754 #editReplace 8 '&Replace...' 9361 1 nil nil nil 2850 4097 2722 1 2754 #addQuotesToSelection 8 'Add &Quotes' 1 1 nil nil nil 2722 1 2754 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 nil nil nil 8 '&Edit' nil 1 nil nil 26395 nil nil 2674 nil true 34 20 2722 1 2754 #sleepAndCommit 8 '&Sleep and Commit' 1 1 nil nil nil 2722 1 2754 #abortTransaction 8 '&Abort Transaction' 1 1 nil nil nil 2722 1 2754 #commitTransaction 8 '&Commit Transaction' 1 1 nil nil nil 2722 1 2754 #disableNativeCode 8 'Disable Native Code' 1 1 nil nil nil 2850 4097 2722 1 2754 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 2722 1 2754 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 2722 1 2754 #jadeExecute 8 '&Execute' 9355 1 nil nil nil 2722 1 2754 #jadeDebug 8 'Debu&g' 9359 1 nil nil nil 2722 1 2754 #fileIn 8 'Fi&le In' 1 1 nil nil nil 2722 1 2754 #openJadeiteProjectsBrowser 8 'Browse &Projects' 9377 1 nil nil nil 2722 1 2754 #openDictionaryBrowser 8 '&Browse Dictionaries' 9349 1 nil nil nil 2722 1 2754 #jadeBrowseObjectLog 8 'Browse &Object Log' 9369 1 nil nil nil 2722 1 2754 #browseProcesses 8 'Browse P&rocesses' 9381 1 nil nil nil 2722 1 2754 #jadeBrowseTests 8 'Browse SUnit &Tests' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'SUnitAbsBrowser.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil nil 2850 4097 2722 1 2754 #inspectOop 8 'Inspect Oop ...' 1 1 nil nil nil 2722 1 2754 #describeErrorNumber 8 'Description for Error Number ...' 1025 1 nil nil nil 2722 1025 2754 #flipTranscript 8 'Transcript writes enabled' 1025 1 nil nil nil 2722 1025 2754 #flipDebugger 8 'Jadeite debugger enabled' 1025 1 nil nil nil 8 '&Jadeite' nil 1 nil nil 26433 nil nil 2674 nil true 34 1 2722 1 2754 #aboutJadeite 8 '&About Jadeite' 1 1 nil nil nil 8 '&Help' nil 1 nil nil 26437 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 8274 nil true 8320 8 'icons\GS32.ico' nil 8274 nil true 8320 8 'icons\GS16.ico' nil nil nil 1 nil nil 930 138 144 34 3 994 #createAt:extent: 34 2 514 7679 21 514 1201 801 416 994 #text: 34 1 8 'Jadeite Session' 416 994 #updateMenuBar 1120 416 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 87 17 0 0 154 1 0 0] 34 2 6096 608 1136 nil 27 )! !
!RowanTranscript class categoriesFor: #resource_Default_view!public!resources-views! !

