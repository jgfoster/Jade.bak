"Filed out from Dolphin Smalltalk 7"!

JadeTranscript subclass: #RowanTranscript
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanTranscript guid: (GUID fromString: '{93b3c6ce-9a55-45db-bd1e-cc1a44a28e4e}')!
RowanTranscript comment: ''!
!RowanTranscript categoriesForClass!Unclassified! !
!RowanTranscript methodsFor!

flipDebugger
	RowanDebugger debuggerClass == RowanDebugger
		ifTrue: [RowanDebugger debuggerClass: JadeDebugger]
		ifFalse: [RowanDebugger debuggerClass: RowanDebugger]!

flipTranscript
	| answeringService |
	answeringService := RowanAnsweringService new.
	answeringService command: #flipTranscript.
	self issueCommand: (Array with: answeringService)!

isTranscriptInstalled
	| answeringService |
	answeringService := RowanAnsweringService new.
	answeringService command: #transcriptInstalled.
	self issueCommand: (Array with: answeringService).
	^answeringService answer!

openDictionaryBrowser
	gciSession hasServer ifTrue: [^RowanSystemBrowser showOnSession: gciSession]!

openJadeiteProjectsBrowser
	gciSession hasServer ifTrue: [^JadeiteProjectsSystemBrowser showOnSession: gciSession].
	MessageBox warning: 'Cannot open Jadeite Projects browser'
		caption: 'Cannot open Jadeite Projects browser'!

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
!RowanTranscript categoriesFor: #flipDebugger!menu handlers!public! !
!RowanTranscript categoriesFor: #flipTranscript!menu handlers!public! !
!RowanTranscript categoriesFor: #isTranscriptInstalled!public! !
!RowanTranscript categoriesFor: #openDictionaryBrowser!menu handlers!public! !
!RowanTranscript categoriesFor: #openJadeiteProjectsBrowser!menu handlers!public! !
!RowanTranscript categoriesFor: #queryCommand:!private! !

!RowanTranscript class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 551 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 608 nil nil nil 7 nil nil nil 608 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeToolsToolBarPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 1169 51 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 25 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 962 #fixedParentRight 1 962 #fixedParentTop 1 962 #fixedViewTop 51 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1140850688 131073 1040 nil 466 496 nil 7 nil nil nil 1040 655878 ##(Smalltalk.CardLayout) 138 144 34 6 721414 ##(Smalltalk.Association) 8 'Workspace' 410 ##(Smalltalk.ReferenceView) 34 14 nil 1040 34 2 8 1140850688 131073 1232 nil 466 8 4278190080 nil 7 nil nil nil 1232 674 ##(Smalltalk.CodeSourcePresenter) #resource_Default_view nil 706 138 144 34 1 770 #createAt:extent: 34 2 514 9 55 514 1153 571 1232 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 896 912 nil 27 1186 8 'My Session' 410 ##(Smalltalk.ReferenceView) 34 14 nil 1040 34 2 8 1140850688 131073 1520 nil nil nil 5 nil nil nil 1520 674 ##(Smalltalk.MySessionPresenter) #resource_Default_view nil 706 138 144 34 1 770 #createAt:extent: 34 2 514 9 55 514 1153 571 1520 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 896 912 nil 27 1186 8 'All Sessions' 410 ##(Smalltalk.ReferenceView) 34 14 nil 1040 34 2 8 1140850688 131073 1776 nil nil nil 5 nil nil nil 1776 674 ##(Smalltalk.AllSessionsPresenter) #resource_Default_view nil 706 138 144 34 1 770 #createAt:extent: 34 2 514 9 55 514 1153 571 1776 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 896 912 nil 27 1186 8 'Transcript' 410 ##(Smalltalk.ReferenceView) 34 14 nil 1040 34 2 8 1140916224 131073 2032 nil 466 8 4278190080 nil 5 nil nil nil 2032 674 ##(Smalltalk.TranscriptPresenter) #resource_Default_view nil 706 138 144 34 1 770 #createAt:extent: 34 2 514 9 55 514 1153 571 2032 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 896 912 nil 27 1186 8 'History' 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 1040 34 2 8 1143017796 1025 2320 nil 466 8 4278190080 nil 5 nil nil nil 2320 nil 8 4294906431 852486 ##(Smalltalk.NullConverter) nil nil 11 706 138 144 34 1 770 #createAt:extent: 34 2 514 9 55 514 1153 571 2320 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 8 #() 912 nil 27 1186 ##(Smalltalk.Stone) 410 ##(Smalltalk.ReferenceView) 34 14 nil 1040 34 2 8 1140850688 131073 2640 nil nil nil 5 nil nil nil 2640 674 ##(Smalltalk.StoneInfoPresenter) #resource_Default_view nil 706 138 144 34 1 770 #createAt:extent: 34 2 514 9 55 514 1153 571 2640 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 56 1 0 0] 896 912 nil 27 1232 170 192 34 12 1232 8 'codePane' 1520 8 'My Session' 1776 8 'All Sessions' 2320 8 'history' 2032 8 'Transcript' 2640 8 'Stone' nil 410 ##(Smalltalk.TabView) 34 23 nil 1040 34 2 8 1140916736 1 2992 590662 2 ##(Smalltalk.ListModel) 138 144 34 6 2016 2304 ##(Smalltalk.Stone) 1504 1760 1216 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 721158 ##(Smalltalk.SystemColor) 31 nil 1 nil nil nil 2992 nil 8 4294905791 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #noIcons 706 138 144 34 3 770 #createAt:extent: 34 2 514 1 1 514 1169 633 2992 770 #basicSelectionsByIndex: 34 1 8 #(6) 2992 770 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2992 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 60 1 0 0] 8 #() 912 nil 27 706 138 144 34 1 770 #createAt:extent: 34 2 514 1 51 514 1169 633 1040 850 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 72 2 0 0 85 1 0 0] 34 7 2032 2320 2640 1520 1776 1232 2992 912 nil 27 930 976 1 992 1 1008 51 962 #fixedParentBottom 1 170 192 34 4 608 8 'Toolbar' 1040 8 'cardContainer' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 4 265030 4 ##(Smalltalk.Menu) nil true 34 9 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #fileNew 8 '&New Workspace' 9373 1 nil nil nil 3826 1 3858 #fileOpen 8 '&Open Workspace...' 9375 1 nil nil nil 3826 1 3858 #fileSave 8 '&Save' 9383 1 nil nil nil 3826 1 3858 #fileSaveAs 8 'Save &As...' 1 1 nil nil nil 3826 1 3858 #fileRevert 8 '&Revert' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 3826 1 3858 #resetCursor 8 'Reset &Cursor' 1025 1 nil nil nil 4098 4097 3826 1 3858 #exit 8 'E&xit Jadeite' 17639 1 nil nil nil 8 '&File' nil 1 nil nil 33971 nil nil 3778 nil true 34 15 3826 1 3858 #undo 8 '&Undo' 9397 1 nil nil nil 3826 1 3858 #redo 8 'R&edo' 9395 1 nil nil nil 4098 4097 3826 1 3858 #editCut 8 'Cu&t' 9393 1 nil nil nil 3826 1 3858 #editCopy 8 '&Copy' 9351 1 nil nil nil 3826 1 3858 #editPaste 8 '&Paste' 9389 1 nil nil nil 3826 1 3858 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 3826 1 3858 #editDelete 8 '&Delete' 1629 1 nil nil nil 4098 4097 3826 1 3858 #editFind 8 '&Find...' 9357 1 nil nil nil 3826 1 3858 #editFindNext 8 'Find &Next' 1253 1 nil nil nil 3826 1 3858 #editReplace 8 '&Replace...' 9361 1 nil nil nil 4098 4097 3826 1 3858 #addQuotesToSelection 8 'Add &Quotes' 1 1 nil nil nil 3826 1 3858 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 nil nil nil 8 '&Edit' nil 1 nil nil 33997 nil nil 3778 nil true 34 21 3826 1 3858 #sleepAndCommit 8 '&Sleep and Commit' 1 1 nil nil nil 3826 1 3858 #abortTransaction 8 '&Abort Transaction' 1 1 nil nil nil 3826 1 3858 #commitTransaction 8 '&Commit Transaction' 1 1 nil nil nil 3826 1 3858 #disableNativeCode 8 'Disable Native Code' 1 1 nil nil nil 4098 4097 3826 1 3858 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 3826 1 3858 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 3826 1 3858 #jadeExecute 8 '&Execute' 9355 1 nil nil nil 3826 1 3858 #jadeDebug 8 'Debu&g' 9359 1 nil nil nil 3826 1 3858 #fileIn 8 'Fi&le In' 1 1 nil nil nil 3826 1 3858 #openRowanProjectList 8 'Browse Project List' 1 1 nil nil nil 3826 1 3858 #openJadeiteProjectsBrowser 8 'Browse &Projects' 9377 1 nil nil nil 3826 1 3858 #openDictionaryBrowser 8 '&Browse Dictionaries' 9349 1 nil nil nil 3826 1 3858 #jadeBrowseObjectLog 8 'Browse &Object Log' 9369 1 nil nil nil 3826 1 3858 #browseProcesses 8 'Browse P&rocesses' 9381 1 nil nil nil 3826 1 3858 #jadeBrowseTests 8 'Browse SUnit &Tests' 1 1 nil nil nil 4098 4097 3826 1 3858 #inspectOop 8 'Inspect Oop ...' 1 1 nil nil nil 3826 1 3858 #describeErrorNumber 8 'Description for Error Number ...' 1025 1 nil nil nil 3826 1025 3858 #flipTranscript 8 'Transcript writes enabled' 1025 1 nil nil nil 3826 1025 3858 #flipDebugger 8 'Jadeite debugger enabled' 1025 1 nil nil nil 8 '&Jadeite' nil 1 nil nil 34037 nil nil 3778 nil true 34 1 3826 1 3858 #aboutJadeite 8 '&About Jadeite' 1 1 nil nil nil 8 '&Help' nil 1 nil nil 34041 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'icons\GS32.ico' nil 6034 nil true 6080 8 'icons\GS16.ico' nil nil nil 1 nil nil 706 138 144 34 3 770 #createAt:extent: 34 2 514 7679 21 514 1201 801 416 770 #text: 34 1 8 'Jadeite Session' 416 770 #updateMenuBar 896 416 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 87 17 0 0 154 1 0 0] 34 2 608 1040 912 nil 27 )! !
!RowanTranscript class categoriesFor: #resource_Default_view!public!resources-views! !

