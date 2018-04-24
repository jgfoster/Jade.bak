"Filed out from Dolphin Smalltalk 7"!

JadeInspector subclass: #JadeNavigationInspector
	instanceVariableNames: 'oopsCollection objectIndex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeNavigationInspector guid: (GUID fromString: '{fd0e32b8-c1ae-427c-9ba2-ae30b7d8cff9}')!
JadeNavigationInspector comment: ''!
!JadeNavigationInspector categoriesForClass!Unclassified! !
!JadeNavigationInspector methodsFor!

addOop: aGsOop

	aGsOop isNil ifTrue: [^self].
	(aGsOop isKindOf: String) ifTrue: [^self].
	(oopsCollection includes: aGsOop value) ifTrue: [^self].
	oopsCollection add: aGsOop value.
	objectIndex := objectIndex + 1.
!

getNextObject

	objectIndex := (objectIndex >= oopsCollection size) 
				ifTrue: [1]
				ifFalse:[objectIndex + 1].

	self setNewInspectedObject.
!

getPreviousObject

	objectIndex := (objectIndex <= 1) 
				ifTrue: [oopsCollection size]
				ifFalse:[objectIndex - 1].

	self setNewInspectedObject.!

initialize

	super initialize.

	oopsCollection := OrderedCollection new.

	objectIndex := 0.!

inspectInstVar
	| oop |

	oop := instVarListPresenter selection value. "instVarListPresenter selection is <anAssociation>"

	(oop isKindOf: String) ifTrue: [^self].
	self model: oop.

	self addOop: oop.

	self setDocumentPresenterLabel.

	self displayObject.!

objectIndex
	^objectIndex!

objectIndex: anObject
	objectIndex := anObject!

onViewOpened

	super onViewOpened.

	self addOop: self model.!

oopsCollection
	^oopsCollection!

oopsCollection: anObject
	oopsCollection := anObject!

openNormalInspector

	instVarListPresenter hasSelection ifFalse: [^JadeInspector showOn: (self gciSession oopTypeWithOop: oopsCollection first) session: gciSession].

	^(instVarListPresenter selection key = 'self')
		ifTrue: [JadeInspector showOn: (self gciSession oopTypeWithOop: oopsCollection first) session: gciSession] 
		ifFalse: [JadeInspector showOn: instVarListPresenter selection value session: gciSession]
!

removeObject

	(oopsCollection size > 1) ifFalse: [^self].

	oopsCollection remove: self model value ifAbsent: [^self].

	objectIndex := (objectIndex <= 1) 
				ifTrue: [oopsCollection size]
				ifFalse:[objectIndex - 1].

	self setNewInspectedObject.
!

selectedInstVar

	instVarListPresenter hasSelection ifFalse: [^self].

	super selectedInstVar.!

setNewInspectedObject

	self model: (self gciSession oopTypeWithOop: (oopsCollection at: objectIndex)).

	self setDocumentPresenterLabel.

	self displayObject.
! !
!JadeNavigationInspector categoriesFor: #addOop:!public! !
!JadeNavigationInspector categoriesFor: #getNextObject!public! !
!JadeNavigationInspector categoriesFor: #getPreviousObject!public! !
!JadeNavigationInspector categoriesFor: #initialize!public! !
!JadeNavigationInspector categoriesFor: #inspectInstVar!public! !
!JadeNavigationInspector categoriesFor: #objectIndex!accessing!private! !
!JadeNavigationInspector categoriesFor: #objectIndex:!accessing!private! !
!JadeNavigationInspector categoriesFor: #onViewOpened!public! !
!JadeNavigationInspector categoriesFor: #oopsCollection!accessing!private! !
!JadeNavigationInspector categoriesFor: #oopsCollection:!accessing!private! !
!JadeNavigationInspector categoriesFor: #openNormalInspector!public! !
!JadeNavigationInspector categoriesFor: #removeObject!public! !
!JadeNavigationInspector categoriesFor: #selectedInstVar!public! !
!JadeNavigationInspector categoriesFor: #setNewInspectedObject!public! !

!JadeNavigationInspector class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13565952 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 801 601 551 0 0 0 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 576 0 721158 ##(Smalltalk.SystemColor) 31 0 7 0 0 0 576 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 720 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 769 49 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 24 0 0 0] 34 1 410 ##(Smalltalk.Toolbar) 34 25 0 576 34 2 8 1409288972 131137 960 0 466 8 4278190080 0 519 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 0 960 466 1040 8 4294905247 170 192 720 170 192 34 8 17935 1115910 ##(Smalltalk.ToolbarIconButton) 17935 0 960 1 1180998 4 ##(Smalltalk.CommandDescription) #getPreviousObject 8 'Previous Object' 1 1 263494 3 ##(Smalltalk.Icon) 0 true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'HistoryBack.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 395334 3 ##(Smalltalk.Bitmap) 0 true 0 0 0 0 3 514 33 33 1 17937 1234 17937 0 960 1 1266 #getNextObject 8 'Next Object' 1 1 1314 0 true 1360 8 'HistoryForward.ico' 1408 1442 0 true 0 0 0 0 3 514 33 33 1 17939 1234 17939 0 960 1 1266 #removeObject 8 'Remove Object' 1 1 1314 0 true 1360 8 'False.ico' 1408 1442 0 true 0 0 0 0 3 514 33 33 1 17941 1234 17941 0 960 1 1266 #openNavigationInspector 8 'Open Navigation Inspector' 1 1 1314 0 true 1360 8 'BasicInspector.ico' 1408 1442 0 true 0 0 0 0 3 514 33 33 1 34 6 1248 1488 1050118 ##(Smalltalk.ToolbarSeparator) 0 0 960 3 0 1 1600 1842 0 0 960 3 0 1 1712 170 176 34 8 1568 3 1680 5 1792 7 1456 1 0 1 0 514 33 33 514 45 45 0 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 754 138 144 34 2 818 #createAt:extent: 34 2 514 1 1 514 769 49 960 818 #updateSizePosted 720 960 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 24 0 0 0] 8 #() 514 193 193 0 27 2160 0 27 0 0 0 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 2176 0 642 31 0 7 0 0 0 2176 674 170 176 720 false 170 192 34 4 410 ##(Smalltalk.ListBox) 34 17 0 2176 34 2 8 1144062209 1025 2320 590662 2 ##(Smalltalk.ListModel) 138 144 720 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 0 7 0 0 0 2320 0 8 4294905191 459270 ##(Smalltalk.Message) #key 8 #() 720 false 754 138 144 34 2 818 #createAt:extent: 34 2 514 1 1 514 379 435 2320 818 #horizontalExtent: 8 #(0) 2320 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 189 0 0 0 217 0 0 0] 8 #() 2160 0 27 8 'instVarList' 410 ##(Smalltalk.ReferenceView) 34 14 0 2176 34 2 8 1140850688 131073 2768 0 466 8 4278190080 0 7 0 0 0 2768 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.CodeSourcePresenter) #resource_Default_view 0 754 138 144 34 1 818 #createAt:extent: 34 2 514 389 1 514 381 435 2768 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 194 0 0 0 0 0 0 0 128 1 0 0 217 0 0 0] 720 2160 0 27 8 'codePane' 0 754 138 144 34 1 818 #createAt:extent: 34 2 514 1 49 514 769 435 2176 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 24 0 0 0 128 1 0 0 241 0 0 0] 34 3 2320 410 ##(Smalltalk.Splitter) 34 12 0 2176 34 2 8 1140850688 1 3216 0 466 8 4278190080 0 519 0 0 0 3216 754 138 144 34 1 818 #createAt:extent: 34 2 514 379 1 514 11 435 3216 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 189 0 0 0 0 0 0 0 194 0 0 0 217 0 0 0] 8 #() 2160 0 27 2768 2160 0 27 170 192 720 0 461638 4 ##(Smalltalk.MenuBar) 0 true 34 4 265030 4 ##(Smalltalk.Menu) 0 true 34 7 984134 2 ##(Smalltalk.CommandMenuItem) 1 1266 #fileNew 8 '&New Workspace' 9373 1 0 0 0 3586 1 1266 #fileOpen 8 '&Open Workspace...' 9375 1 0 0 0 3586 1 1266 #fileSave 8 '&Save' 9383 1 0 0 0 3586 1 1266 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 3586 1 1266 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 3586 1 1266 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 17887 0 0 3538 0 true 34 15 3586 1 1266 #undo 8 '&Undo' 9397 1 0 0 0 3586 1 1266 #redo 8 'R&edo' 9395 1 0 0 0 3842 4097 3586 1 1266 #editCut 8 'Cu&t' 9393 1 0 0 0 3586 1 1266 #editCopy 8 '&Copy' 9351 1 0 0 0 3586 1 1266 #editPaste 8 '&Paste' 9389 1 0 0 0 3586 1 1266 #editSelectAll 8 'Select &All' 9347 1 0 0 0 3586 1 1266 #editDelete 8 '&Delete' 1629 1 0 0 0 3842 4097 3586 1 1266 #editFind 8 '&Find...' 9357 1 0 0 0 3586 1 1266 #editFindNext 8 'Find &Next' 9359 1 0 0 0 3586 1 1266 #editReplace 8 '&Replace...' 9361 1 0 0 0 3842 4097 3586 1 1266 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 3586 1 1266 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 17913 0 0 3538 0 true 34 9 3586 1 1266 #abortTransaction 8 '&Abort Transaction' 1 1 0 0 0 3586 1 1266 #commitTransaction 8 '&Commit Transaction' 1 1 0 0 0 3842 4097 3586 1 1266 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 3586 1 1266 #jadeDisplay 8 '&Display' 9353 1 0 0 0 3586 1 1266 #jadeExecute 8 '&Execute' 9355 1 0 0 0 3586 1 1266 #fileIn 8 'Fi&le In' 1 1 0 0 0 3842 4097 3586 1 1266 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 0 0 0 8 '&Jade' 0 1 0 0 17929 0 0 3538 0 true 34 1 3586 1 1266 #aboutJadeite 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 17933 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 514 2719 21 514 801 601 416 818 #text: 34 1 8 'Jade Object Navigation Inspector' 416 818 #updateMenuBar 720 416 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 79 5 0 0 10 0 0 0 223 6 0 0 54 1 0 0] 34 2 576 2176 2160 0 27)! !
!JadeNavigationInspector class categoriesFor: #resource_Default_view!public! !

