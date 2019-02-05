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
				ifTrue: [^self]
				ifFalse:[objectIndex + 1].

	self setNewInspectedObject.
!

getPreviousObject

	objectIndex := (objectIndex <= 1) 
				ifTrue: [^self]
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

queryCommand: aCommandQuery
	| cmd |

	cmd := aCommandQuery commandSymbol.

	cmd == #getNextObject ifTrue: [aCommandQuery isEnabled: (objectIndex < oopsCollection size). ^true].
	cmd == #getPreviousObject ifTrue: [aCommandQuery isEnabled: (objectIndex > 1). ^true].

	super queryCommand: aCommandQuery.!

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
!JadeNavigationInspector categoriesFor: #getNextObject!object navigation!public! !
!JadeNavigationInspector categoriesFor: #getPreviousObject!object navigation!public! !
!JadeNavigationInspector categoriesFor: #initialize!public! !
!JadeNavigationInspector categoriesFor: #inspectInstVar!public! !
!JadeNavigationInspector categoriesFor: #objectIndex!accessing!private! !
!JadeNavigationInspector categoriesFor: #objectIndex:!accessing!private! !
!JadeNavigationInspector categoriesFor: #onViewOpened!public! !
!JadeNavigationInspector categoriesFor: #oopsCollection!accessing!private! !
!JadeNavigationInspector categoriesFor: #oopsCollection:!accessing!private! !
!JadeNavigationInspector categoriesFor: #openNormalInspector!public! !
!JadeNavigationInspector categoriesFor: #queryCommand:!object navigation!public! !
!JadeNavigationInspector categoriesFor: #removeObject!public! !
!JadeNavigationInspector categoriesFor: #selectedInstVar!public! !
!JadeNavigationInspector categoriesFor: #setNewInspectedObject!object navigation!public! !

!JadeNavigationInspector class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 801 601 549 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 576 nil 721158 ##(Smalltalk.SystemColor) 31 nil 7 nil nil nil 576 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 720 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 769 49 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 24 0 0 0] 34 1 410 ##(Smalltalk.Toolbar) 34 25 nil 576 34 2 8 1409288972 131137 960 nil 480 nil 519 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 960 480 8 1949831345 170 192 720 170 192 34 8 53511 1115206 1 ##(Smalltalk.ToolbarIconButton) 960 1 1180998 4 ##(Smalltalk.CommandDescription) #getPreviousObject 8 'Previous Object' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'HistoryBack.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil 53511 53513 1186 960 1 1218 #getNextObject 8 'Next Object' 1 1 1266 nil true 1312 8 'HistoryForward.ico' 1360 53513 53515 1186 960 1 1218 #removeObject 8 'Remove Object' 1 1 788806 1 ##(Smalltalk.TextTileIcon) $\x2716 nil nil 786694 ##(Smalltalk.IndexedColor) 33554459 nil nil 1 nil nil 53515 53517 1186 960 1 1218 #openNavigationInspector 8 'Open Navigation Inspector' 1 1 1266 nil true 1312 8 'BasicInspector.ico' 1360 53517 34 6 1200 1392 1049158 1 ##(Smalltalk.ToolbarSeparator) 960 1 1472 1682 960 1 1584 nil nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 754 138 144 34 2 818 #createAt:extent: 34 2 514 1 1 514 769 49 960 818 #updateSizePosted 720 960 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 24 0 0 0] 8 #() 514 193 193 nil 27 1968 nil 27 nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 1984 nil 656 nil 7 nil nil nil 1984 674 170 176 720 false 170 192 34 4 410 ##(Smalltalk.ReferenceView) 34 14 nil 1984 34 2 8 1140850688 131073 2112 nil 480 nil 7 nil nil nil 2112 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.CodeSourcePresenter) #resource_Default_view nil 754 138 144 34 1 818 #createAt:extent: 34 2 514 389 1 514 381 435 2112 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 194 0 0 0 0 0 0 0 128 1 0 0 217 0 0 0] 720 1968 nil 27 8 'codePane' 410 ##(Smalltalk.ListBox) 34 17 nil 1984 34 2 8 1144062209 1025 2368 590662 2 ##(Smalltalk.ListModel) 138 144 720 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 480 nil 7 nil nil nil 2368 nil 8 1949515567 459270 ##(Smalltalk.Message) #key 8 #() 720 false 754 138 144 34 2 818 #createAt:extent: 34 2 514 1 1 514 379 435 2368 818 #horizontalExtent: 8 #(0) 2368 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 189 0 0 0 217 0 0 0] 8 #() 1968 nil 27 8 'instVarList' nil 754 138 144 34 1 818 #createAt:extent: 34 2 514 1 49 514 769 435 1984 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 24 0 0 0 128 1 0 0 241 0 0 0] 34 3 2368 410 ##(Smalltalk.Splitter) 34 12 nil 1984 34 2 8 1140850688 1 2944 nil 480 nil 519 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 2944 nil 1 #left nil nil nil 514 1 1 514 9 9 nil 3040 nil 754 138 144 34 1 818 #createAt:extent: 34 2 514 379 1 514 11 435 2944 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 189 0 0 0 0 0 0 0 194 0 0 0 217 0 0 0] 8 #() 1968 nil 27 2112 1968 nil 27 170 192 720 nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 4 265030 4 ##(Smalltalk.Menu) nil true 34 7 984134 2 ##(Smalltalk.CommandMenuItem) 1 1218 #fileNew 8 '&New Workspace' 9373 1 nil nil nil 3346 1 1218 #fileOpen 8 '&Open Workspace...' 9375 1 nil nil nil 3346 1 1218 #fileSave 8 '&Save' 9383 1 nil nil nil 3346 1 1218 #fileSaveAs 8 'Save &As...' 1 1 nil nil nil 3346 1 1218 #fileRevert 8 '&Revert' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 3346 1 1218 #exit 8 'E&xit Jade' 17639 1 nil nil nil 8 '&File' nil 1 nil nil 53463 nil nil 3298 nil true 34 15 3346 1 1218 #undo 8 '&Undo' 9397 1 nil nil nil 3346 1 1218 #redo 8 'R&edo' 9395 1 nil nil nil 3602 4097 3346 1 1218 #editCut 8 'Cu&t' 9393 1 nil nil nil 3346 1 1218 #editCopy 8 '&Copy' 9351 1 nil nil nil 3346 1 1218 #editPaste 8 '&Paste' 9389 1 nil nil nil 3346 1 1218 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 3346 1 1218 #editDelete 8 '&Delete' 1629 1 nil nil nil 3602 4097 3346 1 1218 #editFind 8 '&Find...' 9357 1 nil nil nil 3346 1 1218 #editFindNext 8 'Find &Next' 9359 1 nil nil nil 3346 1 1218 #editReplace 8 '&Replace...' 9361 1 nil nil nil 3602 4097 3346 1 1218 #addQuotesToSelection 8 'Add &Quotes' 1 1 nil nil nil 3346 1 1218 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 nil nil nil 8 '&Edit' nil 1 nil nil 53489 nil nil 3298 nil true 34 9 3346 1 1218 #abortTransaction 8 '&Abort Transaction' 1 1 nil nil nil 3346 1 1218 #commitTransaction 8 '&Commit Transaction' 1 1 nil nil nil 3602 4097 3346 1 1218 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 3346 1 1218 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 3346 1 1218 #jadeExecute 8 '&Execute' 9355 1 nil nil nil 3346 1 1218 #fileIn 8 'Fi&le In' 1 1 nil nil nil 3602 4097 3346 1 1218 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 nil nil nil 8 '&Jade' nil 1 nil nil 53505 nil nil 3298 nil true 34 1 3346 1 1218 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 1 nil nil 53509 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 754 138 144 34 3 818 #createAt:extent: 34 2 514 3059 21 514 801 601 416 818 #text: 34 1 8 'Jade Object Navigation Inspector' 416 818 #updateMenuBar 720 416 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 5 0 0 10 0 0 0 137 7 0 0 54 1 0 0] 34 2 576 1984 1968 nil 27 )! !
!JadeNavigationInspector class categoriesFor: #resource_Default_view!public!resources-views! !

