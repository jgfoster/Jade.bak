"Filed out from Dolphin Smalltalk 7"!

JadeMethodListPresenter subclass: #JadeTreeMethodListPresenter
	instanceVariableNames: 'treeMethodsPresenter treeMethodsMap historyEntries entriesIndex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeTreeMethodListPresenter guid: (GUID fromString: '{559c51b0-b6cb-4ee5-877d-5d232334437c}')!
JadeTreeMethodListPresenter comment: ''!
!JadeTreeMethodListPresenter categoriesForClass!Unclassified! !
!JadeTreeMethodListPresenter methodsFor!

addList
	| node |

	node := Prompter prompt: 'Enter List Name'.

	treeMethodsPresenter model add: node asChildOf: treeMethodsPresenter selectionOrNil.

	treeMethodsPresenter expand: treeMethodsPresenter selectionOrNil.

	treeMethodsMap at: node put: SortedCollection new.

	^node!

addTreeListRoot: methodName type: browseType
	| node |

	node := self getLabelForMethod: methodName type: browseType .

	treeMethodsPresenter model: (TreeModel withRoots: (Array with: node)).

	^node!

browse: performSelector method: aGsMethodOrString

	| string |
	(aGsMethodOrString isKindOf: String) ifTrue: [
		self browse: performSelector methodSelector: aGsMethodOrString.
		string := aGsMethodOrString.
	] ifFalse: [
		self browse: performSelector gsMethod: aGsMethodOrString.
		string := aGsMethodOrString name.
	].
	self selecting: string.!

browse: performSelector methodSelector: aString

	| string |
	string := self gciSession 
		serverPerform: performSelector
		with: aString.
	self browseMethodsFromString: string.!

browseImplementorsOf: aGsMethod
 	| methodName |

	methodName := (aGsMethod class = String) ifTrue: [aGsMethod] ifFalse: [aGsMethod name].

	self browseMethodsFromString: (self gsBrowse: #implementorsOf: methodSelector: aGsMethod) methodName: methodName type: #definitions.!

browseMethodsFromString: aString
	| list |

	"self hasTreeListSelection ifFalse: [self addTreeListRoot]."

	list := GsMethod2
		listFromString: aString
		session: self gciSession.
	methodListPresenter list: list asSortedCollection.
	list notEmpty ifTrue: [
		"methodListPresenter selectFirstMethod."
	].!

browseMethodsFromString: aString methodName: methodName
	| list |

	self hasTreeListSelection ifFalse: [self addTreeListRoot: methodName].

	list := GsMethod2
		listFromString: aString
		session: self gciSession.
	methodListPresenter list: list asSortedCollection.
	list notEmpty ifTrue: [
		methodListPresenter selectFirstMethod.
	].
!

browseMethodsFromString: aString methodName: methodName type: browseType
	| list  node |

	node := self hasTreeListSelection 
			ifFalse: [self addTreeListRoot: methodName type: browseType] 
			ifTrue: [treeMethodsPresenter model add: (self getLabelForMethod: methodName type: browseType) asChildOf: treeMethodsPresenter selection.].

	list := GsMethod2
		listFromString: aString
		session: self gciSession.
	methodListPresenter list: list asSortedCollection.
	list notEmpty ifTrue: [
		"methodListPresenter selectFirstMethod."
	].

	treeMethodsMap at: node put: list asSortedCollection.

	treeMethodsPresenter selection: node!

browseSendersOf: aGsMethod
	| methodName |

	methodName := (aGsMethod class = String) ifTrue: [aGsMethod] ifFalse: [aGsMethod name].

	self browseMethodsFromString: (self gsBrowse: #sendersOf: methodSelector: aGsMethod) methodName: methodName type: #references.
!

buildHistoryBackMenu

	^(self buildHistoryMenuWith: (historyEntries copyFrom: 1 to: entriesIndex) command: #historyBack:)
		text: 'Backward';
		yourself!

buildHistoryForwardMenu

	^(self buildHistoryMenuWith: (historyEntries copyFrom: entriesIndex to: historyEntries size) command: #historyForward:)
		text: 'Forward';
		yourself!

buildHistoryMenuWith: entryCollection command: command
	| popup subMenu classesNames |

	classesNames := Set new.
	popup := Menu new.

	(entryCollection select: [:each | each notNil]) do: [:historyEntry | 
		(classesNames includes: historyEntry getClassName) 
		ifFalse: [subMenu := popup addSubmenu: historyEntry getClassName.
				classesNames add: historyEntry getClassName].
		subMenu addCommand: (Message selector: command argument: historyEntry) description: historyEntry getMethodName.
		subMenu setDefault: 1
	].

	popup setDefault: 1.
	^popup!

createComponents

	super createComponents.

	treeMethodsPresenter := self add: TreePresenter new name: 'treeMethods'.!

createSchematicWiring

	super createSchematicWiring.

	treeMethodsPresenter 
		when: #selectionChanged send: #onTreeListChanged to: self;
		when: #dragOver: send: #onDragOverMethodsTree: to: self;
		when: #drop: send: #onDropMethodsTree: to: self;
		when: #drag: send: #onDragMethodsTree: to: self.

	methodListPresenter when: #methodSelectionChanged send: #onMethodSelectionChanged to: self.!

deleteHistory

	historyEntries := OrderedCollection new.
	entriesIndex := 1.!

dropGsMethod: aDragDropSession 

	(treeMethodsMap at: aDragDropSession suggestedTarget ifAbsent: [^self]) add: aDragDropSession suggestedSource.!

dropNode: aDragDropSession

	treeMethodsPresenter model move: aDragDropSession suggestedSource asChildOf: aDragDropSession suggestedTarget.

	treeMethodsPresenter expand: aDragDropSession suggestedTarget.!

entriesIndex
	^entriesIndex!

entriesIndex: anObject
	entriesIndex := anObject!

getLabelForMethod: methodName type: browseType

	^(browseType = #definitions) ifTrue: ['Implementors of ', methodName] ifFalse: ['Senders of ', methodName]!

gsBrowse: performSelector methodSelector: aString

	^self gciSession serverPerform: performSelector with: aString!

hasTreeListSelection

	^treeMethodsPresenter hasSelection!

historyBack

	entriesIndex := entriesIndex - 1.
	(entriesIndex <= 0) ifTrue: [entriesIndex := 1].

	(historyEntries size = 0) ifTrue: [^self].

	self restoreFrom: (historyEntries at: entriesIndex).!

historyBack: aJadeHistoryEntry
	
	self restoreFrom: aJadeHistoryEntry!

historyEntries
	^historyEntries!

historyEntries: anObject
	historyEntries := anObject!

historyForward

	entriesIndex := entriesIndex + 1.
	(entriesIndex > historyEntries size) ifTrue: [entriesIndex := 1].

	(historyEntries size = 0) ifTrue: [^self].

	self restoreFrom: (historyEntries at: entriesIndex).!

historyForward: aJadeHistoryEntry

	self restoreFrom: aJadeHistoryEntry!

initialize

	super initialize.

	historyEntries := OrderedCollection new.
	entriesIndex := 1.
	treeMethodsMap := Dictionary new.!

onDragMethodsTree: aDragDropSession

!

onDragOverMethodsTree: aDragDropSession

	aDragDropSession operation: nil.
	aDragDropSession dragObjects isEmpty ifTrue: [^self].

	aDragDropSession
		supportedOperations: #(#'move');
		operation: #'move';
		yourself.!

onDropMethodsTree: aDragDropSession

	(aDragDropSession operation = #move) ifFalse: [^self].

	(aDragDropSession suggestedSource class = GsMethod2) ifTrue: [^self dropGsMethod: aDragDropSession].

	^self dropNode: aDragDropSession
!

onHistoryDropDown: aToolbarButton 
	"Private - The receiver's toolbar has sent a notification that a button's drop-down arrow
	has been pressed. Generate and pop-up the appropriate menu."
	| popup |

	aToolbarButton command == #historyBack ifTrue: [popup :=  self buildHistoryBackMenu].
	aToolbarButton command == #historyForward ifTrue: [popup := self buildHistoryForwardMenu].
	popup queryAllFromView: self topShell view.
	popup showIn: self topShell position: aToolbarButton screenRectangle bottomLeft.

	^0!

onMethodSelectionChanged
	| entry |

	self selectedMethod ifNil: [^self].

	entry := JadeMethodExplorerHistoryEntry newFrom: self.

	(historyEntries anySatisfy: [:each | each gsMethod == entry gsMethod]) ifFalse: [historyEntries add: entry].!

onTreeListChanged

	self hasTreeListSelection ifFalse: [^methodListPresenter model: ListModel new].

	methodListPresenter list: (treeMethodsMap at: treeMethodsPresenter selection ifAbsent: [#()]).

	"methodListPresenter selectFirstMethod"
!

onViewOpened
	| toolbar |

	super onViewOpened.

	methodSourcePresenter documentPresenter view styler: SmalltalkMethodStyler new.

	toolbar := self topShell view viewNamed: 'historyToolbar'.

	toolbar presenterConnectionPoint 
		when: #dropDown:
		send: #onHistoryDropDown:
		to: self.
!

removeQueryElement

	treeMethodsPresenter hasSelection ifFalse: [^self].

	treeMethodsPresenter model remove: treeMethodsPresenter selection ifAbsent: []!

restoreFrom: aJadeHistoryEntry

	treeMethodsPresenter selection: aJadeHistoryEntry treeSelection ifAbsent: [].

	methodListPresenter selectMethod: aJadeHistoryEntry gsMethod!

selectedExplorerNode

	^treeMethodsPresenter selectionOrNil!

selectedMethod

	^methodListPresenter selectionOrNil!

treeListHasSelection

	^treeMethodsPresenter hasSelection!

treeMethodsMap
	^treeMethodsMap!

treeMethodsMap: anObject
	treeMethodsMap := anObject!

treeMethodsPresenter
	^treeMethodsPresenter!

treeMethodsPresenter: anObject
	treeMethodsPresenter := anObject! !
!JadeTreeMethodListPresenter categoriesFor: #addList!public! !
!JadeTreeMethodListPresenter categoriesFor: #addTreeListRoot:type:!public! !
!JadeTreeMethodListPresenter categoriesFor: #browse:method:!public! !
!JadeTreeMethodListPresenter categoriesFor: #browse:methodSelector:!public! !
!JadeTreeMethodListPresenter categoriesFor: #browseImplementorsOf:!public! !
!JadeTreeMethodListPresenter categoriesFor: #browseMethodsFromString:!public! !
!JadeTreeMethodListPresenter categoriesFor: #browseMethodsFromString:methodName:!public! !
!JadeTreeMethodListPresenter categoriesFor: #browseMethodsFromString:methodName:type:!public! !
!JadeTreeMethodListPresenter categoriesFor: #browseSendersOf:!public! !
!JadeTreeMethodListPresenter categoriesFor: #buildHistoryBackMenu!public! !
!JadeTreeMethodListPresenter categoriesFor: #buildHistoryForwardMenu!public! !
!JadeTreeMethodListPresenter categoriesFor: #buildHistoryMenuWith:command:!public! !
!JadeTreeMethodListPresenter categoriesFor: #createComponents!public! !
!JadeTreeMethodListPresenter categoriesFor: #createSchematicWiring!public! !
!JadeTreeMethodListPresenter categoriesFor: #deleteHistory!public! !
!JadeTreeMethodListPresenter categoriesFor: #dropGsMethod:!public! !
!JadeTreeMethodListPresenter categoriesFor: #dropNode:!public! !
!JadeTreeMethodListPresenter categoriesFor: #entriesIndex!accessing!private! !
!JadeTreeMethodListPresenter categoriesFor: #entriesIndex:!accessing!private! !
!JadeTreeMethodListPresenter categoriesFor: #getLabelForMethod:type:!public! !
!JadeTreeMethodListPresenter categoriesFor: #gsBrowse:methodSelector:!public! !
!JadeTreeMethodListPresenter categoriesFor: #hasTreeListSelection!public! !
!JadeTreeMethodListPresenter categoriesFor: #historyBack!public! !
!JadeTreeMethodListPresenter categoriesFor: #historyBack:!public! !
!JadeTreeMethodListPresenter categoriesFor: #historyEntries!accessing!private! !
!JadeTreeMethodListPresenter categoriesFor: #historyEntries:!accessing!private! !
!JadeTreeMethodListPresenter categoriesFor: #historyForward!public! !
!JadeTreeMethodListPresenter categoriesFor: #historyForward:!public! !
!JadeTreeMethodListPresenter categoriesFor: #initialize!public! !
!JadeTreeMethodListPresenter categoriesFor: #onDragMethodsTree:!public! !
!JadeTreeMethodListPresenter categoriesFor: #onDragOverMethodsTree:!public! !
!JadeTreeMethodListPresenter categoriesFor: #onDropMethodsTree:!public! !
!JadeTreeMethodListPresenter categoriesFor: #onHistoryDropDown:!public! !
!JadeTreeMethodListPresenter categoriesFor: #onMethodSelectionChanged!public! !
!JadeTreeMethodListPresenter categoriesFor: #onTreeListChanged!public! !
!JadeTreeMethodListPresenter categoriesFor: #onViewOpened!public! !
!JadeTreeMethodListPresenter categoriesFor: #removeQueryElement!public! !
!JadeTreeMethodListPresenter categoriesFor: #restoreFrom:!public! !
!JadeTreeMethodListPresenter categoriesFor: #selectedExplorerNode!public! !
!JadeTreeMethodListPresenter categoriesFor: #selectedMethod!public! !
!JadeTreeMethodListPresenter categoriesFor: #treeListHasSelection!public! !
!JadeTreeMethodListPresenter categoriesFor: #treeMethodsMap!accessing!private! !
!JadeTreeMethodListPresenter categoriesFor: #treeMethodsMap:!accessing!private! !
!JadeTreeMethodListPresenter categoriesFor: #treeMethodsPresenter!accessing!private! !
!JadeTreeMethodListPresenter categoriesFor: #treeMethodsPresenter:!accessing!private! !

!JadeTreeMethodListPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 0 0 34 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 528 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 2719 21 674 1501 1011 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 5 0 0 10 0 0 0 252 7 0 0 3 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 784 0 0 0 5 0 0 0 784 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.ContainerView) 34 15 0 784 34 2 8 1140850688 131073 880 0 721158 ##(Smalltalk.SystemColor) 31 0 5 0 0 0 880 482 170 176 528 false 170 192 34 2 410 ##(Smalltalk.Toolbar) 34 25 0 880 34 2 8 1409288972 131137 1040 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 517 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 674 193 193 0 1040 1106 1136 8 4294905199 170 192 528 170 192 34 6 39477 1115910 ##(Smalltalk.ToolbarIconButton) 39477 0 1040 1 1180998 4 ##(Smalltalk.CommandDescription) #deleteHistory 8 'Delete History' 1 1 263494 3 ##(Smalltalk.Icon) 0 true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'CompileFailedMethod.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 395334 3 ##(Smalltalk.Bitmap) 0 true 0 0 0 0 3 674 33 33 1 39327 1330 39327 0 1040 17 1362 #historyBack 8 'History Back' 1 1 1410 0 true 1456 8 'HistoryBack.ico' 1504 1538 0 true 0 0 0 0 3 674 33 33 1 39329 1330 39329 0 1040 17 1362 #historyForward 8 'History Forward' 1 1 1410 0 true 1456 8 'HistoryForward.ico' 1504 1538 0 true 0 0 0 0 3 674 33 33 1 34 3 1584 1696 1344 170 176 34 6 1776 3 1552 5 1664 1 0 1 0 674 33 33 674 45 45 0 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 562 138 144 34 2 626 #createAt:extent: 34 2 674 1 1 674 1501 51 1040 626 #updateSizePosted 528 1040 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 238 2 0 0 25 0 0 0] 8 #() 674 193 193 0 27 8 'historyToolbar' 0 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 1 674 1501 51 880 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 238 2 0 0 25 0 0 0] 34 1 1040 2096 0 27 0 0 0 410 ##(Smalltalk.ContainerView) 34 15 0 784 34 2 8 1140850688 131073 2288 0 0 0 5 0 0 0 2288 482 170 176 34 2 410 ##(Smalltalk.ReferenceView) 34 14 0 2288 34 2 8 1140850688 131073 2400 0 1106 8 4278190080 0 5 0 0 0 2400 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.MethodListPresenter) #resource_MultiClass_view 0 562 138 144 34 1 626 #createAt:extent: 34 2 674 507 1 674 995 451 2400 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 253 0 0 0 0 0 0 0 238 2 0 0 225 0 0 0] 528 2096 0 27 5 false 170 192 34 4 410 ##(Smalltalk.TreeView) 34 27 0 2288 34 2 8 1409352231 1025 2704 590918 3 ##(Smalltalk.TreeModel) 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 525062 ##(Smalltalk.TreeNode) 0 0 0 170 192 528 1106 8 4278190080 0 29 265030 4 ##(Smalltalk.Menu) 0 true 34 2 984134 2 ##(Smalltalk.CommandMenuItem) 1 1362 #addList 8 'Add List' 1 1 0 0 0 2962 1 1362 #removeQueryElement 8 'Remove Query' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 2704 0 8 4294906017 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 170 176 528 17 #noIcons 1 0 562 138 144 34 2 626 #createAt:extent: 34 2 674 1 1 674 497 451 2704 626 #contextMenu: 34 1 2928 2704 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 248 0 0 0 225 0 0 0] 8 #() 2096 0 27 8 'treeMethods' 2400 8 'methodList' 0 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 51 674 1501 451 2288 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 238 2 0 0 250 0 0 0] 34 3 2704 410 ##(Smalltalk.Splitter) 34 12 0 2288 34 2 8 1140850688 1 3584 0 1106 8 4278190080 0 517 0 0 0 3584 562 138 144 34 1 626 #createAt:extent: 34 2 674 497 1 674 11 451 3584 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 248 0 0 0 0 0 0 0 253 0 0 0 225 0 0 0] 8 #() 2096 0 27 2400 2096 0 27 170 192 528 0 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 1 674 1501 501 784 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 238 2 0 0 250 0 0 0] 34 2 880 2288 2096 0 27 410 ##(Smalltalk.Splitter) 34 12 0 416 34 2 8 1140850688 1 4016 0 1106 8 4278190080 0 517 0 0 0 4016 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 501 674 1501 11 4016 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 250 0 0 0 238 2 0 0 255 0 0 0] 8 #() 2096 0 27 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 4272 0 0 0 5 0 0 0 4272 482 170 176 528 false 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 0 4272 34 2 8 1140850688 131073 4400 0 1106 2480 0 5 0 0 0 4400 2498 ##(Smalltalk.MethodSourcePresenter) #resource_Default_view 0 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 1 674 1501 501 4400 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 238 2 0 0 250 0 0 0] 528 2096 0 27 8 'methodSource' 0 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 511 674 1501 501 4272 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 255 0 0 0 238 2 0 0 249 1 0 0] 34 1 4400 2096 0 27 2096 0 27 )!

resource_List_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_List_view)
	"

	^#(#'!!!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 4 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 560 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 560 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.MethodListPresenter)  8 #resource_MultiClass_view 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 882 1201 351 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 175 0 0 0] 98 0 882 193 193 0 27 9 410 576 98 14 0 416 98 2 8 1140850688 131073 1008 0 642 672 0 5 0 0 0 1008 690 8 ##(Smalltalk.MethodSourcePresenter)  8 #resource_Default_view 0 754 202 208 98 1 818 848 98 2 882 1 361 882 1201 441 1008 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 180 0 0 0 88 2 0 0 144 1 0 0] 976 992 0 27 11 16 234 256 98 4 560 8 'methodList' 1008 8 'methodSource' 0 754 202 208 98 1 818 848 98 2 882 2799 21 882 1201 801 416 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 5 0 0 10 0 0 0 207 7 0 0 154 1 0 0] 98 3 560 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 1504 0 642 8 4278190080 0 517 0 0 0 1504 754 202 208 98 1 818 848 98 2 882 1 351 882 1201 11 1504 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 175 0 0 0 88 2 0 0 180 0 0 0] 98 0 992 0 27 1008 992 0 27 )! !
!JadeTreeMethodListPresenter class categoriesFor: #resource_Default_view!public! !
!JadeTreeMethodListPresenter class categoriesFor: #resource_List_view!public! !

