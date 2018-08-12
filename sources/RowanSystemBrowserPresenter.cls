"Filed out from Dolphin Smalltalk 7"!

JadeAutoSystemBrowserPresenter subclass: #RowanSystemBrowserPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanSystemBrowserPresenter guid: (GUID fromString: '{ad879b98-36f4-461e-8b30-f07a614dd9d2}')!
RowanSystemBrowserPresenter comment: ''!
!RowanSystemBrowserPresenter categoriesForClass!Unclassified! !
!RowanSystemBrowserPresenter methodsFor!

addPackage
	| service |
	service := RowanProjectService on: gciSession.
	(service remoteDefaultProjectName isNil or: [service defaultProjectName isNil])
		ifTrue: 
			[^MessageBox notify: 'No default project has been created. Please specify in the project list.'].
	^super addPackage!

addSubclass
	classDefinitionPresenter
		ensureVisible;
		value: self classService subclassCreationTemplate;
		yourself!

browseClassReferencesOf: className
	
	| string |
	string := 'browseClassReferences' , Character tab asString , self behaviorIdentifier , Character lf asString.
	(string := self updateCommand: string) = 'browseClassReferences' ifFalse: [self error: 'Unrecognized response'].
	self browseMethodsAndSelect: className.!

categoriesMenuStrings

	false ifTrue: [
		self addMethodCategory; removeMethodCategories.
	].
	^#(
		'&Categories'
		'&Add Method Category//addMethodCategory'
		'&Remove Method Categories//removeMethodCategories'
	).
!

classesMenuStrings

	false ifTrue: [
		self browseClassReferences; fileOutClass; addSubclass; removeClass;  runClassTests.
	].
	^#(
		'&Classes'
		'&Browse References//browseClassReferences'
		'&File Out Class//fileOutClass'
		'Add &Subclass//addSubclass'
		'&Remove//removeClass'
		'-'
		'Set Compiler &Environment ...//setEnvironment'
		'Run &Tests//runClassTests'
	).
!

classService
	^(RowanClassService on: gciSession)
		name: self selectedClassNameWithoutVersion;
		yourself!

createSchematicWiringForClassHierarchyTabs
	classHierarchyTabs
		when: #currentCardChanging:
			send: #aboutToChange:
			to: self;
		when: #currentCardChanged
			send: #selectedClassChanged:
			to: self
			with: true;
		when: #currentCardChanged
			send: #updateAndSelect:
			to: self
			with: classDefinitionPresenter!

getViews

	super getViews.
	self registerTabPresentersForUpdates!

labelOfClass: oldName changedTo: newName

	| stream |
	oldName = self selectedClassNameWithoutVersion.
	self browseClassReferencesOf: oldName.  "Jadeite gets in trouble deep in server code if the class isn't a class"
	stream := (WriteStream on: String new)
		nextPutAll: 'changeClassName'; tab;
		nextPutAll: self selectedClassNameWithoutVersion; tab;
		nextPutAll: selectedClassOop printString; tab;
		nextPutAll: newName; tab;
		lf; nextPutAll: self requestString;
		yourself.
	self selectedClassName: newName.
	self updateCommand: stream contents.

!

onViewClosed
	"copied to RowanDebugger"

	super onViewClosed.
	subPresenters do:[:presenter |
		BrowserUpdate current removeEventsTriggeredFor: presenter]!

packageTabName
	^'Rowan Packages'!

projectDictionaryChanged
!

registerPresentersForUpdates
	BrowserUpdate current register: dictionaryListPresenter selector: #dictionaryUpdate:.
	BrowserUpdate current register: globalsPresenter selector: #globalsUpdate:.
	BrowserUpdate current register: classCategoryPresenter selector: #classCategoryUpdate:.
	BrowserUpdate current register: classListPresenter selector: #classesUpdate:.
	BrowserUpdate current register: classDefinitionPresenter selector: #classDefinitionUpdate:.
	BrowserUpdate current register: classCommentPresenter selector: #classCommentUpdate:.
	BrowserUpdate current register: classHierarchyPresenter selector: #classHierarchyUpdate:.
	BrowserUpdate current register: superclassListPresenter selector: #superclassListUpdate:.
	BrowserUpdate current register: methodListPresenter selector: #methodsUpdate:.
	BrowserUpdate current register: methodSourcePresenter selector: #updateSelectorFor:!

registerTabPresentersForUpdates
	BrowserUpdate current register: self methodFilterListPresenter selector: #methodFilterUpdate:!

resetClassDefinitionPresenter
	classDefinitionPresenter view
		backcolor: Color white;
		isModified: false;
		yourself!

updateAfterFindClass: anArray isMeta: aBoolean selector: aString

	super updateAfterFindClass: anArray isMeta: aBoolean selector: aString.!

updateClassHierarchyFrom: update

	| list paths treeModel x y currentSelection newSelection flags |
	list := update classes. 
	newSelection := update selections reverse.
	flags := #().
	classHierarchyPresenter ensureVisible.
	list := list collect: [:each | each reverse].
	paths := Set new.
	list do: [:eachClass | 
		| string |
		string := ''.
		eachClass do: [:each | 
			string := string , each.
			paths add: string.
			string := string , Character tab asString.
		].
	].
	paths := paths asSortedCollection asArray.
	treeModel := TreeModel new
		searchPolicy: SearchPolicy equality;
		reset;
		yourself.
	paths do: [:each | 
		| path parent |
		path := each subStrings: Character tab.
		parent := path copyFrom: 1 to: path size - 1.
		parent isEmpty ifTrue: [parent := nil].
		treeModel add: path asChildOf: parent.
	].
	x := (treeModel asBag collect: [:each | each printString]) asSortedCollection asArray.
	y := (classHierarchyPresenter model asBag collect: [:each | each printString]) asSortedCollection asArray.
	x = y ifFalse: [
		classHierarchyPresenter
			model: treeModel;
			yourself.
	].
	currentSelection := classHierarchyPresenter selectionIfNone: [#()].
	(currentSelection isEmpty and: [newSelection isEmpty and: [classListPresenter selections size = 1]]) ifTrue: [
		x := classListPresenter selection.
		newSelection := classHierarchyPresenter model asBag asArray
			detect: [:each | each last = x]
			ifNone: [nil].
	].
	(newSelection notEmpty and: [currentSelection ~= newSelection]) ifTrue: [
		classHierarchyPresenter 
			selection: newSelection;
			expand: newSelection;
			yourself.
	].
	self selectedClassName: (newSelection notEmpty ifTrue: [newSelection last] ifFalse: ['']).
	classListPresenter selectionOrNil: nil.

	selectedClassesAreTestCases := (flags at: 1) = 'true'.
	sunitPresenter testClassName: (selectedClassesAreTestCases ifTrue: [selectedClassName] ifFalse: ['']).!

updateClassInfo
	| index newClassDefinition |
	selectedClassOop := readStream nextLine asNumber.
	newClassDefinition := readStream nextParagraph.
	classCommentPresenter value: readStream nextParagraph.
	classDefinitionPresenter view isModified
		ifTrue: 
			[newClassDefinition = classDefinition ifTrue: [^self].
			(MessageBox confirm: 'Copy changes to clipboard?' caption: 'Class has unsaved changes!!')
				ifTrue: [classDefinitionPresenter value copyToClipboard]].
	classDefinition := newClassDefinition.
	0 < (classDefinition indexOfSubCollection: 'MyNewClass')
		ifTrue: 
			["Template is based on Stream class, but we don't want to use 'Globals' in template"
			index := classDefinition indexOfSubCollection: ': Globals'.
			(0 < index and: [(classDefinition copyFrom: index - 2 to: index + 1) ~= 'User'])
				ifTrue: 
					[classDefinition := (classDefinition copyFrom: 1 to: index + 1)
								, (dictionaryListPresenter selectionIfNone: ['UserGlobals' -> false]) key
									, (classDefinition copyFrom: index + 9 to: classDefinition size).
					index := classDefinition indexOfSubCollection: 'User Classes'.
					0 < index
						ifTrue: 
							[classDefinition := (classDefinition copyFrom: 1 to: index - 1) , 'User Classes'
										, (classDefinition copyFrom: index + 12 to: classDefinition size)]]].
	classDefinitionPresenter value: classDefinition.
	classDefinitionPresenter view
		backcolor: Color white;
		isModified: false;
		yourself!

updateCommand: aString onSuccessDo: aBlock
	super updateCommand: aString onSuccessDo: aBlock.
	ignoreNextSetFocusEvent := true.	"Need to stop runaway set focus events somehow. There's a better fix somewhere"
	^nil!

updateFromCommand: update
	| index newClassDefinition |
	selectedClassOop := update oop.
	newClassDefinition := update template.
	classCommentPresenter value: update comment.
	classDefinitionPresenter view isModified
		ifTrue: 
			[newClassDefinition = classDefinition ifTrue: [^self].
			(MessageBox confirm: 'Copy changes to clipboard?' caption: 'Class has unsaved changes!!')
				ifTrue: [classDefinitionPresenter value copyToClipboard]].
	classDefinition := newClassDefinition.
	0 < (classDefinition indexOfSubCollection: 'MyNewClass')
		ifTrue: 
			["Template is based on Stream class, but we don't want to use 'Globals' in template"
			index := classDefinition indexOfSubCollection: ': Globals'.
			(0 < index and: [(classDefinition copyFrom: index - 2 to: index + 1) ~= 'User'])
				ifTrue: 
					[classDefinition := (classDefinition copyFrom: 1 to: index + 1)
								, (dictionaryListPresenter selectionIfNone: ['UserGlobals' -> false]) key
									, (classDefinition copyFrom: index + 9 to: classDefinition size).
					index := classDefinition indexOfSubCollection: 'User Classes'.
					0 < index
						ifTrue: 
							[classDefinition := (classDefinition copyFrom: 1 to: index - 1) , 'User Classes'
										, (classDefinition copyFrom: index + 12 to: classDefinition size)]]].
	classDefinitionPresenter value: classDefinition.
	classDefinitionPresenter view
		backcolor: Color white;
		isModified: false;
		yourself!

updateMethodFilter

	| listPresenter pieces type tabs tab filters selections |
	listPresenter := self methodFilterListPresenter.
	pieces := readStream nextLine subStrings: Character tab.
	pieces isEmpty ifTrue:[^self].
	type := pieces at: 1.
	1 < pieces size ifTrue: [environment := (pieces at: 2) asNumber].
	(tabs := methodFilterTabs cards) isEmpty ifTrue: [^self].
	tab := tabs detect: [:each | each name = type].
	tab ensureVisible.
	filters := readStream nextLineAsList reverse.
	filters = listPresenter list ifFalse: [
		listPresenter list: filters.
	].
	selections := readStream nextLineAsList.
	selections := listPresenter list select: [:each | selections includes: each].
	selections = listPresenter selections ifFalse: [
		listPresenter selections: selections.
	].
	selections notEmpty ifTrue: [
		listPresenter view ensureSelectionVisible.
	] ifFalse: [
		filters notEmpty ifTrue: [
			listPresenter view ensureVisible: 1.
		].
	].
!

updateMethodFilterFrom: update

	| listPresenter type tabs tab filters selections |
	listPresenter := self methodFilterListPresenter.
	type := update type. 
	(tabs := methodFilterTabs cards) isEmpty ifTrue: [^self].
	tab := tabs detect: [:each | each name = type].
	tab ensureVisible.
	filters :=update methodFilters reverse.
	filters = listPresenter list ifFalse: [
		listPresenter list: filters.
	].
	selections := update selections. 
	selections := listPresenter list select: [:each | selections includes: each].
	selections = listPresenter selections ifFalse: [
		listPresenter selections: selections.
	].
	selections notEmpty ifTrue: [
		listPresenter view ensureSelectionVisible.
	] ifFalse: [
		filters notEmpty ifTrue: [
			listPresenter view ensureVisible: 1.
		].
	].
!

updateMethodFrom: update

	| gsMethod isReadOnly listModel methodSourceLabel newSource warnings x |
	methodSourcePresenter view cancelCallTip.
	gsMethod := update gsMethod session: gciSession.
	isReadOnly := gsMethod isReadOnly.	"current user has write permission for the class"
	newSource := gsMethod source.
	methodSourcePresenter view isModified ifTrue: [
		newSource = methodSource ifTrue: [^self].
		(MessageBox confirm: 'Copy changes to clipboard?' caption: 'Method has unsaved changes!!') ifTrue: [
			methodSourcePresenter value copyToClipboard.
		].
	].
	methodSource := newSource.
	methodSourcePresenter value: methodSource.
	unimplementedSelectors := gsMethod unimplementedSelectors.
	stepPoints := gsMethod stepPoints.
	breakPoints := gsMethod breakPoints.
	self updateMethodStepPoints.
	methodSourcePresenter view 
		backcolor: JadeTextStyles default colorForNoEdits;
		isModified: false;
		isReadOnly: isReadOnly;
		yourself.
	(overrideListPresenter list notEmpty and: [overrideListPresenter selectionOrNil ~= overrideListPresenter list last]) ifTrue: [
		methodSourcePresenter view
			backcolor: nil;
			isReadOnly: true;
			yourself.
	].
	((methodCategory := gsMethod category) notEmpty and: [self isCategoriesTabSelected]) ifTrue: [
		| fullList selections index newName |
		fullList := categoryListPresenter list.
		selections := categoryListPresenter selections.
		0 < (index := fullList indexOf: methodCategory) ifTrue: [
			fullList at: index put: (newName := ' ' , methodCategory).
			categoryListPresenter list: fullList.
			0 < (index := selections indexOf: methodCategory) ifTrue: [
				selections at: index put: newName.
				categoryListPresenter selections: selections.
			].
			categoryListPresenter view invalidate.
		].
	].
	listModel := (textAreaTabs subViews detect: [:each | each class == TabViewXP]) model.
	originalSourceLabel ifNil: [originalSourceLabel := listModel detect: [:each | each text  = 'Original Source']].
	methodSourceLabel := listModel detect: [:each | each text = 'Method Source'].
	(x := gsMethod originalSource) isEmpty ifTrue: [
		listModel remove: originalSourceLabel ifAbsent: [].
	] ifFalse: [
		(listModel includes: originalSourceLabel) ifFalse: [listModel add: originalSourceLabel before: methodSourceLabel].
		originalSourcePresenter value: x.
	].
	(warnings := gsMethod warnings) notEmpty ifTrue: [
		MessageBox warning: warnings caption: 'Jade Compile Warning'.
	].
	!

updateMethodsFrom: update
	| fullList selections |
	fullList := update methods collect: 
					[:methodUpdate |
					Array
						with: methodUpdate selector
						with: methodUpdate isOverride
						with: methodUpdate isTestClass
						with: 'F'	"monticello related, ignore"].
	fullList = methodListPresenter list ifFalse: [methodListPresenter list: fullList].
	selections := update selections.
	selections := methodListPresenter list select: [:eachArray | selections includes: eachArray first].
	selections = methodListPresenter selections ifFalse: [methodListPresenter selections: selections].
	selections notEmpty
		ifTrue: [methodListPresenter view ensureSelectionVisible]
		ifFalse: [fullList notEmpty ifTrue: [methodListPresenter view ensureVisible: 1]]!

updateOverrideListFrom: update

	| list selection |
	list := update inheritedClasses.		
	list = overrideListPresenter list ifFalse: [
		overrideListPresenter list: list.
	].
	selection := update inheritedClass.
	selection isEmpty ifTrue: [selection := nil].
	selection notNil ifTrue: [
		selection := overrideListPresenter list
			detect: [:each | each = selection]
			ifNone: [nil].
	].
	selection = overrideListPresenter selectionOrNil ifFalse: [
		overrideListPresenter selectionOrNil: selection.
	].
!

updatePresenters
	
	[inUpdate := true.
	self
		updatePackageDictionaryList;
		updateClassCategoryTree;
		updateClassListOrHierarchy;
		updateClassInfo;
		updateSuperclassList;
		updateMethodFilter;
		updateMethodList;
		updateOverrideList;
		updateSelector;
		updateTabLabel;
		yourself]
			ensure: [inUpdate := false].
	self resetClassDefinitionPresenter!

updateSelectionsForUpdate: update

	update updateDictionarySelectionsFor: dictionaryListPresenter.
	update updateClassCategorySelectionsFor: classCategoryPresenter.
	update updateClassSelectionFor: classListPresenter.
	update updateClassHierarchySelectionFor: classHierarchyPresenter.
	update updateSuperclassSelectionFor: superclassListPresenter.
	update updateMethodFilterSelectionFor: self methodFilterListPresenter.
	update updateMethodSelectionFor: self methodListPresenter.
	!

updateTabLabel
	| cardNumber newLabel |
	cardNumber := self parentPresenter view cards indexOf: self view.
	(newLabel := self selectedClassNameWithoutVersion) notEmpty
		ifTrue: 
			[instanceClassTabs currentCard name = 'classTab' ifTrue: [newLabel := newLabel , ' class'].
			0 < environment ifTrue: [newLabel := newLabel , ' [' , environment printString , ']']]
		ifFalse: 
			[| selections |
			newLabel := 'Dictionaries'.
			selections := dictionaryListPresenter selections.
			selections size = 1 ifTrue: [newLabel := selections first key]].
	self view arrangement: cardNumber printString , ' ' , newLabel.
	self parentPresenter view updateTabs! !
!RowanSystemBrowserPresenter categoriesFor: #addPackage!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #addSubclass!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #browseClassReferencesOf:!public! !
!RowanSystemBrowserPresenter categoriesFor: #categoriesMenuStrings!menus!public! !
!RowanSystemBrowserPresenter categoriesFor: #classesMenuStrings!menus!public! !
!RowanSystemBrowserPresenter categoriesFor: #classService!private!rowan! !
!RowanSystemBrowserPresenter categoriesFor: #createSchematicWiringForClassHierarchyTabs!create schemantic wiring!public! !
!RowanSystemBrowserPresenter categoriesFor: #getViews!public! !
!RowanSystemBrowserPresenter categoriesFor: #labelOfClass:changedTo:!event handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #onViewClosed!event handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #packageTabName!menus!overrides!private! !
!RowanSystemBrowserPresenter categoriesFor: #projectDictionaryChanged!event handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #registerPresentersForUpdates!event handlers!initialization!public!rowan! !
!RowanSystemBrowserPresenter categoriesFor: #registerTabPresentersForUpdates!event handlers!initialization!public!rowan! !
!RowanSystemBrowserPresenter categoriesFor: #resetClassDefinitionPresenter!private!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateAfterFindClass:isMeta:selector:!public! !
!RowanSystemBrowserPresenter categoriesFor: #updateClassHierarchyFrom:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateClassInfo!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateCommand:onSuccessDo:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateFromCommand:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateMethodFilter!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateMethodFilterFrom:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateMethodFrom:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateMethodsFrom:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateOverrideListFrom:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updatePresenters!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateSelectionsForUpdate:!public!rowan! !
!RowanSystemBrowserPresenter categoriesFor: #updateTabLabel!public!updating! !

!RowanSystemBrowserPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 656 nil nil nil 5 nil nil nil 656 482 170 176 8 #() false 170 192 752 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1075 1 898 711 539 656 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 2 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 656 34 2 8 1409286144 131073 1008 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 517 nil nil nil 1008 655878 ##(Smalltalk.CardLayout) 138 144 34 3 721414 ##(Smalltalk.Association) 8 'Categories' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 1008 34 2 8 1140850688 131073 1264 nil 1074 1104 nil 5 nil nil nil 1264 1136 170 192 34 6 410 ##(Smalltalk.ListView) 34 30 nil 1264 34 2 8 1409372233 1025 1376 590662 2 ##(Smalltalk.ListModel) 138 144 752 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 1074 8 4278190080 nil 21 nil nil nil 1376 nil 8 4294910881 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 283 #left 1570 #displayString 1600 ##(Smalltalk.SortedCollection) nil nil 1376 nil 3 nil 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 9 1 1744 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 1760 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 291 487 1376 850 #text: 34 1 8 'Column 1' 1376 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 898 193 193 nil 27 8 'pragmaList' 1232 8 'categoryList' 410 ##(Smalltalk.ListView) 34 30 nil 1264 34 2 8 1409372233 1025 2144 1442 138 144 752 nil 1504 1074 1536 nil 21 nil nil nil 2144 nil 8 4294910881 1570 #displayString 1600 nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 283 #left 1570 #displayString 1600 ##(Smalltalk.SortedCollection) nil nil 2144 nil 3 nil 1746 0 nil 1778 9 1 1744 1808 1824 1840 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 2368 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 291 487 2144 850 #text: 34 1 8 'Column 1' 2144 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2096 nil 27 8 'variableList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 1008 34 2 8 1140916864 1 2656 1442 138 144 34 3 1216 8 'Variables' 8 'Pragmas' nil 1504 1074 1104 nil 1 nil nil nil 2656 nil 8 4294907187 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil #noIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 351 539 2656 850 #basicSelectionsByIndex: 34 1 8 #(1) 2656 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2656 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 8 #() 2096 nil 27 nil 1008 898 33 33 1049862 ##(Smalltalk.ButtonInteractor) 1264 nil 590342 ##(Smalltalk.Rectangle) 898 255 3 898 287 35 1 1180998 4 ##(Smalltalk.CommandDescription) #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 53 9 898 291 523 1264 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 171 0 0 0 9 1 0 0] 34 3 1232 2144 1376 2096 nil 27 34 2 8 1409372233 1025 1232 1442 138 144 752 nil 1504 1074 1536 nil 21 nil nil nil 1232 nil 8 4294910881 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 283 #left 1570 #displayString 3504 ##(Smalltalk.SortedCollection) nil nil 1232 nil 3 nil 1746 0 nil 1778 1 83886081 1744 8 'doIt' 8 '[:each | each ]' 1779525917 3600 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 291 487 1232 850 #text: 34 1 8 'Column 1' 1232 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2096 nil 27 1186 2768 2144 1186 2784 1376 1232 170 192 34 2 1264 8 'methodFilterTabs' nil 2656 1264 898 201 201 401 1 31 nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 351 539 1008 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 34 2 1264 2656 2096 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 4160 nil 1074 8 4278190080 nil 517 nil nil nil 4160 786 138 144 34 1 850 #createAt:extent: 34 2 898 351 1 898 11 539 4160 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 0 0 0 0 180 0 0 0 13 1 0 0] 8 #() 2096 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 4416 nil nil nil 5 nil nil nil 4416 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ListView) 34 30 nil 4416 34 2 8 1409372233 1025 4544 1442 138 144 752 nil 1504 1074 1536 nil 13 nil nil nil 4544 nil 8 4294910881 1570 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil nil 138 144 34 2 1682 8 'Column 1' 311 #left 1570 #displayString 4688 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 1 83886081 1744 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 4784 7 257 nil nil 4544 nil 3 nil 1746 0 nil 1778 5 1 1744 8 'doIt' 8 '[:each | (each item at: 4) ifTrue: [each font: each font beItalic]]' 8 #[33 105 226 0 214 4 148 123 17 226 1 160 179 106 60 106] #item #font #beItalic #font: 4864 7 257 nil 1682 8 '' 33 #center 850 #empty 8 #() 16 1570 #<= 8 #() nil nil 4544 1746 0 nil 1778 6 1 1744 8 'doIt' 8 '[:each | ((each at: 2) ifTrue: [RowanSystemBrowserPresenter overriddenIcon] ifFalse: [Icon blank]) 
	imageIndex]' 8 #[34 105 17 64 148 120 29 159 111 31 161 162 106] ##(Smalltalk.RowanSystemBrowserPresenter) #overriddenIcon ##(Smalltalk.Icon) #blank #imageIndex 5040 7 257 nil 1 nil nil #report 752 nil 131173 nil 8 #(nil) 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 47 898 351 447 4544 850 #text: 34 1 8 'Column 1' 4544 850 #columnOrder: 34 1 8 #(2 1) 4544 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 175 0 0 0 246 0 0 0] 8 #() 2096 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 5426 #fixedParentRight 1 5426 #fixedParentTop 47 5426 #fixedParentBottom -45 410 ##(Smalltalk.ComboBox) 34 17 nil 4416 34 2 8 1412498947 1025 5504 1442 138 144 752 nil 1504 1074 8 4278190080 nil 5 nil nil nil 5504 nil 8 4294909895 1570 #displayString 8 #() 752 401 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 351 53 5504 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 26 0 0 0] 8 #() 2096 nil 27 5394 5440 1 5456 1 5472 1 5426 #fixedViewTop 47 410 ##(Smalltalk.ComboBox) 34 17 nil 4416 34 2 8 1412498947 1025 5872 1442 138 144 752 nil 1504 1074 5616 nil 5 nil nil nil 5872 nil 8 4294909895 1570 #displayString 8 #() 752 401 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 493 898 351 53 5872 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 246 0 0 0 175 0 0 0 16 1 0 0] 8 #() 2096 nil 27 5394 5440 1 5456 1 5488 -45 5856 47 170 192 34 6 4544 8 'methodList' 5504 8 'superclassList' 5872 8 'overrideList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 361 1 898 351 539 4416 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 99 1 0 0 13 1 0 0] 34 3 5504 4544 5872 2096 nil 27 2096 nil 27 5 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 6448 nil nil nil 5 nil nil nil 6448 482 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 6448 34 2 8 1140850688 131073 6560 nil nil nil 5 nil nil nil 6560 4482 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 6560 34 2 8 1409286144 131073 6672 nil 1074 8 4278190080 nil 5 nil nil nil 6672 1122 138 144 34 2 1186 590662 1 ##(Smalltalk.CardLabel) 8 'Instance' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 6864 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 6880 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 6672 34 2 8 1140850688 131073 6976 nil nil nil 5 nil nil nil 6976 nil 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 9 898 435 1 6976 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 221 0 0 0 4 0 0 0] 8 #() 2096 nil 27 1186 6834 8 'Class' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 7248 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 7264 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 6672 34 2 8 1140850688 131073 7360 nil nil nil 5 nil nil nil 7360 nil 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 9 898 435 1 7360 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 221 0 0 0 4 0 0 0] 8 #() 2096 nil 27 6976 170 192 34 4 6976 8 'instanceTab' 7360 8 'classTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 6672 34 2 8 1140916738 1 7664 1442 138 144 34 2 6848 7232 nil 1504 nil nil 1 nil nil nil 7664 nil 8 4294907187 1746 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 7792 7 257 nil 1746 0 nil 7810 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 7856 7 257 nil 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 451 51 7664 850 #basicSelectionsByIndex: 34 1 8 #(1) 7664 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 7664 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 25 0 0 0] 8 #() 2096 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 489 898 451 51 6672 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 225 0 0 0 13 1 0 0] 34 3 6976 7360 7664 2096 nil 27 5394 5440 1 5456 1 5488 -49 5856 51 410 ##(Smalltalk.CardContainer) 34 16 nil 6560 34 2 8 1409286144 131073 8320 nil 1074 8 4278190080 nil 5 nil nil nil 8320 1122 138 144 34 2 1186 6834 8 'Classes' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 8496 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 8512 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 8320 34 2 8 1409372233 1025 8608 1442 138 144 752 nil 1504 1074 8 4278190080 nil 29 nil nil nil 8608 nil 8 4294910881 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 2 1682 8 'Column 1' 359 #left 1746 0 nil 1778 5 1 1744 8 'doIt' 8 '[:each | (each subStrings: Character space) first]' 8 #[33 105 17 29 159 178 161 106] ##(Smalltalk.Character) #space #subStrings: #first 8848 7 257 nil ##(Smalltalk.SortedCollection) nil nil 8608 nil 3 nil nil 1682 8 '' 69 #left 1746 0 nil 1778 6 1 1744 8 'doIt' 8 '[:each | | index |
	index := each indexOf: Character space.
	0 < index ifTrue: [each copyFrom: index + 2 to: each size - 1] ifFalse: ['''']]' 8 #[34 105 17 29 159 178 90 62 18 128 221 9 233 1 64 126 17 145 99 193 106 33 106] ##(Smalltalk.Character) #space #indexOf: #copyFrom:to: 8944 8960 7 65793 nil 1570 #<= 8 #() nil nil 8608 nil 1 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 9 55 898 435 433 8608 850 #text: 34 1 8 'Column 1' 8608 850 #columnOrder: 34 1 8 #(2 1) 8608 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 221 0 0 0 243 0 0 0] 8 #() 2096 nil 27 1186 6834 8 'Hierarchy' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 9392 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 9408 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 8320 34 2 8 1409352231 1025 9504 590918 3 ##(Smalltalk.TreeModel) nil 1504 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 752 1074 8 4278190080 nil 29 nil nil nil 9504 nil 8 4294906685 1746 0 nil 1778 2 1 1568 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 9696 7 257 nil ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil 170 176 752 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 752 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 55 898 435 433 9504 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 221 0 0 0 243 0 0 0] 8 #() 2096 nil 27 8608 170 192 34 4 8608 8 'classList' 9504 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 8320 34 2 8 1140916736 1 10032 1442 138 144 34 2 8480 9376 nil 1504 nil nil 1 nil nil nil 10032 nil 8 4294907187 1746 0 nil 7810 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 10160 7 257 nil 1746 0 nil 7810 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 10208 7 257 nil 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 451 495 10032 850 #basicSelectionsByIndex: 34 1 8 #(1) 10032 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 10032 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 247 0 0 0] 8 #() 2096 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 451 495 8320 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 247 0 0 0] 34 3 8608 9504 10032 2096 nil 27 5394 5440 1 5456 1 5472 1 5488 -43 170 192 34 4 6672 8 'instanceClassTabs' 8320 8 'classHierarchyTabs' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 615 1 898 451 539 6560 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 51 1 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 2 8320 6672 2096 nil 27 13 410 ##(Smalltalk.ContainerView) 34 15 nil 6448 34 2 8 1140850688 131073 10896 nil nil nil 5 nil nil nil 10896 482 170 176 752 false 170 192 34 2 410 ##(Smalltalk.TreeView) 34 27 nil 10896 34 2 8 1409352231 1025 11024 9570 nil 1504 9602 nil nil nil 170 192 752 1074 8 4278190080 nil 21 nil nil nil 11024 nil 8 4294906685 1746 0 nil 1778 5 1 1744 8 'doIt' 8 '[:each | each key isEmpty ifTrue: [''--All Categories--''] ifFalse: [each key last]].' 8 #[33 105 226 0 159 119 31 106 226 0 161 106] #key #isEmpty 8 '--All Categories--' #last 11184 7 257 nil ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil 170 176 752 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 752 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 223 539 11024 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 111 0 0 0 13 1 0 0] 8 #() 2096 nil 27 8 'classCategoryList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 383 1 898 223 539 10896 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 191 0 0 0 0 0 0 0 46 1 0 0 13 1 0 0] 34 1 11024 2096 nil 27 7 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 6448 34 2 8 1409286144 131073 11648 nil 1074 8 4278190080 nil 517 nil nil nil 11648 1122 138 144 34 1 1186 6834 8 'Dictionaries' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Dictionaries'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 11824 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 11840 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 11648 34 2 8 1140850688 131073 11968 nil 1074 11728 nil 5 nil nil nil 11968 11744 170 192 34 2 11936 8 'dictionaryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 11648 34 2 8 1140916864 1 12096 1442 138 144 34 1 11808 nil 1504 1074 11728 nil 1 nil nil nil 12096 nil 8 4294907187 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 373 539 12096 850 #basicSelectionsByIndex: 34 1 8 #(1) 12096 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 12096 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 186 0 0 0 13 1 0 0] 8 #() 2096 nil 27 nil 11648 898 33 33 3090 11968 nil 3122 898 277 3 898 309 35 1 3186 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 53 9 898 313 523 11968 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 182 0 0 0 9 1 0 0] 34 1 11936 2096 nil 27 34 2 8 1409372233 1025 11936 1442 138 144 752 nil 1504 1074 8720 nil 21 nil nil nil 11936 nil 8 4294910881 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 305 #left 1570 #displayString 12864 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 2 1 1744 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 12960 7 257 nil nil 11936 nil 3 nil 1746 0 nil 1778 5 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] #item #font #beBold #font: 13040 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 313 487 11936 850 #text: 34 1 8 'Column 1' 11936 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 156 0 0 0 5 1 0 0] 8 #() 2096 nil 27 11936 170 192 34 2 11968 8 'packageDictionaryTabs' nil 12096 11968 898 201 201 401 1 31 nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 373 539 11648 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 186 0 0 0 13 1 0 0] 34 2 11968 12096 2096 nil 27 11 false 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 1065 539 6448 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 5 11648 410 ##(Smalltalk.Splitter) 34 12 nil 6448 34 2 8 1140850688 1 13760 nil 1074 4240 nil 517 nil nil nil 13760 786 138 144 34 1 850 #createAt:extent: 34 2 898 373 1 898 11 539 13760 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 186 0 0 0 0 0 0 0 191 0 0 0 13 1 0 0] 8 #() 2096 nil 27 10896 410 ##(Smalltalk.Splitter) 34 12 nil 6448 34 2 8 1140850688 1 14000 nil 1074 4240 nil 517 nil nil nil 14000 786 138 144 34 1 850 #createAt:extent: 34 2 898 605 1 898 11 539 14000 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 46 1 0 0 0 0 0 0 51 1 0 0 13 1 0 0] 8 #() 2096 nil 27 6560 2096 nil 27 7 false 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 1785 539 544 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 6448 410 ##(Smalltalk.Splitter) 34 12 nil 544 34 2 8 1140850688 1 14416 nil 1074 4240 nil 517 nil nil nil 14416 786 138 144 34 1 850 #createAt:extent: 34 2 898 1065 1 898 11 539 14416 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 2 0 0 0 0 0 0 25 2 0 0 13 1 0 0] 8 #() 2096 nil 27 656 2096 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 14688 nil 1074 8400 nil 5 nil nil nil 14688 1122 138 144 34 7 1186 6834 8 'Package' 1746 0 nil 1778 7 1 6832 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 14848 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 14864 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 14688 34 2 8 1140850688 131073 14960 nil nil nil 5 nil nil nil 14960 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 14960 34 2 8 1409355853 1025 15072 1442 138 144 752 nil 1504 1074 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 5 984134 2 ##(Smalltalk.CommandMenuItem) 1 3186 #addRepository 8 'Add &Repository' 1 1 nil nil nil 15250 1 3186 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 15250 1 3186 #savePackage 8 '&Save Package' 1 1 nil nil nil 15250 1 3186 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 15072 nil 8 4294910881 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 4 1682 8 'Repository Type' 211 #left 1570 #displayString 15536 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 2 1 1744 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 15632 7 257 nil nil 15072 nil 1 nil nil 1682 8 'Description' 1161 #left 1570 #displayString 8 #() 1570 #<= 15760 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 15792 7 257 nil nil 15072 nil 3 nil nil 1682 8 'User' 201 #left 1570 #displayString 8 #() 1570 #<= 15920 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 15952 7 257 nil nil 15072 nil 1 nil nil 1682 8 'Password' 201 #left 1570 #displayString 15920 1570 #<= 15920 1746 0 nil 1778 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 16096 1746 0 nil 16112 19 257 nil 7 257 nil nil 15072 nil 1 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 263 898 1769 217 15072 850 #contextMenu: 34 1 15216 15072 850 #text: 34 1 8 'Repository Type' 15072 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 2096 nil 27 19 410 ##(Smalltalk.ListView) 34 30 nil 14960 34 2 8 1409355853 1025 16464 1442 138 144 752 nil 1504 1074 15184 nil 5 15202 nil true 34 1 15250 1 3186 #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 16464 nil 8 4294910881 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 4 1682 8 'Ancestor' 131 #left 1570 #displayString 15920 1570 #<= 15920 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 16816 7 257 nil nil 16464 nil 1 nil nil 1682 8 'Name' 401 #left 1570 #displayString 16704 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 1 83886081 1744 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 16944 7 257 nil nil 16464 nil 1 nil nil 1682 8 'Timestamp' 301 #left 1570 #displayString 15760 1570 #<= 15760 1746 0 nil 1778 1 83886081 1744 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 17088 7 257 nil nil 16464 nil 1 nil nil 1682 8 'Message' 941 #left 1570 #displayString 15760 1570 #<= 15760 1746 0 nil 1778 1 83886081 1744 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 17232 7 257 nil nil 16464 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 1769 263 16464 850 #contextMenu: 34 1 16576 16464 850 #text: 34 1 8 'Ancestor' 16464 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 2096 nil 27 23 true 170 192 34 4 15072 8 'repositoryList' 16464 8 'ancestorList' nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 9 55 898 1769 479 14960 850 #text: 34 1 8 'Package' 14960 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 34 2 16464 15072 2096 nil 27 1186 6834 8 'Globals' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 17888 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 17904 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 14688 34 2 8 1409355853 1025 18000 1442 138 144 752 nil 1504 1074 15184 nil 5 nil nil nil 18000 nil 8 4294910881 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 3 1682 8 'Name' 587 #left 1570 #displayString 18144 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 18240 7 257 nil nil 18000 nil 3 nil nil 1682 8 'Class' 589 #left 1570 #displayString 15760 1570 #<= 15760 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 18384 7 257 nil nil 18000 nil 3 nil nil 1682 8 'Value' 587 #left 1570 #displayString 15760 1570 #<= 15760 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 18528 7 257 nil nil 18000 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 9 55 898 1769 479 18000 850 #text: 34 1 8 'Name' 18000 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 27 1186 6834 8 'Original Source' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 18880 ##(Smalltalk.Icon) 8 'COMPILEDMETHOD_PRIVATE.ico' #fromId: #text:iconBlock: 18896 11 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 14688 34 2 8 1445007428 1025 18992 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 1074 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 18992 nil 8 4294910645 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 19330 77 786694 ##(Smalltalk.IndexedColor) 33554457 19378 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 19344 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 19360 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 752 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 18992 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 19378 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 19344 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 18992 65025 3 false 1 nil nil nil nil nil nil 3 19794 3 18992 33423361 5 false 3 nil nil nil nil nil nil 5 19794 5 18992 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 19330 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 19330 1027 19906 60239 19906 31326207 1 nil nil nil nil #Warning nil nil nil #Error 19330 1031 19906 335 19906 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 7 850 #createAt:extent: 34 2 898 9 55 898 1769 479 18992 850 #modificationEventMask: 8 #(4607) 18992 850 #caretPeriod: 8 #(530) 18992 850 #wordWrap: 8 #(true) 18992 850 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 18992 61 3 nil nil 5 nil 20290 3 18992 1 nil 3 67108863 5 nil 20290 5 18992 1 nil 3 -67108863 5 nil 18992 850 #tabWidth: 8 #(4) 18992 850 #maxCompletionListHeight: 8 #(9) 18992 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 29 1186 6834 8 'SUnit' 1746 0 nil 1778 4 1 1744 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 20512 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 14688 34 2 8 1140850688 131073 20640 nil 1074 8 4278190080 nil 5 nil nil nil 20640 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 55 898 1769 479 20640 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 752 2096 nil 27 1186 6834 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 14688 34 2 8 1445007428 1025 20960 19058 nil false 19104 nil 1074 8 4278190080 nil 5 15202 nil true 34 11 15250 1 3186 #editSave 8 '&Save' 9383 1 nil nil nil 15362 4097 15250 1 3186 #editUndo 8 '&Undo' 9397 1 nil nil nil 15250 1 3186 #editRedo 8 '&Redo' 9395 1 nil nil nil 15362 4097 15250 1 3186 #editCut 8 'Cu&t' 9393 1 nil nil nil 15250 1 3186 #editCopy 8 '&Copy' 9351 1 nil nil nil 15250 1 3186 #editPaste 8 '&Paste' 9389 1 nil nil nil 15250 1 3186 #editDelete 8 '&Delete' 1629 1 nil nil nil 15362 4097 15250 1 3186 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 19154 nil true 19186 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 20960 nil 8 4294910645 19266 nil nil 13 nil 170 192 34 4 #normal 19330 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 19330 77 19392 19408 1 nil nil nil nil #callTip nil nil nil 34 256 21680 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 21696 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 752 170 192 34 2 #default 19522 1 nil nil nil 20960 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 19584 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 21680 nil nil nil nil 1 nil 170 192 34 6 1 19794 1 20960 65025 3 false 1 nil nil nil nil nil nil 3 19794 3 20960 33423361 5 false 3 nil nil nil nil nil nil 5 19794 5 20960 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 19330 1029 nil 19906 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 19330 1027 19906 60239 19906 31326207 1 nil nil nil nil #Warning nil nil nil #Error 19330 1031 19906 335 19906 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 8 850 #createAt:extent: 34 2 898 9 55 898 1769 479 20960 850 #contextMenu: 34 1 21072 20960 850 #modificationEventMask: 8 #(4607) 20960 850 #caretPeriod: 8 #(530) 20960 850 #wordWrap: 8 #(true) 20960 850 #margins: 34 1 34 3 20290 1 20960 1 3 nil nil 5 nil 20290 3 20960 33 nil 3 67108863 5 nil 20290 5 20960 1 nil 3 -67108863 5 nil 20960 850 #tabWidth: 8 #(4) 20960 850 #maxCompletionListHeight: 8 #(9) 20960 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 29 1186 6834 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 14688 34 2 8 1445007428 1025 22752 19058 nil false 19104 nil 1074 21056 nil 5 15202 nil true 34 1 15250 1 3186 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 19154 nil true 19186 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 22752 nil 8 4294910645 19266 nil nil 11 nil 170 192 34 4 #normal 19330 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 19330 77 19392 19408 1 nil nil nil nil #callTip nil nil nil 34 256 23072 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 23088 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 19522 1 nil nil nil 22752 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 19584 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 23072 nil nil nil nil 1 nil 170 192 34 6 1 19794 1 22752 65025 3 false 1 nil nil nil nil nil nil 3 19794 3 22752 33423361 5 false 3 nil nil nil nil nil nil 5 19794 5 22752 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 19330 1029 nil 19906 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 19330 1027 19906 60239 19906 31326207 1 nil nil nil nil #Warning nil nil nil #Error 19330 1031 19906 335 19906 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 8 850 #createAt:extent: 34 2 898 9 55 898 1769 479 22752 850 #contextMenu: 34 1 22848 22752 850 #modificationEventMask: 8 #(4607) 22752 850 #caretPeriod: 8 #(530) 22752 850 #wordWrap: 8 #(true) 22752 850 #margins: 34 1 34 3 20290 1 22752 1 3 nil nil 5 nil 20290 3 22752 33 nil 3 67108863 5 nil 20290 5 22752 1 nil 3 -67108863 5 nil 22752 850 #tabWidth: 8 #(4) 22752 850 #maxCompletionListHeight: 8 #(9) 22752 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 29 1186 6834 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 14688 34 2 8 1445007428 1025 24048 19058 nil false 19104 nil 1074 8 4278190080 nil 5 nil 19154 nil true 19186 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 24048 nil 8 4294910645 19266 nil nil 13 nil 170 192 34 4 #normal 19330 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 19330 77 19392 19408 1 nil nil nil nil #callTip nil nil nil 34 256 24288 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 24304 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 19442 1 nil nil false 138 144 752 170 192 34 2 #default 19522 1 nil nil nil 24048 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 19584 nil nil 65 nil nil 21856 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 24288 #callTip 24304 nil nil nil nil 1 nil 170 192 34 12 1 19794 1 24048 65025 3 false 1 nil nil nil nil nil nil 3 19794 3 24048 33423361 5 false 3 nil nil nil nil nil nil 5 19794 5 24048 511 1 false 5 nil nil nil nil nil nil 8 'indicator9' 19794 19 24048 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator8' 19794 17 24048 33554447 1 nil nil nil nil nil nil nil nil 8 'indicator10' 19794 21 24048 33554459 3 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 19330 1029 nil 19906 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 19330 1027 19906 60239 19906 31326207 1 nil nil nil nil #Warning nil nil nil #Error 19330 1031 19906 335 19906 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 7 850 #createAt:extent: 34 2 898 9 55 898 1769 479 24048 850 #modificationEventMask: 8 #(4607) 24048 850 #hoverTime: 8 #(200) 24048 850 #caretPeriod: 8 #(530) 24048 850 #margins: 34 1 34 3 20290 1 24048 61 3 nil nil 5 nil 20290 3 24048 1 nil 3 67108863 nil nil 20290 5 24048 1 nil 3 -67108863 nil nil 24048 850 #tabWidth: 8 #(4) 24048 850 #maxCompletionListHeight: 8 #(9) 24048 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 29 22752 170 192 34 14 14960 8 'packageInfo' 18000 8 'globals' 18992 8 'originalSource' 20960 8 'classDefinition' 24048 8 'methodSource' 22752 8 'classDocumentation' 20640 8 'sunit' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 14688 34 2 8 1140916736 1 25424 1442 138 144 34 7 14832 17872 20928 22720 18864 24016 20480 nil 1504 nil nil 1 nil nil nil 25424 nil 8 4294907187 1746 0 nil 7810 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 25552 7 257 nil 1746 0 nil 7810 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 25600 7 257 nil 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 1785 541 25424 850 #basicSelectionsByIndex: 34 1 8 #(4) 25424 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 25424 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 14 1 0 0] 8 #() 2096 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 549 898 1785 541 14688 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 1 0 0 124 3 0 0 32 2 0 0] 34 8 14960 18000 20960 22752 18992 24048 20640 25424 2096 nil 27 14658 3 5 true 170 192 34 4 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 26096 nil 1074 4240 nil 517 nil nil nil 26096 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 539 898 1785 11 26096 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 13 1 0 0 124 3 0 0 18 1 0 0] 8 #() 2096 nil 27 8 'splitter' 14688 8 'textAreaTabs' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 7679 21 898 1785 1089 416 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 123 18 0 0 42 2 0 0] 34 3 544 26096 14688 2096 nil 27 )! !
!RowanSystemBrowserPresenter class categoriesFor: #resource_Default_view!private!resources-views! !

