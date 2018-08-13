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

addPackageDictionaryInfoTo: aStream
	aStream
		nextPutAll: 'dictionaryList';
		lf.
	dictionaryListPresenter selections do: 
			[:each |
			aStream
				nextPutAll: each key;
				tab].
	aStream lf!

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

createSchematicWiringForPackageDictionaryTabs

	!

getViews
	classHierarchyTabs := self view viewNamed: 'classHierarchyTabs'.
	instanceClassTabs := self view viewNamed: 'instanceClassTabs'.
	methodFilterTabs := self view viewNamed: 'methodFilterTabs'.
	textAreaTabs := self view viewNamed: 'textAreaTabs'.
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

textTabChanged
	methodSourcePresenter view cancelCallTip.
	self updateMenuBar!

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
!RowanSystemBrowserPresenter categoriesFor: #addPackageDictionaryInfoTo:!create schemantic wiring!public! !
!RowanSystemBrowserPresenter categoriesFor: #addSubclass!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #browseClassReferencesOf:!public! !
!RowanSystemBrowserPresenter categoriesFor: #categoriesMenuStrings!menus!public! !
!RowanSystemBrowserPresenter categoriesFor: #classesMenuStrings!menus!public! !
!RowanSystemBrowserPresenter categoriesFor: #classService!private!rowan! !
!RowanSystemBrowserPresenter categoriesFor: #createSchematicWiringForClassHierarchyTabs!create schemantic wiring!public! !
!RowanSystemBrowserPresenter categoriesFor: #createSchematicWiringForPackageDictionaryTabs!create schemantic wiring!public! !
!RowanSystemBrowserPresenter categoriesFor: #getViews!public! !
!RowanSystemBrowserPresenter categoriesFor: #labelOfClass:changedTo:!event handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #onViewClosed!event handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #projectDictionaryChanged!event handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #registerPresentersForUpdates!event handlers!initialization!public!rowan! !
!RowanSystemBrowserPresenter categoriesFor: #registerTabPresentersForUpdates!event handlers!initialization!public!rowan! !
!RowanSystemBrowserPresenter categoriesFor: #resetClassDefinitionPresenter!private!updating! !
!RowanSystemBrowserPresenter categoriesFor: #textTabChanged!create schemantic wiring!public! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 656 nil nil nil 5 nil nil nil 656 482 170 176 8 #() false 170 192 752 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1075 1 898 711 539 656 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 2 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 656 34 2 8 1409286144 131073 1008 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 517 nil nil nil 1008 655878 ##(Smalltalk.CardLayout) 138 144 34 3 721414 ##(Smalltalk.Association) 8 'Categories' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 1008 34 2 8 1140850688 131073 1264 nil 1074 1104 nil 5 nil nil nil 1264 1136 170 192 34 6 410 ##(Smalltalk.ListView) 34 30 nil 1264 34 2 8 1409372233 1025 1376 590662 2 ##(Smalltalk.ListModel) 138 144 752 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 1074 8 4278190080 nil 21 nil nil nil 1376 nil 8 4294910881 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 283 #left 1570 #displayString 1600 ##(Smalltalk.SortedCollection) nil nil 1376 nil 3 nil 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 9 1 1744 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 1760 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 291 487 1376 850 #text: 34 1 8 'Column 1' 1376 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 898 193 193 nil 27 8 'pragmaList' 410 ##(Smalltalk.ListView) 34 30 nil 1264 34 2 8 1409372233 1025 2128 1442 138 144 752 nil 1504 1074 1536 nil 21 nil nil nil 2128 nil 8 4294910881 1570 #displayString 1600 nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 283 #left 1570 #displayString 1600 ##(Smalltalk.SortedCollection) nil nil 2128 nil 3 nil 1746 0 nil 1778 9 1 1744 1808 1824 1840 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 2352 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 291 487 2128 850 #text: 34 1 8 'Column 1' 2128 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2096 nil 27 8 'variableList' 1232 8 'categoryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 1008 34 2 8 1140916864 1 2656 1442 138 144 34 3 1216 8 'Variables' 8 'Pragmas' nil 1504 1074 1104 nil 1 nil nil nil 2656 nil 8 4294907187 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil #noIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 351 539 2656 850 #basicSelectionsByIndex: 34 1 8 #(1) 2656 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2656 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 8 #() 2096 nil 27 nil 1008 898 33 33 1049862 ##(Smalltalk.ButtonInteractor) 1264 nil 590342 ##(Smalltalk.Rectangle) 898 255 3 898 287 35 1 1180998 4 ##(Smalltalk.CommandDescription) #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 53 9 898 291 523 1264 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 171 0 0 0 9 1 0 0] 34 3 1232 2128 1376 2096 nil 27 34 2 8 1409372233 1025 1232 1442 138 144 752 nil 1504 1074 1536 nil 21 nil nil nil 1232 nil 8 4294910881 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 283 #left 1570 #displayString 3504 ##(Smalltalk.SortedCollection) nil nil 1232 nil 3 nil 1746 0 nil 1778 1 83886081 1744 8 'doIt' 8 '[:each | each ]' 1779525917 3600 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 291 487 1232 850 #text: 34 1 8 'Column 1' 1232 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2096 nil 27 1186 2768 2128 1186 2784 1376 1232 170 192 34 2 1264 8 'methodFilterTabs' nil 2656 1264 898 201 201 401 1 31 nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 351 539 1008 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 34 2 1264 2656 2096 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 4160 nil 1074 8 4278190080 nil 517 nil nil nil 4160 786 138 144 34 1 850 #createAt:extent: 34 2 898 351 1 898 11 539 4160 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 0 0 0 0 180 0 0 0 13 1 0 0] 8 #() 2096 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 4416 nil nil nil 5 nil nil nil 4416 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ComboBox) 34 17 nil 4416 34 2 8 1412498947 1025 4544 1442 138 144 752 nil 1504 1074 8 4278190080 nil 5 nil nil nil 4544 nil 8 4294909895 1570 #displayString 8 #() 752 401 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 351 53 4544 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 26 0 0 0] 8 #() 2096 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 4914 #fixedParentRight 1 4914 #fixedParentTop 1 4914 #fixedViewTop 47 410 ##(Smalltalk.ComboBox) 34 17 nil 4416 34 2 8 1412498947 1025 4992 1442 138 144 752 nil 1504 1074 4656 nil 5 nil nil nil 4992 nil 8 4294909895 1570 #displayString 8 #() 752 401 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 493 898 351 53 4992 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 246 0 0 0 175 0 0 0 16 1 0 0] 8 #() 2096 nil 27 4882 4928 1 4944 1 4914 #fixedParentBottom -45 4976 47 410 ##(Smalltalk.ListView) 34 30 nil 4416 34 2 8 1409372233 1025 5344 1442 138 144 752 nil 1504 1074 1536 nil 13 nil nil nil 5344 nil 8 4294910881 1570 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil nil 138 144 34 2 1682 8 'Column 1' 311 #left 1570 #displayString 5488 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 1 83886081 1744 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 5584 7 257 nil nil 5344 nil 3 nil 1746 0 nil 1778 5 1 1744 8 'doIt' 8 '[:each | (each item at: 4) ifTrue: [each font: each font beItalic]]' 8 #[33 105 226 0 214 4 148 123 17 226 1 160 179 106 60 106] #item #font #beItalic #font: 5664 7 257 nil 1682 8 '' 33 #center 850 #empty 8 #() 16 1570 #<= 8 #() nil nil 5344 1746 0 nil 1778 6 1 1744 8 'doIt' 8 '[:each | ((each at: 2) ifTrue: [RowanSystemBrowserPresenter overriddenIcon] ifFalse: [Icon blank]) 
	imageIndex]' 8 #[34 105 17 64 148 120 29 159 111 31 161 162 106] ##(Smalltalk.RowanSystemBrowserPresenter) #overriddenIcon ##(Smalltalk.Icon) #blank #imageIndex 5840 7 257 nil 1 nil nil #report 752 nil 131173 nil 8 #(nil) 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 47 898 351 447 5344 850 #text: 34 1 8 'Column 1' 5344 850 #columnOrder: 34 1 8 #(2 1) 5344 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 175 0 0 0 246 0 0 0] 8 #() 2096 nil 27 4882 4928 1 4944 1 4960 47 5328 -45 170 192 34 6 4544 8 'superclassList' 4992 8 'overrideList' 5344 8 'methodList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 361 1 898 351 539 4416 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 99 1 0 0 13 1 0 0] 34 3 4544 5344 4992 2096 nil 27 2096 nil 27 5 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 6448 nil nil nil 5 nil nil nil 6448 482 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 6448 34 2 8 1140850688 131073 6560 nil nil nil 5 nil nil nil 6560 4482 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 6560 34 2 8 1409286144 131073 6672 nil 1074 8 4278190080 nil 5 nil nil nil 6672 1122 138 144 34 2 1186 590662 1 ##(Smalltalk.CardLabel) 8 'Classes' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 6864 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 6880 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 6672 34 2 8 1409372233 1025 6976 1442 138 144 752 nil 1504 1074 8 4278190080 nil 29 nil nil nil 6976 nil 8 4294910881 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 2 1682 8 'Column 1' 393 #left 1746 0 nil 1778 5 1 1744 8 'doIt' 8 '[:each | (each subStrings: Character space) first]' 8 #[33 105 17 29 159 178 161 106] ##(Smalltalk.Character) #space #subStrings: #first 7216 7 257 nil ##(Smalltalk.SortedCollection) nil nil 6976 nil 3 nil nil 1682 8 '' 69 #left 1746 0 nil 1778 6 1 1744 8 'doIt' 8 '[:each | | index |
	index := each indexOf: Character space.
	0 < index ifTrue: [each copyFrom: index + 2 to: each size - 1] ifFalse: ['''']]' 8 #[34 105 17 29 159 178 90 62 18 128 221 9 233 1 64 126 17 145 99 193 106 33 106] ##(Smalltalk.Character) #space #indexOf: #copyFrom:to: 7312 7328 7 65793 nil 1570 #<= 8 #() nil nil 6976 nil 1 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 9 55 898 469 433 6976 850 #text: 34 1 8 'Column 1' 6976 850 #columnOrder: 34 1 8 #(2 1) 6976 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 238 0 0 0 243 0 0 0] 8 #() 2096 nil 27 1186 6834 8 'Hierarchy' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 7760 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 7776 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 6672 34 2 8 1409352231 1025 7872 590918 3 ##(Smalltalk.TreeModel) nil 1504 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 752 1074 8 4278190080 nil 29 nil nil nil 7872 nil 8 4294906685 1746 0 nil 1778 2 1 1568 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 8064 7 257 nil ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil 170 176 752 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 752 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 55 898 469 433 7872 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 238 0 0 0 243 0 0 0] 8 #() 2096 nil 27 6976 170 192 34 4 6976 8 'classList' 7872 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 6672 34 2 8 1140916736 1 8400 1442 138 144 34 2 6848 7744 nil 1504 nil nil 1 nil nil nil 8400 nil 8 4294907187 1746 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 8528 7 257 nil 1746 0 nil 8546 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 8592 7 257 nil 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 485 495 8400 850 #basicSelectionsByIndex: 34 1 8 #(1) 8400 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 8400 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 242 0 0 0 247 0 0 0] 8 #() 2096 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 485 495 6672 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 242 0 0 0 247 0 0 0] 34 3 6976 7872 8400 2096 nil 27 4882 4928 1 4944 1 4960 1 5328 -43 410 ##(Smalltalk.CardContainer) 34 16 nil 6560 34 2 8 1409286144 131073 9056 nil 1074 8 4278190080 nil 5 nil nil nil 9056 1122 138 144 34 2 1186 6834 8 'Instance' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 9232 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 9248 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 9056 34 2 8 1140850688 131073 9344 nil nil nil 5 nil nil nil 9344 nil 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 9 898 469 1 9344 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 238 0 0 0 4 0 0 0] 8 #() 2096 nil 27 1186 6834 8 'Class' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 9616 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 9632 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 9056 34 2 8 1140850688 131073 9728 nil nil nil 5 nil nil nil 9728 nil 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 9 898 469 1 9728 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 238 0 0 0 4 0 0 0] 8 #() 2096 nil 27 9344 170 192 34 4 9728 8 'classTab' 9344 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 9056 34 2 8 1140916738 1 10032 1442 138 144 34 2 9216 9600 nil 1504 nil nil 1 nil nil nil 10032 nil 8 4294907187 1746 0 nil 8546 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 10160 7 257 nil 1746 0 nil 8546 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 10208 7 257 nil 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 485 51 10032 850 #basicSelectionsByIndex: 34 1 8 #(1) 10032 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 10032 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 242 0 0 0 25 0 0 0] 8 #() 2096 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 489 898 485 51 9056 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 242 0 0 0 13 1 0 0] 34 3 9344 9728 10032 2096 nil 27 4882 4928 1 4944 1 5328 -49 4976 51 170 192 34 4 6672 8 'classHierarchyTabs' 9056 8 'instanceClassTabs' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 581 1 898 485 539 6560 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 34 1 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 2 6672 9056 2096 nil 27 13 410 ##(Smalltalk.ListView) 34 30 nil 6448 34 2 8 1409372233 1025 10896 1442 138 144 752 nil 1504 1074 7088 nil 21 nil nil nil 10896 nil 8 4294910881 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 313 #left 1570 #displayString 11040 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 2 1 1744 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 11136 7 257 nil nil 10896 nil 3 nil 1746 0 nil 1778 5 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] #item #font #beBold #font: 11216 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 1 898 321 539 10896 850 #text: 34 1 8 'Column 1' 10896 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 8 #() 2096 nil 27 9 410 ##(Smalltalk.ContainerView) 34 15 nil 6448 34 2 8 1140850688 131073 11536 nil nil nil 5 nil nil nil 11536 482 170 176 752 false 170 192 34 2 410 ##(Smalltalk.TreeView) 34 27 nil 11536 34 2 8 1409352231 1025 11664 7938 nil 1504 7970 nil nil nil 170 192 752 1074 8 4278190080 nil 21 nil nil nil 11664 nil 8 4294906685 1746 0 nil 1778 5 1 1744 8 'doIt' 8 '[:each | each key isEmpty ifTrue: [''--All Categories--''] ifFalse: [each key last]].' 8 #[33 105 226 0 159 119 31 106 226 0 161 106] #key #isEmpty 8 '--All Categories--' #last 11824 7 257 nil ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil 170 176 752 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 752 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 241 539 11664 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 120 0 0 0 13 1 0 0] 8 #() 2096 nil 27 8 'classCategoryList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 331 1 898 241 539 11536 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 29 1 0 0 13 1 0 0] 34 1 11664 2096 nil 27 7 false 170 192 34 2 10896 8 'dictionaryList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 1065 539 6448 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 5 10896 410 ##(Smalltalk.Splitter) 34 12 nil 6448 34 2 8 1140850688 1 12496 nil 1074 4240 nil 517 nil nil nil 12496 786 138 144 34 1 850 #createAt:extent: 34 2 898 321 1 898 11 539 12496 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 0 0 0 0 165 0 0 0 13 1 0 0] 8 #() 2096 nil 27 11536 410 ##(Smalltalk.Splitter) 34 12 nil 6448 34 2 8 1140850688 1 12736 nil 1074 4240 nil 517 nil nil nil 12736 786 138 144 34 1 850 #createAt:extent: 34 2 898 571 1 898 11 539 12736 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 29 1 0 0 0 0 0 0 34 1 0 0 13 1 0 0] 8 #() 2096 nil 27 6560 2096 nil 27 7 false 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 1785 539 544 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 6448 410 ##(Smalltalk.Splitter) 34 12 nil 544 34 2 8 1140850688 1 13152 nil 1074 4240 nil 517 nil nil nil 13152 786 138 144 34 1 850 #createAt:extent: 34 2 898 1065 1 898 11 539 13152 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 2 0 0 0 0 0 0 25 2 0 0 13 1 0 0] 8 #() 2096 nil 27 656 2096 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 13424 nil 1074 6752 nil 5 nil nil nil 13424 1122 138 144 34 6 1186 6834 8 'Globals' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 13584 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 13600 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 13424 34 2 8 1409355853 1025 13696 1442 138 144 752 nil 1504 1074 8 4278190080 nil 5 nil nil nil 13696 nil 8 4294910881 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 3 1682 8 'Name' 587 #left 1570 #displayString 13856 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 13952 7 257 nil nil 13696 nil 3 nil nil 1682 8 'Class' 589 #left 1570 #displayString 8 #() 1570 #<= 14080 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 14112 7 257 nil nil 13696 nil 3 nil nil 1682 8 'Value' 587 #left 1570 #displayString 14080 1570 #<= 14080 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 14256 7 257 nil nil 13696 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 9 55 898 1769 479 13696 850 #text: 34 1 8 'Name' 13696 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 27 1186 6834 8 'Original Source' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6832 14608 ##(Smalltalk.Icon) 8 'COMPILEDMETHOD_PRIVATE.ico' #fromId: #text:iconBlock: 14624 11 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13424 34 2 8 1445007428 1025 14720 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 1074 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 14720 nil 8 4294910645 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 15058 77 786694 ##(Smalltalk.IndexedColor) 33554457 15106 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 15072 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 15088 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 752 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 14720 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 15106 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 15072 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 14720 65025 3 false 1 nil nil nil nil nil nil 3 15522 3 14720 33423361 5 false 3 nil nil nil nil nil nil 5 15522 5 14720 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 15058 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 15058 1027 15634 60239 15634 31326207 1 nil nil nil nil #Warning nil nil nil #Error 15058 1031 15634 335 15634 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 7 850 #createAt:extent: 34 2 898 9 55 898 1769 479 14720 850 #modificationEventMask: 8 #(4607) 14720 850 #caretPeriod: 8 #(530) 14720 850 #wordWrap: 8 #(true) 14720 850 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 14720 61 3 nil nil 5 nil 16018 3 14720 1 nil 3 67108863 5 nil 16018 5 14720 1 nil 3 -67108863 5 nil 14720 850 #tabWidth: 8 #(4) 14720 850 #maxCompletionListHeight: 8 #(9) 14720 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 29 1186 6834 8 'SUnit' 1746 0 nil 1778 4 1 1744 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 16240 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 13424 34 2 8 1140850688 131073 16368 nil 1074 8 4278190080 nil 5 nil nil nil 16368 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 55 898 1769 479 16368 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 752 2096 nil 27 1186 6834 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13424 34 2 8 1445007428 1025 16688 14786 nil false 14832 nil 1074 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 11 984134 2 ##(Smalltalk.CommandMenuItem) 1 3186 #editSave 8 '&Save' 9383 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 16850 1 3186 #editUndo 8 '&Undo' 9397 1 nil nil nil 16850 1 3186 #editRedo 8 '&Redo' 9395 1 nil nil nil 16914 4097 16850 1 3186 #editCut 8 'Cu&t' 9393 1 nil nil nil 16850 1 3186 #editCopy 8 '&Copy' 9351 1 nil nil nil 16850 1 3186 #editPaste 8 '&Paste' 9389 1 nil nil nil 16850 1 3186 #editDelete 8 '&Delete' 1629 1 nil nil nil 16914 4097 16850 1 3186 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 14882 nil true 14914 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 16688 nil 8 4294910645 14994 nil nil 13 nil 170 192 34 4 #normal 15058 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 15058 77 15120 15136 1 nil nil nil nil #callTip nil nil nil 34 256 17456 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 17472 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 752 170 192 34 2 #default 15250 1 nil nil nil 16688 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 15312 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 17456 nil nil nil nil 1 nil 170 192 34 6 1 15522 1 16688 65025 3 false 1 nil nil nil nil nil nil 3 15522 3 16688 33423361 5 false 3 nil nil nil nil nil nil 5 15522 5 16688 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 15058 1029 nil 15634 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 15058 1027 15634 60239 15634 31326207 1 nil nil nil nil #Warning nil nil nil #Error 15058 1031 15634 335 15634 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 8 850 #createAt:extent: 34 2 898 9 55 898 1769 479 16688 850 #contextMenu: 34 1 16816 16688 850 #modificationEventMask: 8 #(4607) 16688 850 #caretPeriod: 8 #(530) 16688 850 #wordWrap: 8 #(true) 16688 850 #margins: 34 1 34 3 16018 1 16688 1 3 nil nil 5 nil 16018 3 16688 33 nil 3 67108863 5 nil 16018 5 16688 1 nil 3 -67108863 5 nil 16688 850 #tabWidth: 8 #(4) 16688 850 #maxCompletionListHeight: 8 #(9) 16688 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 29 1186 6834 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13424 34 2 8 1445007428 1025 18528 14786 nil false 14832 nil 1074 16784 nil 5 16802 nil true 34 1 16850 1 3186 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 14882 nil true 14914 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 18528 nil 8 4294910645 14994 nil nil 11 nil 170 192 34 4 #normal 15058 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 15058 77 15120 15136 1 nil nil nil nil #callTip nil nil nil 34 256 18848 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 18864 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 15250 1 nil nil nil 18528 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 15312 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 18848 nil nil nil nil 1 nil 170 192 34 6 1 15522 1 18528 65025 3 false 1 nil nil nil nil nil nil 3 15522 3 18528 33423361 5 false 3 nil nil nil nil nil nil 5 15522 5 18528 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 15058 1029 nil 15634 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 15058 1027 15634 60239 15634 31326207 1 nil nil nil nil #Warning nil nil nil #Error 15058 1031 15634 335 15634 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 8 850 #createAt:extent: 34 2 898 9 55 898 1769 479 18528 850 #contextMenu: 34 1 18624 18528 850 #modificationEventMask: 8 #(4607) 18528 850 #caretPeriod: 8 #(530) 18528 850 #wordWrap: 8 #(true) 18528 850 #margins: 34 1 34 3 16018 1 18528 1 3 nil nil 5 nil 16018 3 18528 33 nil 3 67108863 5 nil 16018 5 18528 1 nil 3 -67108863 5 nil 18528 850 #tabWidth: 8 #(4) 18528 850 #maxCompletionListHeight: 8 #(9) 18528 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 29 1186 6834 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13424 34 2 8 1445007428 1025 19824 14786 nil false 14832 nil 1074 8 4278190080 nil 5 nil 14882 nil true 14914 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 19824 nil 8 4294910645 14994 nil nil 13 nil 170 192 34 4 #normal 15058 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 15058 77 15120 15136 1 nil nil nil nil #callTip nil nil nil 34 256 20064 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20080 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 15170 1 nil nil false 138 144 752 170 192 34 2 #default 15250 1 nil nil nil 19824 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 15312 nil nil 65 nil nil 17632 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 20064 #callTip 20080 nil nil nil nil 1 nil 170 192 34 12 1 15522 1 19824 65025 3 false 1 nil nil nil nil nil nil 3 15522 3 19824 33423361 5 false 3 nil nil nil nil nil nil 5 15522 5 19824 511 1 false 5 nil nil nil nil nil nil 8 'indicator9' 15522 19 19824 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator8' 15522 17 19824 33554447 1 nil nil nil nil nil nil nil nil 8 'indicator10' 15522 21 19824 33554459 3 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 15058 1029 nil 15634 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 15058 1027 15634 60239 15634 31326207 1 nil nil nil nil #Warning nil nil nil #Error 15058 1031 15634 335 15634 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 7 850 #createAt:extent: 34 2 898 9 55 898 1769 479 19824 850 #modificationEventMask: 8 #(4607) 19824 850 #hoverTime: 8 #(200) 19824 850 #caretPeriod: 8 #(530) 19824 850 #margins: 34 1 34 3 16018 1 19824 61 3 nil nil 5 nil 16018 3 19824 1 nil 3 67108863 nil nil 16018 5 19824 1 nil 3 -67108863 nil nil 19824 850 #tabWidth: 8 #(4) 19824 850 #maxCompletionListHeight: 8 #(9) 19824 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 29 13696 170 192 34 12 16368 8 'sunit' 16688 8 'classDefinition' 19824 8 'methodSource' 14720 8 'originalSource' 18528 8 'classDocumentation' 13696 8 'globals' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 13424 34 2 8 1140916736 1 21184 1442 138 144 34 6 13568 16656 18496 14592 19792 16208 nil 1504 nil nil 1 nil nil nil 21184 nil 8 4294907187 1746 0 nil 8546 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 21312 7 257 nil 1746 0 nil 8546 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 21360 7 257 nil 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 1785 541 21184 850 #basicSelectionsByIndex: 34 1 8 #(1) 21184 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 21184 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 14 1 0 0] 8 #() 2096 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 549 898 1785 541 13424 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 1 0 0 124 3 0 0 32 2 0 0] 34 7 13696 16688 18528 14720 19824 16368 21184 2096 nil 27 13394 3 5 true 170 192 34 4 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 21856 nil 1074 4240 nil 517 nil nil nil 21856 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 539 898 1785 11 21856 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 13 1 0 0 124 3 0 0 18 1 0 0] 8 #() 2096 nil 27 8 'splitter' 13424 8 'textAreaTabs' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 7679 21 898 1785 1089 416 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 123 18 0 0 42 2 0 0] 34 3 544 21856 13424 2096 nil 27 )! !
!RowanSystemBrowserPresenter class categoriesFor: #resource_Default_view!private!resources-views! !

