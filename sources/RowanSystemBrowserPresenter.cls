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

compareAncestor

	| repository stream patch |
	repository := repositoryListPresenter hasSelection 
		ifTrue: [repositoryListPresenter selection]
		ifFalse: [repositoryListPresenter list first].
	stream := (WriteStream on: String new)
		nextPutAll: 'comparePackages'; tab;
		nextPutAll: packageListPresenter selections first key; tab;
		nextPutAll: (ancestorListPresenter selections first at: 2); tab;
		nextPutAll: (repository at: 2); tab;
		lf.
	(self updateCommand: stream contents) = 'comparePackages' ifFalse: [self error: 'Unexpected response!!'].
	patch := MCPatch
		fromString: readStream upToEnd
		session: self gciSession.
	patch operations isEmpty ifTrue: [
		MessageBox notify: 'No changes!!'.
		^self.
	].
	RowanPatchBrowser showOn: patch name: packageListPresenter selections first key
!

createComponents

	super createComponents.
	self registerPresenterForUpdates.!

deletePackage
	| packageName |
	packageListPresenter selections isEmpty ifTrue:[^MessageBox notify: 'No package selected'].
	packageName := packageListPresenter selections first key.
	(MessageBox confirm: 'Do you want to delete Rowan package ''' , packageName , '''?' caption: 'Confirm Delete')
		ifFalse: [^self].
	self updateCommand: 'unloadPackage' , Character tab asString , packageName , Character lf asString
				, self requestString!

inspectPackage

	| service |
	service := (RowanPackageService on: gciSession) name: packageListPresenter selection key.
	service inspectDefinition!

intialize
	super initialize. !

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

packageTabName
	^'Rowan Packages'!

pkgsMenuStrings

	0 == 1 ifTrue: [
		self findClass; addPackage; loadLatestVersion; savePackage; showPackageChanges; deletePackage; inspectDefinition.
	].
	^#(
		'&Packages'
		'&Find Class.../Ctrl+Shift+F/findClass'
		'&Add Package...//addPackage'
		'-'
		'&Load Latest Version//loadLatestVersion'
		'&Save Package..//savePackage'
		'Show &Changes//showPackageChanges'
		'&Delete Package...//deletePackage'
		'-'
		'&Inspect Package...//inspectPackage'
	).
!

possiblyShowGenericPackageClassTemplate: newDef
	"We got a template back from the server. However, it might not be
	in the rowan style. Get a new one under some conditions."

	| packageService newClassDef selections |
	newClassDef := newDef.
	(classListPresenter selections isEmpty and: [classHierarchyPresenter selections isEmpty])
		ifTrue: 
			[packageService := (self isPackageListTabSelected
						and: [(selections := packageListPresenter selections) notEmpty])
							ifTrue: [(RowanPackageService on: gciSession) name: selections first key]
							ifFalse: 
								[(classCategoryPresenter selections isEmpty
									or: [classCategoryPresenter selections first value = '--Categories--'])
										ifTrue: [RowanPackageService on: gciSession]
										ifFalse: [(RowanPackageService on: gciSession) name: classCategoryPresenter selections first value]].
			newClassDef := packageService genericClassCreationTemplate].
	^newClassDef!

presentersToRegister
	| presenters |
	presenters := Dictionary new.
	presenters at: dictionaryListPresenter put: #dictionaryUpdate:.
	^presenters!

resetClassDefinitionPresenter
	classDefinitionPresenter view
		backcolor: Color white;
		isModified: false;
		yourself!

showPackageChanges
	|  stream patch |
	stream := (WriteStream on: String new)
				nextPutAll: 'changesInPackage';
				tab;
				nextPutAll: packageListPresenter selections first key;
				tab;
				nextPutAll: (RowanProjectService on: gciSession) sampleProjectName;
				tab;
				lf.	 
	(self updateCommand: stream contents) = 'changesInPackage'
		ifFalse: [self error: 'Unexpected response!!'].
	patch := MCPatch fromString: readStream upToEnd session: self gciSession. 
	patch operations isEmpty
		ifTrue: 
			[MessageBox notify: 'No changes!!'.
			^self]. 
	RowanPatchBrowser showOn: patch name: packageListPresenter selections first key!

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
							[classDefinition := (classDefinition copyFrom: 1 to: index - 1)
										, (packageListPresenter selectionIfNone: ['User Classes' -> nil]) key
											, (classDefinition copyFrom: index + 12 to: classDefinition size)]]].
	classDefinitionPresenter value: classDefinition.
	classDefinitionPresenter view
		backcolor: Color white;
		isModified: false;
		yourself!

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
							[classDefinition := (classDefinition copyFrom: 1 to: index - 1)
										, (packageListPresenter selectionIfNone: ['User Classes' -> nil]) key
											, (classDefinition copyFrom: index + 12 to: classDefinition size)]]].
	classDefinitionPresenter value: classDefinition.
	classDefinitionPresenter view
		backcolor: Color white;
		isModified: false;
		yourself!

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
		updateMethod;
		updateTabLabel;
		yourself]
			ensure: [inUpdate := false].
	classDefinitionPresenter value: (self possiblyShowGenericPackageClassTemplate: classDefinition).
	self resetClassDefinitionPresenter!

updateTabLabel

	| cardNumber newLabel |
	cardNumber := self parentPresenter view cards indexOf: self view.
	(newLabel := self selectedClassNameWithoutVersion) notEmpty ifTrue: [
		instanceClassTabs currentCard name = 'classTab' ifTrue: [
			newLabel := newLabel , ' class'.
		].
		0 < environment ifTrue: [newLabel := newLabel , ' [' , environment printString , ']'].
	] ifFalse: [
		| tabName selections |
		tabName := packageDictionaryTabs currentCard name.
		tabName = 'packageList' ifTrue: [
			newLabel := 'Rowan Packages'.
			selections := packageListPresenter selections.
		] ifFalse: [
			newLabel := 'Dictionaries'.
			selections := dictionaryListPresenter selections.
		].
		selections size = 1 ifTrue: [newLabel := selections first key].
	].
	self view arrangement: cardNumber printString , ' ' , newLabel.
	self parentPresenter view updateTabs.
! !
!RowanSystemBrowserPresenter categoriesFor: #addPackage!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #addSubclass!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #browseClassReferencesOf:!public! !
!RowanSystemBrowserPresenter categoriesFor: #categoriesMenuStrings!menus!public! !
!RowanSystemBrowserPresenter categoriesFor: #classesMenuStrings!menus!public! !
!RowanSystemBrowserPresenter categoriesFor: #classService!private!rowan! !
!RowanSystemBrowserPresenter categoriesFor: #compareAncestor!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #createComponents!initialization!public! !
!RowanSystemBrowserPresenter categoriesFor: #deletePackage!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #inspectPackage!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #intialize!initialization!public! !
!RowanSystemBrowserPresenter categoriesFor: #labelOfClass:changedTo:!event handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #packageTabName!menus!overrides!private! !
!RowanSystemBrowserPresenter categoriesFor: #pkgsMenuStrings!menus!public! !
!RowanSystemBrowserPresenter categoriesFor: #possiblyShowGenericPackageClassTemplate:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #presentersToRegister!initialization!public!rowan! !
!RowanSystemBrowserPresenter categoriesFor: #resetClassDefinitionPresenter!private!updating! !
!RowanSystemBrowserPresenter categoriesFor: #showPackageChanges!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #updateClassHierarchyFrom:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateClassInfo!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateFromCommand:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateMethodFilterFrom:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateMethodFrom:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateMethodsFrom:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateOverrideListFrom:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updatePresenters!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateTabLabel!public!updating! !

!RowanSystemBrowserPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 656 nil nil nil 5 nil nil nil 656 482 170 176 8 #() false 170 192 752 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1075 1 898 711 539 656 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 2 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 656 34 2 8 1409286144 131073 1008 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 517 nil nil nil 1008 655878 ##(Smalltalk.CardLayout) 138 144 34 3 721414 ##(Smalltalk.Association) 8 'Categories' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 1008 34 2 8 1140850688 131073 1264 nil 1074 1104 nil 5 nil nil nil 1264 1136 170 192 34 6 410 ##(Smalltalk.ListView) 34 30 nil 1264 34 2 8 1409372233 1025 1376 590662 2 ##(Smalltalk.ListModel) 138 144 752 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 1074 8 4278190080 nil 21 nil nil nil 1376 nil 8 4294903061 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 283 #left 1570 #displayString 1600 ##(Smalltalk.SortedCollection) nil nil 1376 nil 3 nil 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 9 1 1744 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 1760 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 291 487 1376 850 #text: 34 1 8 'Column 1' 1376 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 898 193 193 nil 27 8 'pragmaList' 1232 8 'categoryList' 410 ##(Smalltalk.ListView) 34 30 nil 1264 34 2 8 1409372233 1025 2144 1442 138 144 752 nil 1504 1074 1536 nil 21 nil nil nil 2144 nil 8 4294903061 1570 #displayString 1600 nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 283 #left 1570 #displayString 1600 ##(Smalltalk.SortedCollection) nil nil 2144 nil 3 nil 1746 0 nil 1778 9 1 1744 1808 1824 1840 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 2368 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 291 487 2144 850 #text: 34 1 8 'Column 1' 2144 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2096 nil 27 8 'variableList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 1008 34 2 8 1140916864 1 2656 1442 138 144 34 3 1216 8 'Variables' 8 'Pragmas' nil 1504 1074 1104 nil 1 nil nil nil 2656 nil 8 4294903839 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil #noIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 351 539 2656 850 #basicSelectionsByIndex: 34 1 8 #(1) 2656 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2656 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 8 #() 2096 nil 27 nil 1008 898 33 33 1049862 ##(Smalltalk.ButtonInteractor) 1264 nil 590342 ##(Smalltalk.Rectangle) 898 255 3 898 287 35 1 1180998 4 ##(Smalltalk.CommandDescription) #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 53 9 898 291 523 1264 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 171 0 0 0 9 1 0 0] 34 3 1232 2144 1376 2096 nil 27 34 2 8 1409372233 1025 1232 1442 138 144 752 nil 1504 1074 1536 nil 21 nil nil nil 1232 nil 8 4294903061 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 283 #left 1570 #displayString 3504 ##(Smalltalk.SortedCollection) nil nil 1232 nil 3 nil 1746 0 nil 1778 1 83886081 1744 8 'doIt' 8 '[:each | each ]' 1779525917 3600 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 291 487 1232 850 #text: 34 1 8 'Column 1' 1232 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2096 nil 27 1186 2768 2144 1186 2784 1376 1232 170 192 34 2 1264 8 'methodFilterTabs' nil 2656 1264 898 201 201 401 1 31 nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 351 539 1008 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 34 2 1264 2656 2096 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 4160 nil 1074 8 4278190080 nil 517 nil nil nil 4160 786 138 144 34 1 850 #createAt:extent: 34 2 898 351 1 898 11 539 4160 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 0 0 0 0 180 0 0 0 13 1 0 0] 8 #() 2096 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 4416 nil nil nil 5 nil nil nil 4416 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ComboBox) 34 17 nil 4416 34 2 8 1412498947 1025 4544 1442 138 144 752 nil 1504 1074 8 4278190080 nil 5 nil nil nil 4544 nil 8 4294903815 1570 #displayString 8 #() 752 401 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 493 898 351 53 4544 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 246 0 0 0 175 0 0 0 16 1 0 0] 8 #() 2096 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 4914 #fixedParentRight 1 4914 #fixedParentBottom -45 4914 #fixedViewTop 47 410 ##(Smalltalk.ListView) 34 30 nil 4416 34 2 8 1409372233 1025 4992 1442 138 144 752 nil 1504 1074 1536 nil 13 nil nil nil 4992 nil 8 4294903061 1570 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil nil 138 144 34 2 1682 8 'Column 1' 311 #left 1570 #displayString 5136 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 1 83886081 1744 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 5232 7 257 nil nil 4992 nil 3 nil 1746 0 nil 1778 5 1 1744 8 'doIt' 8 '[:each | (each item at: 4) ifTrue: [each font: each font beItalic]]' 8 #[33 105 226 0 214 4 148 123 17 226 1 160 179 106 60 106] #item #font #beItalic #font: 5312 7 257 nil 1682 8 '' 33 #center 850 #empty 8 #() 16 1570 #<= 8 #() nil nil 4992 1746 0 nil 1778 6 1 1744 8 'doIt' 8 '[:each | ((each at: 2) ifTrue: [RowanSystemBrowserPresenter overriddenIcon] ifFalse: [Icon blank]) 
	imageIndex]' 8 #[34 105 17 64 148 120 29 159 111 31 161 162 106] ##(Smalltalk.RowanSystemBrowserPresenter) #overriddenIcon ##(Smalltalk.Icon) #blank #imageIndex 5488 7 257 nil 1 nil nil #report 752 nil 131173 nil 8 #(nil) 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 47 898 351 447 4992 850 #text: 34 1 8 'Column 1' 4992 850 #columnOrder: 34 1 8 #(2 1) 4992 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 175 0 0 0 246 0 0 0] 8 #() 2096 nil 27 4882 4928 1 4944 1 4914 #fixedParentTop 47 4960 -45 410 ##(Smalltalk.ComboBox) 34 17 nil 4416 34 2 8 1412498947 1025 5872 1442 138 144 752 nil 1504 1074 4656 nil 5 nil nil nil 5872 nil 8 4294903815 1570 #displayString 8 #() 752 401 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 351 53 5872 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 26 0 0 0] 8 #() 2096 nil 27 4882 4928 1 4944 1 5856 1 4976 47 170 192 34 6 4544 8 'overrideList' 4992 8 'methodList' 5872 8 'superclassList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 361 1 898 351 539 4416 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 99 1 0 0 13 1 0 0] 34 3 5872 4992 4544 2096 nil 27 2096 nil 27 5 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 6448 nil nil nil 5 nil nil nil 6448 482 170 176 34 6 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 6448 34 2 8 1409286144 131073 6560 nil 1074 8 4278190080 nil 517 nil nil nil 6560 1122 138 144 34 2 1186 590662 1 ##(Smalltalk.CardLabel) 8 'Dictionaries' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Dictionaries'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6720 6752 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 6768 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 6560 34 2 8 1140850688 131073 6896 nil 1074 6640 nil 5 nil nil nil 6896 6656 170 192 34 4 410 ##(Smalltalk.ListView) 34 30 nil 6896 34 2 8 1409405001 1025 7008 1442 138 144 752 nil 1504 1074 8 4278190080 nil 5 nil nil nil 7008 nil 8 4294903061 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 305 #left 1570 #displayString 7168 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 2 1 1744 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 7264 7 257 nil nil 7008 nil 3 nil 1746 0 nil 1778 6 1 1744 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[34 105 226 0 142 221 9 17 226 1 100 160 97 161 180 106 60 106] #item #font #beBold #beItalic #font: 7344 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 313 487 7008 850 #text: 34 1 8 'Column 1' 7008 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 156 0 0 0 5 1 0 0] 8 #() 2096 nil 27 8 'packageList' 6864 8 'dictionaryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 6560 34 2 8 1140916864 1 7696 1442 138 144 34 2 6722 8 'Rowan Packages' 1746 0 nil 1778 7 1 6720 8 'doIt' 8 '(CardLabel text: ''Rowan Packages'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[45 30 35 113 47 32 180 106 195 105] 983558 ##(Smalltalk.VariableBinding) #CardLabel 6720 7824 7922 #Icon ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 7840 11 1 nil nil 6736 nil 1504 1074 6640 nil 1 nil nil nil 7696 nil 8 4294903839 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 373 539 7696 850 #basicSelectionsByIndex: 34 1 8 #(1) 7696 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 7696 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 186 0 0 0 13 1 0 0] 8 #() 2096 nil 27 nil 6560 898 33 33 3090 6896 nil 3122 898 277 3 898 309 35 1 3186 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 53 9 898 313 523 6896 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 182 0 0 0 9 1 0 0] 34 2 7008 6864 2096 nil 27 34 2 8 1409372233 1025 6864 1442 138 144 752 nil 1504 1074 7120 nil 21 nil nil nil 6864 nil 8 4294903061 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 305 #left 1570 #displayString 8640 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 2 1 1744 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 8736 7 257 nil nil 6864 nil 3 nil 1746 0 nil 1778 5 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] #item #font #beBold #font: 8816 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 313 487 6864 850 #text: 34 1 8 'Column 1' 6864 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 156 0 0 0 5 1 0 0] 8 #() 2096 nil 27 1186 7808 7008 7008 170 192 34 2 6896 8 'packageDictionaryTabs' nil 7696 6896 898 201 201 401 1 31 nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 373 539 6560 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 186 0 0 0 13 1 0 0] 34 2 6896 7696 2096 nil 27 11 410 ##(Smalltalk.ContainerView) 34 15 nil 6448 34 2 8 1140850688 131073 9376 nil nil nil 5 nil nil nil 9376 4482 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 9376 34 2 8 1409286144 131073 9488 nil 1074 8 4278190080 nil 5 nil nil nil 9488 1122 138 144 34 2 1186 6722 8 'Instance' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6720 9664 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 9680 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 9488 34 2 8 1140850688 131073 9776 nil nil nil 5 nil nil nil 9776 nil 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 9 898 435 1 9776 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 221 0 0 0 4 0 0 0] 8 #() 2096 nil 27 1186 6722 8 'Class' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6720 10048 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 10064 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 9488 34 2 8 1140850688 131073 10160 nil nil nil 5 nil nil nil 10160 nil 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 9 898 435 1 10160 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 221 0 0 0 4 0 0 0] 8 #() 2096 nil 27 9776 170 192 34 4 10160 8 'classTab' 9776 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 9488 34 2 8 1140916738 1 10464 1442 138 144 34 2 9648 10032 nil 1504 nil nil 1 nil nil nil 10464 nil 8 4294903839 1746 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 10592 7 257 nil 1746 0 nil 10610 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 10656 7 257 nil 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 451 51 10464 850 #basicSelectionsByIndex: 34 1 8 #(1) 10464 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 10464 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 25 0 0 0] 8 #() 2096 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 489 898 451 51 9488 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 225 0 0 0 13 1 0 0] 34 3 9776 10160 10464 2096 nil 27 4882 4928 1 4944 1 4960 -49 4976 51 410 ##(Smalltalk.CardContainer) 34 16 nil 9376 34 2 8 1409286144 131073 11120 nil 1074 8 4278190080 nil 5 nil nil nil 11120 1122 138 144 34 2 1186 6722 8 'Classes' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6720 11296 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 11312 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 11120 34 2 8 1409372233 1025 11408 1442 138 144 752 nil 1504 1074 7120 nil 29 nil nil nil 11408 nil 8 4294903061 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 2 1682 8 'Column 1' 359 #left 1746 0 nil 1778 5 1 1744 8 'doIt' 8 '[:each | (each subStrings: Character space) first]' 8 #[33 105 17 29 159 178 161 106] ##(Smalltalk.Character) #space #subStrings: #first 11632 7 257 nil ##(Smalltalk.SortedCollection) nil nil 11408 nil 3 nil nil 1682 8 '' 69 #left 1746 0 nil 1778 6 1 1744 8 'doIt' 8 '[:each | | index |
	index := each indexOf: Character space.
	0 < index ifTrue: [each copyFrom: index + 2 to: each size - 1] ifFalse: ['''']]' 8 #[34 105 17 29 159 178 90 62 18 128 221 9 233 1 64 126 17 145 99 193 106 33 106] ##(Smalltalk.Character) #space #indexOf: #copyFrom:to: 11728 11744 7 65793 nil 1570 #<= 8 #() nil nil 11408 nil 1 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 9 55 898 435 433 11408 850 #text: 34 1 8 'Column 1' 11408 850 #columnOrder: 34 1 8 #(2 1) 11408 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 221 0 0 0 243 0 0 0] 8 #() 2096 nil 27 1186 6722 8 'Hierarchy' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6720 12176 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 12192 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 11120 34 2 8 1409352231 1025 12288 590918 3 ##(Smalltalk.TreeModel) nil 1504 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 752 1074 8 4278190080 nil 29 nil nil nil 12288 nil 8 4294902459 1746 0 nil 1778 2 1 1568 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 12480 7 257 nil ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil 170 176 752 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 752 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 55 898 435 433 12288 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 221 0 0 0 243 0 0 0] 8 #() 2096 nil 27 11408 170 192 34 4 11408 8 'classList' 12288 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 11120 34 2 8 1140916736 1 12816 1442 138 144 34 2 11280 12160 nil 1504 nil nil 1 nil nil nil 12816 nil 8 4294903839 1746 0 nil 10610 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 12944 7 257 nil 1746 0 nil 10610 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 12992 7 257 nil 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 451 495 12816 850 #basicSelectionsByIndex: 34 1 8 #(1) 12816 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 12816 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 247 0 0 0] 8 #() 2096 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 451 495 11120 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 247 0 0 0] 34 3 11408 12288 12816 2096 nil 27 4882 4928 1 4944 1 5856 1 4960 -43 170 192 34 4 9488 8 'instanceClassTabs' 11120 8 'classHierarchyTabs' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 615 1 898 451 539 9376 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 51 1 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 2 11120 9488 2096 nil 27 13 410 ##(Smalltalk.ContainerView) 34 15 nil 6448 34 2 8 1140850688 131073 13680 nil nil nil 5 nil nil nil 13680 482 170 176 752 false 170 192 34 2 410 ##(Smalltalk.TreeView) 34 27 nil 13680 34 2 8 1409352231 1025 13808 12354 nil 1504 12386 nil nil nil 170 192 752 1074 8 4278190080 nil 21 nil nil nil 13808 nil 8 4294902459 1746 0 nil 1778 5 1 1744 8 'doIt' 8 '[:each | each key isEmpty ifTrue: [''--All Categories--''] ifFalse: [each key last]].' 8 #[33 105 226 0 159 119 31 106 226 0 161 106] #key #isEmpty 8 '--All Categories--' #last 13968 7 257 nil ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil 170 176 752 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 752 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 223 539 13808 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 111 0 0 0 13 1 0 0] 8 #() 2096 nil 27 8 'classCategoryList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 383 1 898 223 539 13680 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 191 0 0 0 0 0 0 0 46 1 0 0 13 1 0 0] 34 1 13808 2096 nil 27 7 false 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 1065 539 6448 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 5 6560 410 ##(Smalltalk.Splitter) 34 12 nil 6448 34 2 8 1140850688 1 14608 nil 1074 4240 nil 517 nil nil nil 14608 786 138 144 34 1 850 #createAt:extent: 34 2 898 373 1 898 11 539 14608 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 186 0 0 0 0 0 0 0 191 0 0 0 13 1 0 0] 8 #() 2096 nil 27 13680 410 ##(Smalltalk.Splitter) 34 12 nil 6448 34 2 8 1140850688 1 14848 nil 1074 4240 nil 517 nil nil nil 14848 786 138 144 34 1 850 #createAt:extent: 34 2 898 605 1 898 11 539 14848 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 46 1 0 0 0 0 0 0 51 1 0 0 13 1 0 0] 8 #() 2096 nil 27 9376 2096 nil 27 7 false 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 1785 539 544 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 6448 410 ##(Smalltalk.Splitter) 34 12 nil 544 34 2 8 1140850688 1 15264 nil 1074 4240 nil 517 nil nil nil 15264 786 138 144 34 1 850 #createAt:extent: 34 2 898 1065 1 898 11 539 15264 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 2 0 0 0 0 0 0 25 2 0 0 13 1 0 0] 8 #() 2096 nil 27 656 2096 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 15536 nil 1074 11200 nil 5 nil nil nil 15536 1122 138 144 34 7 1186 6722 8 'Package' 1746 0 nil 1778 7 1 6720 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6720 15696 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 15712 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 15536 34 2 8 1140850688 131073 15808 nil nil nil 5 nil nil nil 15808 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 15808 34 2 8 1409355853 1025 15920 1442 138 144 752 nil 1504 1074 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 3186 #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 15920 nil 8 4294903061 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 4 1682 8 'Ancestor' 131 #left 1570 #displayString 8 #() 1570 #<= 16304 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 16336 7 257 nil nil 15920 nil 1 nil nil 1682 8 'Name' 401 #left 1570 #displayString 16208 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 1 83886081 1744 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 16464 7 257 nil nil 15920 nil 1 nil nil 1682 8 'Timestamp' 301 #left 1570 #displayString 8 #() 1570 #<= 16592 1746 0 nil 1778 1 83886081 1744 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 16624 7 257 nil nil 15920 nil 1 nil nil 1682 8 'Message' 941 #left 1570 #displayString 16592 1570 #<= 16592 1746 0 nil 1778 1 83886081 1744 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 16768 7 257 nil nil 15920 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 1769 263 15920 850 #contextMenu: 34 1 16064 15920 850 #text: 34 1 8 'Ancestor' 15920 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 2096 nil 27 23 410 ##(Smalltalk.ListView) 34 30 nil 15808 34 2 8 1409355853 1025 17120 1442 138 144 752 nil 1504 1074 16032 nil 5 16050 nil true 34 5 16098 1 3186 #addRepository 8 'Add &Repository' 1 1 nil nil nil 16098 1 3186 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 16098 1 3186 #savePackage 8 '&Save Package' 1 1 nil nil nil 16098 1 3186 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 17120 nil 8 4294903061 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 4 1682 8 'Repository Type' 211 #left 1570 #displayString 17536 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 2 1 1744 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 17632 7 257 nil nil 17120 nil 1 nil nil 1682 8 'Description' 1161 #left 1570 #displayString 16592 1570 #<= 16592 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 17776 7 257 nil nil 17120 nil 3 nil nil 1682 8 'User' 201 #left 1570 #displayString 16304 1570 #<= 16304 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 17920 7 257 nil nil 17120 nil 1 nil nil 1682 8 'Password' 201 #left 1570 #displayString 16304 1570 #<= 16304 1746 0 nil 1778 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 18064 1746 0 nil 18080 19 257 nil 7 257 nil nil 17120 nil 1 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 263 898 1769 217 17120 850 #contextMenu: 34 1 17232 17120 850 #text: 34 1 8 'Repository Type' 17120 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 2096 nil 27 19 true 170 192 34 4 15920 8 'ancestorList' 17120 8 'repositoryList' nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 9 55 898 1769 479 15808 850 #text: 34 1 8 'Package' 15808 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 34 2 15920 17120 2096 nil 27 1186 6722 8 'Globals' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6720 18736 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 18752 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 15536 34 2 8 1409355853 1025 18848 1442 138 144 752 nil 1504 1074 16032 nil 5 nil nil nil 18848 nil 8 4294903061 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 3 1682 8 'Name' 587 #left 1570 #displayString 18992 ##(Smalltalk.SortedCollection) 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 19088 7 257 nil nil 18848 nil 3 nil nil 1682 8 'Class' 589 #left 1570 #displayString 16592 1570 #<= 16592 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 19232 7 257 nil nil 18848 nil 3 nil nil 1682 8 'Value' 587 #left 1570 #displayString 16592 1570 #<= 16592 1746 0 nil 1778 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 19376 7 257 nil nil 18848 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 9 55 898 1769 479 18848 850 #text: 34 1 8 'Name' 18848 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 27 1186 6722 8 'Original Source' 1746 0 nil 1778 7 1 16 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6720 19728 ##(Smalltalk.Icon) 8 'COMPILEDMETHOD_PRIVATE.ico' #fromId: #text:iconBlock: 19744 11 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15536 34 2 8 1445007428 1025 19840 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 1074 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 19840 nil 8 4294904147 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20178 77 786694 ##(Smalltalk.IndexedColor) 33554457 20226 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 20192 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20208 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 752 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 19840 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 20226 33554447 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 20192 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 19840 65025 3 false 1 nil nil nil nil nil nil 3 20642 3 19840 33423361 5 false 3 nil nil nil nil nil nil 5 20642 5 19840 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20178 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20178 1027 20754 60239 20754 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20178 1031 20754 335 20754 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 7 850 #createAt:extent: 34 2 898 9 55 898 1769 479 19840 850 #modificationEventMask: 8 #(4607) 19840 850 #caretPeriod: 8 #(530) 19840 850 #wordWrap: 8 #(true) 19840 850 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 19840 61 3 nil nil 5 nil 21138 3 19840 1 nil 3 67108863 5 nil 21138 5 19840 1 nil 3 -67108863 5 nil 19840 850 #tabWidth: 8 #(4) 19840 850 #maxCompletionListHeight: 8 #(9) 19840 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 29 1186 6722 8 'SUnit' 1746 0 nil 1778 4 1 1744 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 7922 #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 21360 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 15536 34 2 8 1140850688 131073 21472 nil 1074 8 4278190080 nil 5 nil nil nil 21472 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 55 898 1769 479 21472 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 752 2096 nil 27 1186 6722 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15536 34 2 8 1445007428 1025 21792 19906 nil false 19952 nil 1074 8 4278190080 nil 5 16050 nil true 34 11 16098 1 3186 #editSave 8 '&Save' 9383 1 nil nil nil 17362 4097 16098 1 3186 #editUndo 8 '&Undo' 9397 1 nil nil nil 16098 1 3186 #editRedo 8 '&Redo' 9395 1 nil nil nil 17362 4097 16098 1 3186 #editCut 8 'Cu&t' 9393 1 nil nil nil 16098 1 3186 #editCopy 8 '&Copy' 9351 1 nil nil nil 16098 1 3186 #editPaste 8 '&Paste' 9389 1 nil nil nil 16098 1 3186 #editDelete 8 '&Delete' 1629 1 nil nil nil 17362 4097 16098 1 3186 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 20002 nil true 20034 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 21792 nil 8 4294904147 20114 nil nil 13 nil 170 192 34 4 #normal 20178 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20178 77 20240 20256 1 nil nil nil nil #callTip nil nil nil 34 256 22512 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 22528 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 752 170 192 34 2 #default 20370 1 nil nil nil 21792 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 20432 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 22512 nil nil nil nil 1 nil 170 192 34 6 1 20642 1 21792 65025 3 false 1 nil nil nil nil nil nil 3 20642 3 21792 33423361 5 false 3 nil nil nil nil nil nil 5 20642 5 21792 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20178 1029 nil 20754 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20178 1027 20754 60239 20754 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20178 1031 20754 335 20754 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 8 850 #createAt:extent: 34 2 898 9 55 898 1769 479 21792 850 #contextMenu: 34 1 21904 21792 850 #modificationEventMask: 8 #(4607) 21792 850 #caretPeriod: 8 #(530) 21792 850 #wordWrap: 8 #(true) 21792 850 #margins: 34 1 34 3 21138 1 21792 1 3 nil nil 5 nil 21138 3 21792 33 nil 3 67108863 5 nil 21138 5 21792 1 nil 3 -67108863 5 nil 21792 850 #tabWidth: 8 #(4) 21792 850 #maxCompletionListHeight: 8 #(9) 21792 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 29 1186 6722 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15536 34 2 8 1445007428 1025 23584 19906 nil false 19952 nil 1074 21888 nil 5 16050 nil true 34 1 16098 1 3186 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 20002 nil true 20034 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 23584 nil 8 4294904147 20114 nil nil 11 nil 170 192 34 4 #normal 20178 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20178 77 20240 20256 1 nil nil nil nil #callTip nil nil nil 34 256 23904 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 23920 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 20370 1 nil nil nil 23584 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 20432 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 23904 nil nil nil nil 1 nil 170 192 34 6 1 20642 1 23584 65025 3 false 1 nil nil nil nil nil nil 3 20642 3 23584 33423361 5 false 3 nil nil nil nil nil nil 5 20642 5 23584 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20178 1029 nil 20754 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20178 1027 20754 60239 20754 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20178 1031 20754 335 20754 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 8 850 #createAt:extent: 34 2 898 9 55 898 1769 479 23584 850 #contextMenu: 34 1 23680 23584 850 #modificationEventMask: 8 #(4607) 23584 850 #caretPeriod: 8 #(530) 23584 850 #wordWrap: 8 #(true) 23584 850 #margins: 34 1 34 3 21138 1 23584 1 3 nil nil 5 nil 21138 3 23584 33 nil 3 67108863 5 nil 21138 5 23584 1 nil 3 -67108863 5 nil 23584 850 #tabWidth: 8 #(4) 23584 850 #maxCompletionListHeight: 8 #(9) 23584 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 29 1186 6722 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15536 34 2 8 1445007428 1025 24880 19906 nil false 19952 nil 1074 8 4278190080 nil 5 nil 20002 nil true 20034 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 24880 nil 8 4294904147 20114 nil nil 13 nil 170 192 34 4 #normal 20178 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20178 77 20240 20256 1 nil nil nil nil #callTip nil nil nil 34 256 25120 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 25136 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20290 1 nil nil false 138 144 752 170 192 34 2 #default 20370 1 nil nil nil 24880 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 20432 nil nil 65 nil nil 22688 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 25120 #callTip 25136 nil nil nil nil 1 nil 170 192 34 12 1 20642 1 24880 65025 3 false 1 nil nil nil nil nil nil 3 20642 3 24880 33423361 5 false 3 nil nil nil nil nil nil 5 20642 5 24880 511 1 false 5 nil nil nil nil nil nil 8 'indicator10' 20642 21 24880 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator9' 20642 19 24880 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator8' 20642 17 24880 33554447 1 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20178 1029 nil 20754 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20178 1027 20754 60239 20754 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20178 1031 20754 335 20754 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 7 850 #createAt:extent: 34 2 898 9 55 898 1769 479 24880 850 #modificationEventMask: 8 #(4607) 24880 850 #hoverTime: 8 #(200) 24880 850 #caretPeriod: 8 #(530) 24880 850 #margins: 34 1 34 3 21138 1 24880 61 3 nil nil 5 nil 21138 3 24880 1 nil 3 67108863 nil nil 21138 5 24880 1 nil 3 -67108863 nil nil 24880 850 #tabWidth: 8 #(4) 24880 850 #maxCompletionListHeight: 8 #(9) 24880 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2096 nil 29 23584 170 192 34 14 21792 8 'classDefinition' 21472 8 'sunit' 15808 8 'packageInfo' 18848 8 'globals' 24880 8 'methodSource' 23584 8 'classDocumentation' 19840 8 'originalSource' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 15536 34 2 8 1140916736 1 26256 1442 138 144 34 7 15680 18720 21760 23552 19712 24848 21328 nil 1504 nil nil 1 nil nil nil 26256 nil 8 4294903839 1746 0 nil 10610 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 26384 7 257 nil 1746 0 nil 10610 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 26432 7 257 nil 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 1785 541 26256 850 #basicSelectionsByIndex: 34 1 8 #(4) 26256 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 26256 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 14 1 0 0] 8 #() 2096 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 549 898 1785 541 15536 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 1 0 0 124 3 0 0 32 2 0 0] 34 8 15808 18848 21792 23584 19840 24880 21472 26256 2096 nil 27 15506 3 5 true 170 192 34 4 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 26928 nil 1074 4240 nil 517 nil nil nil 26928 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 539 898 1785 11 26928 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 13 1 0 0 124 3 0 0 18 1 0 0] 8 #() 2096 nil 27 8 'splitter' 15536 8 'textAreaTabs' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 3839 21 898 1785 1089 416 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 251 10 0 0 42 2 0 0] 34 3 544 26928 15536 2096 nil 27 )! !
!RowanSystemBrowserPresenter class categoriesFor: #resource_Default_view!private!resources-views! !

