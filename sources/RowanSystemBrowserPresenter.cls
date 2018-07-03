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

deletePackage
	| packageName |
	packageListPresenter selections isEmpty ifTrue:[^MessageBox notify: 'No package selected'].
	packageName := packageListPresenter selections first key.
	(MessageBox confirm: 'Do you want to delete Rowan package ''' , packageName , '''?' caption: 'Confirm Delete')
		ifFalse: [^self].
	self updateCommand: 'unloadPackage' , Character tab asString , packageName , Character lf asString
				, self requestString!

getViews

	super getViews.
	self registerTabPresentersForUpdates!

inspectPackage

	| service |
	service := (RowanPackageService on: gciSession) name: packageListPresenter selection key.
	service inspectDefinition!

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
	
	super onViewClosed.
	subPresenters do:[:presenter |
		BrowserUpdate current removeEventsTriggeredFor: presenter]!

onViewOpened
	
	super onViewOpened.
	self registerPresentersForUpdates!

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

registerPresentersForUpdates
	BrowserUpdate current register: dictionaryListPresenter with: #dictionaryUpdate:.
	BrowserUpdate current register: globalsPresenter with: #globalsUpdate:.
	BrowserUpdate current register: packageListPresenter with: #packagesUpdate:.
	BrowserUpdate current register: classCategoryPresenter with: #classCategoryUpdate:.
	BrowserUpdate current register: classListPresenter with: #classesUpdate:.
	BrowserUpdate current register: classDefinitionPresenter with: #classDefinitionUpdate:.
	BrowserUpdate current register: classCommentPresenter with: #classCommentUpdate:.
	BrowserUpdate current register: classHierarchyPresenter with: #classHierarchyUpdate:.
	BrowserUpdate current register: superclassListPresenter with: #superclassListUpdate:.
	BrowserUpdate current register: methodListPresenter with: #methodsUpdate:.
	BrowserUpdate current register: methodSourcePresenter with: #updateSelectorFor:.!

registerTabPresentersForUpdates
	BrowserUpdate current register: self methodFilterListPresenter with: #methodFilterUpdate:!

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

updateCommand: aString onSuccessDo: aBlock
	Smalltalk at: #STONFlag ifAbsentPut: [false].	"ensure temporary flag is initialized"
	
	[| time1 time2 time3 string |
	(Smalltalk at: #STONFlag)
		ifFalse: 
			[Transcript
				cr;
				show: '---FALSE---'.
			Transcript
				cr;
				show: aString.
			Transcript
				cr;
				show: '------'.
			time1 := Time millisecondsToRun: 
							[string := self gciSession serverPerform: #systemBrowser:
										with: environment printString , ' ' , aString.
							eventCount := self gciSession eventCount].
			time2 := Time millisecondsToRun: 
							[| responseType |
							readStream := JadeServerReadStream on: string.
							time3 := readStream nextLine asNumber.
							aBlock value.
							(responseType := readStream nextLine) = 'update' ifFalse: [^responseType].
							self updatePresenters].
			self
				statusBarServerRequestText: 'server took ' , time3 printString , 'ms; ' , 'network took '
						, (time1 - time3) printString , 'ms; '
						, 'client took ' , time2 printString
						, 'ms; ' , 'total of '
						, (time1 + time2) printString , 'ms'].
	(Smalltalk at: #STONFlag)
		ifTrue: 
			[| stonResults update |
			Transcript
				cr;
				show: 'Doing STON update: ';
				show: TimeStamp current printString.
			Transcript
				cr;
				show: '---TRUE---'.
			Transcript
				cr;
				show: aString.
			Transcript
				cr;
				show: '------'.
			update := BrowserUpdate current initializeUpdates.
			stonResults := self gciSession serverPerform: #systemBrowserSTON:
						with: environment printString , ' ' , aString.
			update updates: (STON fromString: stonResults).
			self updateSelectionsFor: update.
			update updates: #()]]
			ensure: [selectedClassChanged := false].
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
							[classDefinition := (classDefinition copyFrom: 1 to: index - 1)
										, (packageListPresenter selectionIfNone: ['User Classes' -> nil]) key
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
	classDefinitionPresenter value: (self possiblyShowGenericPackageClassTemplate: classDefinition).
	self resetClassDefinitionPresenter!

updateSelectionsForUpdate: update

	update updatePackageSelectionsFor: packageListPresenter.
	update updateDictionarySelectionsFor: dictionaryListPresenter.
	update updateClassCategorySelectionsFor: classCategoryPresenter.
	update updateClassSelectionFor: classListPresenter.
	update updateClassHierarchySelectionFor: classHierarchyPresenter.
	update updateSuperclassSelectionFor: superclassListPresenter.
	update updateMethodFilterSelectionFor: self methodFilterListPresenter.
	update updateMethodSelectionFor: self methodListPresenter.!

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
!RowanSystemBrowserPresenter categoriesFor: #deletePackage!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #getViews!public! !
!RowanSystemBrowserPresenter categoriesFor: #inspectPackage!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #labelOfClass:changedTo:!event handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #onViewClosed!event handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #onViewOpened!event handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #packageTabName!menus!overrides!private! !
!RowanSystemBrowserPresenter categoriesFor: #pkgsMenuStrings!menus!public! !
!RowanSystemBrowserPresenter categoriesFor: #possiblyShowGenericPackageClassTemplate:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #registerPresentersForUpdates!event handlers!initialization!public!rowan! !
!RowanSystemBrowserPresenter categoriesFor: #registerTabPresentersForUpdates!event handlers!initialization!public!rowan! !
!RowanSystemBrowserPresenter categoriesFor: #resetClassDefinitionPresenter!private!updating! !
!RowanSystemBrowserPresenter categoriesFor: #showPackageChanges!menu handlers!public! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 544 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 544 655878 ##(Smalltalk.CardLayout) 138 144 34 7 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Package' 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 7 1 752 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 784 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 816 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 928 nil nil nil 5 nil nil nil 928 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 928 34 2 8 1409355853 1025 1040 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 610 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 1040 nil 8 4294902685 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Ancestor' 131 #left 1394 #displayString 8 #() 1394 #<= 1568 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 1600 7 257 nil nil 1040 nil 1 nil nil 1506 8 'Name' 401 #left 1394 #displayString 1424 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 1728 7 257 nil nil 1040 nil 1 nil nil 1506 8 'Timestamp' 301 #left 1394 #displayString 8 #() 1394 #<= 1856 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 1888 7 257 nil nil 1040 nil 1 nil nil 1506 8 'Message' 941 #left 1394 #displayString 1856 1394 #<= 1856 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 2032 7 257 nil nil 1040 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 328198 ##(Smalltalk.Point) 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 2130 1 1 2130 1769 263 1040 2226 #contextMenu: 34 1 1248 1040 2226 #text: 34 1 8 'Ancestor' 1040 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 2130 193 193 nil 27 23 410 ##(Smalltalk.ListView) 34 30 nil 928 34 2 8 1409355853 1025 2464 1106 138 144 1152 nil 1184 610 1216 nil 5 1234 nil true 34 5 1282 1 1314 #addRepository 8 'Add &Repository' 1 1 nil nil nil 1282 1 1314 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 1282 1 1314 #savePackage 8 '&Save Package' 1 1 nil nil nil 1282 1 1314 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 2464 nil 8 4294902685 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 4 1506 8 'Repository Type' 211 #left 1394 #displayString 2880 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 2976 7 257 nil nil 2464 nil 1 nil nil 1506 8 'Description' 1161 #left 1394 #displayString 1856 1394 #<= 1856 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 3120 7 257 nil nil 2464 nil 3 nil nil 1506 8 'User' 201 #left 1394 #displayString 1568 1394 #<= 1568 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 3264 7 257 nil nil 2464 nil 1 nil nil 1506 8 'Password' 201 #left 1394 #displayString 1568 1394 #<= 1568 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 3408 802 0 nil 3424 19 257 nil 7 257 nil nil 2464 nil 1 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 263 2130 1769 217 2464 2226 #contextMenu: 34 1 2576 2464 2226 #text: 34 1 8 'Repository Type' 2464 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 2448 nil 27 19 true 170 192 34 4 1040 8 'ancestorList' 2464 8 'repositoryList' nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 928 2226 #text: 34 1 8 'Package' 928 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 34 2 1040 2464 2448 nil 27 722 754 8 'Globals' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 4080 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 4096 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 544 34 2 8 1409355853 1025 4192 1106 138 144 1152 nil 1184 610 1216 nil 5 nil nil nil 4192 nil 8 4294902685 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 3 1506 8 'Name' 587 #left 1394 #displayString 4336 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 4432 7 257 nil nil 4192 nil 3 nil nil 1506 8 'Class' 589 #left 1394 #displayString 1856 1394 #<= 1856 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 4576 7 257 nil nil 4192 nil 3 nil nil 1506 8 'Value' 587 #left 1394 #displayString 1856 1394 #<= 1856 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 4720 7 257 nil nil 4192 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 4192 2226 #text: 34 1 8 'Name' 4192 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2448 nil 27 722 754 8 'Original Source' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 5072 ##(Smalltalk.Icon) 8 'COMPILEDMETHOD_PRIVATE.ico' #fromId: #text:iconBlock: 5088 11 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 5184 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 610 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2130 193 193 nil 5184 nil 8 4294902675 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5522 77 786694 ##(Smalltalk.IndexedColor) 33554457 5570 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 5536 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 5552 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 1152 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 5184 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5570 33554447 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 5536 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 5184 65025 3 false 1 nil nil nil nil nil nil 3 5986 3 5184 33423361 5 false 3 nil nil nil nil nil nil 5 5986 5 5184 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 5522 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 5522 1027 6098 60239 6098 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5522 1031 6098 335 6098 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 7 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 5184 2226 #modificationEventMask: 8 #(4607) 5184 2226 #caretPeriod: 8 #(530) 5184 2226 #wordWrap: 8 #(true) 5184 2226 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 5184 61 3 nil nil 5 nil 6482 3 5184 1 nil 3 67108863 5 nil 6482 5 5184 1 nil 3 -67108863 5 nil 5184 2226 #tabWidth: 8 #(4) 5184 2226 #maxCompletionListHeight: 8 #(9) 5184 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2448 nil 29 722 754 8 'SUnit' 802 0 nil 834 4 1 800 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 6704 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 544 34 2 8 1140850688 131073 6832 nil 610 8 4278190080 nil 5 nil nil nil 6832 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 6832 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 1152 2448 nil 27 722 754 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 7152 5250 nil false 5296 nil 610 8 4278190080 nil 5 1234 nil true 34 11 1282 1 1314 #editSave 8 '&Save' 9383 1 nil nil nil 2706 4097 1282 1 1314 #editUndo 8 '&Undo' 9397 1 nil nil nil 1282 1 1314 #editRedo 8 '&Redo' 9395 1 nil nil nil 2706 4097 1282 1 1314 #editCut 8 'Cu&t' 9393 1 nil nil nil 1282 1 1314 #editCopy 8 '&Copy' 9351 1 nil nil nil 1282 1 1314 #editPaste 8 '&Paste' 9389 1 nil nil nil 1282 1 1314 #editDelete 8 '&Delete' 1629 1 nil nil nil 2706 4097 1282 1 1314 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 5346 nil true 5378 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2130 193 193 nil 7152 nil 8 4294902675 5458 nil nil 13 nil 170 192 34 4 #normal 5522 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5522 77 5584 5600 1 nil nil nil nil #callTip nil nil nil 34 256 7872 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 7888 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 1152 170 192 34 2 #default 5714 1 nil nil nil 7152 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5776 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 7872 nil nil nil nil 1 nil 170 192 34 6 1 5986 1 7152 65025 3 false 1 nil nil nil nil nil nil 3 5986 3 7152 33423361 5 false 3 nil nil nil nil nil nil 5 5986 5 7152 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 5522 1029 nil 6098 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 5522 1027 6098 60239 6098 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5522 1031 6098 335 6098 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 8 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 7152 2226 #contextMenu: 34 1 7264 7152 2226 #modificationEventMask: 8 #(4607) 7152 2226 #caretPeriod: 8 #(530) 7152 2226 #wordWrap: 8 #(true) 7152 2226 #margins: 34 1 34 3 6482 1 7152 1 3 nil nil 5 nil 6482 3 7152 33 nil 3 67108863 5 nil 6482 5 7152 1 nil 3 -67108863 5 nil 7152 2226 #tabWidth: 8 #(4) 7152 2226 #maxCompletionListHeight: 8 #(9) 7152 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2448 nil 29 722 754 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 8944 5250 nil false 5296 nil 610 7248 nil 5 1234 nil true 34 1 1282 1 1314 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 5346 nil true 5378 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2130 193 193 nil 8944 nil 8 4294902675 5458 nil nil 11 nil 170 192 34 4 #normal 5522 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5522 77 5584 5600 1 nil nil nil nil #callTip nil nil nil 34 256 9264 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 9280 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 5714 1 nil nil nil 8944 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5776 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 9264 nil nil nil nil 1 nil 170 192 34 6 1 5986 1 8944 65025 3 false 1 nil nil nil nil nil nil 3 5986 3 8944 33423361 5 false 3 nil nil nil nil nil nil 5 5986 5 8944 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 5522 1029 nil 6098 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 5522 1027 6098 60239 6098 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5522 1031 6098 335 6098 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 8 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 8944 2226 #contextMenu: 34 1 9040 8944 2226 #modificationEventMask: 8 #(4607) 8944 2226 #caretPeriod: 8 #(530) 8944 2226 #wordWrap: 8 #(true) 8944 2226 #margins: 34 1 34 3 6482 1 8944 1 3 nil nil 5 nil 6482 3 8944 33 nil 3 67108863 5 nil 6482 5 8944 1 nil 3 -67108863 5 nil 8944 2226 #tabWidth: 8 #(4) 8944 2226 #maxCompletionListHeight: 8 #(9) 8944 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2448 nil 29 722 754 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 10240 5250 nil false 5296 nil 610 8 4278190080 nil 5 nil 5346 nil true 5378 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2130 193 193 nil 10240 nil 8 4294902675 5458 nil nil 13 nil 170 192 34 4 #normal 5522 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5522 77 5584 5600 1 nil nil nil nil #callTip nil nil nil 34 256 10480 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 10496 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 5634 1 nil nil false 138 144 1152 170 192 34 2 #default 5714 1 nil nil nil 10240 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5776 nil nil 65 nil nil 8048 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 10480 #callTip 10496 nil nil nil nil 1 nil 170 192 34 12 1 5986 1 10240 65025 3 false 1 nil nil nil nil nil nil 3 5986 3 10240 33423361 5 false 3 nil nil nil nil nil nil 5 5986 5 10240 511 1 false 5 nil nil nil nil nil nil 8 'indicator8' 5986 17 10240 33554447 1 nil nil nil nil nil nil nil nil 8 'indicator9' 5986 19 10240 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator10' 5986 21 10240 33554459 3 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 5522 1029 nil 6098 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 5522 1027 6098 60239 6098 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5522 1031 6098 335 6098 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 7 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 10240 2226 #modificationEventMask: 8 #(4607) 10240 2226 #hoverTime: 8 #(200) 10240 2226 #caretPeriod: 8 #(530) 10240 2226 #margins: 34 1 34 3 6482 1 10240 61 3 nil nil 5 nil 6482 3 10240 1 nil 3 67108863 nil nil 6482 5 10240 1 nil 3 -67108863 nil nil 10240 2226 #tabWidth: 8 #(4) 10240 2226 #maxCompletionListHeight: 8 #(9) 10240 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2448 nil 29 8944 170 192 34 14 5184 8 'originalSource' 7152 8 'classDefinition' 928 8 'packageInfo' 10240 8 'methodSource' 8944 8 'classDocumentation' 4192 8 'globals' 6832 8 'sunit' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 544 34 2 8 1140916736 1 11616 1106 138 144 34 7 768 4064 7120 8912 5056 10208 6672 nil 1184 nil nil 1 nil nil nil 11616 nil 8 4294902785 802 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 11744 7 257 nil 802 0 nil 11762 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 11808 7 257 nil 1456 nil nil nil nil nil #smallIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 1785 541 11616 2226 #basicSelectionsByIndex: 34 1 8 #(4) 11616 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 11616 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 14 1 0 0] 8 #() 2448 nil 27 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 549 2130 1785 541 544 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 1 0 0 124 3 0 0 32 2 0 0] 34 8 928 4192 7152 8944 5184 10240 6832 11616 2448 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 12288 nil nil nil 5 nil nil nil 12288 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 12288 34 2 8 1140850688 131073 12400 nil nil nil 5 nil nil nil 12400 482 170 176 1152 false 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1075 1 2130 711 539 12400 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 2 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 12400 34 2 8 1409286144 131073 12672 nil 610 8 4278190080 nil 517 nil nil nil 12672 658 138 144 34 3 722 8 'Categories' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 12672 34 2 8 1140850688 131073 12880 nil 610 12752 nil 5 nil nil nil 12880 12768 170 192 34 6 410 ##(Smalltalk.ListView) 34 30 nil 12880 34 2 8 1409372233 1025 12992 1106 138 144 1152 nil 1184 610 8 4278190080 nil 21 nil nil nil 12992 nil 8 4294902685 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 283 #left 1394 #displayString 13152 ##(Smalltalk.SortedCollection) nil nil 12992 nil 3 nil 802 0 nil 834 9 1 800 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 13248 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 37 2130 291 487 12992 2226 #text: 34 1 8 'Column 1' 12992 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2448 nil 27 8 'pragmaList' 410 ##(Smalltalk.ListView) 34 30 nil 12880 34 2 8 1409372233 1025 13584 1106 138 144 1152 nil 1184 610 13104 nil 21 nil nil nil 13584 nil 8 4294902685 1394 #displayString 13152 nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 283 #left 1394 #displayString 13152 ##(Smalltalk.SortedCollection) nil nil 13584 nil 3 nil 802 0 nil 834 9 1 800 13280 13296 13312 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 13808 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 37 2130 291 487 13584 2226 #text: 34 1 8 'Column 1' 13584 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2448 nil 27 8 'variableList' 12848 8 'categoryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 12672 34 2 8 1140916864 1 14112 1106 138 144 34 3 12832 8 'Variables' 8 'Pragmas' nil 1184 610 12752 nil 1 nil nil nil 14112 nil 8 4294902785 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil #noIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 351 539 14112 2226 #basicSelectionsByIndex: 34 1 8 #(1) 14112 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 14112 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 8 #() 2448 nil 27 nil 12672 2130 33 33 1049862 ##(Smalltalk.ButtonInteractor) 12880 nil 590342 ##(Smalltalk.Rectangle) 2130 255 3 2130 287 35 1 1314 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 53 9 2130 291 523 12880 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 171 0 0 0 9 1 0 0] 34 3 12848 13584 12992 2448 nil 27 34 2 8 1409372233 1025 12848 1106 138 144 1152 nil 1184 610 13104 nil 21 nil nil nil 12848 nil 8 4294902685 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 283 #left 1394 #displayString 14944 ##(Smalltalk.SortedCollection) nil nil 12848 nil 3 nil 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each ]' 1779525917 15040 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 37 2130 291 487 12848 2226 #text: 34 1 8 'Column 1' 12848 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2448 nil 27 722 14224 13584 722 14240 12992 12848 170 192 34 2 12880 8 'methodFilterTabs' nil 14112 12880 2130 201 201 401 1 31 nil nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 351 539 12672 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 34 2 12880 14112 2448 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 12400 34 2 8 1140850688 1 15600 nil 610 8 4278190080 nil 517 nil nil nil 15600 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 351 1 2130 11 539 15600 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 0 0 0 0 180 0 0 0 13 1 0 0] 8 #() 2448 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 12400 34 2 8 1140850688 131073 15856 nil nil nil 5 nil nil nil 15856 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ListView) 34 30 nil 15856 34 2 8 1409372233 1025 15984 1106 138 144 1152 nil 1184 610 13104 nil 13 nil nil nil 15984 nil 8 4294902685 1394 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil nil 138 144 34 2 1506 8 'Column 1' 311 #left 1394 #displayString 16128 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 16224 7 257 nil nil 15984 nil 3 nil 802 0 nil 834 5 1 800 8 'doIt' 8 '[:each | (each item at: 4) ifTrue: [each font: each font beItalic]]' 8 #[33 105 226 0 214 4 148 123 17 226 1 160 179 106 60 106] #item #font #beItalic #font: 16304 7 257 nil 1506 8 '' 33 #center 2226 #empty 8 #() 16 1394 #<= 8 #() nil nil 15984 802 0 nil 834 6 1 800 8 'doIt' 8 '[:each | ((each at: 2) ifTrue: [RowanSystemBrowserPresenter overriddenIcon] ifFalse: [Icon blank]) 
	imageIndex]' 8 #[34 105 17 64 148 120 29 159 111 31 161 162 106] ##(Smalltalk.RowanSystemBrowserPresenter) #overriddenIcon ##(Smalltalk.Icon) #blank #imageIndex 16480 7 257 nil 1 nil nil #report 1152 nil 131173 nil 8 #(nil) 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 47 2130 351 447 15984 2226 #text: 34 1 8 'Column 1' 15984 2226 #columnOrder: 34 1 8 #(2 1) 15984 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 175 0 0 0 246 0 0 0] 8 #() 2448 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 16866 #fixedParentRight 1 16866 #fixedParentTop 47 16866 #fixedParentBottom -45 410 ##(Smalltalk.ComboBox) 34 17 nil 15856 34 2 8 1412498947 1025 16944 1106 138 144 1152 nil 1184 610 8 4278190080 nil 5 nil nil nil 16944 nil 8 4294902771 1394 #displayString 8 #() 1152 401 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 351 53 16944 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 26 0 0 0] 8 #() 2448 nil 27 16834 16880 1 16896 1 16912 1 16866 #fixedViewTop 47 410 ##(Smalltalk.ComboBox) 34 17 nil 15856 34 2 8 1412498947 1025 17312 1106 138 144 1152 nil 1184 610 17056 nil 5 nil nil nil 17312 nil 8 4294902771 1394 #displayString 8 #() 1152 401 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 493 2130 351 53 17312 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 246 0 0 0 175 0 0 0 16 1 0 0] 8 #() 2448 nil 27 16834 16880 1 16896 1 16928 -45 17296 47 170 192 34 6 15984 8 'methodList' 16944 8 'superclassList' 17312 8 'overrideList' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 361 1 2130 351 539 15856 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 99 1 0 0 13 1 0 0] 34 3 16944 15984 17312 2448 nil 27 2448 nil 27 5 410 ##(Smalltalk.ContainerView) 34 15 nil 12288 34 2 8 1140850688 131073 17888 nil nil nil 5 nil nil nil 17888 482 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 17888 34 2 8 1140850688 131073 18000 nil nil nil 5 nil nil nil 18000 15922 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 18000 34 2 8 1409286144 131073 18112 nil 610 8 4278190080 nil 5 nil nil nil 18112 658 138 144 34 2 722 754 8 'Instance' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 18288 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 18304 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 18112 34 2 8 1140850688 131073 18400 nil nil nil 5 nil nil nil 18400 nil 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 9 2130 435 1 18400 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 221 0 0 0 4 0 0 0] 8 #() 2448 nil 27 722 754 8 'Class' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 18672 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 18688 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 18112 34 2 8 1140850688 131073 18784 nil nil nil 5 nil nil nil 18784 nil 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 9 2130 435 1 18784 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 221 0 0 0 4 0 0 0] 8 #() 2448 nil 27 18400 170 192 34 4 18784 8 'classTab' 18400 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 18112 34 2 8 1140916738 1 19088 1106 138 144 34 2 18272 18656 nil 1184 nil nil 1 nil nil nil 19088 nil 8 4294902785 802 0 nil 11762 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 19216 7 257 nil 802 0 nil 11762 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 19264 7 257 nil 1456 nil nil nil nil nil #smallIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 451 51 19088 2226 #basicSelectionsByIndex: 34 1 8 #(1) 19088 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 19088 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 25 0 0 0] 8 #() 2448 nil 27 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 489 2130 451 51 18112 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 225 0 0 0 13 1 0 0] 34 3 18400 18784 19088 2448 nil 27 16834 16880 1 16896 1 16928 -49 17296 51 410 ##(Smalltalk.CardContainer) 34 16 nil 18000 34 2 8 1409286144 131073 19728 nil 610 640 nil 5 nil nil nil 19728 658 138 144 34 2 722 754 8 'Classes' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 19888 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 19904 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 19728 34 2 8 1409372233 1025 20000 1106 138 144 1152 nil 1184 610 8 4278190080 nil 29 nil nil nil 20000 nil 8 4294902685 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 2 1506 8 'Column 1' 359 #left 802 0 nil 834 5 1 800 8 'doIt' 8 '[:each | (each subStrings: Character space) first]' 8 #[33 105 17 29 159 178 161 106] ##(Smalltalk.Character) #space #subStrings: #first 20240 7 257 nil ##(Smalltalk.SortedCollection) nil nil 20000 nil 3 nil nil 1506 8 '' 69 #left 802 0 nil 834 6 1 800 8 'doIt' 8 '[:each | | index |
	index := each indexOf: Character space.
	0 < index ifTrue: [each copyFrom: index + 2 to: each size - 1] ifFalse: ['''']]' 8 #[34 105 17 29 159 178 90 62 18 128 221 9 233 1 64 126 17 145 99 193 106 33 106] ##(Smalltalk.Character) #space #indexOf: #copyFrom:to: 20336 20352 7 65793 nil 1394 #<= 8 #() nil nil 20000 nil 1 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 9 55 2130 435 433 20000 2226 #text: 34 1 8 'Column 1' 20000 2226 #columnOrder: 34 1 8 #(2 1) 20000 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 221 0 0 0 243 0 0 0] 8 #() 2448 nil 27 722 754 8 'Hierarchy' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 20784 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 20800 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 19728 34 2 8 1409352231 1025 20896 590918 3 ##(Smalltalk.TreeModel) nil 1184 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 1152 610 8 4278190080 nil 29 nil nil nil 20896 nil 8 4294902927 802 0 nil 834 2 1 1392 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 21088 7 257 nil ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil 170 176 1152 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 1152 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 55 2130 435 433 20896 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 221 0 0 0 243 0 0 0] 8 #() 2448 nil 27 20000 170 192 34 4 20000 8 'classList' 20896 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 19728 34 2 8 1140916736 1 21424 1106 138 144 34 2 19872 20768 nil 1184 nil nil 1 nil nil nil 21424 nil 8 4294902785 802 0 nil 11762 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 21552 7 257 nil 802 0 nil 11762 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 21600 7 257 nil 1456 nil nil nil nil nil #smallIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 451 495 21424 2226 #basicSelectionsByIndex: 34 1 8 #(1) 21424 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 21424 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 247 0 0 0] 8 #() 2448 nil 27 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 451 495 19728 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 247 0 0 0] 34 3 20000 20896 21424 2448 nil 27 16834 16880 1 16896 1 16912 1 16928 -43 170 192 34 4 18112 8 'instanceClassTabs' 19728 8 'classHierarchyTabs' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 615 1 2130 451 539 18000 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 51 1 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 2 19728 18112 2448 nil 27 13 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 17888 34 2 8 1409286144 131073 22288 nil 610 8 4278190080 nil 517 nil nil nil 22288 658 138 144 34 2 722 754 8 'Dictionaries' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Dictionaries'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 22464 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 22480 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 22288 34 2 8 1140850688 131073 22608 nil 610 22368 nil 5 nil nil nil 22608 22384 170 192 34 4 22576 8 'dictionaryList' 410 ##(Smalltalk.ListView) 34 30 nil 22608 34 2 8 1409405001 1025 22736 1106 138 144 1152 nil 1184 610 20112 nil 5 nil nil nil 22736 nil 8 4294902685 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 305 #left 1394 #displayString 22880 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 22976 7 257 nil nil 22736 nil 3 nil 802 0 nil 834 6 1 800 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[34 105 226 0 142 221 9 17 226 1 100 160 97 161 180 106 60 106] #item #font #beBold #beItalic #font: 23056 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 37 2130 313 487 22736 2226 #text: 34 1 8 'Column 1' 22736 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 156 0 0 0 5 1 0 0] 8 #() 2448 nil 27 8 'packageList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 22288 34 2 8 1140916864 1 23392 1106 138 144 34 2 754 8 'Rowan Packages' 802 0 nil 834 7 1 752 8 'doIt' 8 '(CardLabel text: ''Rowan Packages'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[45 30 35 113 47 32 180 106 195 105] 6786 #CardLabel 752 8 'Rowan Packages' 6786 #Icon ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 23536 11 1 nil nil 22448 nil 1184 610 22368 nil 1 nil nil nil 23392 nil 8 4294902785 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil #smallIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 373 539 23392 2226 #basicSelectionsByIndex: 34 1 8 #(1) 23392 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 23392 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 186 0 0 0 13 1 0 0] 8 #() 2448 nil 27 nil 22288 2130 33 33 14546 22608 nil 14578 2130 277 3 2130 309 35 1 1314 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 53 9 2130 313 523 22608 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 182 0 0 0 9 1 0 0] 34 2 22736 22576 2448 nil 27 34 2 8 1409372233 1025 22576 1106 138 144 1152 nil 1184 610 20112 nil 21 nil nil nil 22576 nil 8 4294902685 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 305 #left 1394 #displayString 24336 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 24432 7 257 nil nil 22576 nil 3 nil 802 0 nil 834 5 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] #item #font #beBold #font: 24512 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 37 2130 313 487 22576 2226 #text: 34 1 8 'Column 1' 22576 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 156 0 0 0 5 1 0 0] 8 #() 2448 nil 27 722 23504 22736 22736 170 192 34 2 22608 8 'packageDictionaryTabs' nil 23392 22608 2130 201 201 401 1 31 nil nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 373 539 22288 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 186 0 0 0 13 1 0 0] 34 2 22608 23392 2448 nil 27 11 410 ##(Smalltalk.ContainerView) 34 15 nil 17888 34 2 8 1140850688 131073 25072 nil nil nil 5 nil nil nil 25072 482 170 176 1152 false 170 192 34 2 410 ##(Smalltalk.TreeView) 34 27 nil 25072 34 2 8 1409352231 1025 25200 20962 nil 1184 20994 nil nil nil 170 192 1152 610 8 4278190080 nil 21 nil nil nil 25200 nil 8 4294902927 802 0 nil 834 5 1 800 8 'doIt' 8 '[:each | each key isEmpty ifTrue: [''--All Categories--''] ifFalse: [each key last]].' 8 #[33 105 226 0 159 119 31 106 226 0 161 106] #key #isEmpty 8 '--All Categories--' #last 25360 7 257 nil ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil 170 176 1152 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 1152 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 223 539 25200 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 111 0 0 0 13 1 0 0] 8 #() 2448 nil 27 8 'classCategoryList' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 383 1 2130 223 539 25072 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 191 0 0 0 0 0 0 0 46 1 0 0 13 1 0 0] 34 1 25200 2448 nil 27 7 false 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 1065 539 17888 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 5 22288 410 ##(Smalltalk.Splitter) 34 12 nil 17888 34 2 8 1140850688 1 26000 nil 610 15680 nil 517 nil nil nil 26000 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 373 1 2130 11 539 26000 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 186 0 0 0 0 0 0 0 191 0 0 0 13 1 0 0] 8 #() 2448 nil 27 25072 410 ##(Smalltalk.Splitter) 34 12 nil 17888 34 2 8 1140850688 1 26240 nil 610 15680 nil 517 nil nil nil 26240 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 605 1 2130 11 539 26240 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 46 1 0 0 0 0 0 0 51 1 0 0 13 1 0 0] 8 #() 2448 nil 27 18000 2448 nil 27 7 false 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 1785 539 12288 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 17888 410 ##(Smalltalk.Splitter) 34 12 nil 12288 34 2 8 1140850688 1 26656 nil 610 15680 nil 517 nil nil nil 26656 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1065 1 2130 11 539 26656 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 2 0 0 0 0 0 0 25 2 0 0 13 1 0 0] 8 #() 2448 nil 27 12400 2448 nil 27 12258 3 5 true 170 192 34 4 544 8 'textAreaTabs' 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 26960 nil 610 15680 nil 517 nil nil nil 26960 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 539 2130 1785 11 26960 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 13 1 0 0 124 3 0 0 18 1 0 0] 8 #() 2448 nil 27 8 'splitter' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 7679 21 2130 1785 1089 416 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 123 18 0 0 42 2 0 0] 34 3 12288 26960 544 2448 nil 27 )! !
!RowanSystemBrowserPresenter class categoriesFor: #resource_Default_view!private!resources-views! !

