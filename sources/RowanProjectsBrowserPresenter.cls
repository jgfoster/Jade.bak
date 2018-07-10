"Filed out from Dolphin Smalltalk 7"!

RowanSystemBrowserPresenter subclass: #RowanProjectsBrowserPresenter
	instanceVariableNames: 'projectListPresenter stonCritical projectInfoTab projectLabel projectDictionaryTabs'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectsBrowserPresenter guid: (GUID fromString: '{258266f3-948d-47d3-afa6-c6d0c941ddc5}')!
RowanProjectsBrowserPresenter comment: 'In packages pane, show projects & packages. Remove class categories.'!
!RowanProjectsBrowserPresenter categoriesForClass!Unclassified! !
!RowanProjectsBrowserPresenter methodsFor!

addPackageDictionaryInfoTo: aStream

	| tabName |
	self rowanFixMe. "remove when string update is completely gone"
	tabName := projectDictionaryTabs currentCard name.
	aStream nextPutAll: tabName; lf.
	tabName = 'projectList' ifTrue: [
		projectListPresenter selections do: [:each | aStream nextPutAll: each name; tab].
	] ifFalse: [
		dictionaryListPresenter selections do: [:each | aStream nextPutAll: each key; tab].
	].
	aStream lf.
!

createComponents
	super createComponents.
	projectListPresenter := self add: ListPresenter new name: 'projectList'.
	packageListPresenter := self add: ListPresenter new name: 'packagesList'!

createSchematicWiring

	super createSchematicWiring.
	self createSchematicWiringForProjectList;
		createSchematicWiringForProjectDictionaryList.
!

createSchematicWiringForPackageDictionaryTabs

	"Yes, I know. Subclasses should not remove behavior."!

createSchematicWiringForPackageList

	packageListPresenter
		when: #'drag:'				send: #'onDragPackageList:'		to: self;
		when: #'dragCut:'			send: #'onDragCutPackageList:'		to: self;
		when: #'dragOver:'			send: #'onDragOverPackageList:'	to: self;
		when: #'drop:'				send: #'onDropOnPackageList:'		to: self;
		when: #'keyTyped:'			send: #'delayUpdate'				to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'			to: self;
		"when: #'selectionChanged' 		send: #'list:' 					to: globalsPresenter 	with: #();"
		when: #'selectionChanged'		send: #'updatePackage' 			to: self;
		"when: #'selectionChanged'		send: #'updateAndSelect:'			to: self			with: packageInfoTab;"
		yourself.
!

createSchematicWiringForProjectDictionaryList
	projectDictionaryTabs
		when: #currentCardChanging:
			send: #aboutToChange:
			to: self;
		when: #currentCardChanged
			send: #projectDictionaryChanged
			to: self;
		yourself!

createSchematicWiringForProjectList

	projectListPresenter 
		when: #'drag:'				send: #'onDragPackageList:'		to: self;
		when: #'dragCut:'			send: #'onDragCutPackageList:'		to: self;
		when: #'dragOver:'			send: #'onDragOverPackageList:'	to: self;
		when: #'drop:'				send: #'onDropOnPackageList:'		to: self;
		when: #'keyTyped:'			send: #'delayUpdate'				to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'			to: self;
		when: #'selectionChanged' 		send: #'ensureVisible'				to: projectListPresenter;
		when: #'selectionChanged'		send: #'updateProject' 			to: self
!

createStateEntry

	^projectDictionaryTabs currentCard == projectListPresenter view!

getViews
	classHierarchyTabs := self view viewNamed: 'classHierarchyTabs'.
	instanceClassTabs := self view viewNamed: 'instanceClassTabs'.
	methodFilterTabs := self view viewNamed: 'methodFilterTabs'.
	textAreaTabs := self view viewNamed: 'textAreaTabs'.
	packageInfoTab := self view viewNamed: 'packageInfo'.
	projectDictionaryTabs := self view viewNamed: 'projectDictionaryTabs'.
	projectInfoTab := self view viewNamed: 'projectInfo'!

initialize

	super initialize.
	stonCritical := Semaphore forMutualExclusion.!

isDictionariesTabSelected

	^projectDictionaryTabs currentCard name = 'dictionaryList'.
!

isProjectListTabSelected

	^projectDictionaryTabs currentCard name = 'projectList'.
!

onViewOpened
	super onViewOpened.
	projectListPresenter view contextMenu: (Menu fromStrings: self pkgsMenuStrings).
!

projectDictionaryChanged
	| listModel |
	listModel := (textAreaTabs subViews detect: [:each | each class == TabViewXP]) model.
	globalsLabel ifNil: [globalsLabel := listModel detect: [:each | each text = 'Globals']].
	projectLabel ifNil: [projectLabel := listModel detect: [:each | each text = 'Project']].
	packageLabel ifNil: [packageLabel := listModel detect: [:each | each text = 'Package']].
	self isProjectListTabSelected
		ifTrue: 
			[(listModel includes: projectLabel) ifFalse: [listModel addFirst: projectLabel].
			self updateProjectInfo.
			projectInfoTab ensureVisible.
			listModel remove: globalsLabel ifAbsent: [].
			listModel remove: packageLabel ifAbsent: []]
		ifFalse: 
			[(listModel includes: globalsLabel) ifFalse: [listModel addFirst: globalsLabel].
			globalsPresenter ensureVisible.
			listModel remove: packageLabel ifAbsent: [].
			listModel remove: projectLabel ifAbsent: [].
			self updateAndSelect: globalsPresenter]!

registerPresentersForUpdates
	super registerPresentersForUpdates.
	BrowserUpdate current register: projectListPresenter with: #projectsUpdate:.
	BrowserUpdate current register: packageListPresenter with: #projectPackagesUpdate:.
	BrowserUpdate current register: classListPresenter with: #classesUpdate:.
!

textTabChanged
	methodSourcePresenter view cancelCallTip.
	(textAreaTabs currentCard name = 'projectInfo' and: [self isProjectListTabSelected])
		ifTrue: [projectInfoTab ensureVisible].
	self updateMenuBar.

!

updateAndSelectA: aView

	inUpdate ifTrue: [^self].
	"self updateCommand: self requestStringForUpdate."
	self updateMenuBar.
	aView notNil ifTrue: [aView ensureVisible].
!

updateClasses
	"assume that we are already under stonCritical semaphore protection"

	| browserUpdate stonString stonResults |
	browserUpdate := BrowserUpdate current initializeUpdates.
	classListPresenter clear.
	stonString := STON toString: packageListPresenter selections.
	stonResults := self gciSession serverPerform: #updateFromSton: with: stonString.
	browserUpdate updates: (STON fromString: stonResults)!

updateCommand: aString onSuccessDo: aBlock
	"Only use STON to update in our new browser"
	stonCritical critical: 
			[
			[| stonResults update |
			update := BrowserUpdate current initializeUpdates.
			stonResults := self gciSession serverPerform: #systemBrowserSTON:
						with: environment printString , ' ' , aString.
			update updates: (STON fromString: stonResults).
			self updateSelectionsFor: update.
			self updateTabLabel.
			update updates: #()]
					ensure: [selectedClassChanged := false]].
	^nil!

updatePackage
	self updateClasses.
	self updatePackageInfo.!

updatePackageInfo!

updatePackages
	"assume that we are already under stonCritical semaphore protection"

	| browserUpdate stonString stonResults |
	browserUpdate := BrowserUpdate current initializeUpdates.
	packageListPresenter clear. 
	stonString := STON toString: projectListPresenter selections.
	stonResults := self gciSession serverPerform: #updateFromSton: with: stonString.
	browserUpdate updates: (STON fromString: stonResults).
!

updateProject
	self updatePackages.
	self updateProjectInfo!

updateProjectInfo
	| services array |
	services := projectListPresenter selections.
	services isEmpty ifTrue: [^projectInfoTab list: OrderedCollection new].
	array := Array writeStream: 4.
	services do: 
			[:service |
			array nextPut: (Array with: service with: 'name').
			array nextPut: (Array with: service with: 'sha').
			array nextPut: (Array with: service with: 'branch').
			array nextPut: (Array with: service with: 'isSkew').
			service == services last ifFalse: [array nextPut: (Array with: RowanProjectService new with: nil)]].
	projectInfoTab list: array contents.
	projectInfoTab ensureVisible!

updateSelectionsForUpdate: update
	update updatePackageSelectionsFor: packageListPresenter.
	update updateDictionarySelectionsFor: dictionaryListPresenter.
	update updateClassCategorySelectionsFor: classCategoryPresenter.
	update updateClassSelectionFor: classListPresenter.
	update updateClassHierarchySelectionFor: classHierarchyPresenter.
	update updateSuperclassSelectionFor: superclassListPresenter.
	update updateMethodFilterSelectionFor: self methodFilterListPresenter.
	update updateMethodSelectionFor: self methodListPresenter.
	update projectSelectionUpdate: projectListPresenter!

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
		tabName := projectDictionaryTabs currentCard name.
		tabName = 'projectList' ifTrue: [
			newLabel := 'Projects'.
			selections := projectListPresenter selections.
		] ifFalse: [
			newLabel := 'Dictionaries'.
			selections := dictionaryListPresenter selections.
		].
		selections size = 1 ifTrue: [newLabel := selections first key].
	].
	self view arrangement: cardNumber printString , ' ' , newLabel.
	self parentPresenter view updateTabs.
! !
!RowanProjectsBrowserPresenter categoriesFor: #addPackageDictionaryInfoTo:!public!request string! !
!RowanProjectsBrowserPresenter categoriesFor: #createComponents!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiring!create schemantic wiring!overrides!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForPackageDictionaryTabs!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForPackageList!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForProjectDictionaryList!create schemantic wiring!overrides!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForProjectList!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createStateEntry!accessing!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #getViews!public! !
!RowanProjectsBrowserPresenter categoriesFor: #initialize!initialization!public! !
!RowanProjectsBrowserPresenter categoriesFor: #isDictionariesTabSelected!public!testing! !
!RowanProjectsBrowserPresenter categoriesFor: #isProjectListTabSelected!public!testing! !
!RowanProjectsBrowserPresenter categoriesFor: #onViewOpened!menus!overrides!public! !
!RowanProjectsBrowserPresenter categoriesFor: #projectDictionaryChanged!event handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #registerPresentersForUpdates!event handlers!initialization!public!rowan! !
!RowanProjectsBrowserPresenter categoriesFor: #textTabChanged!event handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #updateAndSelectA:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClasses!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateCommand:onSuccessDo:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackage!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackageInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackages!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProject!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProjectInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateSelectionsForUpdate:!public!rowan! !
!RowanProjectsBrowserPresenter categoriesFor: #updateTabLabel!public!updating! !

!RowanProjectsBrowserPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 544 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 544 655878 ##(Smalltalk.CardLayout) 138 144 34 8 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Package' 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 7 1 752 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 784 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 816 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 928 nil nil nil 5 nil nil nil 928 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 928 34 2 8 1409355853 1025 1040 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 610 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 1040 nil 8 4294908349 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Ancestor' 131 #left 1394 #displayString 8 #() 1394 #<= 1568 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 1600 7 257 nil nil 1040 nil 1 nil nil 1506 8 'Name' 401 #left 1394 #displayString 1424 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 1728 7 257 nil nil 1040 nil 1 nil nil 1506 8 'Timestamp' 301 #left 1394 #displayString 8 #() 1394 #<= 1856 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 1888 7 257 nil nil 1040 nil 1 nil nil 1506 8 'Message' 941 #left 1394 #displayString 1856 1394 #<= 1856 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 2032 7 257 nil nil 1040 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 328198 ##(Smalltalk.Point) 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 2130 1 1 2130 1769 263 1040 2226 #contextMenu: 34 1 1248 1040 2226 #text: 34 1 8 'Ancestor' 1040 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 2130 193 193 nil 27 23 410 ##(Smalltalk.ListView) 34 30 nil 928 34 2 8 1409355853 1025 2464 1106 138 144 1152 nil 1184 610 1216 nil 5 1234 nil true 34 5 1282 1 1314 #addRepository 8 'Add &Repository' 1 1 nil nil nil 1282 1 1314 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 1282 1 1314 #savePackage 8 '&Save Package' 1 1 nil nil nil 1282 1 1314 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 2464 nil 8 4294908349 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 4 1506 8 'Repository Type' 211 #left 1394 #displayString 2880 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 2976 7 257 nil nil 2464 nil 1 nil nil 1506 8 'Description' 1161 #left 1394 #displayString 1856 1394 #<= 1856 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 3120 7 257 nil nil 2464 nil 3 nil nil 1506 8 'User' 201 #left 1394 #displayString 1568 1394 #<= 1568 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 3264 7 257 nil nil 2464 nil 1 nil nil 1506 8 'Password' 201 #left 1394 #displayString 1568 1394 #<= 1568 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 3408 802 0 nil 3424 19 257 nil 7 257 nil nil 2464 nil 1 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 263 2130 1769 217 2464 2226 #contextMenu: 34 1 2576 2464 2226 #text: 34 1 8 'Repository Type' 2464 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 2448 nil 27 19 true 170 192 34 4 1040 8 'ancestorList' 2464 8 'repositoryList' nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 928 2226 #text: 34 1 8 'Package' 928 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 34 2 1040 2464 2448 nil 27 722 754 8 'Globals' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 4080 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 4096 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 544 34 2 8 1409355853 1025 4192 1106 138 144 1152 nil 1184 610 1216 nil 5 nil nil nil 4192 nil 8 4294908349 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 3 1506 8 'Name' 587 #left 1394 #displayString 4336 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 4432 7 257 nil nil 4192 nil 3 nil nil 1506 8 'Class' 589 #left 1394 #displayString 1856 1394 #<= 1856 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 4576 7 257 nil nil 4192 nil 3 nil nil 1506 8 'Value' 587 #left 1394 #displayString 1856 1394 #<= 1856 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 4720 7 257 nil nil 4192 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 4192 2226 #text: 34 1 8 'Name' 4192 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2448 nil 27 722 754 8 'Original Source' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 5072 ##(Smalltalk.Icon) 8 'COMPILEDMETHOD_PRIVATE.ico' #fromId: #text:iconBlock: 5088 11 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 5184 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 610 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2130 193 193 nil 5184 nil 8 4294904157 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5522 77 786694 ##(Smalltalk.IndexedColor) 33554457 5570 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 5536 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 5552 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 1152 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 5184 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5570 33554447 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 5536 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 5184 65025 3 false 1 nil nil nil nil nil nil 3 5986 3 5184 33423361 5 false 3 nil nil nil nil nil nil 5 5986 5 5184 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 5522 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 5522 1027 6098 60239 6098 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5522 1031 6098 335 6098 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 7 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 5184 2226 #modificationEventMask: 8 #(4607) 5184 2226 #caretPeriod: 8 #(530) 5184 2226 #wordWrap: 8 #(true) 5184 2226 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 5184 61 3 nil nil 5 nil 6482 3 5184 1 nil 3 67108863 5 nil 6482 5 5184 1 nil 3 -67108863 5 nil 5184 2226 #tabWidth: 8 #(4) 5184 2226 #maxCompletionListHeight: 8 #(9) 5184 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2448 nil 29 722 754 8 'SUnit' 802 0 nil 834 4 1 800 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 6704 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 544 34 2 8 1140850688 131073 6832 nil 610 8 4278190080 nil 5 nil nil nil 6832 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 6832 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 1152 2448 nil 27 722 754 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 7152 5250 nil false 5296 nil 610 8 4278190080 nil 5 1234 nil true 34 11 1282 1 1314 #editSave 8 '&Save' 9383 1 nil nil nil 2706 4097 1282 1 1314 #editUndo 8 '&Undo' 9397 1 nil nil nil 1282 1 1314 #editRedo 8 '&Redo' 9395 1 nil nil nil 2706 4097 1282 1 1314 #editCut 8 'Cu&t' 9393 1 nil nil nil 1282 1 1314 #editCopy 8 '&Copy' 9351 1 nil nil nil 1282 1 1314 #editPaste 8 '&Paste' 9389 1 nil nil nil 1282 1 1314 #editDelete 8 '&Delete' 1629 1 nil nil nil 2706 4097 1282 1 1314 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 5346 nil true 5378 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2130 193 193 nil 7152 nil 8 4294904157 5458 nil nil 13 nil 170 192 34 4 #normal 5522 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5522 77 5584 5600 1 nil nil nil nil #callTip nil nil nil 34 256 7872 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 7888 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 1152 170 192 34 2 #default 5714 1 nil nil nil 7152 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5776 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 7872 nil nil nil nil 1 nil 170 192 34 6 1 5986 1 7152 65025 3 false 1 nil nil nil nil nil nil 3 5986 3 7152 33423361 5 false 3 nil nil nil nil nil nil 5 5986 5 7152 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 5522 1029 nil 6098 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 5522 1027 6098 60239 6098 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5522 1031 6098 335 6098 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 8 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 7152 2226 #contextMenu: 34 1 7264 7152 2226 #modificationEventMask: 8 #(4607) 7152 2226 #caretPeriod: 8 #(530) 7152 2226 #wordWrap: 8 #(true) 7152 2226 #margins: 34 1 34 3 6482 1 7152 1 3 nil nil 5 nil 6482 3 7152 33 nil 3 67108863 5 nil 6482 5 7152 1 nil 3 -67108863 5 nil 7152 2226 #tabWidth: 8 #(4) 7152 2226 #maxCompletionListHeight: 8 #(9) 7152 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2448 nil 29 722 754 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 8944 5250 nil false 5296 nil 610 7248 nil 5 1234 nil true 34 1 1282 1 1314 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 5346 nil true 5378 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2130 193 193 nil 8944 nil 8 4294904157 5458 nil nil 11 nil 170 192 34 4 #normal 5522 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5522 77 5584 5600 1 nil nil nil nil #callTip nil nil nil 34 256 9264 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 9280 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 5714 1 nil nil nil 8944 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5776 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 9264 nil nil nil nil 1 nil 170 192 34 6 1 5986 1 8944 65025 3 false 1 nil nil nil nil nil nil 3 5986 3 8944 33423361 5 false 3 nil nil nil nil nil nil 5 5986 5 8944 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 5522 1029 nil 6098 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 5522 1027 6098 60239 6098 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5522 1031 6098 335 6098 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 8 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 8944 2226 #contextMenu: 34 1 9040 8944 2226 #modificationEventMask: 8 #(4607) 8944 2226 #caretPeriod: 8 #(530) 8944 2226 #wordWrap: 8 #(true) 8944 2226 #margins: 34 1 34 3 6482 1 8944 1 3 nil nil 5 nil 6482 3 8944 33 nil 3 67108863 5 nil 6482 5 8944 1 nil 3 -67108863 5 nil 8944 2226 #tabWidth: 8 #(4) 8944 2226 #maxCompletionListHeight: 8 #(9) 8944 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2448 nil 29 722 754 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 10240 5250 nil false 5296 nil 610 8 4278190080 nil 5 nil 5346 nil true 5378 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2130 193 193 nil 10240 nil 8 4294904157 5458 nil nil 13 nil 170 192 34 4 #normal 5522 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5522 77 5584 5600 1 nil nil nil nil #callTip nil nil nil 34 256 10480 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 10496 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 5634 1 nil nil false 138 144 1152 170 192 34 2 #default 5714 1 nil nil nil 10240 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5776 nil nil 65 nil nil 8048 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 10480 #callTip 10496 nil nil nil nil 1 nil 170 192 34 12 1 5986 1 10240 65025 3 false 1 nil nil nil nil nil nil 3 5986 3 10240 33423361 5 false 3 nil nil nil nil nil nil 5 5986 5 10240 511 1 false 5 nil nil nil nil nil nil 8 'indicator9' 5986 19 10240 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator8' 5986 17 10240 33554447 1 nil nil nil nil nil nil nil nil 8 'indicator10' 5986 21 10240 33554459 3 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 5522 1029 nil 6098 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 5522 1027 6098 60239 6098 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5522 1031 6098 335 6098 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 7 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 10240 2226 #modificationEventMask: 8 #(4607) 10240 2226 #hoverTime: 8 #(200) 10240 2226 #caretPeriod: 8 #(530) 10240 2226 #margins: 34 1 34 3 6482 1 10240 61 3 nil nil 5 nil 6482 3 10240 1 nil 3 67108863 nil nil 6482 5 10240 1 nil 3 -67108863 nil nil 10240 2226 #tabWidth: 8 #(4) 10240 2226 #maxCompletionListHeight: 8 #(9) 10240 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2448 nil 29 722 754 8 'Project' nil nil 410 ##(Smalltalk.ListView) 34 30 nil 544 34 2 8 1409372237 1025 11520 1106 138 144 1152 nil 1184 610 1216 nil 5 nil nil nil 11520 nil 8 4294908349 1394 #displayString 4336 nil 1456 nil nil nil nil nil nil 138 144 34 2 1506 8 'Name' 201 #left 1394 #displayString 4336 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | (each at: 2)]' 8 #[29 105 17 64 148 106] 11744 7 257 nil nil 11520 nil 1 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each item at: 1) displayStringFor: each]' 8 #[31 105 226 0 63 148 17 177 106] #item #displayStringFor: 11824 7 257 nil 1506 8 'Value' 1561 #left 1394 #displayString 8 #() 1394 #<= 11952 802 0 nil 834 4 1 800 8 'doIt' 8 '[:each | (each at: 2) ifNil:[String new] ifNotNil: [:iv |
(each at: 1) instVarNamed: iv]]' 8 #[32 105 17 64 148 100 223 4 97 45 146 106 90 17 63 148 18 178 106] 6786 #String 16 #ifNil:ifNotNil: #instVarNamed: 11984 7 65793 nil nil 11520 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 9 55 2130 1769 479 11520 2226 #text: 34 1 8 'Name' 11520 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2448 nil 27 11520 170 192 34 16 928 8 'packageInfo' 6832 8 'sunit' 8944 8 'classDocumentation' 10240 8 'methodSource' 11520 8 'projectInfo' 5184 8 'originalSource' 4192 8 'globals' 7152 8 'classDefinition' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 544 34 2 8 1140916736 1 12480 1106 138 144 34 8 11488 768 4064 7120 8912 5056 10208 6672 nil 1184 nil nil 1 nil nil nil 12480 nil 8 4294908629 802 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 12608 7 257 nil 802 0 nil 12626 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 12672 7 257 nil 1456 nil nil nil nil nil #smallIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 1785 541 12480 2226 #basicSelectionsByIndex: 34 1 8 #(1) 12480 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 12480 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 14 1 0 0] 8 #() 2448 nil 27 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 549 2130 1785 541 544 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 1 0 0 124 3 0 0 32 2 0 0] 34 9 11520 928 4192 7152 8944 5184 10240 6832 12480 2448 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 13152 nil nil nil 5 nil nil nil 13152 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 13152 34 2 8 1140850688 131073 13264 nil nil nil 5 nil nil nil 13264 482 170 176 1152 false 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1075 1 2130 711 539 13264 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 2 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 13264 34 2 8 1409286144 131073 13536 nil 610 8 4278190080 nil 517 nil nil nil 13536 658 138 144 34 3 722 8 'Categories' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 13536 34 2 8 1140850688 131073 13744 nil 610 13616 nil 5 nil nil nil 13744 13632 170 192 34 6 410 ##(Smalltalk.ListView) 34 30 nil 13744 34 2 8 1409372233 1025 13856 1106 138 144 1152 nil 1184 610 8 4278190080 nil 21 nil nil nil 13856 nil 8 4294908349 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 283 #left 1394 #displayString 14016 ##(Smalltalk.SortedCollection) nil nil 13856 nil 3 nil 802 0 nil 834 9 1 800 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 14112 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 37 2130 291 487 13856 2226 #text: 34 1 8 'Column 1' 13856 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2448 nil 27 8 'variableList' 13712 8 'categoryList' 410 ##(Smalltalk.ListView) 34 30 nil 13744 34 2 8 1409372233 1025 14464 1106 138 144 1152 nil 1184 610 13968 nil 21 nil nil nil 14464 nil 8 4294908349 1394 #displayString 14016 nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 283 #left 1394 #displayString 14016 ##(Smalltalk.SortedCollection) nil nil 14464 nil 3 nil 802 0 nil 834 9 1 800 14144 14160 14176 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 14688 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 37 2130 291 487 14464 2226 #text: 34 1 8 'Column 1' 14464 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2448 nil 27 8 'pragmaList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 13536 34 2 8 1140916864 1 14976 1106 138 144 34 3 13696 8 'Variables' 8 'Pragmas' nil 1184 610 13616 nil 1 nil nil nil 14976 nil 8 4294908629 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil #noIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 351 539 14976 2226 #basicSelectionsByIndex: 34 1 8 #(1) 14976 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 14976 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 8 #() 2448 nil 27 nil 13536 2130 33 33 1049862 ##(Smalltalk.ButtonInteractor) 13744 nil 590342 ##(Smalltalk.Rectangle) 2130 255 3 2130 287 35 1 1314 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 53 9 2130 291 523 13744 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 171 0 0 0 9 1 0 0] 34 3 13712 13856 14464 2448 nil 27 34 2 8 1409372233 1025 13712 1106 138 144 1152 nil 1184 610 13968 nil 21 nil nil nil 13712 nil 8 4294908349 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 283 #left 1394 #displayString 15808 ##(Smalltalk.SortedCollection) nil nil 13712 nil 3 nil 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each ]' 1779525917 15904 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 37 2130 291 487 13712 2226 #text: 34 1 8 'Column 1' 13712 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2448 nil 27 722 15088 13856 722 15104 14464 13712 170 192 34 2 13744 8 'methodFilterTabs' nil 14976 13744 2130 201 201 401 1 31 nil nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 351 539 13536 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 34 2 13744 14976 2448 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 13264 34 2 8 1140850688 1 16464 nil 610 8 4278190080 nil 517 nil nil nil 16464 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 351 1 2130 11 539 16464 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 0 0 0 0 180 0 0 0 13 1 0 0] 8 #() 2448 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 13264 34 2 8 1140850688 131073 16720 nil nil nil 5 nil nil nil 16720 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ComboBox) 34 17 nil 16720 34 2 8 1412498947 1025 16848 1106 138 144 1152 nil 1184 610 8 4278190080 nil 5 nil nil nil 16848 nil 8 4294907581 1394 #displayString 8 #() 1152 401 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 351 53 16848 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 26 0 0 0] 8 #() 2448 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 17218 #fixedParentRight 1 17218 #fixedParentTop 1 17218 #fixedViewTop 47 410 ##(Smalltalk.ListView) 34 30 nil 16720 34 2 8 1409372233 1025 17296 1106 138 144 1152 nil 1184 610 13968 nil 13 nil nil nil 17296 nil 8 4294908349 1394 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil nil 138 144 34 2 1506 8 'Column 1' 311 #left 1394 #displayString 17440 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 17536 7 257 nil nil 17296 nil 3 nil 802 0 nil 834 5 1 800 8 'doIt' 8 '[:each | (each item at: 4) ifTrue: [each font: each font beItalic]]' 8 #[33 105 226 0 214 4 148 123 17 226 1 160 179 106 60 106] #item #font #beItalic #font: 17616 7 257 nil 1506 8 '' 33 #center 2226 #empty 8 #() 16 1394 #<= 8 #() nil nil 17296 802 0 nil 834 6 1 800 8 'doIt' 8 '[:each | ((each at: 2) ifTrue: [RowanSystemBrowserPresenter overriddenIcon] ifFalse: [Icon blank]) 
	imageIndex]' 8 #[34 105 17 64 148 120 29 159 111 31 161 162 106] ##(Smalltalk.RowanSystemBrowserPresenter) #overriddenIcon ##(Smalltalk.Icon) #blank #imageIndex 17792 7 257 nil 1 nil nil #report 1152 nil 131173 nil 8 #(nil) 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 47 2130 351 447 17296 2226 #text: 34 1 8 'Column 1' 17296 2226 #columnOrder: 34 1 8 #(2 1) 17296 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 175 0 0 0 246 0 0 0] 8 #() 2448 nil 27 17186 17232 1 17248 1 17264 47 17218 #fixedParentBottom -45 410 ##(Smalltalk.ComboBox) 34 17 nil 16720 34 2 8 1412498947 1025 18176 1106 138 144 1152 nil 1184 610 16960 nil 5 nil nil nil 18176 nil 8 4294907581 1394 #displayString 8 #() 1152 401 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 493 2130 351 53 18176 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 246 0 0 0 175 0 0 0 16 1 0 0] 8 #() 2448 nil 27 17186 17232 1 17248 1 18160 -45 17280 47 170 192 34 6 16848 8 'superclassList' 17296 8 'methodList' 18176 8 'overrideList' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 361 1 2130 351 539 16720 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 99 1 0 0 13 1 0 0] 34 3 16848 17296 18176 2448 nil 27 2448 nil 27 5 410 ##(Smalltalk.ContainerView) 34 15 nil 13152 34 2 8 1140850688 131073 18752 nil nil nil 5 nil nil nil 18752 482 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 18752 34 2 8 1140850688 131073 18864 nil nil nil 5 nil nil nil 18864 16786 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 18864 34 2 8 1409286144 131073 18976 nil 610 640 nil 5 nil nil nil 18976 658 138 144 34 2 722 754 8 'Classes' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 19136 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 19152 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 18976 34 2 8 1409372233 1025 19248 1106 138 144 1152 nil 1184 610 8 4278190080 nil 29 nil nil nil 19248 nil 8 4294908349 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 '' 373 #left 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 19488 7 257 nil 1394 #<= 8 #() nil nil 19248 nil 3 802 0 nil 834 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 19600 7 257 nil nil #report 1152 nil 133217 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 55 2130 389 433 19248 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 198 0 0 0 243 0 0 0] 8 #() 2448 nil 27 722 754 8 'Hierarchy' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 19904 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 19920 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 18976 34 2 8 1409352231 1025 20016 590918 3 ##(Smalltalk.TreeModel) nil 1184 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 1152 610 8 4278190080 nil 29 nil nil nil 20016 nil 8 4294908199 802 0 nil 834 2 1 1392 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 20208 7 257 nil ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil 170 176 1152 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 1152 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 55 2130 389 433 20016 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 198 0 0 0 243 0 0 0] 8 #() 2448 nil 27 19248 170 192 34 4 19248 8 'classList' 20016 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 18976 34 2 8 1140916736 1 20544 1106 138 144 34 2 19120 19888 nil 1184 nil nil 1 nil nil nil 20544 nil 8 4294908629 802 0 nil 12626 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 20672 7 257 nil 802 0 nil 12626 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 20720 7 257 nil 1456 nil nil nil nil nil #smallIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 405 495 20544 2226 #basicSelectionsByIndex: 34 1 8 #(1) 20544 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 20544 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 247 0 0 0] 8 #() 2448 nil 27 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 405 495 18976 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 247 0 0 0] 34 3 19248 20016 20544 2448 nil 27 17186 17232 1 17248 1 17264 1 18160 -43 410 ##(Smalltalk.CardContainer) 34 16 nil 18864 34 2 8 1409286144 131073 21184 nil 610 8 4278190080 nil 5 nil nil nil 21184 658 138 144 34 2 722 754 8 'Instance' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 21360 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 21376 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 21184 34 2 8 1140850688 131073 21472 nil nil nil 5 nil nil nil 21472 nil 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 9 2130 389 1 21472 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 198 0 0 0 4 0 0 0] 8 #() 2448 nil 27 722 754 8 'Class' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 21744 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 21760 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 21184 34 2 8 1140850688 131073 21856 nil nil nil 5 nil nil nil 21856 nil 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 9 2130 389 1 21856 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 198 0 0 0 4 0 0 0] 8 #() 2448 nil 27 21472 170 192 34 4 21856 8 'classTab' 21472 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 21184 34 2 8 1140916738 1 22160 1106 138 144 34 2 21344 21728 nil 1184 nil nil 1 nil nil nil 22160 nil 8 4294908629 802 0 nil 12626 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 22288 7 257 nil 802 0 nil 12626 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 22336 7 257 nil 1456 nil nil nil nil nil #smallIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 405 51 22160 2226 #basicSelectionsByIndex: 34 1 8 #(1) 22160 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 22160 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 25 0 0 0] 8 #() 2448 nil 27 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 489 2130 405 51 21184 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 202 0 0 0 13 1 0 0] 34 3 21472 21856 22160 2448 nil 27 17186 17232 1 17248 1 18160 -49 17280 51 170 192 34 4 18976 8 'classHierarchyTabs' 21184 8 'instanceClassTabs' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 661 1 2130 405 539 18864 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 74 1 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 2 18976 21184 2448 nil 27 11 410 ##(Smalltalk.ContainerView) 34 15 nil 18752 34 2 8 1140850688 131073 23024 nil nil nil 5 nil nil nil 23024 482 170 176 1152 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 23024 34 2 8 1140936777 1025 23152 1106 138 144 1152 nil 1184 610 8 4278190080 nil 21 nil nil nil 23152 nil 8 4294908349 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each |  each].' 1779525917 23296 7 257 nil ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 305 #left 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 23424 7 257 nil 1394 #<= 8 #() nil nil 23152 nil 3 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 23536 7 257 nil #report 1152 nil 131185 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 1 2130 321 539 23152 2226 #text: 34 1 8 'Column 1' 23152 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 8 #() 2448 nil 27 8 'packagesList' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 331 1 2130 321 539 23024 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 69 1 0 0 13 1 0 0] 34 1 23152 2448 nil 27 9 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 18752 34 2 8 1409286144 131073 24032 nil 610 8 4278190080 nil 517 nil nil nil 24032 658 138 144 34 2 722 754 8 'Dictionaries' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Dictionaries'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 24208 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 24224 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 24032 34 2 8 1140850688 131073 24352 nil 610 24112 nil 5 nil nil nil 24352 24128 170 192 34 4 410 ##(Smalltalk.ListView) 34 30 nil 24352 34 2 8 1409405001 1025 24464 1106 138 144 1152 nil 1184 610 19360 nil 5 nil nil nil 24464 nil 8 4294908349 1394 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 297 #left 1394 #displayString 24608 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 24704 7 257 nil nil 24464 nil 3 nil 802 0 nil 834 3 1 800 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 24784 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 1 2130 305 395 24464 2226 #text: 34 1 8 'Column 1' 24464 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 152 0 0 0 197 0 0 0] 8 #() 2448 nil 27 8 'projectList' 24320 8 'dictionaryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 24032 34 2 8 1140916736 1 25136 1106 138 144 34 2 754 8 'Projects' 802 0 nil 834 4 1 800 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 6786 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 25280 7 1 nil nil 24192 nil 1184 610 24112 nil 1 nil nil nil 25136 nil 8 4294908629 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil #smallIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 321 539 25136 2226 #basicSelectionsByIndex: 34 1 8 #(1) 25136 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 25136 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 8 #() 2448 nil 27 nil 24032 2130 33 33 15410 24352 nil 15442 2130 269 397 2130 301 429 1 1314 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 101 2130 305 431 24352 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 50 0 0 0 156 0 0 0 9 1 0 0] 34 2 24464 24320 2448 nil 27 34 2 8 1409372233 1025 24320 1106 138 144 1152 nil 1184 610 19360 nil 21 nil nil nil 24320 nil 8 4294908349 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 297 #left 1394 #displayString 26048 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 26144 7 257 nil nil 24320 nil 3 nil 802 0 nil 834 5 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] #item #font #beBold #font: 26224 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 1 2130 305 395 24320 2226 #text: 34 1 8 'Column 1' 24320 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 152 0 0 0 197 0 0 0] 8 #() 2448 nil 27 722 25248 24464 24464 170 192 34 2 24352 8 'projectDictionaryTabs' nil 25136 24352 2130 201 201 401 1 29 nil nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 321 539 24032 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 34 2 24352 25136 2448 nil 27 9 false 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 1065 539 18752 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 5 24032 410 ##(Smalltalk.Splitter) 34 12 nil 18752 34 2 8 1140850688 1 26960 nil 610 16544 nil 517 nil nil nil 26960 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 321 1 2130 11 539 26960 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 0 0 0 0 165 0 0 0 13 1 0 0] 8 #() 2448 nil 27 23024 410 ##(Smalltalk.Splitter) 34 12 nil 18752 34 2 8 1140850688 1 27200 nil 610 16544 nil 517 nil nil nil 27200 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 651 1 2130 11 539 27200 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 69 1 0 0 0 0 0 0 74 1 0 0 13 1 0 0] 8 #() 2448 nil 27 18864 2448 nil 27 7 false 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 1785 539 13152 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 18752 410 ##(Smalltalk.Splitter) 34 12 nil 13152 34 2 8 1140850688 1 27616 nil 610 16544 nil 517 nil nil nil 27616 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1065 1 2130 11 539 27616 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 2 0 0 0 0 0 0 25 2 0 0 13 1 0 0] 8 #() 2448 nil 27 13264 2448 nil 27 13122 3 5 true 170 192 34 4 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 27904 nil 610 16544 nil 517 nil nil nil 27904 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 539 2130 1785 11 27904 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 13 1 0 0 124 3 0 0 18 1 0 0] 8 #() 2448 nil 27 8 'splitter' 544 8 'textAreaTabs' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 7679 21 2130 1785 1089 416 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 123 18 0 0 42 2 0 0] 34 3 13152 27904 544 2448 nil 27 )! !
!RowanProjectsBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

