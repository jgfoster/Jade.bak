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

checkout
	| service branchName |
	projectListPresenter hasSelection
		ifFalse: 
			[MessageBox notify: 'No project selected'.
			^false].
	service := (RowanProjectService on: gciSession) name: projectListPresenter selection name.
	branchName := ChoicePrompter choices: service branches caption: 'Select branch to checkout:'.
	branchName isNil ifTrue: [^false] ifFalse: [service checkout: branchName]!

cloneProject
	"Does a clone and 'attach'"
	| filePath |
	filePath := RowanKeyValuePrompter
				on: 'file:$ROWAN_PROJECTS_HOME/Rowan/samples/RowanSample1.ston' -> '$ROWAN_PROJECTS_HOME'
				prompt: 'Please select location of the code repository to clone:'
				caption: 'Clone Git Repository'.
	filePath isNil ifTrue: [^false].
	(RowanProjectService on: gciSession) 
		newGitProject: filePath key
		root: filePath value
		useSsh: true!

commit
	| projectName message service promptResult |
	projectListPresenter hasSelection
		ifFalse: 
			[MessageBox notify: 'No project selected'.
			^false].
	projectName := projectListPresenter selection name.
	message := String new asValue.
	promptResult := StsMultilinePrompter
				on: message
				prompt: 'Enter commit message. (This will do a GemStone commit after the git commit as well)'
				caption: 'Committing Project ' , projectName.
	promptResult isNil ifTrue: [^self].
	message value isEmpty
		ifTrue: [^MessageBox notify: 'Commit message must not be empty. Commit to local repository not done.'].
	service := (RowanProjectService on: gciSession) name: projectName.
	service commitWithMessage: message value asString.
	MessageBox notify: 'Project ' , projectName , ' committed to local repository!!'!

createComponents
	super createComponents.
	projectListPresenter := self add: ListPresenter new name: 'projectList'.
	packageListPresenter := self add: ListPresenter new name: 'packagesList'!

createSchematicWiring

	super createSchematicWiring.
	self createSchematicWiringForProjectList;
		createSchematicWiringForProjectDictionaryList.
!

createSchematicWiringForClassList
	classListPresenter				
		when: #'drag:'					send: #'onDragClassList:'				to: self;
		when: #'dragCut:'				send: #'onDragCutClassList:'			to: self;
		when: #'dragOver:'				send: #'onDragOverClassList:'			to: self;
		when: #'drop:'					send: #'onDropOnClassList:'			to: self;
		when: #'keyTyped:'				send: #'delayUpdate'					to: self;
		when: #'selectionChanging:'		send: #'aboutToChange:'				to: self;
		when: #'selectionChanged'			send: #'selectedClassChanged:'			to: self	with: true;
		when: #'selectionChanged'			send: #updateClass					to: self;

		when: #labelOf:changedTo: 		send: #labelOfClass:changedTo:			to: self;
		when: #aboutToEditLabel:accept: 	send: #aboutToEditClassLabel:accept:	to: self;
		when: #labelOf:editedTo:accept: 	send: #labelOfClass:editedTo:accept:		to: self
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

inspectProject

	| service |
	service := (RowanProjectService on: gciSession) name: projectListPresenter selection name.
	service inspectDefinition.!

isDictionariesTabSelected

	^projectDictionaryTabs currentCard name = 'dictionaryList'.
!

isProjectListTabSelected

	^projectDictionaryTabs currentCard name = 'projectList'.
!

loadProject
	| name |
	name := projectListPresenter selection
				ifNil: 
					[MessageBox notify: 'No project selected'.
					^false]
				ifNotNil: [:sel | sel name].
	(MessageBox confirm: 'Really load project ' , name , '? This will destroy any changes you may have.')
		ifFalse: [^false].
	(RowanProjectService on: gciSession) loadProjectNamed: name!

makeDefault
	(RowanProjectService on: gciSession) defaultProjectName: projectListPresenter selection name!

newService
	^(RowanProjectService on: gciSession) name: projectListPresenter selection name!

onViewOpened
	super onViewOpened.
	projectListPresenter view contextMenu: (Menu fromStrings: self projectMenuStrings).
!

projectChanges
	| service changesString patch |
	projectListPresenter hasSelection
		ifFalse: 
			[MessageBox notify: 'No project selected'.
			^false].
	service := (RowanProjectService on: gciSession) name: projectListPresenter selection name.
	patch := service changes.
	RowanPatchBrowser showOn: patch name: projectListPresenter selection name!

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

projectLog
	| service |
	projectListPresenter selection isNil
		ifTrue: 
			[MessageBox notify: 'No project selected'.
			^false].
	service := (RowanProjectService on: gciSession) name: projectListPresenter selection name.
	MessageBox notify: service log!

projectMenuStrings
	0 == 1
		ifTrue: 
			[self
				makeDefault;
				cloneProject;
				loadProject;
				unloadProject;
				pullFromGit;
				commit;
				pushToGit;
				projectChanges;
				writeProject;
				checkout;
				projectLog;
				inspectProject;
				refresh
				].
	^#(	'Projects' '&MakeDefault//makeDefault' 
		'-' 
		'Clone Git Project ...//cloneProject'
		'Load// loadProject'
		'Unload// unloadProject'
		'-'
		'Pull from Git ...// pullFromGit'
		'Commit ...//commit' '-' 
		'Push to Git ...// pushToGit'
		'-'
		'&Changes ...// projectChanges'
		'Write ...//writeProject' '-' 
		'Checkout ...// checkout'
		'-'
		'Log// projectLog'
		'&Inspect//inspectProject' '-' 
		'&Refresh ...// refresh'
)!

pullFromGit
	| service |
	projectListPresenter hasSelection
		ifFalse: 
			[MessageBox notify: 'No project selected'.
			^false].
	(MessageBox
		confirm: 'It is recommended that you commit changes before doing a pull from git. Proceed?')
			ifTrue: 
				[service := self newService.
				service pullFromGit]
			ifFalse: [^false]!

pushToGit
	| service |
	projectListPresenter hasSelection
		ifFalse: 
			[MessageBox notify: 'No project selected'.
			^false].
	(MessageBox
		confirm: 'This will push committed changes from the local repository to the remote server. Proceed?')
			ifTrue: 
				[service := self newService.
				service pushToGit]
			ifFalse: [^false]!

refresh
	MessageBox notify: 'This feature not yet implemented'!

registerPresentersForUpdates
	super registerPresentersForUpdates.
	BrowserUpdate current register: projectListPresenter with: #projectsUpdate:.
	BrowserUpdate current register: packageListPresenter with: #projectPackagesUpdate:.
	BrowserUpdate current register: classListPresenter with: #classesUpdate:.
!

showPackages
	| service |
	projectListPresenter selection isNil
		ifTrue: 
			[MessageBox notify: 'No project selected'.
			^false].
	service := (RowanProjectService on: gciSession) name: projectListPresenter selection name.
	MessageBox notify: service packageNameString!

textTabChanged
	methodSourcePresenter view cancelCallTip.
	(textAreaTabs currentCard name = 'projectInfo' and: [self isProjectListTabSelected])
		ifTrue: [projectInfoTab ensureVisible].
	self updateMenuBar.

!

unloadProject
	MessageBox notify: 'This feature not yet implemented'!

updateAndSelectA: aView

	inUpdate ifTrue: [^self].
	"self updateCommand: self requestStringForUpdate."
	self updateMenuBar.
	aView notNil ifTrue: [aView ensureVisible].
!

updateClass
	self updateMethods.
	self updateClassInfo.!

updateClasses
	"assume that we are already under stonCritical semaphore protection"

	| browserUpdate stonString stonResults |
	browserUpdate := BrowserUpdate current initializeUpdates.
	classListPresenter clear.
	stonString := STON toString: packageListPresenter selections.
	stonResults := self gciSession serverPerform: #updateFromSton: with: stonString.
	browserUpdate updates: (STON fromString: stonResults)!

updateClassInfo
	| service |
	service := classListPresenter selectionIfNone: [^self].
	classCommentPresenter value: service comment.
	classDefinitionPresenter view isModified
		ifTrue: 
			[service template = classDefinition ifTrue: [^self].
			(MessageBox confirm: 'Copy changes to clipboard?' caption: 'Class has unsaved changes!!')
				ifTrue: [classDefinitionPresenter value copyToClipboard]].
	classDefinitionPresenter value: service template.
	classDefinitionPresenter view
		backcolor: Color white;
		isModified: false.
	classDefinitionPresenter ensureVisible.
	categoryListPresenter list: service filters
	!

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

updateMethods!

updatePackage
	self updateClasses.
	self updatePackageInfo.!

updatePackageInfo!

updatePackages
	"assume that we are already under stonCritical semaphore protection"

	| browserUpdate stonString stonResults selections |
	browserUpdate := BrowserUpdate current initializeUpdates.
	packageListPresenter clear.
	selections := projectListPresenter selections isEmpty
				ifTrue: [Array with: RowanProjectService new]
				ifFalse: [projectListPresenter selections].
	stonString := STON toString: selections.
	stonResults := self gciSession serverPerform: #updateFromSton: with: stonString.
	browserUpdate updates: (STON fromString: stonResults)!

updateProject
	self updatePackages.
	self updateProjectInfo!

updateProjectInfo
	| services array projectTabLabel |
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
	projectInfoTab ensureVisible.
	"update the project tab"
	projectTabLabel := (projectListPresenter selectionIfNone: []) ifNil: ['Projects'] ifNotNil: [:sel | 'Projects - ', sel name]. 
	self view arrangement: projectTabLabel.
	self parentPresenter view updateTabs.!

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
		selections size = 1 ifTrue: [newLabel := selections first name].
	].
	self view arrangement: cardNumber printString , ' ' , newLabel.
	self parentPresenter view updateTabs.
!

writeProject
	| projectName |
	projectName := projectListPresenter selection name.
	(MessageBox confirm: 'Write project ' , projectName , ' to disk without committing?')
		ifFalse: [^false].
	((RowanProjectService on: gciSession) name: projectName) write.
	MessageBox notify: 'Project ' , projectName , ' written to disk'! !
!RowanProjectsBrowserPresenter categoriesFor: #addPackageDictionaryInfoTo:!public!request string! !
!RowanProjectsBrowserPresenter categoriesFor: #checkout!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #cloneProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #commit!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createComponents!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiring!create schemantic wiring!overrides!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForClassList!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForPackageDictionaryTabs!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForPackageList!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForProjectDictionaryList!create schemantic wiring!overrides!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForProjectList!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createStateEntry!accessing!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #getViews!public! !
!RowanProjectsBrowserPresenter categoriesFor: #initialize!initialization!public! !
!RowanProjectsBrowserPresenter categoriesFor: #inspectProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #isDictionariesTabSelected!public!testing! !
!RowanProjectsBrowserPresenter categoriesFor: #isProjectListTabSelected!public!testing! !
!RowanProjectsBrowserPresenter categoriesFor: #loadProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #makeDefault!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #newService!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #onViewOpened!menus!overrides!public! !
!RowanProjectsBrowserPresenter categoriesFor: #projectChanges!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #projectDictionaryChanged!event handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #projectLog!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #projectMenuStrings!menus!public! !
!RowanProjectsBrowserPresenter categoriesFor: #pullFromGit!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #pushToGit!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #refresh!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #registerPresentersForUpdates!event handlers!initialization!public!rowan! !
!RowanProjectsBrowserPresenter categoriesFor: #showPackages!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #textTabChanged!event handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #unloadProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #updateAndSelectA:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClass!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClasses!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClassInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateCommand:onSuccessDo:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateMethods!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackage!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackageInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackages!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProject!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProjectInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateSelectionsForUpdate:!public!rowan! !
!RowanProjectsBrowserPresenter categoriesFor: #updateTabLabel!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #writeProject!project menu handlers!public! !

!RowanProjectsBrowserPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 656 nil nil nil 5 nil nil nil 656 482 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 768 nil nil nil 5 nil nil nil 768 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 768 34 2 8 1409286144 131073 896 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 896 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Classes' 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 1136 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 1168 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 896 34 2 8 1409372233 1025 1280 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 962 8 4278190080 nil 29 nil nil nil 1280 nil 8 4294908349 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 '' 373 #left 1490 #displayString 8 #() 1490 #<= 8 #() nil nil 1280 nil 3 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 1712 7 257 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 1792 7 257 nil #report 1392 nil 133217 nil 34 4 nil nil 328198 ##(Smalltalk.Point) 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 1890 9 55 1890 389 433 1280 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 198 0 0 0 243 0 0 0] 8 #() 1890 193 193 nil 27 1074 1106 8 'Hierarchy' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 2176 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 2192 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 896 34 2 8 1409352231 1025 2288 590918 3 ##(Smalltalk.TreeModel) nil 1424 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 1392 962 8 4278190080 nil 29 nil nil nil 2288 nil 8 4294908199 1154 0 nil 1186 2 1 1488 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 2480 7 257 nil ##(Smalltalk.IconicListAbstract) 1552 nil nil nil nil nil 170 176 1392 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 1392 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 9 55 1890 389 433 2288 2066 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 198 0 0 0 243 0 0 0] 8 #() 2128 nil 27 1280 170 192 34 4 1280 8 'classList' 2288 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 896 34 2 8 1140916736 1 2816 1346 138 144 34 2 1120 2160 nil 1424 nil nil 1 nil nil nil 2816 nil 8 4294908629 1154 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 2944 7 257 nil 1154 0 nil 2962 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 3008 7 257 nil 1552 nil nil nil nil nil #smallIcons nil nil nil nil nil 1922 138 144 34 3 1986 #createAt:extent: 34 2 1890 1 1 1890 405 495 2816 1986 #basicSelectionsByIndex: 34 1 8 #(1) 2816 1986 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2816 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 247 0 0 0] 8 #() 2128 nil 27 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 1 1 1890 405 495 896 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 247 0 0 0] 34 3 1280 2288 2816 2128 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 3490 #fixedParentRight 1 3490 #fixedParentTop 1 3490 #fixedParentBottom -43 410 ##(Smalltalk.CardContainer) 34 16 nil 768 34 2 8 1409286144 131073 3568 nil 962 8 4278190080 nil 5 nil nil nil 3568 1010 138 144 34 2 1074 1106 8 'Instance' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 3744 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 3760 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 3568 34 2 8 1140850688 131073 3856 nil nil nil 5 nil nil nil 3856 nil 170 192 1392 nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 9 9 1890 389 1 3856 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 198 0 0 0 4 0 0 0] 8 #() 2128 nil 27 1074 1106 8 'Class' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 4128 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 4144 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 3568 34 2 8 1140850688 131073 4240 nil nil nil 5 nil nil nil 4240 nil 170 192 1392 nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 9 9 1890 389 1 4240 2066 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 198 0 0 0 4 0 0 0] 8 #() 2128 nil 27 3856 170 192 34 4 4240 8 'classTab' 3856 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 3568 34 2 8 1140916738 1 4544 1346 138 144 34 2 3728 4112 nil 1424 nil nil 1 nil nil nil 4544 nil 8 4294908629 1154 0 nil 2962 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 4672 7 257 nil 1154 0 nil 2962 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 4720 7 257 nil 1552 nil nil nil nil nil #smallIcons nil nil nil nil nil 1922 138 144 34 3 1986 #createAt:extent: 34 2 1890 1 1 1890 405 51 4544 1986 #basicSelectionsByIndex: 34 1 8 #(1) 4544 1986 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 4544 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 25 0 0 0] 8 #() 2128 nil 27 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 1 489 1890 405 51 3568 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 202 0 0 0 13 1 0 0] 34 3 3856 4240 4544 2128 nil 27 3458 3504 1 3520 1 3552 -49 3490 #fixedViewTop 51 170 192 34 4 896 8 'classHierarchyTabs' 3568 8 'instanceClassTabs' nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 661 1 1890 405 539 768 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 74 1 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 2 896 3568 2128 nil 27 11 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 656 34 2 8 1409286144 131073 5424 nil 962 8 4278190080 nil 517 nil nil nil 5424 1010 138 144 34 2 1074 1106 8 'Dictionaries' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Dictionaries'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 5600 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 5616 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 5424 34 2 8 1140850688 131073 5744 nil 962 5504 nil 5 nil nil nil 5744 5520 170 192 34 4 410 ##(Smalltalk.ListView) 34 30 nil 5744 34 2 8 1409405001 1025 5856 1346 138 144 1392 nil 1424 962 1456 nil 5 nil nil nil 5856 nil 8 4294908349 1490 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1552 nil nil nil nil nil nil 138 144 34 1 1602 8 'Column 1' 297 #left 1490 #displayString 6000 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 6096 7 257 nil nil 5856 nil 3 nil 1154 0 nil 1186 3 1 1152 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 6176 7 257 nil #report 1392 nil 131169 nil 34 4 nil nil 1890 1 1 nil 1922 138 144 34 2 1986 #createAt:extent: 34 2 1890 1 1 1890 305 395 5856 1986 #text: 34 1 8 'Column 1' 5856 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 152 0 0 0 197 0 0 0] 8 #() 2128 nil 27 8 'projectList' 5712 8 'dictionaryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 5424 34 2 8 1140916736 1 6528 1346 138 144 34 2 1106 8 'Projects' 1154 0 nil 1186 4 1 1152 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 6672 7 1 nil nil 5584 nil 1424 962 5504 nil 1 nil nil nil 6528 nil 8 4294908629 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1552 nil nil nil nil nil #smallIcons nil nil nil nil nil 1922 138 144 34 3 1986 #createAt:extent: 34 2 1890 1 1 1890 321 539 6528 1986 #basicSelectionsByIndex: 34 1 8 #(1) 6528 1986 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 6528 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 8 #() 2128 nil 27 nil 5424 1890 33 33 1049862 ##(Smalltalk.ButtonInteractor) 5744 nil 590342 ##(Smalltalk.Rectangle) 1890 269 397 1890 301 429 1 1180998 4 ##(Smalltalk.CommandDescription) #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 9 101 1890 305 431 5744 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 50 0 0 0 156 0 0 0 9 1 0 0] 34 2 5856 5712 2128 nil 27 34 2 8 1409372233 1025 5712 1346 138 144 1392 nil 1424 962 1456 nil 21 nil nil nil 5712 nil 8 4294908349 1490 #displayString 8 #() nil 1552 nil nil nil nil nil nil 138 144 34 1 1602 8 'Column 1' 297 #left 1490 #displayString 7504 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 7600 7 257 nil nil 5712 nil 3 nil 1154 0 nil 1186 5 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] #item #font #beBold #font: 7680 7 257 nil #report 1392 nil 131169 nil 34 4 nil nil 1890 1 1 nil 1922 138 144 34 2 1986 #createAt:extent: 34 2 1890 1 1 1890 305 395 5712 1986 #text: 34 1 8 'Column 1' 5712 2066 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 152 0 0 0 197 0 0 0] 8 #() 2128 nil 27 1074 6640 5856 5856 170 192 34 2 5744 8 'projectDictionaryTabs' nil 6528 5744 1890 201 201 401 1 29 nil nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 1 1 1890 321 539 5424 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 34 2 5744 6528 2128 nil 27 9 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 8240 nil nil nil 5 nil nil nil 8240 482 170 176 1392 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 8240 34 2 8 1140936777 1025 8368 1346 138 144 1392 nil 1424 962 8 4278190080 nil 21 nil nil nil 8368 nil 8 4294908349 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each |  each].' 1779525917 8512 7 257 nil ##(Smalltalk.IconicListAbstract) 1552 nil nil nil nil nil nil 138 144 34 1 1602 8 'Column 1' 305 #left 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 8640 7 257 nil 1490 #<= 8 #() nil nil 8368 nil 3 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 8752 7 257 nil #report 1392 nil 131185 nil 34 4 nil nil 1890 1 1 nil 1922 138 144 34 2 1986 #createAt:extent: 34 2 1890 1 1 1890 321 539 8368 1986 #text: 34 1 8 'Column 1' 8368 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 8 #() 2128 nil 27 8 'packagesList' nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 331 1 1890 321 539 8240 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 69 1 0 0 13 1 0 0] 34 1 8368 2128 nil 27 9 false 170 192 1392 nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 1 1 1890 1065 539 656 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 5 5424 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 9424 nil 962 8 4278190080 nil 517 nil nil nil 9424 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 321 1 1890 11 539 9424 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 0 0 0 0 165 0 0 0 13 1 0 0] 8 #() 2128 nil 27 8240 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 9680 nil 962 9504 nil 517 nil nil nil 9680 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 651 1 1890 11 539 9680 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 69 1 0 0 0 0 0 0 74 1 0 0 13 1 0 0] 8 #() 2128 nil 27 768 2128 nil 27 7 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 9920 nil nil nil 5 nil nil nil 9920 482 170 176 1392 false 170 192 1392 nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 1075 1 1890 711 539 9920 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 2 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 9920 34 2 8 1409286144 131073 10192 nil 962 8 4278190080 nil 517 nil nil nil 10192 1010 138 144 34 3 1074 8 'Categories' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 10192 34 2 8 1140850688 131073 10400 nil 962 10272 nil 5 nil nil nil 10400 10288 170 192 34 6 10368 8 'categoryList' 410 ##(Smalltalk.ListView) 34 30 nil 10400 34 2 8 1409372233 1025 10528 1346 138 144 1392 nil 1424 962 8 4278190080 nil 21 nil nil nil 10528 nil 8 4294908349 1490 #displayString 8 #() nil 1552 nil nil nil nil nil nil 138 144 34 1 1602 8 'Column 1' 283 #left 1490 #displayString 10688 ##(Smalltalk.SortedCollection) nil nil 10528 nil 3 nil 1154 0 nil 1186 9 1 1152 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 10784 7 257 nil #report 1392 nil 131169 nil 34 4 nil nil 1890 1 1 nil 1922 138 144 34 2 1986 #createAt:extent: 34 2 1890 1 37 1890 291 487 10528 1986 #text: 34 1 8 'Column 1' 10528 2066 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2128 nil 27 8 'pragmaList' 410 ##(Smalltalk.ListView) 34 30 nil 10400 34 2 8 1409372233 1025 11120 1346 138 144 1392 nil 1424 962 10640 nil 21 nil nil nil 11120 nil 8 4294908349 1490 #displayString 10688 nil 1552 nil nil nil nil nil nil 138 144 34 1 1602 8 'Column 1' 283 #left 1490 #displayString 10688 ##(Smalltalk.SortedCollection) nil nil 11120 nil 3 nil 1154 0 nil 1186 9 1 1152 10816 10832 10848 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 11344 7 257 nil #report 1392 nil 131169 nil 34 4 nil nil 1890 1 1 nil 1922 138 144 34 2 1986 #createAt:extent: 34 2 1890 1 37 1890 291 487 11120 1986 #text: 34 1 8 'Column 1' 11120 2066 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2128 nil 27 8 'variableList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 10192 34 2 8 1140916864 1 11632 1346 138 144 34 3 10352 8 'Variables' 8 'Pragmas' nil 1424 962 10272 nil 1 nil nil nil 11632 nil 8 4294908629 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1552 nil nil nil nil nil #noIcons nil nil nil nil nil 1922 138 144 34 3 1986 #createAt:extent: 34 2 1890 1 1 1890 351 539 11632 1986 #basicSelectionsByIndex: 34 1 8 #(1) 11632 1986 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 11632 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 8 #() 2128 nil 27 nil 10192 1890 33 33 7090 10400 nil 7122 1890 255 3 1890 287 35 1 7186 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 53 9 1890 291 523 10400 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 171 0 0 0 9 1 0 0] 34 3 10368 11120 10528 2128 nil 27 34 2 8 1409372233 1025 10368 1346 138 144 1392 nil 1424 962 10640 nil 21 nil nil nil 10368 nil 8 4294908349 1490 #displayString 8 #() nil 1552 nil nil nil nil nil nil 138 144 34 1 1602 8 'Column 1' 283 #left 1490 #displayString 12432 ##(Smalltalk.SortedCollection) nil nil 10368 nil 3 nil 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each | each ]' 1779525917 12528 7 257 nil #report 1392 nil 131169 nil 34 4 nil nil 1890 1 1 nil 1922 138 144 34 2 1986 #createAt:extent: 34 2 1890 1 37 1890 291 487 10368 1986 #text: 34 1 8 'Column 1' 10368 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2128 nil 27 1074 11744 11120 1074 11760 10528 10368 170 192 34 2 10400 8 'methodFilterTabs' nil 11632 10400 1890 201 201 401 1 31 nil nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 1 1 1890 351 539 10192 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 34 2 10400 11632 2128 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 9920 34 2 8 1140850688 1 13088 nil 962 9504 nil 517 nil nil nil 13088 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 351 1 1890 11 539 13088 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 0 0 0 0 180 0 0 0 13 1 0 0] 8 #() 2128 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 9920 34 2 8 1140850688 131073 13328 nil nil nil 5 nil nil nil 13328 834 170 176 34 6 410 ##(Smalltalk.ComboBox) 34 17 nil 13328 34 2 8 1412498947 1025 13440 1346 138 144 1392 nil 1424 962 8 4278190080 nil 5 nil nil nil 13440 nil 8 4294907581 1490 #displayString 8 #() 1392 401 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 1 493 1890 351 53 13440 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 246 0 0 0 175 0 0 0 16 1 0 0] 8 #() 2128 nil 27 3458 3504 1 3520 1 3552 -45 5184 47 410 ##(Smalltalk.ListView) 34 30 nil 13328 34 2 8 1409372233 1025 13792 1346 138 144 1392 nil 1424 962 10640 nil 13 nil nil nil 13792 nil 8 4294908349 1490 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1552 nil nil nil nil nil nil 138 144 34 2 1602 8 'Column 1' 311 #left 1490 #displayString 13936 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 14032 7 257 nil nil 13792 nil 3 nil 1154 0 nil 1186 5 1 1152 8 'doIt' 8 '[:each | (each item at: 4) ifTrue: [each font: each font beItalic]]' 8 #[33 105 226 0 214 4 148 123 17 226 1 160 179 106 60 106] #item #font #beItalic #font: 14112 7 257 nil 1602 8 '' 33 #center 1986 #empty 8 #() 16 1490 #<= 8 #() nil nil 13792 1154 0 nil 1186 6 1 1152 8 'doIt' 8 '[:each | ((each at: 2) ifTrue: [RowanSystemBrowserPresenter overriddenIcon] ifFalse: [Icon blank]) 
	imageIndex]' 8 #[34 105 17 64 148 120 29 159 111 31 161 162 106] ##(Smalltalk.RowanSystemBrowserPresenter) #overriddenIcon ##(Smalltalk.Icon) #blank #imageIndex 14288 7 257 nil 1 nil nil #report 1392 nil 131173 nil 8 #(nil) 1922 138 144 34 3 1986 #createAt:extent: 34 2 1890 1 47 1890 351 447 13792 1986 #text: 34 1 8 'Column 1' 13792 1986 #columnOrder: 34 1 8 #(2 1) 13792 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 175 0 0 0 246 0 0 0] 8 #() 2128 nil 27 3458 3504 1 3520 1 3536 47 3552 -45 410 ##(Smalltalk.ComboBox) 34 17 nil 13328 34 2 8 1412498947 1025 14656 1346 138 144 1392 nil 1424 962 13552 nil 5 nil nil nil 14656 nil 8 4294907581 1490 #displayString 8 #() 1392 401 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 1 1 1890 351 53 14656 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 26 0 0 0] 8 #() 2128 nil 27 3458 3504 1 3520 1 3536 1 5184 47 170 192 34 6 13440 8 'overrideList' 13792 8 'methodList' 14656 8 'superclassList' nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 361 1 1890 351 539 13328 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 99 1 0 0 13 1 0 0] 34 3 14656 13792 13440 2128 nil 27 2128 nil 27 5 false 170 192 1392 nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 1 1 1890 1785 539 544 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 656 410 ##(Smalltalk.Splitter) 34 12 nil 544 34 2 8 1140850688 1 15408 nil 962 9504 nil 517 nil nil nil 15408 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 1065 1 1890 11 539 15408 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 2 0 0 0 0 0 0 25 2 0 0 13 1 0 0] 8 #() 2128 nil 27 9920 2128 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 15680 nil 962 992 nil 5 nil nil nil 15680 1010 138 144 34 8 1074 1106 8 'Package' 1154 0 nil 1186 7 1 1104 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 15840 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 15856 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 15680 34 2 8 1140850688 131073 15952 nil nil nil 5 nil nil nil 15952 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 15952 34 2 8 1409355853 1025 16064 1346 138 144 1392 nil 1424 962 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 5 984134 2 ##(Smalltalk.CommandMenuItem) 1 7186 #addRepository 8 'Add &Repository' 1 1 nil nil nil 16242 1 7186 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 16242 1 7186 #savePackage 8 '&Save Package' 1 1 nil nil nil 16242 1 7186 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 16064 nil 8 4294908349 1490 #displayString 8 #() nil 1552 nil nil nil nil nil nil 138 144 34 4 1602 8 'Repository Type' 211 #left 1490 #displayString 16528 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 16624 7 257 nil nil 16064 nil 1 nil nil 1602 8 'Description' 1161 #left 1490 #displayString 8 #() 1490 #<= 16752 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 16784 7 257 nil nil 16064 nil 3 nil nil 1602 8 'User' 201 #left 1490 #displayString 8 #() 1490 #<= 16912 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 16944 7 257 nil nil 16064 nil 1 nil nil 1602 8 'Password' 201 #left 1490 #displayString 16912 1490 #<= 16912 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 17088 1154 0 nil 17104 19 257 nil 7 257 nil nil 16064 nil 1 nil nil #report 1392 nil 131169 nil 34 4 nil nil 1890 1 1 nil 1922 138 144 34 3 1986 #createAt:extent: 34 2 1890 1 263 1890 1769 217 16064 1986 #contextMenu: 34 1 16208 16064 1986 #text: 34 1 8 'Repository Type' 16064 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 2128 nil 27 19 410 ##(Smalltalk.ListView) 34 30 nil 15952 34 2 8 1409355853 1025 17456 1346 138 144 1392 nil 1424 962 16176 nil 5 16194 nil true 34 1 16242 1 7186 #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 17456 nil 8 4294908349 1490 #displayString 8 #() nil 1552 nil nil nil nil nil nil 138 144 34 4 1602 8 'Ancestor' 131 #left 1490 #displayString 16912 1490 #<= 16912 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 17808 7 257 nil nil 17456 nil 1 nil nil 1602 8 'Name' 401 #left 1490 #displayString 17696 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 17936 7 257 nil nil 17456 nil 1 nil nil 1602 8 'Timestamp' 301 #left 1490 #displayString 16752 1490 #<= 16752 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 18080 7 257 nil nil 17456 nil 1 nil nil 1602 8 'Message' 941 #left 1490 #displayString 16752 1490 #<= 16752 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 18224 7 257 nil nil 17456 nil 3 nil nil #report 1392 nil 131169 nil 34 4 nil nil 1890 1 1 nil 1922 138 144 34 3 1986 #createAt:extent: 34 2 1890 1 1 1890 1769 263 17456 1986 #contextMenu: 34 1 17568 17456 1986 #text: 34 1 8 'Ancestor' 17456 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 2128 nil 27 23 true 170 192 34 4 16064 8 'repositoryList' 17456 8 'ancestorList' nil 1922 138 144 34 2 1986 #createAt:extent: 34 2 1890 9 55 1890 1769 479 15952 1986 #text: 34 1 8 'Package' 15952 2066 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 34 2 17456 16064 2128 nil 27 1074 1106 8 'Globals' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 18880 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 18896 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 15680 34 2 8 1409355853 1025 18992 1346 138 144 1392 nil 1424 962 16176 nil 5 nil nil nil 18992 nil 8 4294908349 1490 #displayString 8 #() nil 1552 nil nil nil nil nil nil 138 144 34 3 1602 8 'Name' 587 #left 1490 #displayString 19136 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 19232 7 257 nil nil 18992 nil 3 nil nil 1602 8 'Class' 589 #left 1490 #displayString 16752 1490 #<= 16752 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 19376 7 257 nil nil 18992 nil 3 nil nil 1602 8 'Value' 587 #left 1490 #displayString 16752 1490 #<= 16752 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 19520 7 257 nil nil 18992 nil 3 nil nil #report 1392 nil 131169 nil 34 4 nil nil 1890 1 1 nil 1922 138 144 34 2 1986 #createAt:extent: 34 2 1890 9 55 1890 1769 479 18992 1986 #text: 34 1 8 'Name' 18992 2066 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2128 nil 27 1074 1106 8 'Original Source' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 19872 ##(Smalltalk.Icon) 8 'COMPILEDMETHOD_PRIVATE.ico' #fromId: #text:iconBlock: 19888 11 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15680 34 2 8 1445007428 1025 19984 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 962 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1890 193 193 nil 19984 nil 8 4294904157 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20322 77 786694 ##(Smalltalk.IndexedColor) 33554457 20370 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 20336 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20352 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 1392 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 19984 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1392 nil 170 176 1392 9215 nil nil nil nil 20370 33554447 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 20336 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 19984 65025 3 false 1 nil nil nil nil nil nil 3 20786 3 19984 33423361 5 false 3 nil nil nil nil nil nil 5 20786 5 19984 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20322 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20322 1027 20898 60239 20898 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20322 1031 20898 335 20898 31317503 1 nil nil nil nil #Error nil nil nil nil 1922 138 144 34 7 1986 #createAt:extent: 34 2 1890 9 55 1890 1769 479 19984 1986 #modificationEventMask: 8 #(4607) 19984 1986 #caretPeriod: 8 #(530) 19984 1986 #wordWrap: 8 #(true) 19984 1986 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 19984 61 3 nil nil 5 nil 21282 3 19984 1 nil 3 67108863 5 nil 21282 5 19984 1 nil 3 -67108863 5 nil 19984 1986 #tabWidth: 8 #(4) 19984 1986 #maxCompletionListHeight: 8 #(9) 19984 2066 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2128 nil 29 1074 1106 8 'SUnit' 1154 0 nil 1186 4 1 1152 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 6754 #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 21504 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 15680 34 2 8 1140850688 131073 21616 nil 962 8 4278190080 nil 5 nil nil nil 21616 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 9 55 1890 1769 479 21616 2066 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 1392 2128 nil 27 1074 1106 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15680 34 2 8 1445007428 1025 21936 20050 nil false 20096 nil 962 8 4278190080 nil 5 16194 nil true 34 11 16242 1 7186 #editSave 8 '&Save' 9383 1 nil nil nil 16354 4097 16242 1 7186 #editUndo 8 '&Undo' 9397 1 nil nil nil 16242 1 7186 #editRedo 8 '&Redo' 9395 1 nil nil nil 16354 4097 16242 1 7186 #editCut 8 'Cu&t' 9393 1 nil nil nil 16242 1 7186 #editCopy 8 '&Copy' 9351 1 nil nil nil 16242 1 7186 #editPaste 8 '&Paste' 9389 1 nil nil nil 16242 1 7186 #editDelete 8 '&Delete' 1629 1 nil nil nil 16354 4097 16242 1 7186 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 20146 nil true 20178 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1890 193 193 nil 21936 nil 8 4294904157 20258 nil nil 13 nil 170 192 34 4 #normal 20322 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20322 77 20384 20400 1 nil nil nil nil #callTip nil nil nil 34 256 22656 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 22672 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 1392 170 192 34 2 #default 20514 1 nil nil nil 21936 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1392 nil 170 176 1392 9215 nil nil nil nil 20576 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 22656 nil nil nil nil 1 nil 170 192 34 6 1 20786 1 21936 65025 3 false 1 nil nil nil nil nil nil 3 20786 3 21936 33423361 5 false 3 nil nil nil nil nil nil 5 20786 5 21936 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20322 1029 nil 20898 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20322 1027 20898 60239 20898 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20322 1031 20898 335 20898 31317503 1 nil nil nil nil #Error nil nil nil nil 1922 138 144 34 8 1986 #createAt:extent: 34 2 1890 9 55 1890 1769 479 21936 1986 #contextMenu: 34 1 22048 21936 1986 #modificationEventMask: 8 #(4607) 21936 1986 #caretPeriod: 8 #(530) 21936 1986 #wordWrap: 8 #(true) 21936 1986 #margins: 34 1 34 3 21282 1 21936 1 3 nil nil 5 nil 21282 3 21936 33 nil 3 67108863 5 nil 21282 5 21936 1 nil 3 -67108863 5 nil 21936 1986 #tabWidth: 8 #(4) 21936 1986 #maxCompletionListHeight: 8 #(9) 21936 2066 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2128 nil 29 1074 1106 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15680 34 2 8 1445007428 1025 23728 20050 nil false 20096 nil 962 22032 nil 5 16194 nil true 34 1 16242 1 7186 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 20146 nil true 20178 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1890 193 193 nil 23728 nil 8 4294904157 20258 nil nil 11 nil 170 192 34 4 #normal 20322 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20322 77 20384 20400 1 nil nil nil nil #callTip nil nil nil 34 256 24048 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 24064 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 20514 1 nil nil nil 23728 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1392 nil 170 176 1392 9215 nil nil nil nil 20576 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 24048 nil nil nil nil 1 nil 170 192 34 6 1 20786 1 23728 65025 3 false 1 nil nil nil nil nil nil 3 20786 3 23728 33423361 5 false 3 nil nil nil nil nil nil 5 20786 5 23728 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20322 1029 nil 20898 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20322 1027 20898 60239 20898 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20322 1031 20898 335 20898 31317503 1 nil nil nil nil #Error nil nil nil nil 1922 138 144 34 8 1986 #createAt:extent: 34 2 1890 9 55 1890 1769 479 23728 1986 #contextMenu: 34 1 23824 23728 1986 #modificationEventMask: 8 #(4607) 23728 1986 #caretPeriod: 8 #(530) 23728 1986 #wordWrap: 8 #(true) 23728 1986 #margins: 34 1 34 3 21282 1 23728 1 3 nil nil 5 nil 21282 3 23728 33 nil 3 67108863 5 nil 21282 5 23728 1 nil 3 -67108863 5 nil 23728 1986 #tabWidth: 8 #(4) 23728 1986 #maxCompletionListHeight: 8 #(9) 23728 2066 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2128 nil 29 1074 1106 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15680 34 2 8 1445007428 1025 25024 20050 nil false 20096 nil 962 8 4278190080 nil 5 nil 20146 nil true 20178 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1890 193 193 nil 25024 nil 8 4294904157 20258 nil nil 13 nil 170 192 34 4 #normal 20322 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20322 77 20384 20400 1 nil nil nil nil #callTip nil nil nil 34 256 25264 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 25280 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20434 1 nil nil false 138 144 1392 170 192 34 2 #default 20514 1 nil nil nil 25024 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1392 nil 170 176 1392 9215 nil nil nil nil 20576 nil nil 65 nil nil 22832 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 25264 #callTip 25280 nil nil nil nil 1 nil 170 192 34 12 1 20786 1 25024 65025 3 false 1 nil nil nil nil nil nil 3 20786 3 25024 33423361 5 false 3 nil nil nil nil nil nil 5 20786 5 25024 511 1 false 5 nil nil nil nil nil nil 8 'indicator10' 20786 21 25024 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator9' 20786 19 25024 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator8' 20786 17 25024 33554447 1 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20322 1029 nil 20898 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20322 1027 20898 60239 20898 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20322 1031 20898 335 20898 31317503 1 nil nil nil nil #Error nil nil nil nil 1922 138 144 34 7 1986 #createAt:extent: 34 2 1890 9 55 1890 1769 479 25024 1986 #modificationEventMask: 8 #(4607) 25024 1986 #hoverTime: 8 #(200) 25024 1986 #caretPeriod: 8 #(530) 25024 1986 #margins: 34 1 34 3 21282 1 25024 61 3 nil nil 5 nil 21282 3 25024 1 nil 3 67108863 nil nil 21282 5 25024 1 nil 3 -67108863 nil nil 25024 1986 #tabWidth: 8 #(4) 25024 1986 #maxCompletionListHeight: 8 #(9) 25024 2066 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2128 nil 29 1074 1106 8 'Project' 1154 0 nil 1186 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 6754 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 26304 7 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 15680 34 2 8 1409372237 1025 26416 1346 138 144 1392 nil 1424 962 16176 nil 5 nil nil nil 26416 nil 8 4294908349 1490 #displayString 19136 nil 1552 nil nil nil nil nil nil 138 144 34 2 1602 8 'Name' 201 #left 1490 #displayString 19136 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each | (each at: 2)]' 8 #[29 105 17 64 148 106] 26640 7 257 nil nil 26416 nil 1 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each item at: 1) displayStringFor: each]' 8 #[31 105 226 0 63 148 17 177 106] #item #displayStringFor: 26720 7 257 nil 1602 8 'Value' 1561 #left 1490 #displayString 8 #() 1490 #<= 26848 1154 0 nil 1186 4 1 1152 8 'doIt' 8 '[:each | (each at: 2) ifNil:[String new] ifNotNil: [:iv |
(each at: 1) instVarNamed: iv]]' 8 #[32 105 17 64 148 100 223 4 97 45 146 106 90 17 63 148 18 178 106] 6754 #String 16 #ifNil:ifNotNil: #instVarNamed: 26880 7 65793 nil nil 26416 nil 3 nil nil #report 1392 nil 131169 nil 34 4 nil nil 1890 1 1 nil 1922 138 144 34 2 1986 #createAt:extent: 34 2 1890 9 55 1890 1769 479 26416 1986 #text: 34 1 8 'Name' 26416 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2128 nil 27 26416 170 192 34 16 21616 8 'sunit' 18992 8 'globals' 26416 8 'projectInfo' 23728 8 'classDocumentation' 21936 8 'classDefinition' 19984 8 'originalSource' 15952 8 'packageInfo' 25024 8 'methodSource' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 15680 34 2 8 1140916736 1 27376 1346 138 144 34 8 26272 15824 18864 21904 23696 19856 24992 21472 nil 1424 nil nil 1 nil nil nil 27376 nil 8 4294908629 1154 0 nil 2962 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 27504 7 257 nil 1154 0 nil 2962 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 27552 7 257 nil 1552 nil nil nil nil nil #smallIcons nil nil nil nil nil 1922 138 144 34 3 1986 #createAt:extent: 34 2 1890 1 1 1890 1785 541 27376 1986 #basicSelectionsByIndex: 34 1 8 #(1) 27376 1986 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 27376 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 14 1 0 0] 8 #() 2128 nil 27 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 1 549 1890 1785 541 15680 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 1 0 0 124 3 0 0 32 2 0 0] 34 9 26416 15952 18992 21936 23728 19984 25024 21616 27376 2128 nil 27 15650 3 5 true 170 192 34 4 15680 8 'textAreaTabs' 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 28064 nil 962 9504 nil 517 nil nil nil 28064 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 1 539 1890 1785 11 28064 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 13 1 0 0 124 3 0 0 18 1 0 0] 8 #() 2128 nil 27 8 'splitter' nil 1922 138 144 34 1 1986 #createAt:extent: 34 2 1890 7679 21 1890 1785 1089 416 2066 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 123 18 0 0 42 2 0 0] 34 3 544 28064 15680 2128 nil 27 )! !
!RowanProjectsBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

