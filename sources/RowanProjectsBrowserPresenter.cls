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

createSchematicWiringForInstanceClassTabs
	instanceClassTabs
		when: #currentCardChanging: send: #aboutToChange: to: self;
		when: #currentCardChanged send: #updateClass to: self!

createSchematicWiringForMethodCategoryList

	categoryListPresenter
		when: #'dragOver:'			send: #'onDragOverMethodCategory:'	to: self;
		when: #'drop:'				send: #'onDropOnMethodCategory:'	to: self;
		when: #'keyTyped:'			send: #'delayUpdate'				to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'			to: self;
		when: #'selectionChanged'		send: #'updateTabLabel'			to: self;
		when: #'selectionChanged'		send: #updateClass				to: self;
		yourself.
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
	BrowserUpdate current register: categoryListPresenter with: #categoryUpdate:.
	BrowserUpdate current register: methodListPresenter with: #classMethodsUpdate:!

setClassMeta
	| meta |
	meta := instanceClassTabs currentCard name = 'classTab'.
	classListPresenter list do: [:service | service meta: meta]!

setMethodFilter
	| filters |
	filters := categoryListPresenter selections. 
	classListPresenter list do: [:service | service filters: filters]!

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
	| selection | 
	self setClassMeta.
	self setMethodFilter. 
	selection := classListPresenter selectionIfNone: [^self].
	self updateServices: (Array with: selection).
	self updateClassInfo!

updateClasses
	classListPresenter clear.
	self updateServices: packageListPresenter selections!

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

updatePackage
	self updateClasses.
	self updatePackageInfo.!

updatePackageInfo!

updatePackages
	"assume that we are already under stonCritical semaphore protection"

	|  selections |
	packageListPresenter clear.
	selections := projectListPresenter selections isEmpty
				ifTrue: [Array with: RowanProjectService new]
				ifFalse: [projectListPresenter selections].
	self updateServices: selections !

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
	projectInfoTab ensureVisible.
	self updateProjectTab!

updateProjectTab
	| projectTabLabel |
	projectTabLabel := (projectListPresenter selectionIfNone: [])
				ifNil: ['Projects']
				ifNotNil: [:sel | 'Projects - ' , sel name].
	self view arrangement: projectTabLabel.
	self parentPresenter view updateTabs!

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

updateServices: services
	| browserUpdate stonString stonResults updatedServices |
	stonString := STON toString: services.
	stonResults := self gciSession serverPerform: #updateFromSton: with: stonString.
	browserUpdate := BrowserUpdate current initializeUpdates.
	updatedServices := STON fromString: stonResults.
	updatedServices do: [:newService | services do: [:service | service replicateFrom: newService]].
	browserUpdate updates: updatedServices!

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
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForInstanceClassTabs!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForMethodCategoryList!create schemantic wiring!public! !
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
!RowanProjectsBrowserPresenter categoriesFor: #registerPresentersForUpdates!event handlers!initialization!public!register presenters! !
!RowanProjectsBrowserPresenter categoriesFor: #setClassMeta!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #setMethodFilter!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #showPackages!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #textTabChanged!event handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #unloadProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #updateAndSelectA:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClass!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClasses!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClassInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateCommand:onSuccessDo:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackage!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackageInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackages!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProject!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProjectInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProjectTab!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateSelectionsForUpdate:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateServices:!public!ston! !
!RowanProjectsBrowserPresenter categoriesFor: #updateTabLabel!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #writeProject!project menu handlers!public! !

!RowanProjectsBrowserPresenter class methodsFor!

downArrowIcon

	^Icon fromId: 'DownArrow.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 544 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 544 655878 ##(Smalltalk.CardLayout) 138 144 34 8 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Package' 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 7 1 752 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 784 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 816 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 928 nil nil nil 5 nil nil nil 928 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 928 34 2 8 1409355853 1025 1040 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 610 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 5 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #addRepository 8 'Add &Repository' 1 1 nil nil nil 1282 1 1314 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 1282 1 1314 #savePackage 8 '&Save Package' 1 1 nil nil nil 1282 1 1314 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 1040 nil 8 4294902661 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Repository Type' 211 #left 1570 #displayString 1600 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 1744 7 257 nil nil 1040 nil 1 nil nil 1682 8 'Description' 1161 #left 1570 #displayString 8 #() 1570 #<= 1872 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 1904 7 257 nil nil 1040 nil 3 nil nil 1682 8 'User' 201 #left 1570 #displayString 8 #() 1570 #<= 2032 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 2064 7 257 nil nil 1040 nil 1 nil nil 1682 8 'Password' 201 #left 1570 #displayString 2032 1570 #<= 2032 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 2208 802 0 nil 2224 19 257 nil 7 257 nil nil 1040 nil 1 nil nil #report 1152 nil 131169 nil 34 4 nil nil 328198 ##(Smalltalk.Point) 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 2322 1 263 2322 1769 217 1040 2418 #contextMenu: 34 1 1248 1040 2418 #text: 34 1 8 'Repository Type' 1040 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 2322 193 193 nil 27 19 410 ##(Smalltalk.ListView) 34 30 nil 928 34 2 8 1409355853 1025 2656 1106 138 144 1152 nil 1184 610 1216 nil 5 1234 nil true 34 1 1282 1 1314 #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 2656 nil 8 4294902661 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 4 1682 8 'Ancestor' 131 #left 1570 #displayString 2032 1570 #<= 2032 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 3008 7 257 nil nil 2656 nil 1 nil nil 1682 8 'Name' 401 #left 1570 #displayString 2896 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 3136 7 257 nil nil 2656 nil 1 nil nil 1682 8 'Timestamp' 301 #left 1570 #displayString 1872 1570 #<= 1872 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 3280 7 257 nil nil 2656 nil 1 nil nil 1682 8 'Message' 941 #left 1570 #displayString 1872 1570 #<= 1872 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 3424 7 257 nil nil 2656 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2322 1 1 nil 2354 138 144 34 3 2418 #createAt:extent: 34 2 2322 1 1 2322 1769 263 2656 2418 #contextMenu: 34 1 2768 2656 2418 #text: 34 1 8 'Ancestor' 2656 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 2640 nil 27 23 true 170 192 34 4 1040 8 'repositoryList' 2656 8 'ancestorList' nil 2354 138 144 34 2 2418 #createAt:extent: 34 2 2322 9 55 2322 1769 479 928 2418 #text: 34 1 8 'Package' 928 2578 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 34 2 2656 1040 2640 nil 27 722 754 8 'Globals' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 4080 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 4096 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 544 34 2 8 1409355853 1025 4192 1106 138 144 1152 nil 1184 610 1216 nil 5 nil nil nil 4192 nil 8 4294902661 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 3 1682 8 'Name' 587 #left 1570 #displayString 4336 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 4432 7 257 nil nil 4192 nil 3 nil nil 1682 8 'Class' 589 #left 1570 #displayString 1872 1570 #<= 1872 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 4576 7 257 nil nil 4192 nil 3 nil nil 1682 8 'Value' 587 #left 1570 #displayString 1872 1570 #<= 1872 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 4720 7 257 nil nil 4192 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2322 1 1 nil 2354 138 144 34 2 2418 #createAt:extent: 34 2 2322 9 55 2322 1769 479 4192 2418 #text: 34 1 8 'Name' 4192 2578 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2640 nil 27 722 754 8 'Original Source' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 5072 ##(Smalltalk.Icon) 8 'COMPILEDMETHOD_PRIVATE.ico' #fromId: #text:iconBlock: 5088 11 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 5184 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 610 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2322 193 193 nil 5184 nil 8 4294907549 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5522 77 786694 ##(Smalltalk.IndexedColor) 33554457 5570 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 5536 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 5552 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 1152 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 5184 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5570 33554447 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 5536 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 5184 65025 3 false 1 nil nil nil nil nil nil 3 5986 3 5184 33423361 5 false 3 nil nil nil nil nil nil 5 5986 5 5184 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 5522 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 5522 1027 6098 60239 6098 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5522 1031 6098 335 6098 31317503 1 nil nil nil nil #Error nil nil nil nil 2354 138 144 34 7 2418 #createAt:extent: 34 2 2322 9 55 2322 1769 479 5184 2418 #modificationEventMask: 8 #(4607) 5184 2418 #caretPeriod: 8 #(530) 5184 2418 #wordWrap: 8 #(true) 5184 2418 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 5184 61 3 nil nil 5 nil 6482 3 5184 1 nil 3 67108863 5 nil 6482 5 5184 1 nil 3 -67108863 5 nil 5184 2418 #tabWidth: 8 #(4) 5184 2418 #maxCompletionListHeight: 8 #(9) 5184 2578 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2640 nil 29 722 754 8 'SUnit' 802 0 nil 834 4 1 800 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 6704 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 544 34 2 8 1140850688 131073 6832 nil 610 8 4278190080 nil 5 nil nil nil 6832 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 9 55 2322 1769 479 6832 2578 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 1152 2640 nil 27 722 754 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 7152 5250 nil false 5296 nil 610 8 4278190080 nil 5 1234 nil true 34 11 1282 1 1314 #editSave 8 '&Save' 9383 1 nil nil nil 1410 4097 1282 1 1314 #editUndo 8 '&Undo' 9397 1 nil nil nil 1282 1 1314 #editRedo 8 '&Redo' 9395 1 nil nil nil 1410 4097 1282 1 1314 #editCut 8 'Cu&t' 9393 1 nil nil nil 1282 1 1314 #editCopy 8 '&Copy' 9351 1 nil nil nil 1282 1 1314 #editPaste 8 '&Paste' 9389 1 nil nil nil 1282 1 1314 #editDelete 8 '&Delete' 1629 1 nil nil nil 1410 4097 1282 1 1314 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 5346 nil true 5378 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2322 193 193 nil 7152 nil 8 4294907549 5458 nil nil 13 nil 170 192 34 4 #normal 5522 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5522 77 5584 5600 1 nil nil nil nil #callTip nil nil nil 34 256 7872 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 7888 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 1152 170 192 34 2 #default 5714 1 nil nil nil 7152 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5776 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 7872 nil nil nil nil 1 nil 170 192 34 6 1 5986 1 7152 65025 3 false 1 nil nil nil nil nil nil 3 5986 3 7152 33423361 5 false 3 nil nil nil nil nil nil 5 5986 5 7152 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 5522 1029 nil 6098 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 5522 1027 6098 60239 6098 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5522 1031 6098 335 6098 31317503 1 nil nil nil nil #Error nil nil nil nil 2354 138 144 34 8 2418 #createAt:extent: 34 2 2322 9 55 2322 1769 479 7152 2418 #contextMenu: 34 1 7264 7152 2418 #modificationEventMask: 8 #(4607) 7152 2418 #caretPeriod: 8 #(530) 7152 2418 #wordWrap: 8 #(true) 7152 2418 #margins: 34 1 34 3 6482 1 7152 1 3 nil nil 5 nil 6482 3 7152 33 nil 3 67108863 5 nil 6482 5 7152 1 nil 3 -67108863 5 nil 7152 2418 #tabWidth: 8 #(4) 7152 2418 #maxCompletionListHeight: 8 #(9) 7152 2578 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2640 nil 29 722 754 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 8944 5250 nil false 5296 nil 610 7248 nil 5 1234 nil true 34 1 1282 1 1314 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 5346 nil true 5378 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2322 193 193 nil 8944 nil 8 4294907549 5458 nil nil 11 nil 170 192 34 4 #normal 5522 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5522 77 5584 5600 1 nil nil nil nil #callTip nil nil nil 34 256 9264 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 9280 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 5714 1 nil nil nil 8944 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5776 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 9264 nil nil nil nil 1 nil 170 192 34 6 1 5986 1 8944 65025 3 false 1 nil nil nil nil nil nil 3 5986 3 8944 33423361 5 false 3 nil nil nil nil nil nil 5 5986 5 8944 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 5522 1029 nil 6098 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 5522 1027 6098 60239 6098 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5522 1031 6098 335 6098 31317503 1 nil nil nil nil #Error nil nil nil nil 2354 138 144 34 8 2418 #createAt:extent: 34 2 2322 9 55 2322 1769 479 8944 2418 #contextMenu: 34 1 9040 8944 2418 #modificationEventMask: 8 #(4607) 8944 2418 #caretPeriod: 8 #(530) 8944 2418 #wordWrap: 8 #(true) 8944 2418 #margins: 34 1 34 3 6482 1 8944 1 3 nil nil 5 nil 6482 3 8944 33 nil 3 67108863 5 nil 6482 5 8944 1 nil 3 -67108863 5 nil 8944 2418 #tabWidth: 8 #(4) 8944 2418 #maxCompletionListHeight: 8 #(9) 8944 2578 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2640 nil 29 722 754 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 10240 5250 nil false 5296 nil 610 8 4278190080 nil 5 nil 5346 nil true 5378 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2322 193 193 nil 10240 nil 8 4294907549 5458 nil nil 13 nil 170 192 34 4 #normal 5522 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5522 77 5584 5600 1 nil nil nil nil #callTip nil nil nil 34 256 10480 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 10496 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 5634 1 nil nil false 138 144 1152 170 192 34 2 #default 5714 1 nil nil nil 10240 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5776 nil nil 65 nil nil 8048 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 10480 #callTip 10496 nil nil nil nil 1 nil 170 192 34 12 1 5986 1 10240 65025 3 false 1 nil nil nil nil nil nil 3 5986 3 10240 33423361 5 false 3 nil nil nil nil nil nil 5 5986 5 10240 511 1 false 5 nil nil nil nil nil nil 8 'indicator9' 5986 19 10240 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator10' 5986 21 10240 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator8' 5986 17 10240 33554447 1 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 5522 1029 nil 6098 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 5522 1027 6098 60239 6098 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5522 1031 6098 335 6098 31317503 1 nil nil nil nil #Error nil nil nil nil 2354 138 144 34 7 2418 #createAt:extent: 34 2 2322 9 55 2322 1769 479 10240 2418 #modificationEventMask: 8 #(4607) 10240 2418 #hoverTime: 8 #(200) 10240 2418 #caretPeriod: 8 #(530) 10240 2418 #margins: 34 1 34 3 6482 1 10240 61 3 nil nil 5 nil 6482 3 10240 1 nil 3 67108863 nil nil 6482 5 10240 1 nil 3 -67108863 nil nil 10240 2418 #tabWidth: 8 #(4) 10240 2418 #maxCompletionListHeight: 8 #(9) 10240 2578 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2640 nil 29 722 754 8 'Project' 802 0 nil 834 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 6786 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 11520 7 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 544 34 2 8 1409372237 1025 11632 1106 138 144 1152 nil 1184 610 1216 nil 5 nil nil nil 11632 nil 8 4294902661 1570 #displayString 4336 nil 1632 nil nil nil nil nil nil 138 144 34 2 1682 8 'Name' 201 #left 1570 #displayString 4336 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | (each at: 2)]' 8 #[29 105 17 64 148 106] 11856 7 257 nil nil 11632 nil 1 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each item at: 1) displayStringFor: each]' 8 #[31 105 226 0 63 148 17 177 106] #item #displayStringFor: 11936 7 257 nil 1682 8 'Value' 1561 #left 1570 #displayString 8 #() 1570 #<= 12064 802 0 nil 834 4 1 800 8 'doIt' 8 '[:each | (each at: 2) ifNil:[String new] ifNotNil: [:iv |
(each at: 1) instVarNamed: iv]]' 8 #[32 105 17 64 148 100 223 4 97 45 146 106 90 17 63 148 18 178 106] 6786 #String 16 #ifNil:ifNotNil: #instVarNamed: 12096 7 65793 nil nil 11632 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2322 1 1 nil 2354 138 144 34 2 2418 #createAt:extent: 34 2 2322 9 55 2322 1769 479 11632 2418 #text: 34 1 8 'Name' 11632 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2640 nil 27 11632 170 192 34 16 8944 8 'classDocumentation' 10240 8 'methodSource' 928 8 'packageInfo' 7152 8 'classDefinition' 6832 8 'sunit' 11632 8 'projectInfo' 4192 8 'globals' 5184 8 'originalSource' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 544 34 2 8 1140916736 1 12592 1106 138 144 34 8 11488 768 4064 7120 8912 5056 10208 6672 nil 1184 nil nil 1 nil nil nil 12592 nil 8 4294907555 802 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 12720 7 257 nil 802 0 nil 12738 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 12784 7 257 nil 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 2354 138 144 34 3 2418 #createAt:extent: 34 2 2322 1 1 2322 1785 541 12592 2418 #basicSelectionsByIndex: 34 1 8 #(1) 12592 2418 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 12592 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 14 1 0 0] 8 #() 2640 nil 27 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1 549 2322 1785 541 544 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 1 0 0 124 3 0 0 32 2 0 0] 34 9 11632 928 4192 7152 8944 5184 10240 6832 12592 2640 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 13264 nil nil nil 5 nil nil nil 13264 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 13264 34 2 8 1140850688 131073 13376 nil nil nil 5 nil nil nil 13376 482 170 176 1152 false 170 192 1152 nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1075 1 2322 711 539 13376 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 2 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 13376 34 2 8 1409286144 131073 13648 nil 610 8 4278190080 nil 517 nil nil nil 13648 658 138 144 34 3 722 8 'Categories' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 13648 34 2 8 1140850688 131073 13856 nil 610 13728 nil 5 nil nil nil 13856 13744 170 192 34 6 410 ##(Smalltalk.ListView) 34 30 nil 13856 34 2 8 1409372233 1025 13968 1106 138 144 1152 nil 1184 610 8 4278190080 nil 21 nil nil nil 13968 nil 8 4294902661 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 283 #left 1570 #displayString 14128 ##(Smalltalk.SortedCollection) nil nil 13968 nil 3 nil 802 0 nil 834 9 1 800 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 14224 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2322 1 1 nil 2354 138 144 34 2 2418 #createAt:extent: 34 2 2322 1 37 2322 291 487 13968 2418 #text: 34 1 8 'Column 1' 13968 2578 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2640 nil 27 8 'pragmaList' 13824 8 'categoryList' 410 ##(Smalltalk.ListView) 34 30 nil 13856 34 2 8 1409372233 1025 14576 1106 138 144 1152 nil 1184 610 14080 nil 21 nil nil nil 14576 nil 8 4294902661 1570 #displayString 14128 nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 283 #left 1570 #displayString 14128 ##(Smalltalk.SortedCollection) nil nil 14576 nil 3 nil 802 0 nil 834 9 1 800 14256 14272 14288 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 14800 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2322 1 1 nil 2354 138 144 34 2 2418 #createAt:extent: 34 2 2322 1 37 2322 291 487 14576 2418 #text: 34 1 8 'Column 1' 14576 2578 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2640 nil 27 8 'variableList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 13648 34 2 8 1140916864 1 15088 1106 138 144 34 3 13808 8 'Variables' 8 'Pragmas' nil 1184 610 13728 nil 1 nil nil nil 15088 nil 8 4294907555 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil #noIcons nil nil nil nil nil 2354 138 144 34 3 2418 #createAt:extent: 34 2 2322 1 1 2322 351 539 15088 2418 #basicSelectionsByIndex: 34 1 8 #(1) 15088 2418 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 15088 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 8 #() 2640 nil 27 nil 13648 2322 33 33 1049862 ##(Smalltalk.ButtonInteractor) 13856 nil 590342 ##(Smalltalk.Rectangle) 2322 255 3 2322 287 35 1 1314 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 53 9 2322 291 523 13856 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 171 0 0 0 9 1 0 0] 34 3 13824 14576 13968 2640 nil 27 34 2 8 1409372233 1025 13824 1106 138 144 1152 nil 1184 610 14080 nil 21 nil nil nil 13824 nil 8 4294902661 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 283 #left 1570 #displayString 1152 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each]' 1779525917 16016 7 257 nil nil 13824 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2322 1 1 nil 2354 138 144 34 2 2418 #createAt:extent: 34 2 2322 1 37 2322 291 487 13824 2418 #text: 34 1 8 'Column 1' 13824 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2640 nil 27 722 15200 14576 722 15216 13968 13824 170 192 34 2 13856 8 'methodFilterTabs' nil 15088 13856 2322 201 201 401 1 31 nil nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1 1 2322 351 539 13648 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 34 2 13856 15088 2640 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 13376 34 2 8 1140850688 1 16576 nil 610 8 4278190080 nil 517 nil nil nil 16576 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 351 1 2322 11 539 16576 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 0 0 0 0 180 0 0 0 13 1 0 0] 8 #() 2640 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 13376 34 2 8 1140850688 131073 16832 nil nil nil 5 nil nil nil 16832 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ComboBox) 34 17 nil 16832 34 2 8 1412498947 1025 16960 1106 138 144 1152 nil 1184 610 8 4278190080 nil 5 nil nil nil 16960 nil 8 4294907677 1570 #displayString 8 #() 1152 401 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1 1 2322 351 53 16960 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 26 0 0 0] 8 #() 2640 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 17330 #fixedParentRight 1 17330 #fixedParentTop 1 17330 #fixedViewTop 47 410 ##(Smalltalk.ListView) 34 30 nil 16832 34 2 8 1409372233 1025 17408 1106 138 144 1152 nil 1184 610 14080 nil 13 nil nil nil 17408 nil 8 4294902661 1570 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil nil 138 144 34 2 1682 8 '' 41 #left 2418 #empty 8 #() 16 1570 #<= 8 #() nil nil 17408 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each subSuperIcon]' 8 #[30 105 226 0 106] #subSuperIcon 17696 7 257 nil 1 nil nil 1682 8 'Selector' 311 #left 1570 #displayString 17552 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 17824 7 257 nil nil 17408 nil 3 nil nil #report 1152 nil 131173 nil 34 4 nil nil 2322 1 1 nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1 47 2322 351 447 17408 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 175 0 0 0 246 0 0 0] 8 #() 2640 nil 27 17298 17344 1 17360 1 17376 47 17330 #fixedParentBottom -45 410 ##(Smalltalk.ComboBox) 34 17 nil 16832 34 2 8 1412498947 1025 18128 1106 138 144 1152 nil 1184 610 17072 nil 5 nil nil nil 18128 nil 8 4294907677 1570 #displayString 8 #() 1152 401 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1 493 2322 351 53 18128 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 246 0 0 0 175 0 0 0 16 1 0 0] 8 #() 2640 nil 27 17298 17344 1 17360 1 18112 -45 17392 47 170 192 34 6 16960 8 'superclassList' 17408 8 'methodList' 18128 8 'overrideList' nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 361 1 2322 351 539 16832 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 99 1 0 0 13 1 0 0] 34 3 16960 17408 18128 2640 nil 27 2640 nil 27 5 410 ##(Smalltalk.ContainerView) 34 15 nil 13264 34 2 8 1140850688 131073 18704 nil nil nil 5 nil nil nil 18704 482 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 18704 34 2 8 1140850688 131073 18816 nil nil nil 5 nil nil nil 18816 482 170 176 1152 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 18816 34 2 8 1140936777 1025 18944 1106 138 144 1152 nil 1184 610 8 4278190080 nil 21 nil nil nil 18944 nil 8 4294902661 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each |  each].' 1779525917 19088 7 257 nil ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 305 #left 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 19216 7 257 nil 1570 #<= 8 #() nil nil 18944 nil 3 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 19328 7 257 nil #report 1152 nil 131185 nil 34 4 nil nil 2322 1 1 nil 2354 138 144 34 2 2418 #createAt:extent: 34 2 2322 1 1 2322 321 539 18944 2418 #text: 34 1 8 'Column 1' 18944 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 8 #() 2640 nil 27 8 'packagesList' nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 331 1 2322 321 539 18816 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 69 1 0 0 13 1 0 0] 34 1 18944 2640 nil 27 9 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 18704 34 2 8 1409286144 131073 19824 nil 610 8 4278190080 nil 517 nil nil nil 19824 658 138 144 34 2 722 754 8 'Dictionaries' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Dictionaries'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 20000 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 20016 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 19824 34 2 8 1140850688 131073 20144 nil 610 19904 nil 5 nil nil nil 20144 19920 170 192 34 4 410 ##(Smalltalk.ListView) 34 30 nil 20144 34 2 8 1409405001 1025 20256 1106 138 144 1152 nil 1184 610 8 4278190080 nil 5 nil nil nil 20256 nil 8 4294902661 1570 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 297 #left 1570 #displayString 20416 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 20512 7 257 nil nil 20256 nil 3 nil 802 0 nil 834 3 1 800 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 20592 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2322 1 1 nil 2354 138 144 34 2 2418 #createAt:extent: 34 2 2322 1 1 2322 305 395 20256 2418 #text: 34 1 8 'Column 1' 20256 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 152 0 0 0 197 0 0 0] 8 #() 2640 nil 27 8 'projectList' 20112 8 'dictionaryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 19824 34 2 8 1140916736 1 20944 1106 138 144 34 2 754 8 'Projects' 802 0 nil 834 4 1 800 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 6786 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 21088 7 1 nil nil 19984 nil 1184 610 19904 nil 1 nil nil nil 20944 nil 8 4294907555 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 2354 138 144 34 3 2418 #createAt:extent: 34 2 2322 1 1 2322 321 539 20944 2418 #basicSelectionsByIndex: 34 1 8 #(1) 20944 2418 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 20944 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 8 #() 2640 nil 27 nil 19824 2322 33 33 15522 20144 nil 15554 2322 269 397 2322 301 429 1 1314 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 9 101 2322 305 431 20144 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 50 0 0 0 156 0 0 0 9 1 0 0] 34 2 20256 20112 2640 nil 27 34 2 8 1409372233 1025 20112 1106 138 144 1152 nil 1184 610 20368 nil 21 nil nil nil 20112 nil 8 4294902661 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 'Column 1' 297 #left 1570 #displayString 21856 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 21952 7 257 nil nil 20112 nil 3 nil 802 0 nil 834 5 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] #item #font #beBold #font: 22032 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2322 1 1 nil 2354 138 144 34 2 2418 #createAt:extent: 34 2 2322 1 1 2322 305 395 20112 2418 #text: 34 1 8 'Column 1' 20112 2578 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 152 0 0 0 197 0 0 0] 8 #() 2640 nil 27 722 21056 20256 20256 170 192 34 2 20144 8 'projectDictionaryTabs' nil 20944 20144 2322 201 201 401 1 29 nil nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1 1 2322 321 539 19824 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 34 2 20144 20944 2640 nil 27 9 410 ##(Smalltalk.ContainerView) 34 15 nil 18704 34 2 8 1140850688 131073 22592 nil nil nil 5 nil nil nil 22592 16898 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 22592 34 2 8 1409286144 131073 22704 nil 610 640 nil 5 nil nil nil 22704 658 138 144 34 2 722 754 8 'Classes' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 22864 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 22880 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 22704 34 2 8 1409372233 1025 22976 1106 138 144 1152 nil 1184 610 20368 nil 29 nil nil nil 22976 nil 8 4294902661 1570 #displayString 8 #() nil 1632 nil nil nil nil nil nil 138 144 34 1 1682 8 '' 373 #left 1570 #displayString 8 #() 1570 #<= 8 #() nil nil 22976 nil 3 802 0 nil 834 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 23264 7 257 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 23344 7 257 nil #report 1152 nil 133217 nil 34 4 nil nil 2322 1 1 nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 9 55 2322 389 433 22976 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 198 0 0 0 243 0 0 0] 8 #() 2640 nil 27 722 754 8 'Hierarchy' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 23648 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 23664 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 22704 34 2 8 1409352231 1025 23760 590918 3 ##(Smalltalk.TreeModel) nil 1184 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 1152 610 8 4278190080 nil 29 nil nil nil 23760 nil 8 4294903857 802 0 nil 834 2 1 1568 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 23952 7 257 nil ##(Smalltalk.IconicListAbstract) 1632 nil nil nil nil nil 170 176 1152 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 1152 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 9 55 2322 389 433 23760 2578 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 198 0 0 0 243 0 0 0] 8 #() 2640 nil 27 22976 170 192 34 4 23760 8 'classHierarchy' 22976 8 'classList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 22704 34 2 8 1140916736 1 24288 1106 138 144 34 2 22848 23632 nil 1184 nil nil 1 nil nil nil 24288 nil 8 4294907555 802 0 nil 12738 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 24416 7 257 nil 802 0 nil 12738 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 24464 7 257 nil 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 2354 138 144 34 3 2418 #createAt:extent: 34 2 2322 1 1 2322 405 495 24288 2418 #basicSelectionsByIndex: 34 1 8 #(1) 24288 2418 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 24288 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 247 0 0 0] 8 #() 2640 nil 27 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1 1 2322 405 495 22704 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 247 0 0 0] 34 3 22976 23760 24288 2640 nil 27 17298 17344 1 17360 1 17376 1 18112 -43 410 ##(Smalltalk.CardContainer) 34 16 nil 22592 34 2 8 1409286144 131073 24928 nil 610 8 4278190080 nil 5 nil nil nil 24928 658 138 144 34 2 722 754 8 'Instance' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 25104 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 25120 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 24928 34 2 8 1140850688 131073 25216 nil nil nil 5 nil nil nil 25216 nil 170 192 1152 nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 9 9 2322 389 1 25216 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 198 0 0 0 4 0 0 0] 8 #() 2640 nil 27 722 754 8 'Class' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 25488 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 25504 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 24928 34 2 8 1140850688 131073 25600 nil nil nil 5 nil nil nil 25600 nil 170 192 1152 nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 9 9 2322 389 1 25600 2578 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 198 0 0 0 4 0 0 0] 8 #() 2640 nil 27 25216 170 192 34 4 25216 8 'instanceTab' 25600 8 'classTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 24928 34 2 8 1140916738 1 25904 1106 138 144 34 2 25088 25472 nil 1184 nil nil 1 nil nil nil 25904 nil 8 4294907555 802 0 nil 12738 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 26032 7 257 nil 802 0 nil 12738 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 26080 7 257 nil 1632 nil nil nil nil nil #smallIcons nil nil nil nil nil 2354 138 144 34 3 2418 #createAt:extent: 34 2 2322 1 1 2322 405 51 25904 2418 #basicSelectionsByIndex: 34 1 8 #(1) 25904 2418 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 25904 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 25 0 0 0] 8 #() 2640 nil 27 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1 489 2322 405 51 24928 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 202 0 0 0 13 1 0 0] 34 3 25216 25600 25904 2640 nil 27 17298 17344 1 17360 1 18112 -49 17392 51 170 192 34 4 22704 8 'classHierarchyTabs' 24928 8 'instanceClassTabs' nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 661 1 2322 405 539 22592 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 74 1 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 2 22704 24928 2640 nil 27 11 false 170 192 1152 nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1 1 2322 1065 539 18704 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 5 19824 410 ##(Smalltalk.Splitter) 34 12 nil 18704 34 2 8 1140850688 1 26944 nil 610 16656 nil 517 nil nil nil 26944 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 321 1 2322 11 539 26944 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 0 0 0 0 165 0 0 0 13 1 0 0] 8 #() 2640 nil 27 18816 410 ##(Smalltalk.Splitter) 34 12 nil 18704 34 2 8 1140850688 1 27184 nil 610 16656 nil 517 nil nil nil 27184 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 651 1 2322 11 539 27184 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 69 1 0 0 0 0 0 0 74 1 0 0 13 1 0 0] 8 #() 2640 nil 27 22592 2640 nil 27 7 false 170 192 1152 nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1 1 2322 1785 539 13264 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 18704 410 ##(Smalltalk.Splitter) 34 12 nil 13264 34 2 8 1140850688 1 27600 nil 610 16656 nil 517 nil nil nil 27600 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1065 1 2322 11 539 27600 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 2 0 0 0 0 0 0 25 2 0 0 13 1 0 0] 8 #() 2640 nil 27 13376 2640 nil 27 13234 3 5 true 170 192 34 4 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 27888 nil 610 16656 nil 517 nil nil nil 27888 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 1 539 2322 1785 11 27888 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 13 1 0 0 124 3 0 0 18 1 0 0] 8 #() 2640 nil 27 8 'splitter' 544 8 'textAreaTabs' nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2322 7679 21 2322 1785 1089 416 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 123 18 0 0 42 2 0 0] 34 3 13264 27888 544 2640 nil 27 )! !
!RowanProjectsBrowserPresenter class categoriesFor: #downArrowIcon!public! !
!RowanProjectsBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

