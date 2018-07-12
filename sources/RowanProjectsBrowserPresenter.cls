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
	BrowserUpdate current register: categoryListPresenter with: #classUpdate:.
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

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 656 nil nil nil 5 nil nil nil 656 482 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 768 nil nil nil 5 nil nil nil 768 482 170 176 8 #() false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 768 34 2 8 1140936777 1025 912 590662 2 ##(Smalltalk.ListModel) 138 144 864 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 21 nil nil nil 912 nil 8 4294902475 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 1 83886081 1120 8 'doIt' 8 '[:each |  each].' 1779525917 1136 7 257 nil ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 305 #left 1122 0 nil 1154 2 1 1120 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 1328 7 257 nil 459270 ##(Smalltalk.Message) #<= 8 #() nil nil 912 nil 3 nil 1122 0 nil 1154 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 1456 7 257 nil #report 864 nil 131185 nil 34 4 nil nil 328198 ##(Smalltalk.Point) 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 1554 1 1 1554 321 539 912 1650 #text: 34 1 8 'Column 1' 912 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 8 #() 1554 193 193 nil 27 8 'packagesList' nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 331 1 1554 321 539 768 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 69 1 0 0 13 1 0 0] 34 1 912 1840 nil 27 9 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 656 34 2 8 1409286144 131073 2032 nil 1058 8 4278190080 nil 517 nil nil nil 2032 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Dictionaries' 1122 0 nil 1154 7 1 16 8 'doIt' 8 '(CardLabel text: ''Dictionaries'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 2224 2256 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 2272 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 2032 34 2 8 1140850688 131073 2400 nil 1058 2112 nil 5 nil nil nil 2400 2144 170 192 34 4 410 ##(Smalltalk.ListView) 34 30 nil 2400 34 2 8 1409405001 1025 2512 978 138 144 864 nil 1040 1058 8 4278190080 nil 5 nil nil nil 2512 nil 8 4294902475 1410 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1232 nil nil nil nil nil nil 138 144 34 1 1282 8 'Column 1' 297 #left 1410 #displayString 2672 ##(Smalltalk.SortedCollection) 1122 0 nil 1154 2 1 1120 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 2768 7 257 nil nil 2512 nil 3 nil 1122 0 nil 1154 3 1 1120 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 2848 7 257 nil #report 864 nil 131169 nil 34 4 nil nil 1554 1 1 nil 1586 138 144 34 2 1650 #createAt:extent: 34 2 1554 1 1 1554 305 395 2512 1650 #text: 34 1 8 'Column 1' 2512 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 152 0 0 0 197 0 0 0] 8 #() 1840 nil 27 8 'projectList' 2368 8 'dictionaryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 2032 34 2 8 1140916736 1 3200 978 138 144 34 2 2226 8 'Projects' 1122 0 nil 1154 4 1 1120 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 3344 7 1 nil nil 2240 nil 1040 1058 2112 nil 1 nil nil nil 3200 nil 8 4294908745 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1232 nil nil nil nil nil #smallIcons nil nil nil nil nil 1586 138 144 34 3 1650 #createAt:extent: 34 2 1554 1 1 1554 321 539 3200 1650 #basicSelectionsByIndex: 34 1 8 #(1) 3200 1650 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 3200 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 8 #() 1840 nil 27 nil 2032 1554 33 33 1049862 ##(Smalltalk.ButtonInteractor) 2400 nil 590342 ##(Smalltalk.Rectangle) 1554 269 397 1554 301 429 1 1180998 4 ##(Smalltalk.CommandDescription) #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 9 101 1554 305 431 2400 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 50 0 0 0 156 0 0 0 9 1 0 0] 34 2 2512 2368 1840 nil 27 34 2 8 1409372233 1025 2368 978 138 144 864 nil 1040 1058 2624 nil 21 nil nil nil 2368 nil 8 4294902475 1410 #displayString 8 #() nil 1232 nil nil nil nil nil nil 138 144 34 1 1282 8 'Column 1' 297 #left 1410 #displayString 4176 ##(Smalltalk.SortedCollection) 1122 0 nil 1154 2 1 1120 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 4272 7 257 nil nil 2368 nil 3 nil 1122 0 nil 1154 5 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] #item #font #beBold #font: 4352 7 257 nil #report 864 nil 131169 nil 34 4 nil nil 1554 1 1 nil 1586 138 144 34 2 1650 #createAt:extent: 34 2 1554 1 1 1554 305 395 2368 1650 #text: 34 1 8 'Column 1' 2368 1778 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 152 0 0 0 197 0 0 0] 8 #() 1840 nil 27 2194 3312 2512 2512 170 192 34 2 2400 8 'projectDictionaryTabs' nil 3200 2400 1554 201 201 401 1 29 nil nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1 1 1554 321 539 2032 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 34 2 2400 3200 1840 nil 27 9 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 4912 nil nil nil 5 nil nil nil 4912 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 4912 34 2 8 1409286144 131073 5040 nil 1058 8 4278190080 nil 5 nil nil nil 5040 2130 138 144 34 2 2194 2226 8 'Instance' 1122 0 nil 1154 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 2224 5216 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 5232 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 5040 34 2 8 1140850688 131073 5328 nil nil nil 5 nil nil nil 5328 nil 170 192 864 nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 9 9 1554 389 1 5328 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 198 0 0 0 4 0 0 0] 8 #() 1840 nil 27 2194 2226 8 'Class' 1122 0 nil 1154 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 2224 5600 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 5616 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 5040 34 2 8 1140850688 131073 5712 nil nil nil 5 nil nil nil 5712 nil 170 192 864 nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 9 9 1554 389 1 5712 1778 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 198 0 0 0 4 0 0 0] 8 #() 1840 nil 27 5328 170 192 34 4 5712 8 'classTab' 5328 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 5040 34 2 8 1140916738 1 6016 978 138 144 34 2 5200 5584 nil 1040 nil nil 1 nil nil nil 6016 nil 8 4294908745 1122 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 6144 7 257 nil 1122 0 nil 6162 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 6208 7 257 nil 1232 nil nil nil nil nil #smallIcons nil nil nil nil nil 1586 138 144 34 3 1650 #createAt:extent: 34 2 1554 1 1 1554 405 51 6016 1650 #basicSelectionsByIndex: 34 1 8 #(1) 6016 1650 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 6016 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 25 0 0 0] 8 #() 1840 nil 27 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1 489 1554 405 51 5040 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 202 0 0 0 13 1 0 0] 34 3 5328 5712 6016 1840 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 6690 #fixedParentRight 1 6690 #fixedParentBottom -49 6690 #fixedViewTop 51 410 ##(Smalltalk.CardContainer) 34 16 nil 4912 34 2 8 1409286144 131073 6768 nil 1058 8 4278190080 nil 5 nil nil nil 6768 2130 138 144 34 2 2194 2226 8 'Classes' 1122 0 nil 1154 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 2224 6944 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 6960 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 6768 34 2 8 1409372233 1025 7056 978 138 144 864 nil 1040 1058 2624 nil 29 nil nil nil 7056 nil 8 4294902475 1410 #displayString 8 #() nil 1232 nil nil nil nil nil nil 138 144 34 1 1282 8 '' 373 #left 1410 #displayString 8 #() 1410 #<= 8 #() nil nil 7056 nil 3 1122 0 nil 1154 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 7344 7 257 nil 1122 0 nil 1154 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 7424 7 257 nil #report 864 nil 133217 nil 34 4 nil nil 1554 1 1 nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 9 55 1554 389 433 7056 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 198 0 0 0 243 0 0 0] 8 #() 1840 nil 27 2194 2226 8 'Hierarchy' 1122 0 nil 1154 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 2224 7728 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 7744 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 6768 34 2 8 1409352231 1025 7840 590918 3 ##(Smalltalk.TreeModel) nil 1040 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 864 1058 8 4278190080 nil 29 nil nil nil 7840 nil 8 4294908711 1122 0 nil 1154 2 1 1408 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 8032 7 257 nil ##(Smalltalk.IconicListAbstract) 1232 nil nil nil nil nil 170 176 864 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 864 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 9 55 1554 389 433 7840 1778 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 198 0 0 0 243 0 0 0] 8 #() 1840 nil 27 7056 170 192 34 4 7056 8 'classList' 7840 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 6768 34 2 8 1140916736 1 8368 978 138 144 34 2 6928 7712 nil 1040 nil nil 1 nil nil nil 8368 nil 8 4294908745 1122 0 nil 6162 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 8496 7 257 nil 1122 0 nil 6162 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 8544 7 257 nil 1232 nil nil nil nil nil #smallIcons nil nil nil nil nil 1586 138 144 34 3 1650 #createAt:extent: 34 2 1554 1 1 1554 405 495 8368 1650 #basicSelectionsByIndex: 34 1 8 #(1) 8368 1650 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 8368 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 247 0 0 0] 8 #() 1840 nil 27 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1 1 1554 405 495 6768 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 247 0 0 0] 34 3 7056 7840 8368 1840 nil 27 6658 6704 1 6720 1 6690 #fixedParentTop 1 6736 -43 170 192 34 4 5040 8 'instanceClassTabs' 6768 8 'classHierarchyTabs' nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 661 1 1554 405 539 4912 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 74 1 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 2 6768 5040 1840 nil 27 11 false 170 192 864 nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1 1 1554 1065 539 656 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 5 2032 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 9424 nil 1058 8 4278190080 nil 517 nil nil nil 9424 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 321 1 1554 11 539 9424 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 0 0 0 0 165 0 0 0 13 1 0 0] 8 #() 1840 nil 27 768 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 9680 nil 1058 9504 nil 517 nil nil nil 9680 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 651 1 1554 11 539 9680 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 69 1 0 0 0 0 0 0 74 1 0 0 13 1 0 0] 8 #() 1840 nil 27 4912 1840 nil 27 7 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 9920 nil nil nil 5 nil nil nil 9920 482 170 176 864 false 170 192 864 nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1075 1 1554 711 539 9920 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 2 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 9920 34 2 8 1409286144 131073 10192 nil 1058 8 4278190080 nil 517 nil nil nil 10192 2130 138 144 34 3 2194 8 'Categories' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 10192 34 2 8 1140850688 131073 10400 nil 1058 10272 nil 5 nil nil nil 10400 10288 170 192 34 6 410 ##(Smalltalk.ListView) 34 30 nil 10400 34 2 8 1409372233 1025 10512 978 138 144 864 nil 1040 1058 8 4278190080 nil 21 nil nil nil 10512 nil 8 4294902475 1410 #displayString 8 #() nil 1232 nil nil nil nil nil nil 138 144 34 1 1282 8 'Column 1' 283 #left 1410 #displayString 10672 ##(Smalltalk.SortedCollection) nil nil 10512 nil 3 nil 1122 0 nil 1154 9 1 1120 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 10768 7 257 nil #report 864 nil 131169 nil 34 4 nil nil 1554 1 1 nil 1586 138 144 34 2 1650 #createAt:extent: 34 2 1554 1 37 1554 291 487 10512 1650 #text: 34 1 8 'Column 1' 10512 1778 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 1840 nil 27 8 'variableList' 10368 8 'categoryList' 410 ##(Smalltalk.ListView) 34 30 nil 10400 34 2 8 1409372233 1025 11120 978 138 144 864 nil 1040 1058 10624 nil 21 nil nil nil 11120 nil 8 4294902475 1410 #displayString 10672 nil 1232 nil nil nil nil nil nil 138 144 34 1 1282 8 'Column 1' 283 #left 1410 #displayString 10672 ##(Smalltalk.SortedCollection) nil nil 11120 nil 3 nil 1122 0 nil 1154 9 1 1120 10800 10816 10832 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 11344 7 257 nil #report 864 nil 131169 nil 34 4 nil nil 1554 1 1 nil 1586 138 144 34 2 1650 #createAt:extent: 34 2 1554 1 37 1554 291 487 11120 1650 #text: 34 1 8 'Column 1' 11120 1778 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 1840 nil 27 8 'pragmaList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 10192 34 2 8 1140916864 1 11632 978 138 144 34 3 10352 8 'Variables' 8 'Pragmas' nil 1040 1058 10272 nil 1 nil nil nil 11632 nil 8 4294908745 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1232 nil nil nil nil nil #noIcons nil nil nil nil nil 1586 138 144 34 3 1650 #createAt:extent: 34 2 1554 1 1 1554 351 539 11632 1650 #basicSelectionsByIndex: 34 1 8 #(1) 11632 1650 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 11632 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 8 #() 1840 nil 27 nil 10192 1554 33 33 3762 10400 nil 3794 1554 255 3 1554 287 35 1 3858 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 53 9 1554 291 523 10400 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 171 0 0 0 9 1 0 0] 34 3 10368 10512 11120 1840 nil 27 34 2 8 1409372233 1025 10368 978 138 144 864 nil 1040 1058 10624 nil 21 nil nil nil 10368 nil 8 4294902475 1410 #displayString 8 #() nil 1232 nil nil nil nil nil nil 138 144 34 1 1282 8 'Column 1' 283 #left 1410 #displayString 12432 ##(Smalltalk.SortedCollection) nil nil 10368 nil 3 nil 1122 0 nil 1154 1 83886081 1120 8 'doIt' 8 '[:each | each ]' 1779525917 12528 7 257 nil #report 864 nil 131169 nil 34 4 nil nil 1554 1 1 nil 1586 138 144 34 2 1650 #createAt:extent: 34 2 1554 1 37 1554 291 487 10368 1650 #text: 34 1 8 'Column 1' 10368 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 1840 nil 27 2194 11744 10512 2194 11760 11120 10368 170 192 34 2 10400 8 'methodFilterTabs' nil 11632 10400 1554 201 201 401 1 31 nil nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1 1 1554 351 539 10192 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 34 2 10400 11632 1840 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 9920 34 2 8 1140850688 1 13088 nil 1058 9504 nil 517 nil nil nil 13088 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 351 1 1554 11 539 13088 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 0 0 0 0 180 0 0 0 13 1 0 0] 8 #() 1840 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 9920 34 2 8 1140850688 131073 13328 nil nil nil 5 nil nil nil 13328 4978 170 176 34 6 410 ##(Smalltalk.ListView) 34 30 nil 13328 34 2 8 1409372233 1025 13440 978 138 144 864 nil 1040 1058 10624 nil 13 nil nil nil 13440 nil 8 4294902475 1410 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1232 nil nil nil nil nil nil 138 144 34 2 1282 8 '' 41 #left 1410 #displayString 8 #() 1410 #<= 13680 nil nil 13440 nil 1 nil nil 1282 8 'Selector' 311 #left 1410 #displayString 13584 ##(Smalltalk.SortedCollection) 1122 0 nil 1154 2 1 1120 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 13760 7 257 nil nil 13440 nil 3 nil nil #report 864 nil 131173 nil 34 4 nil nil 1554 1 1 nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1 47 1554 351 447 13440 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 175 0 0 0 246 0 0 0] 8 #() 1840 nil 27 6658 6704 1 6720 1 9008 47 6736 -45 410 ##(Smalltalk.ComboBox) 34 17 nil 13328 34 2 8 1412498947 1025 14048 978 138 144 864 nil 1040 1058 8 4278190080 nil 5 nil nil nil 14048 nil 8 4294903317 1410 #displayString 8 #() 864 401 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1 493 1554 351 53 14048 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 246 0 0 0 175 0 0 0 16 1 0 0] 8 #() 1840 nil 27 6658 6704 1 6720 1 6736 -45 6752 47 410 ##(Smalltalk.ComboBox) 34 17 nil 13328 34 2 8 1412498947 1025 14400 978 138 144 864 nil 1040 1058 14160 nil 5 nil nil nil 14400 nil 8 4294903317 1410 #displayString 8 #() 864 401 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1 1 1554 351 53 14400 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 26 0 0 0] 8 #() 1840 nil 27 6658 6704 1 6720 1 9008 1 6752 47 170 192 34 6 13440 8 'methodList' 14048 8 'overrideList' 14400 8 'superclassList' nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 361 1 1554 351 539 13328 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 99 1 0 0 13 1 0 0] 34 3 14400 13440 14048 1840 nil 27 1840 nil 27 5 false 170 192 864 nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1 1 1554 1785 539 544 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 656 410 ##(Smalltalk.Splitter) 34 12 nil 544 34 2 8 1140850688 1 15152 nil 1058 9504 nil 517 nil nil nil 15152 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1065 1 1554 11 539 15152 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 2 0 0 0 0 0 0 25 2 0 0 13 1 0 0] 8 #() 1840 nil 27 9920 1840 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 15424 nil 1058 6848 nil 5 nil nil nil 15424 2130 138 144 34 8 2194 2226 8 'Package' 1122 0 nil 1154 7 1 2224 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 2224 15584 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 15600 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 15424 34 2 8 1140850688 131073 15696 nil nil nil 5 nil nil nil 15696 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 15696 34 2 8 1409355853 1025 15808 978 138 144 864 nil 1040 1058 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 5 984134 2 ##(Smalltalk.CommandMenuItem) 1 3858 #addRepository 8 'Add &Repository' 1 1 nil nil nil 15986 1 3858 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 15986 1 3858 #savePackage 8 '&Save Package' 1 1 nil nil nil 15986 1 3858 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 15808 nil 8 4294902475 1410 #displayString 8 #() nil 1232 nil nil nil nil nil nil 138 144 34 4 1282 8 'Repository Type' 211 #left 1410 #displayString 16272 ##(Smalltalk.SortedCollection) 1122 0 nil 1154 2 1 1120 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 16368 7 257 nil nil 15808 nil 1 nil nil 1282 8 'Description' 1161 #left 1410 #displayString 8 #() 1410 #<= 16496 1122 0 nil 1154 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 16528 7 257 nil nil 15808 nil 3 nil nil 1282 8 'User' 201 #left 1410 #displayString 8 #() 1410 #<= 16656 1122 0 nil 1154 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 16688 7 257 nil nil 15808 nil 1 nil nil 1282 8 'Password' 201 #left 1410 #displayString 16656 1410 #<= 16656 1122 0 nil 1154 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 16832 1122 0 nil 16848 19 257 nil 7 257 nil nil 15808 nil 1 nil nil #report 864 nil 131169 nil 34 4 nil nil 1554 1 1 nil 1586 138 144 34 3 1650 #createAt:extent: 34 2 1554 1 263 1554 1769 217 15808 1650 #contextMenu: 34 1 15952 15808 1650 #text: 34 1 8 'Repository Type' 15808 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 1840 nil 27 19 410 ##(Smalltalk.ListView) 34 30 nil 15696 34 2 8 1409355853 1025 17200 978 138 144 864 nil 1040 1058 15920 nil 5 15938 nil true 34 1 15986 1 3858 #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 17200 nil 8 4294902475 1410 #displayString 8 #() nil 1232 nil nil nil nil nil nil 138 144 34 4 1282 8 'Ancestor' 131 #left 1410 #displayString 16656 1410 #<= 16656 1122 0 nil 1154 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 17552 7 257 nil nil 17200 nil 1 nil nil 1282 8 'Name' 401 #left 1410 #displayString 17440 ##(Smalltalk.SortedCollection) 1122 0 nil 1154 1 83886081 1120 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 17680 7 257 nil nil 17200 nil 1 nil nil 1282 8 'Timestamp' 301 #left 1410 #displayString 16496 1410 #<= 16496 1122 0 nil 1154 1 83886081 1120 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 17824 7 257 nil nil 17200 nil 1 nil nil 1282 8 'Message' 941 #left 1410 #displayString 16496 1410 #<= 16496 1122 0 nil 1154 1 83886081 1120 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 17968 7 257 nil nil 17200 nil 3 nil nil #report 864 nil 131169 nil 34 4 nil nil 1554 1 1 nil 1586 138 144 34 3 1650 #createAt:extent: 34 2 1554 1 1 1554 1769 263 17200 1650 #contextMenu: 34 1 17312 17200 1650 #text: 34 1 8 'Ancestor' 17200 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 1840 nil 27 23 true 170 192 34 4 15808 8 'repositoryList' 17200 8 'ancestorList' nil 1586 138 144 34 2 1650 #createAt:extent: 34 2 1554 9 55 1554 1769 479 15696 1650 #text: 34 1 8 'Package' 15696 1778 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 34 2 17200 15808 1840 nil 27 2194 2226 8 'Globals' 1122 0 nil 1154 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 2224 18624 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 18640 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 15424 34 2 8 1409355853 1025 18736 978 138 144 864 nil 1040 1058 15920 nil 5 nil nil nil 18736 nil 8 4294902475 1410 #displayString 8 #() nil 1232 nil nil nil nil nil nil 138 144 34 3 1282 8 'Name' 587 #left 1410 #displayString 18880 ##(Smalltalk.SortedCollection) 1122 0 nil 1154 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 18976 7 257 nil nil 18736 nil 3 nil nil 1282 8 'Class' 589 #left 1410 #displayString 16496 1410 #<= 16496 1122 0 nil 1154 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 19120 7 257 nil nil 18736 nil 3 nil nil 1282 8 'Value' 587 #left 1410 #displayString 16496 1410 #<= 16496 1122 0 nil 1154 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 19264 7 257 nil nil 18736 nil 3 nil nil #report 864 nil 131169 nil 34 4 nil nil 1554 1 1 nil 1586 138 144 34 2 1650 #createAt:extent: 34 2 1554 9 55 1554 1769 479 18736 1650 #text: 34 1 8 'Name' 18736 1778 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 1840 nil 27 2194 2226 8 'Original Source' 1122 0 nil 1154 7 1 16 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 2224 19616 ##(Smalltalk.Icon) 8 'COMPILEDMETHOD_PRIVATE.ico' #fromId: #text:iconBlock: 19632 11 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15424 34 2 8 1445007428 1025 19728 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 1058 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1554 193 193 nil 19728 nil 8 4294908713 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20066 77 786694 ##(Smalltalk.IndexedColor) 33554457 20114 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 20080 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20096 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 864 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 19728 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 864 nil 170 176 864 9215 nil nil nil nil 20114 33554447 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 20080 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 19728 65025 3 false 1 nil nil nil nil nil nil 3 20530 3 19728 33423361 5 false 3 nil nil nil nil nil nil 5 20530 5 19728 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20066 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20066 1027 20642 60239 20642 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20066 1031 20642 335 20642 31317503 1 nil nil nil nil #Error nil nil nil nil 1586 138 144 34 7 1650 #createAt:extent: 34 2 1554 9 55 1554 1769 479 19728 1650 #modificationEventMask: 8 #(4607) 19728 1650 #caretPeriod: 8 #(530) 19728 1650 #wordWrap: 8 #(true) 19728 1650 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 19728 61 3 nil nil 5 nil 21026 3 19728 1 nil 3 67108863 5 nil 21026 5 19728 1 nil 3 -67108863 5 nil 19728 1650 #tabWidth: 8 #(4) 19728 1650 #maxCompletionListHeight: 8 #(9) 19728 1778 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 1840 nil 29 2194 2226 8 'SUnit' 1122 0 nil 1154 4 1 1120 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 3426 #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 21248 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 15424 34 2 8 1140850688 131073 21360 nil 1058 8 4278190080 nil 5 nil nil nil 21360 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 9 55 1554 1769 479 21360 1778 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 864 1840 nil 27 2194 2226 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15424 34 2 8 1445007428 1025 21680 19794 nil false 19840 nil 1058 8 4278190080 nil 5 15938 nil true 34 11 15986 1 3858 #editSave 8 '&Save' 9383 1 nil nil nil 16098 4097 15986 1 3858 #editUndo 8 '&Undo' 9397 1 nil nil nil 15986 1 3858 #editRedo 8 '&Redo' 9395 1 nil nil nil 16098 4097 15986 1 3858 #editCut 8 'Cu&t' 9393 1 nil nil nil 15986 1 3858 #editCopy 8 '&Copy' 9351 1 nil nil nil 15986 1 3858 #editPaste 8 '&Paste' 9389 1 nil nil nil 15986 1 3858 #editDelete 8 '&Delete' 1629 1 nil nil nil 16098 4097 15986 1 3858 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 19890 nil true 19922 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1554 193 193 nil 21680 nil 8 4294908713 20002 nil nil 13 nil 170 192 34 4 #normal 20066 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20066 77 20128 20144 1 nil nil nil nil #callTip nil nil nil 34 256 22400 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 22416 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 864 170 192 34 2 #default 20258 1 nil nil nil 21680 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 864 nil 170 176 864 9215 nil nil nil nil 20320 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 22400 nil nil nil nil 1 nil 170 192 34 6 1 20530 1 21680 65025 3 false 1 nil nil nil nil nil nil 3 20530 3 21680 33423361 5 false 3 nil nil nil nil nil nil 5 20530 5 21680 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20066 1029 nil 20642 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20066 1027 20642 60239 20642 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20066 1031 20642 335 20642 31317503 1 nil nil nil nil #Error nil nil nil nil 1586 138 144 34 8 1650 #createAt:extent: 34 2 1554 9 55 1554 1769 479 21680 1650 #contextMenu: 34 1 21792 21680 1650 #modificationEventMask: 8 #(4607) 21680 1650 #caretPeriod: 8 #(530) 21680 1650 #wordWrap: 8 #(true) 21680 1650 #margins: 34 1 34 3 21026 1 21680 1 3 nil nil 5 nil 21026 3 21680 33 nil 3 67108863 5 nil 21026 5 21680 1 nil 3 -67108863 5 nil 21680 1650 #tabWidth: 8 #(4) 21680 1650 #maxCompletionListHeight: 8 #(9) 21680 1778 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 1840 nil 29 2194 2226 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15424 34 2 8 1445007428 1025 23472 19794 nil false 19840 nil 1058 21776 nil 5 15938 nil true 34 1 15986 1 3858 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 19890 nil true 19922 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1554 193 193 nil 23472 nil 8 4294908713 20002 nil nil 11 nil 170 192 34 4 #normal 20066 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20066 77 20128 20144 1 nil nil nil nil #callTip nil nil nil 34 256 23792 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 23808 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 20258 1 nil nil nil 23472 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 864 nil 170 176 864 9215 nil nil nil nil 20320 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 23792 nil nil nil nil 1 nil 170 192 34 6 1 20530 1 23472 65025 3 false 1 nil nil nil nil nil nil 3 20530 3 23472 33423361 5 false 3 nil nil nil nil nil nil 5 20530 5 23472 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20066 1029 nil 20642 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20066 1027 20642 60239 20642 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20066 1031 20642 335 20642 31317503 1 nil nil nil nil #Error nil nil nil nil 1586 138 144 34 8 1650 #createAt:extent: 34 2 1554 9 55 1554 1769 479 23472 1650 #contextMenu: 34 1 23568 23472 1650 #modificationEventMask: 8 #(4607) 23472 1650 #caretPeriod: 8 #(530) 23472 1650 #wordWrap: 8 #(true) 23472 1650 #margins: 34 1 34 3 21026 1 23472 1 3 nil nil 5 nil 21026 3 23472 33 nil 3 67108863 5 nil 21026 5 23472 1 nil 3 -67108863 5 nil 23472 1650 #tabWidth: 8 #(4) 23472 1650 #maxCompletionListHeight: 8 #(9) 23472 1778 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 1840 nil 29 2194 2226 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15424 34 2 8 1445007428 1025 24768 19794 nil false 19840 nil 1058 8 4278190080 nil 5 nil 19890 nil true 19922 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1554 193 193 nil 24768 nil 8 4294908713 20002 nil nil 13 nil 170 192 34 4 #normal 20066 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20066 77 20128 20144 1 nil nil nil nil #callTip nil nil nil 34 256 25008 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 25024 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20178 1 nil nil false 138 144 864 170 192 34 2 #default 20258 1 nil nil nil 24768 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 864 nil 170 176 864 9215 nil nil nil nil 20320 nil nil 65 nil nil 22576 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 25008 #callTip 25024 nil nil nil nil 1 nil 170 192 34 12 1 20530 1 24768 65025 3 false 1 nil nil nil nil nil nil 3 20530 3 24768 33423361 5 false 3 nil nil nil nil nil nil 5 20530 5 24768 511 1 false 5 nil nil nil nil nil nil 8 'indicator10' 20530 21 24768 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator8' 20530 17 24768 33554447 1 nil nil nil nil nil nil nil nil 8 'indicator9' 20530 19 24768 33554459 13 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20066 1029 nil 20642 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20066 1027 20642 60239 20642 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20066 1031 20642 335 20642 31317503 1 nil nil nil nil #Error nil nil nil nil 1586 138 144 34 7 1650 #createAt:extent: 34 2 1554 9 55 1554 1769 479 24768 1650 #modificationEventMask: 8 #(4607) 24768 1650 #hoverTime: 8 #(200) 24768 1650 #caretPeriod: 8 #(530) 24768 1650 #margins: 34 1 34 3 21026 1 24768 61 3 nil nil 5 nil 21026 3 24768 1 nil 3 67108863 nil nil 21026 5 24768 1 nil 3 -67108863 nil nil 24768 1650 #tabWidth: 8 #(4) 24768 1650 #maxCompletionListHeight: 8 #(9) 24768 1778 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 1840 nil 29 2194 2226 8 'Project' 1122 0 nil 1154 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 3426 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 26048 7 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 15424 34 2 8 1409372237 1025 26160 978 138 144 864 nil 1040 1058 15920 nil 5 nil nil nil 26160 nil 8 4294902475 1410 #displayString 18880 nil 1232 nil nil nil nil nil nil 138 144 34 2 1282 8 'Name' 201 #left 1410 #displayString 18880 ##(Smalltalk.SortedCollection) 1122 0 nil 1154 1 83886081 1120 8 'doIt' 8 '[:each | (each at: 2)]' 8 #[29 105 17 64 148 106] 26384 7 257 nil nil 26160 nil 1 nil 1122 0 nil 1154 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each item at: 1) displayStringFor: each]' 8 #[31 105 226 0 63 148 17 177 106] #item #displayStringFor: 26464 7 257 nil 1282 8 'Value' 1561 #left 1410 #displayString 8 #() 1410 #<= 26592 1122 0 nil 1154 4 1 1120 8 'doIt' 8 '[:each | (each at: 2) ifNil:[String new] ifNotNil: [:iv |
(each at: 1) instVarNamed: iv]]' 8 #[32 105 17 64 148 100 223 4 97 45 146 106 90 17 63 148 18 178 106] 3426 #String 16 #ifNil:ifNotNil: #instVarNamed: 26624 7 65793 nil nil 26160 nil 3 nil nil #report 864 nil 131169 nil 34 4 nil nil 1554 1 1 nil 1586 138 144 34 2 1650 #createAt:extent: 34 2 1554 9 55 1554 1769 479 26160 1650 #text: 34 1 8 'Name' 26160 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 1840 nil 27 26160 170 192 34 16 26160 8 'projectInfo' 21680 8 'classDefinition' 15696 8 'packageInfo' 19728 8 'originalSource' 18736 8 'globals' 23472 8 'classDocumentation' 24768 8 'methodSource' 21360 8 'sunit' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 15424 34 2 8 1140916736 1 27120 978 138 144 34 8 26016 15568 18608 21648 23440 19600 24736 21216 nil 1040 nil nil 1 nil nil nil 27120 nil 8 4294908745 1122 0 nil 6162 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 27248 7 257 nil 1122 0 nil 6162 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 27296 7 257 nil 1232 nil nil nil nil nil #smallIcons nil nil nil nil nil 1586 138 144 34 3 1650 #createAt:extent: 34 2 1554 1 1 1554 1785 541 27120 1650 #basicSelectionsByIndex: 34 1 8 #(1) 27120 1650 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 27120 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 14 1 0 0] 8 #() 1840 nil 27 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1 549 1554 1785 541 15424 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 1 0 0 124 3 0 0 32 2 0 0] 34 9 26160 15696 18736 21680 23472 19728 24768 21360 27120 1840 nil 27 15394 3 5 true 170 192 34 4 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 27792 nil 1058 9504 nil 517 nil nil nil 27792 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 1 539 1554 1785 11 27792 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 13 1 0 0 124 3 0 0 18 1 0 0] 8 #() 1840 nil 27 8 'splitter' 15424 8 'textAreaTabs' nil 1586 138 144 34 1 1650 #createAt:extent: 34 2 1554 7679 21 1554 1785 1089 416 1778 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 123 18 0 0 42 2 0 0] 34 3 544 27792 15424 1840 nil 27 )! !
!RowanProjectsBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

