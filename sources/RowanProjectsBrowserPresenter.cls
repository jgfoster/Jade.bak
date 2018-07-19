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

basicRemoveMethods
	| services |
	services := methodListPresenter selections.
	services do: [:service | service source: nil].
	self updateServices: services!

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

createSchematicWiringForMethodList

	methodListPresenter			
		when: #'drag:'				send: #'onDragMethod:'			to: self;
		when: #'dragCut:'			send: #'onDragCutMethod:'			to: self;
		when: #'keyTyped:'			send: #'delayUpdate'				to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'			to: self;
		when: #'selectionChanged'		send: #updateMethodSource		to: self;
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

editSaveMethod
	| caretPosition firstVisibleLine service |
	caretPosition := methodSourcePresenter view caretPosition.
	firstVisibleLine := methodSourcePresenter view lineScroll.
	service := methodListPresenter selectionIfNone: [^self].
	(self isCategoriesTabSelected and: [categoryListPresenter hasSelection])
		ifTrue: [service category: categoryListPresenter selection trimBlanks].
	service source: self currentMethodSource.
	[self updateServices: (Array with: service)] on: GsCompileError
		do: [:ex | self reportCompileError: ex list].
	methodSourcePresenter view
		isModified: false;
		ensureVisible.
	methodSourcePresenter view firstVisibleLine: firstVisibleLine.
	methodSourcePresenter view goto: caretPosition!

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

methodSourceEqual
	^methodListPresenter selectionOrNil
		ifNil: [true]
		ifNotNil: [:selection | methodSourcePresenter value = selection source]!

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

registerClassData
	"temporarily disabled"!

registerPresentersForUpdates
	super registerPresentersForUpdates.
	BrowserUpdate current register: projectListPresenter with: #projectsUpdate:.
	BrowserUpdate current register: packageListPresenter with: #projectPackagesUpdate:.
	BrowserUpdate current register: classListPresenter with: #classesUpdate:.
	BrowserUpdate current register: categoryListPresenter with: #categoryUpdate:.
	BrowserUpdate current register: methodListPresenter with: #classMethodsUpdate:!

removeMethodServices: classService
	| stonString stonResults |
	classService command: #removeMethods.
	classService methods: (methodListPresenter selections). 
	stonString := STON toString: classService.
	stonResults := self gciSession serverPerform: #updateFromSton: with: stonString.
	self update: classService afterStonReplication: stonResults!

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

update: services afterStonReplication: stonResults
	| updatedServices browserUpdate |
	browserUpdate := BrowserUpdate current initializeUpdates.
	updatedServices := STON fromString: stonResults.
	updatedServices do: [:newService | services do: [:service | service replicateFrom: newService]].
	browserUpdate updates: updatedServices!

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
	selection := classListPresenter selectionIfNone: 
					[categoryListPresenter clear.
					^methodListPresenter clear].
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

updateMethodSource
	methodListPresenter selectionIfNone: [^methodSourcePresenter value: String new].
	methodSourcePresenter value: methodListPresenter selection source.
	methodSourcePresenter ensureVisible!

updatePackage
	self updateClasses.
	self updatePackageInfo.!

updatePackageInfo!

updatePackages
	| selections |
	packageListPresenter clear.
	selections := projectListPresenter selections isEmpty
				ifTrue: [Array with: RowanProjectService new]
				ifFalse: [projectListPresenter selections].
	self updateServices: selections!

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
	| stonString stonResults |
	stonString := STON toString: services.
	stonResults := self gciSession serverPerform: #updateFromSton: with: stonString.
	self update: services afterStonReplication: stonResults!

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
!RowanProjectsBrowserPresenter categoriesFor: #basicRemoveMethods!menu handlers!private! !
!RowanProjectsBrowserPresenter categoriesFor: #checkout!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #cloneProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #commit!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createComponents!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiring!create schemantic wiring!overrides!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForClassList!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForInstanceClassTabs!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForMethodCategoryList!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForMethodList!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForPackageDictionaryTabs!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForPackageList!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForProjectDictionaryList!create schemantic wiring!overrides!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createSchematicWiringForProjectList!create schemantic wiring!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createStateEntry!accessing!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #editSaveMethod!menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #getViews!public! !
!RowanProjectsBrowserPresenter categoriesFor: #initialize!initialization!public! !
!RowanProjectsBrowserPresenter categoriesFor: #inspectProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #isDictionariesTabSelected!public!testing! !
!RowanProjectsBrowserPresenter categoriesFor: #isProjectListTabSelected!public!testing! !
!RowanProjectsBrowserPresenter categoriesFor: #loadProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #makeDefault!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #methodSourceEqual!public!testing! !
!RowanProjectsBrowserPresenter categoriesFor: #newService!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #onViewOpened!menus!overrides!public! !
!RowanProjectsBrowserPresenter categoriesFor: #projectChanges!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #projectDictionaryChanged!event handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #projectLog!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #projectMenuStrings!menus!public! !
!RowanProjectsBrowserPresenter categoriesFor: #pullFromGit!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #pushToGit!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #refresh!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #registerClassData!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #registerPresentersForUpdates!event handlers!initialization!public!register presenters! !
!RowanProjectsBrowserPresenter categoriesFor: #removeMethodServices:!public!ston! !
!RowanProjectsBrowserPresenter categoriesFor: #setClassMeta!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #setMethodFilter!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #showPackages!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #textTabChanged!event handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #unloadProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #update:afterStonReplication:!public!ston! !
!RowanProjectsBrowserPresenter categoriesFor: #updateAndSelectA:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClass!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClasses!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClassInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateCommand:onSuccessDo:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateMethodSource!event handlers!public!updating! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 656 nil nil nil 5 nil nil nil 656 482 170 176 8 #() false 170 192 752 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1075 1 898 711 539 656 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 2 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 656 34 2 8 1409286144 131073 1008 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 517 nil nil nil 1008 655878 ##(Smalltalk.CardLayout) 138 144 34 3 721414 ##(Smalltalk.Association) 8 'Categories' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 1008 34 2 8 1140850688 131073 1264 nil 1074 1104 nil 5 nil nil nil 1264 1136 170 192 34 6 1232 8 'categoryList' 410 ##(Smalltalk.ListView) 34 30 nil 1264 34 2 8 1409372233 1025 1392 590662 2 ##(Smalltalk.ListModel) 138 144 752 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 1074 8 4278190080 nil 21 nil nil nil 1392 nil 8 4294908191 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 283 #left 1586 #displayString 1616 ##(Smalltalk.SortedCollection) nil nil 1392 nil 3 nil 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 9 1 1760 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 1776 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 291 487 1392 850 #text: 34 1 8 'Column 1' 1392 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 898 193 193 nil 27 8 'variableList' 410 ##(Smalltalk.ListView) 34 30 nil 1264 34 2 8 1409372233 1025 2144 1458 138 144 752 nil 1520 1074 1552 nil 21 nil nil nil 2144 nil 8 4294908191 1586 #displayString 1616 nil 1648 nil nil nil nil nil nil 138 144 34 1 1698 8 'Column 1' 283 #left 1586 #displayString 1616 ##(Smalltalk.SortedCollection) nil nil 2144 nil 3 nil 1762 0 nil 1794 9 1 1760 1824 1840 1856 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 2368 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 291 487 2144 850 #text: 34 1 8 'Column 1' 2144 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2112 nil 27 8 'pragmaList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 1008 34 2 8 1140916864 1 2656 1458 138 144 34 3 1216 8 'Variables' 8 'Pragmas' nil 1520 1074 1104 nil 1 nil nil nil 2656 nil 8 4294907417 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1648 nil nil nil nil nil #noIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 351 539 2656 850 #basicSelectionsByIndex: 34 1 8 #(1) 2656 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2656 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 8 #() 2112 nil 27 nil 1008 898 33 33 1049862 ##(Smalltalk.ButtonInteractor) 1264 nil 590342 ##(Smalltalk.Rectangle) 898 255 3 898 287 35 1 1180998 4 ##(Smalltalk.CommandDescription) #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 53 9 898 291 523 1264 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 171 0 0 0 9 1 0 0] 34 3 1232 1392 2144 2112 nil 27 34 2 8 1409372233 1025 1232 1458 138 144 752 nil 1520 1074 1552 nil 21 nil nil nil 1232 nil 8 4294908191 1586 #displayString 8 #() nil 1648 nil nil nil nil nil nil 138 144 34 1 1698 8 'Column 1' 283 #left 1586 #displayString 8 #() ##(Smalltalk.SortedCollection) 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each]' 1779525917 3616 7 257 nil nil 1232 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 291 487 1232 850 #text: 34 1 8 'Column 1' 1232 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2112 nil 27 1186 2768 1392 1186 2784 2144 1232 170 192 34 2 1264 8 'methodFilterTabs' nil 2656 1264 898 201 201 401 1 31 nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 351 539 1008 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 34 2 1264 2656 2112 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 4176 nil 1074 8 4278190080 nil 517 nil nil nil 4176 786 138 144 34 1 850 #createAt:extent: 34 2 898 351 1 898 11 539 4176 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 0 0 0 0 180 0 0 0 13 1 0 0] 8 #() 2112 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 4432 nil nil nil 5 nil nil nil 4432 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ComboBox) 34 17 nil 4432 34 2 8 1412498947 1025 4560 1458 138 144 752 nil 1520 1074 8 4278190080 nil 5 nil nil nil 4560 nil 8 4294905903 1586 #displayString 8 #() 752 401 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 493 898 351 53 4560 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 246 0 0 0 175 0 0 0 16 1 0 0] 8 #() 2112 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 4930 #fixedParentRight 1 4930 #fixedParentBottom -45 4930 #fixedViewTop 47 410 ##(Smalltalk.ListView) 34 30 nil 4432 34 2 8 1409372233 1025 5008 1458 138 144 752 nil 1520 1074 1552 nil 13 nil nil nil 5008 nil 8 4294908191 1586 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1648 nil nil nil nil nil nil 138 144 34 2 1698 8 '' 41 #left 850 #empty 8 #() 16 1586 #<= 8 #() nil nil 5008 1762 0 nil 1794 2 1 1760 8 'doIt' 8 '[:each | each subSuperIcon]' 8 #[30 105 226 0 106] #subSuperIcon 5296 7 257 nil 1 nil nil 1698 8 'Selector' 311 #left 1586 #displayString 5152 ##(Smalltalk.SortedCollection) 1762 0 nil 1794 2 1 1760 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 5424 7 257 nil nil 5008 nil 3 nil 1762 0 nil 1794 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 5504 7 257 nil #report 752 nil 131173 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 47 898 351 447 5008 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 175 0 0 0 246 0 0 0] 8 #() 2112 nil 27 4898 4944 1 4960 1 4930 #fixedParentTop 47 4976 -45 410 ##(Smalltalk.ComboBox) 34 17 nil 4432 34 2 8 1412498947 1025 5808 1458 138 144 752 nil 1520 1074 4672 nil 5 nil nil nil 5808 nil 8 4294905903 1586 #displayString 8 #() 752 401 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 351 53 5808 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 26 0 0 0] 8 #() 2112 nil 27 4898 4944 1 4960 1 5792 1 4992 47 170 192 34 6 4560 8 'overrideList' 5008 8 'methodList' 5808 8 'superclassList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 361 1 898 351 539 4432 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 99 1 0 0 13 1 0 0] 34 3 5808 5008 4560 2112 nil 27 2112 nil 27 5 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 6384 nil nil nil 5 nil nil nil 6384 482 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 6384 34 2 8 1140850688 131073 6496 nil nil nil 5 nil nil nil 6496 4498 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 6496 34 2 8 1409286144 131073 6608 nil 1074 8 4278190080 nil 5 nil nil nil 6608 1122 138 144 34 2 1186 590662 1 ##(Smalltalk.CardLabel) 8 'Instance' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6768 6800 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 6816 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 6608 34 2 8 1140850688 131073 6912 nil nil nil 5 nil nil nil 6912 nil 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 9 898 389 1 6912 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 198 0 0 0 4 0 0 0] 8 #() 2112 nil 27 1186 6770 8 'Class' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6768 7184 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 7200 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 6608 34 2 8 1140850688 131073 7296 nil nil nil 5 nil nil nil 7296 nil 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 9 898 389 1 7296 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 198 0 0 0 4 0 0 0] 8 #() 2112 nil 27 6912 170 192 34 4 7296 8 'classTab' 6912 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 6608 34 2 8 1140916738 1 7600 1458 138 144 34 2 6784 7168 nil 1520 nil nil 1 nil nil nil 7600 nil 8 4294907417 1762 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 7728 7 257 nil 1762 0 nil 7746 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 7792 7 257 nil 1648 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 405 51 7600 850 #basicSelectionsByIndex: 34 1 8 #(1) 7600 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 7600 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 25 0 0 0] 8 #() 2112 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 489 898 405 51 6608 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 202 0 0 0 13 1 0 0] 34 3 6912 7296 7600 2112 nil 27 4898 4944 1 4960 1 4976 -49 4992 51 410 ##(Smalltalk.CardContainer) 34 16 nil 6496 34 2 8 1409286144 131073 8256 nil 1074 8 4278190080 nil 5 nil nil nil 8256 1122 138 144 34 2 1186 6770 8 'Classes' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6768 8432 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 8448 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 8256 34 2 8 1409372237 1025 8544 1458 138 144 752 nil 1520 1074 8 4278190080 nil 29 nil nil nil 8544 nil 8 4294908191 1586 #displayString 8 #() nil 1648 nil nil nil nil nil nil 138 144 34 1 1698 8 '' 373 #left 1586 #displayString 8 #() 1586 #<= 8 #() nil nil 8544 nil 3 1762 0 nil 1794 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 8848 7 257 nil 1762 0 nil 1794 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 8928 7 257 nil #report 752 nil 133217 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 55 898 389 433 8544 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 198 0 0 0 243 0 0 0] 8 #() 2112 nil 27 1186 6770 8 'Hierarchy' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6768 9232 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 9248 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 8256 34 2 8 1409352231 1025 9344 590918 3 ##(Smalltalk.TreeModel) nil 1520 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 752 1074 8 4278190080 nil 29 nil nil nil 9344 nil 8 4294907959 1762 0 nil 1794 2 1 1584 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 9536 7 257 nil ##(Smalltalk.IconicListAbstract) 1648 nil nil nil nil nil 170 176 752 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 752 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 55 898 389 433 9344 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 198 0 0 0 243 0 0 0] 8 #() 2112 nil 27 8544 170 192 34 4 8544 8 'classList' 9344 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 8256 34 2 8 1140916736 1 9872 1458 138 144 34 2 8416 9216 nil 1520 nil nil 1 nil nil nil 9872 nil 8 4294907417 1762 0 nil 7746 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 10000 7 257 nil 1762 0 nil 7746 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 10048 7 257 nil 1648 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 405 495 9872 850 #basicSelectionsByIndex: 34 1 8 #(1) 9872 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 9872 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 247 0 0 0] 8 #() 2112 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 405 495 8256 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 202 0 0 0 247 0 0 0] 34 3 8544 9344 9872 2112 nil 27 4898 4944 1 4960 1 5792 1 4976 -43 170 192 34 4 6608 8 'instanceClassTabs' 8256 8 'classHierarchyTabs' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 661 1 898 405 539 6496 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 74 1 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 2 8256 6608 2112 nil 27 11 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 6384 34 2 8 1409286144 131073 10736 nil 1074 8 4278190080 nil 517 nil nil nil 10736 1122 138 144 34 2 1186 6770 8 'Dictionaries' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Dictionaries'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6768 10912 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 10928 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 10736 34 2 8 1140850688 131073 11056 nil 1074 10816 nil 5 nil nil nil 11056 10832 170 192 34 4 410 ##(Smalltalk.ListView) 34 30 nil 11056 34 2 8 1409405001 1025 11168 1458 138 144 752 nil 1520 1074 8656 nil 5 nil nil nil 11168 nil 8 4294908191 1586 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1648 nil nil nil nil nil nil 138 144 34 1 1698 8 'Column 1' 297 #left 1586 #displayString 11312 ##(Smalltalk.SortedCollection) 1762 0 nil 1794 2 1 1760 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 11408 7 257 nil nil 11168 nil 3 nil 1762 0 nil 1794 3 1 1760 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 11488 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 1 898 305 395 11168 850 #text: 34 1 8 'Column 1' 11168 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 152 0 0 0 197 0 0 0] 8 #() 2112 nil 27 8 'projectList' 11024 8 'dictionaryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 10736 34 2 8 1140916736 1 11840 1458 138 144 34 2 6770 8 'Projects' 1762 0 nil 1794 4 1 1760 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 11984 7 1 nil nil 10896 nil 1520 1074 10816 nil 1 nil nil nil 11840 nil 8 4294907417 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1648 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 321 539 11840 850 #basicSelectionsByIndex: 34 1 8 #(1) 11840 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 11840 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 8 #() 2112 nil 27 nil 10736 898 33 33 3090 11056 nil 3122 898 269 397 898 301 429 1 3186 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 101 898 305 431 11056 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 50 0 0 0 156 0 0 0 9 1 0 0] 34 2 11168 11024 2112 nil 27 34 2 8 1409372233 1025 11024 1458 138 144 752 nil 1520 1074 8656 nil 21 nil nil nil 11024 nil 8 4294908191 1586 #displayString 8 #() nil 1648 nil nil nil nil nil nil 138 144 34 1 1698 8 'Column 1' 297 #left 1586 #displayString 12768 ##(Smalltalk.SortedCollection) 1762 0 nil 1794 2 1 1760 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 12864 7 257 nil nil 11024 nil 3 nil 1762 0 nil 1794 5 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] #item #font #beBold #font: 12944 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 1 898 305 395 11024 850 #text: 34 1 8 'Column 1' 11024 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 152 0 0 0 197 0 0 0] 8 #() 2112 nil 27 1186 11952 11168 11168 170 192 34 2 11056 8 'projectDictionaryTabs' nil 11840 11056 898 201 201 401 1 29 nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 321 539 10736 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 34 2 11056 11840 2112 nil 27 9 410 ##(Smalltalk.ContainerView) 34 15 nil 6384 34 2 8 1140850688 131073 13504 nil nil nil 5 nil nil nil 13504 482 170 176 752 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 13504 34 2 8 1140936777 1025 13632 1458 138 144 752 nil 1520 1074 8 4278190080 nil 21 nil nil nil 13632 nil 8 4294908191 1762 0 nil 1794 1 83886081 1760 8 'doIt' 8 '[:each |  each].' 1779525917 13776 7 257 nil ##(Smalltalk.IconicListAbstract) 1648 nil nil nil nil nil nil 138 144 34 1 1698 8 'Column 1' 305 #left 1762 0 nil 1794 2 1 1760 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 13904 7 257 nil 1586 #<= 8 #() nil nil 13632 nil 3 nil 1762 0 nil 1794 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 14016 7 257 nil #report 752 nil 131185 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 1 898 321 539 13632 850 #text: 34 1 8 'Column 1' 13632 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 8 #() 2112 nil 27 8 'packagesList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 331 1 898 321 539 13504 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 0 0 0 0 69 1 0 0 13 1 0 0] 34 1 13632 2112 nil 27 9 false 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 1065 539 6384 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 5 10736 410 ##(Smalltalk.Splitter) 34 12 nil 6384 34 2 8 1140850688 1 14688 nil 1074 4256 nil 517 nil nil nil 14688 786 138 144 34 1 850 #createAt:extent: 34 2 898 321 1 898 11 539 14688 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 0 0 0 0 165 0 0 0 13 1 0 0] 8 #() 2112 nil 27 13504 410 ##(Smalltalk.Splitter) 34 12 nil 6384 34 2 8 1140850688 1 14928 nil 1074 4256 nil 517 nil nil nil 14928 786 138 144 34 1 850 #createAt:extent: 34 2 898 651 1 898 11 539 14928 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 69 1 0 0 0 0 0 0 74 1 0 0 13 1 0 0] 8 #() 2112 nil 27 6496 2112 nil 27 7 false 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 1785 539 544 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 6384 410 ##(Smalltalk.Splitter) 34 12 nil 544 34 2 8 1140850688 1 15344 nil 1074 4256 nil 517 nil nil nil 15344 786 138 144 34 1 850 #createAt:extent: 34 2 898 1065 1 898 11 539 15344 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 2 0 0 0 0 0 0 25 2 0 0 13 1 0 0] 8 #() 2112 nil 27 656 2112 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 15616 nil 1074 8336 nil 5 nil nil nil 15616 1122 138 144 34 8 1186 6770 8 'Package' 1762 0 nil 1794 7 1 6768 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6768 15776 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 15792 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 15616 34 2 8 1140850688 131073 15888 nil nil nil 5 nil nil nil 15888 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 15888 34 2 8 1409355853 1025 16000 1458 138 144 752 nil 1520 1074 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 3186 #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 16000 nil 8 4294908191 1586 #displayString 8 #() nil 1648 nil nil nil nil nil nil 138 144 34 4 1698 8 'Ancestor' 131 #left 1586 #displayString 8 #() 1586 #<= 16384 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 16416 7 257 nil nil 16000 nil 1 nil nil 1698 8 'Name' 401 #left 1586 #displayString 16288 ##(Smalltalk.SortedCollection) 1762 0 nil 1794 1 83886081 1760 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 16544 7 257 nil nil 16000 nil 1 nil nil 1698 8 'Timestamp' 301 #left 1586 #displayString 8 #() 1586 #<= 16672 1762 0 nil 1794 1 83886081 1760 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 16704 7 257 nil nil 16000 nil 1 nil nil 1698 8 'Message' 941 #left 1586 #displayString 16672 1586 #<= 16672 1762 0 nil 1794 1 83886081 1760 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 16848 7 257 nil nil 16000 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 1769 263 16000 850 #contextMenu: 34 1 16144 16000 850 #text: 34 1 8 'Ancestor' 16000 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 2112 nil 27 23 410 ##(Smalltalk.ListView) 34 30 nil 15888 34 2 8 1409355853 1025 17200 1458 138 144 752 nil 1520 1074 16112 nil 5 16130 nil true 34 5 16178 1 3186 #addRepository 8 'Add &Repository' 1 1 nil nil nil 16178 1 3186 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 16178 1 3186 #savePackage 8 '&Save Package' 1 1 nil nil nil 16178 1 3186 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 17200 nil 8 4294908191 1586 #displayString 8 #() nil 1648 nil nil nil nil nil nil 138 144 34 4 1698 8 'Repository Type' 211 #left 1586 #displayString 17616 ##(Smalltalk.SortedCollection) 1762 0 nil 1794 2 1 1760 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 17712 7 257 nil nil 17200 nil 1 nil nil 1698 8 'Description' 1161 #left 1586 #displayString 16672 1586 #<= 16672 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 17856 7 257 nil nil 17200 nil 3 nil nil 1698 8 'User' 201 #left 1586 #displayString 16384 1586 #<= 16384 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 18000 7 257 nil nil 17200 nil 1 nil nil 1698 8 'Password' 201 #left 1586 #displayString 16384 1586 #<= 16384 1762 0 nil 1794 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 18144 1762 0 nil 18160 19 257 nil 7 257 nil nil 17200 nil 1 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 263 898 1769 217 17200 850 #contextMenu: 34 1 17312 17200 850 #text: 34 1 8 'Repository Type' 17200 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 2112 nil 27 19 true 170 192 34 4 16000 8 'ancestorList' 17200 8 'repositoryList' nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 9 55 898 1769 479 15888 850 #text: 34 1 8 'Package' 15888 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 34 2 16000 17200 2112 nil 27 1186 6770 8 'Globals' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6768 18816 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 18832 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 15616 34 2 8 1409355853 1025 18928 1458 138 144 752 nil 1520 1074 16112 nil 5 nil nil nil 18928 nil 8 4294908191 1586 #displayString 8 #() nil 1648 nil nil nil nil nil nil 138 144 34 3 1698 8 'Name' 587 #left 1586 #displayString 19072 ##(Smalltalk.SortedCollection) 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 19168 7 257 nil nil 18928 nil 3 nil nil 1698 8 'Class' 589 #left 1586 #displayString 16672 1586 #<= 16672 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 19312 7 257 nil nil 18928 nil 3 nil nil 1698 8 'Value' 587 #left 1586 #displayString 16672 1586 #<= 16672 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 19456 7 257 nil nil 18928 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 9 55 898 1769 479 18928 850 #text: 34 1 8 'Name' 18928 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2112 nil 27 1186 6770 8 'Original Source' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6768 19808 ##(Smalltalk.Icon) 8 'COMPILEDMETHOD_PRIVATE.ico' #fromId: #text:iconBlock: 19824 11 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15616 34 2 8 1445007428 1025 19920 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 1074 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 19920 nil 8 4294908209 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20258 77 786694 ##(Smalltalk.IndexedColor) 33554457 20306 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 20272 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20288 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 752 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 19920 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 20306 33554447 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 20272 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 19920 65025 3 false 1 nil nil nil nil nil nil 3 20722 3 19920 33423361 5 false 3 nil nil nil nil nil nil 5 20722 5 19920 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20258 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20258 1027 20834 60239 20834 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20258 1031 20834 335 20834 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 7 850 #createAt:extent: 34 2 898 9 55 898 1769 479 19920 850 #modificationEventMask: 8 #(4607) 19920 850 #caretPeriod: 8 #(530) 19920 850 #wordWrap: 8 #(true) 19920 850 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 19920 61 3 nil nil 5 nil 21218 3 19920 1 nil 3 67108863 5 nil 21218 5 19920 1 nil 3 -67108863 5 nil 19920 850 #tabWidth: 8 #(4) 19920 850 #maxCompletionListHeight: 8 #(9) 19920 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2112 nil 29 1186 6770 8 'SUnit' 1762 0 nil 1794 4 1 1760 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 12066 #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 21440 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 15616 34 2 8 1140850688 131073 21552 nil 1074 8 4278190080 nil 5 nil nil nil 21552 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 55 898 1769 479 21552 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 752 2112 nil 27 1186 6770 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15616 34 2 8 1445007428 1025 21872 19986 nil false 20032 nil 1074 8 4278190080 nil 5 16130 nil true 34 11 16178 1 3186 #editSave 8 '&Save' 9383 1 nil nil nil 17442 4097 16178 1 3186 #editUndo 8 '&Undo' 9397 1 nil nil nil 16178 1 3186 #editRedo 8 '&Redo' 9395 1 nil nil nil 17442 4097 16178 1 3186 #editCut 8 'Cu&t' 9393 1 nil nil nil 16178 1 3186 #editCopy 8 '&Copy' 9351 1 nil nil nil 16178 1 3186 #editPaste 8 '&Paste' 9389 1 nil nil nil 16178 1 3186 #editDelete 8 '&Delete' 1629 1 nil nil nil 17442 4097 16178 1 3186 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 20082 nil true 20114 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 21872 nil 8 4294908209 20194 nil nil 13 nil 170 192 34 4 #normal 20258 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20258 77 20320 20336 1 nil nil nil nil #callTip nil nil nil 34 256 22592 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 22608 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 752 170 192 34 2 #default 20450 1 nil nil nil 21872 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 20512 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 22592 nil nil nil nil 1 nil 170 192 34 6 1 20722 1 21872 65025 3 false 1 nil nil nil nil nil nil 3 20722 3 21872 33423361 5 false 3 nil nil nil nil nil nil 5 20722 5 21872 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20258 1029 nil 20834 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20258 1027 20834 60239 20834 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20258 1031 20834 335 20834 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 8 850 #createAt:extent: 34 2 898 9 55 898 1769 479 21872 850 #contextMenu: 34 1 21984 21872 850 #modificationEventMask: 8 #(4607) 21872 850 #caretPeriod: 8 #(530) 21872 850 #wordWrap: 8 #(true) 21872 850 #margins: 34 1 34 3 21218 1 21872 1 3 nil nil 5 nil 21218 3 21872 33 nil 3 67108863 5 nil 21218 5 21872 1 nil 3 -67108863 5 nil 21872 850 #tabWidth: 8 #(4) 21872 850 #maxCompletionListHeight: 8 #(9) 21872 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2112 nil 29 1186 6770 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15616 34 2 8 1445007428 1025 23664 19986 nil false 20032 nil 1074 21968 nil 5 16130 nil true 34 1 16178 1 3186 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 20082 nil true 20114 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 23664 nil 8 4294908209 20194 nil nil 11 nil 170 192 34 4 #normal 20258 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20258 77 20320 20336 1 nil nil nil nil #callTip nil nil nil 34 256 23984 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 24000 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 20450 1 nil nil nil 23664 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 20512 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 23984 nil nil nil nil 1 nil 170 192 34 6 1 20722 1 23664 65025 3 false 1 nil nil nil nil nil nil 3 20722 3 23664 33423361 5 false 3 nil nil nil nil nil nil 5 20722 5 23664 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20258 1029 nil 20834 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20258 1027 20834 60239 20834 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20258 1031 20834 335 20834 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 8 850 #createAt:extent: 34 2 898 9 55 898 1769 479 23664 850 #contextMenu: 34 1 23760 23664 850 #modificationEventMask: 8 #(4607) 23664 850 #caretPeriod: 8 #(530) 23664 850 #wordWrap: 8 #(true) 23664 850 #margins: 34 1 34 3 21218 1 23664 1 3 nil nil 5 nil 21218 3 23664 33 nil 3 67108863 5 nil 21218 5 23664 1 nil 3 -67108863 5 nil 23664 850 #tabWidth: 8 #(4) 23664 850 #maxCompletionListHeight: 8 #(9) 23664 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2112 nil 29 1186 6770 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15616 34 2 8 1445007428 1025 24960 19986 nil false 20032 nil 1074 8 4278190080 nil 5 nil 20082 nil true 20114 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 24960 nil 8 4294908209 20194 nil nil 13 nil 170 192 34 4 #normal 20258 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20258 77 20320 20336 1 nil nil nil nil #callTip nil nil nil 34 256 25200 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 25216 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20370 1 nil nil false 138 144 752 170 192 34 2 #default 20450 1 nil nil nil 24960 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 20512 nil nil 65 nil nil 22768 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 25200 #callTip 25216 nil nil nil nil 1 nil 170 192 34 12 1 20722 1 24960 65025 3 false 1 nil nil nil nil nil nil 3 20722 3 24960 33423361 5 false 3 nil nil nil nil nil nil 5 20722 5 24960 511 1 false 5 nil nil nil nil nil nil 8 'indicator9' 20722 19 24960 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator10' 20722 21 24960 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator8' 20722 17 24960 33554447 1 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20258 1029 nil 20834 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20258 1027 20834 60239 20834 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20258 1031 20834 335 20834 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 7 850 #createAt:extent: 34 2 898 9 55 898 1769 479 24960 850 #modificationEventMask: 8 #(4607) 24960 850 #hoverTime: 8 #(200) 24960 850 #caretPeriod: 8 #(530) 24960 850 #margins: 34 1 34 3 21218 1 24960 61 3 nil nil 5 nil 21218 3 24960 1 nil 3 67108863 nil nil 21218 5 24960 1 nil 3 -67108863 nil nil 24960 850 #tabWidth: 8 #(4) 24960 850 #maxCompletionListHeight: 8 #(9) 24960 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2112 nil 29 1186 6770 8 'Project' 1762 0 nil 1794 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 12066 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 26240 7 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 15616 34 2 8 1409372237 1025 26352 1458 138 144 752 nil 1520 1074 16112 nil 5 nil nil nil 26352 nil 8 4294908191 1586 #displayString 19072 nil 1648 nil nil nil nil nil nil 138 144 34 2 1698 8 'Name' 201 #left 1586 #displayString 19072 ##(Smalltalk.SortedCollection) 1762 0 nil 1794 1 83886081 1760 8 'doIt' 8 '[:each | (each at: 2)]' 8 #[29 105 17 64 148 106] 26576 7 257 nil nil 26352 nil 1 nil 1762 0 nil 1794 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each item at: 1) displayStringFor: each]' 8 #[31 105 226 0 63 148 17 177 106] #item #displayStringFor: 26656 7 257 nil 1698 8 'Value' 1561 #left 1586 #displayString 8 #() 1586 #<= 26784 1762 0 nil 1794 4 1 1760 8 'doIt' 8 '[:each | (each at: 2) ifNil:[String new] ifNotNil: [:iv |
(each at: 1) instVarNamed: iv]]' 8 #[32 105 17 64 148 100 223 4 97 45 146 106 90 17 63 148 18 178 106] 12066 #String 16 #ifNil:ifNotNil: #instVarNamed: 26816 7 65793 nil nil 26352 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 9 55 898 1769 479 26352 850 #text: 34 1 8 'Name' 26352 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2112 nil 27 26352 170 192 34 16 15888 8 'packageInfo' 19920 8 'originalSource' 23664 8 'classDocumentation' 18928 8 'globals' 21872 8 'classDefinition' 21552 8 'sunit' 26352 8 'projectInfo' 24960 8 'methodSource' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 15616 34 2 8 1140916736 1 27312 1458 138 144 34 8 26208 15760 18800 21840 23632 19792 24928 21408 nil 1520 nil nil 1 nil nil nil 27312 nil 8 4294907417 1762 0 nil 7746 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 27440 7 257 nil 1762 0 nil 7746 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 27488 7 257 nil 1648 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 1785 541 27312 850 #basicSelectionsByIndex: 34 1 8 #(1) 27312 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 27312 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 14 1 0 0] 8 #() 2112 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 549 898 1785 541 15616 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 1 0 0 124 3 0 0 32 2 0 0] 34 9 26352 15888 18928 21872 23664 19920 24960 21552 27312 2112 nil 27 15586 3 5 true 170 192 34 4 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 27984 nil 1074 4256 nil 517 nil nil nil 27984 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 539 898 1785 11 27984 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 13 1 0 0 124 3 0 0 18 1 0 0] 8 #() 2112 nil 27 8 'splitter' 15616 8 'textAreaTabs' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 7679 21 898 1785 1089 416 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 123 18 0 0 42 2 0 0] 34 3 544 27984 15616 2112 nil 27 )! !
!RowanProjectsBrowserPresenter class categoriesFor: #downArrowIcon!public! !
!RowanProjectsBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

