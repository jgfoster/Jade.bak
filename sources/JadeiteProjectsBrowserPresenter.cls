"Filed out from Dolphin Smalltalk 7"!

RowanSystemBrowserPresenter subclass: #JadeiteProjectsBrowserPresenter
	instanceVariableNames: 'projectListPresenter stonCritical projectInfoTab projectLabel projectDictionaryTabs comparisonPresenter comparisonTab'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteProjectsBrowserPresenter guid: (GUID fromString: '{258266f3-948d-47d3-afa6-c6d0c941ddc5}')!
JadeiteProjectsBrowserPresenter comment: 'In packages pane, show projects & packages. Remove class categories.'!
!JadeiteProjectsBrowserPresenter categoriesForClass!Unclassified! !
!JadeiteProjectsBrowserPresenter methodsFor!

addPackage
	MessageBox notify: 'This feature is not yet available'!

addPackageDictionaryInfoTo: aStream

	"Private - projects don't write themselves to string"!

addSubclass
	| classService answerService |
	classService := classListPresenter selectionOrNil ifNil: [^self].
	classService
		command: #subclassCreationTemplate;
		commandArgs: Array new. 
	answerService := (self issueCommand: (Array with: classService)) first.
	classDefinitionPresenter
		ensureVisible;
		value: answerService answer!

allClasses
	| browserService |
	browserService := RowanBrowserService new.
	browserService
		command: #allClasses;
		commandArgs: Array new.
	self issueCommand: (Array with: browserService).
	^browserService allClasses!

basicAddMethodCategory: string
	| service |
	service := classListPresenter selection
				command: #addCategory:;
				commandArgs: (Array with: string).
	self issueCommand: (Array with: service)!

basicRemoveMethods
	| service |
	service := classListPresenter selectionIfNone: [^self].
	self setClassMeta.
	self removeMethods: (Array with: service)!

browseClassReferences
	| service className |
	className := (classListPresenter selectionIfNone: [^self]) name.
	service := RowanQueryService command: #browseClassReferences: withArgs: (Array with: className).
	self issueCommand: (Array with: service).
	self openMethodListOn: service selecting: className!

browseHierarchyImplementors

	self browseHierarchyImplementorsOf: self selectedMethodName.
!

browseHierarchyImplementorsOf: string
	| service className |
	className := (classListPresenter selectionIfNone: [^self]) name.
	service := RowanQueryService command: #hierarchyImplementorsOf:inClass:
				withArgs: (Array with: string asSymbol with: className).
	self issueCommand: (Array with: service).
	self openMethodListOn: service selecting: string!

browseHierarchySenders

	self browseHierarchySendersOf: self selectedMethodName.
!

browseHierarchySendersOf: string
	| service className |
	className := (classListPresenter selectionIfNone: [^self]) name.
	service := RowanQueryService command: #hierarchySendersOf:inClass:
				withArgs: (Array with: string asSymbol with: className).
	self issueCommand: (Array with: service).
	self openMethodListOn: service selecting: string!

browseImplementorsOf: string
	| service |
	service := RowanQueryService command: #implementorsOf: withArgs: (Array with: string asSymbol).
	self issueCommand: (Array with: service).
	self openMethodListOn: service selecting: string!

browseMethodHistory

	MessageBox warning: 'Method history is not available yet'. !

browseMethodsContaining

	| string service |
	(string := Prompter prompt: 'Enter string:') isNil ifTrue: [^self].
	service := RowanQueryService command: #methodsContaining:
				withArgs: (Array with: string).
	self issueCommand: (Array with: service).
	self openMethodListOn: service selecting: string
	
!

browseSendersOf: string
	| service |
	service := RowanQueryService command: #sendersOf: withArgs: (Array with: string asSymbol).
	self issueCommand: (Array with: service).
	self openMethodListOn: service selecting: string!

checkout
	| service branchName |
	projectListPresenter hasSelection
		ifFalse: 
			[MessageBox notify: 'No project selected'.
			^false].
	service := (RowanProjectService on: gciSession) name: projectListPresenter selection name.
	branchName := ChoicePrompter choices: service branches caption: 'Select branch to checkout:'.
	branchName isNil ifTrue: [^false] ifFalse: [service checkout: branchName]!

classesMenuStrings

	false ifTrue: [
		self browseClassReferences; fileOutClass; addSubclass; addMissingAccessors; removeClass; removePriorVersions; runClassTests.
	].
	^#(
		'&Classes'
		'&Browse References//browseClassReferences'
		'&File Out Class//fileOutClass'
		'Add &Subclass//addSubclass'
		'Add &Missing Accessors//addMissingAccessors'
		'&Remove//removeClass'
		'Remove Prior &Versions//removePriorVersions'
		'-'
		'Set Compiler &Environment ...//setEnvironment'
		'Run &Tests//runClassTests'
		'-'
		'Go To Defining Package//gotoClassPackage'
	).
!

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

comparisonTab
	^comparisonTab
		ifNil: 
			[comparisonTab := self textTabs
						detect: [:each | (each class canUnderstand: #text) ifTrue: [each text = 'Comparison'] ifFalse: [each = 'Comparison']]]!

createComponents
	super createComponents.
	projectListPresenter := self add: ListPresenter new name: 'projectList'.
	packageListPresenter := self add: ListPresenter new name: 'packagesList'.
	comparisonPresenter := self add: DifferencesPresenter new name: 'comparisonTab'.!

createSchematicWiring
	super createSchematicWiring.
	self
		createSchematicWiringForProjectList;
		createSchematicWiringForMethodFilterTabs!

createSchematicWiringForClassList
	classListPresenter
		when: #drag:
			send: #onDragClassList:
			to: self;
		when: #dragCut:
			send: #onDragCutClassList:
			to: self;
		when: #dragOver:
			send: #onDragOverClassList:
			to: self;
		when: #drop:
			send: #onDropOnClassList:
			to: self;
		when: #keyTyped:
			send: #delayUpdate
			to: self;
		when: #selectionChanging:
			send: #aboutToChange:
			to: self;
		when: #selectionChanged
			send: #selectedClassChanged:
			to: self
			with: true;
		when: #selectionChanged
			send: #refreshClass
			to: self;
		when: #labelOf:changedTo:
			send: #labelOfClass:changedTo:
			to: self;
		when: #aboutToEditLabel:accept:
			send: #aboutToEditClassLabel:accept:
			to: self;
		when: #labelOf:editedTo:accept:
			send: #labelOfClass:editedTo:accept:
			to: self!

createSchematicWiringForInstanceClassTabs
	instanceClassTabs
		when: #currentCardChanging:
			send: #aboutToChange:
			to: self;
		when: #currentCardChanged
			send: #refreshClass
			to: self!

createSchematicWiringForMethodCategoryList
	categoryListPresenter
		when: #dragOver:
			send: #onDragOverMethodCategory:
			to: self;
		when: #drop:
			send: #onDropOnMethodCategory:
			to: self;
		when: #keyTyped:
			send: #delayUpdate
			to: self;
		when: #selectionChanging:
			send: #aboutToChange:
			to: self;
		when: #selectionChanged
			send: #updateTabLabel
			to: self;
		when: #selectionChanged
			send: #refreshClass
			to: self;
		yourself!

createSchematicWiringForMethodFilterTabs
	methodFilterTabs
		when: #currentCardChanged
		send: #filterTabChanged
		to: self!

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

createSchematicWiringForVariableList

	variableListPresenter
		when: #'selectionChanged'		send: #refreshClass	to: self;
		yourself.
!

createStateEntry

	^projectDictionaryTabs currentCard == projectListPresenter view!

editSaveClass
	| packageService visibleClasses |
	packageService := packageListPresenter selection.
	packageService
		command: #compileClass:;
		commandArgs: (Array with: classDefinitionPresenter value).
	visibleClasses := classListPresenter list.
	classListPresenter clear.
	[self issueCommand: (Array with: packageService)] on: GsCompileError
		do: 
			[:ex |
			classListPresenter list: visibleClasses.
			^self handleClassCompileError: ex].
	classDefinitionPresenter view isModified: false!

editSaveClassComment
	| classService |
	classService := classListPresenter selection.
	classService
		command: #classComment:;
		commandArgs: (Array with: classCommentPresenter value trimBlanks).
	self issueCommand: (Array with: classService)!

editSaveMethod
	self editSaveMethod: classListPresenter selection!

editSaveMethod: classService
	| caretPosition firstVisibleLine category |
	caretPosition := methodSourcePresenter view caretPosition.
	firstVisibleLine := methodSourcePresenter view lineScroll.
	category := (self isCategoriesTabSelected and: [categoryListPresenter hasSelection])
				ifTrue: [categoryListPresenter selection trimBlanks].
	classService
		command: #saveMethodSource:category:;
		commandArgs: (Array with: self currentMethodSource with: category).
	[self issueCommand: (Array with: classService)] on: GsCompileError
		do: [:ex | ^self reportCompileError: ex list].
	self selectMethodsFrom: classService.
	methodSourcePresenter view firstVisibleLine: firstVisibleLine.
	methodSourcePresenter view goto: caretPosition!

filterTabChanged
	categoryListPresenter resetSelection.
	self refreshClass!

findClass
	| find |
	find := JadeiteFindClassDialog showModal: 'DefaultView' on: self allClasses.
	find ifNotNil: [self updateAfterFindClass: find]!

getViews
	classHierarchyTabs := self view viewNamed: 'classHierarchyTabs'.
	instanceClassTabs := self view viewNamed: 'instanceClassTabs'.
	methodFilterTabs := self view viewNamed: 'methodFilterTabs'.
	textAreaTabs := self view viewNamed: 'textAreaTabs'.
	projectInfoTab := self view viewNamed: 'projectInfo'!

gitCommit
	| projectName message service promptResult |
	projectListPresenter hasSelection ifFalse: [MessageBox notify: 'No project selected'].
	projectListPresenter selections size > 1
		ifTrue: [MessageBox notify: 'Only one project may be committed'].
	projectName := projectListPresenter selection name.
	message := String new asValue.
	promptResult := StsMultilinePrompter
				on: message
				prompt: 'Enter Git commit message. (Note - This will NOT do a GemStone commit)'
				caption: 'Committing Project ' , projectName.
	promptResult isNil ifTrue: [^self].
	message value isEmpty
		ifTrue: [^MessageBox notify: 'Commit message must not be empty. Commit to local repository NOT done.'].
	service := projectListPresenter selection.
	service
		command: #commitWithMessage:;
		commandArgs: (Array with: message value asString).
	self issueCommand: (Array with: service).
	MessageBox notify: 'Project ' , projectName , ' committed to local repository!!'!

gotoClassPackage
	| newPackageSelection target |
	target := classListPresenter selection definedPackageName ifNil: [classListPresenter selection packageName].
	newPackageSelection := packageListPresenter list
				detect: [:svc | svc name = target].
	self selectPackage: newPackageSelection!

gotoMethodPackage
	| newPackageSelection |
	newPackageSelection := packageListPresenter list
				detect: [:svc | svc name = methodListPresenter selection packageName].
	self selectPackage: newPackageSelection!

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

methodsMenuStrings

	false ifTrue: [	"This helps us find senders"
		self browseImplementors; browseImplementorsOf; browseSenders; browseSendersOf; browseHierarchyImplementors; browseHierarchySenders; browseMethodsContaining; browseMethodPragmas; removeMethods; runMethodTests; setEnvironment0; setEnvironment1; setEnvironment2; fileOutMethod; gotoMethodPackage.
	].
	^#(
		'&Methods'
		'Browse &Implementors//browseImplementors'
		'Browse Implementors of ...//browseImplementorsOf'
		'Browse &Senders//browseSenders'
		'Browse Senders of ...//browseSendersOf'
		'-'
		'Browse Hierarchy Implementors//browseHierarchyImplementors'
		'Browse Hierarchy Senders//browseHierarchySenders'
		'-'
		'Browse Methods &Containing ...//browseMethodsContaining'
		'Browse &History//browseMethodHistory'
		'BrowseMethod &Pragmas ...//browseMethodPragmas'
		'&File out method ...//fileOutMethod'
		'-'
		'&Remove Method(s) ...//removeMethods'
		#('&Method Environment' '&0//setEnvironment0' '&1//setEnvironment1' '&2//setEnvironment2')
		'Run &Tests//runMethodTests'
		'Go To Defining Package//gotoMethodPackage'
	).
!

newService
	^(RowanProjectService on: gciSession) name: projectListPresenter selection name!

onDragMethod: aSession 

	| list |
	list := methodListPresenter selections collect: [:each | 
		(aSession newDragObject: each)
			format: #method data: each;
			yourself.
	].
	aSession 
		dragObjects: list;
		supportedOperations: #(#move #copy);
		defaultOperation: #copy;
		yourself.
!

onDropOnClassList: aSession
	| classService |
	classService := aSession suggestedTarget.
	self editSaveMethod: classService.
	classListPresenter selection == classService ifFalse: [self selectClass: classService].
	self selectMethodsFrom: classService!

onDropOnMethodCategory: aSession
	| methodServices targetCategory classService |
	methodServices := aSession dragObjects collect: [:each | each format: #method].
	targetCategory := aSession suggestedTarget trimBlanks.
	classService := classListPresenter selection.
	categoryListPresenter selections: (categoryListPresenter selections copyWith: targetCategory asSymbol).
	classService
		command: #moveMethods:to:;
		commandArgs: (Array with: methodServices with: targetCategory).
	self issueCommand: (Array with: classService).
	self selectMethodsFrom: classService!

onViewOpened
	"copied to RowanDebugger"
	super onViewOpened.
	self registerPresentersForUpdates. 
	projectListPresenter view contextMenu: (self projectMenu).
	self updateProjects. !

openDictionaryBrowserOn: classService
	| shell |
	Cursor wait showWhile: 
			[shell := RowanSystemBrowser showOnSession: gciSession.
			shell selectClass: classService name asString selector: String new]!

openMethodListOn: service selecting: string
	(JadeiteMethodListBrowser showOnSession: self gciSession) primaryPresenter
		methods: service queryResults
		selecting: string!

pkgsMenuStrings
	0 == 1
		ifTrue: 
			[self
				findClass;
				addPackage;
				savePackage;
				unloadPackage].
	^#('&Packages' 
	'&Find Class.../Ctrl+Shift+F/findClass'
	'&Add Package...//addPackage'
	'-' 
	'&Save Package...//savePackage'
	'&Unload Package...//unloadPackage')!

postOkToChangeEvent
	| service |
	service := classListPresenter selectionOrNil ifNil: [^self].
	classDefinitionPresenter value: service template.
	classDefinitionPresenter view
		backcolor: Color white;
		isModified: false!

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
	listModel := self textTabs.
	globalsLabel ifNil: [globalsLabel := listModel detect: [:each | each text = 'Globals']].
	self setDefaultProjectTablLabel.
	packageLabel ifNil: [packageLabel := listModel detect: [:each | each text = 'Package']].
	(listModel includes: projectLabel) ifFalse: [listModel addFirst: projectLabel].
	self updateProjectInfo.
	projectInfoTab ensureVisible.
	listModel remove: globalsLabel ifAbsent: [].
	listModel remove: packageLabel ifAbsent: []!

projectListPresenter
	^projectListPresenter!

projectLog
	| service |
	projectListPresenter selection isNil
		ifTrue: 
			[MessageBox notify: 'No project selected'.
			^false].
	service := (RowanProjectService on: gciSession) name: projectListPresenter selection name.
	MessageBox notify: service log!

projectMenu
	^Menu fromStrings: self projectMenuStrings!

projectMenuCommands
	^self projectMenu items collect: [:item | item command]!

projectMenuStrings
	self rowanFixMe. "this really is a cumbersome way to build a menu."
	0 == 1
		ifTrue: 
			[self
				findClass;
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
	^#(	'Projects' 
		'&Find Class.../Ctrl+Shift+F/findClass'
		'&MakeDefault//makeDefault' 
		'-' 
		'Clone Git Project ...//cloneProject'
		'Load//loadProject'
		'Unload//unloadProject'
		'-'
		'Pull from Git ...//pullFromGit'
		'Commit ...//gitCommit' 
		'Push to Git ...//pushToGit'
		'-'
		'&Changes ...//projectChanges'
		'Write ...//writeProject' 
		'Checkout ...//checkout'
		'-'
		'Log//projectLog'
		'&Inspect//inspectProject' 
		'&Refresh ...//refresh'
)!

projectTabLabel
	"Selections get mixed up switching between system presenters unless the tabs
	have a unique label. Rather silly, but we'll give the tab a unique number"

	| cardNumber label |
	label := (projectListPresenter selectionIfNone: [])
				ifNil: ['Projects']
				ifNotNil: 
					[:projectService |
					packageListPresenter selectionOrNil
						ifNil: [projectService name]
						ifNotNil: [:packageService | packageService name]].
	cardNumber := self parentPresenter view cards indexOf: self view.
	^cardNumber printString , ' ' , label!

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

queryCommand: aCommandQuery
	| command focusView isTextEdit |
	self rowanFixMe.	"enablement of method test thingy + this is a complicated way to method enablement. Would like to improve"
	command := aCommandQuery command.
	focusView := View focus.
	isTextEdit := focusView isKindOf: TextEdit.
	(#(#browseMethodPragmas) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: gciSession gsHasPragmas.
			^true].
	(#(#editSave) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: (isTextEdit and: [focusView isModified]).
			^true].
	(#(#editUndo) includes: aCommandQuery command)
		ifTrue: 
			[aCommandQuery isEnabled: (isTextEdit and: [focusView canUndo]).
			^true].
	(#(#editRedo) includes: aCommandQuery command)
		ifTrue: 
			[aCommandQuery isEnabled: (isTextEdit and: [focusView canRedo]).
			^true].
	(#(#editCut #editCopy) includes: aCommandQuery command)
		ifTrue: 
			[aCommandQuery isEnabled: (isTextEdit and: [focusView hasSelection]).
			^true].	"#'editDelete'"
	(#(#editPaste) includes: aCommandQuery command)
		ifTrue: 
			[aCommandQuery isEnabled: (isTextEdit and: [Clipboard current isTextAvailable]).
			^true].
	(#(#addRepository) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: packageListPresenter selections notEmpty.
			^true].
	(#(#showPackageChanges) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: true.
			^true].
	(#(#savePackage) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: packageListPresenter selections size = 1.
			^true].
	(#(#removeRepository) includes: command)
		ifTrue: 
			[aCommandQuery
				isEnabled: (repositoryListPresenter hasSelection and: [packageListPresenter selections size = 1]).
			^true].
	(#(#compareAncestor) includes: command)
		ifTrue: 
			[aCommandQuery
				isEnabled: (packageListPresenter selections size = 1 and: [ancestorListPresenter hasSelection]).
			^true].
	(#(#browseMethodHistory #browseImplementors #browseSenders #fileOutMethod) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: methodListPresenter selections size = 1.
			^true].
	(#(#inspectGlobal #browseGlobalReferences) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: globalsPresenter selections size = 1.
			^true].
	(#(#removeGlobals) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: globalsPresenter selections notEmpty.
			^true].
	"(#(#'runMethodTests') includes: command) ifTrue: [aCommandQuery isEnabled: (methodListPresenter selections notEmpty and: [methodListPresenter selections first at: 3]). ^true]."
	(#(#gotoMethodPackage) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: self methodListPresenter selectionOrNil notNil.
			^true].
	(#(#gotoClassPackage) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: self classListPresenter selectionOrNil notNil.
			^true].
	(#(#removeMethods) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: self methodListPresenter selections notEmpty.
			^true].
	(#(#loadLatestVersion) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: (packageListPresenter selections notEmpty
						and: [packageListPresenter selections allSatisfy: [:each | each key beginsWith: 'ConfigurationOf']]).
			^true].
	(#(#browseClassReferences #fileOutClass #addSubclass #addMissingAccessors #removeClass #removePriorVersions)
		includes: command)
			ifTrue: 
				[aCommandQuery isEnabled: self selectedClasses size == 1.
				^true].
	(#(#runClassTests) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: selectedClassesAreTestCases.
			^true].
	(#(#browseSelectedClass) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: self isClassSelectedInEditor.
			^true].
	command == #historyBack
		ifTrue: 
			[aCommandQuery isEnabled: self hasCommandHistoryBack.
			^true].
	command == #historyForward
		ifTrue: 
			[aCommandQuery isEnabled: self hasCommandHistoryForward.
			^true].
	
	(command == #setEnvironment0 and: [environment == 0]) ifTrue: [aCommandQuery isChecked: true.^true].
	(command == #setEnvironment1 and: [environment == 1]) ifTrue: [aCommandQuery isChecked: true.^true].
	(command == #setEnvironment2 and: [environment == 2]) ifTrue: [aCommandQuery isChecked: true.^true].

	(self projectMenuCommands includes: command) ifTrue:  [aCommandQuery isEnabled: projectListPresenter selections notEmpty. ^true].
	(#(#browseHierarchyImplementors #browseHierarchySenders) includes: command) ifTrue: [aCommandQuery isEnabled: methodListPresenter selections notEmpty. ^true].

	(self queryCommandHandlers: aCommandQuery) ifTrue: [^true].
	(aCommandQuery canPerformAgainst: self) ifTrue: [aCommandQuery beEnabled].
	^false	"There was no specific enablement of the command at this level"!

refresh
	MessageBox notify: 'This feature not yet implemented'!

refreshClass
	| selection |
	self setClassMeta.
	self setMethodFilter.
	selection := classListPresenter selectionIfNone: 
					[categoryListPresenter clear.
					^methodListPresenter clear].
	selection selectedPackageServices: packageListPresenter selections.
	self setFilterType: selection.
	self updateServices: (Array with: selection).
	self updateClassInfo!

registerClassData
	"temporarily disabled"!

registerPresentersForUpdates
	"copied to RowanDebugger"

	BrowserUpdate current register: projectListPresenter selector: #projectsUpdate:.
	BrowserUpdate current
		register: packageListPresenter
		selector: #projectPackagesUpdate:browser:
		browser: self.
	BrowserUpdate current
		register: classListPresenter
		selector: #classesUpdate:browser:
		browser: self.
	BrowserUpdate current
		register: categoryListPresenter
		selector: #filterUpdate:browser:
		browser: self.
	BrowserUpdate current
		register: variableListPresenter
		selector: #filterUpdate:browser:
		browser: self.
	BrowserUpdate current
		register: methodListPresenter
		selector: #classMethodsUpdate:browser:
		browser: self.
	BrowserUpdate current register: methodListPresenter selector: #methodsRemoved:.
	BrowserUpdate current register: methodSourcePresenter selector: #methodSourceUpdate:!

removeClass
	| packageService selection |
	selection := classListPresenter selectionIfNone: [^self].
	(MessageBox confirm: 'Really delete class ' , selection name , '?') ifFalse: [^self].
	packageService := packageListPresenter selection.
	packageService
		command: #removeClass:;
		commandArgs: (Array with: selection).
	self issueCommand: (Array with: packageService)!

removeMethodCategories
	| service |
	service := classListPresenter selection
				command: #removeCategories:;
				commandArgs: (Array with: categoryListPresenter selections).
	self issueCommand: (Array with: service)!

removeMethods: services
	services do: 
			[:classService |
			classService command: #removeMethods.
			classService methods: methodListPresenter selections].
	self issueCommand: services!

savePackage
	MessageBox notify: 'This feature is not yet available'!

selectClass: classSelection
	| newClassSelection |
	newClassSelection := classListPresenter list detect: [:svc | svc name = classSelection name]
				ifNone: [].
	^newClassSelection ifNotNil: [classListPresenter selection: newClassSelection]!

selectedMethodName

	^methodListPresenter selection selector
!

selectionMethod: methodSelection
	| newMethodSelection |
	newMethodSelection := methodListPresenter list
				detect: [:svc | svc selector = methodSelection selector]
				ifNone: [].
	^newMethodSelection ifNotNil: [methodListPresenter selection: newMethodSelection]!

selectMethodsFrom: classService
	| selections |
	selections := OrderedCollection new.
	classService selectedMethods do: 
			[:methodService |
			| selection |
			selection := methodListPresenter list detect: [:svc | svc selector == methodService selector]
						ifNone: [].
			selection ifNotNil: [:sel | selections add: sel]].
	methodListPresenter selections: selections!

selectorsString
	| stream |
	self updateMethodReferences: methodListPresenter selections.
	stream := String new writeStream. 
	methodListPresenter selections do: 
			[:service |
			stream nextPutAll: service selectorsDisplayString.
			service references notEmpty
				ifTrue: 
					[stream
						nextPutAll: ' (referenced by ';
						nextPutAll: service references size printString;
						nextPutAll: ' methods)'].
			stream cr].
	^stream contents!

selectPackage: newPackageSelection
	| methodSelection classSelection |
	methodSelection := methodListPresenter selectionOrNil.
	classSelection := classListPresenter selectionOrNil.
	packageListPresenter selection: newPackageSelection.
	classSelection ifNotNil: [self selectClass: classSelection].
	methodSelection ifNotNil: [self selectionMethod: methodSelection]!

setClassMeta
	| meta |
	meta := instanceClassTabs currentCard name = 'classTab'.
	classListPresenter list do: [:service | service meta: meta]!

setDefaultProjectTablLabel
	projectLabel ifNil: [projectLabel := self textTabs detect: [:each | each text = 'Project']]!

setDefaultProjectTablLabel: listModel
	projectLabel ifNil: [projectLabel := listModel detect: [:each | each text = 'Project']]!

setFilterType: selection
	self isCategoriesTabSelected
		ifTrue: [selection filterType: 'categories']
		ifFalse: [selection filterType: 'variables']!

setMethodFilter
	| filters |
	self isCategoriesTabSelected
		ifTrue: [filters := categoryListPresenter selections]
		ifFalse: [filters := variableListPresenter selections].
	classListPresenter list do: [:service | service filters: filters]!

showSuperClassComparison
	| methodService |
	methodService := methodListPresenter selection.
	methodService hasSupers
		ifTrue: 
			[comparisonPresenter
				beforeTitle: methodService classMethodDisplayString;
				afterTitle: methodService superDisplayString;
				compare: methodService source with: methodService superSource]
		ifFalse: 
			[comparisonPresenter
				beforeTitle: methodService classMethodDisplayString;
				afterTitle: 'No super implementation';
				compare: methodService source with: String new]!

textTabChanged
	methodSourcePresenter view cancelCallTip.
	textAreaTabs currentCard name = 'projectInfo' ifTrue: [projectInfoTab ensureVisible].
	self updateMenuBar!

textTabs
	^(textAreaTabs subViews detect: [:each | each class == TabViewXP]) model!

unloadPackage
	MessageBox notify: 'This feature is not yet available'!

unloadProject
	MessageBox notify: 'This feature not yet implemented'!

updateAfterFindClass: classService
	| projectService packageService updatedClassService |
	projectService := projectListPresenter list
				detect: [:service | service name = classService projectName]
				ifNone: 
					["old school"
					^self openDictionaryBrowserOn: classService].
	projectListPresenter selections: (Array with: projectService).
	packageService := packageListPresenter list
				detect: [:service | service name = classService packageName]
				ifNone: [^self].
	packageListPresenter selections: (Array with: packageService).
	updatedClassService := classListPresenter list
				detect: [:service | service name asString = classService name asString]
				ifNone: [^self].
	classListPresenter selections: (Array with: updatedClassService)!

updateAndSelectA: aView

	inUpdate ifTrue: [^self].
	"self updateCommand: self requestStringForUpdate."
	self updateMenuBar.
	aView notNil ifTrue: [aView ensureVisible].
!

updateClasses
	classListPresenter clear.
	self updateServices: packageListPresenter selections. !

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

updateMenuBar

	| shellView menuBar dictsPkgsMenu methodFilterMenu textAreaMenu |
	methodFilterMenu := Menu fromStrings: (self isCategoriesTabSelected ifTrue: [self categoriesMenuStrings] ifFalse: [self variablesMenuStrings]).
	textAreaMenu := Menu fromStrings: (self isGlobalsTabSelected ifTrue: [self globalsMenuStrings] ifFalse: [self editMenuStrings]).
	shellView := self parentPresenter parentPresenter view.
	menuBar := shellView menuBar
		clear;
		addItem: (Menu fromStrings: self jadeMenuStrings);
		addItem: self projectMenu;
		addItem: (Menu fromStrings: self classesMenuStrings);
		addItem: methodFilterMenu;
		addItem: (Menu fromStrings: self methodsMenuStrings);
		addItem: textAreaMenu;
		addItem: (Menu fromStrings: self preferencesMenuStrings);
		yourself.
	shellView menuBar: menuBar.
!

updateMethodReferences: services
	| stonString stonResultString |
	services do: [:service | service command: #allReferences].
	stonString := STON toString: services.
	stonResultString := self gciSession serverPerform: #updateFromSton: with: stonString.
	self update: services afterStonReplication: stonResultString!

updateMethodSource
	methodListPresenter selections isEmpty ifTrue: [^methodSourcePresenter setDefaultMethod].
	self updateSingleMethodSource.
	methodListPresenter selections size > 1 ifTrue:[
		self updateMultiMethodComparison] 
	ifFalse:[
		self showSuperClassComparison].!

updateMultiMethodComparison
	comparisonPresenter
		beforeTitle: (methodListPresenter selections at: 1) classMethodDisplayString;
		afterTitle: (methodListPresenter selections at: 2) classMethodDisplayString;
		compare: (methodListPresenter selections at: 1) source
			with: (methodListPresenter selections at: 2) source.!

updatePackage
	self updateClasses.
	self updatePackageInfo.!

updatePackageInfo
	| service |
	service := packageListPresenter selectionIfNone: [^self].
	self updateProjectTab.
	classDefinitionPresenter value: service defaultTemplate.
	classDefinitionPresenter view
		backcolor: Color white;
		isModified: false.
	classDefinitionPresenter ensureVisible!

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
	self updateProjectTab.
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
	projectInfoTab ensureVisible.!

updateProjects
	| browserService |
	browserService := RowanBrowserService new.
	browserService
		command: #updateProjects;
		commandArgs: Array new.
	self issueCommand: (Array with: browserService)!

updateProjectTab
	| projectTabLabel |
	projectTabLabel := self projectTabLabel.
	self view arrangement: projectTabLabel.
	self parentPresenter view updateTabs!

updateSelectionsForUpdate: update
	update updatePackageSelectionsFor: packageListPresenter.
	update updateClassCategorySelectionsFor: classCategoryPresenter.
	update updateClassSelectionFor: classListPresenter.
	update updateClassHierarchySelectionFor: classHierarchyPresenter.
	update updateSuperclassSelectionFor: superclassListPresenter.
	update updateMethodFilterSelectionFor: self methodFilterListPresenter.
	update updateMethodSelectionFor: self methodListPresenter.
	update projectSelectionUpdate: projectListPresenter!

updateServices: services
	"Private - copied to RowanDebugger"

	| stonString stonResults |
	stonString := STON toString: services.
	stonResults := self gciSession serverPerform: #updateFromSton: with: stonString.
	self update: services afterStonReplication: stonResults!

updateSingleMethodSource
	methodSourcePresenter value: methodListPresenter selections first source.
	methodSourcePresenter view
		backcolor: JadeTextStyles default colorForNoEdits;
		isModified: false;
		isReadOnly: false;
		yourself.
	methodSourcePresenter ensureVisible!

updateTabLabel
	| cardNumber newLabel |
	cardNumber := self parentPresenter view cards indexOf: self view.
	(newLabel := self selectedClassNameWithoutVersion) notEmpty
		ifTrue: 
			[instanceClassTabs currentCard name = 'classTab' ifTrue: [newLabel := newLabel , ' class'].
			0 < environment ifTrue: [newLabel := newLabel , ' [' , environment printString , ']']]
		ifFalse: 
			[|  selections |
			newLabel := 'Projects'.
			selections := projectListPresenter selections.
			selections size = 1 ifTrue: [newLabel := selections first name]].
	self view arrangement: cardNumber printString , ' ' , newLabel.
	self parentPresenter view updateTabs!

writeProject
	| projectName |
	projectName := projectListPresenter selection name.
	(MessageBox confirm: 'Write project ' , projectName , ' to disk without committing?')
		ifFalse: [^false].
	((RowanProjectService on: gciSession) name: projectName) write.
	MessageBox notify: 'Project ' , projectName , ' written to disk'! !
!JadeiteProjectsBrowserPresenter categoriesFor: #addPackage!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #addPackageDictionaryInfoTo:!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #addSubclass!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #allClasses!public!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #basicAddMethodCategory:!menu handlers!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #basicRemoveMethods!menu handlers!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseClassReferences!menu handlers!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseHierarchyImplementors!menu handlers!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseHierarchyImplementorsOf:!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseHierarchySenders!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseHierarchySendersOf:!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseImplementorsOf:!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseMethodHistory!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseMethodsContaining!menu handlers!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseSendersOf:!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #checkout!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #classesMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #cloneProject!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #comparisonTab!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createComponents!initialization!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiring!create schemantic wiring!overrides!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForClassList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForInstanceClassTabs!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForMethodCategoryList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForMethodFilterTabs!create schemantic wiring!overrides!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForMethodList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForPackageDictionaryTabs!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForPackageList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForProjectDictionaryList!create schemantic wiring!overrides!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForProjectList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForVariableList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createStateEntry!accessing!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #editSaveClass!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #editSaveClassComment!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #editSaveMethod!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #editSaveMethod:!menu handlers!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #filterTabChanged!event handlers!public!services!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #findClass!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #getViews!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #gitCommit!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #gotoClassPackage!menu handlers!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #gotoMethodPackage!menu handlers!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #initialize!initialization!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #inspectProject!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #isDictionariesTabSelected!public!testing! !
!JadeiteProjectsBrowserPresenter categoriesFor: #loadProject!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #makeDefault!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #methodsMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #newService!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onDragMethod:!drag & drop!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onDropOnClassList:!drag & drop!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onDropOnMethodCategory:!drag & drop!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onViewOpened!event handlers!overrides!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #openDictionaryBrowserOn:!menu handlers!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #openMethodListOn:selecting:!menu handlers!private!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #pkgsMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #postOkToChangeEvent!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectChanges!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectDictionaryChanged!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectListPresenter!accessing!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectLog!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectMenu!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectMenuCommands!menus!overrides!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectTabLabel!displaying!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #pullFromGit!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #pushToGit!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #queryCommand:!menu handlers!overrides!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #refresh!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #refreshClass!event handlers!public!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #registerClassData!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #registerPresentersForUpdates!event handlers!initialization!public!register presenters! !
!JadeiteProjectsBrowserPresenter categoriesFor: #removeClass!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #removeMethodCategories!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #removeMethods:!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #savePackage!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectClass:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectedMethodName!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectionMethod:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectMethodsFrom:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectorsString!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectPackage:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setClassMeta!event handlers!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setDefaultProjectTablLabel!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setDefaultProjectTablLabel:!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setFilterType:!event handlers!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setMethodFilter!event handlers!private!services!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #showSuperClassComparison!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #textTabChanged!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #textTabs!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #unloadPackage!menu handlers!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #unloadProject!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateAfterFindClass:!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateAndSelectA:!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateClasses!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateClassInfo!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateCommand:onSuccessDo:!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateMenuBar!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateMethodReferences:!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateMethodSource!event handlers!public!text tabs!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateMultiMethodComparison!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updatePackage!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updatePackageInfo!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updatePackages!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateProject!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateProjectInfo!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateProjects!event handlers!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateProjectTab!event handlers!private!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateSelectionsForUpdate:!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateServices:!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateSingleMethodSource!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateTabLabel!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #writeProject!project menu handlers!public! !

!JadeiteProjectsBrowserPresenter class methodsFor!

downArrowIcon

	^Icon fromId: 'DownArrow.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 544 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 544 655878 ##(Smalltalk.CardLayout) 138 144 34 6 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'SUnit' 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 4 1 800 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 816 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 544 34 2 8 1140850688 131073 960 nil 610 8 4278190080 nil 5 nil nil nil 960 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 9 55 1202 2225 635 960 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1202 193 193 nil 27 722 754 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 1376 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 610 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 11 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #editSave 8 '&Save' 9383 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 1586 1 1618 #editUndo 8 '&Undo' 9397 1 nil nil nil 1586 1 1618 #editRedo 8 '&Redo' 9395 1 nil nil nil 1666 4097 1586 1 1618 #editCut 8 'Cu&t' 9393 1 nil nil nil 1586 1 1618 #editCopy 8 '&Copy' 9351 1 nil nil nil 1586 1 1618 #editPaste 8 '&Paste' 9389 1 nil nil nil 1586 1 1618 #editDelete 8 '&Delete' 1629 1 nil nil nil 1666 4097 1586 1 1618 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1202 193 193 nil 1376 nil 8 4294910145 852486 ##(Smalltalk.NullConverter) nil nil 13 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 2258 77 786694 ##(Smalltalk.IndexedColor) 33554457 2306 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 2272 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 2288 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 1296 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 1376 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1296 nil 170 176 1296 9215 nil nil nil nil 2306 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 2272 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 1376 65025 3 false 1 nil nil nil nil nil nil 3 2722 3 1376 33423361 5 false 3 nil nil nil nil nil nil 5 2722 5 1376 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 2258 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 2258 1027 2834 60239 2834 31326207 1 nil nil nil nil #Warning nil nil nil #Error 2258 1031 2834 335 2834 31317503 1 nil nil nil nil #Error nil nil nil nil 1090 138 144 34 8 1154 #createAt:extent: 34 2 1202 9 55 1202 2225 635 1376 1154 #contextMenu: 34 1 1552 1376 1154 #modificationEventMask: 8 #(4607) 1376 1154 #caretPeriod: 8 #(530) 1376 1154 #wordWrap: 8 #(true) 1376 1154 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 1376 1 3 nil nil 5 nil 3250 3 1376 33 nil 3 67108863 5 nil 3250 5 1376 1 nil 3 -67108863 5 nil 1376 1154 #tabWidth: 8 #(4) 1376 1154 #maxCompletionListHeight: 8 #(9) 1376 1250 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1312 nil 29 722 754 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 3472 1442 nil false 1488 nil 610 1520 nil 5 1538 nil true 34 1 1586 1 1618 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 2082 nil true 2114 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1202 193 193 nil 3472 nil 8 4294910145 2194 nil nil 11 nil 170 192 34 4 #normal 2258 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 2258 77 2320 2336 1 nil nil nil nil #callTip nil nil nil 34 256 3792 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 3808 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 2450 1 nil nil nil 3472 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1296 nil 170 176 1296 9215 nil nil nil nil 2512 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 3792 nil nil nil nil 1 nil 170 192 34 6 1 2722 1 3472 65025 3 false 1 nil nil nil nil nil nil 3 2722 3 3472 33423361 5 false 3 nil nil nil nil nil nil 5 2722 5 3472 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 2258 1029 nil 2834 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 2258 1027 2834 60239 2834 31326207 1 nil nil nil nil #Warning nil nil nil #Error 2258 1031 2834 335 2834 31317503 1 nil nil nil nil #Error nil nil nil nil 1090 138 144 34 8 1154 #createAt:extent: 34 2 1202 9 55 1202 2225 635 3472 1154 #contextMenu: 34 1 3568 3472 1154 #modificationEventMask: 8 #(4607) 3472 1154 #caretPeriod: 8 #(530) 3472 1154 #wordWrap: 8 #(true) 3472 1154 #margins: 34 1 34 3 3250 1 3472 1 3 nil nil 5 nil 3250 3 3472 33 nil 3 67108863 5 nil 3250 5 3472 1 nil 3 -67108863 5 nil 3472 1154 #tabWidth: 8 #(4) 3472 1154 #maxCompletionListHeight: 8 #(9) 3472 1250 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1312 nil 29 722 754 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 4768 1442 nil false 1488 nil 610 8 4278190080 nil 5 nil 2082 nil true 2114 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1202 193 193 nil 4768 nil 8 4294910145 2194 nil nil 13 nil 170 192 34 4 #normal 2258 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 2258 77 2320 2336 1 nil nil nil nil #callTip nil nil nil 34 256 5008 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 5024 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 1296 170 192 34 2 #default 2450 1 nil nil nil 4768 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1296 nil 170 176 1296 9215 nil nil nil nil 2512 nil nil 65 nil nil 2528 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 5008 #callTip 5024 nil nil nil nil 1 nil 170 192 34 12 1 2722 1 4768 65025 3 false 1 nil nil nil nil nil nil 3 2722 3 4768 33423361 5 false 3 nil nil nil nil nil nil 5 2722 5 4768 511 1 false 5 nil nil nil nil nil nil 8 'indicator9' 2722 19 4768 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator10' 2722 21 4768 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator8' 2722 17 4768 33554447 1 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 2258 1029 nil 2834 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 2258 1027 2834 60239 2834 31326207 1 nil nil nil nil #Warning nil nil nil #Error 2258 1031 2834 335 2834 31317503 1 nil nil nil nil #Error nil nil nil nil 1090 138 144 34 8 1154 #createAt:extent: 34 2 1202 9 55 1202 2225 635 4768 1154 #modificationEventMask: 8 #(4607) 4768 1154 #hoverTime: 8 #(200) 4768 1154 #caretPeriod: 8 #(530) 4768 1154 #wordWrap: 8 #(true) 4768 1154 #margins: 34 1 34 3 3250 1 4768 61 3 nil nil 5 nil 3250 3 4768 1 nil 3 67108863 nil nil 3250 5 4768 1 nil 3 -67108863 nil nil 4768 1154 #tabWidth: 8 #(4) 4768 1154 #maxCompletionListHeight: 8 #(9) 4768 1250 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1312 nil 29 722 754 8 'Project' 802 0 nil 834 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 914 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 6096 7 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 544 34 2 8 1409372237 1025 6208 590662 2 ##(Smalltalk.ListModel) 138 144 1296 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 610 8 4278190080 nil 5 nil nil nil 6208 nil 8 4294910083 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Name' 201 #left 6402 #displayString 6432 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | (each at: 2)]' 8 #[29 105 17 64 148 106] 6576 7 257 nil nil 6208 nil 1 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each item at: 1) displayStringFor: each]' 8 #[31 105 226 0 63 148 17 177 106] #item #displayStringFor: 6656 7 257 nil 6514 8 'Value' 2017 #left 6402 #displayString 8 #() 6402 #<= 6784 802 0 nil 834 4 1 800 8 'doIt' 8 '[:each | (each at: 2) ifNil:[String new] ifNotNil: [:iv |
(each at: 1) instVarNamed: iv]]' 8 #[32 105 17 64 148 100 223 4 97 45 146 106 90 17 63 148 18 178 106] 914 #String 16 #ifNil:ifNotNil: #instVarNamed: 6816 7 65793 nil nil 6208 nil 3 nil nil #report 1296 nil 131169 nil 34 4 nil nil 1202 1 1 nil 1090 138 144 34 2 1154 #createAt:extent: 34 2 1202 9 55 1202 2225 635 6208 1154 #text: 34 1 8 'Name' 6208 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1312 nil 27 722 8 'Comparison' 410 ##(Smalltalk.ShellView) 34 27 nil 544 34 2 8 1078919168 131073 7184 nil 610 8 4278190080 nil 517 nil nil nil 7184 788230 ##(Smalltalk.BorderLayout) 1 1 nil nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 7184 34 2 8 1140850688 131073 7312 nil 610 7264 nil 5 1538 nil true 34 5 1586 1 1618 #toggleIndentGuides 8 'Indentation &Guides' 1 1 nil nil nil 1586 1 1618 #toggleVisibleLineEndings 8 'Line &Endings' 1 1 nil nil nil 1586 1 1618 #toggleLineNumberMargins 8 'Line N&umbers' 1 1 nil nil nil 1586 1 1618 #toggleVisibleWhitespace 8 'W&hitespace' 1 1 nil nil nil 1586 1 1618 #toggleWrapping 8 '&Word Wrap' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 7312 482 170 176 1296 false 170 192 1296 nil 1090 138 144 34 2 1154 #createAt:extent: 34 2 1202 1 1 1202 2193 603 7312 1154 #contextMenu: 34 1 7392 7312 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 7312 34 2 8 1140850688 131073 7920 nil 610 7264 nil 5 nil nil nil 7920 7282 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 7920 34 2 8 1140850945 262145 8016 nil nil nil 5 nil nil nil 8016 nil 8 1660708144 2194 nil nil nil 1090 138 144 34 2 1154 #createAt:extent: 34 2 1202 1 563 1202 1091 41 8016 1154 #text: 34 1 8 '
' 8016 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 33 2 0 0 45 1 0 0] 8 #() 1312 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 7920 34 2 8 1140920580 1025 8320 1442 nil false 1488 nil 610 7264 nil 5 nil nil nil 8320 nil 8 4294910145 2194 nil nil 11 nil 170 192 34 12 #indentGuide 2258 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 2258 3 2306 33554445 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 2258 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 2258 77 2320 2336 1 nil nil nil nil #callTip nil nil nil #normal 2258 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 2258 5 nil 2834 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 8560 8496 8576 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 8528 nil nil nil 8480 nil nil 1310982 ##(Smalltalk.DiffsScintillaStyler) 8 #() 170 192 34 2 #default 2450 1 nil nil nil 8320 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1296 nil 170 176 1296 9215 nil nil nil nil 2512 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 8448 nil nil nil nil 1 nil 170 192 34 6 8 'indicator1' 2722 3 8320 33423361 5 false nil nil nil nil nil nil nil 8 'indicator2' 2722 5 8320 511 1 false nil nil nil nil nil nil nil 8 'indicator0' 2722 1 8320 33554459 13 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 2258 1029 nil 2834 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 2258 1027 2834 60239 2834 31326207 1 nil nil nil nil #Warning nil nil nil #Error 2258 1031 2834 335 2834 31317503 1 nil nil nil nil #Error nil nil nil nil 1090 138 144 34 7 1154 #createAt:extent: 34 2 1202 1 1 1202 1091 563 8320 1154 #modificationEventMask: 8 #(4607) 8320 1154 #caretPeriod: 8 #(530) 8320 1154 #wordWrap: 8 #(true) 8320 1154 #margins: 34 1 34 3 3250 1 8320 49 3 nil nil nil nil 3250 3 8320 1 nil nil 67108863 nil nil 3250 5 8320 1 nil nil nil nil nil 8320 1154 #canHScroll: 8 #(false) 8320 1154 #maxCompletionListHeight: 8 #(9) 8320 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 25 1 0 0] 8 #() 1312 nil 29 170 192 34 4 8016 8 'beforeTitle' 8320 8 'before' nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1 1 1202 1091 603 7920 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 45 1 0 0] 34 2 8320 8016 1312 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 7312 34 2 8 1140850688 1 9728 nil 2834 66047 nil 517 nil nil nil 9728 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1091 1 1202 11 603 9728 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 33 2 0 0 0 0 0 0 38 2 0 0 45 1 0 0] 8 #() 1312 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 7312 34 2 8 1140850688 131073 9968 nil 610 7264 nil 5 nil nil nil 9968 7282 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 9968 34 2 8 1140850945 262145 10064 nil nil nil 5 nil nil nil 10064 nil 8 1660708144 2194 nil nil nil 1090 138 144 34 2 1154 #createAt:extent: 34 2 1202 1 563 1202 1093 41 10064 1154 #text: 34 1 8 '
' 10064 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 34 2 0 0 45 1 0 0] 8 #() 1312 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 9968 34 2 8 1140920580 1025 10368 1442 nil false 1488 nil 610 7264 nil 5 nil nil nil 10368 nil 8 4294910145 2194 nil nil 11 nil 170 192 34 12 #indentGuide 2258 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 2258 3 8512 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 2258 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 2258 77 2320 2336 1 nil nil nil nil #callTip nil nil nil #normal 2258 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 2258 5 nil 2834 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 10592 10544 10608 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 10560 nil nil nil 10528 nil nil 8626 8656 170 192 34 2 #default 2450 1 nil nil nil 10368 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1296 nil 170 176 1296 9215 nil nil nil nil 2512 nil nil 65 nil nil nil 8752 3 170 192 34 2 #container 10496 nil nil nil nil 1 nil 170 192 34 6 8 'indicator2' 2722 5 10368 511 1 false nil nil nil nil nil nil nil 8 'indicator1' 2722 3 10368 33423361 5 false nil nil nil nil nil nil nil 8 'indicator0' 2722 1 10368 33554459 13 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 2258 1029 nil 2834 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 2258 1027 2834 60239 2834 31326207 1 nil nil nil nil #Warning nil nil nil #Error 2258 1031 2834 335 2834 31317503 1 nil nil nil nil #Error nil nil nil nil 1090 138 144 34 7 1154 #createAt:extent: 34 2 1202 1 1 1202 1093 563 10368 1154 #modificationEventMask: 8 #(4607) 10368 1154 #caretPeriod: 8 #(530) 10368 1154 #wordWrap: 8 #(true) 10368 1154 #margins: 34 1 34 3 3250 1 10368 49 3 nil nil nil nil 3250 3 10368 1 nil nil 67108863 nil nil 3250 5 10368 1 nil nil nil nil nil 10368 1154 #canHScroll: 8 #(false) 10368 1154 #maxCompletionListHeight: 8 #(9) 10368 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 34 2 0 0 25 1 0 0] 8 #() 1312 nil 29 170 192 34 4 10064 8 'afterTitle' 10368 8 'after' nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1101 1 1202 1093 603 9968 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 2 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 2 10368 10064 1312 nil 27 1312 nil 27 170 192 34 2 7312 8 'diffs' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 1538 nil true 34 4 1586 1 1618 #selectUpper 8 'Select &upper' 1 1 nil nil nil 1586 1 1618 #selectLower 8 'Select &lower' 1 1 nil nil nil 1666 4097 1586 1 1618 #exit 8 '&Close' 1 1 nil nil nil 8 '&File' nil 1 nil nil 45957 nil nil 1538 nil true 34 3 1586 1 1618 #character 8 '&Character' 1 1 nil nil nil 1586 1 1618 #word 8 '&Word' 1 1 nil nil nil 1586 1 1618 #line 8 '&Line' 1 1 nil nil nil 8 '&Mode' nil 1 nil nil 45959 nil nil 1538 nil true 34 1 1586 1 1618 #about 8 '&About' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 45961 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1090 138 144 34 3 1154 #createAt:extent: 34 2 1202 9 55 1202 2225 635 7184 1154 #text: 34 1 8 'Comparison' 7184 1154 #updateMenuBar 1296 7184 1250 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 34 1 7312 1312 nil 27 6208 170 192 34 12 1376 8 'classDefinition' 7184 8 'comparisonTab' 4768 8 'methodSource' 3472 8 'classDocumentation' 6208 8 'projectInfo' 960 8 'sunit' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 544 34 2 8 1140916736 1 12672 6274 138 144 34 6 6064 1344 3440 4736 768 7168 nil 6336 nil nil 1 nil nil nil 12672 nil 8 4294909877 802 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 12800 7 257 nil 802 0 nil 12818 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 12864 7 257 nil 6464 nil nil nil nil nil #smallIcons nil nil nil nil nil 1090 138 144 34 3 1154 #createAt:extent: 34 2 1202 1 1 1202 2241 697 12672 1154 #basicSelectionsByIndex: 34 1 8 #(1) 12672 1154 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 12672 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 92 1 0 0] 8 #() 1312 nil 27 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1 705 1202 2241 697 544 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 96 1 0 0 96 4 0 0 188 2 0 0] 34 7 6208 1376 3472 4768 960 7184 12672 1312 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 13344 nil nil nil 5 nil nil nil 13344 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 13344 34 2 8 1140850688 131073 13456 nil nil nil 5 nil nil nil 13456 482 170 176 1296 false 170 192 34 2 410 ##(Smalltalk.CardContainer) 34 16 nil 13456 34 2 8 1409286144 131073 13584 nil 610 8 4278190080 nil 5 nil nil nil 13584 658 138 144 34 2 722 8 'Category' 410 ##(Smalltalk.ListView) 34 30 nil 13584 34 2 8 1409372233 1025 13760 6274 138 144 1296 nil 6336 610 8 4278190080 nil 21 nil nil nil 13760 nil 8 4294910083 6402 #displayString 8 #() nil 6464 nil nil nil nil nil nil 138 144 34 1 6514 8 'Column 1' 417 #left 6402 #displayString 8 #() ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each]' 1779525917 14032 7 257 nil nil 13760 nil 3 nil nil #report 1296 nil 131169 nil 34 4 nil nil 1202 1 1 nil 1090 138 144 34 2 1154 #createAt:extent: 34 2 1202 9 55 1202 425 633 13760 1154 #text: 34 1 8 'Column 1' 13760 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 216 0 0 0 87 1 0 0] 8 #() 1312 nil 27 722 8 'Variables' 410 ##(Smalltalk.ListView) 34 30 nil 13584 34 2 8 1409372233 1025 14368 6274 138 144 1296 nil 6336 610 13872 nil 21 nil nil nil 14368 nil 8 4294910083 6402 #displayString 8 #() nil 6464 nil nil nil nil nil nil 138 144 34 1 6514 8 'Column 1' 417 #left 6402 #displayString 14512 ##(Smalltalk.SortedCollection) nil nil 14368 nil 3 nil 802 0 nil 834 9 1 800 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 14608 7 257 nil #report 1296 nil 131169 nil 34 4 nil nil 1202 1 1 nil 1090 138 144 34 2 1154 #createAt:extent: 34 2 1202 9 55 1202 425 633 14368 1154 #text: 34 1 8 'Column 1' 14368 1250 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 216 0 0 0 87 1 0 0] 8 #() 1312 nil 27 13760 170 192 34 4 13760 8 'categoryList' 14368 8 'variableList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 13584 34 2 8 1140916736 1 14992 6274 138 144 34 2 13744 14352 nil 6336 nil nil 1 nil nil nil 14992 nil 8 4294909877 802 0 nil 12818 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 15120 7 257 nil 802 0 nil 12818 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 15168 7 257 nil 6464 nil nil nil nil nil #noIcons nil nil nil nil nil 1090 138 144 34 3 1154 #createAt:extent: 34 2 1202 1 1 1202 441 695 14992 1154 #basicSelectionsByIndex: 34 1 8 #(1) 14992 1154 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 14992 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 8 #() 1312 nil 27 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1 1 1202 441 695 13584 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 34 3 13760 14368 14992 1312 nil 27 8 'methodFilterTabs' nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1349 1 1202 893 695 13456 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 162 2 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 13584 410 ##(Smalltalk.Splitter) 34 12 nil 13456 34 2 8 1140850688 1 15792 nil 610 8 4278190080 nil 517 nil nil nil 15792 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 441 1 1202 11 695 15792 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 220 0 0 0 0 0 0 0 225 0 0 0 91 1 0 0] 8 #() 1312 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 13456 34 2 8 1140850688 131073 16048 nil nil nil 5 nil nil nil 16048 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ComboBox) 34 17 nil 16048 34 2 8 1412498947 1025 16176 6274 138 144 1296 nil 6336 610 8 4278190080 nil 5 nil nil nil 16176 nil 8 4294906641 6402 #displayString 8 #() 1296 401 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1 1 1202 443 53 16176 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 221 0 0 0 26 0 0 0] 8 #() 1312 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 16546 #fixedParentRight 1 16546 #fixedParentTop 1 16546 #fixedViewTop 47 410 ##(Smalltalk.ComboBox) 34 17 nil 16048 34 2 8 1412498947 1025 16624 6274 138 144 1296 nil 6336 610 16288 nil 5 nil nil nil 16624 nil 8 4294906641 6402 #displayString 8 #() 1296 401 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1 649 1202 443 53 16624 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 68 1 0 0 221 0 0 0 94 1 0 0] 8 #() 1312 nil 27 16514 16560 1 16576 1 16546 #fixedParentBottom -45 16608 47 410 ##(Smalltalk.ListView) 34 30 nil 16048 34 2 8 1409372233 1025 16976 6274 138 144 1296 nil 6336 610 13872 nil 13 nil nil nil 16976 nil 8 4294910083 6402 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 6464 nil nil nil nil nil nil 138 144 34 2 6514 8 '' 41 #left 1154 #empty 8 #() 16 6402 #<= 8 #() nil nil 16976 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each subSuperIcon]' 8 #[30 105 226 0 106] #subSuperIcon 17264 7 257 nil 1 nil nil 6514 8 'Selector' 395 #left 6402 #displayString 17120 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 17392 7 257 nil nil 16976 nil 3 802 0 nil 834 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 17472 7 257 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 17552 7 257 nil #report 1296 nil 133221 nil 34 4 nil nil 1202 1 1 nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1 47 1202 443 603 16976 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 221 0 0 0 68 1 0 0] 8 #() 1312 nil 27 16514 16560 1 16576 1 16592 47 16960 -45 170 192 34 6 16176 8 'superclassList' 16624 8 'overrideList' 16976 8 'methodList' nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 451 1 1202 443 695 16048 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 225 0 0 0 0 0 0 0 190 1 0 0 91 1 0 0] 34 3 16176 16976 16624 1312 nil 27 1312 nil 27 5 410 ##(Smalltalk.ContainerView) 34 15 nil 13344 34 2 8 1140850688 131073 18080 nil nil nil 5 nil nil nil 18080 482 170 176 34 6 410 ##(Smalltalk.ListView) 34 30 nil 18080 34 2 8 1409405257 1025 18192 6274 138 144 1296 nil 6336 610 8 4278190080 nil 5 nil nil nil 18192 nil 8 4294910083 6402 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 6464 nil nil nil nil nil nil 138 144 34 1 6514 8 'Column 1' 361 #left 6402 #displayString 8 #() ##(Smalltalk.SortedCollection) nil nil 18192 nil 3 802 0 nil 834 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 18464 7 257 nil 802 0 nil 834 3 1 800 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 18544 7 257 nil #report 1296 nil 133217 nil 34 4 nil nil 1202 1 1 nil 1090 138 144 34 2 1154 #createAt:extent: 34 2 1202 1 1 1202 369 695 18192 1154 #text: 34 1 8 'Column 1' 18192 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 1312 nil 27 13314 9 11 410 ##(Smalltalk.ContainerView) 34 15 nil 18080 34 2 8 1140850688 131073 18880 nil nil nil 5 nil nil nil 18880 482 170 176 1296 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 18880 34 2 8 1140936777 1025 19008 6274 138 144 1296 nil 6336 610 8 4278190080 nil 21 nil nil nil 19008 nil 8 4294910083 6402 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 6464 nil nil nil nil nil nil 138 144 34 1 6514 8 'Column 1' 361 #left 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 19248 7 257 nil 6402 #<= 8 #() nil nil 19008 nil 3 802 0 nil 834 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 19360 7 257 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 19440 7 257 nil #report 1296 nil 133217 nil 34 4 nil nil 1202 1 1 nil 1090 138 144 34 2 1154 #createAt:extent: 34 2 1202 1 1 1202 369 695 19008 1154 #text: 34 1 8 'Column 1' 19008 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 1312 nil 27 8 'packagesList' nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 379 1 1202 369 695 18880 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 189 0 0 0 0 0 0 0 117 1 0 0 91 1 0 0] 34 1 19008 1312 nil 27 13314 9 11 410 ##(Smalltalk.ContainerView) 34 15 nil 18080 34 2 8 1140850688 131073 19952 nil nil nil 5 nil nil nil 19952 16114 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 19952 34 2 8 1409286144 131073 20064 nil 610 640 nil 5 nil nil nil 20064 658 138 144 34 2 722 754 8 'Classes' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 20224 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 20240 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 20064 34 2 8 1409372237 1025 20336 6274 138 144 1296 nil 6336 610 18304 nil 29 nil nil nil 20336 nil 8 4294910083 6402 #displayString 8 #() nil 6464 nil nil nil nil nil nil 138 144 34 1 6514 8 '' 559 #left 6402 #displayString 8 #() 6402 #<= 8 #() nil nil 20336 nil 3 802 0 nil 834 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 20624 7 257 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 20704 7 257 nil #report 1296 nil 133217 nil 34 4 nil nil 1202 1 1 nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 9 55 1202 567 589 20336 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 1312 nil 27 722 754 8 'Hierarchy' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 21008 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 21024 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 20064 34 2 8 1409352231 1025 21120 590918 3 ##(Smalltalk.TreeModel) nil 6336 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 1296 610 8 4278190080 nil 29 nil nil nil 21120 nil 8 4294908615 802 0 nil 834 2 1 6400 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 21312 7 257 nil ##(Smalltalk.IconicListAbstract) 6464 nil nil nil nil nil 170 176 1296 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 1296 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 9 55 1202 567 589 21120 1250 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 1312 nil 27 20336 170 192 34 4 20336 8 'classList' 21120 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 20064 34 2 8 1140916736 1 21648 6274 138 144 34 2 20208 20992 nil 6336 nil nil 1 nil nil nil 21648 nil 8 4294909877 802 0 nil 12818 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 21776 7 257 nil 802 0 nil 12818 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 21824 7 257 nil 6464 nil nil nil nil nil #smallIcons nil nil nil nil nil 1090 138 144 34 3 1154 #createAt:extent: 34 2 1202 1 1 1202 583 651 21648 1154 #basicSelectionsByIndex: 34 1 8 #(1) 21648 1154 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 21648 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 8 #() 1312 nil 27 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1 1 1202 583 651 20064 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 34 3 20336 21120 21648 1312 nil 27 16514 16560 1 16576 1 16592 1 16960 -43 410 ##(Smalltalk.CardContainer) 34 16 nil 19952 34 2 8 1409286144 131073 22288 nil 610 8 4278190080 nil 5 nil nil nil 22288 658 138 144 34 2 722 754 8 'Instance' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 22464 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 22480 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 22288 34 2 8 1140850688 131073 22576 nil nil nil 5 nil nil nil 22576 nil 170 192 1296 nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 9 9 1202 567 1 22576 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 1312 nil 27 722 754 8 'Class' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 22848 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 22864 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 22288 34 2 8 1140850688 131073 22960 nil nil nil 5 nil nil nil 22960 nil 170 192 1296 nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 9 9 1202 567 1 22960 1250 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 1312 nil 27 22576 170 192 34 4 22960 8 'classTab' 22576 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 22288 34 2 8 1140916738 1 23264 6274 138 144 34 2 22448 22832 nil 6336 nil nil 1 nil nil nil 23264 nil 8 4294909877 802 0 nil 12818 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 23392 7 257 nil 802 0 nil 12818 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 23440 7 257 nil 6464 nil nil nil nil nil #smallIcons nil nil nil nil nil 1090 138 144 34 3 1154 #createAt:extent: 34 2 1202 1 1 1202 583 51 23264 1154 #basicSelectionsByIndex: 34 1 8 #(1) 23264 1154 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 23264 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 25 0 0 0] 8 #() 1312 nil 27 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1 645 1202 583 51 22288 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 66 1 0 0 35 1 0 0 91 1 0 0] 34 3 22576 22960 23264 1312 nil 27 16514 16560 1 16576 1 16960 -49 16608 51 170 192 34 4 20064 8 'classHierarchyTabs' 22288 8 'instanceClassTabs' nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 757 1 1202 583 695 19952 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 122 1 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 2 20064 22288 1312 nil 27 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 244 63 false 170 192 34 2 18192 8 'projectList' nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1 1 1202 1339 695 18080 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 5 18192 410 ##(Smalltalk.Splitter) 34 12 nil 18080 34 2 8 1140850688 1 24368 nil 610 15872 nil 517 nil nil nil 24368 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 369 1 1202 11 695 24368 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 184 0 0 0 0 0 0 0 189 0 0 0 91 1 0 0] 8 #() 1312 nil 27 18880 410 ##(Smalltalk.Splitter) 34 12 nil 18080 34 2 8 1140850688 1 24608 nil 610 15872 nil 517 nil nil nil 24608 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 747 1 1202 11 695 24608 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 117 1 0 0 0 0 0 0 122 1 0 0 91 1 0 0] 8 #() 1312 nil 27 19952 1312 nil 27 7 false 170 192 1296 nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1 1 1202 2241 695 13344 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 18080 410 ##(Smalltalk.Splitter) 34 12 nil 13344 34 2 8 1140850688 1 25024 nil 610 15872 nil 517 nil nil nil 25024 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1339 1 1202 11 695 25024 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 157 2 0 0 0 0 0 0 162 2 0 0 91 1 0 0] 8 #() 1312 nil 27 13456 1312 nil 27 13314 3 5 true 170 192 34 4 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 25312 nil 610 15872 nil 517 nil nil nil 25312 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 1 695 1202 2241 11 25312 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 91 1 0 0 96 4 0 0 96 1 0 0] 8 #() 1312 nil 27 8 'splitter' 544 8 'textAreaTabs' nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 1202 7679 21 1202 2241 1401 416 1250 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 95 19 0 0 198 2 0 0] 34 3 13344 25312 544 1312 nil 27 )! !
!JadeiteProjectsBrowserPresenter class categoriesFor: #downArrowIcon!public! !
!JadeiteProjectsBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

