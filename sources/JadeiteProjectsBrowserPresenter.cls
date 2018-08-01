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

addPackageDictionaryInfoTo: aStream

	"projects don't write themselves to string"!

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
	| packageService |
	packageService := packageListPresenter selection.
	packageService
		command: #compileClass:;
		commandArgs: (Array with: classDefinitionPresenter value).
	[self issueCommand: (Array with: packageService)] on: GsCompileError
		do: [:ex | ^self handleClassCompileError: ex].
	classDefinitionPresenter view isModified: false.!

editSaveMethod
	| caretPosition firstVisibleLine classService category |
	caretPosition := methodSourcePresenter view caretPosition.
	firstVisibleLine := methodSourcePresenter view lineScroll.
	category := (self isCategoriesTabSelected and: [categoryListPresenter hasSelection])
				ifTrue: [categoryListPresenter selection trimBlanks].
	classService := classListPresenter selection.
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

getViews
	classHierarchyTabs := self view viewNamed: 'classHierarchyTabs'.
	instanceClassTabs := self view viewNamed: 'instanceClassTabs'.
	methodFilterTabs := self view viewNamed: 'methodFilterTabs'.
	textAreaTabs := self view viewNamed: 'textAreaTabs'.
	packageInfoTab := self view viewNamed: 'packageInfo'.
	projectInfoTab := self view viewNamed: 'projectInfo'!

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

onDropOnMethodCategory: aSession
	| methodServices targetCategory classService |
	methodServices := aSession dragObjects collect: [:each | each format: #method].
	targetCategory := aSession suggestedTarget trimBlanks.
	classService := classListPresenter selection. 
	classService
		command: #moveMethods:to:;
		commandArgs: (Array with: methodServices with: targetCategory).
	self issueCommand: (Array with: classService)!

onViewOpened
	"copied to RowanDebugger"

	super onViewOpened.
	self registerPresentersForUpdates. 
	projectListPresenter view contextMenu: (Menu fromStrings: self projectMenuStrings).
	self updateProjects. !

openMethodListOn: service selecting: string
	(JadeiteMethodListBrowser showOnSession: self gciSession) primaryPresenter
		methods: service queryResults
		selecting: string!

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
	projectLabel ifNil: [projectLabel := listModel detect: [:each | each text = 'Project']].
	packageLabel ifNil: [packageLabel := listModel detect: [:each | each text = 'Package']].
	(listModel includes: projectLabel) ifFalse: [listModel addFirst: projectLabel].
	self updateProjectInfo.
	projectInfoTab ensureVisible.
	listModel remove: globalsLabel ifAbsent: [].
	listModel remove: packageLabel ifAbsent: []!

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
		'Commit ...//commit' 
		'Push to Git ...// pushToGit'
		'-'
		'&Changes ...// projectChanges'
		'Write ...//writeProject' 
		'Checkout ...// checkout'
		'-'
		'Log// projectLog'
		'&Inspect//inspectProject' 
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

queryCommand: aCommandQuery

	| command focusView isTextEdit |
	self rowanFixMe. "enablement of method test thingy"
	command := aCommandQuery command.
	focusView := View focus.
	isTextEdit := focusView isKindOf: TextEdit.
	(#(#'browseMethodPragmas') includes: command) ifTrue: [aCommandQuery isEnabled: gciSession gsHasPragmas. ^true].
	(#(#'editSave') includes: command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [focusView isModified]). ^true].
	(#(#'editUndo') includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [focusView canUndo]). ^true].
	(#(#'editRedo') includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [focusView canRedo]). ^true].
	(#(#'editCut' #'editCopy' "#'editDelete'" ) includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [focusView hasSelection]). ^true].
	(#(#'editPaste') includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [Clipboard current isTextAvailable]). ^true].
	(#(#'addRepository') includes: command) ifTrue: [aCommandQuery isEnabled: packageListPresenter selections notEmpty. ^true].
	(#(#'showPackageChanges') includes: command) ifTrue: [
		aCommandQuery isEnabled: true . ^true].
	(#(#'savePackage') includes: command) ifTrue: [aCommandQuery isEnabled: packageListPresenter selections size = 1. ^true].
	(#(#'removeRepository') includes: command) ifTrue: [
		aCommandQuery isEnabled: (repositoryListPresenter hasSelection and: [packageListPresenter selections size = 1]). ^true].
	(#(#'compareAncestor') includes: command) ifTrue: [
		aCommandQuery isEnabled: (packageListPresenter selections size = 1 and: [ancestorListPresenter hasSelection]). ^true].
	(#(#'browseMethodHistory' #'browseImplementors' #'browseSenders' #'fileOutMethod') includes: command) ifTrue: [
		aCommandQuery isEnabled: methodListPresenter selections size = 1. ^true].
	(#(#'inspectGlobal' #'browseGlobalReferences') includes: command) ifTrue: [aCommandQuery isEnabled: globalsPresenter selections size = 1. ^true].
	(#(#'removeGlobals') includes: command) ifTrue: [aCommandQuery isEnabled: globalsPresenter selections notEmpty. ^true].
	"(#(#'runMethodTests') includes: command) ifTrue: [aCommandQuery isEnabled: (methodListPresenter selections notEmpty and: [methodListPresenter selections first at: 3]). ^true]."
	(#(#gotoMethodPackage) includes: command) ifTrue:[aCommandQuery isEnabled: self methodListPresenter selectionOrNil notNil. ^true].
	(#(#gotoClassPackage) includes: command) ifTrue:[aCommandQuery isEnabled: self classListPresenter selectionOrNil notNil. ^true].
	(#(#removeMethods) includes: command) ifTrue:[aCommandQuery isEnabled: self methodListPresenter selections notEmpty. ^true].
	(#(#'loadLatestVersion') includes: command) ifTrue: [
		aCommandQuery isEnabled: (packageListPresenter selections notEmpty and: [packageListPresenter selections allSatisfy: [:each | each key beginsWith: 'ConfigurationOf']]). ^true].
	(#(#'browseClassReferences' #'fileOutClass' #'addSubclass' #'addMissingAccessors' #'removeClass' #'removePriorVersions') includes: command) ifTrue: [
		aCommandQuery isEnabled: self selectedClasses size == 1. ^true.
	].
	(#(#'runClassTests') includes: command) ifTrue: [aCommandQuery isEnabled: selectedClassesAreTestCases. ^true].
	(#(#'browseSelectedClass') includes: command) ifTrue: [aCommandQuery isEnabled: self isClassSelectedInEditor. ^true].

	(command == #historyBack) ifTrue: [aCommandQuery isEnabled: self hasCommandHistoryBack. ^true].
	(command == #historyForward) ifTrue: [aCommandQuery isEnabled: self hasCommandHistoryForward. ^true].

	(command == #'setEnvironment0' and: [environment == 0]) ifTrue: [aCommandQuery isChecked: true. ^true].
	(command == #'setEnvironment1' and: [environment == 1]) ifTrue: [aCommandQuery isChecked: true. ^true].
	(command == #'setEnvironment2' and: [environment == 2]) ifTrue: [aCommandQuery isChecked: true. ^true].
	(self queryCommandHandlers: aCommandQuery) ifTrue: [^true].
	(aCommandQuery canPerformAgainst: self) ifTrue: [aCommandQuery beEnabled].
	^false	"There was no specific enablement of the command at this level"

!

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

	BrowserUpdate current register: projectListPresenter with: #projectsUpdate:.
	BrowserUpdate current register: packageListPresenter with: #projectPackagesUpdate:.
	BrowserUpdate current register: classListPresenter with: #classesUpdate:.
	BrowserUpdate current register: categoryListPresenter with: #filterUpdate:.
	BrowserUpdate current register: variableListPresenter with: #filterUpdate:.
	BrowserUpdate current register: methodListPresenter with: #classMethodsUpdate:.
	BrowserUpdate current register: methodListPresenter with: #methodsRemoved:. 
	BrowserUpdate current register: methodSourcePresenter with: #methodSourceUpdate:.!

removeClass
	| packageService selection |
	selection := classListPresenter selectionIfNone: [^self].
	(MessageBox confirm: 'Really delete class ' , selection name , '?') ifFalse: [^self].
	packageService := packageListPresenter selection.
	packageService
		command: #removeClass:;
		commandArgs: (Array with: selection).
	self issueCommand: (Array with: packageService)!

removeComparisonTab
	self textTabs remove: self comparisonTab ifAbsent: []!

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

showComparisonTab
	self textTabs add: self comparisonTab.
	methodSourcePresenter setDefaultMethod.
	comparisonPresenter ensureVisible!

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
	textAreaTabs currentCard name = 'projectInfo' ifTrue: [projectInfoTab ensureVisible].
	self updateMenuBar!

textTabs
	^(textAreaTabs subViews detect: [:each | each class == TabViewXP]) model!

unloadProject
	MessageBox notify: 'This feature not yet implemented'!

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
	self removeComparisonTab.
	methodListPresenter selections size = 1 ifTrue: [^self updateSingleMethodSource].
	self updateMultiMethodComparison!

updateMultiMethodComparison
	comparisonPresenter
		beforeTitle: (methodListPresenter selections at: 1) classMethodDisplayString;
		afterTitle: (methodListPresenter selections at: 2) classMethodDisplayString;
		compare: (methodListPresenter selections at: 1) source
			with: (methodListPresenter selections at: 2) source.
	self showComparisonTab!

updatePackage
	self updateClasses.
	self updatePackageInfo.!

updatePackageInfo
	| service |
	service := packageListPresenter selectionIfNone: [^self].
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
	"copied to RowanDebugger>>updatePresenters"

	self updateServices: (Array with: RowanBrowserService new)!

updateProjectTab
	| projectTabLabel |
	projectTabLabel := (projectListPresenter selectionIfNone: [])
				ifNil: ['Projects']
				ifNotNil: [:sel | sel name].
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
!JadeiteProjectsBrowserPresenter categoriesFor: #addPackageDictionaryInfoTo:!public!request string! !
!JadeiteProjectsBrowserPresenter categoriesFor: #basicAddMethodCategory:!menu handlers!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #basicRemoveMethods!menu handlers!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseHierarchyImplementors!menu handlers!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseHierarchyImplementorsOf:!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseHierarchySenders!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseHierarchySendersOf:!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseImplementorsOf:!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseMethodHistory!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseMethodsContaining!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseSendersOf:!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #checkout!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #classesMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #cloneProject!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #commit!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #comparisonTab!private!selection!text tabs! !
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
!JadeiteProjectsBrowserPresenter categoriesFor: #editSaveMethod!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #filterTabChanged!event handlers!public!services!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #getViews!public! !
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
!JadeiteProjectsBrowserPresenter categoriesFor: #onDropOnMethodCategory:!drag & drop!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onViewOpened!event handlers!overrides!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #openMethodListOn:selecting:!menu handlers!private!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectChanges!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectDictionaryChanged!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectLog!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #pullFromGit!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #pushToGit!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #queryCommand:!event handlers!overrides!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #refresh!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #refreshClass!event handlers!public!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #registerClassData!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #registerPresentersForUpdates!event handlers!initialization!public!register presenters! !
!JadeiteProjectsBrowserPresenter categoriesFor: #removeClass!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #removeComparisonTab!private!selection!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #removeMethodCategories!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #removeMethods:!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectClass:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectedMethodName!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectionMethod:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectMethodsFrom:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectorsString!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectPackage:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setClassMeta!event handlers!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setFilterType:!event handlers!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setMethodFilter!event handlers!private!services!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #showComparisonTab!private!selection!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #showPackages!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #textTabChanged!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #textTabs!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #unloadProject!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateAndSelectA:!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateClasses!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateClassInfo!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateCommand:onSuccessDo:!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateMenuBar!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateMethodReferences:!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateMethodSource!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateMultiMethodComparison!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updatePackage!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updatePackageInfo!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updatePackages!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateProject!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateProjectInfo!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateProjects!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateProjectTab!event handlers!public!updating! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 656 nil nil nil 5 nil nil nil 656 482 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 768 nil nil nil 5 nil nil nil 768 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 768 34 2 8 1409286144 131073 896 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 896 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Instance' 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 1136 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 1168 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 896 34 2 8 1140850688 131073 1280 nil nil nil 5 nil nil nil 1280 nil 170 192 8 #() nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 9 9 1490 567 1 1280 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 1490 193 193 nil 27 1074 1106 8 'Class' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 1648 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 1664 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 896 34 2 8 1140850688 131073 1760 nil nil nil 5 nil nil nil 1760 nil 170 192 1360 nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 9 9 1490 567 1 1760 1538 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 1600 nil 27 1280 170 192 34 4 1760 8 'classTab' 1280 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 896 34 2 8 1140916738 1 2064 590662 2 ##(Smalltalk.ListModel) 138 144 34 2 1120 1632 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) nil nil 1 nil nil nil 2064 nil 8 4294906827 1154 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 2240 7 257 nil 1154 0 nil 2258 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 2304 7 257 nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #smallIcons nil nil nil nil nil 1378 138 144 34 3 1442 #createAt:extent: 34 2 1490 1 1 1490 583 51 2064 1442 #basicSelectionsByIndex: 34 1 8 #(1) 2064 1442 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2064 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 25 0 0 0] 8 #() 1600 nil 27 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1 645 1490 583 51 896 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 66 1 0 0 35 1 0 0 91 1 0 0] 34 3 1280 1760 2064 1600 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2818 #fixedParentRight 1 2818 #fixedParentBottom -49 2818 #fixedViewTop 51 410 ##(Smalltalk.CardContainer) 34 16 nil 768 34 2 8 1409286144 131073 2896 nil 962 8 4278190080 nil 5 nil nil nil 2896 1010 138 144 34 2 1074 1106 8 'Classes' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 3072 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 3088 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 2896 34 2 8 1409372237 1025 3184 2130 138 144 1360 nil 2208 962 8 4278190080 nil 29 nil nil nil 3184 nil 8 4294903483 459270 ##(Smalltalk.Message) #displayString 8 #() nil 2368 nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 '' 559 #left 3330 #displayString 8 #() 3330 #<= 8 #() nil nil 3184 nil 3 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 3520 7 257 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 3600 7 257 nil #report 1360 nil 133217 nil 34 4 nil nil 1490 1 1 nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 9 55 1490 567 589 3184 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 1600 nil 27 1074 1106 8 'Hierarchy' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 3904 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 3920 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 2896 34 2 8 1409352231 1025 4016 590918 3 ##(Smalltalk.TreeModel) nil 2208 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 1360 962 8 4278190080 nil 29 nil nil nil 4016 nil 8 4294907113 1154 0 nil 1186 2 1 3328 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 4208 7 257 nil ##(Smalltalk.IconicListAbstract) 2368 nil nil nil nil nil 170 176 1360 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 1360 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 9 55 1490 567 589 4016 1538 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 1600 nil 27 3184 170 192 34 4 4016 8 'classHierarchy' 3184 8 'classList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 2896 34 2 8 1140916736 1 4544 2130 138 144 34 2 3056 3888 nil 2208 nil nil 1 nil nil nil 4544 nil 8 4294906827 1154 0 nil 2258 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 4672 7 257 nil 1154 0 nil 2258 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 4720 7 257 nil 2368 nil nil nil nil nil #smallIcons nil nil nil nil nil 1378 138 144 34 3 1442 #createAt:extent: 34 2 1490 1 1 1490 583 651 4544 1442 #basicSelectionsByIndex: 34 1 8 #(1) 4544 1442 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 4544 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 8 #() 1600 nil 27 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1 1 1490 583 651 2896 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 34 3 3184 4016 4544 1600 nil 27 2786 2832 1 2848 1 2818 #fixedParentTop 1 2864 -43 170 192 34 4 896 8 'instanceClassTabs' 2896 8 'classHierarchyTabs' nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 757 1 1490 583 695 768 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 122 1 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 2 2896 896 1600 nil 27 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 244 63 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 5456 nil nil nil 5 nil nil nil 5456 482 170 176 1360 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 5456 34 2 8 1140936777 1025 5584 2130 138 144 1360 nil 2208 962 8 4278190080 nil 21 nil nil nil 5584 nil 8 4294903483 3330 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 2368 nil nil nil nil nil nil 138 144 34 1 3410 8 'Column 1' 361 #left 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 5824 7 257 nil 3330 #<= 8 #() nil nil 5584 nil 3 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 5936 7 257 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 6016 7 257 nil #report 1360 nil 133217 nil 34 4 nil nil 1490 1 1 nil 1378 138 144 34 2 1442 #createAt:extent: 34 2 1490 1 1 1490 369 695 5584 1442 #text: 34 1 8 'Column 1' 5584 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 1600 nil 27 8 'packagesList' nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 379 1 1490 369 695 5456 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 189 0 0 0 0 0 0 0 117 1 0 0 91 1 0 0] 34 1 5584 1600 nil 27 524806 ##(Smalltalk.Fraction) 9 11 410 ##(Smalltalk.ListView) 34 30 nil 656 34 2 8 1409405257 1025 6544 2130 138 144 1360 nil 2208 962 3296 nil 5 nil nil nil 6544 nil 8 4294903483 3330 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 2368 nil nil nil nil nil nil 138 144 34 1 3410 8 'Column 1' 361 #left 3330 #displayString 8 #() ##(Smalltalk.SortedCollection) nil nil 6544 nil 3 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 6800 7 257 nil 1154 0 nil 1186 3 1 1152 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 6880 7 257 nil #report 1360 nil 133217 nil 34 4 nil nil 1490 1 1 nil 1378 138 144 34 2 1442 #createAt:extent: 34 2 1490 1 1 1490 369 695 6544 1442 #text: 34 1 8 'Column 1' 6544 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 1600 nil 27 6514 9 11 false 170 192 34 2 6544 8 'projectList' nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1 1 1490 1339 695 656 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 5 6544 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 7424 nil 962 8 4278190080 nil 517 nil nil nil 7424 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 369 1 1490 11 695 7424 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 184 0 0 0 0 0 0 0 189 0 0 0 91 1 0 0] 8 #() 1600 nil 27 5456 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 7680 nil 962 7504 nil 517 nil nil nil 7680 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 747 1 1490 11 695 7680 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 117 1 0 0 0 0 0 0 122 1 0 0 91 1 0 0] 8 #() 1600 nil 27 768 1600 nil 27 7 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 7920 nil nil nil 5 nil nil nil 7920 482 170 176 1360 false 170 192 34 2 410 ##(Smalltalk.CardContainer) 34 16 nil 7920 34 2 8 1409286144 131073 8048 nil 962 8 4278190080 nil 5 nil nil nil 8048 1010 138 144 34 2 1074 8 'Category' 410 ##(Smalltalk.ListView) 34 30 nil 8048 34 2 8 1409372233 1025 8224 2130 138 144 1360 nil 2208 962 8 4278190080 nil 21 nil nil nil 8224 nil 8 4294903483 3330 #displayString 8 #() nil 2368 nil nil nil nil nil nil 138 144 34 1 3410 8 'Column 1' 417 #left 3330 #displayString 8 #() ##(Smalltalk.SortedCollection) 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each]' 1779525917 8496 7 257 nil nil 8224 nil 3 nil nil #report 1360 nil 131169 nil 34 4 nil nil 1490 1 1 nil 1378 138 144 34 2 1442 #createAt:extent: 34 2 1490 9 55 1490 425 633 8224 1442 #text: 34 1 8 'Column 1' 8224 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 216 0 0 0 87 1 0 0] 8 #() 1600 nil 27 1074 8 'Variables' 410 ##(Smalltalk.ListView) 34 30 nil 8048 34 2 8 1409372233 1025 8832 2130 138 144 1360 nil 2208 962 8336 nil 21 nil nil nil 8832 nil 8 4294903483 3330 #displayString 8 #() nil 2368 nil nil nil nil nil nil 138 144 34 1 3410 8 'Column 1' 417 #left 3330 #displayString 8976 ##(Smalltalk.SortedCollection) nil nil 8832 nil 3 nil 1154 0 nil 1186 9 1 1152 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 9072 7 257 nil #report 1360 nil 131169 nil 34 4 nil nil 1490 1 1 nil 1378 138 144 34 2 1442 #createAt:extent: 34 2 1490 9 55 1490 425 633 8832 1442 #text: 34 1 8 'Column 1' 8832 1538 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 216 0 0 0 87 1 0 0] 8 #() 1600 nil 27 8224 170 192 34 4 8832 8 'variableList' 8224 8 'categoryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 8048 34 2 8 1140916736 1 9456 2130 138 144 34 2 8208 8816 nil 2208 nil nil 1 nil nil nil 9456 nil 8 4294906827 1154 0 nil 2258 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 9584 7 257 nil 1154 0 nil 2258 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 9632 7 257 nil 2368 nil nil nil nil nil #noIcons nil nil nil nil nil 1378 138 144 34 3 1442 #createAt:extent: 34 2 1490 1 1 1490 441 695 9456 1442 #basicSelectionsByIndex: 34 1 8 #(1) 9456 1442 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 9456 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 8 #() 1600 nil 27 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1 1 1490 441 695 8048 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 34 3 8224 8832 9456 1600 nil 27 8 'methodFilterTabs' nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1349 1 1490 893 695 7920 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 162 2 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 8048 410 ##(Smalltalk.Splitter) 34 12 nil 7920 34 2 8 1140850688 1 10256 nil 962 7504 nil 517 nil nil nil 10256 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 441 1 1490 11 695 10256 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 220 0 0 0 0 0 0 0 225 0 0 0 91 1 0 0] 8 #() 1600 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 7920 34 2 8 1140850688 131073 10496 nil nil nil 5 nil nil nil 10496 834 170 176 34 6 410 ##(Smalltalk.ComboBox) 34 17 nil 10496 34 2 8 1412498947 1025 10608 2130 138 144 1360 nil 2208 962 8 4278190080 nil 5 nil nil nil 10608 nil 8 4294908397 3330 #displayString 8 #() 1360 401 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1 649 1490 443 53 10608 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 68 1 0 0 221 0 0 0 94 1 0 0] 8 #() 1600 nil 27 2786 2832 1 2848 1 2864 -45 2880 47 410 ##(Smalltalk.ComboBox) 34 17 nil 10496 34 2 8 1412498947 1025 10960 2130 138 144 1360 nil 2208 962 10720 nil 5 nil nil nil 10960 nil 8 4294908397 3330 #displayString 8 #() 1360 401 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1 1 1490 443 53 10960 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 221 0 0 0 26 0 0 0] 8 #() 1600 nil 27 2786 2832 1 2848 1 5184 1 2880 47 410 ##(Smalltalk.ListView) 34 30 nil 10496 34 2 8 1409372233 1025 11296 2130 138 144 1360 nil 2208 962 8336 nil 13 nil nil nil 11296 nil 8 4294903483 3330 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 2368 nil nil nil nil nil nil 138 144 34 2 3410 8 '' 41 #left 1442 #empty 8 #() 16 3330 #<= 8 #() nil nil 11296 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each subSuperIcon]' 8 #[30 105 226 0 106] #subSuperIcon 11584 7 257 nil 1 nil nil 3410 8 'Selector' 395 #left 3330 #displayString 11440 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 11712 7 257 nil nil 11296 nil 3 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 11792 7 257 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 11872 7 257 nil #report 1360 nil 133221 nil 34 4 nil nil 1490 1 1 nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1 47 1490 443 603 11296 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 221 0 0 0 68 1 0 0] 8 #() 1600 nil 27 2786 2832 1 2848 1 5184 47 2864 -45 170 192 34 6 10608 8 'overrideList' 10960 8 'superclassList' 11296 8 'methodList' nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 451 1 1490 443 695 10496 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 225 0 0 0 0 0 0 0 190 1 0 0 91 1 0 0] 34 3 10960 11296 10608 1600 nil 27 1600 nil 27 5 false 170 192 1360 nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1 1 1490 2241 695 544 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 656 410 ##(Smalltalk.Splitter) 34 12 nil 544 34 2 8 1140850688 1 12576 nil 962 7504 nil 517 nil nil nil 12576 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1339 1 1490 11 695 12576 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 157 2 0 0 0 0 0 0 162 2 0 0 91 1 0 0] 8 #() 1600 nil 27 7920 1600 nil 27 6514 3 5 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 12832 nil 962 2976 nil 5 nil nil nil 12832 1010 138 144 34 8 1074 1106 8 'Package' 1154 0 nil 1186 7 1 1104 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 12992 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 13008 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 12832 34 2 8 1140850688 131073 13104 nil nil nil 5 nil nil nil 13104 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 13104 34 2 8 1409355853 1025 13216 2130 138 144 1360 nil 2208 962 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 5 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #addRepository 8 'Add &Repository' 1 1 nil nil nil 13394 1 13426 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 13394 1 13426 #savePackage 8 '&Save Package' 1 1 nil nil nil 13394 1 13426 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 13216 nil 8 4294903483 3330 #displayString 8 #() nil 2368 nil nil nil nil nil nil 138 144 34 4 3410 8 'Repository Type' 211 #left 3330 #displayString 13696 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 13792 7 257 nil nil 13216 nil 1 nil nil 3410 8 'Description' 1161 #left 3330 #displayString 8 #() 3330 #<= 13920 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 13952 7 257 nil nil 13216 nil 3 nil nil 3410 8 'User' 201 #left 3330 #displayString 8 #() 3330 #<= 14080 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 14112 7 257 nil nil 13216 nil 1 nil nil 3410 8 'Password' 201 #left 3330 #displayString 14080 3330 #<= 14080 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 14256 1154 0 nil 14272 19 257 nil 7 257 nil nil 13216 nil 1 nil nil #report 1360 nil 131169 nil 34 4 nil nil 1490 1 1 nil 1378 138 144 34 3 1442 #createAt:extent: 34 2 1490 1 263 1490 1769 217 13216 1442 #contextMenu: 34 1 13360 13216 1442 #text: 34 1 8 'Repository Type' 13216 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 1600 nil 27 19 410 ##(Smalltalk.ListView) 34 30 nil 13104 34 2 8 1409355853 1025 14624 2130 138 144 1360 nil 2208 962 13328 nil 5 13346 nil true 34 1 13394 1 13426 #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 14624 nil 8 4294903483 3330 #displayString 8 #() nil 2368 nil nil nil nil nil nil 138 144 34 4 3410 8 'Ancestor' 131 #left 3330 #displayString 14080 3330 #<= 14080 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 14976 7 257 nil nil 14624 nil 1 nil nil 3410 8 'Name' 401 #left 3330 #displayString 14864 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 15104 7 257 nil nil 14624 nil 1 nil nil 3410 8 'Timestamp' 301 #left 3330 #displayString 13920 3330 #<= 13920 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 15248 7 257 nil nil 14624 nil 1 nil nil 3410 8 'Message' 941 #left 3330 #displayString 13920 3330 #<= 13920 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 15392 7 257 nil nil 14624 nil 3 nil nil #report 1360 nil 131169 nil 34 4 nil nil 1490 1 1 nil 1378 138 144 34 3 1442 #createAt:extent: 34 2 1490 1 1 1490 1769 263 14624 1442 #contextMenu: 34 1 14736 14624 1442 #text: 34 1 8 'Ancestor' 14624 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 1600 nil 27 23 true 170 192 34 4 13216 8 'repositoryList' 14624 8 'ancestorList' nil 1378 138 144 34 2 1442 #createAt:extent: 34 2 1490 9 55 1490 2225 635 13104 1442 #text: 34 1 8 'Package' 13104 1538 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 34 2 14624 13216 1600 nil 27 1074 1106 8 'Globals' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 1104 16048 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 16064 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 12832 34 2 8 1409355853 1025 16160 2130 138 144 1360 nil 2208 962 13328 nil 5 nil nil nil 16160 nil 8 4294903483 3330 #displayString 8 #() nil 2368 nil nil nil nil nil nil 138 144 34 3 3410 8 'Name' 739 #left 3330 #displayString 16304 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 16400 7 257 nil nil 16160 nil 3 nil nil 3410 8 'Class' 741 #left 3330 #displayString 13920 3330 #<= 13920 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 16544 7 257 nil nil 16160 nil 3 nil nil 3410 8 'Value' 739 #left 3330 #displayString 13920 3330 #<= 13920 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 16688 7 257 nil nil 16160 nil 3 nil nil #report 1360 nil 131169 nil 34 4 nil nil 1490 1 1 nil 1378 138 144 34 2 1442 #createAt:extent: 34 2 1490 9 55 1490 2225 635 16160 1442 #text: 34 1 8 'Name' 16160 1538 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1600 nil 27 1074 1106 8 'SUnit' 1154 0 nil 1186 4 1 1152 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 17056 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 12832 34 2 8 1140850688 131073 17184 nil 962 8 4278190080 nil 5 nil nil nil 17184 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 9 55 1490 2225 635 17184 1538 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 1360 1600 nil 27 1074 1106 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 12832 34 2 8 1445007428 1025 17504 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 962 8 4278190080 nil 5 13346 nil true 34 11 13394 1 13426 #editSave 8 '&Save' 9383 1 nil nil nil 13522 4097 13394 1 13426 #editUndo 8 '&Undo' 9397 1 nil nil nil 13394 1 13426 #editRedo 8 '&Redo' 9395 1 nil nil nil 13522 4097 13394 1 13426 #editCut 8 'Cu&t' 9393 1 nil nil nil 13394 1 13426 #editCopy 8 '&Copy' 9351 1 nil nil nil 13394 1 13426 #editPaste 8 '&Paste' 9389 1 nil nil nil 13394 1 13426 #editDelete 8 '&Delete' 1629 1 nil nil nil 13522 4097 13394 1 13426 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1490 193 193 nil 17504 nil 8 4294907095 852486 ##(Smalltalk.NullConverter) nil nil 13 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 18322 77 786694 ##(Smalltalk.IndexedColor) 33554457 18370 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 18336 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 18352 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 1360 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 17504 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1360 nil 170 176 1360 9215 nil nil nil nil 18370 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 18336 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 17504 65025 3 false 1 nil nil nil nil nil nil 3 18786 3 17504 33423361 5 false 3 nil nil nil nil nil nil 5 18786 5 17504 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18322 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18322 1027 18898 60239 18898 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18322 1031 18898 335 18898 31317503 1 nil nil nil nil #Error nil nil nil nil 1378 138 144 34 8 1442 #createAt:extent: 34 2 1490 9 55 1490 2225 635 17504 1442 #contextMenu: 34 1 17664 17504 1442 #modificationEventMask: 8 #(4607) 17504 1442 #caretPeriod: 8 #(530) 17504 1442 #wordWrap: 8 #(true) 17504 1442 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 17504 1 3 nil nil 5 nil 19314 3 17504 33 nil 3 67108863 5 nil 19314 5 17504 1 nil 3 -67108863 5 nil 17504 1442 #tabWidth: 8 #(4) 17504 1442 #maxCompletionListHeight: 8 #(9) 17504 1538 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1600 nil 29 1074 1106 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 12832 34 2 8 1445007428 1025 19536 17570 nil false 17616 nil 962 17648 nil 5 13346 nil true 34 1 13394 1 13426 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 18146 nil true 18178 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1490 193 193 nil 19536 nil 8 4294907095 18258 nil nil 11 nil 170 192 34 4 #normal 18322 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 18322 77 18384 18400 1 nil nil nil nil #callTip nil nil nil 34 256 19856 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 19872 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 18514 1 nil nil nil 19536 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1360 nil 170 176 1360 9215 nil nil nil nil 18576 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 19856 nil nil nil nil 1 nil 170 192 34 6 1 18786 1 19536 65025 3 false 1 nil nil nil nil nil nil 3 18786 3 19536 33423361 5 false 3 nil nil nil nil nil nil 5 18786 5 19536 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18322 1029 nil 18898 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18322 1027 18898 60239 18898 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18322 1031 18898 335 18898 31317503 1 nil nil nil nil #Error nil nil nil nil 1378 138 144 34 8 1442 #createAt:extent: 34 2 1490 9 55 1490 2225 635 19536 1442 #contextMenu: 34 1 19632 19536 1442 #modificationEventMask: 8 #(4607) 19536 1442 #caretPeriod: 8 #(530) 19536 1442 #wordWrap: 8 #(true) 19536 1442 #margins: 34 1 34 3 19314 1 19536 1 3 nil nil 5 nil 19314 3 19536 33 nil 3 67108863 5 nil 19314 5 19536 1 nil 3 -67108863 5 nil 19536 1442 #tabWidth: 8 #(4) 19536 1442 #maxCompletionListHeight: 8 #(9) 19536 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1600 nil 29 1074 1106 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 12832 34 2 8 1445007428 1025 20832 17570 nil false 17616 nil 962 8 4278190080 nil 5 nil 18146 nil true 18178 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1490 193 193 nil 20832 nil 8 4294907095 18258 nil nil 13 nil 170 192 34 4 #normal 18322 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 18322 77 18384 18400 1 nil nil nil nil #callTip nil nil nil 34 256 21072 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 21088 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 1360 170 192 34 2 #default 18514 1 nil nil nil 20832 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1360 nil 170 176 1360 9215 nil nil nil nil 18576 nil nil 65 nil nil 18592 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 21072 #callTip 21088 nil nil nil nil 1 nil 170 192 34 12 1 18786 1 20832 65025 3 false 1 nil nil nil nil nil nil 3 18786 3 20832 33423361 5 false 3 nil nil nil nil nil nil 5 18786 5 20832 511 1 false 5 nil nil nil nil nil nil 8 'indicator10' 18786 21 20832 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator9' 18786 19 20832 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator8' 18786 17 20832 33554447 1 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18322 1029 nil 18898 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18322 1027 18898 60239 18898 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18322 1031 18898 335 18898 31317503 1 nil nil nil nil #Error nil nil nil nil 1378 138 144 34 8 1442 #createAt:extent: 34 2 1490 9 55 1490 2225 635 20832 1442 #modificationEventMask: 8 #(4607) 20832 1442 #hoverTime: 8 #(200) 20832 1442 #caretPeriod: 8 #(530) 20832 1442 #wordWrap: 8 #(true) 20832 1442 #margins: 34 1 34 3 19314 1 20832 61 3 nil nil 5 nil 19314 3 20832 1 nil 3 67108863 nil nil 19314 5 20832 1 nil 3 -67108863 nil nil 20832 1442 #tabWidth: 8 #(4) 20832 1442 #maxCompletionListHeight: 8 #(9) 20832 1538 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1600 nil 29 1074 1106 8 'Project' 1154 0 nil 1186 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 17138 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 22160 7 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 12832 34 2 8 1409372237 1025 22272 2130 138 144 1360 nil 2208 962 13328 nil 5 nil nil nil 22272 nil 8 4294903483 3330 #displayString 16304 nil 2368 nil nil nil nil nil nil 138 144 34 2 3410 8 'Name' 201 #left 3330 #displayString 16304 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each | (each at: 2)]' 8 #[29 105 17 64 148 106] 22496 7 257 nil nil 22272 nil 1 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each item at: 1) displayStringFor: each]' 8 #[31 105 226 0 63 148 17 177 106] #item #displayStringFor: 22576 7 257 nil 3410 8 'Value' 2017 #left 3330 #displayString 8 #() 3330 #<= 22704 1154 0 nil 1186 4 1 1152 8 'doIt' 8 '[:each | (each at: 2) ifNil:[String new] ifNotNil: [:iv |
(each at: 1) instVarNamed: iv]]' 8 #[32 105 17 64 148 100 223 4 97 45 146 106 90 17 63 148 18 178 106] 17138 #String 16 #ifNil:ifNotNil: #instVarNamed: 22736 7 65793 nil nil 22272 nil 3 nil nil #report 1360 nil 131169 nil 34 4 nil nil 1490 1 1 nil 1378 138 144 34 2 1442 #createAt:extent: 34 2 1490 9 55 1490 2225 635 22272 1442 #text: 34 1 8 'Name' 22272 1538 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1600 nil 27 1074 8 'Comparison' 410 ##(Smalltalk.ShellView) 34 27 nil 12832 34 2 8 1078919168 131073 23104 nil 962 8 4278190080 nil 517 nil nil nil 23104 788230 ##(Smalltalk.BorderLayout) 1 1 nil nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 23104 34 2 8 1140850688 131073 23232 nil 962 23184 nil 5 13346 nil true 34 5 13394 1 13426 #toggleIndentGuides 8 'Indentation &Guides' 1 1 nil nil nil 13394 1 13426 #toggleVisibleLineEndings 8 'Line &Endings' 1 1 nil nil nil 13394 1 13426 #toggleLineNumberMargins 8 'Line N&umbers' 1 1 nil nil nil 13394 1 13426 #toggleVisibleWhitespace 8 'W&hitespace' 1 1 nil nil nil 13394 1 13426 #toggleWrapping 8 '&Word Wrap' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 23232 482 170 176 1360 false 170 192 1360 nil 1378 138 144 34 2 1442 #createAt:extent: 34 2 1490 1 1 1490 2193 603 23232 1442 #contextMenu: 34 1 23312 23232 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 23232 34 2 8 1140850688 131073 23840 nil 962 23184 nil 5 nil nil nil 23840 23202 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 23840 34 2 8 1140850945 262145 23936 nil nil nil 5 nil nil nil 23936 nil 8 1660708144 18258 nil nil nil 1378 138 144 34 2 1442 #createAt:extent: 34 2 1490 1 563 1490 1091 41 23936 1442 #text: 34 1 8 '
' 23936 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 33 2 0 0 45 1 0 0] 8 #() 1600 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 23840 34 2 8 1140920580 1025 24240 17570 nil false 17616 nil 962 23184 nil 5 nil nil nil 24240 nil 8 4294907095 18258 nil nil 11 nil 170 192 34 12 #indentGuide 18322 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 18322 3 18370 33554445 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 18322 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 18322 77 18384 18400 1 nil nil nil nil #callTip nil nil nil #normal 18322 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 18322 5 nil 18898 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 24480 24416 24496 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 24448 nil nil nil 24400 nil nil 1310982 ##(Smalltalk.DiffsScintillaStyler) 8 #() 170 192 34 2 #default 18514 1 nil nil nil 24240 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1360 nil 170 176 1360 9215 nil nil nil nil 18576 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 24368 nil nil nil nil 1 nil 170 192 34 6 8 'indicator1' 18786 3 24240 33423361 5 false nil nil nil nil nil nil nil 8 'indicator2' 18786 5 24240 511 1 false nil nil nil nil nil nil nil 8 'indicator0' 18786 1 24240 33554459 13 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18322 1029 nil 18898 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18322 1027 18898 60239 18898 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18322 1031 18898 335 18898 31317503 1 nil nil nil nil #Error nil nil nil nil 1378 138 144 34 7 1442 #createAt:extent: 34 2 1490 1 1 1490 1091 563 24240 1442 #modificationEventMask: 8 #(4607) 24240 1442 #caretPeriod: 8 #(530) 24240 1442 #wordWrap: 8 #(true) 24240 1442 #margins: 34 1 34 3 19314 1 24240 49 3 nil nil nil nil 19314 3 24240 1 nil nil 67108863 nil nil 19314 5 24240 1 nil nil nil nil nil 24240 1442 #canHScroll: 8 #(false) 24240 1442 #maxCompletionListHeight: 8 #(9) 24240 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 25 1 0 0] 8 #() 1600 nil 29 170 192 34 4 24240 8 'before' 23936 8 'beforeTitle' nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1 1 1490 1091 603 23840 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 45 1 0 0] 34 2 24240 23936 1600 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 23232 34 2 8 1140850688 1 25648 nil 18898 66047 nil 517 nil nil nil 25648 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1091 1 1490 11 603 25648 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 33 2 0 0 0 0 0 0 38 2 0 0 45 1 0 0] 8 #() 1600 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 23232 34 2 8 1140850688 131073 25888 nil 962 23184 nil 5 nil nil nil 25888 23202 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 25888 34 2 8 1140850945 262145 25984 nil nil nil 5 nil nil nil 25984 nil 8 1660708144 18258 nil nil nil 1378 138 144 34 2 1442 #createAt:extent: 34 2 1490 1 563 1490 1093 41 25984 1442 #text: 34 1 8 '
' 25984 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 34 2 0 0 45 1 0 0] 8 #() 1600 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 25888 34 2 8 1140920580 1025 26288 17570 nil false 17616 nil 962 23184 nil 5 nil nil nil 26288 nil 8 4294907095 18258 nil nil 11 nil 170 192 34 12 #indentGuide 18322 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 18322 3 24432 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 18322 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 18322 77 18384 18400 1 nil nil nil nil #callTip nil nil nil #normal 18322 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 18322 5 nil 18898 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 26512 26464 26528 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 26480 nil nil nil 26448 nil nil 24546 24576 170 192 34 2 #default 18514 1 nil nil nil 26288 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1360 nil 170 176 1360 9215 nil nil nil nil 18576 nil nil 65 nil nil nil 24672 3 170 192 34 2 #container 26416 nil nil nil nil 1 nil 170 192 34 6 8 'indicator1' 18786 3 26288 33423361 5 false nil nil nil nil nil nil nil 8 'indicator0' 18786 1 26288 33554459 13 false nil nil nil nil nil nil nil 8 'indicator2' 18786 5 26288 511 1 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18322 1029 nil 18898 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18322 1027 18898 60239 18898 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18322 1031 18898 335 18898 31317503 1 nil nil nil nil #Error nil nil nil nil 1378 138 144 34 7 1442 #createAt:extent: 34 2 1490 1 1 1490 1093 563 26288 1442 #modificationEventMask: 8 #(4607) 26288 1442 #caretPeriod: 8 #(530) 26288 1442 #wordWrap: 8 #(true) 26288 1442 #margins: 34 1 34 3 19314 1 26288 49 3 nil nil nil nil 19314 3 26288 1 nil nil 67108863 nil nil 19314 5 26288 1 nil nil nil nil nil 26288 1442 #canHScroll: 8 #(false) 26288 1442 #maxCompletionListHeight: 8 #(9) 26288 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 34 2 0 0 25 1 0 0] 8 #() 1600 nil 29 170 192 34 4 25984 8 'afterTitle' 26288 8 'after' nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1101 1 1490 1093 603 25888 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 2 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 2 26288 25984 1600 nil 27 1600 nil 27 170 192 34 2 23232 8 'diffs' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 13346 nil true 34 4 13394 1 13426 #selectUpper 8 'Select &upper' 1 1 nil nil nil 13394 1 13426 #selectLower 8 'Select &lower' 1 1 nil nil nil 13522 4097 13394 1 13426 #exit 8 '&Close' 1 1 nil nil nil 8 '&File' nil 1 nil nil 10593 nil nil 13346 nil true 34 3 13394 1 13426 #character 8 '&Character' 1 1 nil nil nil 13394 1 13426 #word 8 '&Word' 1 1 nil nil nil 13394 1 13426 #line 8 '&Line' 1 1 nil nil nil 8 '&Mode' nil 1 nil nil 10595 nil nil 13346 nil true 34 1 13394 1 13426 #about 8 '&About' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 10597 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1378 138 144 34 3 1442 #createAt:extent: 34 2 1490 9 55 1490 2225 635 23104 1442 #text: 34 1 8 'Comparison' 23104 1442 #updateMenuBar 1360 23104 1538 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 34 1 23232 1600 nil 27 19536 170 192 34 16 13104 8 'packageInfo' 23104 8 'comparisonTab' 19536 8 'classDocumentation' 17184 8 'sunit' 16160 8 'globals' 17504 8 'classDefinition' 22272 8 'projectInfo' 20832 8 'methodSource' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 12832 34 2 8 1140916736 1 28624 2130 138 144 34 8 22128 12976 16032 17472 19504 20800 17024 23088 nil 2208 nil nil 1 nil nil nil 28624 nil 8 4294906827 1154 0 nil 2258 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 28752 7 257 nil 1154 0 nil 2258 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 28800 7 257 nil 2368 nil nil nil nil nil #smallIcons nil nil nil nil nil 1378 138 144 34 3 1442 #createAt:extent: 34 2 1490 1 1 1490 2241 697 28624 1442 #basicSelectionsByIndex: 34 1 8 #(5) 28624 1442 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 28624 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 92 1 0 0] 8 #() 1600 nil 27 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1 705 1490 2241 697 12832 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 96 1 0 0 96 4 0 0 188 2 0 0] 34 9 22272 13104 16160 17504 19536 20832 17184 23104 28624 1600 nil 27 6514 3 5 true 170 192 34 4 12832 8 'textAreaTabs' 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 29312 nil 962 7504 nil 517 nil nil nil 29312 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 1 695 1490 2241 11 29312 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 91 1 0 0 96 4 0 0 96 1 0 0] 8 #() 1600 nil 27 8 'splitter' nil 1378 138 144 34 1 1442 #createAt:extent: 34 2 1490 7679 21 1490 2241 1401 416 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 95 19 0 0 198 2 0 0] 34 3 544 29312 12832 1600 nil 27 )! !
!JadeiteProjectsBrowserPresenter class categoriesFor: #downArrowIcon!public! !
!JadeiteProjectsBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

