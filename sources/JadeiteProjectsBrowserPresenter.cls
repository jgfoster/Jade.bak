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
	| caretPosition firstVisibleLine service |
	caretPosition := methodSourcePresenter view caretPosition.
	firstVisibleLine := methodSourcePresenter view lineScroll.
	service := methodListPresenter selectionIfNone: [^self].
	service selectedPackageServices: packageListPresenter selections.
	(self isCategoriesTabSelected and: [categoryListPresenter hasSelection])
		ifTrue: [service category: categoryListPresenter selection trimBlanks].
	service source: self currentMethodSource.
	[self updateServices: (Array with: service)] on: GsCompileError
		do: [:ex | self reportCompileError: ex list].
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

methodSourceEqual
	^methodListPresenter selectionOrNil
		ifNil: [true]
		ifNotNil: [:selection | methodSourcePresenter value = selection source]!

newService
	^(RowanProjectService on: gciSession) name: projectListPresenter selection name!

onViewOpened
	"copied to RowanDebugger"

	super onViewOpened.
	self registerPresentersForUpdates.
	projectListPresenter view contextMenu: (Menu fromStrings: self projectMenuStrings).
	self rowanFixMe. "We're overriding saved jade layout info. Restore it someday. "
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
!JadeiteProjectsBrowserPresenter categoriesFor: #methodSourceEqual!public!testing! !
!JadeiteProjectsBrowserPresenter categoriesFor: #newService!project menu handlers!public! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 544 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 544 655878 ##(Smalltalk.CardLayout) 138 144 34 8 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Package' 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 7 1 752 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 784 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 816 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 928 nil nil nil 5 nil nil nil 928 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 928 34 2 8 1409355853 1025 1040 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 610 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 1040 nil 8 4294910247 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Ancestor' 131 #left 1394 #displayString 8 #() 1394 #<= 1568 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 1600 7 257 nil nil 1040 nil 1 nil nil 1506 8 'Name' 401 #left 1394 #displayString 1424 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 1728 7 257 nil nil 1040 nil 1 nil nil 1506 8 'Timestamp' 301 #left 1394 #displayString 8 #() 1394 #<= 1856 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 1888 7 257 nil nil 1040 nil 1 nil nil 1506 8 'Message' 941 #left 1394 #displayString 1856 1394 #<= 1856 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 2032 7 257 nil nil 1040 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 328198 ##(Smalltalk.Point) 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 2130 1 1 2130 1769 263 1040 2226 #contextMenu: 34 1 1248 1040 2226 #text: 34 1 8 'Ancestor' 1040 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 2130 193 193 nil 27 23 410 ##(Smalltalk.ListView) 34 30 nil 928 34 2 8 1409355853 1025 2464 1106 138 144 1152 nil 1184 610 1216 nil 5 1234 nil true 34 5 1282 1 1314 #addRepository 8 'Add &Repository' 1 1 nil nil nil 1282 1 1314 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 1282 1 1314 #savePackage 8 '&Save Package' 1 1 nil nil nil 1282 1 1314 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 2464 nil 8 4294910247 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 4 1506 8 'Repository Type' 211 #left 1394 #displayString 2880 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 2976 7 257 nil nil 2464 nil 1 nil nil 1506 8 'Description' 1161 #left 1394 #displayString 1856 1394 #<= 1856 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 3120 7 257 nil nil 2464 nil 3 nil nil 1506 8 'User' 201 #left 1394 #displayString 1568 1394 #<= 1568 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 3264 7 257 nil nil 2464 nil 1 nil nil 1506 8 'Password' 201 #left 1394 #displayString 1568 1394 #<= 1568 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 3408 802 0 nil 3424 19 257 nil 7 257 nil nil 2464 nil 1 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 263 2130 1769 217 2464 2226 #contextMenu: 34 1 2576 2464 2226 #text: 34 1 8 'Repository Type' 2464 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 2448 nil 27 19 true 170 192 34 4 1040 8 'ancestorList' 2464 8 'repositoryList' nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 9 55 2130 2225 635 928 2226 #text: 34 1 8 'Package' 928 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 34 2 1040 2464 2448 nil 27 722 754 8 'Globals' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 4080 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 4096 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 544 34 2 8 1409355853 1025 4192 1106 138 144 1152 nil 1184 610 1216 nil 5 nil nil nil 4192 nil 8 4294910247 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 3 1506 8 'Name' 739 #left 1394 #displayString 4336 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 4432 7 257 nil nil 4192 nil 3 nil nil 1506 8 'Class' 741 #left 1394 #displayString 1856 1394 #<= 1856 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 4576 7 257 nil nil 4192 nil 3 nil nil 1506 8 'Value' 739 #left 1394 #displayString 1856 1394 #<= 1856 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 4720 7 257 nil nil 4192 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 9 55 2130 2225 635 4192 2226 #text: 34 1 8 'Name' 4192 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 2448 nil 27 722 754 8 'SUnit' 802 0 nil 834 4 1 800 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 5088 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 544 34 2 8 1140850688 131073 5216 nil 610 8 4278190080 nil 5 nil nil nil 5216 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 55 2130 2225 635 5216 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 1152 2448 nil 27 722 754 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 5536 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 610 8 4278190080 nil 5 1234 nil true 34 11 1282 1 1314 #editSave 8 '&Save' 9383 1 nil nil nil 2706 4097 1282 1 1314 #editUndo 8 '&Undo' 9397 1 nil nil nil 1282 1 1314 #editRedo 8 '&Redo' 9395 1 nil nil nil 2706 4097 1282 1 1314 #editCut 8 'Cu&t' 9393 1 nil nil nil 1282 1 1314 #editCopy 8 '&Copy' 9351 1 nil nil nil 1282 1 1314 #editPaste 8 '&Paste' 9389 1 nil nil nil 1282 1 1314 #editDelete 8 '&Delete' 1629 1 nil nil nil 2706 4097 1282 1 1314 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2130 193 193 nil 5536 nil 8 4294907031 852486 ##(Smalltalk.NullConverter) nil nil 13 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 6354 77 786694 ##(Smalltalk.IndexedColor) 33554457 6402 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 6368 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 6384 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 1152 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 5536 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 6402 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 6368 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 5536 65025 3 false 1 nil nil nil nil nil nil 3 6818 3 5536 33423361 5 false 3 nil nil nil nil nil nil 5 6818 5 5536 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 6354 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 6354 1027 6930 60239 6930 31326207 1 nil nil nil nil #Warning nil nil nil #Error 6354 1031 6930 335 6930 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 8 2226 #createAt:extent: 34 2 2130 9 55 2130 2225 635 5536 2226 #contextMenu: 34 1 5696 5536 2226 #modificationEventMask: 8 #(4607) 5536 2226 #caretPeriod: 8 #(530) 5536 2226 #wordWrap: 8 #(true) 5536 2226 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 5536 1 3 nil nil 5 nil 7346 3 5536 33 nil 3 67108863 5 nil 7346 5 5536 1 nil 3 -67108863 5 nil 5536 2226 #tabWidth: 8 #(4) 5536 2226 #maxCompletionListHeight: 8 #(9) 5536 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 2448 nil 29 722 754 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 7568 5602 nil false 5648 nil 610 5680 nil 5 1234 nil true 34 1 1282 1 1314 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 6178 nil true 6210 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2130 193 193 nil 7568 nil 8 4294907031 6290 nil nil 11 nil 170 192 34 4 #normal 6354 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 6354 77 6416 6432 1 nil nil nil nil #callTip nil nil nil 34 256 7888 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 7904 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 6546 1 nil nil nil 7568 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 6608 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 7888 nil nil nil nil 1 nil 170 192 34 6 1 6818 1 7568 65025 3 false 1 nil nil nil nil nil nil 3 6818 3 7568 33423361 5 false 3 nil nil nil nil nil nil 5 6818 5 7568 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 6354 1029 nil 6930 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 6354 1027 6930 60239 6930 31326207 1 nil nil nil nil #Warning nil nil nil #Error 6354 1031 6930 335 6930 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 8 2226 #createAt:extent: 34 2 2130 9 55 2130 2225 635 7568 2226 #contextMenu: 34 1 7664 7568 2226 #modificationEventMask: 8 #(4607) 7568 2226 #caretPeriod: 8 #(530) 7568 2226 #wordWrap: 8 #(true) 7568 2226 #margins: 34 1 34 3 7346 1 7568 1 3 nil nil 5 nil 7346 3 7568 33 nil 3 67108863 5 nil 7346 5 7568 1 nil 3 -67108863 5 nil 7568 2226 #tabWidth: 8 #(4) 7568 2226 #maxCompletionListHeight: 8 #(9) 7568 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 2448 nil 29 722 754 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 8864 5602 nil false 5648 nil 610 8 4278190080 nil 5 nil 6178 nil true 6210 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2130 193 193 nil 8864 nil 8 4294907031 6290 nil nil 13 nil 170 192 34 4 #normal 6354 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 6354 77 6416 6432 1 nil nil nil nil #callTip nil nil nil 34 256 9104 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 9120 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 1152 170 192 34 2 #default 6546 1 nil nil nil 8864 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 6608 nil nil 65 nil nil 6624 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 9104 #callTip 9120 nil nil nil nil 1 nil 170 192 34 12 1 6818 1 8864 65025 3 false 1 nil nil nil nil nil nil 3 6818 3 8864 33423361 5 false 3 nil nil nil nil nil nil 5 6818 5 8864 511 1 false 5 nil nil nil nil nil nil 8 'indicator8' 6818 17 8864 33554447 1 nil nil nil nil nil nil nil nil 8 'indicator9' 6818 19 8864 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator10' 6818 21 8864 33554459 3 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 6354 1029 nil 6930 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 6354 1027 6930 60239 6930 31326207 1 nil nil nil nil #Warning nil nil nil #Error 6354 1031 6930 335 6930 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 8 2226 #createAt:extent: 34 2 2130 9 55 2130 2225 635 8864 2226 #modificationEventMask: 8 #(4607) 8864 2226 #hoverTime: 8 #(200) 8864 2226 #caretPeriod: 8 #(530) 8864 2226 #wordWrap: 8 #(true) 8864 2226 #margins: 34 1 34 3 7346 1 8864 61 3 nil nil 5 nil 7346 3 8864 1 nil 3 67108863 nil nil 7346 5 8864 1 nil 3 -67108863 nil nil 8864 2226 #tabWidth: 8 #(4) 8864 2226 #maxCompletionListHeight: 8 #(9) 8864 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 2448 nil 29 722 754 8 'Project' 802 0 nil 834 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 5170 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 10192 7 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 544 34 2 8 1409372237 1025 10304 1106 138 144 1152 nil 1184 610 1216 nil 5 nil nil nil 10304 nil 8 4294910247 1394 #displayString 4336 nil 1456 nil nil nil nil nil nil 138 144 34 2 1506 8 'Name' 201 #left 1394 #displayString 4336 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | (each at: 2)]' 8 #[29 105 17 64 148 106] 10528 7 257 nil nil 10304 nil 1 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each item at: 1) displayStringFor: each]' 8 #[31 105 226 0 63 148 17 177 106] #item #displayStringFor: 10608 7 257 nil 1506 8 'Value' 2017 #left 1394 #displayString 8 #() 1394 #<= 10736 802 0 nil 834 4 1 800 8 'doIt' 8 '[:each | (each at: 2) ifNil:[String new] ifNotNil: [:iv |
(each at: 1) instVarNamed: iv]]' 8 #[32 105 17 64 148 100 223 4 97 45 146 106 90 17 63 148 18 178 106] 5170 #String 16 #ifNil:ifNotNil: #instVarNamed: 10768 7 65793 nil nil 10304 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 9 55 2130 2225 635 10304 2226 #text: 34 1 8 'Name' 10304 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 2448 nil 27 722 8 'Comparison' 410 ##(Smalltalk.ShellView) 34 27 nil 544 34 2 8 1078919168 131073 11136 nil 610 8 4278190080 nil 517 nil nil nil 11136 788230 ##(Smalltalk.BorderLayout) 1 1 nil nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 11136 34 2 8 1140850688 131073 11264 nil 610 11216 nil 5 1234 nil true 34 5 1282 1 1314 #toggleIndentGuides 8 'Indentation &Guides' 1 1 nil nil nil 1282 1 1314 #toggleVisibleLineEndings 8 'Line &Endings' 1 1 nil nil nil 1282 1 1314 #toggleLineNumberMargins 8 'Line N&umbers' 1 1 nil nil nil 1282 1 1314 #toggleVisibleWhitespace 8 'W&hitespace' 1 1 nil nil nil 1282 1 1314 #toggleWrapping 8 '&Word Wrap' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 11264 482 170 176 1152 false 170 192 1152 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 1 2130 2193 603 11264 2226 #contextMenu: 34 1 11344 11264 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 11264 34 2 8 1140850688 131073 11872 nil 610 11216 nil 5 nil nil nil 11872 11234 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 11872 34 2 8 1140850945 262145 11968 nil nil nil 5 nil nil nil 11968 nil 8 1660708144 6290 nil nil nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 563 2130 1091 41 11968 2226 #text: 34 1 8 '
' 11968 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 33 2 0 0 45 1 0 0] 8 #() 2448 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 11872 34 2 8 1140920580 1025 12272 5602 nil false 5648 nil 610 11216 nil 5 nil nil nil 12272 nil 8 4294907031 6290 nil nil 11 nil 170 192 34 12 #indentGuide 6354 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 6354 3 6402 33554445 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 6354 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 6354 77 6416 6432 1 nil nil nil nil #callTip nil nil nil #normal 6354 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 6354 5 nil 6930 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 12512 12448 12528 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 12480 nil nil nil 12432 nil nil 1310982 ##(Smalltalk.DiffsScintillaStyler) 8 #() 170 192 34 2 #default 6546 1 nil nil nil 12272 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 6608 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 12400 nil nil nil nil 1 nil 170 192 34 6 8 'indicator1' 6818 3 12272 33423361 5 false nil nil nil nil nil nil nil 8 'indicator0' 6818 1 12272 33554459 13 false nil nil nil nil nil nil nil 8 'indicator2' 6818 5 12272 511 1 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 6354 1029 nil 6930 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 6354 1027 6930 60239 6930 31326207 1 nil nil nil nil #Warning nil nil nil #Error 6354 1031 6930 335 6930 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 7 2226 #createAt:extent: 34 2 2130 1 1 2130 1091 563 12272 2226 #modificationEventMask: 8 #(4607) 12272 2226 #caretPeriod: 8 #(530) 12272 2226 #wordWrap: 8 #(true) 12272 2226 #margins: 34 1 34 3 7346 1 12272 49 3 nil nil nil nil 7346 3 12272 1 nil nil 67108863 nil nil 7346 5 12272 1 nil nil nil nil nil 12272 2226 #canHScroll: 8 #(false) 12272 2226 #maxCompletionListHeight: 8 #(9) 12272 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 25 1 0 0] 8 #() 2448 nil 29 170 192 34 4 11968 8 'beforeTitle' 12272 8 'before' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 1091 603 11872 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 45 1 0 0] 34 2 12272 11968 2448 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 11264 34 2 8 1140850688 1 13680 nil 6930 66047 nil 517 nil nil nil 13680 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1091 1 2130 11 603 13680 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 33 2 0 0 0 0 0 0 38 2 0 0 45 1 0 0] 8 #() 2448 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 11264 34 2 8 1140850688 131073 13920 nil 610 11216 nil 5 nil nil nil 13920 11234 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 13920 34 2 8 1140850945 262145 14016 nil nil nil 5 nil nil nil 14016 nil 8 1660708144 6290 nil nil nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 563 2130 1093 41 14016 2226 #text: 34 1 8 '
' 14016 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 34 2 0 0 45 1 0 0] 8 #() 2448 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13920 34 2 8 1140920580 1025 14320 5602 nil false 5648 nil 610 11216 nil 5 nil nil nil 14320 nil 8 4294907031 6290 nil nil 11 nil 170 192 34 12 #indentGuide 6354 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 6354 3 12464 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 6354 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 6354 77 6416 6432 1 nil nil nil nil #callTip nil nil nil #normal 6354 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 6354 5 nil 6930 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 14544 14496 14560 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 14512 nil nil nil 14480 nil nil 12578 12608 170 192 34 2 #default 6546 1 nil nil nil 14320 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 6608 nil nil 65 nil nil nil 12704 3 170 192 34 2 #container 14448 nil nil nil nil 1 nil 170 192 34 6 8 'indicator2' 6818 5 14320 511 1 false nil nil nil nil nil nil nil 8 'indicator0' 6818 1 14320 33554459 13 false nil nil nil nil nil nil nil 8 'indicator1' 6818 3 14320 33423361 5 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 6354 1029 nil 6930 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 6354 1027 6930 60239 6930 31326207 1 nil nil nil nil #Warning nil nil nil #Error 6354 1031 6930 335 6930 31317503 1 nil nil nil nil #Error nil nil nil nil 2162 138 144 34 7 2226 #createAt:extent: 34 2 2130 1 1 2130 1093 563 14320 2226 #modificationEventMask: 8 #(4607) 14320 2226 #caretPeriod: 8 #(530) 14320 2226 #wordWrap: 8 #(true) 14320 2226 #margins: 34 1 34 3 7346 1 14320 49 3 nil nil nil nil 7346 3 14320 1 nil nil 67108863 nil nil 7346 5 14320 1 nil nil nil nil nil 14320 2226 #canHScroll: 8 #(false) 14320 2226 #maxCompletionListHeight: 8 #(9) 14320 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 34 2 0 0 25 1 0 0] 8 #() 2448 nil 29 170 192 34 4 14320 8 'after' 14016 8 'afterTitle' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1101 1 2130 1093 603 13920 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 2 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 2 14320 14016 2448 nil 27 2448 nil 27 170 192 34 2 11264 8 'diffs' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 1234 nil true 34 4 1282 1 1314 #selectUpper 8 'Select &upper' 1 1 nil nil nil 1282 1 1314 #selectLower 8 'Select &lower' 1 1 nil nil nil 2706 4097 1282 1 1314 #exit 8 '&Close' 1 1 nil nil nil 8 '&File' nil 1 nil nil 56813 nil nil 1234 nil true 34 3 1282 1 1314 #character 8 '&Character' 1 1 nil nil nil 1282 1 1314 #word 8 '&Word' 1 1 nil nil nil 1282 1 1314 #line 8 '&Line' 1 1 nil nil nil 8 '&Mode' nil 1 nil nil 56815 nil nil 1234 nil true 34 1 1282 1 1314 #about 8 '&About' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 56817 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 9 55 2130 2225 635 11136 2226 #text: 34 1 8 'Comparison' 11136 2226 #updateMenuBar 1152 11136 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 34 1 11264 2448 nil 27 7568 170 192 34 16 5536 8 'classDefinition' 11136 8 'comparisonTab' 10304 8 'projectInfo' 5216 8 'sunit' 928 8 'packageInfo' 4192 8 'globals' 7568 8 'classDocumentation' 8864 8 'methodSource' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 544 34 2 8 1140916736 1 16656 1106 138 144 34 8 10160 768 4064 5504 7536 8832 5056 11120 nil 1184 nil nil 1 nil nil nil 16656 nil 8 4294910695 802 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 16784 7 257 nil 802 0 nil 16802 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 16848 7 257 nil 1456 nil nil nil nil nil #smallIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 2241 697 16656 2226 #basicSelectionsByIndex: 34 1 8 #(5) 16656 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 16656 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 92 1 0 0] 8 #() 2448 nil 27 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 705 2130 2241 697 544 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 96 1 0 0 96 4 0 0 188 2 0 0] 34 9 10304 928 4192 5536 7568 8864 5216 11136 16656 2448 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 17328 nil nil nil 5 nil nil nil 17328 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 17328 34 2 8 1140850688 131073 17440 nil nil nil 5 nil nil nil 17440 482 170 176 1152 false 170 192 34 2 410 ##(Smalltalk.CardContainer) 34 16 nil 17440 34 2 8 1409286144 131073 17568 nil 610 8 4278190080 nil 5 nil nil nil 17568 658 138 144 34 2 722 8 'Category' 410 ##(Smalltalk.ListView) 34 30 nil 17568 34 2 8 1409372233 1025 17744 1106 138 144 1152 nil 1184 610 8 4278190080 nil 21 nil nil nil 17744 nil 8 4294910247 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 417 #left 1394 #displayString 8 #() ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each]' 1779525917 18016 7 257 nil nil 17744 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 9 55 2130 425 633 17744 2226 #text: 34 1 8 'Column 1' 17744 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 216 0 0 0 87 1 0 0] 8 #() 2448 nil 27 722 8 'Variables' 410 ##(Smalltalk.ListView) 34 30 nil 17568 34 2 8 1409372233 1025 18352 1106 138 144 1152 nil 1184 610 17856 nil 21 nil nil nil 18352 nil 8 4294910247 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 417 #left 1394 #displayString 18496 ##(Smalltalk.SortedCollection) nil nil 18352 nil 3 nil 802 0 nil 834 9 1 800 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 18592 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 9 55 2130 425 633 18352 2226 #text: 34 1 8 'Column 1' 18352 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 216 0 0 0 87 1 0 0] 8 #() 2448 nil 27 17744 170 192 34 4 18352 8 'variableList' 17744 8 'categoryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 17568 34 2 8 1140916736 1 18976 1106 138 144 34 2 17728 18336 nil 1184 nil nil 1 nil nil nil 18976 nil 8 4294910695 802 0 nil 16802 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 19104 7 257 nil 802 0 nil 16802 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 19152 7 257 nil 1456 nil nil nil nil nil #noIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 441 695 18976 2226 #basicSelectionsByIndex: 34 1 8 #(1) 18976 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 18976 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 8 #() 2448 nil 27 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 441 695 17568 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 34 3 17744 18352 18976 2448 nil 27 8 'methodFilterTabs' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1349 1 2130 893 695 17440 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 162 2 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 17568 410 ##(Smalltalk.Splitter) 34 12 nil 17440 34 2 8 1140850688 1 19776 nil 610 8 4278190080 nil 517 nil nil nil 19776 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 441 1 2130 11 695 19776 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 220 0 0 0 0 0 0 0 225 0 0 0 91 1 0 0] 8 #() 2448 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 17440 34 2 8 1140850688 131073 20032 nil nil nil 5 nil nil nil 20032 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ComboBox) 34 17 nil 20032 34 2 8 1412498947 1025 20160 1106 138 144 1152 nil 1184 610 8 4278190080 nil 5 nil nil nil 20160 nil 8 4294910703 1394 #displayString 8 #() 1152 401 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 649 2130 443 53 20160 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 68 1 0 0 221 0 0 0 94 1 0 0] 8 #() 2448 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 20530 #fixedParentRight 1 20530 #fixedParentBottom -45 20530 #fixedViewTop 47 410 ##(Smalltalk.ComboBox) 34 17 nil 20032 34 2 8 1412498947 1025 20608 1106 138 144 1152 nil 1184 610 20272 nil 5 nil nil nil 20608 nil 8 4294910703 1394 #displayString 8 #() 1152 401 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 443 53 20608 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 221 0 0 0 26 0 0 0] 8 #() 2448 nil 27 20498 20544 1 20560 1 20530 #fixedParentTop 1 20592 47 410 ##(Smalltalk.ListView) 34 30 nil 20032 34 2 8 1409372233 1025 20960 1106 138 144 1152 nil 1184 610 17856 nil 13 nil nil nil 20960 nil 8 4294910247 1394 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil nil 138 144 34 2 1506 8 '' 41 #left 2226 #empty 8 #() 16 1394 #<= 8 #() nil nil 20960 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each subSuperIcon]' 8 #[30 105 226 0 106] #subSuperIcon 21248 7 257 nil 1 nil nil 1506 8 'Selector' 395 #left 1394 #displayString 21104 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 21376 7 257 nil nil 20960 nil 3 802 0 nil 834 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 21456 7 257 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 21536 7 257 nil #report 1152 nil 133221 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 47 2130 443 603 20960 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 221 0 0 0 68 1 0 0] 8 #() 2448 nil 27 20498 20544 1 20560 1 20944 47 20576 -45 170 192 34 6 20160 8 'overrideList' 20608 8 'superclassList' 20960 8 'methodList' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 451 1 2130 443 695 20032 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 225 0 0 0 0 0 0 0 190 1 0 0 91 1 0 0] 34 3 20608 20960 20160 2448 nil 27 2448 nil 27 5 410 ##(Smalltalk.ContainerView) 34 15 nil 17328 34 2 8 1140850688 131073 22064 nil nil nil 5 nil nil nil 22064 482 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 22064 34 2 8 1140850688 131073 22176 nil nil nil 5 nil nil nil 22176 482 170 176 1152 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 22176 34 2 8 1140936777 1025 22304 1106 138 144 1152 nil 1184 610 8 4278190080 nil 21 nil nil nil 22304 nil 8 4294910247 1394 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 361 #left 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 22544 7 257 nil 1394 #<= 8 #() nil nil 22304 nil 3 802 0 nil 834 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 22656 7 257 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 22736 7 257 nil #report 1152 nil 133217 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 1 2130 369 695 22304 2226 #text: 34 1 8 'Column 1' 22304 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 2448 nil 27 8 'packagesList' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 379 1 2130 369 695 22176 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 189 0 0 0 0 0 0 0 117 1 0 0 91 1 0 0] 34 1 22304 2448 nil 27 17298 9 11 410 ##(Smalltalk.ListView) 34 30 nil 22064 34 2 8 1409405257 1025 23248 1106 138 144 1152 nil 1184 610 8 4278190080 nil 5 nil nil nil 23248 nil 8 4294910247 1394 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 'Column 1' 361 #left 1394 #displayString 8 #() ##(Smalltalk.SortedCollection) nil nil 23248 nil 3 802 0 nil 834 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 23520 7 257 nil 802 0 nil 834 3 1 800 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 23600 7 257 nil #report 1152 nil 133217 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 2 2226 #createAt:extent: 34 2 2130 1 1 2130 369 695 23248 2226 #text: 34 1 8 'Column 1' 23248 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 2448 nil 27 17298 9 11 410 ##(Smalltalk.ContainerView) 34 15 nil 22064 34 2 8 1140850688 131073 23936 nil nil nil 5 nil nil nil 23936 20098 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 23936 34 2 8 1409286144 131073 24048 nil 610 8 4278190080 nil 5 nil nil nil 24048 658 138 144 34 2 722 754 8 'Instance' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 24224 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 24240 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 24048 34 2 8 1140850688 131073 24336 nil nil nil 5 nil nil nil 24336 nil 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 9 2130 567 1 24336 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 2448 nil 27 722 754 8 'Class' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 24608 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 24624 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 24048 34 2 8 1140850688 131073 24720 nil nil nil 5 nil nil nil 24720 nil 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 9 2130 567 1 24720 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 2448 nil 27 24336 170 192 34 4 24720 8 'classTab' 24336 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 24048 34 2 8 1140916738 1 25024 1106 138 144 34 2 24208 24592 nil 1184 nil nil 1 nil nil nil 25024 nil 8 4294910695 802 0 nil 16802 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 25152 7 257 nil 802 0 nil 16802 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 25200 7 257 nil 1456 nil nil nil nil nil #smallIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 583 51 25024 2226 #basicSelectionsByIndex: 34 1 8 #(1) 25024 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 25024 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 25 0 0 0] 8 #() 2448 nil 27 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 645 2130 583 51 24048 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 66 1 0 0 35 1 0 0 91 1 0 0] 34 3 24336 24720 25024 2448 nil 27 20498 20544 1 20560 1 20576 -49 20592 51 410 ##(Smalltalk.CardContainer) 34 16 nil 23936 34 2 8 1409286144 131073 25664 nil 610 640 nil 5 nil nil nil 25664 658 138 144 34 2 722 754 8 'Classes' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 25824 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 25840 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 25664 34 2 8 1409372237 1025 25936 1106 138 144 1152 nil 1184 610 23360 nil 29 nil nil nil 25936 nil 8 4294910247 1394 #displayString 8 #() nil 1456 nil nil nil nil nil nil 138 144 34 1 1506 8 '' 559 #left 1394 #displayString 8 #() 1394 #<= 8 #() nil nil 25936 nil 3 802 0 nil 834 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 26224 7 257 nil 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 26304 7 257 nil #report 1152 nil 133217 nil 34 4 nil nil 2130 1 1 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 55 2130 567 589 25936 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 2448 nil 27 722 754 8 'Hierarchy' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 26608 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 26624 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 25664 34 2 8 1409352231 1025 26720 590918 3 ##(Smalltalk.TreeModel) nil 1184 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 1152 610 8 4278190080 nil 29 nil nil nil 26720 nil 8 4294910689 802 0 nil 834 2 1 1392 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 26912 7 257 nil ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil 170 176 1152 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 1152 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 9 55 2130 567 589 26720 2386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 2448 nil 27 25936 170 192 34 4 25936 8 'classList' 26720 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 25664 34 2 8 1140916736 1 27248 1106 138 144 34 2 25808 26592 nil 1184 nil nil 1 nil nil nil 27248 nil 8 4294910695 802 0 nil 16802 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 27376 7 257 nil 802 0 nil 16802 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 27424 7 257 nil 1456 nil nil nil nil nil #smallIcons nil nil nil nil nil 2162 138 144 34 3 2226 #createAt:extent: 34 2 2130 1 1 2130 583 651 27248 2226 #basicSelectionsByIndex: 34 1 8 #(1) 27248 2226 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 27248 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 8 #() 2448 nil 27 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 583 651 25664 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 34 3 25936 26720 27248 2448 nil 27 20498 20544 1 20560 1 20944 1 20576 -43 170 192 34 4 24048 8 'instanceClassTabs' 25664 8 'classHierarchyTabs' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 757 1 2130 583 695 23936 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 122 1 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 2 25664 24048 2448 nil 27 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 244 63 false 170 192 34 2 23248 8 'projectList' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 1339 695 22064 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 5 23248 410 ##(Smalltalk.Splitter) 34 12 nil 22064 34 2 8 1140850688 1 28352 nil 610 19856 nil 517 nil nil nil 28352 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 369 1 2130 11 695 28352 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 184 0 0 0 0 0 0 0 189 0 0 0 91 1 0 0] 8 #() 2448 nil 27 22176 410 ##(Smalltalk.Splitter) 34 12 nil 22064 34 2 8 1140850688 1 28592 nil 610 19856 nil 517 nil nil nil 28592 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 747 1 2130 11 695 28592 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 117 1 0 0 0 0 0 0 122 1 0 0 91 1 0 0] 8 #() 2448 nil 27 23936 2448 nil 27 7 false 170 192 1152 nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 1 2130 2241 695 17328 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 22064 410 ##(Smalltalk.Splitter) 34 12 nil 17328 34 2 8 1140850688 1 29008 nil 610 19856 nil 517 nil nil nil 29008 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1339 1 2130 11 695 29008 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 157 2 0 0 0 0 0 0 162 2 0 0 91 1 0 0] 8 #() 2448 nil 27 17440 2448 nil 27 17298 3 5 true 170 192 34 4 544 8 'textAreaTabs' 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 29312 nil 610 19856 nil 517 nil nil nil 29312 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 1 695 2130 2241 11 29312 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 91 1 0 0 96 4 0 0 96 1 0 0] 8 #() 2448 nil 27 8 'splitter' nil 2162 138 144 34 1 2226 #createAt:extent: 34 2 2130 7679 21 2130 2241 1401 416 2386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 95 19 0 0 198 2 0 0] 34 3 17328 29312 544 2448 nil 27 )! !
!JadeiteProjectsBrowserPresenter class categoriesFor: #downArrowIcon!public! !
!JadeiteProjectsBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

