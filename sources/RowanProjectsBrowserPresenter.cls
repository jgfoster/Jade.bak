"Filed out from Dolphin Smalltalk 7"!

RowanSystemBrowserPresenter subclass: #RowanProjectsBrowserPresenter
	instanceVariableNames: 'projectListPresenter stonCritical projectInfoTab projectLabel projectDictionaryTabs comparisonPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectsBrowserPresenter guid: (GUID fromString: '{258266f3-948d-47d3-afa6-c6d0c941ddc5}')!
RowanProjectsBrowserPresenter comment: 'In packages pane, show projects & packages. Remove class categories.'!
!RowanProjectsBrowserPresenter categoriesForClass!Unclassified! !
!RowanProjectsBrowserPresenter methodsFor!

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
	self removeMethodsFrom: (Array with: service)!

browseImplementorsOf: string
	| service |
	service := RowanQueryService command: #implementorsOf: withArgs: (Array with: string asSymbol).
	self issueCommand: (Array with: service).
	self openMethodListOn: service selecting: string!

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

createComponents
	super createComponents.
	projectListPresenter := self add: ListPresenter new name: 'projectList'.
	packageListPresenter := self add: ListPresenter new name: 'packagesList'.
	comparisonPresenter := self add: DifferencesPresenter new name: 'comparisonTab'!

createSchematicWiring

	super createSchematicWiring.
	self createSchematicWiringForProjectList
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
	methodSourcePresenter view firstVisibleLine: firstVisibleLine.
	methodSourcePresenter view goto: caretPosition!

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

issueCommand: services
	| stonString stonResultString |
	stonString := STON toString: services.
	[stonResultString := self gciSession serverPerform: #updateFromSton: with: stonString]
		ensure: [services do: [:service | service clearCommand]].
	self update: services afterStonReplication: stonResultString!

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
		self browseImplementors; browseImplementorsOf; browseSenders; browseSendersOf; browseMethodsContaining; browseMethodPragmas; removeMethods; runMethodTests; setEnvironment0; setEnvironment1; setEnvironment2; fileOutMethod; gotoMethodPackage.
	].
	^#(
		'&Methods'
		'Browse &Implementors//browseImplementors'
		'Browse Implementors of ...//browseImplementorsOf'
		'Browse &Senders//browseSenders'
		'Browse Senders of ...//browseSendersOf'
		'Browse Methods &Containing ...//browseMethodsContaining'
		'Browse &History//browseMethodHistory'
		'BrowseMethod &Pragmas ...//browseMethodPragmas'
		'&File out method ...//fileOutMethod'
		'-'
		'&Delete Method(s) ...//removeMethods'
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
	listModel := (textAreaTabs subViews detect: [:each | each class == TabViewXP]) model.
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

registerClassData
	"temporarily disabled"!

registerPresentersForUpdates
	"copied to RowanDebugger"

	super registerPresentersForUpdates.
	BrowserUpdate current register: projectListPresenter with: #projectsUpdate:.
	BrowserUpdate current register: packageListPresenter with: #projectPackagesUpdate:.
	BrowserUpdate current register: classListPresenter with: #classesUpdate:.
	BrowserUpdate current register: categoryListPresenter with: #categoryUpdate:.
	BrowserUpdate current register: methodListPresenter with: #classMethodsUpdate:.
	BrowserUpdate current register: methodSourcePresenter with: #methodSourceUpdate:.
	BrowserUpdate current register: originalSourcePresenter  with: #methodSourceUpdate:!

removeMethodCategories
	| service |
	service := classListPresenter selection
				command: #removeCategories:;
				commandArgs: (Array with: categoryListPresenter selections).
	self issueCommand: (Array with: service)!

removeMethodsFrom: services
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
			stream nextPutAll: service displayString.
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
	textAreaTabs currentCard name = 'projectInfo' ifTrue: [projectInfoTab ensureVisible].
	self updateMenuBar!

unloadProject
	MessageBox notify: 'This feature not yet implemented'!

update: services afterStonReplication: stonResults
	BrowserUpdate current update: services afterStonReplication: stonResults!

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
	selection selectedPackageServices: packageListPresenter selections. 
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
	methodListPresenter selections isEmpty ifTrue: [^methodSourcePresenter value: String new].
	methodListPresenter selections size = 1 ifTrue:[^self updateSingleMethodSource].	
	self updateMultiMethodComparison
	
!

updateMultiMethodComparison
	methodListPresenter selections size < 2
		ifTrue: 
			[comparisonPresenter
				beforeTitle: methodListPresenter selection classMethodDisplayString;
				afterTitle: String new;
				compare: methodListPresenter selection source with: String new]
		ifFalse: 
			[comparisonPresenter
				beforeTitle: (methodListPresenter selections at: 1) classMethodDisplayString;
				afterTitle: (methodListPresenter selections at: 2) classMethodDisplayString;
				compare: (methodListPresenter selections at: 1) source
					with: (methodListPresenter selections at: 2) source].
	comparisonPresenter ensureVisible.
	methodSourcePresenter value: String new.
	originalSourcePresenter value: String new!

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
	originalSourcePresenter value: methodListPresenter selections first source.
	methodSourcePresenter ensureVisible.
	comparisonPresenter
		beforeTitle: String new;
		afterTitle: String new;
		compare: String new with: String new!

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
!RowanProjectsBrowserPresenter categoriesFor: #addPackageDictionaryInfoTo:!public!request string! !
!RowanProjectsBrowserPresenter categoriesFor: #basicAddMethodCategory:!menu handlers!private!services! !
!RowanProjectsBrowserPresenter categoriesFor: #basicRemoveMethods!menu handlers!private! !
!RowanProjectsBrowserPresenter categoriesFor: #browseImplementorsOf:!menu handlers!public!queries! !
!RowanProjectsBrowserPresenter categoriesFor: #browseSendersOf:!menu handlers!public!queries! !
!RowanProjectsBrowserPresenter categoriesFor: #checkout!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #classesMenuStrings!menus!public! !
!RowanProjectsBrowserPresenter categoriesFor: #cloneProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #commit!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #createComponents!initialization!public! !
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
!RowanProjectsBrowserPresenter categoriesFor: #gotoClassPackage!menu handlers!private! !
!RowanProjectsBrowserPresenter categoriesFor: #gotoMethodPackage!menu handlers!private! !
!RowanProjectsBrowserPresenter categoriesFor: #initialize!initialization!public! !
!RowanProjectsBrowserPresenter categoriesFor: #inspectProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #isDictionariesTabSelected!public!testing! !
!RowanProjectsBrowserPresenter categoriesFor: #issueCommand:!private!services! !
!RowanProjectsBrowserPresenter categoriesFor: #loadProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #makeDefault!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #methodsMenuStrings!menus!public! !
!RowanProjectsBrowserPresenter categoriesFor: #methodSourceEqual!public!testing! !
!RowanProjectsBrowserPresenter categoriesFor: #newService!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #onViewOpened!event handlers!overrides!public! !
!RowanProjectsBrowserPresenter categoriesFor: #openMethodListOn:selecting:!menu handlers!private!queries! !
!RowanProjectsBrowserPresenter categoriesFor: #projectChanges!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #projectDictionaryChanged!event handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #projectLog!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #projectMenuStrings!menus!public! !
!RowanProjectsBrowserPresenter categoriesFor: #pullFromGit!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #pushToGit!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #queryCommand:!event handlers!overrides!public! !
!RowanProjectsBrowserPresenter categoriesFor: #refresh!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #registerClassData!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #registerPresentersForUpdates!event handlers!initialization!public!register presenters! !
!RowanProjectsBrowserPresenter categoriesFor: #removeMethodCategories!private!services! !
!RowanProjectsBrowserPresenter categoriesFor: #removeMethodsFrom:!private!services! !
!RowanProjectsBrowserPresenter categoriesFor: #selectClass:!private!selection! !
!RowanProjectsBrowserPresenter categoriesFor: #selectedMethodName!menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #selectionMethod:!private!selection! !
!RowanProjectsBrowserPresenter categoriesFor: #selectorsString!private!selection! !
!RowanProjectsBrowserPresenter categoriesFor: #selectPackage:!private!selection! !
!RowanProjectsBrowserPresenter categoriesFor: #setClassMeta!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #setMethodFilter!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #showPackages!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #textTabChanged!event handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #unloadProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #update:afterStonReplication:!private!services! !
!RowanProjectsBrowserPresenter categoriesFor: #updateAndSelectA:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClass!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClasses!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateClassInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateCommand:onSuccessDo:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateMenuBar!menus!public! !
!RowanProjectsBrowserPresenter categoriesFor: #updateMethodReferences:!private!services! !
!RowanProjectsBrowserPresenter categoriesFor: #updateMethodSource!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateMultiMethodComparison!private! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackage!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackageInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackages!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProject!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProjectInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProjects!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProjectTab!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateSelectionsForUpdate:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateServices:!private!services! !
!RowanProjectsBrowserPresenter categoriesFor: #updateSingleMethodSource!private! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 656 nil nil nil 5 nil nil nil 656 482 170 176 8 #() false 170 192 752 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1349 1 898 893 695 656 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 162 2 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 656 34 2 8 1409286144 131073 1008 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 517 nil nil nil 1008 655878 ##(Smalltalk.CardLayout) 138 144 34 3 721414 ##(Smalltalk.Association) 8 'Categories' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 1008 34 2 8 1140850688 131073 1264 nil 1074 1104 nil 5 nil nil nil 1264 1136 170 192 34 6 1232 8 'categoryList' 410 ##(Smalltalk.ListView) 34 30 nil 1264 34 2 8 1409372233 1025 1392 590662 2 ##(Smalltalk.ListModel) 138 144 752 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 1074 8 4278190080 nil 21 nil nil nil 1392 nil 8 4294902921 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 373 #left 1586 #displayString 1616 ##(Smalltalk.SortedCollection) nil nil 1392 nil 3 nil 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 9 1 1760 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 1776 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 381 643 1392 850 #text: 34 1 8 'Column 1' 1392 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 190 0 0 0 83 1 0 0] 8 #() 898 193 193 nil 27 8 'pragmaList' 410 ##(Smalltalk.ListView) 34 30 nil 1264 34 2 8 1409372233 1025 2144 1458 138 144 752 nil 1520 1074 1552 nil 21 nil nil nil 2144 nil 8 4294902921 1586 #displayString 1616 nil 1648 nil nil nil nil nil nil 138 144 34 1 1698 8 'Column 1' 373 #left 1586 #displayString 1616 ##(Smalltalk.SortedCollection) nil nil 2144 nil 3 nil 1762 0 nil 1794 9 1 1760 1824 1840 1856 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 2368 7 257 nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 381 643 2144 850 #text: 34 1 8 'Column 1' 2144 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 190 0 0 0 83 1 0 0] 8 #() 2112 nil 27 8 'variableList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 1008 34 2 8 1140916864 1 2656 1458 138 144 34 3 1216 8 'Variables' 8 'Pragmas' nil 1520 1074 1104 nil 1 nil nil nil 2656 nil 8 4294907939 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1648 nil nil nil nil nil #noIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 441 695 2656 850 #basicSelectionsByIndex: 34 1 8 #(1) 2656 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2656 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 8 #() 2112 nil 27 nil 1008 898 33 33 1049862 ##(Smalltalk.ButtonInteractor) 1264 nil 590342 ##(Smalltalk.Rectangle) 898 345 3 898 377 35 1 1180998 4 ##(Smalltalk.CommandDescription) #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 53 9 898 381 679 1264 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 216 0 0 0 87 1 0 0] 34 3 1232 2144 1392 2112 nil 27 34 2 8 1409372233 1025 1232 1458 138 144 752 nil 1520 1074 1552 nil 21 nil nil nil 1232 nil 8 4294902921 1586 #displayString 8 #() nil 1648 nil nil nil nil nil nil 138 144 34 1 1698 8 'Column 1' 373 #left 1586 #displayString 8 #() ##(Smalltalk.SortedCollection) 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each]' 1779525917 3616 7 257 nil nil 1232 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 37 898 381 643 1232 850 #text: 34 1 8 'Column 1' 1232 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 190 0 0 0 83 1 0 0] 8 #() 2112 nil 27 1186 2768 2144 1186 2784 1392 1232 170 192 34 2 1264 8 'methodFilterTabs' nil 2656 1264 898 201 201 401 1 31 nil nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 441 695 1008 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 34 2 1264 2656 2112 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 4176 nil 1074 8 4278190080 nil 517 nil nil nil 4176 786 138 144 34 1 850 #createAt:extent: 34 2 898 441 1 898 11 695 4176 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 220 0 0 0 0 0 0 0 225 0 0 0 91 1 0 0] 8 #() 2112 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 4432 nil nil nil 5 nil nil nil 4432 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ComboBox) 34 17 nil 4432 34 2 8 1412498947 1025 4560 1458 138 144 752 nil 1520 1074 8 4278190080 nil 5 nil nil nil 4560 nil 8 4294902727 1586 #displayString 8 #() 752 401 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 443 53 4560 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 221 0 0 0 26 0 0 0] 8 #() 2112 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 4930 #fixedParentRight 1 4930 #fixedParentTop 1 4930 #fixedViewTop 47 410 ##(Smalltalk.ListView) 34 30 nil 4432 34 2 8 1409372233 1025 5008 1458 138 144 752 nil 1520 1074 1552 nil 13 nil nil nil 5008 nil 8 4294902921 1586 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1648 nil nil nil nil nil nil 138 144 34 2 1698 8 '' 41 #left 850 #empty 8 #() 16 1586 #<= 8 #() nil nil 5008 1762 0 nil 1794 2 1 1760 8 'doIt' 8 '[:each | each subSuperIcon]' 8 #[30 105 226 0 106] #subSuperIcon 5296 7 257 nil 1 nil nil 1698 8 'Selector' 395 #left 1586 #displayString 5152 ##(Smalltalk.SortedCollection) 1762 0 nil 1794 2 1 1760 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 5424 7 257 nil nil 5008 nil 3 nil 1762 0 nil 1794 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 5504 7 257 nil #report 752 nil 131173 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 47 898 443 603 5008 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 221 0 0 0 68 1 0 0] 8 #() 2112 nil 27 4898 4944 1 4960 1 4976 47 4930 #fixedParentBottom -45 410 ##(Smalltalk.ComboBox) 34 17 nil 4432 34 2 8 1412498947 1025 5808 1458 138 144 752 nil 1520 1074 4672 nil 5 nil nil nil 5808 nil 8 4294902727 1586 #displayString 8 #() 752 401 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 649 898 443 53 5808 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 68 1 0 0 221 0 0 0 94 1 0 0] 8 #() 2112 nil 27 4898 4944 1 4960 1 5792 -45 4992 47 170 192 34 6 4560 8 'superclassList' 5008 8 'methodList' 5808 8 'overrideList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 451 1 898 443 695 4432 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 225 0 0 0 0 0 0 0 190 1 0 0 91 1 0 0] 34 3 4560 5008 5808 2112 nil 27 2112 nil 27 5 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 6384 nil nil nil 5 nil nil nil 6384 482 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 6384 34 2 8 1140850688 131073 6496 nil nil nil 5 nil nil nil 6496 482 170 176 752 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 6496 34 2 8 1140936777 1025 6624 1458 138 144 752 nil 1520 1074 8 4278190080 nil 21 nil nil nil 6624 nil 8 4294902921 1586 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1648 nil nil nil nil nil nil 138 144 34 1 1698 8 'Column 1' 361 #left 1762 0 nil 1794 2 1 1760 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 6864 7 257 nil 1586 #<= 8 #() nil nil 6624 nil 3 1762 0 nil 1794 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 6976 7 257 nil 1762 0 nil 1794 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 7056 7 257 nil #report 752 nil 133217 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 1 898 369 695 6624 850 #text: 34 1 8 'Column 1' 6624 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 2112 nil 27 8 'packagesList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 379 1 898 369 695 6496 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 189 0 0 0 0 0 0 0 117 1 0 0 91 1 0 0] 34 1 6624 2112 nil 27 524806 ##(Smalltalk.Fraction) 9 11 410 ##(Smalltalk.ContainerView) 34 15 nil 6384 34 2 8 1140850688 131073 7584 nil nil nil 5 nil nil nil 7584 4498 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 7584 34 2 8 1409286144 131073 7696 nil 1074 8 4278190080 nil 5 nil nil nil 7696 1122 138 144 34 2 1186 590662 1 ##(Smalltalk.CardLabel) 8 'Instance' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 7856 7888 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 7904 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 7696 34 2 8 1140850688 131073 8000 nil nil nil 5 nil nil nil 8000 nil 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 9 898 567 1 8000 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 2112 nil 27 1186 7858 8 'Class' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 7856 8272 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 8288 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 7696 34 2 8 1140850688 131073 8384 nil nil nil 5 nil nil nil 8384 nil 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 9 898 567 1 8384 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 2112 nil 27 8000 170 192 34 4 8384 8 'classTab' 8000 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 7696 34 2 8 1140916738 1 8688 1458 138 144 34 2 7872 8256 nil 1520 nil nil 1 nil nil nil 8688 nil 8 4294907939 1762 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 8816 7 257 nil 1762 0 nil 8834 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 8880 7 257 nil 1648 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 583 51 8688 850 #basicSelectionsByIndex: 34 1 8 #(1) 8688 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 8688 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 25 0 0 0] 8 #() 2112 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 645 898 583 51 7696 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 66 1 0 0 35 1 0 0 91 1 0 0] 34 3 8000 8384 8688 2112 nil 27 4898 4944 1 4960 1 5792 -49 4992 51 410 ##(Smalltalk.CardContainer) 34 16 nil 7584 34 2 8 1409286144 131073 9344 nil 1074 8 4278190080 nil 5 nil nil nil 9344 1122 138 144 34 2 1186 7858 8 'Classes' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 7856 9520 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 9536 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 9344 34 2 8 1409372237 1025 9632 1458 138 144 752 nil 1520 1074 8 4278190080 nil 29 nil nil nil 9632 nil 8 4294902921 1586 #displayString 8 #() nil 1648 nil nil nil nil nil nil 138 144 34 1 1698 8 '' 559 #left 1586 #displayString 8 #() 1586 #<= 8 #() nil nil 9632 nil 3 1762 0 nil 1794 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 9936 7 257 nil 1762 0 nil 1794 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 10016 7 257 nil #report 752 nil 133217 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 55 898 567 589 9632 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 2112 nil 27 1186 7858 8 'Hierarchy' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 7856 10320 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 10336 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 9344 34 2 8 1409352231 1025 10432 590918 3 ##(Smalltalk.TreeModel) nil 1520 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 752 1074 8 4278190080 nil 29 nil nil nil 10432 nil 8 4294908757 1762 0 nil 1794 2 1 1584 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 10624 7 257 nil ##(Smalltalk.IconicListAbstract) 1648 nil nil nil nil nil 170 176 752 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 752 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 55 898 567 589 10432 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 2112 nil 27 9632 170 192 34 4 9632 8 'classList' 10432 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 9344 34 2 8 1140916736 1 10960 1458 138 144 34 2 9504 10304 nil 1520 nil nil 1 nil nil nil 10960 nil 8 4294907939 1762 0 nil 8834 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 11088 7 257 nil 1762 0 nil 8834 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 11136 7 257 nil 1648 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 583 651 10960 850 #basicSelectionsByIndex: 34 1 8 #(1) 10960 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 10960 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 8 #() 2112 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 583 651 9344 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 34 3 9632 10432 10960 2112 nil 27 4898 4944 1 4960 1 4976 1 5792 -43 170 192 34 4 7696 8 'instanceClassTabs' 9344 8 'classHierarchyTabs' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 757 1 898 583 695 7584 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 122 1 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 2 9344 7696 2112 nil 27 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 244 63 410 ##(Smalltalk.ListView) 34 30 nil 6384 34 2 8 1409405257 1025 11856 1458 138 144 752 nil 1520 1074 9744 nil 5 nil nil nil 11856 nil 8 4294902921 1586 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1648 nil nil nil nil nil nil 138 144 34 1 1698 8 'Column 1' 361 #left 1586 #displayString 8 #() ##(Smalltalk.SortedCollection) nil nil 11856 nil 3 1762 0 nil 1794 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 12112 7 257 nil 1762 0 nil 1794 3 1 1760 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 12192 7 257 nil #report 752 nil 133217 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 1 898 369 695 11856 850 #text: 34 1 8 'Column 1' 11856 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 2112 nil 27 7554 9 11 false 170 192 34 2 11856 8 'projectList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 1339 695 6384 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 5 11856 410 ##(Smalltalk.Splitter) 34 12 nil 6384 34 2 8 1140850688 1 12736 nil 1074 4256 nil 517 nil nil nil 12736 786 138 144 34 1 850 #createAt:extent: 34 2 898 369 1 898 11 695 12736 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 184 0 0 0 0 0 0 0 189 0 0 0 91 1 0 0] 8 #() 2112 nil 27 6496 410 ##(Smalltalk.Splitter) 34 12 nil 6384 34 2 8 1140850688 1 12976 nil 1074 4256 nil 517 nil nil nil 12976 786 138 144 34 1 850 #createAt:extent: 34 2 898 747 1 898 11 695 12976 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 117 1 0 0 0 0 0 0 122 1 0 0 91 1 0 0] 8 #() 2112 nil 27 7584 2112 nil 27 7 false 170 192 752 nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 2241 695 544 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 6384 410 ##(Smalltalk.Splitter) 34 12 nil 544 34 2 8 1140850688 1 13392 nil 1074 4256 nil 517 nil nil nil 13392 786 138 144 34 1 850 #createAt:extent: 34 2 898 1339 1 898 11 695 13392 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 157 2 0 0 0 0 0 0 162 2 0 0 91 1 0 0] 8 #() 2112 nil 27 656 2112 nil 27 7554 3 5 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 13648 nil 1074 9424 nil 5 nil nil nil 13648 1122 138 144 34 9 1186 7858 8 'Package' 1762 0 nil 1794 7 1 7856 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 7856 13808 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 13824 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 13648 34 2 8 1140850688 131073 13920 nil nil nil 5 nil nil nil 13920 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 13920 34 2 8 1409355853 1025 14032 1458 138 144 752 nil 1520 1074 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 5 984134 2 ##(Smalltalk.CommandMenuItem) 1 3186 #addRepository 8 'Add &Repository' 1 1 nil nil nil 14210 1 3186 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 14210 1 3186 #savePackage 8 '&Save Package' 1 1 nil nil nil 14210 1 3186 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 14032 nil 8 4294902921 1586 #displayString 8 #() nil 1648 nil nil nil nil nil nil 138 144 34 4 1698 8 'Repository Type' 211 #left 1586 #displayString 14496 ##(Smalltalk.SortedCollection) 1762 0 nil 1794 2 1 1760 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 14592 7 257 nil nil 14032 nil 1 nil nil 1698 8 'Description' 1161 #left 1586 #displayString 8 #() 1586 #<= 14720 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 14752 7 257 nil nil 14032 nil 3 nil nil 1698 8 'User' 201 #left 1586 #displayString 8 #() 1586 #<= 14880 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 14912 7 257 nil nil 14032 nil 1 nil nil 1698 8 'Password' 201 #left 1586 #displayString 14880 1586 #<= 14880 1762 0 nil 1794 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 15056 1762 0 nil 15072 19 257 nil 7 257 nil nil 14032 nil 1 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 263 898 1769 217 14032 850 #contextMenu: 34 1 14176 14032 850 #text: 34 1 8 'Repository Type' 14032 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 2112 nil 27 19 410 ##(Smalltalk.ListView) 34 30 nil 13920 34 2 8 1409355853 1025 15424 1458 138 144 752 nil 1520 1074 14144 nil 5 14162 nil true 34 1 14210 1 3186 #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 15424 nil 8 4294902921 1586 #displayString 8 #() nil 1648 nil nil nil nil nil nil 138 144 34 4 1698 8 'Ancestor' 131 #left 1586 #displayString 14880 1586 #<= 14880 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 15776 7 257 nil nil 15424 nil 1 nil nil 1698 8 'Name' 401 #left 1586 #displayString 15664 ##(Smalltalk.SortedCollection) 1762 0 nil 1794 1 83886081 1760 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 15904 7 257 nil nil 15424 nil 1 nil nil 1698 8 'Timestamp' 301 #left 1586 #displayString 14720 1586 #<= 14720 1762 0 nil 1794 1 83886081 1760 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 16048 7 257 nil nil 15424 nil 1 nil nil 1698 8 'Message' 941 #left 1586 #displayString 14720 1586 #<= 14720 1762 0 nil 1794 1 83886081 1760 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 16192 7 257 nil nil 15424 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 1769 263 15424 850 #contextMenu: 34 1 15536 15424 850 #text: 34 1 8 'Ancestor' 15424 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 2112 nil 27 23 true 170 192 34 4 14032 8 'repositoryList' 15424 8 'ancestorList' nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 9 55 898 2225 635 13920 850 #text: 34 1 8 'Package' 13920 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 34 2 15424 14032 2112 nil 27 1186 7858 8 'Globals' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 7856 16848 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 16864 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 13648 34 2 8 1409355853 1025 16960 1458 138 144 752 nil 1520 1074 14144 nil 5 nil nil nil 16960 nil 8 4294902921 1586 #displayString 8 #() nil 1648 nil nil nil nil nil nil 138 144 34 3 1698 8 'Name' 739 #left 1586 #displayString 17104 ##(Smalltalk.SortedCollection) 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 17200 7 257 nil nil 16960 nil 3 nil nil 1698 8 'Class' 741 #left 1586 #displayString 14720 1586 #<= 14720 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 17344 7 257 nil nil 16960 nil 3 nil nil 1698 8 'Value' 739 #left 1586 #displayString 14720 1586 #<= 14720 1762 0 nil 1794 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 17488 7 257 nil nil 16960 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 9 55 898 2225 635 16960 850 #text: 34 1 8 'Name' 16960 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 2112 nil 27 1186 7858 8 'Original Source' 1762 0 nil 1794 7 1 16 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 7856 17840 ##(Smalltalk.Icon) 8 'COMPILEDMETHOD_PRIVATE.ico' #fromId: #text:iconBlock: 17856 11 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13648 34 2 8 1445007428 1025 17952 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 1074 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 17952 nil 8 4294902887 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 18290 77 786694 ##(Smalltalk.IndexedColor) 33554457 18338 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 18304 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 18320 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 752 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 17952 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 18338 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 18304 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 17952 65025 3 false 1 nil nil nil nil nil nil 3 18754 3 17952 33423361 5 false 3 nil nil nil nil nil nil 5 18754 5 17952 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18290 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18290 1027 18866 60239 18866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18290 1031 18866 335 18866 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 7 850 #createAt:extent: 34 2 898 9 55 898 2225 635 17952 850 #modificationEventMask: 8 #(4607) 17952 850 #caretPeriod: 8 #(530) 17952 850 #wordWrap: 8 #(true) 17952 850 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 17952 61 3 nil nil 5 nil 19250 3 17952 1 nil 3 67108863 5 nil 19250 5 17952 1 nil 3 -67108863 5 nil 17952 850 #tabWidth: 8 #(4) 17952 850 #maxCompletionListHeight: 8 #(9) 17952 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 2112 nil 29 1186 7858 8 'SUnit' 1762 0 nil 1794 4 1 1760 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 19472 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 13648 34 2 8 1140850688 131073 19600 nil 1074 8 4278190080 nil 5 nil nil nil 19600 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 9 55 898 2225 635 19600 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 752 2112 nil 27 1186 7858 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13648 34 2 8 1445007428 1025 19920 18018 nil false 18064 nil 1074 8 4278190080 nil 5 14162 nil true 34 11 14210 1 3186 #editSave 8 '&Save' 9383 1 nil nil nil 14322 4097 14210 1 3186 #editUndo 8 '&Undo' 9397 1 nil nil nil 14210 1 3186 #editRedo 8 '&Redo' 9395 1 nil nil nil 14322 4097 14210 1 3186 #editCut 8 'Cu&t' 9393 1 nil nil nil 14210 1 3186 #editCopy 8 '&Copy' 9351 1 nil nil nil 14210 1 3186 #editPaste 8 '&Paste' 9389 1 nil nil nil 14210 1 3186 #editDelete 8 '&Delete' 1629 1 nil nil nil 14322 4097 14210 1 3186 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 18114 nil true 18146 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 19920 nil 8 4294902887 18226 nil nil 13 nil 170 192 34 4 #normal 18290 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 18290 77 18352 18368 1 nil nil nil nil #callTip nil nil nil 34 256 20640 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20656 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 752 170 192 34 2 #default 18482 1 nil nil nil 19920 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 18544 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 20640 nil nil nil nil 1 nil 170 192 34 6 1 18754 1 19920 65025 3 false 1 nil nil nil nil nil nil 3 18754 3 19920 33423361 5 false 3 nil nil nil nil nil nil 5 18754 5 19920 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18290 1029 nil 18866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18290 1027 18866 60239 18866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18290 1031 18866 335 18866 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 8 850 #createAt:extent: 34 2 898 9 55 898 2225 635 19920 850 #contextMenu: 34 1 20032 19920 850 #modificationEventMask: 8 #(4607) 19920 850 #caretPeriod: 8 #(530) 19920 850 #wordWrap: 8 #(true) 19920 850 #margins: 34 1 34 3 19250 1 19920 1 3 nil nil 5 nil 19250 3 19920 33 nil 3 67108863 5 nil 19250 5 19920 1 nil 3 -67108863 5 nil 19920 850 #tabWidth: 8 #(4) 19920 850 #maxCompletionListHeight: 8 #(9) 19920 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 2112 nil 29 1186 7858 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13648 34 2 8 1445007428 1025 21712 18018 nil false 18064 nil 1074 20016 nil 5 14162 nil true 34 1 14210 1 3186 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 18114 nil true 18146 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 21712 nil 8 4294902887 18226 nil nil 11 nil 170 192 34 4 #normal 18290 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 18290 77 18352 18368 1 nil nil nil nil #callTip nil nil nil 34 256 22032 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 22048 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 18482 1 nil nil nil 21712 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 18544 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 22032 nil nil nil nil 1 nil 170 192 34 6 1 18754 1 21712 65025 3 false 1 nil nil nil nil nil nil 3 18754 3 21712 33423361 5 false 3 nil nil nil nil nil nil 5 18754 5 21712 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18290 1029 nil 18866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18290 1027 18866 60239 18866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18290 1031 18866 335 18866 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 8 850 #createAt:extent: 34 2 898 9 55 898 2225 635 21712 850 #contextMenu: 34 1 21808 21712 850 #modificationEventMask: 8 #(4607) 21712 850 #caretPeriod: 8 #(530) 21712 850 #wordWrap: 8 #(true) 21712 850 #margins: 34 1 34 3 19250 1 21712 1 3 nil nil 5 nil 19250 3 21712 33 nil 3 67108863 5 nil 19250 5 21712 1 nil 3 -67108863 5 nil 21712 850 #tabWidth: 8 #(4) 21712 850 #maxCompletionListHeight: 8 #(9) 21712 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 2112 nil 29 1186 7858 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13648 34 2 8 1445007428 1025 23008 18018 nil false 18064 nil 1074 8 4278190080 nil 5 nil 18114 nil true 18146 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 898 193 193 nil 23008 nil 8 4294902887 18226 nil nil 13 nil 170 192 34 4 #normal 18290 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 18290 77 18352 18368 1 nil nil nil nil #callTip nil nil nil 34 256 23248 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 23264 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 18402 1 nil nil false 138 144 752 170 192 34 2 #default 18482 1 nil nil nil 23008 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 18544 nil nil 65 nil nil 20816 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 23248 #callTip 23264 nil nil nil nil 1 nil 170 192 34 12 1 18754 1 23008 65025 3 false 1 nil nil nil nil nil nil 3 18754 3 23008 33423361 5 false 3 nil nil nil nil nil nil 5 18754 5 23008 511 1 false 5 nil nil nil nil nil nil 8 'indicator10' 18754 21 23008 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator9' 18754 19 23008 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator8' 18754 17 23008 33554447 1 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18290 1029 nil 18866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18290 1027 18866 60239 18866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18290 1031 18866 335 18866 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 8 850 #createAt:extent: 34 2 898 9 55 898 2225 635 23008 850 #modificationEventMask: 8 #(4607) 23008 850 #hoverTime: 8 #(200) 23008 850 #caretPeriod: 8 #(530) 23008 850 #wordWrap: 8 #(true) 23008 850 #margins: 34 1 34 3 19250 1 23008 61 3 nil nil 5 nil 19250 3 23008 1 nil 3 67108863 nil nil 19250 5 23008 1 nil 3 -67108863 nil nil 23008 850 #tabWidth: 8 #(4) 23008 850 #maxCompletionListHeight: 8 #(9) 23008 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 2112 nil 29 1186 7858 8 'Project' 1762 0 nil 1794 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 19554 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 24320 7 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 13648 34 2 8 1409372237 1025 24432 1458 138 144 752 nil 1520 1074 14144 nil 5 nil nil nil 24432 nil 8 4294902921 1586 #displayString 17104 nil 1648 nil nil nil nil nil nil 138 144 34 2 1698 8 'Name' 201 #left 1586 #displayString 17104 ##(Smalltalk.SortedCollection) 1762 0 nil 1794 1 83886081 1760 8 'doIt' 8 '[:each | (each at: 2)]' 8 #[29 105 17 64 148 106] 24656 7 257 nil nil 24432 nil 1 nil 1762 0 nil 1794 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each item at: 1) displayStringFor: each]' 8 #[31 105 226 0 63 148 17 177 106] #item #displayStringFor: 24736 7 257 nil 1698 8 'Value' 2017 #left 1586 #displayString 8 #() 1586 #<= 24864 1762 0 nil 1794 4 1 1760 8 'doIt' 8 '[:each | (each at: 2) ifNil:[String new] ifNotNil: [:iv |
(each at: 1) instVarNamed: iv]]' 8 #[32 105 17 64 148 100 223 4 97 45 146 106 90 17 63 148 18 178 106] 19554 #String 16 #ifNil:ifNotNil: #instVarNamed: 24896 7 65793 nil nil 24432 nil 3 nil nil #report 752 nil 131169 nil 34 4 nil nil 898 1 1 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 9 55 898 2225 635 24432 850 #text: 34 1 8 'Name' 24432 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 2112 nil 27 1186 8 'Comparison' 410 ##(Smalltalk.ShellView) 34 27 nil 13648 34 2 8 1078919168 131073 25264 nil 1074 8 4278190080 nil 517 nil nil nil 25264 788230 ##(Smalltalk.BorderLayout) 1 1 nil nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 25264 34 2 8 1140850688 131073 25392 nil 1074 25344 nil 5 14162 nil true 34 5 14210 1 3186 #toggleIndentGuides 8 'Indentation &Guides' 1 1 nil nil nil 14210 1 3186 #toggleVisibleLineEndings 8 'Line &Endings' 1 1 nil nil nil 14210 1 3186 #toggleLineNumberMargins 8 'Line N&umbers' 1 1 nil nil nil 14210 1 3186 #toggleVisibleWhitespace 8 'W&hitespace' 1 1 nil nil nil 14210 1 3186 #toggleWrapping 8 '&Word Wrap' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 25392 482 170 176 752 false 170 192 752 nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 1 898 2193 603 25392 850 #contextMenu: 34 1 25472 25392 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 25392 34 2 8 1140850688 131073 26000 nil 1074 25344 nil 5 nil nil nil 26000 25362 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 26000 34 2 8 1140850945 262145 26096 nil nil nil 5 nil nil nil 26096 nil 8 1660708144 18226 nil nil nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 563 898 1091 41 26096 850 #text: 34 1 8 '
' 26096 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 33 2 0 0 45 1 0 0] 8 #() 2112 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 26000 34 2 8 1140920580 1025 26400 18018 nil false 18064 nil 1074 25344 nil 5 nil nil nil 26400 nil 8 4294902887 18226 nil nil 11 nil 170 192 34 12 #indentGuide 18290 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 18290 3 18338 33554445 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 18290 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 18290 77 18352 18368 1 nil nil nil nil #callTip nil nil nil #normal 18290 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 18290 5 nil 18866 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 26640 26576 26656 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 26608 nil nil nil 26560 nil nil 1310982 ##(Smalltalk.DiffsScintillaStyler) 8 #() 170 192 34 2 #default 18482 1 nil nil nil 26400 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 18544 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 26528 nil nil nil nil 1 nil 170 192 34 6 8 'indicator2' 18754 5 26400 511 1 false nil nil nil nil nil nil nil 8 'indicator0' 18754 1 26400 33554459 13 false nil nil nil nil nil nil nil 8 'indicator1' 18754 3 26400 33423361 5 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18290 1029 nil 18866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18290 1027 18866 60239 18866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18290 1031 18866 335 18866 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 7 850 #createAt:extent: 34 2 898 1 1 898 1091 563 26400 850 #modificationEventMask: 8 #(4607) 26400 850 #caretPeriod: 8 #(530) 26400 850 #wordWrap: 8 #(true) 26400 850 #margins: 34 1 34 3 19250 1 26400 49 3 nil nil nil nil 19250 3 26400 1 nil nil 67108863 nil nil 19250 5 26400 1 nil nil nil nil nil 26400 850 #canHScroll: 8 #(false) 26400 850 #maxCompletionListHeight: 8 #(9) 26400 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 25 1 0 0] 8 #() 2112 nil 29 170 192 34 4 26400 8 'before' 26096 8 'beforeTitle' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 1 898 1091 603 26000 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 45 1 0 0] 34 2 26400 26096 2112 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 25392 34 2 8 1140850688 1 27808 nil 18866 66047 nil 517 nil nil nil 27808 786 138 144 34 1 850 #createAt:extent: 34 2 898 1091 1 898 11 603 27808 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 33 2 0 0 0 0 0 0 38 2 0 0 45 1 0 0] 8 #() 2112 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 25392 34 2 8 1140850688 131073 28048 nil 1074 25344 nil 5 nil nil nil 28048 25362 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 28048 34 2 8 1140850945 262145 28144 nil nil nil 5 nil nil nil 28144 nil 8 1660708144 18226 nil nil nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 1 563 898 1093 41 28144 850 #text: 34 1 8 '
' 28144 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 34 2 0 0 45 1 0 0] 8 #() 2112 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 28048 34 2 8 1140920580 1025 28448 18018 nil false 18064 nil 1074 25344 nil 5 nil nil nil 28448 nil 8 4294902887 18226 nil nil 11 nil 170 192 34 12 #indentGuide 18290 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 18290 3 26592 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 18290 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 18290 77 18352 18368 1 nil nil nil nil #callTip nil nil nil #normal 18290 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 18290 5 nil 18866 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 28672 28624 28688 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 28640 nil nil nil 28608 nil nil 26706 26736 170 192 34 2 #default 18482 1 nil nil nil 28448 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 752 nil 170 176 752 9215 nil nil nil nil 18544 nil nil 65 nil nil nil 26832 3 170 192 34 2 #container 28576 nil nil nil nil 1 nil 170 192 34 6 8 'indicator1' 18754 3 28448 33423361 5 false nil nil nil nil nil nil nil 8 'indicator2' 18754 5 28448 511 1 false nil nil nil nil nil nil nil 8 'indicator0' 18754 1 28448 33554459 13 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18290 1029 nil 18866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18290 1027 18866 60239 18866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18290 1031 18866 335 18866 31317503 1 nil nil nil nil #Error nil nil nil nil 786 138 144 34 7 850 #createAt:extent: 34 2 898 1 1 898 1093 563 28448 850 #modificationEventMask: 8 #(4607) 28448 850 #caretPeriod: 8 #(530) 28448 850 #wordWrap: 8 #(true) 28448 850 #margins: 34 1 34 3 19250 1 28448 49 3 nil nil nil nil 19250 3 28448 1 nil nil 67108863 nil nil 19250 5 28448 1 nil nil nil nil nil 28448 850 #canHScroll: 8 #(false) 28448 850 #maxCompletionListHeight: 8 #(9) 28448 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 34 2 0 0 25 1 0 0] 8 #() 2112 nil 29 170 192 34 4 28448 8 'after' 28144 8 'afterTitle' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 1101 1 898 1093 603 28048 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 2 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 2 28448 28144 2112 nil 27 2112 nil 27 170 192 34 2 25392 8 'diffs' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 14162 nil true 34 4 14210 1 3186 #selectUpper 8 'Select &upper' 1 1 nil nil nil 14210 1 3186 #selectLower 8 'Select &lower' 1 1 nil nil nil 14322 4097 14210 1 3186 #exit 8 '&Close' 1 1 nil nil nil 8 '&File' nil 1 nil nil 42001 nil nil 14162 nil true 34 3 14210 1 3186 #character 8 '&Character' 1 1 nil nil nil 14210 1 3186 #word 8 '&Word' 1 1 nil nil nil 14210 1 3186 #line 8 '&Line' 1 1 nil nil nil 8 '&Mode' nil 1 nil nil 42003 nil nil 14162 nil true 34 1 14210 1 3186 #about 8 '&About' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 42005 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 786 138 144 34 2 850 #createAt:extent: 34 2 898 9 55 898 2225 635 25264 850 #updateMenuBar 752 25264 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 34 1 25392 2112 nil 27 25264 170 192 34 18 23008 8 'methodSource' 19600 8 'sunit' 19920 8 'classDefinition' 25264 8 'comparisonTab' 24432 8 'projectInfo' 16960 8 'globals' 17952 8 'originalSource' 13920 8 'packageInfo' 21712 8 'classDocumentation' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 13648 34 2 8 1140916736 1 30752 1458 138 144 34 9 24288 13792 16832 19888 21680 17824 22976 19440 25248 nil 1520 nil nil 1 nil nil nil 30752 nil 8 4294907939 1762 0 nil 8834 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 30880 7 257 nil 1762 0 nil 8834 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 30928 7 257 nil 1648 nil nil nil nil nil #smallIcons nil nil nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 898 1 1 898 2241 697 30752 850 #basicSelectionsByIndex: 34 1 8 #(9) 30752 850 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 30752 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 92 1 0 0] 8 #() 2112 nil 27 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 705 898 2241 697 13648 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 96 1 0 0 96 4 0 0 188 2 0 0] 34 10 24432 13920 16960 19920 21712 17952 23008 19600 25264 30752 2112 nil 27 7554 3 5 true 170 192 34 4 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 31424 nil 1074 4256 nil 517 nil nil nil 31424 786 138 144 34 1 850 #createAt:extent: 34 2 898 1 695 898 2241 11 31424 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 91 1 0 0 96 4 0 0 96 1 0 0] 8 #() 2112 nil 27 8 'splitter' 13648 8 'textAreaTabs' nil 786 138 144 34 1 850 #createAt:extent: 34 2 898 7679 21 898 2241 1401 416 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 95 19 0 0 198 2 0 0] 34 3 544 31424 13648 2112 nil 27 )! !
!RowanProjectsBrowserPresenter class categoriesFor: #downArrowIcon!public! !
!RowanProjectsBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

