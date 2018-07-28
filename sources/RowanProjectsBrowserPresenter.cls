"Filed out from Dolphin Smalltalk 7"!

RowanSystemBrowserPresenter subclass: #RowanProjectsBrowserPresenter
	instanceVariableNames: 'projectListPresenter stonCritical projectInfoTab projectLabel projectDictionaryTabs comparisonPresenter comparisonTab'
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

removeComparisonTab
	self textTabs remove: self comparisonTab ifAbsent: []!

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
!RowanProjectsBrowserPresenter categoriesFor: #addPackageDictionaryInfoTo:!public!request string! !
!RowanProjectsBrowserPresenter categoriesFor: #basicAddMethodCategory:!menu handlers!private!services! !
!RowanProjectsBrowserPresenter categoriesFor: #basicRemoveMethods!menu handlers!private! !
!RowanProjectsBrowserPresenter categoriesFor: #browseImplementorsOf:!menu handlers!public!queries! !
!RowanProjectsBrowserPresenter categoriesFor: #browseSendersOf:!menu handlers!public!queries! !
!RowanProjectsBrowserPresenter categoriesFor: #checkout!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #classesMenuStrings!menus!public! !
!RowanProjectsBrowserPresenter categoriesFor: #cloneProject!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #commit!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #comparisonTab!private!selection!text tabs! !
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
!RowanProjectsBrowserPresenter categoriesFor: #removeComparisonTab!private!selection!text tabs! !
!RowanProjectsBrowserPresenter categoriesFor: #removeMethodCategories!private!services! !
!RowanProjectsBrowserPresenter categoriesFor: #removeMethodsFrom:!private!services! !
!RowanProjectsBrowserPresenter categoriesFor: #selectClass:!private!selection! !
!RowanProjectsBrowserPresenter categoriesFor: #selectedMethodName!menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #selectionMethod:!private!selection! !
!RowanProjectsBrowserPresenter categoriesFor: #selectorsString!private!selection! !
!RowanProjectsBrowserPresenter categoriesFor: #selectPackage:!private!selection! !
!RowanProjectsBrowserPresenter categoriesFor: #setClassMeta!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #setMethodFilter!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #showComparisonTab!private!selection!text tabs! !
!RowanProjectsBrowserPresenter categoriesFor: #showPackages!project menu handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #textTabChanged!event handlers!public! !
!RowanProjectsBrowserPresenter categoriesFor: #textTabs!private!text tabs! !
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
!RowanProjectsBrowserPresenter categoriesFor: #updateMultiMethodComparison!private!text tabs! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackage!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackageInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updatePackages!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProject!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProjectInfo!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProjects!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateProjectTab!event handlers!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateSelectionsForUpdate:!public!updating! !
!RowanProjectsBrowserPresenter categoriesFor: #updateServices:!private!services! !
!RowanProjectsBrowserPresenter categoriesFor: #updateSingleMethodSource!private!text tabs! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 656 nil nil nil 5 nil nil nil 656 482 170 176 34 6 410 ##(Smalltalk.ListView) 34 30 nil 656 34 2 8 1409405257 1025 768 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 768 nil 8 4294902921 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 361 #left 994 #displayString 8 #() ##(Smalltalk.SortedCollection) nil nil 768 nil 3 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 1200 7 257 nil 1186 0 nil 1218 3 1 1184 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 1296 7 257 nil #report 880 nil 133217 nil 34 4 nil nil 328198 ##(Smalltalk.Point) 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 1394 1 1 1394 369 695 768 1490 #text: 34 1 8 'Column 1' 768 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 1394 193 193 nil 27 524806 ##(Smalltalk.Fraction) 9 11 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 1728 nil nil nil 5 nil nil nil 1728 482 170 176 880 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 1728 34 2 8 1140936777 1025 1856 834 138 144 880 nil 912 930 8 4278190080 nil 21 nil nil nil 1856 nil 8 4294902921 994 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 'Column 1' 361 #left 1186 0 nil 1218 2 1 1184 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 2096 7 257 nil 994 #<= 8 #() nil nil 1856 nil 3 1186 0 nil 1218 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 2208 7 257 nil 1186 0 nil 1218 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 2288 7 257 nil #report 880 nil 133217 nil 34 4 nil nil 1394 1 1 nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 1394 1 1 1394 369 695 1856 1490 #text: 34 1 8 'Column 1' 1856 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 1680 nil 27 8 'packagesList' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 379 1 1394 369 695 1728 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 189 0 0 0 0 0 0 0 117 1 0 0 91 1 0 0] 34 1 1856 1680 nil 27 1698 9 11 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 2800 nil nil nil 5 nil nil nil 2800 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 2800 34 2 8 1409286144 131073 2928 nil 930 8 4278190080 nil 5 nil nil nil 2928 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Instance' 1186 0 nil 1218 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 3120 3152 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 3168 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 2928 34 2 8 1140850688 131073 3264 nil nil nil 5 nil nil nil 3264 nil 170 192 880 nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 9 9 1394 567 1 3264 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 1680 nil 27 3090 3122 8 'Class' 1186 0 nil 1218 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 3120 3536 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 3552 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 2928 34 2 8 1140850688 131073 3648 nil nil nil 5 nil nil nil 3648 nil 170 192 880 nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 9 9 1394 567 1 3648 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 1680 nil 27 3264 170 192 34 4 3648 8 'classTab' 3264 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 2928 34 2 8 1140916738 1 3952 834 138 144 34 2 3136 3520 nil 912 nil nil 1 nil nil nil 3952 nil 8 4294907939 1186 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 4080 7 257 nil 1186 0 nil 4098 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 4144 7 257 nil 1056 nil nil nil nil nil #smallIcons nil nil nil nil nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 1394 1 1 1394 583 51 3952 1490 #basicSelectionsByIndex: 34 1 8 #(1) 3952 1490 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 3952 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 25 0 0 0] 8 #() 1680 nil 27 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1 645 1394 583 51 2928 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 66 1 0 0 35 1 0 0 91 1 0 0] 34 3 3264 3648 3952 1680 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 4626 #fixedParentRight 1 4626 #fixedParentBottom -49 4626 #fixedViewTop 51 410 ##(Smalltalk.CardContainer) 34 16 nil 2800 34 2 8 1409286144 131073 4704 nil 930 8 4278190080 nil 5 nil nil nil 4704 3026 138 144 34 2 3090 3122 8 'Classes' 1186 0 nil 1218 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 3120 4880 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 4896 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 4704 34 2 8 1409372237 1025 4992 834 138 144 880 nil 912 930 960 nil 29 nil nil nil 4992 nil 8 4294902921 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 '' 559 #left 994 #displayString 8 #() 994 #<= 8 #() nil nil 4992 nil 3 1186 0 nil 1218 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 5280 7 257 nil 1186 0 nil 1218 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 5360 7 257 nil #report 880 nil 133217 nil 34 4 nil nil 1394 1 1 nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 9 55 1394 567 589 4992 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 1680 nil 27 3090 3122 8 'Hierarchy' 1186 0 nil 1218 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 3120 5664 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 5680 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 4704 34 2 8 1409352231 1025 5776 590918 3 ##(Smalltalk.TreeModel) nil 912 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 880 930 8 4278190080 nil 29 nil nil nil 5776 nil 8 4294908757 1186 0 nil 1218 2 1 992 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 5968 7 257 nil ##(Smalltalk.IconicListAbstract) 1056 nil nil nil nil nil 170 176 880 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 880 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 9 55 1394 567 589 5776 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 1680 nil 27 4992 170 192 34 4 4992 8 'classList' 5776 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 4704 34 2 8 1140916736 1 6304 834 138 144 34 2 4864 5648 nil 912 nil nil 1 nil nil nil 6304 nil 8 4294907939 1186 0 nil 4098 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 6432 7 257 nil 1186 0 nil 4098 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 6480 7 257 nil 1056 nil nil nil nil nil #smallIcons nil nil nil nil nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 1394 1 1 1394 583 651 6304 1490 #basicSelectionsByIndex: 34 1 8 #(1) 6304 1490 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 6304 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 8 #() 1680 nil 27 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1 1 1394 583 651 4704 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 34 3 4992 5776 6304 1680 nil 27 4594 4640 1 4656 1 4626 #fixedParentTop 1 4672 -43 170 192 34 4 2928 8 'instanceClassTabs' 4704 8 'classHierarchyTabs' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 757 1 1394 583 695 2800 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 122 1 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 2 4704 2928 1680 nil 27 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 244 63 false 170 192 34 2 768 8 'projectList' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1 1 1394 1339 695 656 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 5 768 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 7424 nil 930 8 4278190080 nil 517 nil nil nil 7424 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 369 1 1394 11 695 7424 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 184 0 0 0 0 0 0 0 189 0 0 0 91 1 0 0] 8 #() 1680 nil 27 1728 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 7680 nil 930 7504 nil 517 nil nil nil 7680 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 747 1 1394 11 695 7680 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 117 1 0 0 0 0 0 0 122 1 0 0 91 1 0 0] 8 #() 1680 nil 27 2800 1680 nil 27 7 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 7920 nil nil nil 5 nil nil nil 7920 482 170 176 880 false 170 192 880 nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1349 1 1394 893 695 7920 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 162 2 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 7920 34 2 8 1409286144 131073 8192 nil 930 8 4278190080 nil 517 nil nil nil 8192 3026 138 144 34 3 3090 8 'Categories' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 8192 34 2 8 1140850688 131073 8400 nil 930 8272 nil 5 nil nil nil 8400 8288 170 192 34 6 8368 8 'categoryList' 410 ##(Smalltalk.ListView) 34 30 nil 8400 34 2 8 1409372233 1025 8528 834 138 144 880 nil 912 930 8 4278190080 nil 21 nil nil nil 8528 nil 8 4294902921 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 'Column 1' 373 #left 994 #displayString 8688 ##(Smalltalk.SortedCollection) nil nil 8528 nil 3 nil 1186 0 nil 1218 9 1 1184 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 8784 7 257 nil #report 880 nil 131169 nil 34 4 nil nil 1394 1 1 nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 1394 1 37 1394 381 643 8528 1490 #text: 34 1 8 'Column 1' 8528 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 190 0 0 0 83 1 0 0] 8 #() 1680 nil 27 8 'variableList' 410 ##(Smalltalk.ListView) 34 30 nil 8400 34 2 8 1409372233 1025 9120 834 138 144 880 nil 912 930 8640 nil 21 nil nil nil 9120 nil 8 4294902921 994 #displayString 8688 nil 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 'Column 1' 373 #left 994 #displayString 8688 ##(Smalltalk.SortedCollection) nil nil 9120 nil 3 nil 1186 0 nil 1218 9 1 1184 8816 8832 8848 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 9344 7 257 nil #report 880 nil 131169 nil 34 4 nil nil 1394 1 1 nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 1394 1 37 1394 381 643 9120 1490 #text: 34 1 8 'Column 1' 9120 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 190 0 0 0 83 1 0 0] 8 #() 1680 nil 27 8 'pragmaList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 8192 34 2 8 1140916864 1 9632 834 138 144 34 3 8352 8 'Variables' 8 'Pragmas' nil 912 930 8272 nil 1 nil nil nil 9632 nil 8 4294907939 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1056 nil nil nil nil nil #noIcons nil nil nil nil nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 1394 1 1 1394 441 695 9632 1490 #basicSelectionsByIndex: 34 1 8 #(1) 9632 1490 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 9632 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 8 #() 1680 nil 27 nil 8192 1394 33 33 1049862 ##(Smalltalk.ButtonInteractor) 8400 nil 590342 ##(Smalltalk.Rectangle) 1394 345 3 1394 377 35 1 1180998 4 ##(Smalltalk.CommandDescription) #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 53 9 1394 381 679 8400 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 216 0 0 0 87 1 0 0] 34 3 8368 8528 9120 1680 nil 27 34 2 8 1409372233 1025 8368 834 138 144 880 nil 912 930 8640 nil 21 nil nil nil 8368 nil 8 4294902921 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 'Column 1' 373 #left 994 #displayString 8 #() ##(Smalltalk.SortedCollection) 1186 0 nil 1218 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each]' 1779525917 10592 7 257 nil nil 8368 nil 3 nil nil #report 880 nil 131169 nil 34 4 nil nil 1394 1 1 nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 1394 1 37 1394 381 643 8368 1490 #text: 34 1 8 'Column 1' 8368 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 190 0 0 0 83 1 0 0] 8 #() 1680 nil 27 3090 9744 8528 3090 9760 9120 8368 170 192 34 2 8400 8 'methodFilterTabs' nil 9632 8400 1394 201 201 401 1 31 nil nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1 1 1394 441 695 8192 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 34 2 8400 9632 1680 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 7920 34 2 8 1140850688 1 11152 nil 930 7504 nil 517 nil nil nil 11152 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 441 1 1394 11 695 11152 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 220 0 0 0 0 0 0 0 225 0 0 0 91 1 0 0] 8 #() 1680 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 7920 34 2 8 1140850688 131073 11392 nil nil nil 5 nil nil nil 11392 2866 170 176 34 6 410 ##(Smalltalk.ComboBox) 34 17 nil 11392 34 2 8 1412498947 1025 11504 834 138 144 880 nil 912 930 8 4278190080 nil 5 nil nil nil 11504 nil 8 4294902727 994 #displayString 8 #() 880 401 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1 1 1394 443 53 11504 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 221 0 0 0 26 0 0 0] 8 #() 1680 nil 27 4594 4640 1 4656 1 6944 1 4688 47 410 ##(Smalltalk.ListView) 34 30 nil 11392 34 2 8 1409372233 1025 11856 834 138 144 880 nil 912 930 8640 nil 13 nil nil nil 11856 nil 8 4294902921 994 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1056 nil nil nil nil nil nil 138 144 34 2 1106 8 '' 41 #left 1490 #empty 8 #() 16 994 #<= 8 #() nil nil 11856 1186 0 nil 1218 2 1 1184 8 'doIt' 8 '[:each | each subSuperIcon]' 8 #[30 105 226 0 106] #subSuperIcon 12144 7 257 nil 1 nil nil 1106 8 'Selector' 395 #left 994 #displayString 12000 ##(Smalltalk.SortedCollection) 1186 0 nil 1218 2 1 1184 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 12272 7 257 nil nil 11856 nil 3 nil 1186 0 nil 1218 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 12352 7 257 nil #report 880 nil 131173 nil 34 4 nil nil 1394 1 1 nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1 47 1394 443 603 11856 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 221 0 0 0 68 1 0 0] 8 #() 1680 nil 27 4594 4640 1 4656 1 6944 47 4672 -45 410 ##(Smalltalk.ComboBox) 34 17 nil 11392 34 2 8 1412498947 1025 12640 834 138 144 880 nil 912 930 11616 nil 5 nil nil nil 12640 nil 8 4294902727 994 #displayString 8 #() 880 401 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1 649 1394 443 53 12640 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 68 1 0 0 221 0 0 0 94 1 0 0] 8 #() 1680 nil 27 4594 4640 1 4656 1 4672 -45 4688 47 170 192 34 6 11504 8 'superclassList' 11856 8 'methodList' 12640 8 'overrideList' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 451 1 1394 443 695 11392 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 225 0 0 0 0 0 0 0 190 1 0 0 91 1 0 0] 34 3 11504 11856 12640 1680 nil 27 1680 nil 27 5 false 170 192 880 nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1 1 1394 2241 695 544 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 656 410 ##(Smalltalk.Splitter) 34 12 nil 544 34 2 8 1140850688 1 13392 nil 930 7504 nil 517 nil nil nil 13392 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1339 1 1394 11 695 13392 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 157 2 0 0 0 0 0 0 162 2 0 0 91 1 0 0] 8 #() 1680 nil 27 7920 1680 nil 27 1698 3 5 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 13648 nil 930 4784 nil 5 nil nil nil 13648 3026 138 144 34 9 3090 3122 8 'Package' 1186 0 nil 1218 7 1 3120 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 3120 13808 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 13824 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 13648 34 2 8 1140850688 131073 13920 nil nil nil 5 nil nil nil 13920 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 13920 34 2 8 1409355853 1025 14032 834 138 144 880 nil 912 930 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 10162 #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 14032 nil 8 4294902921 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 4 1106 8 'Ancestor' 131 #left 994 #displayString 8 #() 994 #<= 14416 1186 0 nil 1218 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 14448 7 257 nil nil 14032 nil 1 nil nil 1106 8 'Name' 401 #left 994 #displayString 14320 ##(Smalltalk.SortedCollection) 1186 0 nil 1218 1 83886081 1184 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 14576 7 257 nil nil 14032 nil 1 nil nil 1106 8 'Timestamp' 301 #left 994 #displayString 8 #() 994 #<= 14704 1186 0 nil 1218 1 83886081 1184 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 14736 7 257 nil nil 14032 nil 1 nil nil 1106 8 'Message' 941 #left 994 #displayString 14704 994 #<= 14704 1186 0 nil 1218 1 83886081 1184 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 14880 7 257 nil nil 14032 nil 3 nil nil #report 880 nil 131169 nil 34 4 nil nil 1394 1 1 nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 1394 1 1 1394 1769 263 14032 1490 #contextMenu: 34 1 14176 14032 1490 #text: 34 1 8 'Ancestor' 14032 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 1680 nil 27 23 410 ##(Smalltalk.ListView) 34 30 nil 13920 34 2 8 1409355853 1025 15232 834 138 144 880 nil 912 930 14144 nil 5 14162 nil true 34 5 14210 1 10162 #addRepository 8 'Add &Repository' 1 1 nil nil nil 14210 1 10162 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 14210 1 10162 #savePackage 8 '&Save Package' 1 1 nil nil nil 14210 1 10162 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 15232 nil 8 4294902921 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 4 1106 8 'Repository Type' 211 #left 994 #displayString 15648 ##(Smalltalk.SortedCollection) 1186 0 nil 1218 2 1 1184 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 15744 7 257 nil nil 15232 nil 1 nil nil 1106 8 'Description' 1161 #left 994 #displayString 14704 994 #<= 14704 1186 0 nil 1218 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 15888 7 257 nil nil 15232 nil 3 nil nil 1106 8 'User' 201 #left 994 #displayString 14416 994 #<= 14416 1186 0 nil 1218 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 16032 7 257 nil nil 15232 nil 1 nil nil 1106 8 'Password' 201 #left 994 #displayString 14416 994 #<= 14416 1186 0 nil 1218 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 16176 1186 0 nil 16192 19 257 nil 7 257 nil nil 15232 nil 1 nil nil #report 880 nil 131169 nil 34 4 nil nil 1394 1 1 nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 1394 1 263 1394 1769 217 15232 1490 #contextMenu: 34 1 15344 15232 1490 #text: 34 1 8 'Repository Type' 15232 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 1680 nil 27 19 true 170 192 34 4 14032 8 'ancestorList' 15232 8 'repositoryList' nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 1394 9 55 1394 2225 635 13920 1490 #text: 34 1 8 'Package' 13920 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 34 2 14032 15232 1680 nil 27 3090 3122 8 'Globals' 1186 0 nil 1218 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 3120 16848 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 16864 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 13648 34 2 8 1409355853 1025 16960 834 138 144 880 nil 912 930 14144 nil 5 nil nil nil 16960 nil 8 4294902921 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 3 1106 8 'Name' 739 #left 994 #displayString 17104 ##(Smalltalk.SortedCollection) 1186 0 nil 1218 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 17200 7 257 nil nil 16960 nil 3 nil nil 1106 8 'Class' 741 #left 994 #displayString 14704 994 #<= 14704 1186 0 nil 1218 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 17344 7 257 nil nil 16960 nil 3 nil nil 1106 8 'Value' 739 #left 994 #displayString 14704 994 #<= 14704 1186 0 nil 1218 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 17488 7 257 nil nil 16960 nil 3 nil nil #report 880 nil 131169 nil 34 4 nil nil 1394 1 1 nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 1394 9 55 1394 2225 635 16960 1490 #text: 34 1 8 'Name' 16960 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1680 nil 27 3090 3122 8 'Original Source' 1186 0 nil 1218 7 1 16 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 3120 17840 ##(Smalltalk.Icon) 8 'COMPILEDMETHOD_PRIVATE.ico' #fromId: #text:iconBlock: 17856 11 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13648 34 2 8 1445007428 1025 17952 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 930 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1394 193 193 nil 17952 nil 8 4294902887 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 18290 77 786694 ##(Smalltalk.IndexedColor) 33554457 18338 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 18304 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 18320 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 880 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 17952 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 18338 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 18304 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 17952 65025 3 false 1 nil nil nil nil nil nil 3 18754 3 17952 33423361 5 false 3 nil nil nil nil nil nil 5 18754 5 17952 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18290 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18290 1027 18866 60239 18866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18290 1031 18866 335 18866 31317503 1 nil nil nil nil #Error nil nil nil nil 1426 138 144 34 7 1490 #createAt:extent: 34 2 1394 9 55 1394 2225 635 17952 1490 #modificationEventMask: 8 #(4607) 17952 1490 #caretPeriod: 8 #(530) 17952 1490 #wordWrap: 8 #(true) 17952 1490 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 17952 61 3 nil nil 5 nil 19250 3 17952 1 nil 3 67108863 5 nil 19250 5 17952 1 nil 3 -67108863 5 nil 17952 1490 #tabWidth: 8 #(4) 17952 1490 #maxCompletionListHeight: 8 #(9) 17952 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1680 nil 29 3090 3122 8 'SUnit' 1186 0 nil 1218 4 1 1184 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 19472 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 13648 34 2 8 1140850688 131073 19600 nil 930 8 4278190080 nil 5 nil nil nil 19600 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 9 55 1394 2225 635 19600 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 880 1680 nil 27 3090 3122 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13648 34 2 8 1445007428 1025 19920 18018 nil false 18064 nil 930 8 4278190080 nil 5 14162 nil true 34 11 14210 1 10162 #editSave 8 '&Save' 9383 1 nil nil nil 15474 4097 14210 1 10162 #editUndo 8 '&Undo' 9397 1 nil nil nil 14210 1 10162 #editRedo 8 '&Redo' 9395 1 nil nil nil 15474 4097 14210 1 10162 #editCut 8 'Cu&t' 9393 1 nil nil nil 14210 1 10162 #editCopy 8 '&Copy' 9351 1 nil nil nil 14210 1 10162 #editPaste 8 '&Paste' 9389 1 nil nil nil 14210 1 10162 #editDelete 8 '&Delete' 1629 1 nil nil nil 15474 4097 14210 1 10162 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 18114 nil true 18146 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1394 193 193 nil 19920 nil 8 4294902887 18226 nil nil 13 nil 170 192 34 4 #normal 18290 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 18290 77 18352 18368 1 nil nil nil nil #callTip nil nil nil 34 256 20640 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20656 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 880 170 192 34 2 #default 18482 1 nil nil nil 19920 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 18544 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 20640 nil nil nil nil 1 nil 170 192 34 6 1 18754 1 19920 65025 3 false 1 nil nil nil nil nil nil 3 18754 3 19920 33423361 5 false 3 nil nil nil nil nil nil 5 18754 5 19920 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18290 1029 nil 18866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18290 1027 18866 60239 18866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18290 1031 18866 335 18866 31317503 1 nil nil nil nil #Error nil nil nil nil 1426 138 144 34 8 1490 #createAt:extent: 34 2 1394 9 55 1394 2225 635 19920 1490 #contextMenu: 34 1 20032 19920 1490 #modificationEventMask: 8 #(4607) 19920 1490 #caretPeriod: 8 #(530) 19920 1490 #wordWrap: 8 #(true) 19920 1490 #margins: 34 1 34 3 19250 1 19920 1 3 nil nil 5 nil 19250 3 19920 33 nil 3 67108863 5 nil 19250 5 19920 1 nil 3 -67108863 5 nil 19920 1490 #tabWidth: 8 #(4) 19920 1490 #maxCompletionListHeight: 8 #(9) 19920 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1680 nil 29 3090 3122 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13648 34 2 8 1445007428 1025 21712 18018 nil false 18064 nil 930 20016 nil 5 14162 nil true 34 1 14210 1 10162 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 18114 nil true 18146 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1394 193 193 nil 21712 nil 8 4294902887 18226 nil nil 11 nil 170 192 34 4 #normal 18290 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 18290 77 18352 18368 1 nil nil nil nil #callTip nil nil nil 34 256 22032 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 22048 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 18482 1 nil nil nil 21712 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 18544 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 22032 nil nil nil nil 1 nil 170 192 34 6 1 18754 1 21712 65025 3 false 1 nil nil nil nil nil nil 3 18754 3 21712 33423361 5 false 3 nil nil nil nil nil nil 5 18754 5 21712 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18290 1029 nil 18866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18290 1027 18866 60239 18866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18290 1031 18866 335 18866 31317503 1 nil nil nil nil #Error nil nil nil nil 1426 138 144 34 8 1490 #createAt:extent: 34 2 1394 9 55 1394 2225 635 21712 1490 #contextMenu: 34 1 21808 21712 1490 #modificationEventMask: 8 #(4607) 21712 1490 #caretPeriod: 8 #(530) 21712 1490 #wordWrap: 8 #(true) 21712 1490 #margins: 34 1 34 3 19250 1 21712 1 3 nil nil 5 nil 19250 3 21712 33 nil 3 67108863 5 nil 19250 5 21712 1 nil 3 -67108863 5 nil 21712 1490 #tabWidth: 8 #(4) 21712 1490 #maxCompletionListHeight: 8 #(9) 21712 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1680 nil 29 3090 3122 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 13648 34 2 8 1445007428 1025 23008 18018 nil false 18064 nil 930 8 4278190080 nil 5 nil 18114 nil true 18146 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1394 193 193 nil 23008 nil 8 4294902887 18226 nil nil 13 nil 170 192 34 4 #normal 18290 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 18290 77 18352 18368 1 nil nil nil nil #callTip nil nil nil 34 256 23248 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 23264 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 18402 1 nil nil false 138 144 880 170 192 34 2 #default 18482 1 nil nil nil 23008 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 18544 nil nil 65 nil nil 20816 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 23248 #callTip 23264 nil nil nil nil 1 nil 170 192 34 12 1 18754 1 23008 65025 3 false 1 nil nil nil nil nil nil 3 18754 3 23008 33423361 5 false 3 nil nil nil nil nil nil 5 18754 5 23008 511 1 false 5 nil nil nil nil nil nil 8 'indicator8' 18754 17 23008 33554447 1 nil nil nil nil nil nil nil nil 8 'indicator9' 18754 19 23008 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator10' 18754 21 23008 33554459 3 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18290 1029 nil 18866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18290 1027 18866 60239 18866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18290 1031 18866 335 18866 31317503 1 nil nil nil nil #Error nil nil nil nil 1426 138 144 34 8 1490 #createAt:extent: 34 2 1394 9 55 1394 2225 635 23008 1490 #modificationEventMask: 8 #(4607) 23008 1490 #hoverTime: 8 #(200) 23008 1490 #caretPeriod: 8 #(530) 23008 1490 #wordWrap: 8 #(true) 23008 1490 #margins: 34 1 34 3 19250 1 23008 61 3 nil nil 5 nil 19250 3 23008 1 nil 3 67108863 nil nil 19250 5 23008 1 nil 3 -67108863 nil nil 23008 1490 #tabWidth: 8 #(4) 23008 1490 #maxCompletionListHeight: 8 #(9) 23008 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1680 nil 29 3090 3122 8 'Project' 1186 0 nil 1218 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 19554 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 24320 7 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 13648 34 2 8 1409372237 1025 24432 834 138 144 880 nil 912 930 14144 nil 5 nil nil nil 24432 nil 8 4294902921 994 #displayString 17104 nil 1056 nil nil nil nil nil nil 138 144 34 2 1106 8 'Name' 201 #left 994 #displayString 17104 ##(Smalltalk.SortedCollection) 1186 0 nil 1218 1 83886081 1184 8 'doIt' 8 '[:each | (each at: 2)]' 8 #[29 105 17 64 148 106] 24656 7 257 nil nil 24432 nil 1 nil 1186 0 nil 1218 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each item at: 1) displayStringFor: each]' 8 #[31 105 226 0 63 148 17 177 106] #item #displayStringFor: 24736 7 257 nil 1106 8 'Value' 2017 #left 994 #displayString 8 #() 994 #<= 24864 1186 0 nil 1218 4 1 1184 8 'doIt' 8 '[:each | (each at: 2) ifNil:[String new] ifNotNil: [:iv |
(each at: 1) instVarNamed: iv]]' 8 #[32 105 17 64 148 100 223 4 97 45 146 106 90 17 63 148 18 178 106] 19554 #String 16 #ifNil:ifNotNil: #instVarNamed: 24896 7 65793 nil nil 24432 nil 3 nil nil #report 880 nil 131169 nil 34 4 nil nil 1394 1 1 nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 1394 9 55 1394 2225 635 24432 1490 #text: 34 1 8 'Name' 24432 1618 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1680 nil 27 3090 8 'Comparison' 410 ##(Smalltalk.ShellView) 34 27 nil 13648 34 2 8 1078919168 131073 25264 nil 930 8 4278190080 nil 517 nil nil nil 25264 788230 ##(Smalltalk.BorderLayout) 1 1 nil nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 25264 34 2 8 1140850688 131073 25392 nil 930 25344 nil 5 14162 nil true 34 5 14210 1 10162 #toggleIndentGuides 8 'Indentation &Guides' 1 1 nil nil nil 14210 1 10162 #toggleVisibleLineEndings 8 'Line &Endings' 1 1 nil nil nil 14210 1 10162 #toggleLineNumberMargins 8 'Line N&umbers' 1 1 nil nil nil 14210 1 10162 #toggleVisibleWhitespace 8 'W&hitespace' 1 1 nil nil nil 14210 1 10162 #toggleWrapping 8 '&Word Wrap' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 25392 482 170 176 880 false 170 192 880 nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 1394 1 1 1394 2193 603 25392 1490 #contextMenu: 34 1 25472 25392 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 25392 34 2 8 1140850688 131073 26000 nil 930 25344 nil 5 nil nil nil 26000 25362 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 26000 34 2 8 1140850945 262145 26096 nil nil nil 5 nil nil nil 26096 nil 8 1660708144 18226 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 1394 1 563 1394 1091 41 26096 1490 #text: 34 1 8 '
' 26096 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 33 2 0 0 45 1 0 0] 8 #() 1680 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 26000 34 2 8 1140920580 1025 26400 18018 nil false 18064 nil 930 25344 nil 5 nil nil nil 26400 nil 8 4294902887 18226 nil nil 11 nil 170 192 34 12 #indentGuide 18290 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 18290 3 18338 33554445 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 18290 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 18290 77 18352 18368 1 nil nil nil nil #callTip nil nil nil #normal 18290 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 18290 5 nil 18866 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 26640 26576 26656 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 26608 nil nil nil 26560 nil nil 1310982 ##(Smalltalk.DiffsScintillaStyler) 8 #() 170 192 34 2 #default 18482 1 nil nil nil 26400 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 18544 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 26528 nil nil nil nil 1 nil 170 192 34 6 8 'indicator0' 18754 1 26400 33554459 13 false nil nil nil nil nil nil nil 8 'indicator2' 18754 5 26400 511 1 false nil nil nil nil nil nil nil 8 'indicator1' 18754 3 26400 33423361 5 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18290 1029 nil 18866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18290 1027 18866 60239 18866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18290 1031 18866 335 18866 31317503 1 nil nil nil nil #Error nil nil nil nil 1426 138 144 34 7 1490 #createAt:extent: 34 2 1394 1 1 1394 1091 563 26400 1490 #modificationEventMask: 8 #(4607) 26400 1490 #caretPeriod: 8 #(530) 26400 1490 #wordWrap: 8 #(true) 26400 1490 #margins: 34 1 34 3 19250 1 26400 49 3 nil nil nil nil 19250 3 26400 1 nil nil 67108863 nil nil 19250 5 26400 1 nil nil nil nil nil 26400 1490 #canHScroll: 8 #(false) 26400 1490 #maxCompletionListHeight: 8 #(9) 26400 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 25 1 0 0] 8 #() 1680 nil 29 170 192 34 4 26096 8 'beforeTitle' 26400 8 'before' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1 1 1394 1091 603 26000 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 45 1 0 0] 34 2 26400 26096 1680 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 25392 34 2 8 1140850688 1 27808 nil 18866 66047 nil 517 nil nil nil 27808 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1091 1 1394 11 603 27808 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 33 2 0 0 0 0 0 0 38 2 0 0 45 1 0 0] 8 #() 1680 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 25392 34 2 8 1140850688 131073 28048 nil 930 25344 nil 5 nil nil nil 28048 25362 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 28048 34 2 8 1140850945 262145 28144 nil nil nil 5 nil nil nil 28144 nil 8 1660708144 18226 nil nil nil 1426 138 144 34 2 1490 #createAt:extent: 34 2 1394 1 563 1394 1093 41 28144 1490 #text: 34 1 8 '
' 28144 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 34 2 0 0 45 1 0 0] 8 #() 1680 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 28048 34 2 8 1140920580 1025 28448 18018 nil false 18064 nil 930 25344 nil 5 nil nil nil 28448 nil 8 4294902887 18226 nil nil 11 nil 170 192 34 12 #indentGuide 18290 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 18290 3 26592 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 18290 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 18290 77 18352 18368 1 nil nil nil nil #callTip nil nil nil #normal 18290 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 18290 5 nil 18866 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 28672 28624 28688 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 28640 nil nil nil 28608 nil nil 26706 26736 170 192 34 2 #default 18482 1 nil nil nil 28448 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 18544 nil nil 65 nil nil nil 26832 3 170 192 34 2 #container 28576 nil nil nil nil 1 nil 170 192 34 6 8 'indicator2' 18754 5 28448 511 1 false nil nil nil nil nil nil nil 8 'indicator1' 18754 3 28448 33423361 5 false nil nil nil nil nil nil nil 8 'indicator0' 18754 1 28448 33554459 13 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 18290 1029 nil 18866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 18290 1027 18866 60239 18866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 18290 1031 18866 335 18866 31317503 1 nil nil nil nil #Error nil nil nil nil 1426 138 144 34 7 1490 #createAt:extent: 34 2 1394 1 1 1394 1093 563 28448 1490 #modificationEventMask: 8 #(4607) 28448 1490 #caretPeriod: 8 #(530) 28448 1490 #wordWrap: 8 #(true) 28448 1490 #margins: 34 1 34 3 19250 1 28448 49 3 nil nil nil nil 19250 3 28448 1 nil nil 67108863 nil nil 19250 5 28448 1 nil nil nil nil nil 28448 1490 #canHScroll: 8 #(false) 28448 1490 #maxCompletionListHeight: 8 #(9) 28448 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 34 2 0 0 25 1 0 0] 8 #() 1680 nil 29 170 192 34 4 28144 8 'afterTitle' 28448 8 'after' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1101 1 1394 1093 603 28048 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 2 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 2 28448 28144 1680 nil 27 1680 nil 27 170 192 34 2 25392 8 'diffs' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 14162 nil true 34 4 14210 1 10162 #selectUpper 8 'Select &upper' 1 1 nil nil nil 14210 1 10162 #selectLower 8 'Select &lower' 1 1 nil nil nil 15474 4097 14210 1 10162 #exit 8 '&Close' 1 1 nil nil nil 8 '&File' nil 1 nil nil 61255 nil nil 14162 nil true 34 3 14210 1 10162 #character 8 '&Character' 1 1 nil nil nil 14210 1 10162 #word 8 '&Word' 1 1 nil nil nil 14210 1 10162 #line 8 '&Line' 1 1 nil nil nil 8 '&Mode' nil 1 nil nil 61257 nil nil 14162 nil true 34 1 14210 1 10162 #about 8 '&About' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 61259 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 1394 9 55 1394 2225 635 25264 1490 #text: 34 1 8 'Comparison' 25264 1490 #updateMenuBar 880 25264 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 34 1 25392 1680 nil 27 25264 170 192 34 18 24432 8 'projectInfo' 23008 8 'methodSource' 13920 8 'packageInfo' 17952 8 'originalSource' 25264 8 'comparisonTab' 19600 8 'sunit' 19920 8 'classDefinition' 16960 8 'globals' 21712 8 'classDocumentation' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 13648 34 2 8 1140916736 1 30800 834 138 144 34 9 24288 13792 16832 19888 21680 17824 22976 19440 25248 nil 912 nil nil 1 nil nil nil 30800 nil 8 4294907939 1186 0 nil 4098 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 30928 7 257 nil 1186 0 nil 4098 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 30976 7 257 nil 1056 nil nil nil nil nil #smallIcons nil nil nil nil nil 1426 138 144 34 3 1490 #createAt:extent: 34 2 1394 1 1 1394 2241 697 30800 1490 #basicSelectionsByIndex: 34 1 8 #(9) 30800 1490 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 30800 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 92 1 0 0] 8 #() 1680 nil 27 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1 705 1394 2241 697 13648 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 96 1 0 0 96 4 0 0 188 2 0 0] 34 10 24432 13920 16960 19920 21712 17952 23008 19600 25264 30800 1680 nil 27 1698 3 5 true 170 192 34 4 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 31472 nil 930 7504 nil 517 nil nil nil 31472 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 1 695 1394 2241 11 31472 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 91 1 0 0 96 4 0 0 96 1 0 0] 8 #() 1680 nil 27 8 'splitter' 13648 8 'textAreaTabs' nil 1426 138 144 34 1 1490 #createAt:extent: 34 2 1394 7679 21 1394 2241 1401 416 1618 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 95 19 0 0 198 2 0 0] 34 3 544 31472 13648 1680 nil 27 )! !
!RowanProjectsBrowserPresenter class categoriesFor: #downArrowIcon!public! !
!RowanProjectsBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

