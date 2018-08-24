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
	| projectService newPackageName |
	(newPackageName := Prompter prompt: 'Enter new package name:') isNil ifTrue: [^self].
	projectService := projectListPresenter selection.
	projectService addPackageNamed: newPackageName using: self!

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
		command: #allClasses.
	self issueCommand: (Array with: browserService).
	^browserService allClasses!

basicAddMethodCategory: string
	| service |
	service := classListPresenter selection
				command: #addCategory:;
				commandArgs: (Array with: string).
	self issueCommand: (Array with: service).
	categoryListPresenter selection: string. !

basicRemoveMethods
	| service |
	service := classListPresenter selectionIfNone: [^self].
	self setClassMeta.
	self removeMethods: (Array with: service)!

browseClassReferences
	self browseClassReferences: (classListPresenter selectionIfNone: [^self]) name!

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

browseMethodHistory

	MessageBox warning: 'Method history is not available yet'. !

browseSelectedClass
	| range className classService |
	range := methodSourcePresenter view selectionRange.
	className := methodSourcePresenter value copyFrom: range start to: range stop.
	classService := RowanClassService named: className.
	self updateService: classService.
	self updateAfterFindClass: classService!

categoriesMenuStrings

	false ifTrue: [
		self addMethodCategory; renameMethodCategory; removeMethodCategories.
	].
	^#(
		'&Categories'
		'&Add//addMethodCategory'
		'&Rename//renameMethodCategory'
		'&Remove//removeMethodCategories'
	).
!

categoryListPresenter
	^categoryListPresenter!

checkout
	projectListPresenter selection checkoutUsing: self.
	self refreshFromServer!

classesMenuStrings

	false ifTrue: [
		self browseClassReferences; fileOutClass; addSubclass; removeClass; removePriorVersions; runClassTests.
	].
	^#(
		'&Classes'
		'&Browse References//browseClassReferences'
		'&File Out Class//fileOutClass'
		'Add &Subclass//addSubclass'
		'&Remove//removeClass'
		'-'
		'Set Compiler &Environment ...//setEnvironment'
		'Run &Tests//runClassTests'
		'-'
		'Go To Defining Package//gotoClassPackage'
	).
!

cleanupComparisonBrowser
	comparisonPresenter
		beforeTitle: String new;
		afterTitle: String new;
		compare: String new with: String new!

clearFilterLists
	categoryListPresenter list: ListModel new.
	variableListPresenter list: ListModel new!

cloneProject
	super cloneProject.
	self refreshFromServer!

comparisonTab
	^comparisonTab
		ifNil: 
			[comparisonTab := self textTabs
						detect: [:each | (each class canUnderstand: #text) ifTrue: [each text = 'Comparison'] ifFalse: [each = 'Comparison']]]!

confirmCategoryRemoval: selections
	| ws verbiage |
	ws := WriteStream on: String new.
	verbiage := categoryListPresenter selections size = 1 ifTrue: ['category'] ifFalse: ['categories'].
	ws
		nextPutAll: 'Really delete method ';
		nextPutAll: verbiage;
		space.
	selections do: 
			[:category |
			ws
				nextPutAll: category;
				space].
	ws
		nextPutAll: 'and ';
		nextPutAll: methodListPresenter list size printString;
		nextPutAll: ' methods?'.
	^(MessageBox confirm: ws contents) == true!

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

createSchematicWiringForClassHierarchy
	classHierarchyPresenter
		when: #'drag:'				send: #'onDragClassHierarchy:'		to: self;
		when: #'dragCut:'			send: #'onDragCutClassHierarchy:'	to: self;
		when: #'dragOver:'			send: #'onDragOverClassHierarchy:'	to: self;
		when: #'drop:'				send: #'onDropOnClassHierarchy:'	to: self;
		when: #selectionChanging:		send: #aboutToChange:			to: self;
		when: #'selectionChanging:'	send: #'releasedReferencedServices:'	to: self;
		when: #selectionChanged		send: #refreshClass				to: self!

createSchematicWiringForClassHierarchyTabs
	classHierarchyTabs
		when: #currentCardChanging:
			send: #aboutToChange:
			to: self;
		when: #currentCardChanged
			send: #refreshClassListOrHierarchy
			to: self!

createSchematicWiringForClassList
	classListPresenter
		when: #drag: 			send: #onDragClassList: 			to: self;
		when: #dragCut: 		send: #onDragCutClassList: 		to: self;
		when: #dragOver: 		send: #onDragOverClassList: 		to: self;
		when: #drop: 			send: #onDropOnClassList: 		to: self;
		when: #keyTyped: 		send: #delayUpdate 				to: self;
		when: #selectionChanging: send: #aboutToChange: 			to: self;
		when: #selectionChanging: send: #releasedReferencedServices: 	to: self;
		when: #selectionChanged 	send: #selectedClassChanged: 		to: self with: true;
		when: #selectionChanged 	send: #refreshClass 				to: self;
		when: #labelOf:changedTo: send: #labelOfClass:changedTo: 	to: self;
		when: #aboutToEditLabel:accept: send: #aboutToEditClassLabel:accept: 	to: self;
		when: #labelOf:editedTo:accept: send: #labelOfClass:editedTo:accept: 	to: self;
		when: #keyTyped: 		send: #selectServiceStartingWith: 			to: classListPresenter!

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
			send: #selectMethodSourceTab
			to: self;
		when: #selectionChanged
			send: #refreshMethodsOn:
			to: self
			with: methodListPresenter!

createSchematicWiringForMethodFilterTabs
	methodFilterTabs
		when: #currentCardChanged
			send: #filterTabChanged
			to: self;
		when: #currentCardChanged
			send: #refreshMethodsOn:
			to: self
			with: methodListPresenter!

createSchematicWiringForMethodList
	methodListPresenter
		when: #drag: 			send: #onDragMethod: 			to: self; 
		when: #dragCut: 		send: #onDragCutMethod: 			to: self; 
		when: #keyTyped: 		send: #delayUpdate 				to: self; 
		when: #selectionChanging: send: #aboutToChange:			to: self; 
		when: #keyTyped: 		send: #selectServiceStartingWith: 	to: methodListPresenter; 
		when: #selectionChanged 	send: #updateMethodSource 		to: self!

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
		when: #'selectionChanging:'	send: #'releasedReferencedServices:'	to: self;
		when: #'selectionChanged'		send: #'updatePackage' 			to: self;
		when: #keyTyped: 			send: #selectServiceStartingWith: 	to: packageListPresenter
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
		when: #'selectionChanging:'	send: #'releasedReferencedServices:'	to: self;
		when: #'selectionChanged' 		send: #'ensureVisible'				to: projectListPresenter;
		when: #'selectionChanged'		send: #'updateProject' 			to: self
!

createSchematicWiringForVariableList
	variableListPresenter
		when: #selectionChanged send: #selectMethodSourceTab to: self;
		when: #selectionChanged send: #refreshMethodsOn: to: self with: methodListPresenter!

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
	[self issueCommand: (Array with: packageService)] on: Error
		do: 
			[:ex | 
			classListPresenter list: visibleClasses.
			(ex isKindOf: GsCompileError) ifTrue:[
				self handleClassCompileError: ex]
			ifFalse:[
				self statusBarText: 'ERROR: ', ex messageText ]].
	classDefinitionPresenter view isModified: false!

editSaveClassComment
	| classService |
	classService := classListPresenter selection.
	classService
		command: #classComment:;
		commandArgs: (Array with: classCommentPresenter value trimBlanks).
	self issueCommand: (Array with: classService).
	classCommentPresenter ensureVisible. !

editSaveMethod
	self editSaveMethod: classListPresenter selection.
	self redrawAllViews!

editSaveMethod: classService
	self
		editSaveMethod: classService
		in: methodSourcePresenter
		category: self targetCategory.
	classService selectedMethods
		ifNotNil: 
			["there will be no method selected on a compiler error"
			self selectMethodsFrom: classService]!

filterTabChanged
	self isClassSelected
		ifFalse: 
			[self clearFilterLists.
			^self].
	self populateFilterList.
	self isCategoriesTabSelected
		ifTrue: 
			[^methodListPresenter selectionOrNil
				ifNil: [self selectMethodSourceTab]
				ifNotNil: 
					[:methodService |
					self selectCategoryFrom: methodService.
					self selectMethodSourceTab]].
	methodListPresenter selectionOrNil ifNotNil: [self selectMethodSourceTab]!

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
	MessageBox notify: 'Project ' , projectName , ' committed to local repository!!'.
	self refreshFromServer. !

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

hasCategorySelected

	^categoryListPresenter selections notEmpty!

hasExactlyOneCategorySelected

	^categoryListPresenter selections size = 1!

initialize

	super initialize.
	stonCritical := Semaphore forMutualExclusion.!

isClassSelected

	^classListPresenter selectionOrNil isNil not
!

isDictionariesTabSelected

	^projectDictionaryTabs currentCard name = 'dictionaryList'.
!

isOkayToChange
	
	methodSourcePresenter view isModified ifTrue: [
		(self isOkayToChangeSource: methodSourcePresenter) ifFalse:[^false]
	].
	classDefinitionPresenter view isModified ifTrue: [
		classDefinitionPresenter ensureVisible.
		gciSession isValidSession
			ifTrue: [ (MessageBox confirm: 'Stay on unsaved class definition?' caption: 'Class has unsaved changes!!') ifTrue: [^false]]
			ifFalse: [(MessageBox confirm: 'Copy changes to Clipboard?' caption: 'Class has unsaved changes!!') ifTrue: [classDefinitionPresenter selectAll; copySelection]].
		classDefinitionPresenter view isModified: false.
	].
	self postOkToChangeEvent. 
	^true.
!

issueCommand: services
	inUpdate ifTrue: [^self].
	inUpdate := true.
	[^super issueCommand: services] ensure: [inUpdate := false]!

jadeMenuStrings

	false ifTrue: [
		self abortTransaction; commitTransaction; newWorkspace; openSystemBrowser; refreshFromServer; exit.
	]. 
	^#(
		'&Jade'
		'&Abort Transaction//abortTransaction'
		'&CommitTransaction//commitTransaction'
		'-'
		'&New Workspace/Ctrl+N/newWorkspace'
		'&Open System Browser/Ctrl+B/openSystemBrowser'
		'-'
		'&Refresh/Ctrl+R/refreshFromServer'
		'-'
		'E&xit/Shift+F4/exit'
	).
!

loadProjects
	self loadProjectsFrom: self. 
	self refreshFromServer!

makeDefault
	"the server is the gold copy"
	| projectService |
	projectService := projectListPresenter selection.
	projectService command: #setDefaultProject.
	self issueCommand: (Array with: projectService).
	RowanProjectService defaultProjectName: projectService name.
	projectListPresenter view invalidate. !

methodListPresenter
	^methodListPresenter!

methodListSelection

	^methodListPresenter selectionOrNil!

methodMenuBreak: aMenu
	| selection |
	methodSourcePresenter view isModified ifTrue: [^self].
	selection := methodListPresenter selectionOrNil.
	selection ifNil: [^self].
	selection appendToSourceMenu: aMenu presenter: methodSourcePresenter!

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
	categoryListPresenter selections: (categoryListPresenter selections copyWith: targetCategory).
	classService
		command: #moveMethods:to:;
		commandArgs: (Array with: methodServices with: targetCategory).
	self issueCommand: (Array with: classService).
	self selectMethodsFrom: classService!

onSetFocus: aView stack: aString

	"no need for constant trips to the server"!

onViewOpened
	"copied to RowanDebugger"
	super onViewOpened.
	self registerPresentersForUpdates. 
	projectListPresenter view contextMenu: (self projectMenu).
	packageListPresenter view contextMenu: (Menu fromStrings: self pkgsMenuStrings).
	self updateMenuBar.
	self updateProjects. !

openDictionaryBrowserOn: classService
	| shell |
	Cursor wait showWhile: 
			[shell := RowanSystemBrowser showOnSession: gciSession.
			shell selectClass: classService name asString selector: String new]!

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

populateFilterList
	self isCategoriesTabSelected
		ifTrue: [categoryListPresenter list: classListPresenter selection categories]
		ifFalse: [variableListPresenter list: classListPresenter selection variables]!

postOkToChangeEvent
	| service |
	service := self isClassListTabSelected
				ifTrue: [classListPresenter selectionIfNone: [^self]]
				ifFalse: [classHierarchyPresenter selectionIfNone: [^self]].
	classDefinitionPresenter value: service template.
	classDefinitionPresenter view
		backcolor: Color white;
		isModified: false.
	self sundryCleanup!

projectChanges
	projectListPresenter selection changesUsing: self!

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
	projectListPresenter selection projectLogUsing: self!

projectMenu
	^Menu fromStrings: self projectMenuStrings!

projectMenuCommands
	^self projectMenu items collect: [:item | item command]!

projectMenuStrings
	self rowanFixMe.	"this really is a cumbersome way to build a menu."
	0 == 1
		ifTrue: 
			[self
				findClass;
				makeDefault;
				cloneProject;
				loadProjects;
				unloadProject;
				pullFromGit;
				gitCommit;
				pushToGit;
				projectChanges;
				writeProject;
				checkout;
				projectLog;
				refresh].
	^#('Projects' '&Find Class.../Ctrl+Shift+F/findClass' '&Make Default//makeDefault' '-' 'Clone Git Project ...//cloneProject' 'Load//loadProjects' 'Unload//unloadProject' '-' 'Pull from Git ...//pullFromGit' 'Commit ...//gitCommit' 'Push to Git ...//pushToGit' '-' '&Changes ...//projectChanges' 'Write ...//writeProject' 'Checkout ...//checkout' '-' 'Log//projectLog' '&Refresh//refreshFromServer')!

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
	| projectServices ws |
	projectServices := projectListPresenter selections.
	ws := WriteStream on: String new.
	projectServices do: 
			[:service |
			ws
				nextPutAll: service name;
				space].
	(MessageBox
		confirm: 'It is recommended that you commit changes before doing a pulling projects - ' , ws contents
				, ' -  from git. Really proceed?')
			ifFalse: [^self].
	projectServices do: [:service | service command: #pullFromGit].
	self issueCommand: projectServices!

pushToGit
	| projectServices ws |
	projectServices := projectListPresenter selections.
	ws := WriteStream on: String new.
	projectServices do: 
			[:service |
			ws
				nextPutAll: service name;
				space].
	(MessageBox
		confirm: 'Push projects - ' , ws contents , '- from the local repository to the remote server?')
			ifFalse: [^self].
	projectServices do: [:service | service command: #pushToGit].
	self issueCommand: projectServices!

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
	command == #renameMethodCategory ifTrue: [aCommandQuery isEnabled: self hasExactlyOneCategorySelected.^true].
	command == #removeMethodCategories ifTrue:  [aCommandQuery isEnabled: self hasCategorySelected .^true].

	command == #cloneProject ifTrue:[aCommandQuery isEnabled: true. ^true].
	command == #refreshFromServer ifTrue:[aCommandQuery isEnabled: true. ^true].
	command == #findClass ifTrue:[aCommandQuery isEnabled: true. ^true].
	
	(command == #setEnvironment0 and: [environment == 0]) ifTrue: [aCommandQuery isChecked: true.^true].
	(command == #setEnvironment1 and: [environment == 1]) ifTrue: [aCommandQuery isChecked: true.^true].
	(command == #setEnvironment2 and: [environment == 2]) ifTrue: [aCommandQuery isChecked: true.^true].

	(self projectMenuCommands includes: command) ifTrue:  [aCommandQuery isEnabled: projectListPresenter selections notEmpty. ^true].
	(#(#browseHierarchyImplementors #browseHierarchySenders) includes: command) ifTrue: [aCommandQuery isEnabled: methodListPresenter selections notEmpty. ^true].

	(self queryCommandHandlers: aCommandQuery) ifTrue: [^true].
	(aCommandQuery canPerformAgainst: self) ifTrue: [aCommandQuery beEnabled].
	^false	"There was no specific enablement of the command at this level"!

redrawAllViews
	"for now, just update a few views. Not sure if there's a better way to do this."
	projectListPresenter view invalidate.
	packageListPresenter view invalidate.
	classListPresenter view invalidate.
	projectInfoTab view invalidate!

refreshClass
	| selection presenter |
	inUpdate ifTrue:[^self].
	self setClassMeta.
	self setMethodFilter.
	presenter := self isClassListTabSelected
				ifTrue: [classListPresenter]
				ifFalse: [classHierarchyPresenter].
	selection := presenter selectionIfNone: 
					[categoryListPresenter clear.
					^methodListPresenter clear].
	selection selectedPackageServices: packageListPresenter selections.
	self setFilterType: selection.
	self updateServices: (Array with: selection).
	self updateClassInfo!

refreshClassListOrHierarchy
	| newSelection listSelection hierarchySelection |
	self isClassListTabSelected
		ifTrue: 
			[listSelection := classListPresenter selectionOrNil.
			hierarchySelection := classHierarchyPresenter selectionOrNil.
			listSelection = hierarchySelection ifTrue: [^self].
			(hierarchySelection isNil or: [listSelection isNil]) ifTrue: [^self updateClasses].
			self updateClasses.  
			newSelection := classListPresenter list detect: [:service | hierarchySelection name = service name]
						ifNone: [^classListPresenter resetSelection].
			classListPresenter selection: newSelection]
		ifFalse: [self updateHierarchy.]!

refreshFromServer
	| services |
	services := OrderedCollection with: RowanBrowserService new.
	projectListPresenter selections isEmpty ifFalse: [services addAll: projectListPresenter selections].
	packageListPresenter selections isEmpty ifFalse: [services addAll: packageListPresenter selections].
	classListPresenter selectionOrNil ifNotNil: [:classService | services add: classService].
	self updateServices: services.
	self redrawAllViews!

refreshMethodsOn: presenter
	| classService |
	classService := classListPresenter selectionOrNil ifNil:[^self].
	self isCategoriesTabSelected
		ifTrue: [classService filters: categoryListPresenter selections]
		ifFalse: [classService filters: variableListPresenter selections].
	classService filters isEmpty
		ifTrue: [classService filterType: nil]
		ifFalse: 
			[self isCategoriesTabSelected
				ifTrue: [classService filterType: 'categories']
				ifFalse: [classService filterType: 'variables']].
	classService methods isNil ifTrue:[ "if we got an error recently, could be nil"
		self updateServices: (Array with: classService)]. 
	classService displayMethodsOn: presenter!

registerClassData
	"temporarily disabled"!

registerPresentersForUpdates
	"copied to RowanDebugger"

	BrowserUpdate current register: projectListPresenter selector: #projectsUpdate:.
	BrowserUpdate current
		register: packageListPresenter
		selector: #projectPackagesUpdate:browser:
		browser: self.
	BrowserUpdate current register: classListPresenter selector: #removedClass:.
	BrowserUpdate current
		register: classListPresenter
		selector: #classesUpdate:browser:
		browser: self.
	BrowserUpdate current
		register: categoryListPresenter
		selector: #filterUpdate:browser:
		browser: self.
	BrowserUpdate current
		register: classHierarchyPresenter
		selector: #classHierarchyUpdate:browser:
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
	BrowserUpdate current register: methodSourcePresenter selector: #methodSourceUpdate:browser: browser: self. !

releasedReferencedServices: selectionChangingEvent
	selectionChangingEvent window presenter selectionOrNil
		ifNotNil: [:service | service releaseReferencedServices]!

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
	| selections |
	selections := categoryListPresenter selections.
	selections isEmpty ifTrue: [^MessageBox notify: 'No categories selected'].
	(self confirmCategoryRemoval: selections) ifFalse: [^self].
	self basicRemoveMethodCategories!

removeMethods: services
	services do: 
			[:classService |
			classService command: #removeMethods.
			classService methods: methodListPresenter selections].
	self issueCommand: services!

renameMethodCategory
	| new service old |
	old := categoryListPresenter selection.
	(new := Prompter prompt: 'New method category?') isNil ifTrue: [^self].
	(categoryListPresenter list includes: new)
		ifTrue: [^MessageBox notify: 'category ' , new , ' already exists'].
	service := classListPresenter selection
				command: #renameCategoryFrom:to:;
				commandArgs: (Array with: old with: new).
	self issueCommand: (Array with: service).
	categoryListPresenter selection: new!

savePackage
	MessageBox notify: 'This feature is not yet available'!

selectCategoryFrom: methodService
	^categoryListPresenter selectionOrNil = methodService category
		ifFalse: [categoryListPresenter selection: methodService category]!

selectClass: classSelection
	| newClassSelection |
	newClassSelection := classListPresenter list detect: [:svc | svc name = classSelection name]
				ifNone: [].
	^newClassSelection ifNotNil: [classListPresenter selection: newClassSelection]!

selectedMethodCategory
	^methodListPresenter selectionOrNil ifNotNil: [:methodService | methodService category]!

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

selectMethodSourceTab
	| methodSourceTab selection |
	methodSourceTab := self view viewNamed: 'methodSource'.
	methodListPresenter selectionOrNil ifNotNil: [^methodSourceTab ensureVisible].
	selection := self isCategoriesTabSelected
				ifTrue: [categoryListPresenter selectionOrNil]
				ifFalse: [variableListPresenter selectionOrNil].
	selection
		ifNotNil: 
			[methodSourceTab ensureVisible.
			methodListPresenter selectionOrNil ifNil: [methodSourcePresenter setDefaultMethod]]!

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

sundryCleanup
	self statusBarText: String new.
	self cleanupComparisonBrowser!

targetCategory
	self isVariablesTabSelected ifTrue: [^self selectedMethodCategory].
	categoryListPresenter hasSelection ifFalse: [^self selectedMethodCategory].
	categoryListPresenter selections size > 1 ifTrue: [^self selectedMethodCategory].
	^categoryListPresenter selection trimBlanks!

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
	self updateServices: packageListPresenter selections.!

updateClassInfo
	| service |
	service := self isClassListTabSelected
				ifTrue: [classListPresenter selectionIfNone: [^self]]
				ifFalse: [classHierarchyPresenter selectionIfNone: [^self]].
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
	self sundryCleanup!

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

updateHierarchy
	| service |
	service := classListPresenter selectionOrNil
				ifNil: [^classHierarchyPresenter model: TreeModel new]
				ifNotNil: [:classService | classService command: #classHierarchy].
	self issueCommand: (Array with: service)!

updateMenuBar

	| shellView menuBar methodFilterMenu textAreaMenu |
	methodFilterMenu := Menu fromStrings: (self isCategoriesTabSelected ifTrue: [self categoriesMenuStrings] ifFalse: [self variablesMenuStrings]).
	textAreaMenu := Menu fromStrings: (self isGlobalsTabSelected ifTrue: [self globalsMenuStrings] ifFalse: [self editMenuStrings]).
	shellView := self parentPresenter parentPresenter view.
	menuBar := shellView menuBar
		clear;
		addItem: (Menu fromStrings: self jadeMenuStrings);
		addItem: self projectMenu;
		addItem: (Menu fromStrings: self pkgsMenuStrings);
		addItem: (Menu fromStrings: self classesMenuStrings);
		addItem: methodFilterMenu;
		addItem: (Menu fromStrings: self methodsMenuStrings);
		addItem: textAreaMenu;
		addItem: (Menu fromStrings: self preferencesMenuStrings);
		yourself.
	shellView menuBar: menuBar.
!

updateMethodReferences: services
	services do: [:service | service command: #allReferences].
	self updateServices: services.
	^services!

updateMethodSource
	self sundryCleanup.
	methodListPresenter selections isEmpty ifTrue: [^methodSourcePresenter setDefaultMethod].
	self updateServices: methodListPresenter selections. 
	methodListPresenter selections size > 1
		ifTrue: [self updateMultiMethodComparison]
		ifFalse: 
			[self showSuperClassComparison.
			self updateSingleMethodSource].!

updateMultiMethodComparison
	comparisonPresenter
		beforeTitle: (methodListPresenter selections at: 1) classMethodDisplayString;
		afterTitle: (methodListPresenter selections at: 2) classMethodDisplayString;
		compare: (methodListPresenter selections at: 1) source
			with: (methodListPresenter selections at: 2) source.!

updatePackage
	self isClassListTabSelected
		ifTrue: 
			[classListPresenter model: ListModel new.
			self updateClasses]
		ifFalse: [classHierarchyPresenter model: TreeModel new].
	self updatePackageInfo!

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
	self updateServices: selections.
	self sundryCleanup!

updateProject
	self updatePackages.
	self updateProjectInfo.
	self sundryCleanup!

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
		command: #updateProjects.
	self issueCommand: (Array with: browserService).!

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

updateService: service

	^self updateServices: (Array with: service) !

updateServices: services
	inUpdate ifTrue: [^self].
	inUpdate := true.
	[self basicUpdateServices: services] ensure: [inUpdate := false]!

updateSingleMethodSource
	methodSourcePresenter value: methodListPresenter selection source.
	self refreshBreakPointsIn: methodSourcePresenter.
	methodSourcePresenter ensureVisible. 
	self displayNoMods: methodSourcePresenter. !

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
	| projectServices ws |
	projectServices := projectListPresenter selections.
	ws := WriteStream on: String new.
	projectServices do: 
			[:service |
			ws
				nextPutAll: service name;
				space].
	(MessageBox confirm: 'Write projects - ' , ws contents , '- to disk without committing?')
		ifFalse: [^false].
	projectServices do: [:service | service command: #write].
	self issueCommand: projectServices.
	MessageBox notify: 'Projects ' ,  ws contents  , ' written to disk'.
	self refreshFromServer. ! !
!JadeiteProjectsBrowserPresenter categoriesFor: #addPackage!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #addPackageDictionaryInfoTo:!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #addSubclass!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #allClasses!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #basicAddMethodCategory:!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #basicRemoveMethods!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseClassReferences!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseHierarchyImplementors!menu handlers!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseHierarchyImplementorsOf:!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseHierarchySenders!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseHierarchySendersOf:!public!queries! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseMethodHistory!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #browseSelectedClass!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #categoriesMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #categoryListPresenter!accessing!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #checkout!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #classesMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #cleanupComparisonBrowser!initialize/release!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #clearFilterLists!event handlers support!private!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #cloneProject!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #comparisonTab!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #confirmCategoryRemoval:!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createComponents!initialize/release!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiring!create schemantic wiring!overrides!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForClassHierarchy!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForClassHierarchyTabs!create schemantic wiring!overrides!public! !
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
!JadeiteProjectsBrowserPresenter categoriesFor: #editSaveMethod:!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #filterTabChanged!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #findClass!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #getViews!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #gitCommit!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #gotoClassPackage!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #gotoMethodPackage!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #hasCategorySelected!public!testing! !
!JadeiteProjectsBrowserPresenter categoriesFor: #hasExactlyOneCategorySelected!public!testing! !
!JadeiteProjectsBrowserPresenter categoriesFor: #initialize!initialize/release!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #isClassSelected!public!testing! !
!JadeiteProjectsBrowserPresenter categoriesFor: #isDictionariesTabSelected!public!testing! !
!JadeiteProjectsBrowserPresenter categoriesFor: #isOkayToChange!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #issueCommand:!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #jadeMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #loadProjects!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #makeDefault!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #methodListPresenter!accessing!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #methodListSelection!accessing!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #methodMenuBreak:!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #methodsMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #newService!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onDragMethod:!drag & drop!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onDropOnClassList:!drag & drop!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onDropOnMethodCategory:!drag & drop!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onSetFocus:stack:!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onViewOpened!event handlers!overrides!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #openDictionaryBrowserOn:!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #pkgsMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #populateFilterList!event handlers support!private!services!updating! !
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
!JadeiteProjectsBrowserPresenter categoriesFor: #queryCommand:!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #redrawAllViews!displaying!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #refreshClass!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #refreshClassListOrHierarchy!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #refreshFromServer!event handlers!menu handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #refreshMethodsOn:!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #registerClassData!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #registerPresentersForUpdates!event handlers!initialize/release!public!register presenters! !
!JadeiteProjectsBrowserPresenter categoriesFor: #releasedReferencedServices:!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #removeClass!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #removeMethodCategories!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #removeMethods:!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #renameMethodCategory!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #savePackage!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectCategoryFrom:!public!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectClass:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectedMethodCategory!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectedMethodName!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectionMethod:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectMethodsFrom:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectMethodSourceTab!public!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectPackage:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setClassMeta!event handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setDefaultProjectTablLabel!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setDefaultProjectTablLabel:!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setFilterType:!event handlers support!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setMethodFilter!event handlers support!private!services!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #showSuperClassComparison!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #sundryCleanup!initialize/release!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #targetCategory!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #textTabChanged!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #textTabs!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #unloadPackage!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #unloadProject!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateAfterFindClass:!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateAndSelectA:!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateClasses!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateClassInfo!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateCommand:onSuccessDo:!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateHierarchy!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateMenuBar!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateMethodReferences:!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateMethodSource!event handlers!public!text tabs!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateMultiMethodComparison!event handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updatePackage!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updatePackageInfo!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updatePackages!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateProject!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateProjectInfo!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateProjects!event handlers!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateProjectTab!event handlers support!private!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateSelectionsForUpdate:!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateService:!public!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateServices:!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateSingleMethodSource!event handlers support!private! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 656 nil nil nil 5 nil nil nil 656 482 170 176 34 6 410 ##(Smalltalk.ListView) 34 30 nil 656 34 2 8 1409405257 1025 768 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 768 nil 8 4294905089 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 361 #left 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 1152 8 'doIt' 8 '[:each | each displayName]' 8 #[30 105 226 0 106] #displayName 1168 7 257 nil ##(Smalltalk.SortedCollection) nil nil 768 nil 3 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 1264 7 257 nil 1154 0 nil 1186 3 1 1152 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 1344 7 257 nil #report 880 nil 133217 nil 34 4 nil nil 328198 ##(Smalltalk.Point) 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 1442 1 1 1442 369 695 768 1538 #text: 34 1 8 'Column 1' 768 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 1442 193 193 nil 27 524806 ##(Smalltalk.Fraction) 9 11 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 1776 nil nil nil 5 nil nil nil 1776 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 1776 34 2 8 1409286144 131073 1904 nil 930 8 4278190080 nil 5 nil nil nil 1904 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Classes' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 2096 2128 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 2144 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 1904 34 2 8 1409372237 1025 2240 834 138 144 880 nil 912 930 960 nil 29 nil nil nil 2240 nil 8 4294905089 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 '' 559 #left 994 #displayString 8 #() 994 #<= 8 #() nil nil 2240 nil 3 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 2528 7 257 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 2608 7 257 nil #report 880 nil 133217 nil 34 4 nil nil 1442 1 1 nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 9 55 1442 567 589 2240 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 1728 nil 27 2066 2098 8 'Hierarchy' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 2096 2912 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 2928 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 1904 34 2 8 1409352231 1025 3024 590918 3 ##(Smalltalk.TreeModel) nil 912 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 880 930 8 4278190080 nil 29 nil nil nil 3024 nil 8 4294905699 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each displayString]' 8 #[30 105 226 0 106] #displayString 3216 7 257 nil ##(Smalltalk.IconicListAbstract) 1056 nil nil nil nil nil 170 176 880 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 880 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 9 55 1442 567 589 3024 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 1728 nil 27 2240 170 192 34 4 2240 8 'classList' 3024 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 1904 34 2 8 1140916736 1 3552 834 138 144 34 2 2112 2896 nil 912 nil nil 1 nil nil nil 3552 nil 8 4294903035 1154 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 3680 7 257 nil 1154 0 nil 3698 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 3744 7 257 nil 1056 nil nil nil nil nil #smallIcons nil nil nil nil nil 1474 138 144 34 3 1538 #createAt:extent: 34 2 1442 1 1 1442 583 651 3552 1538 #basicSelectionsByIndex: 34 1 8 #(1) 3552 1538 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 3552 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 8 #() 1728 nil 27 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 1 1442 583 651 1904 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 34 3 2240 3024 3552 1728 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 4226 #fixedParentRight 1 4226 #fixedParentTop 1 4226 #fixedParentBottom -43 410 ##(Smalltalk.CardContainer) 34 16 nil 1776 34 2 8 1409286144 131073 4304 nil 930 8 4278190080 nil 5 nil nil nil 4304 2002 138 144 34 2 2066 2098 8 'Instance' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 2096 4480 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 4496 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 4304 34 2 8 1140850688 131073 4592 nil nil nil 5 nil nil nil 4592 nil 170 192 880 nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 9 9 1442 567 1 4592 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 1728 nil 27 2066 2098 8 'Class' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 2096 4864 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 4880 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 4304 34 2 8 1140850688 131073 4976 nil nil nil 5 nil nil nil 4976 nil 170 192 880 nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 9 9 1442 567 1 4976 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 1728 nil 27 4592 170 192 34 4 4592 8 'instanceTab' 4976 8 'classTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 4304 34 2 8 1140916738 1 5280 834 138 144 34 2 4464 4848 nil 912 nil nil 1 nil nil nil 5280 nil 8 4294903035 1154 0 nil 3698 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 5408 7 257 nil 1154 0 nil 3698 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 5456 7 257 nil 1056 nil nil nil nil nil #smallIcons nil nil nil nil nil 1474 138 144 34 3 1538 #createAt:extent: 34 2 1442 1 1 1442 583 51 5280 1538 #basicSelectionsByIndex: 34 1 8 #(1) 5280 1538 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 5280 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 25 0 0 0] 8 #() 1728 nil 27 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 645 1442 583 51 4304 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 66 1 0 0 35 1 0 0 91 1 0 0] 34 3 4592 4976 5280 1728 nil 27 4194 4240 1 4256 1 4288 -49 4226 #fixedViewTop 51 170 192 34 4 1904 8 'classHierarchyTabs' 4304 8 'instanceClassTabs' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 757 1 1442 583 695 1776 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 122 1 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 2 1904 4304 1728 nil 27 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 244 63 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 6192 nil nil nil 5 nil nil nil 6192 482 170 176 880 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 6192 34 2 8 1140936777 1025 6320 834 138 144 880 nil 912 930 8 4278190080 nil 21 nil nil nil 6320 nil 8 4294905089 994 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 'Column 1' 361 #left 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 6560 7 257 nil 994 #<= 8 #() nil nil 6320 nil 3 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 6672 7 257 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 6752 7 257 nil #report 880 nil 133217 nil 34 4 nil nil 1442 1 1 nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 1 1 1442 369 695 6320 1538 #text: 34 1 8 'Column 1' 6320 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 1728 nil 27 8 'packagesList' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 379 1 1442 369 695 6192 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 189 0 0 0 0 0 0 0 117 1 0 0 91 1 0 0] 34 1 6320 1728 nil 27 1746 9 11 false 170 192 34 2 768 8 'projectList' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 1 1442 1339 695 656 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 5 768 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 7472 nil 930 8 4278190080 nil 517 nil nil nil 7472 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 369 1 1442 11 695 7472 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 184 0 0 0 0 0 0 0 189 0 0 0 91 1 0 0] 8 #() 1728 nil 27 6192 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 7728 nil 930 7552 nil 517 nil nil nil 7728 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 747 1 1442 11 695 7728 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 117 1 0 0 0 0 0 0 122 1 0 0 91 1 0 0] 8 #() 1728 nil 27 1776 1728 nil 27 7 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 7968 nil nil nil 5 nil nil nil 7968 482 170 176 880 false 170 192 34 2 410 ##(Smalltalk.CardContainer) 34 16 nil 7968 34 2 8 1409286144 131073 8096 nil 930 8 4278190080 nil 5 nil nil nil 8096 2002 138 144 34 2 2066 8 'Category' 410 ##(Smalltalk.ListView) 34 30 nil 8096 34 2 8 1409372233 1025 8272 834 138 144 880 nil 912 930 8 4278190080 nil 21 nil nil nil 8272 nil 8 4294905089 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 'Column 1' 417 #left 994 #displayString 8 #() ##(Smalltalk.SortedCollection) 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each]' 1779525917 8544 7 257 nil nil 8272 nil 3 nil nil #report 880 nil 131169 nil 34 4 nil nil 1442 1 1 nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 9 55 1442 425 633 8272 1538 #text: 34 1 8 'Column 1' 8272 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 216 0 0 0 87 1 0 0] 8 #() 1728 nil 27 2066 8 'Variables' 410 ##(Smalltalk.ListView) 34 30 nil 8096 34 2 8 1409372233 1025 8880 834 138 144 880 nil 912 930 8384 nil 21 nil nil nil 8880 nil 8 4294905089 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 'Column 1' 417 #left 994 #displayString 9024 ##(Smalltalk.SortedCollection) nil nil 8880 nil 3 nil 1154 0 nil 1186 9 1 1152 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 9120 7 257 nil #report 880 nil 131169 nil 34 4 nil nil 1442 1 1 nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 9 55 1442 425 633 8880 1538 #text: 34 1 8 'Column 1' 8880 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 216 0 0 0 87 1 0 0] 8 #() 1728 nil 27 8272 170 192 34 4 8880 8 'variableList' 8272 8 'categoryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 8096 34 2 8 1140916736 1 9504 834 138 144 34 2 8256 8864 nil 912 nil nil 1 nil nil nil 9504 nil 8 4294903035 1154 0 nil 3698 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 9632 7 257 nil 1154 0 nil 3698 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 9680 7 257 nil 1056 nil nil nil nil nil #noIcons nil nil nil nil nil 1474 138 144 34 3 1538 #createAt:extent: 34 2 1442 1 1 1442 441 695 9504 1538 #basicSelectionsByIndex: 34 1 8 #(1) 9504 1538 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 9504 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 8 #() 1728 nil 27 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 1 1442 441 695 8096 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 34 3 8272 8880 9504 1728 nil 27 8 'methodFilterTabs' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1349 1 1442 893 695 7968 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 162 2 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 8096 410 ##(Smalltalk.Splitter) 34 12 nil 7968 34 2 8 1140850688 1 10304 nil 930 7552 nil 517 nil nil nil 10304 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 441 1 1442 11 695 10304 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 220 0 0 0 0 0 0 0 225 0 0 0 91 1 0 0] 8 #() 1728 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 7968 34 2 8 1140850688 131073 10544 nil nil nil 5 nil nil nil 10544 1842 170 176 34 6 410 ##(Smalltalk.ListView) 34 30 nil 10544 34 2 8 1409372233 1025 10656 834 138 144 880 nil 912 930 8384 nil 13 nil nil nil 10656 nil 8 4294905089 994 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1056 nil nil nil nil nil nil 138 144 34 2 1106 8 '' 41 #left 1538 #empty 8 #() 16 994 #<= 8 #() nil nil 10656 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each subSuperIcon]' 8 #[30 105 226 0 106] #subSuperIcon 10944 7 257 nil 1 nil nil 1106 8 'Selector' 395 #left 994 #displayString 10800 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 11072 7 257 nil nil 10656 nil 3 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 11152 7 257 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 11232 7 257 nil #report 880 nil 133221 nil 34 4 nil nil 1442 1 1 nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 47 1442 443 603 10656 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 221 0 0 0 68 1 0 0] 8 #() 1728 nil 27 4194 4240 1 4256 1 4272 47 4288 -45 410 ##(Smalltalk.ComboBox) 34 17 nil 10544 34 2 8 1412498947 1025 11520 834 138 144 880 nil 912 930 8 4278190080 nil 5 nil nil nil 11520 nil 8 4294906207 994 #displayString 8 #() 880 401 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 649 1442 443 53 11520 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 68 1 0 0 221 0 0 0 94 1 0 0] 8 #() 1728 nil 27 4194 4240 1 4256 1 4288 -45 5920 47 410 ##(Smalltalk.ComboBox) 34 17 nil 10544 34 2 8 1412498947 1025 11872 834 138 144 880 nil 912 930 11632 nil 5 nil nil nil 11872 nil 8 4294906207 994 #displayString 8 #() 880 401 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 1 1442 443 53 11872 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 221 0 0 0 26 0 0 0] 8 #() 1728 nil 27 4194 4240 1 4256 1 4272 1 5920 47 170 192 34 6 10656 8 'methodList' 11520 8 'overrideList' 11872 8 'superclassList' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 451 1 1442 443 695 10544 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 225 0 0 0 0 0 0 0 190 1 0 0 91 1 0 0] 34 3 11872 10656 11520 1728 nil 27 1728 nil 27 5 false 170 192 880 nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 1 1442 2241 695 544 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 656 410 ##(Smalltalk.Splitter) 34 12 nil 544 34 2 8 1140850688 1 12624 nil 930 7552 nil 517 nil nil nil 12624 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1339 1 1442 11 695 12624 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 157 2 0 0 0 0 0 0 162 2 0 0 91 1 0 0] 8 #() 1728 nil 27 7968 1728 nil 27 1746 3 5 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 12880 nil 930 1984 nil 5 nil nil nil 12880 2002 138 144 34 6 2066 2098 8 'SUnit' 1154 0 nil 1186 4 1 1152 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 13056 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 12880 34 2 8 1140850688 131073 13184 nil 930 8 4278190080 nil 5 nil nil nil 13184 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 9 55 1442 2225 635 13184 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 880 1728 nil 27 2066 2098 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 12880 34 2 8 1445007428 1025 13504 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 930 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 11 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #editSave 8 '&Save' 9383 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 13714 1 13746 #editUndo 8 '&Undo' 9397 1 nil nil nil 13714 1 13746 #editRedo 8 '&Redo' 9395 1 nil nil nil 13794 4097 13714 1 13746 #editCut 8 'Cu&t' 9393 1 nil nil nil 13714 1 13746 #editCopy 8 '&Copy' 9351 1 nil nil nil 13714 1 13746 #editPaste 8 '&Paste' 9389 1 nil nil nil 13714 1 13746 #editDelete 8 '&Delete' 1629 1 nil nil nil 13794 4097 13714 1 13746 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1442 193 193 nil 13504 nil 8 4294905147 852486 ##(Smalltalk.NullConverter) nil nil 13 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 14386 77 786694 ##(Smalltalk.IndexedColor) 33554457 14434 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 14400 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 14416 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 880 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 13504 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 14434 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 14400 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 13504 65025 3 false 1 nil nil nil nil nil nil 3 14850 3 13504 33423361 5 false 3 nil nil nil nil nil nil 5 14850 5 13504 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 14386 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 14386 1027 14962 60239 14962 31326207 1 nil nil nil nil #Warning nil nil nil #Error 14386 1031 14962 335 14962 31317503 1 nil nil nil nil #Error nil nil nil nil 1474 138 144 34 8 1538 #createAt:extent: 34 2 1442 9 55 1442 2225 635 13504 1538 #contextMenu: 34 1 13680 13504 1538 #modificationEventMask: 8 #(4607) 13504 1538 #caretPeriod: 8 #(530) 13504 1538 #wordWrap: 8 #(true) 13504 1538 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 13504 1 3 nil nil 5 nil 15378 3 13504 33 nil 3 67108863 5 nil 15378 5 13504 1 nil 3 -67108863 5 nil 13504 1538 #tabWidth: 8 #(4) 13504 1538 #maxCompletionListHeight: 8 #(9) 13504 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1728 nil 29 2066 2098 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 12880 34 2 8 1445007428 1025 15600 13570 nil false 13616 nil 930 13648 nil 5 13666 nil true 34 1 13714 1 13746 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 14210 nil true 14242 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1442 193 193 nil 15600 nil 8 4294905147 14322 nil nil 11 nil 170 192 34 4 #normal 14386 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 14386 77 14448 14464 1 nil nil nil nil #callTip nil nil nil 34 256 15920 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 15936 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 14578 1 nil nil nil 15600 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 14640 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 15920 nil nil nil nil 1 nil 170 192 34 6 1 14850 1 15600 65025 3 false 1 nil nil nil nil nil nil 3 14850 3 15600 33423361 5 false 3 nil nil nil nil nil nil 5 14850 5 15600 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 14386 1029 nil 14962 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 14386 1027 14962 60239 14962 31326207 1 nil nil nil nil #Warning nil nil nil #Error 14386 1031 14962 335 14962 31317503 1 nil nil nil nil #Error nil nil nil nil 1474 138 144 34 8 1538 #createAt:extent: 34 2 1442 9 55 1442 2225 635 15600 1538 #contextMenu: 34 1 15696 15600 1538 #modificationEventMask: 8 #(4607) 15600 1538 #caretPeriod: 8 #(530) 15600 1538 #wordWrap: 8 #(true) 15600 1538 #margins: 34 1 34 3 15378 1 15600 1 3 nil nil 5 nil 15378 3 15600 33 nil 3 67108863 5 nil 15378 5 15600 1 nil 3 -67108863 5 nil 15600 1538 #tabWidth: 8 #(4) 15600 1538 #maxCompletionListHeight: 8 #(9) 15600 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1728 nil 29 2066 2098 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 12880 34 2 8 1445007428 1025 16896 13570 nil false 13616 nil 930 8 4278190080 nil 5 nil 14210 nil true 14242 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1442 193 193 nil 16896 nil 8 4294905147 14322 nil nil 13 nil 170 192 34 4 #normal 14386 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 14386 77 14448 14464 1 nil nil nil nil #callTip nil nil nil 34 256 17136 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 17152 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 880 170 192 34 2 #default 14578 1 nil nil nil 16896 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 14640 nil nil 65 nil nil 14656 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 17136 #callTip 17152 nil nil nil nil 1 nil 170 192 34 12 1 14850 1 16896 65025 3 false 1 nil nil nil nil nil nil 3 14850 3 16896 33423361 5 false 3 nil nil nil nil nil nil 5 14850 5 16896 511 1 false 5 nil nil nil nil nil nil 8 'indicator10' 14850 21 16896 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator9' 14850 19 16896 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator8' 14850 17 16896 33554447 1 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 14386 1029 nil 14962 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 14386 1027 14962 60239 14962 31326207 1 nil nil nil nil #Warning nil nil nil #Error 14386 1031 14962 335 14962 31317503 1 nil nil nil nil #Error nil nil nil nil 1474 138 144 34 8 1538 #createAt:extent: 34 2 1442 9 55 1442 2225 635 16896 1538 #modificationEventMask: 8 #(4607) 16896 1538 #hoverTime: 8 #(200) 16896 1538 #caretPeriod: 8 #(530) 16896 1538 #wordWrap: 8 #(true) 16896 1538 #margins: 34 1 34 3 15378 1 16896 61 3 nil nil 5 nil 15378 3 16896 1 nil 3 67108863 nil nil 15378 5 16896 1 nil 3 -67108863 nil nil 16896 1538 #tabWidth: 8 #(4) 16896 1538 #maxCompletionListHeight: 8 #(9) 16896 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1728 nil 29 2066 2098 8 'Project' 1154 0 nil 1186 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 13138 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 18224 7 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 12880 34 2 8 1409372237 1025 18336 834 138 144 880 nil 912 930 8 4278190080 nil 5 nil nil nil 18336 nil 8 4294905089 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 2 1106 8 'Name' 201 #left 994 #displayString 18496 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each | (each at: 2)]' 8 #[29 105 17 64 148 106] 18592 7 257 nil nil 18336 nil 1 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each item at: 1) displayStringFor: each]' 8 #[31 105 226 0 63 148 17 177 106] #item #displayStringFor: 18672 7 257 nil 1106 8 'Value' 2017 #left 994 #displayString 8 #() 994 #<= 18800 1154 0 nil 1186 4 1 1152 8 'doIt' 8 '[:each | (each at: 2) ifNil:[String new] ifNotNil: [:iv |
(each at: 1) instVarNamed: iv]]' 8 #[32 105 17 64 148 100 223 4 97 45 146 106 90 17 63 148 18 178 106] 13138 #String 16 #ifNil:ifNotNil: #instVarNamed: 18832 7 65793 nil nil 18336 nil 3 nil nil #report 880 nil 131169 nil 34 4 nil nil 1442 1 1 nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 9 55 1442 2225 635 18336 1538 #text: 34 1 8 'Name' 18336 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1728 nil 27 2066 8 'Comparison' 410 ##(Smalltalk.ShellView) 34 27 nil 12880 34 2 8 1078919168 131073 19200 nil 930 8 4278190080 nil 517 nil nil nil 19200 788230 ##(Smalltalk.BorderLayout) 1 1 nil nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 19200 34 2 8 1140850688 131073 19328 nil 930 19280 nil 5 13666 nil true 34 5 13714 1 13746 #toggleIndentGuides 8 'Indentation &Guides' 1 1 nil nil nil 13714 1 13746 #toggleVisibleLineEndings 8 'Line &Endings' 1 1 nil nil nil 13714 1 13746 #toggleLineNumberMargins 8 'Line N&umbers' 1 1 nil nil nil 13714 1 13746 #toggleVisibleWhitespace 8 'W&hitespace' 1 1 nil nil nil 13714 1 13746 #toggleWrapping 8 '&Word Wrap' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 19328 482 170 176 880 false 170 192 880 nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 1 1 1442 2193 603 19328 1538 #contextMenu: 34 1 19408 19328 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 19328 34 2 8 1140850688 131073 19936 nil 930 19280 nil 5 nil nil nil 19936 19298 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 19936 34 2 8 1140850945 262145 20032 nil nil nil 5 nil nil nil 20032 nil 8 1695245616 14322 nil nil nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 1 563 1442 1091 41 20032 1538 #text: 34 1 8 '
' 20032 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 33 2 0 0 45 1 0 0] 8 #() 1728 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 19936 34 2 8 1140920580 1025 20336 13570 nil false 13616 nil 930 19280 nil 5 nil nil nil 20336 nil 8 4294905147 14322 nil nil 11 nil 170 192 34 12 #indentGuide 14386 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 14386 3 14434 33554445 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 14386 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 14386 77 14448 14464 1 nil nil nil nil #callTip nil nil nil #normal 14386 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 14386 5 nil 14962 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 20576 20512 20592 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20544 nil nil nil 20496 nil nil 1310982 ##(Smalltalk.DiffsScintillaStyler) 8 #() 170 192 34 2 #default 14578 1 nil nil nil 20336 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 14640 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 20464 nil nil nil nil 1 nil 170 192 34 6 8 'indicator1' 14850 3 20336 33423361 5 false nil nil nil nil nil nil nil 8 'indicator0' 14850 1 20336 33554459 13 false nil nil nil nil nil nil nil 8 'indicator2' 14850 5 20336 511 1 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 14386 1029 nil 14962 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 14386 1027 14962 60239 14962 31326207 1 nil nil nil nil #Warning nil nil nil #Error 14386 1031 14962 335 14962 31317503 1 nil nil nil nil #Error nil nil nil nil 1474 138 144 34 7 1538 #createAt:extent: 34 2 1442 1 1 1442 1091 563 20336 1538 #modificationEventMask: 8 #(4607) 20336 1538 #caretPeriod: 8 #(530) 20336 1538 #wordWrap: 8 #(true) 20336 1538 #margins: 34 1 34 3 15378 1 20336 49 3 nil nil nil nil 15378 3 20336 1 nil nil 67108863 nil nil 15378 5 20336 1 nil nil nil nil nil 20336 1538 #canHScroll: 8 #(false) 20336 1538 #maxCompletionListHeight: 8 #(9) 20336 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 25 1 0 0] 8 #() 1728 nil 29 170 192 34 4 20032 8 'beforeTitle' 20336 8 'before' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 1 1442 1091 603 19936 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 45 1 0 0] 34 2 20336 20032 1728 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 19328 34 2 8 1140850688 1 21744 nil 14962 66047 nil 517 nil nil nil 21744 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1091 1 1442 11 603 21744 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 33 2 0 0 0 0 0 0 38 2 0 0 45 1 0 0] 8 #() 1728 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 19328 34 2 8 1140850688 131073 21984 nil 930 19280 nil 5 nil nil nil 21984 19298 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 21984 34 2 8 1140850945 262145 22080 nil nil nil 5 nil nil nil 22080 nil 8 1695245616 14322 nil nil nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 1 563 1442 1093 41 22080 1538 #text: 34 1 8 '
' 22080 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 34 2 0 0 45 1 0 0] 8 #() 1728 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 21984 34 2 8 1140920580 1025 22384 13570 nil false 13616 nil 930 19280 nil 5 nil nil nil 22384 nil 8 4294905147 14322 nil nil 11 nil 170 192 34 12 #indentGuide 14386 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 14386 3 20528 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 14386 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 14386 77 14448 14464 1 nil nil nil nil #callTip nil nil nil #normal 14386 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 14386 5 nil 14962 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 22608 22560 22624 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 22576 nil nil nil 22544 nil nil 20642 20672 170 192 34 2 #default 14578 1 nil nil nil 22384 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 14640 nil nil 65 nil nil nil 20768 3 170 192 34 2 #container 22512 nil nil nil nil 1 nil 170 192 34 6 8 'indicator0' 14850 1 22384 33554459 13 false nil nil nil nil nil nil nil 8 'indicator1' 14850 3 22384 33423361 5 false nil nil nil nil nil nil nil 8 'indicator2' 14850 5 22384 511 1 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 14386 1029 nil 14962 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 14386 1027 14962 60239 14962 31326207 1 nil nil nil nil #Warning nil nil nil #Error 14386 1031 14962 335 14962 31317503 1 nil nil nil nil #Error nil nil nil nil 1474 138 144 34 7 1538 #createAt:extent: 34 2 1442 1 1 1442 1093 563 22384 1538 #modificationEventMask: 8 #(4607) 22384 1538 #caretPeriod: 8 #(530) 22384 1538 #wordWrap: 8 #(true) 22384 1538 #margins: 34 1 34 3 15378 1 22384 49 3 nil nil nil nil 15378 3 22384 1 nil nil 67108863 nil nil 15378 5 22384 1 nil nil nil nil nil 22384 1538 #canHScroll: 8 #(false) 22384 1538 #maxCompletionListHeight: 8 #(9) 22384 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 34 2 0 0 25 1 0 0] 8 #() 1728 nil 29 170 192 34 4 22080 8 'afterTitle' 22384 8 'after' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1101 1 1442 1093 603 21984 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 2 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 2 22384 22080 1728 nil 27 1728 nil 27 170 192 34 2 19328 8 'diffs' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 13666 nil true 34 4 13714 1 13746 #selectUpper 8 'Select &upper' 1 1 nil nil nil 13714 1 13746 #selectLower 8 'Select &lower' 1 1 nil nil nil 13794 4097 13714 1 13746 #exit 8 '&Close' 1 1 nil nil nil 8 '&File' nil 1 nil nil 17971 nil nil 13666 nil true 34 3 13714 1 13746 #character 8 '&Character' 1 1 nil nil nil 13714 1 13746 #word 8 '&Word' 1 1 nil nil nil 13714 1 13746 #line 8 '&Line' 1 1 nil nil nil 8 '&Mode' nil 1 nil nil 17973 nil nil 13666 nil true 34 1 13714 1 13746 #about 8 '&About' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 17975 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1474 138 144 34 3 1538 #createAt:extent: 34 2 1442 9 55 1442 2225 635 19200 1538 #text: 34 1 8 'Comparison' 19200 1538 #updateMenuBar 880 19200 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 34 1 19328 1728 nil 27 18336 170 192 34 12 13504 8 'classDefinition' 18336 8 'projectInfo' 19200 8 'comparisonTab' 15600 8 'classDocumentation' 16896 8 'methodSource' 13184 8 'sunit' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 12880 34 2 8 1140916736 1 24688 834 138 144 34 6 18192 13472 15568 16864 13024 19184 nil 912 nil nil 1 nil nil nil 24688 nil 8 4294903035 1154 0 nil 3698 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 24816 7 257 nil 1154 0 nil 3698 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 24864 7 257 nil 1056 nil nil nil nil nil #smallIcons nil nil nil nil nil 1474 138 144 34 3 1538 #createAt:extent: 34 2 1442 1 1 1442 2241 697 24688 1538 #basicSelectionsByIndex: 34 1 8 #(1) 24688 1538 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 24688 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 92 1 0 0] 8 #() 1728 nil 27 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 705 1442 2241 697 12880 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 96 1 0 0 96 4 0 0 188 2 0 0] 34 7 18336 13504 15600 16896 13184 19200 24688 1728 nil 27 1746 3 5 true 170 192 34 4 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 25360 nil 930 7552 nil 517 nil nil nil 25360 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 695 1442 2241 11 25360 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 91 1 0 0 96 4 0 0 96 1 0 0] 8 #() 1728 nil 27 8 'splitter' 12880 8 'textAreaTabs' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 7679 21 1442 2241 1401 416 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 95 19 0 0 198 2 0 0] 34 3 544 25360 12880 1728 nil 27 )! !
!JadeiteProjectsBrowserPresenter class categoriesFor: #downArrowIcon!public! !
!JadeiteProjectsBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

