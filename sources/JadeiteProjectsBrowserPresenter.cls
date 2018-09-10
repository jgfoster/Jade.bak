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
	self selectCategoriesNamed: (Array with: string)!

basicRemoveMethodCategories
	| service |
	service := classListPresenter selection
				command: #removeCategories:;
				commandArgs: (Array with: categoryListPresenter selections).
	self issueCommand: (Array with: service)!

basicRemoveMethods
	| service presenter |
	presenter := self isClassListTabSelected
				ifTrue: [classListPresenter]
				ifFalse: [classHierarchyPresenter].
	service := presenter selectionIfNone: [^self].
	service meta: self isClassSideSelected.
	self removeMethods: (Array with: service)!

basicUnloadProjects: projectNames
	| browserService |
	browserService := RowanBrowserService new.
	browserService
		command: #unloadProjectsNamed:;
		commandArgs: (Array with: projectNames).
	self issueCommand: (Array with: browserService).
	packageListPresenter model: ListModel new.
	classListPresenter model: ListModel new.
	categoryListPresenter model: ListModel new.
	methodListPresenter model: ListModel new!

browseClassReferences
	self browseClassReferences: self selectedClass name!

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
		self addMethodCategory; renameMethodCategory; removeMethodCategories; fileOutCategories.
	].
	^#(
		'&Categories'
		'&Add//addMethodCategory'
		'&Rename//renameMethodCategory'
		'&Remove//removeMethodCategories'
		'-'
		'&File out categories//fileOutCategories'
	)
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
		'Go To Defining Package//gotoClassDefinedPackage'
	).
!

classHierarchyTabs
	^classHierarchyTabs!

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

definingPackageMenuTitle
	| menuItemName |
	menuItemName := 'Go To Defining Package'.
	methodListPresenter selectionIfNone: [^menuItemName].
	^menuItemName, methodListPresenter selection packageName!

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
	classDefinitionPresenter view isModified: false.
	self refreshFromServer. !

editSaveClassComment
	| classService |
	classService := classListPresenter selectionIfNone: [MessageBox notify: 'No class selected'. ^self]. 
	classService
		command: #classComment:;
		commandArgs: (Array with: classCommentPresenter value trimBlanks).
	self issueCommand: (Array with: classService).
	classCommentPresenter ensureVisible. 
	self refreshFromServer. !

editSaveMethod
	| selection |
	selection := self selectedClass.
	selection meta: self isClassSideSelected.
	self editSaveMethod: selection.
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

fileOutCategories
	| path suggestedFileName classSide |
	classSide := self selectedClass meta ifTrue: ['class_'] ifFalse: [String new].
	suggestedFileName := categoryListPresenter selections size = 1
				ifTrue: [classListPresenter selection name , '_' , classSide , categoryListPresenter selection]
				ifFalse: [classListPresenter selection name , classSide , 'categories'].
	suggestedFileName := suggestedFileName copyWithout: $*. 
	path := FileSaveDialog new
				caption: 'File Out Categories';
				fileTypes: self fileTypes;
				defaultExtension: self defaultFileExtension;
				value: suggestedFileName;
				overwritePrompt;
				showModal.
	path ifNotNil: [self fileOutCategoriesOnPath: path]!

fileOutCategoriesOnPath: path
	| file service answeringService |
	service := self selectedClass.
	service
		command: #fileoutCategories:;
		commandArgs: (Array with: categoryListPresenter selections).
	answeringService := (self issueCommand: (Array with: service)) first.
	file := FileStream write: path.
	[file nextPutAll: answeringService answer] ensure: [file close]!

fileOutClass
	| path suggestedFileName classSide |
	suggestedFileName := classListPresenter selection name. 
	path := FileSaveDialog new
				caption: 'File Out Class';
				fileTypes: self fileTypes;
				defaultExtension: self defaultFileExtension;
				value: suggestedFileName;
				overwritePrompt;
				showModal.
	path ifNotNil: [self fileOutClassOnPath: path]!

fileOutClassOnPath: path
	| file answeringService service |
	service := self selectedClass.
	service command: #fileoutClass.
	answeringService := (self issueCommand: (Array with: service)) first.
	file := FileStream write: path.
	[file nextPutAll: answeringService answer] ensure: [file close]!

fileOutMethods
	| path suggestedFileName classSide |
	classSide := self selectedClass meta ifTrue: ['class_'] ifFalse: [String new].
	suggestedFileName := methodListPresenter selections size = 1
				ifTrue: 
					[methodListPresenter selection className , '_' , classSide
						, methodListPresenter selection selector asString]
				ifFalse: [methodListPresenter selections first className , '_' , classSide , 'methods'].
	path := FileSaveDialog new
				caption: 'File Out Method';
				fileTypes: self fileTypes;
				defaultExtension: self defaultFileExtension;
				value: suggestedFileName;
				overwritePrompt;
				showModal.
	path ifNotNil: [self fileOutMethodsOnPath: path]!

fileOutMethodsOnPath: path
	| file answeringService service |
	service := self selectedClass.
	service
		command: #fileoutMethods:;
		commandArgs: (Array with: methodListPresenter selections).
	answeringService := (self issueCommand: (Array with: service)) first.
	file := FileStream write: path.
	[file nextPutAll: answeringService answer] ensure: [file close]!

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

gotoClassDefinedPackage
	" find will go to the class in it's defined package project"

	^self updateAfterFindClass: self selectedClass!

gotoMethodPackage
	"find will go to the defined package of the selected class"

	| selectedCategory selectedMethod className|
	selectedCategory := categoryListPresenter selectionIfNone: [].
	selectedMethod := methodListPresenter selectionIfNone: [].
	className := self selectedClass name. 
	self selectClassListTab.
	self gotoProjectNamed: selectedMethod projectName. 
	self gotoPackageNamed: selectedMethod definedPackage.
	self selectClassNamed: className. 
	selectedCategory ifNotNil: [:category | categoryListPresenter selection: category].
	methodListPresenter selection: (methodListPresenter list
				detect: [:methodService | methodService selector = selectedMethod selector])	"It better be there"!

gotoPackageNamed: packageName
	| packageService |
	packageService := packageListPresenter list detect: [:service | service name = packageName]
				ifNone: [^self].
	packageListPresenter selections: (Array with: packageService)!

gotoProjectNamed: projectName
	| projectService  |
	projectService := projectListPresenter list
				detect: [:service | service name = projectName]
				ifNone: 
					[^self].
	projectListPresenter selections: (Array with: projectService).
	!

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

jadeMenuStrings

	false ifTrue: [
		self abortTransaction; commitTransaction; newWorkspace; openSystemBrowser; refreshFromServer; exit.
	]. 
	^#(
		'&Jadeite'
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
		self browseImplementors; browseImplementorsOf; browseSenders; browseSendersOf; browseHierarchyImplementors; browseHierarchySenders; browseMethodsContaining; browseMethodPragmas; removeMethods; runMethodTests; setEnvironment0; setEnvironment1; setEnvironment2; fileOutMethods; gotoMethodPackage.
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
		'&File out methods ...//fileOutMethods'
		'-'
		'&Remove Method(s) ...//removeMethods'
		#('&Method Environment' '&0//setEnvironment0' '&1//setEnvironment1' '&2//setEnvironment2')
		'Run &Tests//runMethodTests'
		'Go To Defining Package//gotoMethodPackage'
	).
!

moveMethods: methodServices toCategory: targetCategory
	| classService |
	classService := classListPresenter selection.
	self selectCategoriesNamed: (categoryListPresenter selections copyWith: targetCategory).
	classService
		command: #moveMethods:to:;
		commandArgs: (Array with: methodServices with: targetCategory).
	self issueCommand: (Array with: classService).
	self selectMethodsFrom: classService!

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
	| methodServices targetCategory |
	methodServices := aSession dragObjects collect: [:each | each format: #method].
	targetCategory := aSession suggestedTarget trimBlanks.
	self moveMethods: methodServices toCategory: targetCategory!

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
			shell selectClass: classService name asString selector: String new.
			^shell currentCard]!

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

projectInfoTab
	^projectInfoTab!

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
				unloadProjects;
				pullFromGit;
				gitCommit;
				pushToGit;
				projectChanges;
				writeProject;
				checkout;
				projectLog;
				refresh].
	^#('Projects' '&Find Class.../Ctrl+Shift+F/findClass' '&Make Default//makeDefault' '-' 'Clone Git Project ...//cloneProject' 'Load//loadProjects' 'Unload//unloadProjects' '-' 'Pull from Git ...//pullFromGit' 'Commit ...//gitCommit' 'Push to Git ...//pushToGit' '-' '&Changes ...//projectChanges' 'Write ...//writeProject' 'Checkout ...//checkout' '-' 'Log//projectLog' '&Refresh//refreshFromServer')!

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
	(#(#browseMethodHistory #browseImplementors #browseSenders #gotoMethodPackage) includes: command)
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
	(#( #fileOutMethods) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: self methodListPresenter selections notEmpty.
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
	(#(#browseClassReferences #fileOutClass #addSubclass #addMissingAccessors #removeClass #removePriorVersions #gotoClassDefinedPackage)
		includes: command)
			ifTrue: 
				[aCommandQuery isEnabled: self selectedClasses size == 1.
				^true].
	(#(#runClassTests) includes: command) ifTrue: [aCommandQuery isEnabled: (self selectedClass ifNil:[false] ifNotNil:[:classService | classService isTestCase]). ^true].
	(#(#'runMethodTests') includes: command) ifTrue: [aCommandQuery isEnabled: (methodListPresenter selections notEmpty and: [methodListPresenter selections first isTestMethod]). ^true].
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
	command == #renameMethodCategory
		ifTrue: 
			[aCommandQuery isEnabled: self hasExactlyOneCategorySelected.
			^true].
	command == #removeMethodCategories
		ifTrue: 
			[aCommandQuery isEnabled: self hasCategorySelected.
			^true].
	command == #fileOutCategories
		ifTrue: 
			[aCommandQuery isEnabled: self hasCategorySelected.
			^true].
	command == #cloneProject
		ifTrue: 
			[aCommandQuery isEnabled: true.
			^true].
	command == #refreshFromServer
		ifTrue: 
			[aCommandQuery isEnabled: true.
			^true].
	command == #findClass
		ifTrue: 
			[aCommandQuery isEnabled: true.
			^true].
	(command == #setEnvironment0 and: [environment == 0])
		ifTrue: 
			[aCommandQuery isChecked: true.
			^true].
	(command == #setEnvironment1 and: [environment == 1])
		ifTrue: 
			[aCommandQuery isChecked: true.
			^true].
	(command == #setEnvironment2 and: [environment == 2])
		ifTrue: 
			[aCommandQuery isChecked: true.
			^true].
	(self projectMenuCommands includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: projectListPresenter selections notEmpty.
			^true].
	(#(#browseHierarchyImplementors #browseHierarchySenders) includes: command)
		ifTrue: 
			[aCommandQuery isEnabled: methodListPresenter selections notEmpty.
			^true].
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
	presenter := self isClassListTabSelected
				ifTrue: [classListPresenter]
				ifFalse: [classHierarchyPresenter].
	self setMethodFilter.
	selection := presenter selectionIfNone: 
					[categoryListPresenter clear.
					^methodListPresenter clear].
	selection meta: self isClassSideSelected.
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
	| classService classPresenter |
	classPresenter := self isClassListTabSelected
				ifTrue: [classListPresenter]
				ifFalse: [classHierarchyPresenter].
	classService := classPresenter selectionOrNil ifNil: [^self].
	self isCategoriesTabSelected
		ifTrue: [classService filters: categoryListPresenter selections]
		ifFalse: [classService filters: variableListPresenter selections].
	classService filters isEmpty
		ifTrue: [classService filterType: nil]
		ifFalse: 
			[self isCategoriesTabSelected
				ifTrue: [classService filterType: 'categories']
				ifFalse: [classService filterType: 'variables']].
	classService methods isNil
		ifTrue: 
			["if we got an error recently, could be nil"
			self updateServices: (Array with: classService)].
	classService displayMethodsOn: presenter!

registerClassData
	"temporarily disabled"!

registerPresentersForUpdates

	BrowserUpdate current register: projectListPresenter 		selector: #projectsUpdate:.
	BrowserUpdate current register: projectListPresenter 		selector: #removedProject:.
	BrowserUpdate current register: classListPresenter 		selector: #classesUpdate:browser: browser: self.
	BrowserUpdate current register: classListPresenter 		selector: #removedClass:.
	BrowserUpdate current register: packageListPresenter 	selector: #projectPackagesUpdate:browser: browser: self.
	BrowserUpdate current register: categoryListPresenter 	selector: #filterUpdate:browser: browser: self.
	BrowserUpdate current register: classHierarchyPresenter 	selector: #classHierarchyUpdate:browser: browser: self.
	BrowserUpdate current register: variableListPresenter 	selector: #filterUpdate:browser: browser: self.
	BrowserUpdate current register: methodListPresenter 	selector: #classMethodsUpdate:browser: browser: self.
	BrowserUpdate current register: methodListPresenter 	selector: #methodsRemoved:.
	BrowserUpdate current register: methodSourcePresenter 	selector: #methodSourceUpdate:browser: browser: self. !

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
	(new := Prompter
				on: old
				prompt: 'New method category?'
				caption: 'Rename Category') isNil
		ifTrue: [^self].
	(categoryListPresenter list includes: new)
		ifTrue: [^MessageBox notify: 'category ' , new , ' already exists'].
	service := classListPresenter selection
				command: #renameCategoryFrom:to:;
				commandArgs: (Array with: old with: new).
	self issueCommand: (Array with: service).
	categoryListPresenter selection: new!

runMethodTests
	| classService answeringService |
	classService := self selectedClass.
	classService
		command: #runMethodTests:;
		commandArgs: (Array with: methodListPresenter selections).
	answeringService := (self issueCommand: (Array with: classService)) first.
	answeringService answer
		ifTrue: 
			[| testResult |
			sunitPresenter reset. 
			testResult := JadeTestResult new.
			testResult
				passed: (methodListPresenter selections collect: [:methodService | methodService selector]).
			sunitPresenter updateResult: testResult.
			sunitPresenter ensureVisible]!

savePackage
	MessageBox notify: 'This feature is not yet available'!

selectCategoriesNamed: array
	| selections |
	selections := categoryListPresenter list select: [:category | array includes: category].
	categoryListPresenter selections: selections!

selectCategoryFrom: methodService
	^categoryListPresenter selectionOrNil = methodService category
		ifFalse: [categoryListPresenter selection: methodService category]!

selectClass: classSelection
	| selection |
	selection := classListPresenter list detect: [:svc | svc name = classSelection name] ifNone: [].
	^selection ifNotNil: [classListPresenter selection: selection]!

selectClassInClassHierarchyNamed: className
	| selection |
	selection := classHierarchyPresenter model asBag
				detect: [:classService | classService name = className]
				ifNone: [].
	selection ifNotNil: [classHierarchyPresenter selection: selection].
	^selection!

selectClassInClassListNamed: className
	| selection |
	selection := classListPresenter list detect: [:classService | classService name = className]
				ifNone: [].
	selection ifNotNil: [classListPresenter selection: selection].
	^selection!

selectClassListTab
	classHierarchyTabs firstCard!

selectClassNamed: className
	| selection |
	self isClassListTabSelected
		ifTrue: [selection := self selectClassInClassListNamed: className]
		ifFalse: [selection := self selectClassInClassHierarchyNamed: className]!

selectedClass
	^self isClassListTabSelected
		ifTrue: [classListPresenter selectionIfNone: []]
		ifFalse: [classHierarchyPresenter selectionIfNone: []]!

selectedClasses
	^self isClassListTabSelected
		ifTrue: [classListPresenter selections]
		ifFalse: [classHierarchyPresenter selections]!

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

setTestClass: service
	sunitPresenter testClassService: (service isTestCase ifTrue: [service] ifFalse: [nil])!

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

unloadProjects
	| projectNames ws |
	projectNames := projectListPresenter selections collect: [:projectService | projectService name].
	projectNames isEmpty ifTrue: [^self].
	ws := WriteStream on: String new.
	projectNames do: 
			[:projectName |
			ws
				space;
				nextPutAll: projectName].
	(MessageBox
		confirm: 'Really unload project(s) - ' , ws contents , '?  You will lose any unsaved changes!! '
		caption: 'UNLOAD PROJECTS WARNING!!') ifFalse: [^self].
	self basicUnloadProjects: projectNames!

update: services afterStonReplication: stonResults
	| result |
	result := super update: services afterStonReplication: stonResults. 
	self redrawAllViews. 
	^result!

updateAfterFindClass: classService
	| projectService packageService|
	projectService := projectListPresenter list
				detect: [:service | service name = classService projectName]
				ifNone: 
					["old school"
					^self openDictionaryBrowserOn: classService].
	projectListPresenter selections: (Array with: projectService).
	packageService := packageListPresenter list
				detect: [:service | service name = classService definedPackageName]
				ifNone: [^self].
	packageListPresenter selections: (Array with: packageService).
	self isClassListTabSelected
		ifTrue: [self selectClassNamed: classService name]
		ifFalse: 
			[self selectClassInClassListNamed: classService name.
			self updateHierarchy]!

updateAndSelectA: aView
	"is this override still needed?"
	self updateMenuBar.
	aView notNil ifTrue: [aView ensureVisible].
!

updateClasses
	self updateServices: packageListPresenter selections.!

updateClassInfo
	| service |
	service := self selectedClass ifNil:[^self]. 
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
	self setTestClass: service.
	
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
	| service selection |
	classHierarchyPresenter model: TreeModel new.
	packageListPresenter list isEmpty ifTrue: [^self].
	Cursor wait showWhile: 
			[service := classListPresenter selectionOrNil
						ifNil: [^self].
			service command: #classHierarchy.
			self issueCommand: (Array with: service).
			service name = 'Object'
				ifFalse: 
					[selection := classHierarchyPresenter model asBag detect: [:svc | svc name = service name].
					classHierarchyPresenter selection: selection]]!

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
	self issueCommand: services.
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
	classListPresenter model: ListModel new.
	classHierarchyPresenter model: TreeModel new.
	self updateClasses.
	self isClassListTabSelected ifFalse:[self updateHierarchy].
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
	BrowserUpdate current updateServices: services session: gciSession!

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
!JadeiteProjectsBrowserPresenter categoriesFor: #basicRemoveMethodCategories!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #basicRemoveMethods!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #basicUnloadProjects:!menu handlers support!private! !
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
!JadeiteProjectsBrowserPresenter categoriesFor: #classHierarchyTabs!accessing!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #cleanupComparisonBrowser!initialize/release!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #clearFilterLists!event handlers support!private!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #cloneProject!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #comparisonTab!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #confirmCategoryRemoval:!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createComponents!initialize/release!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiring!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForClassHierarchy!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForClassHierarchyTabs!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForClassList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForInstanceClassTabs!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForMethodCategoryList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForMethodFilterTabs!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForMethodList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForPackageDictionaryTabs!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForPackageList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForProjectDictionaryList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForProjectList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createSchematicWiringForVariableList!create schemantic wiring!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #createStateEntry!accessing!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #definingPackageMenuTitle!menus!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #editSaveClass!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #editSaveClassComment!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #editSaveMethod!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #editSaveMethod:!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #fileOutCategories!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #fileOutCategoriesOnPath:!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #fileOutClass!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #fileOutClassOnPath:!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #fileOutMethods!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #fileOutMethodsOnPath:!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #filterTabChanged!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #findClass!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #getViews!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #gitCommit!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #gotoClassDefinedPackage!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #gotoMethodPackage!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #gotoPackageNamed:!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #gotoProjectNamed:!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #hasCategorySelected!public!testing! !
!JadeiteProjectsBrowserPresenter categoriesFor: #hasExactlyOneCategorySelected!public!testing! !
!JadeiteProjectsBrowserPresenter categoriesFor: #initialize!initialize/release!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #isClassSelected!public!testing! !
!JadeiteProjectsBrowserPresenter categoriesFor: #isDictionariesTabSelected!public!testing! !
!JadeiteProjectsBrowserPresenter categoriesFor: #isOkayToChange!event handlers!public!testing! !
!JadeiteProjectsBrowserPresenter categoriesFor: #jadeMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #loadProjects!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #makeDefault!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #methodListPresenter!accessing!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #methodListSelection!accessing!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #methodMenuBreak:!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #methodsMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #moveMethods:toCategory:!drag & drop!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onDragMethod:!drag & drop!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onDropOnClassList:!drag & drop!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onDropOnMethodCategory:!drag & drop!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onSetFocus:stack:!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #onViewOpened!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #openDictionaryBrowserOn:!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #pkgsMenuStrings!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #populateFilterList!event handlers support!private!services!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #postOkToChangeEvent!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectChanges!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectDictionaryChanged!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectInfoTab!accessing!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectListPresenter!accessing!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectLog!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectMenu!menus!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #projectMenuCommands!menus!public! !
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
!JadeiteProjectsBrowserPresenter categoriesFor: #runMethodTests!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #savePackage!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectCategoriesNamed:!public!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectCategoryFrom:!public!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectClass:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectClassInClassHierarchyNamed:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectClassInClassListNamed:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectClassListTab!public!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectClassNamed:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectedClass!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectedClasses!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectedMethodCategory!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectedMethodName!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectionMethod:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectMethodsFrom:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectMethodSourceTab!public!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #selectPackage:!private!selection! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setDefaultProjectTablLabel!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setDefaultProjectTablLabel:!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setFilterType:!event handlers support!private!services! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setMethodFilter!event handlers support!private!services!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #setTestClass:!event handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #showSuperClassComparison!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #sundryCleanup!initialize/release!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #targetCategory!menu handlers support!private! !
!JadeiteProjectsBrowserPresenter categoriesFor: #textTabChanged!event handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #textTabs!private!text tabs! !
!JadeiteProjectsBrowserPresenter categoriesFor: #unloadPackage!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #unloadProjects!project menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #update:afterStonReplication:!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateAfterFindClass:!menu handlers!public! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateAndSelectA:!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateClasses!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateClassInfo!event handlers!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateCommand:onSuccessDo:!public!updating! !
!JadeiteProjectsBrowserPresenter categoriesFor: #updateHierarchy!event handlers!public!updating! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 656 nil nil nil 5 nil nil nil 656 482 170 176 34 6 410 ##(Smalltalk.ListView) 34 30 nil 656 34 2 8 1409405257 1025 768 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 768 nil 8 4294906907 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 361 #left 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 1152 8 'doIt' 8 '[:each | each displayName]' 8 #[30 105 226 0 106] #displayName 1168 7 257 nil ##(Smalltalk.SortedCollection) nil nil 768 nil 3 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 1264 7 257 nil 1154 0 nil 1186 3 1 1152 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 1344 7 257 nil #report 880 nil 133217 nil 34 4 nil nil 328198 ##(Smalltalk.Point) 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 1442 1 1 1442 369 695 768 1538 #text: 34 1 8 'Column 1' 768 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 1442 193 193 nil 27 524806 ##(Smalltalk.Fraction) 9 11 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 1776 nil nil nil 5 nil nil nil 1776 482 170 176 880 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 1776 34 2 8 1140936777 1025 1904 834 138 144 880 nil 912 930 8 4278190080 nil 21 nil nil nil 1904 nil 8 4294906907 994 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 'Column 1' 361 #left 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 2144 7 257 nil 994 #<= 8 #() nil nil 1904 nil 3 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 2256 7 257 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 2336 7 257 nil #report 880 nil 133217 nil 34 4 nil nil 1442 1 1 nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 1 1 1442 369 695 1904 1538 #text: 34 1 8 'Column 1' 1904 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 0 0 0 91 1 0 0] 8 #() 1728 nil 27 8 'packagesList' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 379 1 1442 369 695 1776 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 189 0 0 0 0 0 0 0 117 1 0 0 91 1 0 0] 34 1 1904 1728 nil 27 1746 9 11 410 ##(Smalltalk.ContainerView) 34 15 nil 656 34 2 8 1140850688 131073 2848 nil nil nil 5 nil nil nil 2848 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 2848 34 2 8 1409286144 131073 2976 nil 930 8 4278190080 nil 5 nil nil nil 2976 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Classes' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 3168 3200 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 3216 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 2976 34 2 8 1409372237 1025 3312 834 138 144 880 nil 912 930 960 nil 29 nil nil nil 3312 nil 8 4294906907 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 '' 559 #left 994 #displayString 8 #() 994 #<= 8 #() nil nil 3312 nil 3 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 3600 7 257 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 3680 7 257 nil #report 880 nil 133217 nil 34 4 nil nil 1442 1 1 nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 9 55 1442 567 589 3312 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 1728 nil 27 3138 3170 8 'Hierarchy' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 3168 3984 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 4000 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 2976 34 2 8 1409354279 1025 4096 590918 3 ##(Smalltalk.TreeModel) nil 912 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 880 930 8 4278190080 nil 29 nil nil nil 4096 nil 8 4294905567 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each displayString]' 8 #[30 105 226 0 106] #displayString 4288 7 257 nil ##(Smalltalk.IconicListAbstract) 1056 nil 1154 0 nil 1186 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each toolTip]' 8 #[30 105 226 0 106] #toolTip 4368 7 257 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 4448 7 257 nil nil nil 170 176 880 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 880 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 9 55 1442 567 589 4096 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 31 1 0 0 65 1 0 0] 8 #() 1728 nil 27 3312 170 192 34 4 3312 8 'classList' 4096 8 'classHierarchy' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 2976 34 2 8 1140916736 1 4784 834 138 144 34 2 3184 3968 nil 912 nil nil 1 nil nil nil 4784 nil 8 4294905519 1154 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 4912 7 257 nil 1154 0 nil 4930 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 4976 7 257 nil 1056 nil nil nil nil nil #smallIcons nil nil nil nil nil 1474 138 144 34 3 1538 #createAt:extent: 34 2 1442 1 1 1442 583 651 4784 1538 #basicSelectionsByIndex: 34 1 8 #(1) 4784 1538 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 4784 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 8 #() 1728 nil 27 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 1 1442 583 651 2976 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 69 1 0 0] 34 3 3312 4096 4784 1728 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 5458 #fixedParentRight 1 5458 #fixedParentTop 1 5458 #fixedParentBottom -43 410 ##(Smalltalk.CardContainer) 34 16 nil 2848 34 2 8 1409286144 131073 5536 nil 930 8 4278190080 nil 5 nil nil nil 5536 3074 138 144 34 2 3138 3170 8 'Instance' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 3168 5712 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 5728 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 5536 34 2 8 1140850688 131073 5824 nil nil nil 5 nil nil nil 5824 nil 170 192 880 nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 9 9 1442 567 1 5824 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 1728 nil 27 3138 3170 8 'Class' 1154 0 nil 1186 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 3168 6096 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 6112 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 5536 34 2 8 1140850688 131073 6208 nil nil nil 5 nil nil nil 6208 nil 170 192 880 nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 9 9 1442 567 1 6208 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 31 1 0 0 4 0 0 0] 8 #() 1728 nil 27 5824 170 192 34 4 6208 8 'classTab' 5824 8 'instanceTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 5536 34 2 8 1140916738 1 6512 834 138 144 34 2 5696 6080 nil 912 nil nil 1 nil nil nil 6512 nil 8 4294905519 1154 0 nil 4930 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 6640 7 257 nil 1154 0 nil 4930 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 6688 7 257 nil 1056 nil nil nil nil nil #smallIcons nil nil nil nil nil 1474 138 144 34 3 1538 #createAt:extent: 34 2 1442 1 1 1442 583 51 6512 1538 #basicSelectionsByIndex: 34 1 8 #(1) 6512 1538 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 6512 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 25 0 0 0] 8 #() 1728 nil 27 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 645 1442 583 51 5536 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 66 1 0 0 35 1 0 0 91 1 0 0] 34 3 5824 6208 6512 1728 nil 27 5426 5472 1 5488 1 5520 -49 5458 #fixedViewTop 51 170 192 34 4 2976 8 'classHierarchyTabs' 5536 8 'instanceClassTabs' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 757 1 1442 583 695 2848 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 122 1 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 2 2976 5536 1728 nil 27 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 244 63 false 170 192 34 2 768 8 'projectList' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 1 1442 1339 695 656 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 157 2 0 0 91 1 0 0] 34 5 768 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 7632 nil 930 8 4278190080 nil 517 nil nil nil 7632 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 369 1 1442 11 695 7632 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 184 0 0 0 0 0 0 0 189 0 0 0 91 1 0 0] 8 #() 1728 nil 27 1776 410 ##(Smalltalk.Splitter) 34 12 nil 656 34 2 8 1140850688 1 7888 nil 930 7712 nil 517 nil nil nil 7888 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 747 1 1442 11 695 7888 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 117 1 0 0 0 0 0 0 122 1 0 0 91 1 0 0] 8 #() 1728 nil 27 2848 1728 nil 27 7 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 8128 nil nil nil 5 nil nil nil 8128 482 170 176 880 false 170 192 34 2 410 ##(Smalltalk.CardContainer) 34 16 nil 8128 34 2 8 1409286144 131073 8256 nil 930 8 4278190080 nil 5 nil nil nil 8256 3074 138 144 34 2 3138 8 'Category' 410 ##(Smalltalk.ListView) 34 30 nil 8256 34 2 8 1409372233 1025 8432 834 138 144 880 nil 912 930 8 4278190080 nil 21 nil nil nil 8432 nil 8 4294906907 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 'Column 1' 417 #left 994 #displayString 8 #() ##(Smalltalk.SortedCollection) 1154 0 nil 1186 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each]' 1779525917 8704 7 257 nil nil 8432 nil 3 nil nil #report 880 nil 131169 nil 34 4 nil nil 1442 1 1 nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 9 55 1442 425 633 8432 1538 #text: 34 1 8 'Column 1' 8432 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 216 0 0 0 87 1 0 0] 8 #() 1728 nil 27 3138 8 'Variables' 410 ##(Smalltalk.ListView) 34 30 nil 8256 34 2 8 1409372233 1025 9040 834 138 144 880 nil 912 930 8544 nil 21 nil nil nil 9040 nil 8 4294906907 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 1 1106 8 'Column 1' 417 #left 994 #displayString 9184 ##(Smalltalk.SortedCollection) nil nil 9040 nil 3 nil 1154 0 nil 1186 9 1 1152 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 9280 7 257 nil #report 880 nil 131169 nil 34 4 nil nil 1442 1 1 nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 9 55 1442 425 633 9040 1538 #text: 34 1 8 'Column 1' 9040 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 216 0 0 0 87 1 0 0] 8 #() 1728 nil 27 8432 170 192 34 4 8432 8 'categoryList' 9040 8 'variableList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 8256 34 2 8 1140916736 1 9664 834 138 144 34 2 8416 9024 nil 912 nil nil 1 nil nil nil 9664 nil 8 4294905519 1154 0 nil 4930 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 9792 7 257 nil 1154 0 nil 4930 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 9840 7 257 nil 1056 nil nil nil nil nil #noIcons nil nil nil nil nil 1474 138 144 34 3 1538 #createAt:extent: 34 2 1442 1 1 1442 441 695 9664 1538 #basicSelectionsByIndex: 34 1 8 #(1) 9664 1538 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 9664 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 8 #() 1728 nil 27 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 1 1442 441 695 8256 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 91 1 0 0] 34 3 8432 9040 9664 1728 nil 27 8 'methodFilterTabs' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1349 1 1442 893 695 8128 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 162 2 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 8256 410 ##(Smalltalk.Splitter) 34 12 nil 8128 34 2 8 1140850688 1 10464 nil 930 7712 nil 517 nil nil nil 10464 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 441 1 1442 11 695 10464 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 220 0 0 0 0 0 0 0 225 0 0 0 91 1 0 0] 8 #() 1728 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 8128 34 2 8 1140850688 131073 10704 nil nil nil 5 nil nil nil 10704 2914 170 176 34 2 410 ##(Smalltalk.ListView) 34 30 nil 10704 34 2 8 1409372233 1025 10816 834 138 144 880 nil 912 930 8544 nil 13 nil nil nil 10816 nil 8 4294906907 994 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1056 nil nil nil nil nil nil 138 144 34 2 1106 8 '' 41 #left 1538 #empty 8 #() 16 994 #<= 8 #() nil nil 10816 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:each | each subSuperIcon]' 8 #[30 105 226 0 106] #subSuperIcon 11104 7 257 nil 1 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:methodService | methodService toolTip]' 8 #[30 105 226 0 106] #toolTip 11184 7 257 nil nil 1106 8 'Selector' 395 #left 994 #displayString 10960 ##(Smalltalk.SortedCollection) nil nil 10816 nil 3 1154 0 nil 1186 2 1 1152 8 'doIt' 8 '[:methodService | methodService toolTip]' 8 #[30 105 226 0 106] #toolTip 11312 7 257 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item displayStringFor: each]' 8 #[31 105 226 0 17 177 106] #item #displayStringFor: 11392 7 257 nil #report 880 nil 133221 nil 34 4 nil nil 1442 1 1 nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 55 1442 443 631 10816 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 0 0 0 221 0 0 0 86 1 0 0] 8 #() 1728 nil 27 5426 5472 1 5488 1 5504 55 5520 -9 170 192 34 2 10816 8 'methodList' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 451 1 1442 443 695 10704 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 225 0 0 0 0 0 0 0 190 1 0 0 91 1 0 0] 34 1 10816 1728 nil 27 1728 nil 27 5 false 170 192 880 nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 1 1442 2241 695 544 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 91 1 0 0] 34 3 656 410 ##(Smalltalk.Splitter) 34 12 nil 544 34 2 8 1140850688 1 12064 nil 930 7712 nil 517 nil nil nil 12064 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1339 1 1442 11 695 12064 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 157 2 0 0 0 0 0 0 162 2 0 0 91 1 0 0] 8 #() 1728 nil 27 8128 1728 nil 27 1746 3 5 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 12320 nil 930 3056 nil 5 nil nil nil 12320 3074 138 144 34 6 3138 3170 8 'SUnit' 1154 0 nil 1186 4 1 1152 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 12496 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 12320 34 2 8 1140850688 131073 12624 nil 930 8 4278190080 nil 5 nil nil nil 12624 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 9 55 1442 2225 635 12624 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 880 1728 nil 27 3138 3170 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 12320 34 2 8 1445007428 1025 12944 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 930 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 11 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #editSave 8 '&Save' 9383 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 13154 1 13186 #editUndo 8 '&Undo' 9397 1 nil nil nil 13154 1 13186 #editRedo 8 '&Redo' 9395 1 nil nil nil 13234 4097 13154 1 13186 #editCut 8 'Cu&t' 9393 1 nil nil nil 13154 1 13186 #editCopy 8 '&Copy' 9351 1 nil nil nil 13154 1 13186 #editPaste 8 '&Paste' 9389 1 nil nil nil 13154 1 13186 #editDelete 8 '&Delete' 1629 1 nil nil nil 13234 4097 13154 1 13186 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1442 193 193 nil 12944 nil 8 4294906933 852486 ##(Smalltalk.NullConverter) nil nil 13 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 13826 77 786694 ##(Smalltalk.IndexedColor) 33554457 13874 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 13840 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 13856 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 880 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 12944 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 13874 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 13840 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 12944 65025 3 false 1 nil nil nil nil nil nil 3 14290 3 12944 33423361 5 false 3 nil nil nil nil nil nil 5 14290 5 12944 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 13826 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 13826 1027 14402 60239 14402 31326207 1 nil nil nil nil #Warning nil nil nil #Error 13826 1031 14402 335 14402 31317503 1 nil nil nil nil #Error nil nil nil nil 1474 138 144 34 8 1538 #createAt:extent: 34 2 1442 9 55 1442 2225 635 12944 1538 #contextMenu: 34 1 13120 12944 1538 #modificationEventMask: 8 #(4607) 12944 1538 #caretPeriod: 8 #(530) 12944 1538 #wordWrap: 8 #(true) 12944 1538 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 12944 1 3 nil nil 5 nil 14818 3 12944 33 nil 3 67108863 5 nil 14818 5 12944 1 nil 3 -67108863 5 nil 12944 1538 #tabWidth: 8 #(4) 12944 1538 #maxCompletionListHeight: 8 #(9) 12944 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1728 nil 29 3138 3170 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 12320 34 2 8 1445007428 1025 15040 13010 nil false 13056 nil 930 13088 nil 5 13106 nil true 34 1 13154 1 13186 #editSaveClassComment 8 'Save' 1025 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 13650 nil true 13682 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1442 193 193 nil 15040 nil 8 4294906933 13762 nil nil 11 nil 170 192 34 4 #normal 13826 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 13826 77 13888 13904 1 nil nil nil nil #callTip nil nil nil 34 256 15360 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 15376 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 14018 1 nil nil nil 15040 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 14080 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 15360 nil nil nil nil 1 nil 170 192 34 6 1 14290 1 15040 65025 3 false 1 nil nil nil nil nil nil 3 14290 3 15040 33423361 5 false 3 nil nil nil nil nil nil 5 14290 5 15040 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 13826 1029 nil 14402 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 13826 1027 14402 60239 14402 31326207 1 nil nil nil nil #Warning nil nil nil #Error 13826 1031 14402 335 14402 31317503 1 nil nil nil nil #Error nil nil nil nil 1474 138 144 34 8 1538 #createAt:extent: 34 2 1442 9 55 1442 2225 635 15040 1538 #contextMenu: 34 1 15136 15040 1538 #modificationEventMask: 8 #(4607) 15040 1538 #caretPeriod: 8 #(530) 15040 1538 #wordWrap: 8 #(true) 15040 1538 #margins: 34 1 34 3 14818 1 15040 1 3 nil nil 5 nil 14818 3 15040 33 nil 3 67108863 5 nil 14818 5 15040 1 nil 3 -67108863 5 nil 15040 1538 #tabWidth: 8 #(4) 15040 1538 #maxCompletionListHeight: 8 #(9) 15040 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1728 nil 29 3138 3170 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 12320 34 2 8 1445007428 1025 16336 13010 nil false 13056 nil 930 8 4278190080 nil 5 nil 13650 nil true 13682 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1442 193 193 nil 16336 nil 8 4294906933 13762 nil nil 13 nil 170 192 34 4 #normal 13826 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 13826 77 13888 13904 1 nil nil nil nil #callTip nil nil nil 34 256 16576 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 16592 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 880 170 192 34 2 #default 14018 1 nil nil nil 16336 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 14080 nil nil 65 nil nil 14096 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 16576 #callTip 16592 nil nil nil nil 1 nil 170 192 34 12 1 14290 1 16336 65025 3 false 1 nil nil nil nil nil nil 3 14290 3 16336 33423361 5 false 3 nil nil nil nil nil nil 5 14290 5 16336 511 1 false 5 nil nil nil nil nil nil 8 'indicator10' 14290 21 16336 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator8' 14290 17 16336 33554447 1 nil nil nil nil nil nil nil nil 8 'indicator9' 14290 19 16336 33554459 13 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 13826 1029 nil 14402 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 13826 1027 14402 60239 14402 31326207 1 nil nil nil nil #Warning nil nil nil #Error 13826 1031 14402 335 14402 31317503 1 nil nil nil nil #Error nil nil nil nil 1474 138 144 34 8 1538 #createAt:extent: 34 2 1442 9 55 1442 2225 635 16336 1538 #modificationEventMask: 8 #(4607) 16336 1538 #hoverTime: 8 #(200) 16336 1538 #caretPeriod: 8 #(530) 16336 1538 #wordWrap: 8 #(true) 16336 1538 #margins: 34 1 34 3 14818 1 16336 61 3 nil nil 5 nil 14818 3 16336 1 nil 3 67108863 nil nil 14818 5 16336 1 nil 3 -67108863 nil nil 16336 1538 #tabWidth: 8 #(4) 16336 1538 #maxCompletionListHeight: 8 #(9) 16336 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1728 nil 29 3138 3170 8 'Project' 1154 0 nil 1186 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''Collection.ico'']' 8 #[32 105 45 30 178 106] 12578 #Icon ##(Smalltalk.Icon) 8 'Collection.ico' #fromId: 17664 7 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 12320 34 2 8 1409372237 1025 17776 834 138 144 880 nil 912 930 8 4278190080 nil 5 nil nil nil 17776 nil 8 4294906907 994 #displayString 8 #() nil 1056 nil nil nil nil nil nil 138 144 34 2 1106 8 'Name' 201 #left 994 #displayString 17936 ##(Smalltalk.SortedCollection) 1154 0 nil 1186 1 83886081 1152 8 'doIt' 8 '[:each | (each at: 2)]' 8 #[29 105 17 64 148 106] 18032 7 257 nil nil 17776 nil 1 nil 1154 0 nil 1186 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each item at: 1) displayStringFor: each]' 8 #[31 105 226 0 63 148 17 177 106] #item #displayStringFor: 18112 7 257 nil 1106 8 'Value' 2017 #left 994 #displayString 8 #() 994 #<= 18240 1154 0 nil 1186 4 1 1152 8 'doIt' 8 '[:each | (each at: 2) ifNil:[String new] ifNotNil: [:iv |
(each at: 1) instVarNamed: iv]]' 8 #[32 105 17 64 148 100 223 4 97 45 146 106 90 17 63 148 18 178 106] 12578 #String 16 #ifNil:ifNotNil: #instVarNamed: 18272 7 65793 nil nil 17776 nil 3 nil nil #report 880 nil 131169 nil 34 4 nil nil 1442 1 1 nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 9 55 1442 2225 635 17776 1538 #text: 34 1 8 'Name' 17776 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 8 #() 1728 nil 27 3138 8 'Comparison' 410 ##(Smalltalk.ShellView) 34 27 nil 12320 34 2 8 1078919168 131073 18640 nil 930 8 4278190080 nil 517 nil nil nil 18640 788230 ##(Smalltalk.BorderLayout) 1 1 nil nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 18640 34 2 8 1140850688 131073 18768 nil 930 18720 nil 5 13106 nil true 34 5 13154 1 13186 #toggleIndentGuides 8 'Indentation &Guides' 1 1 nil nil nil 13154 1 13186 #toggleVisibleLineEndings 8 'Line &Endings' 1 1 nil nil nil 13154 1 13186 #toggleLineNumberMargins 8 'Line N&umbers' 1 1 nil nil nil 13154 1 13186 #toggleVisibleWhitespace 8 'W&hitespace' 1 1 nil nil nil 13154 1 13186 #toggleWrapping 8 '&Word Wrap' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 18768 482 170 176 880 false 170 192 880 nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 1 1 1442 2193 603 18768 1538 #contextMenu: 34 1 18848 18768 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 18768 34 2 8 1140850688 131073 19376 nil 930 18720 nil 5 nil nil nil 19376 18738 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 19376 34 2 8 1140850945 262145 19472 nil nil nil 5 nil nil nil 19472 nil 8 1695245616 13762 nil nil nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 1 563 1442 1091 41 19472 1538 #text: 34 1 8 '
' 19472 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 33 2 0 0 45 1 0 0] 8 #() 1728 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 19376 34 2 8 1140920580 1025 19776 13010 nil false 13056 nil 930 18720 nil 5 nil nil nil 19776 nil 8 4294906933 13762 nil nil 11 nil 170 192 34 12 #indentGuide 13826 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 13826 3 13874 33554445 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 13826 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 13826 77 13888 13904 1 nil nil nil nil #callTip nil nil nil #normal 13826 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 13826 5 nil 14402 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 20016 19952 20032 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 19984 nil nil nil 19936 nil nil 1310982 ##(Smalltalk.DiffsScintillaStyler) 8 #() 170 192 34 2 #default 14018 1 nil nil nil 19776 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 14080 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 19904 nil nil nil nil 1 nil 170 192 34 6 8 'indicator0' 14290 1 19776 33554459 13 false nil nil nil nil nil nil nil 8 'indicator1' 14290 3 19776 33423361 5 false nil nil nil nil nil nil nil 8 'indicator2' 14290 5 19776 511 1 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 13826 1029 nil 14402 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 13826 1027 14402 60239 14402 31326207 1 nil nil nil nil #Warning nil nil nil #Error 13826 1031 14402 335 14402 31317503 1 nil nil nil nil #Error nil nil nil nil 1474 138 144 34 7 1538 #createAt:extent: 34 2 1442 1 1 1442 1091 563 19776 1538 #modificationEventMask: 8 #(4607) 19776 1538 #caretPeriod: 8 #(530) 19776 1538 #wordWrap: 8 #(true) 19776 1538 #margins: 34 1 34 3 14818 1 19776 49 3 nil nil nil nil 14818 3 19776 1 nil nil 67108863 nil nil 14818 5 19776 1 nil nil nil nil nil 19776 1538 #canHScroll: 8 #(false) 19776 1538 #maxCompletionListHeight: 8 #(9) 19776 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 25 1 0 0] 8 #() 1728 nil 29 170 192 34 4 19472 8 'beforeTitle' 19776 8 'before' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 1 1442 1091 603 19376 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 33 2 0 0 45 1 0 0] 34 2 19776 19472 1728 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 18768 34 2 8 1140850688 1 21184 nil 14402 66047 nil 517 nil nil nil 21184 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1091 1 1442 11 603 21184 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 33 2 0 0 0 0 0 0 38 2 0 0 45 1 0 0] 8 #() 1728 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 18768 34 2 8 1140850688 131073 21424 nil 930 18720 nil 5 nil nil nil 21424 18738 1 1 nil 410 ##(Smalltalk.StaticText) 34 16 nil 21424 34 2 8 1140850945 262145 21520 nil nil nil 5 nil nil nil 21520 nil 8 1695245616 13762 nil nil nil 1474 138 144 34 2 1538 #createAt:extent: 34 2 1442 1 563 1442 1093 41 21520 1538 #text: 34 1 8 '
' 21520 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 1 0 0 34 2 0 0 45 1 0 0] 8 #() 1728 nil 27 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 21424 34 2 8 1140920580 1025 21824 13010 nil false 13056 nil 930 18720 nil 5 nil nil nil 21824 nil 8 4294906933 13762 nil nil 11 nil 170 192 34 12 #indentGuide 13826 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil #whitespace 13826 3 19968 nil 1 nil nil nil nil #whitespace nil nil nil #lineNumber 13826 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #callTip 13826 77 13888 13904 1 nil nil nil nil #callTip nil nil nil #normal 13826 1 nil nil 1 nil nil nil nil #normal nil nil nil #diff 13826 5 nil 14402 16908287 3 nil nil nil nil #diff nil 1401 nil 34 40 22048 22000 22064 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 22016 nil nil nil 21984 nil nil 20082 20112 170 192 34 2 #default 14018 1 nil nil nil 21824 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 880 nil 170 176 880 9215 nil nil nil nil 14080 nil nil 65 nil nil nil 20208 3 170 192 34 2 #container 21952 nil nil nil nil 1 nil 170 192 34 6 8 'indicator1' 14290 3 21824 33423361 5 false nil nil nil nil nil nil nil 8 'indicator0' 14290 1 21824 33554459 13 false nil nil nil nil nil nil nil 8 'indicator2' 14290 5 21824 511 1 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 13826 1029 nil 14402 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 13826 1027 14402 60239 14402 31326207 1 nil nil nil nil #Warning nil nil nil #Error 13826 1031 14402 335 14402 31317503 1 nil nil nil nil #Error nil nil nil nil 1474 138 144 34 7 1538 #createAt:extent: 34 2 1442 1 1 1442 1093 563 21824 1538 #modificationEventMask: 8 #(4607) 21824 1538 #caretPeriod: 8 #(530) 21824 1538 #wordWrap: 8 #(true) 21824 1538 #margins: 34 1 34 3 14818 1 21824 49 3 nil nil nil nil 14818 3 21824 1 nil nil 67108863 nil nil 14818 5 21824 1 nil nil nil nil nil 21824 1538 #canHScroll: 8 #(false) 21824 1538 #maxCompletionListHeight: 8 #(9) 21824 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 34 2 0 0 25 1 0 0] 8 #() 1728 nil 29 170 192 34 4 21520 8 'afterTitle' 21824 8 'after' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1101 1 1442 1093 603 21424 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 2 0 0 0 0 0 0 72 4 0 0 45 1 0 0] 34 2 21824 21520 1728 nil 27 1728 nil 27 170 192 34 2 18768 8 'diffs' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 13106 nil true 34 4 13154 1 13186 #selectUpper 8 'Select &upper' 1 1 nil nil nil 13154 1 13186 #selectLower 8 'Select &lower' 1 1 nil nil nil 13234 4097 13154 1 13186 #exit 8 '&Close' 1 1 nil nil nil 8 '&File' nil 1 nil nil 57207 nil nil 13106 nil true 34 3 13154 1 13186 #character 8 '&Character' 1 1 nil nil nil 13154 1 13186 #word 8 '&Word' 1 1 nil nil nil 13154 1 13186 #line 8 '&Line' 1 1 nil nil nil 8 '&Mode' nil 1 nil nil 57209 nil nil 13106 nil true 34 1 13154 1 13186 #about 8 '&About' 1 1 nil nil nil 8 '&Help' nil 134217729 nil nil 57211 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1474 138 144 34 3 1538 #createAt:extent: 34 2 1442 9 55 1442 2225 635 18640 1538 #text: 34 1 8 'Comparison' 18640 1538 #updateMenuBar 880 18640 1666 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 92 4 0 0 88 1 0 0] 34 1 18768 1728 nil 27 17776 170 192 34 12 17776 8 'projectInfo' 12944 8 'classDefinition' 12624 8 'sunit' 15040 8 'classDocumentation' 18640 8 'comparisonTab' 16336 8 'methodSource' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 12320 34 2 8 1140916736 1 24128 834 138 144 34 6 17632 12912 15008 16304 12464 18624 nil 912 nil nil 1 nil nil nil 24128 nil 8 4294905519 1154 0 nil 4930 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 24256 7 257 nil 1154 0 nil 4930 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 24304 7 257 nil 1056 nil nil nil nil nil #smallIcons nil nil nil nil nil 1474 138 144 34 3 1538 #createAt:extent: 34 2 1442 1 1 1442 2241 697 24128 1538 #basicSelectionsByIndex: 34 1 8 #(1) 24128 1538 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 24128 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 96 4 0 0 92 1 0 0] 8 #() 1728 nil 27 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 705 1442 2241 697 12320 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 96 1 0 0 96 4 0 0 188 2 0 0] 34 7 17776 12944 15040 16336 12624 18640 24128 1728 nil 27 1746 3 5 true 170 192 34 4 12320 8 'textAreaTabs' 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 24816 nil 930 7712 nil 517 nil nil nil 24816 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 1 695 1442 2241 11 24816 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 91 1 0 0 96 4 0 0 96 1 0 0] 8 #() 1728 nil 27 8 'splitter' nil 1474 138 144 34 1 1538 #createAt:extent: 34 2 1442 7679 21 1442 2241 1401 416 1666 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 95 19 0 0 198 2 0 0] 34 3 544 24816 12320 1728 nil 27 )! !
!JadeiteProjectsBrowserPresenter class categoriesFor: #downArrowIcon!public! !
!JadeiteProjectsBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

