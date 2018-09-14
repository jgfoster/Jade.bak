"Filed out from Dolphin Smalltalk 7"!

JadeiteAbstractTestCase subclass: #JadeiteProjectBrowserTestCase
	instanceVariableNames: 'transcriptWritesWereEnabled methodListBrowser'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteProjectBrowserTestCase guid: (GUID fromString: '{1d6db475-c257-442b-9f91-d1574b03aee2}')!
JadeiteProjectBrowserTestCase comment: 'General Jadeite tests. Assumes a logged in session and transcript present. '!
!JadeiteProjectBrowserTestCase categoriesForClass!Unclassified! !
!JadeiteProjectBrowserTestCase methodsFor!

disableTranscriptWrites
	(transcriptWritesWereEnabled := transcript isTranscriptInstalled)
		ifTrue: [transcript flipTranscript]	"turn off gemstone transcript writes for performance"!

issue170methodCreation
	| source |
	self methodSourcePresenter value: (source := 'another  ^5').
	self projectsPresenter editSaveMethod: self classListPresenter selection.
	self methodSourcePresenter value: (source := 'interesting  ^5').
	self projectsPresenter editSaveMethod: self classListPresenter selection.
	self methodSourcePresenter value: (source := 'test  ^5').
	self projectsPresenter editSaveMethod: self classListPresenter selection.
	self methodSourcePresenter value: (source := 'try  ^5').
	self projectsPresenter editSaveMethod: self classListPresenter selection!

packageNamed: aString
	^self packageListPresenter list
		detect: [:projectService | projectService name = aString]
		ifNone: []!

projectList
	^self projectListPresenter list!

projectNamed: aString
	^self projectsPresenter projectListPresenter list
		detect: [:projectService | projectService name = aString]
		ifNone: []!

rowanProjectNames
	"this may change"

	^#('Cypress' 'Rowan' 'STON' 'Tonel')!

saveHooMethod

	| source |
	source := 'hoo
	"RowanSample1 new hoo"
	^self goo'. 
	self methodSourcePresenter value: source. 
	self projectsPresenter editSaveMethod: self classListPresenter selection. 

	!

saveSampleMethod

	| source |
	source := 'sampleMethod  
		aaa := 123. 
		^aaa'. 
	self methodSourcePresenter value: source. 
	self projectsPresenter editSaveMethod: self classListPresenter selection. 

	!

selectClassInHierarchNamed: className
	self selectTreeServicesNamed: (Array with: className) in: self classHierarchyPresenter!

selectClassListTab
	^self projectsPresenter classHierarchyTabs firstCard!

selectClassTab
	self selectClassTabIn: self projectsPresenter!

selectClassTabIn: presenter
	presenter instanceClassTabs lastCard!

selectHierarchyTab
	self selectHierarchyTabIn: self projectsPresenter!

selectHierarchyTabIn: presenter
	presenter classHierarchyTabs lastCard!

selectTreeServicesNamed: anArray in: presenter
	presenter resetSelection.
	anArray do: 
			[:name |
			| service |
			service := presenter model asBag detect: [:svc | svc name = name]
						ifNone: [self error: 'Tree element ' , name , ' not found'].
			presenter selections: (presenter selections asOrderedCollection
						add: service;
						yourself)]!

setUp
	super setUp.
	self disableTranscriptWrites.	"turn off gemstone transcript writes for performance"
	self openProjectsBrowser.
	self abortTransaction.	"ensure we don't have unsaved changes in the images"
	self ensureRowanSample1Available. "load sample project after abort  - especially until https://github.com/GemTalk/Rowan/issues/349 is fixed."
	self commitTransaction. "take this out after #349 is fixed"!

tearDown
	super tearDown.
	self abortTransaction. 
	self unloadSampleProject.
	self commitTransaction. "can remove this when #349 is fixed. See setUp" 
	projectsBrowser ifNotNil: [projectsBrowser destroy].
	methodListBrowser ifNotNil: [methodListBrowser destroy].
	transcriptWritesWereEnabled ifTrue: [transcript flipTranscript].
!

test_addPackageReloadProject
	self testsIssue: #issue183
		withTitle: 'Add package but don''t write it to disk then reload - walkback'.
	self selectServicesNamed: (Array with: 'RowanSample1') in: self projectListPresenter.
	self assertIsNil: (self packageNamed: 'NewPackage').
	self projectListPresenter selection addPackageNamed: 'NewPackage' using: self projectsPresenter.
	self denyIsNil: (self packageNamed: 'NewPackage').
	TestMessageBox enableJadeiteTestMessageBox.
	
	[TestMessageBox plannedResult: #yes.	"confirm we want to reload project"
	self projectsPresenter loadProjects.
	self assertIsNil: (self packageNamed: 'NewPackage')]
			ensure: [TestMessageBox disableJadeiteTestMessageBox]!

test_addRemoveCategory
	self selectRowanSample1Class.
	self deny: (self categoryListPresenter list includes: 'jadeite test').
	self projectsPresenter basicAddMethodCategory: 'jadeite test'.
	self assert: (self categoryListPresenter list includes: 'jadeite test').
	self assert: self categoryListPresenter selection = 'jadeite test'.
	self projectsPresenter basicRemoveMethodCategories.
	self deny: (self categoryListPresenter list includes: 'jadeite test')!

test_autoDirtyDisplayMoveMethodsToCategory
	self testsIssue: #issue16 withTitle: 'Review/revisit semantics of font color/italic/bold etc.'.
	self selectServicesNamed: #('RowanSample1') in: self projectListPresenter.
	self selectServicesNamed: #('RowanSample1-Core') in: self packageListPresenter.
	self deny: self projectListPresenter selection isDirty.
	self deny: self packageListPresenter selection isDirty.
	self selectRowanSample1Class.
	self projectsPresenter basicAddMethodCategory: 'newCategory'. 
	self selectCategoriesNamed: #('accessing').
	self selectMethodsNamed: #(#foo).
	self projectsPresenter moveMethods: self methodListPresenter selections toCategory: 'newCategory'. 
	self assert: self projectListPresenter selection name = 'RowanSample1'.
	self assert: self projectListPresenter selection isDirty.
	self assert: self packageListPresenter selection name = 'RowanSample1-Core'.
	self assert: self packageListPresenter selection isDirty.
	"verify method moved" 
	self selectCategoriesNamed: #('newCategory'). 
	self selectMethodsNamed: #(#foo). 
	self assert: self methodListPresenter selection selector = #foo. !

test_autoDirtyDisplayRemoveMethod
	self testsIssue: #issue16 withTitle: 'Review/revisit semantics of font color/italic/bold etc.'.
	self selectServicesNamed: #('RowanSample1') in: self projectListPresenter.
	self selectServicesNamed: #('RowanSample1-Core') in: self packageListPresenter.
	self deny: self projectListPresenter selection isDirty.
	self deny: self packageListPresenter selection isDirty.
	self selectRowanSample1Class.
	self selectCategoriesNamed: #('accessing').
	self selectMethodsNamed: #(#foo).
	self projectsPresenter removeMethods: self classListPresenter selections.
	self assert: self projectListPresenter selection name = 'RowanSample1'.
	self assert: self projectListPresenter selection isDirty.
	self assert: self packageListPresenter selection name = 'RowanSample1-Core'.
	self assert: self packageListPresenter selection isDirty!

test_classListNotBlank
	| rowanClass |
	self selectServicesNamed: (Array with: 'Rowan') in: self projectListPresenter.
	self selectServicesNamed: #('Rowan-Services-Core') in: self packageListPresenter.
	self selectServicesNamed: (Array with: 'RowanClassService') in: self classListPresenter.
	self selectHierarchyTabIn: self projectsPresenter.
	self assert: self projectsPresenter classHierarchyPresenter selection name = 'RowanClassService'.
	self selectServicesNamed: #('Rowan-Kernel') in: self packageListPresenter.
	self selectClassListTab.
	self assert: self projectsPresenter classListPresenter list notEmpty. 
	rowanClass := self projectsPresenter classListPresenter list detect:[:classService | classService name = 'Rowan'] ifNone:[].
	self denyIsNil: rowanClass
	
	!

test_classSideSelection
	self selectServicesNamed: (Array with: 'Rowan') in: self projectListPresenter.
	self selectServicesNamed: #('Rowan-Services-Core') in: self packageListPresenter.
	self selectServicesNamed: (Array with: 'RowanService') in: self classListPresenter.
	"select methods in class list view"
	self selectMethodsNamed: (Array with: #stonStringFor:).	"pick some methods that hopefully won't go away someday"
	self assert: self methodListPresenter selection selector == #stonStringFor:.
	self selectMethodsNamed: (Array with: #stonOn:).
	self assert: self methodListPresenter selection selector == #stonOn:.
	self selectMethodsNamed: (Array with: #command:).
	self assert: self methodListPresenter selection selector == #command:.
	"see same methods in hierarchy view"
	self selectHierarchyTab.
	self selectMethodsNamed: (Array with: #stonStringFor:).
	self assert: self methodListPresenter selection selector == #stonStringFor:.
	self selectMethodsNamed: (Array with: #stonOn:).
	self assert: self methodListPresenter selection selector == #stonOn:.
	self selectMethodsNamed: (Array with: #command:).
	self assert: self methodListPresenter selection selector == #command:.
	"see methods in class list view on class side"
	self selectClassListTab.
	self selectClassTab.
	self selectMethodsNamed: (Array with: #new).
	self assert: self methodListPresenter selection selector == #new.
	self methodListPresenter resetSelection.
	self assertIsNil: (self methodListPresenter list
				detect: [:methodService | methodService selector = #stonStringFor:]
				ifNone: []).
	self selectHierarchyTab.
	self selectMethodsNamed: (Array with: #new).
	self assert: self methodListPresenter selection selector == #new.
	self assertIsNil: (self methodListPresenter list
				detect: [:methodService | methodService selector = #stonStringFor:]
				ifNone: [])!

test_extensionColoringInHierarchy
	| testClass |
	self selectServicesNamed: (Array with: 'Rowan') in: self projectListPresenter.
	self selectServicesNamed: #('Rowan-Specifications') in: self packageListPresenter.
	self selectServicesNamed: (Array with: 'RwPlatformSpecification') in: self classListPresenter.
	testClass := self classListPresenter list
				detect: [:classService | classService name = 'RwGemStoneSpecification']
				ifNone: [].
	self assertIsNil: testClass.	"Our guy is an extension"
	self selectHierarchyTab.
	self
		assert: self projectsPresenter classHierarchyPresenter selection name = 'RwPlatformSpecification'.
	self deny: self projectsPresenter classHierarchyPresenter selection shouldColorAsExtension.
	self selectClassInHierarchNamed: 'RwGemStoneSpecification'.
	self assert: self projectsPresenter classHierarchyPresenter selection shouldColorAsExtension!

test_findClass
	self projectsPresenter updateAfterFindClass: (RowanClassService new name: 'RwGsPlatform').
	self assert: self classListPresenter selection name equals: 'RwGsPlatform'.
	self assert: self classListPresenter selection definedPackageName equals: 'Rowan-GemStone-Core'.
	self assert: self packageListPresenter selection name equals: 'Rowan-GemStone-Core'.
	self assert: self projectListPresenter selection name equals: 'Rowan'.
	self projectsPresenter updateAfterFindClass: (RowanClassService new name: 'CypressClassDefinition').
	self assert: self classListPresenter selection name equals: 'CypressClassDefinition'.
	self assert: self classListPresenter selection definedPackageName equals: 'Cypress-Definitions'.
	self assert: self packageListPresenter selection name equals: 'Cypress-Definitions'.
	self assert: self projectListPresenter selection name equals: 'Cypress'!

test_goToDefinedClassFromClassList
	self selectServicesNamed: (Array with: 'Rowan') in: self projectListPresenter.
	self selectServicesNamed: #('Rowan-Services-Extensions') in: self packageListPresenter.
	self selectServicesNamed: (Array with: 'RwGsPlatform') in: self classListPresenter.
	self assert: self projectsPresenter isClassListTabSelected.
	self projectsPresenter gotoClassDefinedPackage.
	self assert: self classListPresenter selection name = 'RwGsPlatform'.
	self assert: self packageListPresenter selection name = 'Rowan-GemStone-Core'!

test_goToDefinedClassFromHierarchy
	| gotoClass |
	self selectServicesNamed: (Array with: 'Rowan') in: self projectListPresenter.
	self selectServicesNamed: #('Rowan-Specifications') in: self packageListPresenter.
	self selectServicesNamed: (Array with: 'RwPlatformSpecification') in: self classListPresenter.
	gotoClass := self classListPresenter list
				detect: [:classService | classService name = 'RwGemStoneSpecification']
				ifNone: [].
	self assertIsNil: gotoClass.	"Our guy is an extension"
	self selectHierarchyTab.
	self deny: self projectsPresenter isClassListTabSelected.
	self selectClassInHierarchNamed: 'RwGemStoneSpecification'.
	self projectsPresenter gotoClassDefinedPackage.
	self assert: self packageListPresenter selection name = 'Rowan-GemStone-Specifications'!

test_goToDefinedMethodFromClassList
	self selectServiceNamed: 'Rowan' in: self projectListPresenter.
	self selectServiceNamed: 'Rowan-Services-Extensions' in: self packageListPresenter.
	self selectServiceNamed: 'RwGsPlatform' in: self classListPresenter.
	self selectCategoryNamed: '*rowan-services-extensions'.
	self selectMethodNamed: #classServiceClass.
	self assert: self projectsPresenter isClassListTabSelected.
	self projectsPresenter gotoMethodPackage.
	self assert: self packageListPresenter selection name = 'Rowan-Services-Extensions'.
	self assert: self classListPresenter selection name = 'RwGsPlatform'.
	self assert: self categoryListPresenter selection = '*rowan-services-extensions'.
	self assert: self methodListPresenter selection selector = #classServiceClass.
	self assert: self projectsPresenter isClassListTabSelected!

test_goToDefinedMethodFromClassListClassSide
	self selectServiceNamed: 'Rowan' in: self projectListPresenter.
	self selectServiceNamed: 'Rowan-GemStone-Loader-Extensions' in: self packageListPresenter.
	self selectServiceNamed: 'RwGsImage' in: self classListPresenter.
	self selectClassTab.
	self selectCategoryNamed: '*rowan-gemstone-loader-extensions'.
	self selectMethodNamed: #_shouldCloneRowanLoader:.
	self assert: self projectsPresenter isClassListTabSelected.
	self projectsPresenter gotoMethodPackage.
	self assert: self packageListPresenter selection name = 'Rowan-GemStone-Loader-Extensions'.
	self assert: self classListPresenter selection name = 'RwGsImage'.
	self assert: self categoryListPresenter selection = '*rowan-gemstone-loader-extensions'.
	self assert: self methodListPresenter selection selector = #_shouldCloneRowanLoader:.
	self assert: self projectsPresenter isClassListTabSelected. 
	self assert: self projectsPresenter isClassSideSelected.!

test_goToDefinedMethodFromHierarchy
	self selectServiceNamed: 'Rowan' in: self projectListPresenter.
	self selectServiceNamed: 'Rowan-Services-Extensions' in: self packageListPresenter.
	self selectServiceNamed: 'RwGsPlatform' in: self classListPresenter.
	self selectHierarchyTab.
	self selectCategoryNamed: '*rowan-services-extensions'.
	self selectMethodNamed: #classServiceClass.
	self deny: self projectsPresenter isClassListTabSelected.
	self projectsPresenter gotoMethodPackage.
	self assert: self packageListPresenter selection name = 'Rowan-Services-Extensions'.
	self assert: self classListPresenter selection name = 'RwGsPlatform'.
	self assert: self categoryListPresenter selection = '*rowan-services-extensions'.
	self assert: self methodListPresenter selection selector = #classServiceClass.
	self assert: self projectsPresenter isClassListTabSelected "make my life easier by forcing the class side to be seleted"!

test_gotoDefinedMethodNotSameAsClass
	self selectServiceNamed: 'Rowan' in: self projectListPresenter.
	self selectServiceNamed: 'Rowan-Definitions' in: self packageListPresenter.
	self selectServiceNamed: 'RwClassDefinition' in: self classListPresenter.
	self selectCategoryNamed: '*rowan-core-definitions-extensions'.
	self selectMethodNamed: #_methodsModificationClass.
	self projectsPresenter gotoMethodPackage.
	self assert: self packageListPresenter selection name = 'Rowan-Core-Definitions-Extensions'.
	self assert: self classListPresenter selection name = 'RwClassDefinition'.
	self assert: self categoryListPresenter selection = '*rowan-core-definitions-extensions'.
	self assert: self methodListPresenter selection selector = #_methodsModificationClass!

test_gotoDefinedPackage
	| dictionaryPresenter |
	self testsIssue: #issue150 withTitle: 'Go to defining package -> Not found: a BlockClosure'.
	"different project within projects browser"
	self selectServiceNamed: 'Rowan' in: self projectListPresenter.
	self selectServiceNamed: 'Rowan-Tools-Kernel' in: self packageListPresenter.
	self selectServiceNamed: 'CypressDefinition' in: self classListPresenter.
	self projectsPresenter gotoClassDefinedPackage.
	self assert: self projectListPresenter selection name = 'Cypress'.
	self assert: self packageListPresenter selection name = 'Cypress-Definitions'.
	self assert: self classListPresenter selection name = 'CypressDefinition'.

	"go to dictionary browser"
	self selectServiceNamed: 'Rowan' in: self projectListPresenter.
	self selectServiceNamed: 'Rowan-GemStone-Kernel' in: self packageListPresenter.
	self selectServiceNamed: 'ByteArray' in: self classListPresenter.
	dictionaryPresenter := self projectsPresenter gotoClassDefinedPackage.
	
	[self assert: dictionaryPresenter dictionaryListPresenter selection key = 'Globals'.
	self assert: dictionaryPresenter classCategoryPresenter selection value = 'Kernel'.
	self assert: dictionaryPresenter classListPresenter selection = 'ByteArray']
			ensure: [dictionaryPresenter topShell destroy]!

test_hierarchySelectTwoBrowsers
	| secondBrowser firstBrowser |
	self testsIssue: #issue122 withTitle: 'Hierarchy tab selection updates incorrectly.'.
	firstBrowser := projectsBrowser.	"for easier reading"
	secondBrowser := self openWindow: [transcript openJadeiteProjectsBrowser].
	
	[self selectRowanSample1Class.
	self selectServicesNamed: (Array with: 'RowanSample1')
		in: secondBrowser currentCard projectListPresenter.
	self selectServicesNamed: #('RowanSample1-Tests') in: secondBrowser currentCard packageListPresenter.
	self selectServicesNamed: (Array with: 'RowanSample1Test')
		in: secondBrowser currentCard classListPresenter.
	self selectHierarchyTabIn: firstBrowser currentCard.
	self assert: firstBrowser currentCard classHierarchyPresenter selection name = 'RowanSample1'.
	self selectHierarchyTabIn: secondBrowser currentCard.
	self assert: firstBrowser currentCard classHierarchyPresenter selection name = 'RowanSample1'.
	self assert: (firstBrowser currentCard classHierarchyPresenter isKindOf: TreePresenter).
	self assert: secondBrowser currentCard classHierarchyPresenter selection name = 'RowanSample1Test'.
	self assert: (secondBrowser currentCard classHierarchyPresenter isKindOf: TreePresenter)]
			ensure: [secondBrowser destroy]!

test_makeDefault
	| nonDefaultService |
	self selectServicesNamed: (Array with: 'RowanSample1') in: self projectListPresenter.
	self projectsPresenter makeDefault.
	self assert: self projectListPresenter selection displayName = 'RowanSample1*'.
	nonDefaultService := self projectListPresenter list
				detect: [:projectService | projectService name = 'Rowan'].
	self assert: nonDefaultService displayName = 'Rowan'.
	self assert: self projectListPresenter selection displayName = 'RowanSample1*'.
	self selectServicesNamed: (Array with: 'Rowan') in: self projectListPresenter.
	self projectsPresenter makeDefault.
	nonDefaultService := self projectListPresenter list
				detect: [:projectService | projectService name = 'RowanSample1'].
	self assert: self projectListPresenter selection displayName = 'Rowan*'!

test_menuAddPackageOnProjectMenu
	| transcriptProjectMenu transcriptCommands |
	self testsIssue: #issue171 withTitle: 'Project menu quibbles'.
	self assert: ((self projectsPresenter projectMenu items collect: [:each | each command])
				includes: #addPackage).	"on projects menu in browser"
	transcriptProjectMenu := transcript projectListPresenter projectListPresenter view parentView
				contextMenu.
	transcriptCommands := transcriptProjectMenu items collect: [:each | each command].
	self deny: (transcriptCommands includes: #addPackage)!

test_menuEnablementDoesNotBreak
	"the bug in the issue was a bad enablement test in a menu item
	during the query. If nothing else, test that we can always query
	all the menus in the toolbar without error regardless if a class is 
	selected or not."

	| view |
	self testsIssue: #issue172 withTitle: 'Classes menu error if no Class selected'.
	view := projectsBrowser view.
	self selectRowanSample1Class.
	self denyIsNil: self classListPresenter selectionOrNil.
	view menuBar items do: [:menu | menu queryAllFromView: view].
	self selectServiceNamed: 'Cypress' in: self projectListPresenter.	"this will deselect the class."
	self assertIsNil: self classListPresenter selectionOrNil.
	view menuBar items do: [:menu | menu queryAllFromView: view]!

test_menusDontAddBreaks
	"incomplete test - make sure we don't add a breakpoint menu item in 
	the menu bar"
	| menuBar jadeiteMenu methodsMenu |
	self testsIssue: #issue162 withTitle: 'In method list browser, get new breakpoint menu items'.
	menuBar := projectsBrowser view menuBar.
	jadeiteMenu := self menuNamed: 'Jadeite' in: menuBar.
	methodsMenu := self menuNamed: 'Methods' in: menuBar.
	self noBreakIn: jadeiteMenu.
	self noBreakIn: methodsMenu.
	self selectRowanSample1Class.
	self saveInterestingLoopMethod.
	self selectMethodNamed: #interestingLoop.
	self methodListPresenter selection appendToSourceMenu: jadeiteMenu
		presenter: self methodSourcePresenter view.
	self noBreakIn: jadeiteMenu.
	self methodListPresenter selection appendToSourceMenu: methodsMenu
		presenter: self methodSourcePresenter view.
	self noBreakIn: methodsMenu!

test_multipleCompileMethod
	self testsIssue: #issue170
		withTitle: 'Editing methods to create new method and updating created method gets errors'.
	self selectRowanSample1Class.
	self issue170methodCreation.
	self selectMethodNamed: #test.
	self methodSourcePresenter value: 'pest  ^55'.
	self projectsPresenter editSaveMethod: self classListPresenter selection.
	"run through the list of methods"
	self selectMethodNamed: #another.
	self selectMethodNamed: #interesting.
	self selectMethodNamed: #test.
	self selectMethodNamed: #try.
	self selectMethodNamed: #test.
	self selectMethodNamed: #interesting.
	self selectMethodNamed: #another.
	self selectMethodNamed: #pest.
	self assert: self methodSourcePresenter value equals: 'pest  ^55'.
	self methodSourcePresenter value: 'pest  ^55 + 1'.
	self projectsPresenter editSaveMethod: self classListPresenter selection.
	self selectMethodNamed: #another.
	self selectMethodNamed: #interesting.
	self selectMethodNamed: #test.
	self selectMethodNamed: #try.
	self selectMethodNamed: #test.
	self selectMethodNamed: #interesting.
	self selectMethodNamed: #another.
	self selectMethodNamed: #pest.	"<< bug occurred here"
	self assert: self methodSourcePresenter value equals: 'pest  ^55 + 1'!

test_noSelectionHierarchy
	| roots children childrensNames grandchildren grandchildrensNames |
	self testsIssue: #issue125 withTitle: 'Hierarchy view empty if class not selected'.
	self selectServicesNamed: (Array with: 'Rowan') in: self projectListPresenter.
	self selectServicesNamed: #('Rowan-Services-Core') in: self packageListPresenter.
	self assertIsNil: self projectsPresenter selectedClass.
	self selectHierarchyTab.
	self assert: self classHierarchyPresenter model notEmpty.
	roots := self classHierarchyPresenter model roots.
	self assert: roots size = 1.
	self assert: roots first name = 'Object'.
	children := self classHierarchyPresenter model getChildrenOf: roots first.
	childrensNames := children collect: [:classService | classService name].
	self assert: (childrensNames includes: 'RowanCommandResult').
	self assert: (childrensNames includes: 'RowanService').
	self deny: (childrensNames includes: 'RowanClassService').
	grandchildren := self classHierarchyPresenter model getChildrenOf: (children detect: [:classService | classService name = 'RowanService']).
	self assert: grandchildren size = 11. "this will probably change but it's a nice test to ensure all the expected services are present"
	grandchildrensNames := grandchildren collect: [:classService | classService name].
	self assert: (grandchildrensNames at: 1) equals: 'RowanAnsweringService'.
	self assert: (grandchildrensNames at: 2) equals: 'RowanBrowserService'.
	self assert: (grandchildrensNames at: 3) equals: 'RowanClassService'.
	self assert: (grandchildrensNames at: 4) equals: 'RowanDebuggerService'.
	self assert: (grandchildrensNames at: 5) equals: 'RowanFrameService'.
	self assert: (grandchildrensNames at: 6) equals: 'RowanMethodService'.
	self assert: (grandchildrensNames at: 7) equals: 'RowanPackageService'.
	self assert: (grandchildrensNames at: 8) equals: 'RowanProcessService'.
	self assert: (grandchildrensNames at: 9) equals: 'RowanProjectService'.
	self assert: (grandchildrensNames at: 10) equals: 'RowanQueryService'.
	self assert: (grandchildrensNames at: 11) equals: 'RowanVariableService'.

!

test_openProjectsBrowser
	"project browser opened in setup"

	self assert: projectsBrowser class equals: JadeiteProjectsSystemBrowser.
	self
		denyIsNil: (self projectList detect: [:projectService | projectService name = 'Rowan'] ifNone: [])!

test_packagesShownInMultipleProjects
	self selectServicesNamed: (Array with: 'Rowan') in: self projectListPresenter.
	self assertIsNil: (self serviceNamed: 'RowanService' in: self classListPresenter).
	self selectServicesNamed: #('Rowan-Services-Core') in: self packageListPresenter.
	self denyIsNil: (self serviceNamed: 'RowanService' in: self classListPresenter).
	self assertIsNil: (self serviceNamed: 'JadeServer' in: self classListPresenter).
	self selectServicesNamed: #('Rowan-JadeServer') in: self packageListPresenter.
	self denyIsNil: (self serviceNamed: 'JadeServer' in: self classListPresenter)!

test_projectDisplaysSync
	"the projects browser and project list in the transcript should
	stay sync'ed automaticall"

	"project removal in projects browser"

	self denyIsNil: (self projectNamed: 'RowanSample1').
	self assert: self transcriptTab isKindOf: RowanProjectListPresenter.
	self denyIsNil: (self transcriptProjectNamed: 'RowanSample1').
	self unloadSampleProject.
	self assertIsNil: (self projectNamed: 'RowanSample1').
	self assertIsNil: (self transcriptProjectNamed: 'RowanSample1').
	"project addition in projects browser"
	self ensureRowanSample1Available.
	
	[self denyIsNil: (self transcriptProjectNamed: 'RowanSample1').
	self denyIsNil: (self projectNamed: 'RowanSample1')]
			ensure: [self unloadSampleProject].
	self assertIsNil: (self projectNamed: 'RowanSample1').
	self assertIsNil: (self transcriptProjectNamed: 'RowanSample1').
	"project addition in project list"
	self loadRowanSample1InProjectList.
	self denyIsNil: (self transcriptProjectNamed: 'RowanSample1').
	self denyIsNil: (self projectNamed: 'RowanSample1').
	"project removal in project list"
	(self transcriptTab projectListPresenter list
		detect: [:projectService | projectService name = self sampleProjectName]
		ifNone: []) notNil
		ifTrue: [self transcriptTab basicUnloadProjects: (Array with: self sampleProjectName)].
	self assertIsNil: (self projectNamed: 'RowanSample1').
	self assertIsNil: (self transcriptProjectNamed: 'RowanSample1')!

test_projectRemoved
	"Gracefully remove a project from the project list if it was 
	removed behind the tool's back"

	| sampleProject |
	self selectServicesNamed: (Array with: 'RowanSample1') in: self projectListPresenter.
	sampleProject := self projectNamed: 'RowanSample1'.
	self denyIsNil: sampleProject.
	self selectServicesNamed: (Array with: 'Rowan') in: self projectListPresenter.
	session executeString: 'Rowan projectTools delete deleteProjectNamed: ''RowanSample1'''.	"delete project behind tool's back"
	self selectServicesNamed: (Array with: 'RowanSample1') in: self projectListPresenter.	"expect project to disappear but not walkback"
	sampleProject := self projectNamed: 'RowanSample1'.
	self assertIsNil: sampleProject!

test_removeProject
	| targetProject |
	self testsIssue: #issue129 withTitle: 'Expose unload project.'.
	targetProject := self projectListPresenter list
				detect: [:projectService | projectService name = self sampleProjectName]
				ifNone: [].
	self denyIsNil: targetProject.
	self projectListPresenter selection: targetProject.
	"just select an element in each list to test cleanup after removal"
	self packageListPresenter selection: self packageListPresenter list first.
	self classListPresenter selection: self classListPresenter list first.
	self categoryListPresenter selection: self categoryListPresenter list first.
	self methodListPresenter selection: self methodListPresenter list first.
	self projectsPresenter basicUnloadProjects: (Array with: self projectListPresenter selection name).
	targetProject := self projectListPresenter list
				detect: [:projectService | projectService name = self sampleProjectName]
				ifNone: [].
	self assertIsNil: targetProject.
	"make sure browser cleans itself up"
	self assert: self projectListPresenter selections isEmpty.
	self assert: self packageListPresenter list isEmpty.
	self assert: self classListPresenter list isEmpty.
	self assert: self categoryListPresenter list isEmpty.
	self assert: self methodListPresenter list isEmpty!

test_saveClassMethod
	| source |
	self selectRowanSample1Class.
	self selectClassTab.
	self projectsPresenter basicAddMethodCategory: 'class side category'.
	self assert: self categoryListPresenter selection = 'class side category'.
	source := 'classSideMethod  ^#classSide'.
	self methodSourcePresenter value: source.
	self projectsPresenter editSaveMethod: self classListPresenter selection.
	self selectServicesNamed: (Array with: 'classSideMethod') in: self methodListPresenter.
	self assert: self methodListPresenter selection selector = #classSideMethod.
	self assert: (session executeString: 'RowanSample1 classSideMethod') = #classSide!

test_saveClassMethodHierarchy
	| source |
	self testsIssue: #issue121 withTitle: 'Package Browser (3.0.20): hierarchy class view and classes defined in different pacakge oddity'.
	self selectRowanSample1Class.
	self assert: self classListPresenter selection name = 'RowanSample1'. 
	self selectHierarchyTab.
	self selectClassTab.
	self projectsPresenter basicAddMethodCategory: 'class side category'.
	self assert: self categoryListPresenter selection = 'class side category'.
	source := 'classSideMethod  ^''on the class side'''.
	self methodSourcePresenter value: source.
	self projectsPresenter editSaveMethod: self projectsPresenter classHierarchyPresenter selection.
	self selectServicesNamed: (Array with: 'classSideMethod') in: self methodListPresenter.
	self assert: self methodListPresenter selection selector = #classSideMethod.
	self assert: (session executeString: 'RowanSample1 classSideMethod') = 'on the class side'!

test_saveMethod
	self selectRowanSample1Class.
	(self categoryListPresenter list includes: 'test_saveMethod')
		ifFalse: [self projectsPresenter basicAddMethodCategory: 'test_saveMethod'].
	self saveSampleMethod.
	self selectServicesNamed: (Array with: 'sampleMethod') in: self methodListPresenter.
	self assert: (session executeString: 'RowanSample1 new sampleMethod') = 123!

test_saveMethodHierarchy
	| source |
	self testsIssue: #issue121 withTitle: 'Package Browser (3.0.20): hierarchy class view and classes defined in different pacakge oddity'.
	self selectRowanSample1Class.
	self selectHierarchyTab.
	self selectCategoriesNamed: #('accessing').
	source := 'sampleMethod  
		aaa := 123. 
		^aaa'.
	self methodSourcePresenter value: source.
	self projectsPresenter editSaveMethod: self projectsPresenter classHierarchyPresenter selection.
	self selectServicesNamed: (Array with: 'sampleMethod') in: self methodListPresenter.
	self assert: self methodListPresenter selection selector = #sampleMethod.
	self assert: (session executeString: 'RowanSample1 new sampleMethod') = 123!

test_selectMultiplePackages
	self selectServicesNamed: #() in: self projectListPresenter .
	self assert: self packageListPresenter list isEmpty.
	self selectServicesNamed: #('Rowan') in: self projectListPresenter.
	self deny: self packageListPresenter list isEmpty.
	self serviceNamed: 'Rowan-Services-Core' in: self packageListPresenter.
	self serviceNamed: 'Rowan-JadeServer' in: self packageListPresenter.
	self selectServicesNamed: #('Rowan-Services-Core' 'Rowan-JadeServer') in: self packageListPresenter!

test_selectMultipleProjects
	self selectServicesNamed: #() in: self projectListPresenter .
	self assert: self projectListPresenter selections isEmpty.
	self selectServicesNamed: self rowanProjectNames in: self projectListPresenter.
	self assert: self projectListPresenter selections size = 4!

test_setAndClearBreakpoint
	" at least set breakpoints in the projects browser and method list browser for now"

	| methodListPresenter |
	self selectRowanSample1Class.
	self saveHooMethod.
	self assert: self methodListPresenter selection breakPoints size equals: 0.
	self projectsPresenter setBreakPointAt: 2 in: self projectsPresenter.
	self assert: self methodListPresenter selection breakPoints size equals: 1.
	self assert: self methodListPresenter selection breakPoints first equals: 2.
	self projectsPresenter clearBreakPointAt: 2 in: self projectsPresenter.
	self assert: self methodListPresenter selection breakPoints size equals: 0.
	"set breaks in method list browser"
	methodListBrowser := self projectsPresenter browseImplementorsOf: #hoo.
	self assert: (methodListBrowser isKindOf: JadeiteMethodListBrowser).
	methodListPresenter := methodListBrowser primaryPresenter.
	self assert: methodListPresenter methodListPresenter selection breakPoints size equals: 0.
	methodListPresenter setBreakPointAt: 2 in: methodListPresenter.
	self assert: methodListPresenter methodListPresenter selection breakPoints size equals: 1.
	self assert: methodListPresenter methodListPresenter selection breakPoints first equals: 2.
	methodListPresenter clearBreakPointAt: 2 in: methodListPresenter.
	self assert: methodListPresenter methodListPresenter selection breakPoints size equals: 0!

test_simpleHierarchyTest
	"just make sure that we can select different classes and the hierarchy browser updates"

	self selectServicesNamed: (Array with: 'Rowan') in: self projectListPresenter.
	self selectServicesNamed: #('Rowan-Services-Core') in: self packageListPresenter.
	self selectServicesNamed: (Array with: 'RowanService') in: self classListPresenter.
	self selectHierarchyTab.
	self assert: self projectsPresenter classHierarchyPresenter selection name = 'RowanService'.
	self selectClassListTab.
	self assert: self projectsPresenter classListPresenter selection name = 'RowanService'.
	self selectServicesNamed: (Array with: 'RowanProjectService') in: self classListPresenter.
	self selectHierarchyTab.
	self assert: self projectsPresenter classHierarchyPresenter selection name = 'RowanProjectService'.
	self selectClassListTab.
	self assert: self projectsPresenter classListPresenter selection name = 'RowanProjectService'!

test_SUnitTabHandlesShortSelector
	self testsIssue: #issue173 withTitle: 'new SUnit browser code walksback on short selector'.
	self selectServicesNamed: (Array with: 'Rowan') in: self projectListPresenter.
	self selectServicesNamed: #('Rowan-Services-Tests') in: self packageListPresenter.
	self selectServiceNamed: 'RwRowanProjectIssuesTest' in: self classListPresenter.
	self selectHierarchyTab.
	self selectClassInHierarchNamed: 'TestCase'.	"<<< bug occurred here"
	self assert: self projectsPresenter selectedClass name = 'TestCase'!

waitForPresenter: presenter
	[presenter list isEmpty] whileTrue: [(Delay forMilliseconds: 50) wait]! !
!JadeiteProjectBrowserTestCase categoriesFor: #disableTranscriptWrites!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #issue170methodCreation!issue test support!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #packageNamed:!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectList!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectNamed:!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #rowanProjectNames!constants!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #saveHooMethod!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #saveSampleMethod!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectClassInHierarchNamed:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectClassListTab!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectClassTab!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectClassTabIn:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectHierarchyTab!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectHierarchyTabIn:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectTreeServicesNamed:in:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #setUp!private!setup teardown! !
!JadeiteProjectBrowserTestCase categoriesFor: #tearDown!private!setup teardown! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_addPackageReloadProject!public!test projects! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_addRemoveCategory!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_autoDirtyDisplayMoveMethodsToCategory!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_autoDirtyDisplayRemoveMethod!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_classListNotBlank!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_classSideSelection!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_extensionColoringInHierarchy!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_findClass!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_goToDefinedClassFromClassList!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_goToDefinedClassFromHierarchy!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_goToDefinedMethodFromClassList!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_goToDefinedMethodFromClassListClassSide!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_goToDefinedMethodFromHierarchy!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_gotoDefinedMethodNotSameAsClass!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_gotoDefinedPackage!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_hierarchySelectTwoBrowsers!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_makeDefault!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_menuAddPackageOnProjectMenu!public!test!test projects! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_menuEnablementDoesNotBreak!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_menusDontAddBreaks!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_multipleCompileMethod!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_noSelectionHierarchy!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_openProjectsBrowser!public!test projects! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_packagesShownInMultipleProjects!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_projectDisplaysSync!public!test projects! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_projectRemoved!public!test projects! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_removeProject!public!test projects! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_saveClassMethod!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_saveClassMethodHierarchy!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_saveMethod!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_saveMethodHierarchy!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_selectMultiplePackages!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_selectMultipleProjects!public!test projects! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_setAndClearBreakpoint!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_simpleHierarchyTest!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_SUnitTabHandlesShortSelector!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #waitForPresenter:!private!selection! !

!JadeiteProjectBrowserTestCase class methodsFor!

resources
	^IdentitySet with: JadeiteTestResource! !
!JadeiteProjectBrowserTestCase class categoriesFor: #resources!public! !

