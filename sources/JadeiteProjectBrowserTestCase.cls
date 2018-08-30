"Filed out from Dolphin Smalltalk 7"!

JadeiteAbstractTestCase subclass: #JadeiteProjectBrowserTestCase
	instanceVariableNames: 'projectsBrowser dictionaryBrowser transcriptWritesWereEnabled'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteProjectBrowserTestCase guid: (GUID fromString: '{1d6db475-c257-442b-9f91-d1574b03aee2}')!
JadeiteProjectBrowserTestCase comment: 'General Jadeite tests. Assumes a logged in session and transcript present. '!
!JadeiteProjectBrowserTestCase categoriesForClass!Unclassified! !
!JadeiteProjectBrowserTestCase methodsFor!

categoryListPresenter
	^self projectsPresenter categoryListPresenter!

classListPresenter
	^self projectsPresenter classListPresenter!

disableTranscriptWrites
	(transcriptWritesWereEnabled := transcript isTranscriptInstalled)
		ifTrue: [transcript flipTranscript]	"turn off gemstone transcript writes for performance"!

ensureRowanSample1Attached
	| sample1Path root |
	sample1Path := self rowanSample1Path.
	root := self rowanProjectsHome.
	self projectsPresenter basicCloneProject: sample1Path root: root.
	self projectsPresenter refreshFromServer.!

ensureRowanSample1Available

	self ensureRowanSample1Attached.
	self ensureRowanSample1Loaded. 
	!

ensureRowanSample1Loaded
	| selections |
	self selectServicesNamed: (Array with: self sampleProjectName)
		in: self projectsPresenter projectListPresenter.
	selections := self projectsPresenter projectListPresenter selections.
	RowanBrowserService new reloadProjects: selections presenter: self projectsPresenter.
	self projectsPresenter refreshFromServer!

isSampleProjectLoaded
	^(self projectsPresenter projectListPresenter list
		detect: [:projectService | projectService name = self sampleProjectName]
		ifNone: [self]) notNil!

methodListPresenter
	^self projectsPresenter methodListPresenter!

methodSourcePresenter
	^self projectsPresenter methodSourcePresenter!

openProjectsBrowser
	projectsBrowser := self openWindow: [transcript openJadeiteProjectsBrowser]!

packageListPresenter
	^self projectsPresenter packageListPresenter!

projectList
	^self projectListPresenter list!

projectListPresenter
	^self projectsPresenter projectListPresenter!

projectNamed: aString
	^self projectsPresenter projectListPresenter list
		detect: [:projectService | projectService name = 'RowanSample1']
		ifNone: []!

projectsPresenter
	^projectsBrowser currentCard!

rowanProjectNames
	"this may change"

	^#('Cypress' 'Rowan' 'STON' 'Tonel')!

rowanProjectsHome
	^'$ROWAN_PROJECTS_HOME'!

rowanSample1Path
	^'file:$ROWAN_PROJECTS_HOME/Rowan/samples/RowanSample1.ston'!

sampleProjectName
	^'RowanSample1'!

saveSampleMethod

	| source |
	source := 'sampleMethod  
		aaa := 123. 
		^aaa'. 
	self methodSourcePresenter value: source. 
	self projectsPresenter editSaveMethod: self classListPresenter selection. 

	!

selectCategoriesNamed: categories
	categories do: 
			[:category |
			| service |
			service := self categoryListPresenter list
						detect: [:listCategory | categories includes: listCategory]
						ifNone: [self error: 'Category ''' , category , ''' not found'].
			self categoryListPresenter selections: (self categoryListPresenter selections asOrderedCollection
						add: service;
						yourself)]!

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

selectMethodsNamed: selectors
	self methodListPresenter resetSelection. 
	selectors do: 
			[:selector |
			| service |
			service := self methodListPresenter list
						detect: [:methodService | methodService selector = selector]
						ifNone: [self error: 'Selector ''' , selector , ''' not found'].
			self methodListPresenter selections: (self methodListPresenter selections asOrderedCollection
						add: service;
						yourself)]!

selectRowanSample1Class
	"big assumption - RowanSample1 class will allways be in RowanSample1 project"

	self selectServicesNamed: (Array with: 'RowanSample1') in: self projectListPresenter.
	self selectServicesNamed: #('RowanSample1-Core') in: self packageListPresenter.
	self selectServicesNamed: (Array with: self class sampleClass) in: self classListPresenter!

selectServicesNamed: anArray in: presenter
	presenter resetSelection.
	anArray isEmpty ifTrue:[^self].
	anArray do: 
			[:name |
			| service |
			service := presenter list detect: [:svc | svc name = name]
						ifNone: [self error: 'List element ' , name , ' not found'].
			presenter selections: (presenter selections asOrderedCollection
						add: service;
						yourself)]!

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

serviceNamed: name in: presenter
	| service |
	service := presenter list detect: [:service | service name = name] ifNone: [].
	^service!

setUp
	super setUp.
	self disableTranscriptWrites.	"turn off gemstone transcript writes for performance"
	self openProjectsBrowser.
	self abortTransaction.	"ensure we don't have unsaved changes in the images"
	self ensureRowanSample1Available.!

tearDown
	super tearDown.
	self abortTransaction.
	projectsBrowser ifNotNil: [projectsBrowser destroy].
	transcriptWritesWereEnabled ifTrue: [transcript flipTranscript]!

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
	self assert: self projectsPresenter classHierarchyPresenter model isEmpty.  "for now"
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
	self selectTreeServicesNamed: (Array with: 'RwGemStoneSpecification')
		in: self projectsPresenter classHierarchyPresenter.
	self assert: self projectsPresenter classHierarchyPresenter selection shouldColorAsExtension!

test_goToDefinedPackageFromClassList
	self selectServicesNamed: (Array with: 'Rowan') in: self projectListPresenter.
	self selectServicesNamed: #('Rowan-Services-Extensions') in: self packageListPresenter.
	self selectServicesNamed: (Array with: 'RwGsPlatform') in: self classListPresenter.
	self assert: self projectsPresenter isClassListTabSelected.
	self projectsPresenter gotoClassDefinedPackage.
	self assert: self classListPresenter selection name = 'RwGsPlatform'.
	self assert: self packageListPresenter selection name = 'Rowan-GemStone-Core'!

test_goToDefinedPackageFromHierarchy
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
	self selectTreeServicesNamed: (Array with: 'RwGemStoneSpecification')
		in: self projectsPresenter classHierarchyPresenter.
	self projectsPresenter gotoClassDefinedPackage.
	self assert: self projectsPresenter isClassListTabSelected.
	self assert: self packageListPresenter selection name = 'Rowan-GemStone-Specifications'!

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

test_markDirty
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

test_projectRemoved
	"Gracefully remove a project from the project list if it was 
	removed behind the tool's back"

	| sampleProject |
	self ensureRowanSample1Attached.
	self projectsPresenter refreshFromServer.
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

unloadSampleProject
	self isSampleProjectLoaded
		ifTrue: [self projectsPresenter basicUnloadProjects: (Array with: self sampleProjectName)]!

waitForPresenter: presenter
	[presenter list isEmpty] whileTrue: [(Delay forMilliseconds: 50) wait]! !
!JadeiteProjectBrowserTestCase categoriesFor: #categoryListPresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #classListPresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #disableTranscriptWrites!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #ensureRowanSample1Attached!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #ensureRowanSample1Available!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #ensureRowanSample1Loaded!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #isSampleProjectLoaded!private!private testing!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #methodListPresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #methodSourcePresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #openProjectsBrowser!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #packageListPresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectList!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectListPresenter!presenter!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectNamed:!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectsPresenter!presenter!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #rowanProjectNames!constants!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #rowanProjectsHome!constants!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #rowanSample1Path!constants!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #sampleProjectName!constants!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #saveSampleMethod!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectCategoriesNamed:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectClassListTab!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectClassTab!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectClassTabIn:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectHierarchyTab!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectHierarchyTabIn:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectMethodsNamed:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectRowanSample1Class!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectServicesNamed:in:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectTreeServicesNamed:in:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #serviceNamed:in:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #setUp!private!setup teardown! !
!JadeiteProjectBrowserTestCase categoriesFor: #tearDown!private!setup teardown! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_addRemoveCategory!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_autoDirtyDisplayMoveMethodsToCategory!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_autoDirtyDisplayRemoveMethod!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_classListNotBlank!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_classSideSelection!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_extensionColoringInHierarchy!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_goToDefinedPackageFromClassList!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_goToDefinedPackageFromHierarchy!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_hierarchySelectTwoBrowsers!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_markDirty!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_openProjectsBrowser!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_packagesShownInMultipleProjects!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_projectRemoved!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_removeProject!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_saveClassMethod!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_saveClassMethodHierarchy!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_saveMethod!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_saveMethodHierarchy!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_selectMultiplePackages!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_selectMultipleProjects!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_simpleHierarchyTest!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #unloadSampleProject!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #waitForPresenter:!private!selection! !

!JadeiteProjectBrowserTestCase class methodsFor!

resources
	^IdentitySet with: JadeiteTestResource!

sampleClass
	^'RowanSample1'! !
!JadeiteProjectBrowserTestCase class categoriesFor: #resources!public! !
!JadeiteProjectBrowserTestCase class categoriesFor: #sampleClass!public! !

