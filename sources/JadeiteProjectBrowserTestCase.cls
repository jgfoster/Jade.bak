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
	self selectServicesNamed: (Array with: 'RowanSample1')
		in: self projectsPresenter projectListPresenter.
	selections := self projectsPresenter projectListPresenter selections.
	RowanBrowserService new reloadProjects: selections presenter: self projectsPresenter.
	self projectsPresenter refreshFromServer!

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

saveSampleMethod

	| source |
	source := 'sampleMethod   ^123'. 
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

selectRowanSample1Class
	"big assumption - RowanSample1 class will allways be in RowanSample1 project"

	self selectServicesNamed: (Array with: 'RowanSample1') in: self projectListPresenter.
	self selectServicesNamed: #('RowanSample1-Core') in: self packageListPresenter.
	self selectServicesNamed: (Array with: self class sampleClass) in: self classListPresenter!

selectServicesNamed: anArray in: presenter
	presenter resetSelection.
	anArray do: 
			[:name |
			| service |
			service := presenter list detect: [:svc | svc name = name]
						ifNone: [self error: 'List element ' , name , ' not found'].
			presenter selections: (presenter selections asOrderedCollection
						add: service;
						yourself)]!

serviceNamed: name in: presenter
	| service |
	service := presenter list detect: [:service | service name = name] ifNone: [].
	^service!

setUp
	super setUp.
	self openProjectsBrowser.
	self abortTransaction.	"ensure we don't have unsaved changes in the images"
	(transcriptWritesWereEnabled := transcript isTranscriptInstalled)
		ifTrue: [transcript flipTranscript]	"turn off gemstone transcript writes for performance"!

tearDown
	super tearDown.
	self abortTransaction.
	projectsBrowser ifNotNil: [projectsBrowser destroy].
	transcriptWritesWereEnabled ifTrue: [transcript flipTranscript]!

test_addRemoveCategory

	self ensureRowanSample1Available.
	self selectRowanSample1Class.
	self deny: (self categoryListPresenter list includes: 'jadeite test').
	self projectsPresenter basicAddMethodCategory: 'jadeite test'.
	self assert: (self categoryListPresenter list includes: 'jadeite test').
	self assert: self categoryListPresenter selection = 'jadeite test'.
	self projectsPresenter basicRemoveMethodCategories.
	self deny: (self categoryListPresenter list includes: 'jadeite test')!

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

test_saveMethod
	self ensureRowanSample1Available.
	self selectRowanSample1Class.
	(self categoryListPresenter list includes: 'test_saveMethod')
		ifFalse: [self projectsPresenter basicAddMethodCategory: 'test_saveMethod'].
	self saveSampleMethod.
	self selectServicesNamed: (Array with: 'sampleMethod') in: self methodListPresenter.
	self assert: (session executeString: 'RowanSample1 new sampleMethod') = 123!

test_selectMultiplePackages
	self assert: self packageListPresenter list isEmpty.
	self selectServicesNamed: #('Rowan') in: self projectListPresenter.
	self deny: self packageListPresenter list isEmpty.
	self serviceNamed: 'Rowan-Services-Core' in: self packageListPresenter.
	self serviceNamed: 'Rowan-JadeServer' in: self packageListPresenter.
	self selectServicesNamed: #('Rowan-Services-Core' 'Rowan-JadeServer') in: self packageListPresenter!

test_selectMultipleProjects
	self assert: self projectListPresenter selections isEmpty.
	self selectServicesNamed: self rowanProjectNames in: self projectListPresenter.
	self assert: self projectListPresenter selections size = 4!

waitForPresenter: presenter
	[presenter list isEmpty] whileTrue: [(Delay forMilliseconds: 50) wait]! !
!JadeiteProjectBrowserTestCase categoriesFor: #categoryListPresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #classListPresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #ensureRowanSample1Attached!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #ensureRowanSample1Available!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #ensureRowanSample1Loaded!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #methodListPresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #methodSourcePresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #openProjectsBrowser!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #packageListPresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectList!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectListPresenter!presenter!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectNamed:!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectsPresenter!presenter!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #rowanProjectNames!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #rowanProjectsHome!constants!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #rowanSample1Path!constants!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #saveSampleMethod!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectCategoriesNamed:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectRowanSample1Class!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectServicesNamed:in:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #serviceNamed:in:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #setUp!private!setup teardown! !
!JadeiteProjectBrowserTestCase categoriesFor: #tearDown!private!setup teardown! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_addRemoveCategory!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_markDirty!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_openProjectsBrowser!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_packagesShownInMultipleProjects!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_projectRemoved!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_saveMethod!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_selectMultiplePackages!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_selectMultipleProjects!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #waitForPresenter:!private!selection! !

!JadeiteProjectBrowserTestCase class methodsFor!

resources
	^IdentitySet with: JadeiteTestResource!

sampleClass
	^'RowanSample1'! !
!JadeiteProjectBrowserTestCase class categoriesFor: #resources!public! !
!JadeiteProjectBrowserTestCase class categoriesFor: #sampleClass!public! !

