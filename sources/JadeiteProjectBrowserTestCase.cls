"Filed out from Dolphin Smalltalk 7"!

JadeiteAbstractTestCase subclass: #JadeiteProjectBrowserTestCase
	instanceVariableNames: 'browser'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteProjectBrowserTestCase guid: (GUID fromString: '{1d6db475-c257-442b-9f91-d1574b03aee2}')!
JadeiteProjectBrowserTestCase comment: 'General Jadeite tests. Assumes a logged in session and transcript present. '!
!JadeiteProjectBrowserTestCase categoriesForClass!Unclassified! !
!JadeiteProjectBrowserTestCase methodsFor!

abortTransaction

	browser abortTransaction!

categoryListPresenter
	^self projectsPresenter categoryListPresenter!

classListPresenter
	^self projectsPresenter classListPresenter!

ensureRowanSample1Attached
	| sample1Path root |
	sample1Path := self rowanSample1Path.
	root := self rowanProjectsHome.
	self projectsPresenter basicCloneProject: sample1Path root: root!

methodListPresenter
	^self projectsPresenter methodListPresenter!

openProjectsBrowser
	browser := self openWindow: [transcript openJadeiteProjectsBrowser]!

openWindow: aBlock
	"evaluate a block which is expected to return an object that responds to #view.
	Wait until the view affirms it is open before continuing"

	| presenter count |
	presenter := aBlock value.
	count := 0.
	[presenter view isOpen] whileFalse: 
			[(Delay forMilliseconds: 50) wait.
			count := count + 1.
			count > 10 ifTrue: [^presenter]].
	^presenter!

packageListPresenter
	^self projectsPresenter packageListPresenter!

projectList
	^self projectListPresenter list!

projectListPresenter
	^self projectsPresenter projectListPresenter!

projectsPresenter
	^browser currentCard!

rowanProjectNames
	"this may change"

	^#('Cypress' 'Rowan' 'STON' 'Tonel')!

rowanProjectsHome
	^'$ROWAN_PROJECTS_HOME'!

rowanSample1Path
	^'file:$ROWAN_PROJECTS_HOME/Rowan/samples/RowanSample1.ston'!

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

selectFooMethod

	"big assumption - #foo will always be in RowanSample1 project"
	self selectServicesNamed: (Array with: 'RowanSample1') in: self projectListPresenter.
	self selectServicesNamed: #('RowanSample1-Core') in: self packageListPresenter.
	self selectServicesNamed: #('RowanSample1') in: self classListPresenter.
	self selectCategoriesNamed: #('accessing').
	self selectServicesNamed: #('foo') in: self methodListPresenter!

selectRowanSample1Class
	"big assumption - RowanSample1 class will allways be in RowanSample1 project"

	self selectServicesNamed: (Array with: 'RowanSample1') in: self projectListPresenter.
	self selectServicesNamed: #('RowanSample1-Core') in: self packageListPresenter.
	self selectServicesNamed: #('RowanSample1') in: self classListPresenter!

selectServicesNamed: anArray in: presenter
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
	self openProjectsBrowser.!

tearDown
	super tearDown.
	self abortTransaction. 
	browser ifNotNil: [browser destroy]!

test_addRemoveCategory
	"This test will fail if RowanSample1 project has not been cloned in ROWAN_PROJECTS_HOME"

	self ensureRowanSample1Attached.
	self selectRowanSample1Class.
	self deny: (self categoryListPresenter list includes: 'jadeite test').
	self projectsPresenter basicAddMethodCategory: 'jadeite test'.
	self assert: (self categoryListPresenter list includes: 'jadeite test').
	self assert: self categoryListPresenter selection = 'jadeite test'.
	self projectsPresenter basicRemoveMethodCategories.
	self deny: (self categoryListPresenter list includes: 'jadeite test')!

test_openProjectsBrowser
	"project browser opened in setup"
	self assert: browser class equals: JadeiteProjectsSystemBrowser.
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

test_saveMethod
	self rowanFixMe. "incomplete test"
	self ensureRowanSample1Attached.
	self selectFooMethod!

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
!JadeiteProjectBrowserTestCase categoriesFor: #abortTransaction!private!setup teardown! !
!JadeiteProjectBrowserTestCase categoriesFor: #categoryListPresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #classListPresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #ensureRowanSample1Attached!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #methodListPresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #openProjectsBrowser!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #openWindow:!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #packageListPresenter!presenter!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectList!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectListPresenter!presenter!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectsPresenter!presenter!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #rowanProjectNames!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #rowanProjectsHome!constants!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #rowanSample1Path!constants!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectCategoriesNamed:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectFooMethod!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectRowanSample1Class!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectServicesNamed:in:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #serviceNamed:in:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #setUp!private!setup teardown! !
!JadeiteProjectBrowserTestCase categoriesFor: #tearDown!private!setup teardown! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_addRemoveCategory!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_openProjectsBrowser!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_packagesShownInMultipleProjects!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_saveMethod!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_selectMultiplePackages!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_selectMultipleProjects!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #waitForPresenter:!private!selection! !

!JadeiteProjectBrowserTestCase class methodsFor!

resources
	^IdentitySet with: JadeiteTestResource! !
!JadeiteProjectBrowserTestCase class categoriesFor: #resources!public! !

