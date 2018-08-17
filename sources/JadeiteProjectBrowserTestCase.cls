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

classListPresenter
	^browser currentCard classListPresenter!

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
	^browser currentCard packageListPresenter!

projectList
	^self projectListPresenter list!

projectListPresenter
	^browser currentCard projectListPresenter!

rowanProjectNames
	"this may change"

	^#('Cypress' 'Rowan' 'STON' 'Tonel')!

selectElementsNamed: anArray in: presenter
	anArray do: 
			[:name |
			| service |
			service := presenter list detect: [:svc | svc name = name]
						ifNone: [self error: 'List element ' , name , ' not found'].
			presenter selections: (presenter selections asOrderedCollection
						add: service;
						yourself)].
!

serviceNamed: name in: presenter
	| service |
	service := presenter list detect: [:service | service name = name] ifNone: [].
	^service!

setUp
	super setUp.
	self openProjectsBrowser.!

tearDown
	super tearDown.
	browser ifNotNil: [browser destroy]!

test_openProjectsBrowser
	"project browser opened in setup"
	self assert: browser class equals: JadeiteProjectsSystemBrowser.
	self
		denyIsNil: (self projectList detect: [:projectService | projectService name = 'Rowan'] ifNone: [])!

test_packagesShownInMultipleProjects
	self selectElementsNamed: (Array with: 'Rowan') in: self projectListPresenter.
	self assertIsNil: (self serviceNamed: 'RowanService' in: self classListPresenter).
	self selectElementsNamed: #('Rowan-Services-Core') in: self packageListPresenter.
	self denyIsNil: (self serviceNamed: 'RowanService' in: self classListPresenter).
	self assertIsNil: (self serviceNamed: 'JadeServer' in: self classListPresenter).
	self selectElementsNamed: #('Rowan-JadeServer') in: self packageListPresenter.
	self denyIsNil: (self serviceNamed: 'JadeServer' in: self classListPresenter)!

test_selectMultiplePackages
	self assert: self packageListPresenter list isEmpty.
	self selectElementsNamed: #('Rowan') in: self projectListPresenter.
	self deny: self packageListPresenter list isEmpty.
	self serviceNamed: 'Rowan-Services-Core' in: self packageListPresenter.
	self serviceNamed: 'Rowan-JadeServer' in: self packageListPresenter.
	self selectElementsNamed: #('Rowan-Services-Core' 'Rowan-JadeServer') in: self packageListPresenter!

test_selectMultipleProjects
	self assert: self projectListPresenter selections isEmpty.
	self selectElementsNamed: self rowanProjectNames in: self projectListPresenter.
	self assert: self projectListPresenter selections size = 4!

waitForPresenter: presenter
	[presenter list isEmpty] whileTrue: [(Delay forMilliseconds: 50) wait]! !
!JadeiteProjectBrowserTestCase categoriesFor: #classListPresenter!presenter!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #openProjectsBrowser!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #openWindow:!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #packageListPresenter!presenter!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectList!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #projectListPresenter!presenter!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #rowanProjectNames!private!project support! !
!JadeiteProjectBrowserTestCase categoriesFor: #selectElementsNamed:in:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #serviceNamed:in:!private!selection! !
!JadeiteProjectBrowserTestCase categoriesFor: #setUp!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #tearDown!private! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_openProjectsBrowser!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_packagesShownInMultipleProjects!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_selectMultiplePackages!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #test_selectMultipleProjects!public!test! !
!JadeiteProjectBrowserTestCase categoriesFor: #waitForPresenter:!private!selection! !

!JadeiteProjectBrowserTestCase class methodsFor!

resources
	^IdentitySet with: JadeiteTestResource! !
!JadeiteProjectBrowserTestCase class categoriesFor: #resources!public! !

