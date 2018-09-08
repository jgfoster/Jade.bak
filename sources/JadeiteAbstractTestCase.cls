"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #JadeiteAbstractTestCase
	instanceVariableNames: 'session transcript projectsBrowser'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteAbstractTestCase guid: (GUID fromString: '{883797a0-f9e4-49bf-8635-85de41e33c6b}')!
JadeiteAbstractTestCase comment: ''!
!JadeiteAbstractTestCase categoriesForClass!Unclassified! !
!JadeiteAbstractTestCase methodsFor!

abortTransaction
	transcript abortTransaction!

categoryListPresenter
	^self projectsPresenter categoryListPresenter!

classHierarchyPresenter
	^self projectsPresenter classHierarchyPresenter!

classListPresenter
	^self projectsPresenter classListPresenter!

commitTransaction
	transcript commitTransaction!

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
		ifNone: []) notNil!

loadRowanSample1InProjectList
	| sample1Path root selections |
	sample1Path := self rowanSample1Path.
	root := self rowanProjectsHome.
	self transcriptTab basicCloneProject: sample1Path root: root.
	self transcriptTab refresh.
	self selectServicesNamed: (Array with: self sampleProjectName)
		in: self transcriptTab projectListPresenter.
	selections := self transcriptTab projectListPresenter selections.
	RowanBrowserService new reloadProjects: selections presenter: self transcriptTab.
	self transcriptTab refresh!

menuNamed: menuName in: menuBar
	^menuBar items detect: [:menu | '*' , menuName , '*' match: menu text]!

methodListPresenter
	^self projectsPresenter methodListPresenter!

methodSourcePresenter
	^self projectsPresenter methodSourcePresenter!

noBreakIn: aMenu
	self assertIsNil: (aMenu items detect: [:command | '*Break*' match: command text] ifNone: [])!

openProjectsBrowser
	projectsBrowser := self openWindow: [transcript openJadeiteProjectsBrowser]!

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

projectListPresenter
	^self projectsPresenter projectListPresenter!

projectsPresenter
	^projectsBrowser currentCard!

rowanProjectsHome
	^'$ROWAN_PROJECTS_HOME'!

rowanSample1Path
	^'file:$ROWAN_PROJECTS_HOME/Rowan/samples/RowanSample1.ston'!

sampleClass
	^'RowanSample1'!

sampleProjectName
	^'RowanSample1'!

saveInterestingLoopMethod
	| source |
	source := 'interestingLoop

	| contents result |
	result := String new. 
	contents := { 23 . 55 . 45 . 999 }.
	contents select: [:n |
		| temp |
		temp := self test + n. 
		(temp / self anotherNumber) > 5
			ifTrue:[result add: ''found'']
			ifFalse: [result add: ''not found''].
		result add: '' - ''.
		n = 2.
	].
	^result'.
	self selectCategoriesNamed: (Array with: 'accessing'). 
	self methodSourcePresenter value: source.
	self projectsPresenter editSaveMethod: self classListPresenter selection!

saveIssue169Method
	| source |
	source := 'issue169

	"RowanSample1 new issue169"

	| image string|
	self halt. 
	image := Rowan image.
	string := String new. 
	^self'.
	self selectCategoriesNamed: (Array with: 'accessing'). 
	self methodSourcePresenter value: source.
	self projectsPresenter editSaveMethod: self classListPresenter selection!

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

selectMethodNamed: selector
	self selectMethodsNamed: (Array with: selector)!

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
	self selectServicesNamed: (Array with: self sampleClass) in: self classListPresenter!

selectServiceNamed: aString in: presenter
	self selectServicesNamed: (Array with: aString) in: presenter!

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

serviceNamed: name in: presenter
	| service |
	service := presenter list detect: [:service | service name = name] ifNone: [].
	^service!

setUp
	super setUp.
	session := JadeiteTestResource current session.
	transcript := JadeiteTestResource current transcript.
!

testsIssue: aSymbol withTitle: anObject
	"send this to help identify which tests test which issues"

	" a helpful parameter format is #issue<issueNumber>"

	"Issues currently reside in: 
		https://github.com/ericwinger/Jade/issues/"

	!

transcript
	^transcript!

transcriptProjectList
	^self transcriptTab projectListPresenter!

transcriptProjectNamed: string
	^self transcriptProjectList list detect: [:projectService | projectService name = string] ifNone: []!

transcriptTab
	^transcript projectsListPresenter !

unloadSampleProject
	self isSampleProjectLoaded
		ifTrue: [self projectsPresenter basicUnloadProjects: (Array with: self sampleProjectName)].
! !
!JadeiteAbstractTestCase categoriesFor: #abortTransaction!private!setup teardown! !
!JadeiteAbstractTestCase categoriesFor: #categoryListPresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #classHierarchyPresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #classListPresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #commitTransaction!private!setup teardown! !
!JadeiteAbstractTestCase categoriesFor: #ensureRowanSample1Attached!private!project support! !
!JadeiteAbstractTestCase categoriesFor: #ensureRowanSample1Available!private!project support! !
!JadeiteAbstractTestCase categoriesFor: #ensureRowanSample1Loaded!private!project support! !
!JadeiteAbstractTestCase categoriesFor: #isSampleProjectLoaded!private!private testing!project support! !
!JadeiteAbstractTestCase categoriesFor: #loadRowanSample1InProjectList!private!project support! !
!JadeiteAbstractTestCase categoriesFor: #menuNamed:in:!issue test support!private! !
!JadeiteAbstractTestCase categoriesFor: #methodListPresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #methodSourcePresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #noBreakIn:!issue test support!private! !
!JadeiteAbstractTestCase categoriesFor: #openProjectsBrowser!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #openWindow:!browser test support!private! !
!JadeiteAbstractTestCase categoriesFor: #packageListPresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #projectListPresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #projectsPresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #rowanProjectsHome!constants!private! !
!JadeiteAbstractTestCase categoriesFor: #rowanSample1Path!constants!private! !
!JadeiteAbstractTestCase categoriesFor: #sampleClass!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #sampleProjectName!constants!private! !
!JadeiteAbstractTestCase categoriesFor: #saveInterestingLoopMethod!browser support!private! !
!JadeiteAbstractTestCase categoriesFor: #saveIssue169Method!issue test support!private! !
!JadeiteAbstractTestCase categoriesFor: #selectCategoriesNamed:!private!project browser support!selection! !
!JadeiteAbstractTestCase categoriesFor: #selectMethodNamed:!private!selection! !
!JadeiteAbstractTestCase categoriesFor: #selectMethodsNamed:!private!selection! !
!JadeiteAbstractTestCase categoriesFor: #selectRowanSample1Class!private!project browser support!selection! !
!JadeiteAbstractTestCase categoriesFor: #selectServiceNamed:in:!private!project browser support!selection! !
!JadeiteAbstractTestCase categoriesFor: #selectServicesNamed:in:!private!project browser support!selection! !
!JadeiteAbstractTestCase categoriesFor: #serviceNamed:in:!private!selection! !
!JadeiteAbstractTestCase categoriesFor: #setUp!private!setup teardown! !
!JadeiteAbstractTestCase categoriesFor: #testsIssue:withTitle:!public!support! !
!JadeiteAbstractTestCase categoriesFor: #transcript!public!transcript support! !
!JadeiteAbstractTestCase categoriesFor: #transcriptProjectList!public!transcript support! !
!JadeiteAbstractTestCase categoriesFor: #transcriptProjectNamed:!public!transcript support! !
!JadeiteAbstractTestCase categoriesFor: #transcriptTab!public!transcript support! !
!JadeiteAbstractTestCase categoriesFor: #unloadSampleProject!private!project support! !

!JadeiteAbstractTestCase class methodsFor!

isAbstract
	^self == JadeiteAbstractTestCase! !
!JadeiteAbstractTestCase class categoriesFor: #isAbstract!public! !

