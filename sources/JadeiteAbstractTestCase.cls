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

methodListPresenter
	^self projectsPresenter methodListPresenter!

methodSourcePresenter
	^self projectsPresenter methodSourcePresenter!

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
!JadeiteAbstractTestCase categoriesFor: #classListPresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #ensureRowanSample1Attached!private!project support! !
!JadeiteAbstractTestCase categoriesFor: #ensureRowanSample1Available!private!project support! !
!JadeiteAbstractTestCase categoriesFor: #ensureRowanSample1Loaded!private!project support! !
!JadeiteAbstractTestCase categoriesFor: #isSampleProjectLoaded!private!private testing!project support! !
!JadeiteAbstractTestCase categoriesFor: #loadRowanSample1InProjectList!private!project support! !
!JadeiteAbstractTestCase categoriesFor: #methodListPresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #methodSourcePresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #openProjectsBrowser!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #openWindow:!browser test support!private! !
!JadeiteAbstractTestCase categoriesFor: #packageListPresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #projectListPresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #projectsPresenter!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #rowanProjectsHome!constants!private! !
!JadeiteAbstractTestCase categoriesFor: #rowanSample1Path!constants!private! !
!JadeiteAbstractTestCase categoriesFor: #sampleClass!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #sampleProjectName!constants!private! !
!JadeiteAbstractTestCase categoriesFor: #selectCategoriesNamed:!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #selectRowanSample1Class!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #selectServiceNamed:in:!private!project browser support! !
!JadeiteAbstractTestCase categoriesFor: #selectServicesNamed:in:!private!project browser support! !
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

