"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanClassService
	instanceVariableNames: 'comment instVarNames classVarNames classInstVarNames superclassName subclassType poolDictionaryNames classType meta isExtension version versions template oop filters filterType methods packageName definedPackageName selectedPackageServices selectedMethods projectName hierarchyServices variables categories'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanClassService guid: (GUID fromString: '{ab96bbd9-d725-4add-b635-94cec9f12a19}')!
RowanClassService comment: ''!
!RowanClassService categoriesForClass!Kernel-Objects! !
!RowanClassService methodsFor!

= classService
	^classService isClassService ifTrue: [name asString = classService name asString] ifFalse: [^false]!

addHierarchyService: service to: treeModel withParent: parentService
	treeModel add: service asChildOf: parentService.
	(hierarchyServices at: service ifAbsent: [^self]) do: 
			[:classService |
			self
				addHierarchyService: classService
				to: treeModel
				withParent: service]!

categories
	^categories!

classCreationTemplate

	^transport send: #classCreationTemplate to: self remoteService!

classHierarchyUpdate: presenter browser: browser
	| treeModel selection |
	hierarchyServices ifNil:[^self]. 
	browser isClassListTabSelected ifTrue: [^self].
	treeModel := TreeModel new
				searchPolicy: SearchPolicy equality;
				reset.
	(hierarchyServices at: #nil) do: 
			[:classService |
			self
				addHierarchyService: classService
				to: treeModel
				withParent: nil].
	presenter model: treeModel.
	presenter view expandAll. 
	selection := (treeModel asBag detect: [:svc | svc name = name asSymbol]). 
	selection hierarchyServices: nil. 
	hierarchyServices := nil. 
	presenter selection: selection
!

classMethodsUpdate: presenter browser: browser
	| classPresenter |
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	classPresenter := self classOrHierarchyPresenter: browser.
	classPresenter selections detect: [:classService | classService name = name] ifNone: [^self].
	self displayMethodsOn: presenter!

classOrHierarchyPresenter: browser
	^browser isClassListTabSelected
		ifTrue: [browser classListPresenter]
		ifFalse: [browser classHierarchyPresenter]!

comment
	^comment!

comment: anObject
	comment := anObject!

computeVisibleMethods
	| visibleMethods |
	filterType isNil ifTrue: [visibleMethods := methods].
	filterType = 'variables'
		ifTrue: 
			[visibleMethods := methods
						select: [:methodService | (filters intersection: methodService accessedInstVars) notEmpty]].
	filterType = 'categories'
		ifTrue: [visibleMethods := methods select: [:methodService | filters includes: methodService category]].
	visibleMethods
		do: [:svc | svc inSelectedPackage: (self selectedPackageServiceNames includes: svc packageName)].
	^visibleMethods!

createSampleClass
	transport send: #createSampleClass to: remoteService.  !

definedPackageName
	^definedPackageName!

definedPackageName: anObject
	definedPackageName := anObject!

displayMethodsOn: presenter
	| selections visibleMethods |
	visibleMethods := self computeVisibleMethods.
	selections := presenter selections collect: [:methodService | methodService selector].
	presenter list: visibleMethods.
	presenter
		selections: (visibleMethods select: [:methodService | selections includes: methodService selector])!

displayString
	| displayString |
	displayString := self name.
	versions ~= 1
		ifTrue: [displayString := displayString , ' (' , version printString , '/' , versions printString , ')'].
	^displayString!

displayStringFor: displayThing
	isExtension ifTrue: [displayThing forecolor: Color darkMagenta ].
	^self displayString!

excludedInstVars

	^#( 'hierarchyServices' )
!

filters
	^filters!

filters: anObject
	filters := anObject!

filterType
	^filterType!

filterType: anObject
	filterType := anObject!

filterUpdate: presenter browser: browser
	| classPresenter |
	presenter list isEmpty ifTrue: [presenter list: SortedCollection new].
	classPresenter := self classOrHierarchyPresenter: browser.
	classPresenter selections detect: [:classService | classService name = name] ifNone: [^self].
	browser isCategoriesTabSelected
		ifTrue: 
			[presenter list = categories ifTrue: [^self].
			self updateCategories: presenter]
		ifFalse: 
			[presenter list = variables ifTrue: [^self].
			self updateVariables: presenter]!

hash
	^self name hash!

hierarchyServices
	^hierarchyServices!

hierarchyServices: anObject
	hierarchyServices := anObject!

initialize
	
	super initialize. 
	filters := Array new. !

isClassService

	^true!

isDefined

	^isExtension not!

isExtension

	^isExtension!

meta
	^meta!

meta: anObject
	meta := anObject!

methods
	^methods!

methods: anObject
	methods := anObject!

packageName
	^packageName!

packageName: anObject
	packageName := anObject!

projectName
	^projectName!

releaseReferencedServices
	| copy |
	selectedPackageServices
		ifNotNil: 
			[copy := selectedPackageServices copy.
			selectedPackageServices := Array new.
			copy do: [:service | service releaseReferencedServices]].
	methods
		ifNotNil: 
			[copy := methods copy. 
			methods := Array new.
			copy do: [:service | service releaseReferencedServices]]!

remoteServiceName
	^'Rowan classServiceClass'!

removeSelector: selector

	transport send: #removeSelector: to: remoteService withAll: (Array with: selector)!

replicateFrom: newService
	self isClassService ifTrue: [^super replicateFrom: newService]!

sampleClassName
	^transport send: #sampleClassName to: remoteService.  !

selectedMethods
	^selectedMethods!

selectedMethods: anObject
	selectedMethods := anObject!

selectedPackageServiceNames
	^selectedPackageServices collect: [:pkgService | pkgService name]!

selectedPackageServices
	^selectedPackageServices!

selectedPackageServices: anObject
	selectedPackageServices := anObject!

stonOn: stonWriter
	| instanceVariableNames |
	(instanceVariableNames := self class allInstVarNames reject: [:iv | self excludedInstVars includes: iv]) isEmpty
		ifTrue: [stonWriter writeObject: self do: [stonWriter encodeMap: #()]]
		ifFalse: 
			[stonWriter writeObject: self
				streamMap: 
					[:dictionary |
					instanceVariableNames do: 
							[:each |
							(self instVarAt: (instanceVariableNames indexOf: each))
								ifNotNil: [:value | dictionary at: each asSymbol put: value]
								ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]]!

subclassCreationTemplate
	^transport send: #subclassCreationTemplate to: self remoteService!

template
	^template!

template: anObject
	template := anObject!

updateCategories: presenter
	| selections availableSelections |
	selections := presenter selections.
	presenter list: categories.
	availableSelections := categories select: [:filter | selections includes: filter].
	presenter selections: availableSelections!

updateVariables: presenter
	| selections availableSelections |
	selections := presenter selections.
	presenter list: variables.
	availableSelections := variables select: [:filter | selections includes: filter].
	presenter selections: availableSelections!

variables
	^variables! !
!RowanClassService categoriesFor: #=!comparing!public! !
!RowanClassService categoriesFor: #addHierarchyService:to:withParent:!private!updating! !
!RowanClassService categoriesFor: #categories!accessing!private! !
!RowanClassService categoriesFor: #classCreationTemplate!public! !
!RowanClassService categoriesFor: #classHierarchyUpdate:browser:!public!updating! !
!RowanClassService categoriesFor: #classMethodsUpdate:browser:!public!updating! !
!RowanClassService categoriesFor: #classOrHierarchyPresenter:!private!updating! !
!RowanClassService categoriesFor: #comment!accessing!public! !
!RowanClassService categoriesFor: #comment:!accessing!public! !
!RowanClassService categoriesFor: #computeVisibleMethods!private!updating! !
!RowanClassService categoriesFor: #createSampleClass!public! !
!RowanClassService categoriesFor: #definedPackageName!accessing!private! !
!RowanClassService categoriesFor: #definedPackageName:!accessing!private! !
!RowanClassService categoriesFor: #displayMethodsOn:!public!updating! !
!RowanClassService categoriesFor: #displayString!printing!public! !
!RowanClassService categoriesFor: #displayStringFor:!printing!public! !
!RowanClassService categoriesFor: #excludedInstVars!public!ston! !
!RowanClassService categoriesFor: #filters!accessing!public! !
!RowanClassService categoriesFor: #filters:!accessing!public! !
!RowanClassService categoriesFor: #filterType!accessing!private! !
!RowanClassService categoriesFor: #filterType:!accessing!private! !
!RowanClassService categoriesFor: #filterUpdate:browser:!public!updating! !
!RowanClassService categoriesFor: #hash!comparing!public! !
!RowanClassService categoriesFor: #hierarchyServices!accessing!private! !
!RowanClassService categoriesFor: #hierarchyServices:!accessing!private! !
!RowanClassService categoriesFor: #initialize!Init / Release!public! !
!RowanClassService categoriesFor: #isClassService!public!testing! !
!RowanClassService categoriesFor: #isDefined!public!testing! !
!RowanClassService categoriesFor: #isExtension!public!testing! !
!RowanClassService categoriesFor: #meta!accessing!private! !
!RowanClassService categoriesFor: #meta:!accessing!private! !
!RowanClassService categoriesFor: #methods!accessing!private! !
!RowanClassService categoriesFor: #methods:!accessing!private! !
!RowanClassService categoriesFor: #packageName!accessing!private! !
!RowanClassService categoriesFor: #packageName:!accessing!private! !
!RowanClassService categoriesFor: #projectName!accessing!private! !
!RowanClassService categoriesFor: #releaseReferencedServices!Init / Release!public! !
!RowanClassService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanClassService categoriesFor: #removeSelector:!public!rowan! !
!RowanClassService categoriesFor: #replicateFrom:!public!updating! !
!RowanClassService categoriesFor: #sampleClassName!public! !
!RowanClassService categoriesFor: #selectedMethods!accessing!private! !
!RowanClassService categoriesFor: #selectedMethods:!accessing!private! !
!RowanClassService categoriesFor: #selectedPackageServiceNames!private!selection! !
!RowanClassService categoriesFor: #selectedPackageServices!accessing!private! !
!RowanClassService categoriesFor: #selectedPackageServices:!accessing!private! !
!RowanClassService categoriesFor: #stonOn:!must not strip!public!ston! !
!RowanClassService categoriesFor: #subclassCreationTemplate!public! !
!RowanClassService categoriesFor: #template!accessing!private! !
!RowanClassService categoriesFor: #template:!accessing!private! !
!RowanClassService categoriesFor: #updateCategories:!private!updating! !
!RowanClassService categoriesFor: #updateVariables:!private!updating! !
!RowanClassService categoriesFor: #variables!accessing!private! !

