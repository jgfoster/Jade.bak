"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanClassService
	instanceVariableNames: 'comment instVarNames classVarNames classInstVarNames superclassName subclassType poolDictionaryNames classType meta isExtension version versions template oop filters filterType methods packageName definedPackageName selectedPackageServices selectedMethods projectName hierarchyServices variables categories isTestCase'
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

classHierarchyUpdate: presenter browser: browser
	| treeModel |
	hierarchyServices ifNil: [^self].
	browser isClassListTabSelected ifTrue: [^self].
	presenter selections notEmpty ifTrue: [(presenter selections includes: self) ifFalse: [^self]].
	treeModel := TreeModel new
				searchPolicy: SearchPolicy equality;
				reset.
	(hierarchyServices at: #nil) do: 
			[:classService |
			self
				addHierarchyService: classService
				to: treeModel
				withParent: nil].
	treeModel asBag do:[:classService | classService selectedPackageServices: browser packageListPresenter selections].
	presenter model: treeModel.
	name = 'Object' ifFalse:[
		presenter view expandAll].
!

classMethodsUpdate: presenter browser: browser
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	browser selectedClass name = name ifFalse: [^self].
	filters isEmpty ifTrue:[filterType := nil].
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

definedPackageName
	^definedPackageName!

definedPackageName: anObject
	definedPackageName := anObject!

displayMethodsOn: presenter
	| visibleMethods |
	visibleMethods := self computeVisibleMethods.
	self updateListIn: presenter from: visibleMethods!

displayString
	| displayString |
	displayString := self name.
	versions ~= 1
		ifTrue: [displayString := displayString , ' (' , version printString , '/' , versions printString , ')'].
	^displayString!

displayStringFor: displayThing
	self shouldColorAsExtension ifTrue: [displayThing forecolor: Color darkMagenta].
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

initialize
	
	super initialize. 
	filters := Array new. 
	selectedPackageServices := Array new. "method list browser will not have packages to select"
	isTestCase := false. !

isClassService

	^true!

isDefined

	^isExtension not!

isExtension

	^isExtension!

isTestCase
	^isTestCase!

meta
	^meta!

meta: anObject
	meta := anObject!

methods
	^methods!

methods: anObject
	methods := anObject!

oop
	^oop!

oop: anObject
	oop := anObject!

packageName
	^packageName!

packageName: anObject
	packageName := anObject!

prepareForReplication
	super prepareForReplication.
	self shouldClearMethods ifTrue: [methods := nil].
	hierarchyServices := nil.
	selectedMethods := nil.
	methods ifNotNil: [methods do: [:methodService | methodService prepareForReplication]].
	selectedMethods ifNotNil: [selectedMethods do: [:methodService | methodService prepareForReplication]].
	selectedPackageServices do: [:packageService | packageService prepareForReplication]!

projectName
	^projectName!

releaseReferencedServices
	| copy |
	methods
		ifNotNil: 
			[copy := methods copy.
			methods := Array new.
			copy do: [:service | service releaseReferencedServices]]!

remoteServiceName
	^'Rowan classServiceClass'!

removedClass: presenter
	| removal |
	updateType == #removedClass: ifFalse: [^self].
	removal := presenter list detect:[:classService | classService name = name] ifNone:[^self ].
	presenter model remove: removal. !

replicateFrom: newService
	newService isClassService ifTrue: [^super replicateFrom: newService].!

selectedMethods

	^selectedMethods
		!

selectedPackageNames
	^selectedPackageServices
		ifNil: [Array new]
		ifNotNil: [selectedPackageServices collect: [:packageService | packageService name]]!

selectedPackageServiceNames
	^selectedPackageServices collect: [:pkgService | pkgService name]!

selectedPackageServices
	^selectedPackageServices!

selectedPackageServices: anObject
	selectedPackageServices := anObject!

shouldClearMethods

	^(#(#removeMethods) includes: command) not!

shouldColorAsExtension
	^definedPackageName = self notRowanizedPackageName
		ifTrue: [false]
		ifFalse: [(self selectedPackageNames includes: definedPackageName) not]!

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

updateListIn: presenter from: visibleMethods
	| newSelections |
	newSelections := presenter selections select:[:methodService | visibleMethods includes: methodService].
	presenter list: visibleMethods. 
	presenter selections: newSelections. !

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
!RowanClassService categoriesFor: #classHierarchyUpdate:browser:!public!updating! !
!RowanClassService categoriesFor: #classMethodsUpdate:browser:!public!updating! !
!RowanClassService categoriesFor: #classOrHierarchyPresenter:!private!updating! !
!RowanClassService categoriesFor: #comment!accessing!public! !
!RowanClassService categoriesFor: #comment:!accessing!public! !
!RowanClassService categoriesFor: #computeVisibleMethods!private!updating! !
!RowanClassService categoriesFor: #definedPackageName!accessing!private! !
!RowanClassService categoriesFor: #definedPackageName:!accessing!private! !
!RowanClassService categoriesFor: #displayMethodsOn:!public!updating! !
!RowanClassService categoriesFor: #displayString!displaying!public! !
!RowanClassService categoriesFor: #displayStringFor:!displaying!public! !
!RowanClassService categoriesFor: #excludedInstVars!public!ston! !
!RowanClassService categoriesFor: #filters!accessing!public! !
!RowanClassService categoriesFor: #filters:!accessing!public! !
!RowanClassService categoriesFor: #filterType!accessing!private! !
!RowanClassService categoriesFor: #filterType:!accessing!private! !
!RowanClassService categoriesFor: #filterUpdate:browser:!public!updating! !
!RowanClassService categoriesFor: #hash!comparing!public! !
!RowanClassService categoriesFor: #initialize!Init / Release!public! !
!RowanClassService categoriesFor: #isClassService!public!testing! !
!RowanClassService categoriesFor: #isDefined!public!testing! !
!RowanClassService categoriesFor: #isExtension!public!testing! !
!RowanClassService categoriesFor: #isTestCase!public!testing! !
!RowanClassService categoriesFor: #meta!accessing!private! !
!RowanClassService categoriesFor: #meta:!accessing!private! !
!RowanClassService categoriesFor: #methods!accessing!private! !
!RowanClassService categoriesFor: #methods:!accessing!private! !
!RowanClassService categoriesFor: #oop!accessing!private! !
!RowanClassService categoriesFor: #oop:!accessing!private! !
!RowanClassService categoriesFor: #packageName!accessing!private! !
!RowanClassService categoriesFor: #packageName:!accessing!private! !
!RowanClassService categoriesFor: #prepareForReplication!public!replication! !
!RowanClassService categoriesFor: #projectName!accessing!private! !
!RowanClassService categoriesFor: #releaseReferencedServices!Init / Release!public! !
!RowanClassService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanClassService categoriesFor: #removedClass:!public!updating! !
!RowanClassService categoriesFor: #replicateFrom:!public!replication! !
!RowanClassService categoriesFor: #selectedMethods!accessing!private! !
!RowanClassService categoriesFor: #selectedPackageNames!private! !
!RowanClassService categoriesFor: #selectedPackageServiceNames!private!updating! !
!RowanClassService categoriesFor: #selectedPackageServices!accessing!private! !
!RowanClassService categoriesFor: #selectedPackageServices:!accessing!private! !
!RowanClassService categoriesFor: #shouldClearMethods!public!replication!testing! !
!RowanClassService categoriesFor: #shouldColorAsExtension!public!testing! !
!RowanClassService categoriesFor: #stonOn:!must not strip!public!ston! !
!RowanClassService categoriesFor: #template!accessing!private! !
!RowanClassService categoriesFor: #template:!accessing!private! !
!RowanClassService categoriesFor: #updateCategories:!private!updating! !
!RowanClassService categoriesFor: #updateListIn:from:!private!updating! !
!RowanClassService categoriesFor: #updateVariables:!private!updating! !
!RowanClassService categoriesFor: #variables!accessing!private! !

!RowanClassService class methodsFor!

defaultIconName
	"Answer a filename to use for an icon of this class."

	^File composeStem: 'Class' extension: 'ico'.!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon!

named: theName
	| inst |
	inst := self new name: theName.
	^inst! !
!RowanClassService class categoriesFor: #defaultIconName!private! !
!RowanClassService class categoriesFor: #icon!private! !
!RowanClassService class categoriesFor: #named:!instance creation!public! !

