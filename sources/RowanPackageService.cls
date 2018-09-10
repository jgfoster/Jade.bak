"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanPackageService
	instanceVariableNames: 'classes defaultTemplate projectName classCompilationFailures testClasses hierarchyServices'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanPackageService guid: (GUID fromString: '{16c34093-697c-4a97-8953-e92983a2f084}')!
RowanPackageService comment: ''!
!RowanPackageService categoriesForClass!Kernel-Objects! !
!RowanPackageService methodsFor!

= packageService

	^name = packageService name!

addHierarchyService: service to: treeModel withParent: parentService
	treeModel getNodeFor: service ifAbsent: [treeModel add: service asChildOf: parentService].
	(hierarchyServices at: service ifAbsent: [^self]) do: 
			[:classService |
			self
				addHierarchyService: classService
				to: treeModel
				withParent: service]!

basicPrepareForReplication
	"don't iterate over sub colletions"
	classes := nil!

classes: anArray
	classes := anArray!

classesUpdate: presenter browser: browser
	"If the selected packages have the same class defined and extended, show the defined class"

	| condensedList |
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	browser packageListPresenter selections detect: [:packageService | packageService name = name]
		ifNone: [^self].
	condensedList := self condenseExtensions: presenter.
	self updateList: presenter whilePreservingSelections: condensedList.
	presenter list
		do: [:classService | classService selectedPackageServices: browser packageListPresenter selections].
	presenter view invalidate. 
	browser isClassSelected ifFalse: [self emptyFilterListsIn: browser]!

classHierarchyUpdate: presenter browser: browser
	| treeModel subclasses parent |
	hierarchyServices ifNil: [^self].
	hierarchyServices isEmpty ifTrue:[^self].
	browser isClassListTabSelected ifTrue: [^self].
	presenter selections notEmpty ifTrue: [(presenter selections includes: self) ifFalse: [^self]].
	treeModel := TreeModel new
				searchPolicy: SearchPolicy equality;
				reset.
	parent := nil.	
	subclasses := hierarchyServices at: #nil ifAbsent: [].
	subclasses
		ifNil: 
			[| subs |
			parent := self.
			treeModel := presenter model.
			subs := hierarchyServices at: #expand.
			1 to: subs size
				do: 
					[:index |
					| classService node |
					classService := subs at: index.
					node := treeModel getNodeFor: classService ifAbsent: [].
					node
						ifNotNil: 
							[treeModel remove: classService ifAbsent: [].
							subs at: index put: node object]].
			subclasses := subs].
	subclasses do: 
			[:classService |
			self
				addHierarchyService: classService
				to: treeModel
				withParent: parent].
	treeModel asBag
		do: [:classService | classService selectedPackageServices: browser packageListPresenter selections].
	presenter model: treeModel.
	presenter view expandAll!

condenseExtensions: presenter
	| defined newList |
	newList := OrderedCollection new.
	newList
		addAll: presenter list;
		addAll: classes.
	defined := newList select: [:svc | svc isDefined] thenCollect: [:svc | svc name].
	newList := newList reject: [:svc | svc isExtension and: [defined includes: svc name]].
	^newList!

defaultTemplate
	^defaultTemplate!

defaultTemplate: anObject
	defaultTemplate := anObject!

displayStringFor: displayThing
	name ifNil: [^self].
	self isDirty == true
		ifTrue: 
			[displayThing font
				beItalic;
				beBold.
			displayThing forecolor: Color black]!

hash
	^self name hash!

hierarchyServices: collection

	hierarchyServices := collection!

isPackageService

	^true!

prepareForReplication
	super prepareForReplication.
	self basicPrepareForReplication!

projectName
	^projectName!

projectName: anObject
	projectName := anObject!

releaseReferencedServices
	| copy |
	classes ifNil: [^self].
	copy := classes copy.
	classes := Array new.
	copy do: [:service | service releaseReferencedServices]!

remoteServiceName
	^'Rowan packageServiceClass'!

replicateFrom: newService
	^(self isPackageService and: [newService isPackageService])
		ifTrue: [super replicateFrom: newService]!

testClasses: presenter
	testClasses
		ifNotNil: 
			[self initializePresenterList: presenter.
			presenter model addAll: testClasses]! !
!RowanPackageService categoriesFor: #=!comparing!public! !
!RowanPackageService categoriesFor: #addHierarchyService:to:withParent:!private!updating! !
!RowanPackageService categoriesFor: #basicPrepareForReplication!public!replication! !
!RowanPackageService categoriesFor: #classes:!public!updating! !
!RowanPackageService categoriesFor: #classesUpdate:browser:!public!updating! !
!RowanPackageService categoriesFor: #classHierarchyUpdate:browser:!public!updating! !
!RowanPackageService categoriesFor: #condenseExtensions:!private! !
!RowanPackageService categoriesFor: #defaultTemplate!accessing!private! !
!RowanPackageService categoriesFor: #defaultTemplate:!accessing!private! !
!RowanPackageService categoriesFor: #displayStringFor:!displaying!public! !
!RowanPackageService categoriesFor: #hash!comparing!public! !
!RowanPackageService categoriesFor: #hierarchyServices:!public!updating! !
!RowanPackageService categoriesFor: #isPackageService!public!testing! !
!RowanPackageService categoriesFor: #prepareForReplication!public!replication! !
!RowanPackageService categoriesFor: #projectName!accessing!private! !
!RowanPackageService categoriesFor: #projectName:!accessing!private! !
!RowanPackageService categoriesFor: #releaseReferencedServices!Init / Release!public! !
!RowanPackageService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanPackageService categoriesFor: #replicateFrom:!public!updating! !
!RowanPackageService categoriesFor: #testClasses:!public!updating! !

!RowanPackageService class methodsFor!

defaultIconName
	"Answer a filename to use for an icon of this class."

	^File composeStem: 'Package' extension: 'ico'.!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon! !
!RowanPackageService class categoriesFor: #defaultIconName!private! !
!RowanPackageService class categoriesFor: #icon!private! !

