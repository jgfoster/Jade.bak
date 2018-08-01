"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanClassService
	instanceVariableNames: 'comment instVarNames classVarNames classInstVarNames superclassName subclassType poolDictionaryNames classType meta isExtension version versions template oop allFilters filters filterType methods packageName definedPackageName selectedPackageServices selectedMethod'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanClassService guid: (GUID fromString: '{ab96bbd9-d725-4add-b635-94cec9f12a19}')!
RowanClassService comment: ''!
!RowanClassService categoriesForClass!Kernel-Objects! !
!RowanClassService methodsFor!

allFilters
	^allFilters!

allFilters: anObject
	allFilters := anObject!

classCreationTemplate

	^transport send: #classCreationTemplate to: self remoteService!

classMethodsUpdate: presenter
	| visibleMethods |
	visibleMethods := OrderedCollection new.
	(filters isEmpty or: [filterType = 'variables'])
		ifTrue: [visibleMethods := methods]
		ifFalse: 
			[filterType = 'categories'
				ifTrue: [visibleMethods := methods select: [:methodService | filters includes: methodService category]]].
	visibleMethods
		do: [:svc | svc inSelectedPackage: (self selectedPackageServiceNames includes: svc packageName)].
	presenter list: visibleMethods!

comment
	^comment!

comment: anObject
	comment := anObject!

createSampleClass
	transport send: #createSampleClass to: remoteService.  !

definedPackageName
	^definedPackageName!

definedPackageName: anObject
	definedPackageName := anObject!

displayString
	| displayString |
	displayString := self name.
	versions ~= 1
		ifTrue: [displayString := displayString , ' (' , version printString , '/' , versions printString , ')'].
	^displayString!

displayStringFor: displayThing
	isExtension ifTrue: [displayThing forecolor: Color darkMagenta ].
	^self displayString!

filters
	^filters!

filters: anObject
	filters := anObject!

filterType
	^filterType!

filterType: anObject
	filterType := anObject!

filterUpdate: presenter
	| selections availableSelections |
	presenter list = allFilters ifTrue: [^self].
	selections := presenter selections.
	presenter list: allFilters.
	availableSelections := allFilters select: [:filter | selections includes: filter].
	presenter selections: availableSelections!

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

selectedPackageServiceNames
	^selectedPackageServices collect: [:pkgService | pkgService name]!

selectedPackageServices
	^selectedPackageServices!

selectedPackageServices: anObject
	selectedPackageServices := anObject!

subclassCreationTemplate
	^transport send: #subclassCreationTemplate to: self remoteService!

template
	^template!

template: anObject
	template := anObject! !
!RowanClassService categoriesFor: #allFilters!accessing!private! !
!RowanClassService categoriesFor: #allFilters:!accessing!private! !
!RowanClassService categoriesFor: #classCreationTemplate!public! !
!RowanClassService categoriesFor: #classMethodsUpdate:!public!updating! !
!RowanClassService categoriesFor: #comment!accessing!public! !
!RowanClassService categoriesFor: #comment:!accessing!public! !
!RowanClassService categoriesFor: #createSampleClass!public! !
!RowanClassService categoriesFor: #definedPackageName!accessing!private! !
!RowanClassService categoriesFor: #definedPackageName:!accessing!private! !
!RowanClassService categoriesFor: #displayString!printing!public! !
!RowanClassService categoriesFor: #displayStringFor:!printing!public! !
!RowanClassService categoriesFor: #filters!accessing!public! !
!RowanClassService categoriesFor: #filters:!accessing!public! !
!RowanClassService categoriesFor: #filterType!accessing!private! !
!RowanClassService categoriesFor: #filterType:!accessing!private! !
!RowanClassService categoriesFor: #filterUpdate:!public!updating! !
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
!RowanClassService categoriesFor: #releaseReferencedServices!Init / Release!public! !
!RowanClassService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanClassService categoriesFor: #removeSelector:!public!rowan! !
!RowanClassService categoriesFor: #replicateFrom:!public!updating! !
!RowanClassService categoriesFor: #sampleClassName!public! !
!RowanClassService categoriesFor: #selectedPackageServiceNames!private!selection! !
!RowanClassService categoriesFor: #selectedPackageServices!accessing!private! !
!RowanClassService categoriesFor: #selectedPackageServices:!accessing!private! !
!RowanClassService categoriesFor: #subclassCreationTemplate!public! !
!RowanClassService categoriesFor: #template!accessing!private! !
!RowanClassService categoriesFor: #template:!accessing!private! !

