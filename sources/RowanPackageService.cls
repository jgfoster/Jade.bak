"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanPackageService
	instanceVariableNames: 'classes defaultTemplate projectName classCompilationFailures'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanPackageService guid: (GUID fromString: '{16c34093-697c-4a97-8953-e92983a2f084}')!
RowanPackageService comment: ''!
!RowanPackageService categoriesForClass!Kernel-Objects! !
!RowanPackageService methodsFor!

= packageService

	^name = packageService name!

classCreationTemplate

	^transport send: #genericClassCreationTemplate to: self remoteService!

classes: anArray

	classes := anArray!

classesUpdate: presenter browser: browser
	"If the selected packages have the same class defined and extended, show the defined class"

	| condensedList |
	browser isClassListTabSelected ifFalse: [^self].
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	browser packageListPresenter selections detect: [:packageService | packageService name = name]
		ifNone: [^self].
	condensedList := self condenseExtensions: presenter.
	self updateList: presenter whilePreservingSelections: condensedList.
	browser isClassSelected ifFalse: [self emptyFilterListsIn: browser]!

condenseExtensions: presenter
	| defined newList |
	newList := OrderedCollection new.
	newList
		addAll: presenter list;
		addAll: classes.
	defined := newList select: [:svc | svc isDefined] thenCollect: [:svc | svc name].
	newList := newList reject: [:svc | svc isExtension and: [defined includes: svc name]].
	^newList!

createPackageNamed: packageName inProject: projectName

	transport send: #createPackageNamed:inProject: to: remoteService withAll: (Array with: packageName with: projectName)!

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

genericClassCreationTemplate

	^transport send: #genericClassCreationTemplate to: remoteService 
!

hash
	^self name hash!

isPackageService

	^true!

name: packageName
	super name: packageName.
	transport
		send: #name:
		to: remoteService
		withAll: (Array with: packageName)!

prepareForReplication

	classes := nil. !

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
		ifTrue: [super replicateFrom: newService]! !
!RowanPackageService categoriesFor: #=!comparing!public! !
!RowanPackageService categoriesFor: #classCreationTemplate!public!rowan! !
!RowanPackageService categoriesFor: #classes:!public!updating! !
!RowanPackageService categoriesFor: #classesUpdate:browser:!public!updating! !
!RowanPackageService categoriesFor: #condenseExtensions:!private! !
!RowanPackageService categoriesFor: #createPackageNamed:inProject:!public!rowan! !
!RowanPackageService categoriesFor: #defaultTemplate!accessing!private! !
!RowanPackageService categoriesFor: #defaultTemplate:!accessing!private! !
!RowanPackageService categoriesFor: #displayStringFor:!displaying!public! !
!RowanPackageService categoriesFor: #genericClassCreationTemplate!public!rowan! !
!RowanPackageService categoriesFor: #hash!comparing!public! !
!RowanPackageService categoriesFor: #isPackageService!public!testing! !
!RowanPackageService categoriesFor: #name:!accessing!public! !
!RowanPackageService categoriesFor: #prepareForReplication!public!replication! !
!RowanPackageService categoriesFor: #projectName!accessing!private! !
!RowanPackageService categoriesFor: #projectName:!accessing!private! !
!RowanPackageService categoriesFor: #releaseReferencedServices!Init / Release!public! !
!RowanPackageService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanPackageService categoriesFor: #replicateFrom:!public!updating! !

!RowanPackageService class methodsFor!

defaultIconName
	"Answer a filename to use for an icon of this class."

	^File composeStem: 'Package' extension: 'ico'.!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon! !
!RowanPackageService class categoriesFor: #defaultIconName!private! !
!RowanPackageService class categoriesFor: #icon!private! !

