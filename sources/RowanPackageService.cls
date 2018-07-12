"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanPackageService
	instanceVariableNames: 'classes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanPackageService guid: (GUID fromString: '{16c34093-697c-4a97-8953-e92983a2f084}')!
RowanPackageService comment: ''!
!RowanPackageService categoriesForClass!Kernel-Objects! !
!RowanPackageService methodsFor!

classCreationTemplate

	^transport send: #genericClassCreationTemplate to: self remoteService!

classesUpdate: presenter
	| newList |
	newList := SortedCollection sortBlock: [:x :y | x name < y name].
	newList
		addAll: presenter list;
		addAll: classes.
	presenter list: newList!

createPackageNamed: packageName inProject: projectName

	transport send: #createPackageNamed:inProject: to: remoteService withAll: (Array with: packageName with: projectName)!

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

isPackageService

	^true!

name: packageName
	super name: packageName.
	transport
		send: #name:
		to: remoteService
		withAll: (Array with: packageName)!

remoteServiceName
	^'Rowan packageServiceClass'!

replicateFrom: newService
	^self isPackageService ifTrue: [super replicateFrom: newService]! !
!RowanPackageService categoriesFor: #classCreationTemplate!public!rowan! !
!RowanPackageService categoriesFor: #classesUpdate:!public!updating! !
!RowanPackageService categoriesFor: #createPackageNamed:inProject:!public!rowan! !
!RowanPackageService categoriesFor: #displayStringFor:!displaying!public! !
!RowanPackageService categoriesFor: #genericClassCreationTemplate!public!rowan! !
!RowanPackageService categoriesFor: #isPackageService!public!testing! !
!RowanPackageService categoriesFor: #name:!accessing!public! !
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

