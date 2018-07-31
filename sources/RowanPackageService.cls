"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanPackageService
	instanceVariableNames: 'classes defaultTemplate'
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
	"If the selected packages have the same class defined and extended, show the defined class"

	| newList defined |
	newList := SortedCollection sortBlock: [:x :y | x name < y name].
	newList addAll: classes.
	defined := newList select: [:svc | svc isDefined] thenCollect: [:svc | svc name].
	newList := newList reject: [:svc | svc isExtension and: [defined includes: svc name]].
	presenter list: newList!

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

isPackageService

	^true!

name: packageName
	super name: packageName.
	transport
		send: #name:
		to: remoteService
		withAll: (Array with: packageName)!

printOn: target
	"Append, to the <puttableStream>, target, a string whose characters are a 
	the same as those which would result from sending a #printString
	message to the receiver.
	N.B. This is really intended for development use. #displayOn: and #displayString
	are complementary methods for generating strings for presentation to an
	end-user."

	| name |
	name := self class name.
	target 
		nextPutAll: (name first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: name!

releaseReferencedServices
	| copy |
	classes ifNil: [^self].
	copy := classes copy.
	classes := Array new.
	copy do: [:service | service releaseReferencedServices]!

remoteServiceName
	^'Rowan packageServiceClass'!

replicateFrom: newService
	^self isPackageService ifTrue: [super replicateFrom: newService]! !
!RowanPackageService categoriesFor: #classCreationTemplate!public!rowan! !
!RowanPackageService categoriesFor: #classesUpdate:!public!updating! !
!RowanPackageService categoriesFor: #createPackageNamed:inProject:!public!rowan! !
!RowanPackageService categoriesFor: #defaultTemplate!accessing!private! !
!RowanPackageService categoriesFor: #defaultTemplate:!accessing!private! !
!RowanPackageService categoriesFor: #displayStringFor:!displaying!public! !
!RowanPackageService categoriesFor: #genericClassCreationTemplate!public!rowan! !
!RowanPackageService categoriesFor: #isPackageService!public!testing! !
!RowanPackageService categoriesFor: #name:!accessing!public! !
!RowanPackageService categoriesFor: #printOn:!printing!public! !
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

