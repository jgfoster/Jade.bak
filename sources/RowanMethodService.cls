"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanMethodService
	instanceVariableNames: 'source selector methodDefinitions category packageName className meta user hasSupers hasSubs isExtension inSelectedPackage references'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodService guid: (GUID fromString: '{f5550fdc-dbb4-4382-af82-88c561c0a9d1}')!
RowanMethodService comment: ''!
!RowanMethodService categoriesForClass!Kernel-Objects! !
!RowanMethodService methodsFor!

basicSubSuperIcon
	hasSupers ifTrue: [^JadeSystemBrowserPresenter overriddenIcon].
	hasSubs ifTrue: [^RowanProjectsBrowserPresenter downArrowIcon].
	^Icon blank!

category
	^category!

category: anObject
	category := anObject!

className
	^className!

className: anObject
	className := anObject!

clientDefinitionClass

	^RowanMethodDefinition!

createSampleMethod
	transport send: #createSampleMethod to: remoteService.  !

displayOn: aStream
	aStream
		nextPutAll: selector;
		nextPutAll: '>>';
		nextPutAll: selector!

displayStringFor: displayThing
	isExtension
		ifTrue: 
			[displayThing forecolor: Color darkMagenta.
			inSelectedPackage ifFalse: [displayThing font beItalic; beUnderlined ]]!

hasSubs
	^hasSubs!

hasSubs: anObject
	hasSubs := anObject!

hasSupers
	^hasSupers!

hasSupers: anObject
	hasSupers := anObject!

implementorsOf: aString

	^methodDefinitions!

initialize
	super initialize. 
	inSelectedPackage := true.  !

inSelectedPackage

	^inSelectedPackage!

inSelectedPackage: anObject
	inSelectedPackage := anObject!

isExtension
	^isExtension!

isExtension: anObject
	isExtension := anObject!

isMethodService

	^true!

meta
	^meta!

meta: anObject
	meta := anObject!

methodDefinitions
	^methodDefinitions!

methodDefinitions: anObject
	methodDefinitions := anObject.
	self trigger: #changed!

packageName
	^packageName!

packageName: anObject
	packageName := anObject!

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

references
	^references!

references: anObject
	references := anObject!

remoteServiceName
	^'Rowan methodServiceClass'!

removeSelector
	^transport send: #removeSelector:class: to: remoteService withAll: (Array with: selector with: className)
	!

removeSelector: aSelector class: class

	^transport send: #removeSelector:class: to: self remoteService withAll: (Array with: aSelector with: class)
	!

replicateFrom: newService
	^self isMethodService ifTrue: [super replicateFrom: newService]!

sampleClassInstance

	^transport send: #sampleClassInstance to: remoteService!

selector
	^selector!

selector: anObject
	selector := anObject!

source
	^source!

source: anObject
	source := anObject!

subSuperIcon
	^(self basicSubSuperIcon) imageIndex!

updateSelectorFromCompiledMethod: aString

	| stream | 
	stream := ReadStream on: aString. 
	stream skipWhile:[:char | char isDigit].
	selector := stream upToEnd. 


!

user
	^user!

user: anObject
	user := anObject! !
!RowanMethodService categoriesFor: #basicSubSuperIcon!displaying!public! !
!RowanMethodService categoriesFor: #category!accessing!private! !
!RowanMethodService categoriesFor: #category:!accessing!private! !
!RowanMethodService categoriesFor: #className!accessing!private! !
!RowanMethodService categoriesFor: #className:!accessing!private! !
!RowanMethodService categoriesFor: #clientDefinitionClass!private! !
!RowanMethodService categoriesFor: #createSampleMethod!examples!public! !
!RowanMethodService categoriesFor: #displayOn:!displaying!public! !
!RowanMethodService categoriesFor: #displayStringFor:!displaying!public! !
!RowanMethodService categoriesFor: #hasSubs!accessing!private! !
!RowanMethodService categoriesFor: #hasSubs:!accessing!private! !
!RowanMethodService categoriesFor: #hasSupers!accessing!private! !
!RowanMethodService categoriesFor: #hasSupers:!accessing!private! !
!RowanMethodService categoriesFor: #implementorsOf:!method accessing!public! !
!RowanMethodService categoriesFor: #initialize!initialization!public! !
!RowanMethodService categoriesFor: #inSelectedPackage!public!testing! !
!RowanMethodService categoriesFor: #inSelectedPackage:!accessing!public! !
!RowanMethodService categoriesFor: #isExtension!accessing!public! !
!RowanMethodService categoriesFor: #isExtension:!accessing!public! !
!RowanMethodService categoriesFor: #isMethodService!public!testing! !
!RowanMethodService categoriesFor: #meta!accessing!private! !
!RowanMethodService categoriesFor: #meta:!accessing!private! !
!RowanMethodService categoriesFor: #methodDefinitions!accessing!private! !
!RowanMethodService categoriesFor: #methodDefinitions:!accessing!private! !
!RowanMethodService categoriesFor: #packageName!accessing!private! !
!RowanMethodService categoriesFor: #packageName:!accessing!private! !
!RowanMethodService categoriesFor: #printOn:!printing!public! !
!RowanMethodService categoriesFor: #references!accessing!private! !
!RowanMethodService categoriesFor: #references:!accessing!private! !
!RowanMethodService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanMethodService categoriesFor: #removeSelector!public!remote! !
!RowanMethodService categoriesFor: #removeSelector:class:!public!remote! !
!RowanMethodService categoriesFor: #replicateFrom:!public!updating! !
!RowanMethodService categoriesFor: #sampleClassInstance!examples!public! !
!RowanMethodService categoriesFor: #selector!accessing!private! !
!RowanMethodService categoriesFor: #selector:!accessing!private! !
!RowanMethodService categoriesFor: #source!accessing!private! !
!RowanMethodService categoriesFor: #source:!accessing!private! !
!RowanMethodService categoriesFor: #subSuperIcon!displaying!public! !
!RowanMethodService categoriesFor: #updateSelectorFromCompiledMethod:!private! !
!RowanMethodService categoriesFor: #user!accessing!private! !
!RowanMethodService categoriesFor: #user:!accessing!private! !

