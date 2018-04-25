"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanMethodService
	instanceVariableNames: 'source selector methodDefinitions category packageName className meta user'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodService guid: (GUID fromString: '{f5550fdc-dbb4-4382-af82-88c561c0a9d1}')!
RowanMethodService comment: ''!
!RowanMethodService categoriesForClass!Kernel-Objects! !
!RowanMethodService methodsFor!

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

compileMethod: aBehavior user: user

	self rowanFixMe. "Handle comple errors"
	
	^transport send: #addOrUpdateMethod to: self remoteService.
	!

createSampleMethod
	transport send: #createSampleMethod to: remoteService.  !

implementorsOf: aString

	^methodDefinitions!

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

remoteAccessors
	^#(#source #selector)!

removeSelector
	^transport send: #removeSelector:class: to: remoteService withAll: (Array with: selector with: className)
	!

removeSelector: selector class: class

	^transport send: #removeSelector:class: to: self remoteService withAll: (Array with: selector with: class)
	!

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
!RowanMethodService categoriesFor: #category!accessing!private! !
!RowanMethodService categoriesFor: #category:!accessing!private! !
!RowanMethodService categoriesFor: #className!accessing!private! !
!RowanMethodService categoriesFor: #className:!accessing!private! !
!RowanMethodService categoriesFor: #clientDefinitionClass!private! !
!RowanMethodService categoriesFor: #compileMethod:user:!public!remote! !
!RowanMethodService categoriesFor: #createSampleMethod!examples!public! !
!RowanMethodService categoriesFor: #implementorsOf:!method accessing!public! !
!RowanMethodService categoriesFor: #meta!accessing!private! !
!RowanMethodService categoriesFor: #meta:!accessing!private! !
!RowanMethodService categoriesFor: #methodDefinitions!accessing!private! !
!RowanMethodService categoriesFor: #methodDefinitions:!accessing!private! !
!RowanMethodService categoriesFor: #packageName!accessing!private! !
!RowanMethodService categoriesFor: #packageName:!accessing!private! !
!RowanMethodService categoriesFor: #remoteAccessors!private! !
!RowanMethodService categoriesFor: #removeSelector!public!remote! !
!RowanMethodService categoriesFor: #removeSelector:class:!public!remote! !
!RowanMethodService categoriesFor: #sampleClassInstance!examples!public! !
!RowanMethodService categoriesFor: #selector!accessing!private! !
!RowanMethodService categoriesFor: #selector:!accessing!private! !
!RowanMethodService categoriesFor: #source!accessing!private! !
!RowanMethodService categoriesFor: #source:!accessing!private! !
!RowanMethodService categoriesFor: #updateSelectorFromCompiledMethod:!private! !
!RowanMethodService categoriesFor: #user!accessing!private! !
!RowanMethodService categoriesFor: #user:!accessing!private! !

