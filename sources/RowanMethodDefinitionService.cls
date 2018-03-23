"Filed out from Dolphin Smalltalk 7"!

RowanDefinitionService subclass: #RowanMethodDefinitionService
	instanceVariableNames: 'source selector methodDefinitions category packageName className meta user'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodDefinitionService guid: (GUID fromString: '{f5550fdc-dbb4-4382-af82-88c561c0a9d1}')!
RowanMethodDefinitionService comment: ''!
!RowanMethodDefinitionService categoriesForClass!Kernel-Objects! !
!RowanMethodDefinitionService methodsFor!

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

remoteService
	"reduce round trips eventually"

	| serviceOop |
	serviceOop := transport executeString: 'RowanMethodDefinitionService new '.
	transport
		send: #source:
		to: serviceOop
		withAll: (Array with: source).
	transport
		send: #selector:
		to: serviceOop
		withAll: (Array with: selector).
	transport
		send: #category:
		to: serviceOop
		withAll: (Array with: category).
	transport
		send: #className:
		to: serviceOop
		withAll: (Array with: className).
	transport
		send: #packageName:
		to: serviceOop
		withAll: (Array with: packageName).
	transport
		send: #meta:
		to: serviceOop
		withAll: (Array with: meta printString).
	^serviceOop!

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
!RowanMethodDefinitionService categoriesFor: #category!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #category:!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #className!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #className:!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #clientDefinitionClass!private! !
!RowanMethodDefinitionService categoriesFor: #compileMethod:user:!public!remote! !
!RowanMethodDefinitionService categoriesFor: #createSampleMethod!examples!public! !
!RowanMethodDefinitionService categoriesFor: #implementorsOf:!method accessing!public! !
!RowanMethodDefinitionService categoriesFor: #meta!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #meta:!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #methodDefinitions!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #methodDefinitions:!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #packageName!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #packageName:!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #remoteAccessors!private! !
!RowanMethodDefinitionService categoriesFor: #remoteService!private!remote! !
!RowanMethodDefinitionService categoriesFor: #sampleClassInstance!examples!public! !
!RowanMethodDefinitionService categoriesFor: #selector!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #selector:!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #source!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #source:!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #updateSelectorFromCompiledMethod:!private! !
!RowanMethodDefinitionService categoriesFor: #user!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #user:!accessing!private! !

