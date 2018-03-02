"Filed out from Dolphin Smalltalk 7"!

RowanDefinition subclass: #RowanMethodDefinition
	instanceVariableNames: 'selector source protocol className packageName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodDefinition guid: (GUID fromString: '{9311aebb-8eaf-49a9-acfd-0df6a70e0b8a}')!
RowanMethodDefinition comment: ''!
!RowanMethodDefinition categoriesForClass!Unclassified! !
!RowanMethodDefinition methodsFor!

category
	"Jade tools send #category"

	^protocol!

className
	^className!

className: anObject
	className := anObject!

initialize
	"can eliminate initialization eventually.
	Rowan should always give us every field"
	super initialize.
	source := String new.
	selector := String new.
	protocol := String new.
	className := String new.
	packageName := String new.

	!

isReadOnly
	self rowanFixMe. 
	^false!

isTestCase
	self rowanFixMe.
	^false!

packageName
	^packageName!

packageName: anObject
	packageName := anObject!

protocol
	^protocol!

protocol: anObject
	protocol := anObject!

selector
	^selector!

selector: anObject
	selector := anObject!

source
	^source!

source: anObject
	source := anObject! !
!RowanMethodDefinition categoriesFor: #category!accessing!public! !
!RowanMethodDefinition categoriesFor: #className!accessing!private! !
!RowanMethodDefinition categoriesFor: #className:!accessing!private! !
!RowanMethodDefinition categoriesFor: #initialize!public! !
!RowanMethodDefinition categoriesFor: #isReadOnly!public!testing! !
!RowanMethodDefinition categoriesFor: #isTestCase!public!testing! !
!RowanMethodDefinition categoriesFor: #packageName!accessing!private! !
!RowanMethodDefinition categoriesFor: #packageName:!accessing!private! !
!RowanMethodDefinition categoriesFor: #protocol!accessing!private! !
!RowanMethodDefinition categoriesFor: #protocol:!accessing!private! !
!RowanMethodDefinition categoriesFor: #selector!accessing!private! !
!RowanMethodDefinition categoriesFor: #selector:!accessing!private! !
!RowanMethodDefinition categoriesFor: #source!accessing!private! !
!RowanMethodDefinition categoriesFor: #source:!accessing!private! !

