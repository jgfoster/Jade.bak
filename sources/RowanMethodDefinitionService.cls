"Filed out from Dolphin Smalltalk 7"!

RowanDefinitionService subclass: #RowanMethodDefinitionService
	instanceVariableNames: 'source selector'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodDefinitionService guid: (GUID fromString: '{f5550fdc-dbb4-4382-af82-88c561c0a9d1}')!
RowanMethodDefinitionService comment: ''!
!RowanMethodDefinitionService categoriesForClass!Kernel-Objects! !
!RowanMethodDefinitionService methodsFor!

initializeFromOop: anOop

	definitionOop := anOop. 
	source := transport rowanMethodSourceFor: anOop. 
	selector := (transport rowanMethodSelectorFor: anOop) asSymbol.!

selector
	^selector!

selector: anObject
	selector := anObject!

source
	^source!

source: anObject
	source := anObject! !
!RowanMethodDefinitionService categoriesFor: #initializeFromOop:!initialization!public! !
!RowanMethodDefinitionService categoriesFor: #selector!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #selector:!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #source!accessing!private! !
!RowanMethodDefinitionService categoriesFor: #source:!accessing!private! !

