"Filed out from Dolphin Smalltalk 7"!

GsMethod2 subclass: #RowanMethodDefinition
	instanceVariableNames: 'selector source className packageName projectName methodDefinitionService'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodDefinition guid: (GUID fromString: '{9311aebb-8eaf-49a9-acfd-0df6a70e0b8a}')!
RowanMethodDefinition comment: ''!
!RowanMethodDefinition categoriesForClass!Unclassified! !
!RowanMethodDefinition methodsFor!

className
	^className!

className: anObject
	className := anObject!

getPackageAndProjectFromServer
	"Rowan methods should always have a project and package.
	Get this during initialization."

	packageName  := gciSession send: #rowanPackageName to: oopType.
	projectName  := gciSession send: #rowanProjectName to: oopType.
!

initialize
	"can eliminate initialization eventually.
	Rowan should always give us every field"
	super initialize.
	source := String new.
	selector := String new.
	className := String new.
	packageName := String new.

	!

isReadOnly
	self rowanFixMe. 
	^false!

isTestCase
	self rowanFixMe.
	^false!

methodDefinitionService
	^methodDefinitionService ifNil: [methodDefinitionService := RowanMethodService on: gciSession]!

packageName
	^packageName!

packageName: anObject
	packageName := anObject!

projectName
	^projectName!

projectName: anObject
	
	projectName := anObject!

selector
	^selector!

selector: anObject
	selector := anObject!

source
	^source!

source: anObject
	source := anObject! !
!RowanMethodDefinition categoriesFor: #className!accessing!private! !
!RowanMethodDefinition categoriesFor: #className:!accessing!private! !
!RowanMethodDefinition categoriesFor: #getPackageAndProjectFromServer!initialization!private! !
!RowanMethodDefinition categoriesFor: #initialize!initialization!public! !
!RowanMethodDefinition categoriesFor: #isReadOnly!public!testing! !
!RowanMethodDefinition categoriesFor: #isTestCase!public!testing! !
!RowanMethodDefinition categoriesFor: #methodDefinitionService!public!services! !
!RowanMethodDefinition categoriesFor: #packageName!accessing!private! !
!RowanMethodDefinition categoriesFor: #packageName:!accessing!private! !
!RowanMethodDefinition categoriesFor: #projectName!accessing!private! !
!RowanMethodDefinition categoriesFor: #projectName:!accessing!private! !
!RowanMethodDefinition categoriesFor: #selector!accessing!private! !
!RowanMethodDefinition categoriesFor: #selector:!accessing!private! !
!RowanMethodDefinition categoriesFor: #source!accessing!private! !
!RowanMethodDefinition categoriesFor: #source:!accessing!private! !

!RowanMethodDefinition class methodsFor!

listFromString: aString session: aGciSession
	| list |
	list := super listFromString: aString session: aGciSession.
	list do: [:methDef | methDef getPackageAndProjectFromServer].
	^list! !
!RowanMethodDefinition class categoriesFor: #listFromString:session:!public! !

