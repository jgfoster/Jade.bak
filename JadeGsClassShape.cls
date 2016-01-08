"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadeGsClassShape
	instanceVariableNames: 'name gsInstVariables gsClassVariables gsInstClassVariables gsPoolDictionaries gsMethods gsClassMethods gsClassHierarchy'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeGsClassShape guid: (GUID fromString: '{706D0CB1-9735-4DB8-9D5A-45B1CA2C55A3}')!
JadeGsClassShape comment: 'JadeGsClassShape represent individual information about GemStone/S classes.

Instance of this class are only created through the user interface JadeAutoSystemBrowserPresenter.

Each time a class is selected in the hierarchy browser an instance of this class is created and updated in the registry (JadeAutocompletationRegistry).

The Autocompletation engine use instance of this class to show autocompletation options.'!
!JadeGsClassShape categoriesForClass!Unclassified! !
!JadeGsClassShape methodsFor!

configuration
	^JadeAutocompletationConfiguration default!

existInRegistry

	^self registry includesClassNamed: name!

getAllInstVarNames

	^self registry getAllInstVarNamesOf: self

!

getAllMethods

	^self registry getAllMethodsFor: self!

getAllVarNames

	^self registry getAllVarNamesOf: self

!

getNamesAfterAssigment

	^self getAllVarNames
	"^self getAllInstVarNames"

	"^(self getAllInstVarNames, self getAllClassesNames asSortedCollection) asSortedCollection."

	!

gsClassHierarchy
	^gsClassHierarchy!

gsClassHierarchy: anObject
	gsClassHierarchy := anObject!

gsClassMethods
	^gsClassMethods!

gsClassMethods: anObject
	gsClassMethods := anObject!

gsClassVariables
	^gsClassVariables!

gsClassVariables: anObject
	gsClassVariables := anObject!

gsInstClassVariables
	^gsInstClassVariables!

gsInstClassVariables: anObject
	gsInstClassVariables := anObject!

gsInstVariables
	^gsInstVariables!

gsInstVariables: anObject
	gsInstVariables := anObject!

gsMethods
	^gsMethods!

gsMethods: anObject
	gsMethods := anObject!

gsPoolDictionaries
	^gsPoolDictionaries!

gsPoolDictionaries: anObject
	gsPoolDictionaries := anObject!

initialize

	super initialize.

	gsInstVariables := OrderedCollection new.
	gsClassVariables := OrderedCollection new.
	gsInstClassVariables := OrderedCollection new.
	gsPoolDictionaries := OrderedCollection new.
	gsMethods := LookupTable new.
	gsClassMethods := LookupTable new.!

name
	^name!

name: anObject
	name := anObject!

registry

	^JadeAutocompletationRegistry default!

update
	| previousShape |

	self existInRegistry ifFalse: [^self registry register: self].

	previousShape := self registry getClass: name.

	gsMethods isEmpty ifTrue: [gsMethods := previousShape gsMethods].
	gsClassMethods isEmpty ifTrue: [gsClassMethods := previousShape gsClassMethods].

	^self registry register: self! !
!JadeGsClassShape categoriesFor: #configuration!public! !
!JadeGsClassShape categoriesFor: #existInRegistry!public! !
!JadeGsClassShape categoriesFor: #getAllInstVarNames!public! !
!JadeGsClassShape categoriesFor: #getAllMethods!auto methods!public! !
!JadeGsClassShape categoriesFor: #getAllVarNames!public! !
!JadeGsClassShape categoriesFor: #getNamesAfterAssigment!public! !
!JadeGsClassShape categoriesFor: #gsClassHierarchy!accessing!private! !
!JadeGsClassShape categoriesFor: #gsClassHierarchy:!accessing!private! !
!JadeGsClassShape categoriesFor: #gsClassMethods!accessing!private! !
!JadeGsClassShape categoriesFor: #gsClassMethods:!accessing!private! !
!JadeGsClassShape categoriesFor: #gsClassVariables!accessing!private! !
!JadeGsClassShape categoriesFor: #gsClassVariables:!accessing!private! !
!JadeGsClassShape categoriesFor: #gsInstClassVariables!accessing!private! !
!JadeGsClassShape categoriesFor: #gsInstClassVariables:!accessing!private! !
!JadeGsClassShape categoriesFor: #gsInstVariables!accessing!private! !
!JadeGsClassShape categoriesFor: #gsInstVariables:!accessing!private! !
!JadeGsClassShape categoriesFor: #gsMethods!accessing!private! !
!JadeGsClassShape categoriesFor: #gsMethods:!accessing!private! !
!JadeGsClassShape categoriesFor: #gsPoolDictionaries!accessing!private! !
!JadeGsClassShape categoriesFor: #gsPoolDictionaries:!accessing!private! !
!JadeGsClassShape categoriesFor: #initialize!public! !
!JadeGsClassShape categoriesFor: #name!accessing!private! !
!JadeGsClassShape categoriesFor: #name:!accessing!private! !
!JadeGsClassShape categoriesFor: #registry!public! !
!JadeGsClassShape categoriesFor: #update!public! !

