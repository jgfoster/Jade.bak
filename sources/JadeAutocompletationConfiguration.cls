"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadeAutocompletationConfiguration
	instanceVariableNames: 'isEnabled logEnabled filterObjectMethods filterPrimitiveMethods hideClassVars hideInstClassVars hidePoolDictionaries'
	classVariableNames: 'Default'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeAutocompletationConfiguration guid: (GUID fromString: '{001e8cda-0541-471a-9188-4ad4b58206ef}')!
JadeAutocompletationConfiguration comment: ''!
!JadeAutocompletationConfiguration categoriesForClass!Unclassified! !
!JadeAutocompletationConfiguration methodsFor!

filterObjectMethods
	^filterObjectMethods!

filterObjectMethods: anObject
	filterObjectMethods := anObject!

filterPrimitiveMethods
	^filterPrimitiveMethods!

filterPrimitiveMethods: anObject
	filterPrimitiveMethods := anObject!

hideClassVars
	^hideClassVars!

hideClassVars: anObject
	hideClassVars := anObject!

hideInstClassVars
	^hideInstClassVars!

hideInstClassVars: anObject
	hideInstClassVars := anObject!

hidePoolDictionaries
	^hidePoolDictionaries!

hidePoolDictionaries: anObject
	hidePoolDictionaries := anObject!

initialize

	super initialize.

	filterObjectMethods := false.
	filterPrimitiveMethods := false.
	isEnabled := true.
	hideClassVars := false.
	hideInstClassVars := false.
	hidePoolDictionaries := false.

	isEnabled := false.		"disable for initial release"
	logEnabled := false.		"off by default"!

isEnabled
	^isEnabled!

isEnabled: anObject
	isEnabled := anObject!

logEnabled
	^logEnabled!

logEnabled: anObject
	logEnabled := anObject! !
!JadeAutocompletationConfiguration categoriesFor: #filterObjectMethods!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #filterObjectMethods:!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #filterPrimitiveMethods!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #filterPrimitiveMethods:!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #hideClassVars!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #hideClassVars:!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #hideInstClassVars!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #hideInstClassVars:!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #hidePoolDictionaries!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #hidePoolDictionaries:!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #initialize!public! !
!JadeAutocompletationConfiguration categoriesFor: #isEnabled!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #isEnabled:!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #logEnabled!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #logEnabled:!accessing!private! !

!JadeAutocompletationConfiguration class methodsFor!

default

	Default ifNil: [Default := super new initialize].

	^Default! !
!JadeAutocompletationConfiguration class categoriesFor: #default!public! !

