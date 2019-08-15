"Filed out from Dolphin Smalltalk 7"!

JadePreferenceObject subclass: #JadeAutoCompletationPreference
	instanceVariableNames: 'isEnabled logEnabled filterObjectMethods filterPrimitiveMethods hideClassVars hideInstClassVars hidePoolDictionaries'
	classVariableNames: 'Default'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeAutoCompletationPreference guid: (GUID fromString: '{a4e50bc1-dc2a-45a1-95a7-fda7c959fa0d}')!
JadeAutoCompletationPreference comment: ''!
!JadeAutoCompletationPreference categoriesForClass!Unclassified! !
!JadeAutoCompletationPreference methodsFor!

displayString

	^'Autocompletion Options'!

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

icon
	"Answers an Icon that can be used to represent this class"

	^TextEdit icon!

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
	logEnabled := anObject!

publishedAspects
	"Answer a <LookupTable> of the <Aspect>s published by the receiver."

	| aspects |
	aspects := super publishedAspects.
	aspects add: (Aspect boolean: #isEnabled);
		add: (Aspect boolean: #logEnabled);
		add: (Aspect boolean: #filterObjectMethods);
		add: (Aspect boolean: #filterPrimitiveMethods);
		add: (Aspect boolean: #hideClassVars);
		add: (Aspect boolean: #hideInstClassVars);
		add: (Aspect boolean: #hidePoolDictionaries).
	^aspects! !
!JadeAutoCompletationPreference categoriesFor: #displayString!public! !
!JadeAutoCompletationPreference categoriesFor: #filterObjectMethods!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #filterObjectMethods:!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #filterPrimitiveMethods!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #filterPrimitiveMethods:!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #hideClassVars!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #hideClassVars:!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #hideInstClassVars!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #hideInstClassVars:!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #hidePoolDictionaries!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #hidePoolDictionaries:!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #icon!public! !
!JadeAutoCompletationPreference categoriesFor: #initialize!public! !
!JadeAutoCompletationPreference categoriesFor: #isEnabled!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #isEnabled:!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #logEnabled!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #logEnabled:!accessing!private! !
!JadeAutoCompletationPreference categoriesFor: #publishedAspects!public! !

!JadeAutoCompletationPreference class methodsFor!

default

	Default ifNil: [Default := super new initialize].

	^Default! !
!JadeAutoCompletationPreference class categoriesFor: #default!public! !

