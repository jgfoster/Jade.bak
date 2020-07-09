"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadeGsMethodShape
	instanceVariableNames: 'ownerClass methodName source replacementSource isInstanceMethod packageName applied'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeGsMethodShape guid: (GUID fromString: '{1aa1fb1a-ff88-4de6-ba91-71450fbb3ec6}')!
JadeGsMethodShape comment: ''!
!JadeGsMethodShape categoriesForClass!Unclassified! !
!JadeGsMethodShape methodsFor!

applied
	^applied!

applied: anObject
	applied := anObject!

apply

	applied := true.!

beClassMethod

	isInstanceMethod := false!

initialize

	super initialize.

	isInstanceMethod := true.
	applied := false.!

isInstanceMethod
	^isInstanceMethod!

isInstanceMethod: anObject
	isInstanceMethod := anObject!

methodName
	^methodName!

methodName: anObject
	methodName := anObject!

ownerClass
	^ownerClass!

ownerClass: anObject
	ownerClass := anObject!

packageName
	^packageName!

packageName: anObject
	packageName := anObject!

printClass

	^isInstanceMethod ifTrue: [ownerClass] ifFalse: [ownerClass , ' class']!

printOn: target

	super printOn: target.

	target nextPutAll: '(', self printClass, '>>', methodName, ')'!

replacementSource
	^replacementSource!

replacementSource: anObject
	replacementSource := anObject!

source
	^source!

source: anObject
	source := anObject! !
!JadeGsMethodShape categoriesFor: #applied!accessing!private! !
!JadeGsMethodShape categoriesFor: #applied:!accessing!private! !
!JadeGsMethodShape categoriesFor: #apply!public! !
!JadeGsMethodShape categoriesFor: #beClassMethod!public! !
!JadeGsMethodShape categoriesFor: #initialize!public! !
!JadeGsMethodShape categoriesFor: #isInstanceMethod!accessing!private! !
!JadeGsMethodShape categoriesFor: #isInstanceMethod:!accessing!private! !
!JadeGsMethodShape categoriesFor: #methodName!accessing!private! !
!JadeGsMethodShape categoriesFor: #methodName:!accessing!private! !
!JadeGsMethodShape categoriesFor: #ownerClass!accessing!private! !
!JadeGsMethodShape categoriesFor: #ownerClass:!accessing!private! !
!JadeGsMethodShape categoriesFor: #packageName!accessing!private! !
!JadeGsMethodShape categoriesFor: #packageName:!accessing!private! !
!JadeGsMethodShape categoriesFor: #printClass!public! !
!JadeGsMethodShape categoriesFor: #printOn:!public! !
!JadeGsMethodShape categoriesFor: #replacementSource!accessing!private! !
!JadeGsMethodShape categoriesFor: #replacementSource:!accessing!private! !
!JadeGsMethodShape categoriesFor: #source!accessing!private! !
!JadeGsMethodShape categoriesFor: #source:!accessing!private! !

!JadeGsMethodShape class methodsFor!

newFor: className methodName: methodName source: source

	^super new initialize
		ownerClass: className;
		methodName: methodName;
		source: source;
		yourself! !
!JadeGsMethodShape class categoriesFor: #newFor:methodName:source:!public! !

