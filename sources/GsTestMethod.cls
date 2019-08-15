"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GsTestMethod
	instanceVariableNames: 'className methodName result'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsTestMethod guid: (GUID fromString: '{49ac780a-e100-43d1-812c-5132b52980ab}')!
GsTestMethod comment: ''!
!GsTestMethod categoriesForClass!Unclassified! !
!GsTestMethod methodsFor!

className
	^className!

className: anObject
	className := anObject!

icon

	result ifNil: [^self class icon].

	(result = 'passed') ifTrue: [^True icon].
	(result = 'failure') ifTrue: [^Warning icon].
	(result = 'error') ifTrue: [^False icon].

	^self class icon

	!

methodName
	^methodName!

methodName: anObject
	methodName := anObject!

result
	^result!

result: anObject
	result := anObject!

setError

	result := 'error'

	!

setFailure

	result := 'failure'!

setPassed

	result := 'passed'
	! !
!GsTestMethod categoriesFor: #className!accessing!private! !
!GsTestMethod categoriesFor: #className:!accessing!private! !
!GsTestMethod categoriesFor: #icon!public! !
!GsTestMethod categoriesFor: #methodName!accessing!private! !
!GsTestMethod categoriesFor: #methodName:!accessing!private! !
!GsTestMethod categoriesFor: #result!accessing!private! !
!GsTestMethod categoriesFor: #result:!accessing!private! !
!GsTestMethod categoriesFor: #setError!public! !
!GsTestMethod categoriesFor: #setFailure!public! !
!GsTestMethod categoriesFor: #setPassed!public! !

!GsTestMethod class methodsFor!

icon
	"Answers an <Icon> that can be used to represent this class"

	^Icon fromId: 'COMInterface.ico'!

name: aString

	^super new initialize
		methodName: aString;
		yourself!

newOn: className methodName: methodName

	^super new initialize
		className: className;
		methodName: methodName;
		yourself! !
!GsTestMethod class categoriesFor: #icon!public! !
!GsTestMethod class categoriesFor: #name:!public! !
!GsTestMethod class categoriesFor: #newOn:methodName:!public! !

