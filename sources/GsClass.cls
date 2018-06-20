"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #GsClass
	instanceVariableNames: 'category dictionary parent children isVisible classHistory isTestCase'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsClass guid: (GUID fromString: '{b3021acc-6eb1-40f5-93e7-6353e8db8a14}')!
GsClass comment: ''!
!GsClass categoriesForClass!Unclassified! !
!GsClass methodsFor!

addChild: aGsClass2

	children add: aGsClass2.
!

addToDictionary: aDictionary

	isVisible ifFalse: [
		| other |
		other := aDictionary
			at: oopType value
			ifAbsent: [nil].
		isVisible := other notNil and: [other isVisible].
	].
	aDictionary
		at: oopType value
		put: self.
!

category

	^category.
!

children
	^children!

classHistory
	^classHistory!

dictionary

	^dictionary
!

implementorsOf: aGsMethod

	| string |
	string := gciSession
		serverPerform: #'implementorsOf:startingAt:'
		with: aGsMethod
		with: self.
	^self class
		listFromString: string 
		session: gciSession.
!

initialize: aList
	"See JadeServer>>#_addClass: aClass toStream: aStream isVisible: aBoolean fromDictionary: aDictionary
	1. OOP; 2. key; 3. category; 4. dictionary name; 5. superclass OOP; 6. children; 7. Visible/Inherited; 8. Class History; 9. isTestCase"

	super initialize: aList.
	category := aList at: 3.
	dictionary := 4 <= aList size ifTrue: [aList at: 4] ifFalse: [''].
	aList size < 5 ifTrue: [^self].	"When we get a Class as the holder of a GsMethod, then we don't get all the details"
	parent := gciSession oopTypeWithOop: (aList at: 5) asNumber.
	children := SortedCollection sortBlock: [:a :b | a name <= b name].
	isVisible := (aList at: 7) = 'V'.
	classHistory := aList at: 8.
	classHistory := classHistory = '1/1' 
	  ifTrue: ['']
	  ifFalse: [' [' , classHistory , ']'].
	isTestCase := (aList at: 9) = 'true'.
!

isTestCase

	^isTestCase.
!

isVisible

	^isVisible.
!

nameWithVersion

	^name , classHistory.
!

parent

	^parent.
!

superclassListForMeta: aBoolean

	| string |
	string := gciSession
		serverPerform: #'superclassesOf:isMeta:'
		with: self
		with: aBoolean.
	^self class
		listFromString: string 
		session: gciSession.
!

updateFrom: aGsClass2

	isVisible := isVisible or: [aGsClass2 isVisible].
!

updateParentInDictionary: aDictionary

	parent := aDictionary
		at: parent value
		ifAbsent: [nil].
	parent notNil ifTrue: [
		parent addChild: self.
	].
! !
!GsClass categoriesFor: #addChild:!public! !
!GsClass categoriesFor: #addToDictionary:!public! !
!GsClass categoriesFor: #category!public! !
!GsClass categoriesFor: #children!accessing!public! !
!GsClass categoriesFor: #classHistory!accessing!public! !
!GsClass categoriesFor: #dictionary!accessing!public! !
!GsClass categoriesFor: #implementorsOf:!public! !
!GsClass categoriesFor: #initialize:!public! !
!GsClass categoriesFor: #isTestCase!public! !
!GsClass categoriesFor: #isVisible!public! !
!GsClass categoriesFor: #nameWithVersion!public! !
!GsClass categoriesFor: #parent!accessing!public! !
!GsClass categoriesFor: #superclassListForMeta:!public! !
!GsClass categoriesFor: #updateFrom:!public! !
!GsClass categoriesFor: #updateParentInDictionary:!public! !

