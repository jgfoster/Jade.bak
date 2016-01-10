"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #MCPackageVersion
	instanceVariableNames: 'date time author id ancestors stepChildren message'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCPackageVersion guid: (GUID fromString: '{75779A06-81A7-42DE-8521-65D983F02D77}')!
MCPackageVersion comment: ''!
!MCPackageVersion categoriesForClass!Unclassified! !
!MCPackageVersion methodsFor!

ancestors
	^ancestors!

author
	^author!

date
	^date!

id
	^id!

initialize: aList

	name 			:= name , '.mcz'.
	date 				:= aList at: 3.
	time 				:= aList at: 4.
	author 			:= aList at: 5.
	id 				:= aList at: 6.
	ancestors 		:= (aList at: 7) subStrings: Character tab.
	stepChildren 	:= (aList at: 8) subStrings: Character tab.
	message 		:= ''.
	(aList copyFrom: 9 to: aList size) do: [:each | 
		message notEmpty ifTrue: [message := message , '
'.].
		message := message , each.
	].
!

initialize: aString session: aGciSession

	| list |
	gciSession := aGciSession.
	list := aString subStrings: Character lf.
	oopType := gciSession oopTypeWithOop: (list at: 1) asNumber.
	name := list at: 2.
	self initialize: list.
!

message
	^message!

stepChildren
	^stepChildren!

time
	^time! !
!MCPackageVersion categoriesFor: #ancestors!accessing!public! !
!MCPackageVersion categoriesFor: #author!accessing!public! !
!MCPackageVersion categoriesFor: #date!accessing!public! !
!MCPackageVersion categoriesFor: #id!accessing!public! !
!MCPackageVersion categoriesFor: #initialize:!public! !
!MCPackageVersion categoriesFor: #initialize:session:!public! !
!MCPackageVersion categoriesFor: #message!accessing!public! !
!MCPackageVersion categoriesFor: #stepChildren!accessing!public! !
!MCPackageVersion categoriesFor: #time!accessing!public! !

