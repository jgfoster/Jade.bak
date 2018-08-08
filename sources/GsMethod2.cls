"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #GsMethod2
	instanceVariableNames: 'gsClass category isTestCase'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsMethod2 guid: (GUID fromString: '{d21971c6-fa84-4d7b-a684-d1de5b9f0519}')!
GsMethod2 comment: ''!
!GsMethod2 categoriesForClass!Unclassified! !
!GsMethod2 methodsFor!

<= aGsMethod

	"self classCategory ~= aGsMethod classCategory ifTrue: [^self classCategory <= aGsMethod classCategory].
	self gsClass name ~= aGsMethod gsClass name ifTrue: [^self gsClass name ~= aGsMethod gsClass name].
	self category ~= aGsMethod category ifTrue: [^self category <= aGsMethod category]."
	^self name <= aGsMethod name.
!

asUpdatedVersion: stringOop
	"Answer <RowanMethodDefinition> of the new method based on the receiver."
	| stringToGetNewMethod |

	stringToGetNewMethod := stringOop, Character tab asString, 
						name, Character tab asString, 
						category, Character tab asString, 
						gsClass oopType lowPart printString, Character tab asString, 
						gsClass name, Character tab asString, 
						gsClass category, Character tab asString, 
						gsClass dictionary , Character tab asString,
						isTestCase printString.

	^GsMethod2 fromString: stringToGetNewMethod session: gciSession.
!

category
	^category!

classCategory

	^gsClass category.
!

clearBreakAtStepPoint: anInteger

	gciSession
		send: #'clearBreakAtStepPoint:'
		to: oopType	
		with: anInteger.
!

clearBreakAtStepPoint: anInteger session: aGciSession

	gciSession
		send: #'clearBreakAtStepPoint:'
		to: oopType	
		with: anInteger.
!

gsClass 
	^gsClass!

initialize: aList 
	"See JadeServer>>_addMethod:toStream:"

	| string |
	category := aList at: 3.
	string := (aList at: 4) , Character tab asString , (aList at: 5) , Character tab asString , (aList at: 6) , Character tab asString , (aList at: 7).
	gsClass := GsClass 
		fromString: string 
		session: gciSession.
	isTestCase := (aList at: 8) = 'true'.
!

isTestCase

	^isTestCase.
!

printOn: aStream

	gsClass printOn: aStream.
	aStream nextPutAll: '>>'.
	super printOn: aStream.
!

setBreakAtStepPoint: anInteger session: aGciSession

	gciSession
		send: #'setBreakAtStepPoint:'
		to: oopType
		with: anInteger.
! !
!GsMethod2 categoriesFor: #<=!public! !
!GsMethod2 categoriesFor: #asUpdatedVersion:!public! !
!GsMethod2 categoriesFor: #category!public! !
!GsMethod2 categoriesFor: #classCategory!public! !
!GsMethod2 categoriesFor: #clearBreakAtStepPoint:!Breakpoints!public! !
!GsMethod2 categoriesFor: #clearBreakAtStepPoint:session:!Breakpoints!public! !
!GsMethod2 categoriesFor: #gsClass!public! !
!GsMethod2 categoriesFor: #initialize:!public! !
!GsMethod2 categoriesFor: #isTestCase!public! !
!GsMethod2 categoriesFor: #printOn:!public! !
!GsMethod2 categoriesFor: #setBreakAtStepPoint:session:!public! !

