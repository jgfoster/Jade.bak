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

gsClass
	^gsClass!

initialize: aList 

	| string |
	category := aList at: 3.
	string := (aList at: 4) , Character tab asString , (aList at: 5) , Character tab asString , (aList at: 6).
	gsClass := GsClass 
		fromString: string 
		session: gciSession.
	isTestCase := (aList at: 7) = 'true'.
!

isTestCase

	^isTestCase.
!

printOn: aStream

	gsClass printOn: aStream.
	aStream nextPutAll: '>>'.
	super printOn: aStream.
!

setBreakAtStepPoint: anInteger

	gciSession
		send: #'setBreakAtStepPoint:'
		to: oopType	
		with: anInteger.
! !
!GsMethod2 categoriesFor: #<=!public! !
!GsMethod2 categoriesFor: #category!public! !
!GsMethod2 categoriesFor: #classCategory!public! !
!GsMethod2 categoriesFor: #clearBreakAtStepPoint:!Breakpoints!public! !
!GsMethod2 categoriesFor: #gsClass!public! !
!GsMethod2 categoriesFor: #initialize:!public! !
!GsMethod2 categoriesFor: #isTestCase!public! !
!GsMethod2 categoriesFor: #printOn:!public! !
!GsMethod2 categoriesFor: #setBreakAtStepPoint:!Breakpoints!public! !

