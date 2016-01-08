"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #GsGlobal
	instanceVariableNames: 'gsClassName data'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsGlobal guid: (GUID fromString: '{E6F20781-FF1E-4553-A0B9-5A06B141A564}')!
GsGlobal comment: ''!
!GsGlobal categoriesForClass!Unclassified! !
!GsGlobal methodsFor!

data

	^data.
!

gsClassName

	^gsClassName.
!

initialize: aList
	"override if you have something beyond OOP and name"

	super initialize: aList.
	gsClassName := aList at: 3.
	data := aList at: 4.
! !
!GsGlobal categoriesFor: #data!public! !
!GsGlobal categoriesFor: #gsClassName!public! !
!GsGlobal categoriesFor: #initialize:!public! !

