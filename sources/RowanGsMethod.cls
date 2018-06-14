"Filed out from Dolphin Smalltalk 7"!

GsMethod subclass: #RowanGsMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanGsMethod guid: (GUID fromString: '{9e12158a-9acd-40a0-a2c8-bfaf232273e0}')!
RowanGsMethod comment: 'Subclass with unique name for STON replication'!
!RowanGsMethod categoriesForClass!Unclassified! !
!RowanGsMethod methodsFor!

behavior
	behavior isNumber ifTrue: [behavior := gciSession oopTypeWithOop: behavior].
	^behavior!

gsBehavior

	^behavior!

initializeMethodSource
	source := 'newMethod: argument
		"Method comment"

	^self yourself.
'!

oopType
	oopType isNumber ifTrue: [oopType := gciSession oopTypeWithOop: oopType].
	^oopType!

session: aSession

	gciSession := aSession!

source
	source isEmpty ifTrue: [self initializeMethodSource ].
	^source! !
!RowanGsMethod categoriesFor: #behavior!accessing!public! !
!RowanGsMethod categoriesFor: #gsBehavior!accessing!public! !
!RowanGsMethod categoriesFor: #initializeMethodSource!private! !
!RowanGsMethod categoriesFor: #oopType!accessing!public! !
!RowanGsMethod categoriesFor: #session:!accessing!public! !
!RowanGsMethod categoriesFor: #source!accessing!public! !

