"Filed out from Dolphin Smalltalk 7"!

Object subclass: #STONReference
	instanceVariableNames: 'index'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STONReference guid: (GUID fromString: '{f73ac3bc-c704-46cd-afce-48611a8d1591}')!
STONReference comment: ''!
!STONReference categoriesForClass!Unclassified! !
!STONReference methodsFor!

= anObject
	^self class == anObject class and: [self index = anObject index]!

hash
	^ index hash
!

index
	^ index
!

index: integer
	index := integer
!

isStonReference
	^ true
!

printOn: stream
	super printOn: stream.
	stream nextPut: $(; print: index; nextPut: $)
! !
!STONReference categoriesFor: #=!public! !
!STONReference categoriesFor: #hash!public! !
!STONReference categoriesFor: #index!public! !
!STONReference categoriesFor: #index:!public! !
!STONReference categoriesFor: #isStonReference!public! !
!STONReference categoriesFor: #printOn:!public! !

!STONReference class methodsFor!

index: integer
	^ self new
		index: integer;
		yourself
! !
!STONReference class categoriesFor: #index:!public! !

