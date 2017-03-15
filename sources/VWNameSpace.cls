"Filed out from Dolphin Smalltalk 7"!

VWComponent subclass: #VWNameSpace
	instanceVariableNames: 'imports'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VWNameSpace guid: (GUID fromString: '{131258e5-5281-42d3-a12c-501cae32fbe4}')!
VWNameSpace comment: ''!
!VWNameSpace categoriesForClass!Unclassified! !
!VWNameSpace methodsFor!

readOther

	imports := (element entityAt: #'imports') elements first string trimBlanks.
! !
!VWNameSpace categoriesFor: #readOther!public! !

