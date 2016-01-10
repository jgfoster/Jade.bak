"Filed out from Dolphin Smalltalk 7"!

VWComponent subclass: #VWNameSpace
	instanceVariableNames: 'imports'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VWNameSpace guid: (GUID fromString: '{131258E5-5281-42D3-A12C-501CAE32FBE4}')!
VWNameSpace comment: ''!
!VWNameSpace categoriesForClass!Unclassified! !
!VWNameSpace methodsFor!

readOther

	imports := (element entityAt: #'imports') elements first string trimBlanks.
! !
!VWNameSpace categoriesFor: #readOther!public! !

