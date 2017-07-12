"Filed out from Dolphin Smalltalk 7"!

JadeServer64bit32 subclass: #JadeServer64bit34
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer64bit34 guid: (GUID fromString: '{1fe2384e-a7cc-4e95-8b1e-52399b34f562}')!
JadeServer64bit34 comment: ''!
!JadeServer64bit34 categoriesForClass!Unclassified! !
!JadeServer64bit34 methodsFor!

classOrganizer

	| org |
	^(org := classOrganizers at: environment + 1) notNil
		ifTrue: [org]
		ifFalse: [classOrganizers at: environment + 1 put: (ClassOrganizer newForEnvironment: environment)]
! !
!JadeServer64bit34 categoriesFor: #classOrganizer!public! !

