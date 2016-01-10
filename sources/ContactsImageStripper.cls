"Filed out from Dolphin Smalltalk 7"!

JadeImageStripper subclass: #ContactsImageStripper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ContactsImageStripper guid: (GUID fromString: '{9B58C50C-364B-4F25-B578-718999CB9C5B}')!
ContactsImageStripper comment: ''!
!ContactsImageStripper categoriesForClass!Unclassified! !
!ContactsImageStripper methodsFor!

closeLoginShells

	ContactsLoginShell allInstances do: [:each | each view close].
	super closeLoginShells.
!

requiredPackageNames

	^super requiredPackageNames
		add: 'Contacts Demo';
		add: 'Jade Login';
		yourself.
! !
!ContactsImageStripper categoriesFor: #closeLoginShells!public! !
!ContactsImageStripper categoriesFor: #requiredPackageNames!public! !

