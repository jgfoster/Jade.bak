"Filed out from Dolphin Smalltalk 7"!

MCServerDirectoryRepository subclass: #MCFileTreeRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCFileTreeRepository guid: (GUID fromString: '{55009128-51c7-4147-86a7-9abd1fa99f29}')!
MCFileTreeRepository comment: ''!
!MCFileTreeRepository categoriesForClass!Unclassified! !
!MCFileTreeRepository methodsFor!

fullNameOfPackage: packageName versionName: versionName 

	^packageName , '.package'.!

packageList
 
	| list |
	list := self allVersionInfoNames.
	list := list select: [:each | each endsWith: '.package'].
	list := list collect: [:each | each copyFrom: 1 to: each size - 8].
	list := list collect: [:each | 
		MCPackage new
			name: each;
			versionNames: #('current');
			repository: self;
			yourself.
	].
	^list.
! !
!MCFileTreeRepository categoriesFor: #fullNameOfPackage:versionName:!public! !
!MCFileTreeRepository categoriesFor: #packageList!public! !

