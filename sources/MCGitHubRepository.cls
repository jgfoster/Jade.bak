"Filed out from Dolphin Smalltalk 7"!

MCFileTreeRepository subclass: #MCGitHubRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCGitHubRepository guid: (GUID fromString: '{FD249A8C-2D38-4C39-88B8-A5C317D8C9CF}')!
MCGitHubRepository comment: ''!
!MCGitHubRepository categoriesForClass!Unclassified! !
!MCGitHubRepository methodsFor!

fullNameOfPackage: packageName versionName: versionName 

	^packageName , '.package'.!

packageList
 
	| list dictionary |
	list := self allVersionInfoNames.
	list := list select: [:each | each endsWith: '.package'].
	dictionary := Dictionary new.
	list do: [:each | 
		| packageName version versions |
		packageName := each copyFrom: 1 to: each size - 8.
		version := 'current'.
		versions := dictionary
			at: packageName
			ifAbsentPut: [OrderedCollection new].
		versions addFirst: version.
	].
	list := OrderedCollection new.
	dictionary keysAndValuesDo: [:eachKey :eachValue | 
		list add: eachKey -> eachValue.
	].
	list := list asSet asSortedCollection.
	list := list collect: [:each | 
		MCPackage new
			name: each key;
			versionNames: each value;
			repository: self;
			yourself.
	].
	^list.
!

versionInfoForPackageNamed: aString version: ignored 

	^self versionInfoForPackageNamed: aString , '.package'.! !
!MCGitHubRepository categoriesFor: #fullNameOfPackage:versionName:!public! !
!MCGitHubRepository categoriesFor: #packageList!public! !
!MCGitHubRepository categoriesFor: #versionInfoForPackageNamed:version:!public! !

