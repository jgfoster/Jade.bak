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
! !
!MCFileTreeRepository categoriesFor: #packageList!public! !

