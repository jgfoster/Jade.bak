"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #MCRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCRepository guid: (GUID fromString: '{ae19dcb0-74d7-4643-b3cb-541231353c0f}')!
MCRepository comment: ''!
!MCRepository categoriesForClass!Unclassified! !
!MCRepository methodsFor!

allVersionInfoNames

	self subclassResponsibility.
!

creationTemplate

	^gciSession
		serverPerform: #'mcCreationTemplateFor:' 
		with: self.
!

fullNameOfPackage: aString versionName: aString2 

	self subclassResponsibility.
!

initialize: aList

!

isDefault

	^false.
!

loadPackageNamed: package versionName: versionName

	self subclassResponsibility.
!

packageList
 
	| list dictionary |
	list := self allVersionInfoNames.
	list := list select: [:each | each endsWith: '.mcz'].
	dictionary := Dictionary new.
	list do: [:each | 
		| index packageName version versions |
		index := each lastIndexOf: $-.
		index = 0 ifTrue: [index := each size + 1].
		packageName := each copyFrom: 1 to: index - 1.
		version := each copyFrom: index + 1 to: each size.
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

patchFrom: string1 to: string2 

	self subclassResponsibility.!

storeVersion: anMCWorkingCopy name: nameString message: messageString from: aMCVersionDialog

	gciSession
		serverPerform: #'mcStore:name:message:repository:'
		with: anMCWorkingCopy
		with: nameString 
		with: messageString 
		with: self.
!

topazFrom: aString 

	^Error notYetImplemented!

versionInfoForPackageNamed: aString

	| string version |
	string := gciSession
		serverPerform: #'mcVersionInfoFromFileNamed:in:' 
		with: aString
		with: self.
	version := MCPackageVersion
		fromString: string
		session: gciSession.
	^version.
!

versionInfoForPackageNamed: aString version: aString2 
	^Error notYetImplemented! !
!MCRepository categoriesFor: #allVersionInfoNames!public! !
!MCRepository categoriesFor: #creationTemplate!public! !
!MCRepository categoriesFor: #fullNameOfPackage:versionName:!public! !
!MCRepository categoriesFor: #initialize:!public! !
!MCRepository categoriesFor: #isDefault!public! !
!MCRepository categoriesFor: #loadPackageNamed:versionName:!public! !
!MCRepository categoriesFor: #packageList!public! !
!MCRepository categoriesFor: #patchFrom:to:!public! !
!MCRepository categoriesFor: #storeVersion:name:message:from:!public! !
!MCRepository categoriesFor: #topazFrom:!public! !
!MCRepository categoriesFor: #versionInfoForPackageNamed:!public! !
!MCRepository categoriesFor: #versionInfoForPackageNamed:version:!public! !

!MCRepository class methodsFor!

allIn: aGciSession

	| string |
	(string := aGciSession serverPerform: #'mcRepositoryList') isNil ifTrue: [^#()].
	^self
		listFromString: string 
		session: aGciSession.
!

fromString: aString session: aGciSession

	| list type class |
	list := aString subStrings: Character tab.
	type := list at: 3.
	type = 'MCDictionaryRepository' 			ifTrue: [class := MCDictionaryRepository].
	type = 'MCDirectoryRepository' 			ifTrue: [class := MCDirectoryRepository].
	type = 'MCFileTreeRepository'				ifTrue: [class := MCFileTreeRepository].
	type = 'MCGitHubRepository'				ifTrue: [class := MCGitHubRepository].
	type = 'MCHttpRepository' 					ifTrue: [class := MCHttpRepository].
	type = 'MCServerDirectoryRepository'	ifTrue: [class := MCServerDirectoryRepository].
	class isNil ifTrue: [
		MessageBox warning: 'Unrecognized repository type ' , type printString , ' will be ignored!!' caption: 'Monticello Repository'.
		^nil.
	].
	^class new 
		initialize: aString
		session:  aGciSession.
!

gciSession: aGciSession oop: anInteger 

	| oopType string |
	oopType := aGciSession oopTypeWithOop: anInteger.
	string := aGciSession
		serverPerform: #'mcRepositoryFrom:' 
		with: oopType.
	^self 
		fromString: string
		session: aGciSession.
!

listFromString: aString session: aGciSession

	^(super listFromString: aString session: aGciSession) reject: [:each | each isNil].
! !
!MCRepository class categoriesFor: #allIn:!public! !
!MCRepository class categoriesFor: #fromString:session:!public! !
!MCRepository class categoriesFor: #gciSession:oop:!public! !
!MCRepository class categoriesFor: #listFromString:session:!public! !

