"Filed out from Dolphin Smalltalk 7"!

MCRepository subclass: #MCDictionaryRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCDictionaryRepository guid: (GUID fromString: '{E3F38D70-6EDE-4358-AC87-4309206F17DB}')!
MCDictionaryRepository comment: ''!
!MCDictionaryRepository categoriesForClass!Unclassified! !
!MCDictionaryRepository methodsFor!

allVersionInfoNames

	| string list |
	string := gciSession
		serverPerform: #'mcAllVersionNamesInDict:' 
		with: self.
	list := string subStrings: Character lf.
	list := list collect: [:each | each , '.mcz'].
	^list.
!

fullNameOfPackage: packageNameString versionName: versionNameString 

	^packageNameString , '-' , versionNameString.
!

isDefault

	^true.
!

loadPackageNamed: packageName versionName: versionName

	gciSession
		serverPerform: #'mcVersionLoad:fromDictionary:autoMigrate:' 
		with: packageName , '-' , (versionName copyFrom: 1 to: versionName size - 4)
		with: self
		with: true.
!

patchFrom: string1 to: string2 

	| string |
	string := gciSession
		serverPerform: #'mcPatchFrom:to:inDictionaryRepository:'
		with: (string1 ifNotNil: [:value | value copyFrom: 1 to: value size - 4])
		with: (string2 ifNotNil: [:value | value copyFrom: 1 to: value size - 4])
		with: self.
	^MCPatch
		fromString: string
		session: gciSession.
!

topazFrom: aString 

	| string |
	string := aString.
	(string endsWith: '.mcz') ifTrue: [string := string copyFrom: 1 to: string size - 4].
	^gciSession
		serverPerform: #'mcTopazFrom:inDictionaryRepository:'
		with: string
		with: self.
!

updateVersionDialogTabIn: aMCVersionDialog

	aMCVersionDialog updateDictionaryTabWith: self.
!

versionInfoFor: aString 

	| string version |
	string := gciSession 
		serverPerform: #mcVersionInfoFromDictionaryPackageNamed:in:
		with: aString
		with: self.
	string isEmpty ifTrue: [^nil].
	version := MCPackageVersion fromString: string session: gciSession.
	^version!

versionInfoForPackageNamed: aString version: aString2 

	| string version |
	string := aString , '-' , aString2.
	(string endsWith: '.mcz') ifTrue: [string := string copyFrom: 1 to: string size - 4].
	string := gciSession 
		serverPerform: #mcVersionInfoFromDictionaryPackageNamed:in:
		with: string
		with: self.
	string isEmpty ifTrue: [^nil].
	version := MCPackageVersion fromString: string session: gciSession.
	^version! !
!MCDictionaryRepository categoriesFor: #allVersionInfoNames!public! !
!MCDictionaryRepository categoriesFor: #fullNameOfPackage:versionName:!public! !
!MCDictionaryRepository categoriesFor: #isDefault!public! !
!MCDictionaryRepository categoriesFor: #loadPackageNamed:versionName:!public! !
!MCDictionaryRepository categoriesFor: #patchFrom:to:!public! !
!MCDictionaryRepository categoriesFor: #topazFrom:!public! !
!MCDictionaryRepository categoriesFor: #updateVersionDialogTabIn:!public! !
!MCDictionaryRepository categoriesFor: #versionInfoFor:!public! !
!MCDictionaryRepository categoriesFor: #versionInfoForPackageNamed:version:!public! !

