"Filed out from Dolphin Smalltalk 7"!

MCFileTreeRepository subclass: #MCGitHubRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCGitHubRepository guid: (GUID fromString: '{fd249a8c-2d38-4c39-88b8-a5c317d8c9cf}')!
MCGitHubRepository comment: ''!
!MCGitHubRepository categoriesForClass!Unclassified! !
!MCGitHubRepository methodsFor!

fullNameOfPackage: packageName versionName: versionName 

	^packageName , '.package'.!

versionInfoForPackageNamed: aString version: ignored 

	^self versionInfoForPackageNamed: aString , '.package'.! !
!MCGitHubRepository categoriesFor: #fullNameOfPackage:versionName:!public! !
!MCGitHubRepository categoriesFor: #versionInfoForPackageNamed:version:!public! !

