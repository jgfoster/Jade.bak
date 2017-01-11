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

versionInfoForPackageNamed: aString version: ignored 

	^self versionInfoForPackageNamed: aString , '.package'.! !
!MCGitHubRepository categoriesFor: #fullNameOfPackage:versionName:!public! !
!MCGitHubRepository categoriesFor: #versionInfoForPackageNamed:version:!public! !

