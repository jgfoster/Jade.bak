"Filed out from Dolphin Smalltalk 7"!

MCRepository subclass: #MCFileBasedRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCFileBasedRepository guid: (GUID fromString: '{02AB65A3-7933-4FF3-A1C8-7960C4414821}')!
MCFileBasedRepository comment: ''!
!MCFileBasedRepository categoriesForClass!Unclassified! !
!MCFileBasedRepository methodsFor!

allVersionInfoNames

	| string list |
	string := gciSession
		serverPerform: #'mcAllFileNamesIn:' 
		with: self.
	list := string subStrings: Character lf.
	^list.
!

fullNameOfPackage: packageName versionName: versionName 

	^packageName , '-' , versionName.!

loadPackageNamed: packageName versionName: versionName

	gciSession
		serverPerform: #'mcVersionLoad:fromFile:autoMigrate:' 
		with: (self fullNameOfPackage: packageName versionName: versionName)
		with: self
		with: true.
!

patchFrom: string1 to: string2 

	| string |
	string := gciSession
		serverPerform: #'mcPatchFrom:to:inFileBasedRepository:'
		with: string1
		with: string2
		with: self.
	^MCPatch
		fromString: string
		session: gciSession.
!

topazFrom: aString 

	^gciSession
		serverPerform: #'mcTopazFrom:inFileRepository:'
		with: aString
		with: self.
!

versionInfoForPackageNamed: packageName version: versionName 

	^self versionInfoForPackageNamed: packageName , '-' , versionName.
! !
!MCFileBasedRepository categoriesFor: #allVersionInfoNames!public! !
!MCFileBasedRepository categoriesFor: #fullNameOfPackage:versionName:!public! !
!MCFileBasedRepository categoriesFor: #loadPackageNamed:versionName:!public! !
!MCFileBasedRepository categoriesFor: #patchFrom:to:!public! !
!MCFileBasedRepository categoriesFor: #topazFrom:!public! !
!MCFileBasedRepository categoriesFor: #versionInfoForPackageNamed:version:!public! !

