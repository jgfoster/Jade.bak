"Filed out from Dolphin Smalltalk 7"!

RowanDefinitionService subclass: #RowanPackageDefinitionService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanPackageDefinitionService guid: (GUID fromString: '{16c34093-697c-4a97-8953-e92983a2f084}')!
RowanPackageDefinitionService comment: ''!
!RowanPackageDefinitionService categoriesForClass!Kernel-Objects! !
!RowanPackageDefinitionService methodsFor!

createPackageNamed: packageName inProject: projectName

	transport send: #createPackageNamed:inProject: to: remoteService withAll: (Array with: packageName with: projectName)!

removeSampleSymbolDictionary

	transport send: #removeSampleSymbolDictionary to: remoteService! !
!RowanPackageDefinitionService categoriesFor: #createPackageNamed:inProject:!public! !
!RowanPackageDefinitionService categoriesFor: #removeSampleSymbolDictionary!public! !

