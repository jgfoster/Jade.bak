"Filed out from Dolphin Smalltalk 7"!

RowanDefinitionService subclass: #RowanPackageDefinitionService
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanPackageDefinitionService guid: (GUID fromString: '{16c34093-697c-4a97-8953-e92983a2f084}')!
RowanPackageDefinitionService comment: ''!
!RowanPackageDefinitionService categoriesForClass!Kernel-Objects! !
!RowanPackageDefinitionService methodsFor!

classCreationTemplate

	^transport send: #genericClassCreationTemplate to: self remoteService!

createPackageNamed: packageName inProject: projectName

	transport send: #createPackageNamed:inProject: to: remoteService withAll: (Array with: packageName with: projectName)!

genericClassCreationTemplate

	^transport send: #genericClassCreationTemplate to: remoteService 
!

name
	^name!

name: packageName
	name := packageName.
	transport send: #name: to: remoteService withAll: (Array with: packageName).!

removeSampleSymbolDictionary

	transport send: #removeSampleSymbolDictionary to: remoteService! !
!RowanPackageDefinitionService categoriesFor: #classCreationTemplate!public! !
!RowanPackageDefinitionService categoriesFor: #createPackageNamed:inProject:!public! !
!RowanPackageDefinitionService categoriesFor: #genericClassCreationTemplate!public! !
!RowanPackageDefinitionService categoriesFor: #name!accessing!public! !
!RowanPackageDefinitionService categoriesFor: #name:!accessing!public! !
!RowanPackageDefinitionService categoriesFor: #removeSampleSymbolDictionary!public! !

