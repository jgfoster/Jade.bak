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

classCreationTemplate

	^transport send: #genericClassCreationTemplate to: self remoteService!

createPackageNamed: packageName inProject: projectName

	transport send: #createPackageNamed:inProject: to: remoteService withAll: (Array with: packageName with: projectName)!

genericClassCreationTemplate

	^transport send: #genericClassCreationTemplate to: remoteService 
!

name: packageName
	super name: packageName.
	transport
		send: #name:
		to: remoteService
		withAll: (Array with: packageName)!

removeSampleSymbolDictionary

	transport send: #removeSampleSymbolDictionary to: remoteService! !
!RowanPackageDefinitionService categoriesFor: #classCreationTemplate!public!rowan! !
!RowanPackageDefinitionService categoriesFor: #createPackageNamed:inProject:!public!rowan! !
!RowanPackageDefinitionService categoriesFor: #genericClassCreationTemplate!public!rowan! !
!RowanPackageDefinitionService categoriesFor: #name:!accessing!public! !
!RowanPackageDefinitionService categoriesFor: #removeSampleSymbolDictionary!public! !
