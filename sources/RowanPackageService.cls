"Filed out from Dolphin Smalltalk 7"!

RowanDefinitionService subclass: #RowanPackageService
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanPackageService guid: (GUID fromString: '{16c34093-697c-4a97-8953-e92983a2f084}')!
RowanPackageService comment: ''!
!RowanPackageService categoriesForClass!Kernel-Objects! !
!RowanPackageService methodsFor!

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

remoteServiceName
	"The remote service name is the same as this class"

	^'RowanPackageDefinitionService'!

removeSampleSymbolDictionary

	transport send: #removeSampleSymbolDictionary to: remoteService! !
!RowanPackageService categoriesFor: #classCreationTemplate!public!rowan! !
!RowanPackageService categoriesFor: #createPackageNamed:inProject:!public!rowan! !
!RowanPackageService categoriesFor: #genericClassCreationTemplate!public!rowan! !
!RowanPackageService categoriesFor: #name:!accessing!public! !
!RowanPackageService categoriesFor: #remoteServiceName!examples!private! !
!RowanPackageService categoriesFor: #removeSampleSymbolDictionary!public! !

