"Filed out from Dolphin Smalltalk 7"!

Model subclass: #RowanService
	instanceVariableNames: 'transport remoteService name isDirty updates'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanService guid: (GUID fromString: '{c91bf577-a5a9-4782-b6be-c05df3222bc9}')!
RowanService comment: ''!
!RowanService categoriesForClass!Kernel-Objects! !
!RowanService methodsFor!

classCategoryUpdate: presenter
	!

classCommentUpdate: presenter
	
	!

classDefinitionUpdate: presenter
	!

classesHierarchyUpdate: presenter
!

classesUpdate: presenter
	!

classHierarchyUpdate: presenter
	!

classNameFrom: aClassOop

	^transport send: #name to: aClassOop!

clientDefinitionClass

	^self subclassResponsibility!

createClientFrom: anArrayOop
	| string |
	string := transport
				send: #stonStringFor:
				to: remoteService
				withAll: (Array with: anArrayOop).
	^STON fromString: string!

dictionaryUpdate: presenter
	!

globalsUpdate: presenter!

initializeRemoteService
	remoteService := transport executeString: self remoteServiceName , ' new'!

inspectDefinition
	| definition |
	definition := transport send: #definition to: remoteService.
	JadeInspector showOn: definition session: transport!

isDirty
	^isDirty!

isDirty: anObject
	isDirty := anObject!

methodFilterUpdate: presenter
	!

methodsUpdate: presenter
	!

name
	^name!

name: aString
	name := aString.
	transport send: #name: to: remoteService withAll: (Array with: aString)!

packagesUpdate: presenter!

projectPackagesUpdate: aPresenter
!

projectSelectionUpdate: aPresenter
!

projectsUpdate: aPresenter!

remoteService
	^remoteService!

remoteServiceName
	self subclassResponsibility!

removeSampleSymbolDictionary

	transport send: #removeSampleSymbolDictionary to: remoteService!

sampleDefinitions
	| anArrayOop |
	anArrayOop := transport send: #sampleDefinitions to: remoteService.
	^self createClientFrom: anArrayOop!

setRemoteName

	transport send: #name: to: remoteService withAll: (Array with: name)!

superclassListUpdate: presenter
	!

transport
	^transport!

transport: anObject
	transport := anObject!

updateClassCategorySelectionsFor: presenter!

updateClassHierarchySelectionFor: presenter!

updateClassSelectionFor: presenter!

updateDictionarySelectionsFor: presenter
	!

updateMethodFilterSelectionFor: presenter!

updateMethodSelectionFor: presenter!

updatePackageSelectionsFor: presenter
	!

updateSelectorFor: presenter!

updateSuperclassSelectionFor: presenter! !
!RowanService categoriesFor: #classCategoryUpdate:!public!updating! !
!RowanService categoriesFor: #classCommentUpdate:!public!updating! !
!RowanService categoriesFor: #classDefinitionUpdate:!public!updating! !
!RowanService categoriesFor: #classesHierarchyUpdate:!public!updating! !
!RowanService categoriesFor: #classesUpdate:!public!updating! !
!RowanService categoriesFor: #classHierarchyUpdate:!public!updating! !
!RowanService categoriesFor: #classNameFrom:!public! !
!RowanService categoriesFor: #clientDefinitionClass!private! !
!RowanService categoriesFor: #createClientFrom:!private! !
!RowanService categoriesFor: #dictionaryUpdate:!public!updating! !
!RowanService categoriesFor: #globalsUpdate:!public!updating! !
!RowanService categoriesFor: #initializeRemoteService!initialization!private! !
!RowanService categoriesFor: #inspectDefinition!public!rowan! !
!RowanService categoriesFor: #isDirty!accessing!private! !
!RowanService categoriesFor: #isDirty:!accessing!private! !
!RowanService categoriesFor: #methodFilterUpdate:!public!updating! !
!RowanService categoriesFor: #methodsUpdate:!public!updating! !
!RowanService categoriesFor: #name!accessing!private! !
!RowanService categoriesFor: #name:!accessing!private! !
!RowanService categoriesFor: #packagesUpdate:!public!updating! !
!RowanService categoriesFor: #projectPackagesUpdate:!public!updating! !
!RowanService categoriesFor: #projectSelectionUpdate:!public!updating! !
!RowanService categoriesFor: #projectsUpdate:!public!updating! !
!RowanService categoriesFor: #remoteService!public! !
!RowanService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanService categoriesFor: #removeSampleSymbolDictionary!public! !
!RowanService categoriesFor: #sampleDefinitions!examples!public! !
!RowanService categoriesFor: #setRemoteName!accessing!public! !
!RowanService categoriesFor: #superclassListUpdate:!public!updating! !
!RowanService categoriesFor: #transport!accessing!private! !
!RowanService categoriesFor: #transport:!accessing!private! !
!RowanService categoriesFor: #updateClassCategorySelectionsFor:!public!updating! !
!RowanService categoriesFor: #updateClassHierarchySelectionFor:!public!updating! !
!RowanService categoriesFor: #updateClassSelectionFor:!public!updating! !
!RowanService categoriesFor: #updateDictionarySelectionsFor:!public!updating! !
!RowanService categoriesFor: #updateMethodFilterSelectionFor:!public!updating! !
!RowanService categoriesFor: #updateMethodSelectionFor:!public!updating! !
!RowanService categoriesFor: #updatePackageSelectionsFor:!public!updating! !
!RowanService categoriesFor: #updateSelectorFor:!public!updating! !
!RowanService categoriesFor: #updateSuperclassSelectionFor:!public!updating! !

!RowanService class methodsFor!

named: projectName
	| inst |
	inst := self new name: projectName.
	inst setRemoteName.
	^inst!

new
	^super new initialize!

on: aSession
	| inst |
	inst := self new.
	inst transport: aSession.
	inst initializeRemoteService.
	^inst!

sampleDefinitions
	^self new sampleDefinitions! !
!RowanService class categoriesFor: #named:!instance creation!public! !
!RowanService class categoriesFor: #new!initialization!public! !
!RowanService class categoriesFor: #on:!initialization!public! !
!RowanService class categoriesFor: #sampleDefinitions!examples!public! !

