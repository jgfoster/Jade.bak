"Filed out from Dolphin Smalltalk 7"!

Model subclass: #RowanService
	instanceVariableNames: 'transport remoteService name isDirty command commandArgs updateType'
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

classMethodsUpdate: presenter
	!

classNameFrom: aClassOop

	^transport send: #name to: aClassOop!

classUpdate: presenter
	!

clearCommand

	command := nil. 
	commandArgs := nil. !

clientDefinitionClass

	^self subclassResponsibility!

command
	^command!

command: anObject
	command := anObject!

commandArgs
	^commandArgs!

commandArgs: anObject
	commandArgs := anObject!

createClientFrom: anArrayOop
	| string |
	string := transport
				send: #stonStringFor:
				to: remoteService
				withAll: (Array with: anArrayOop).
	^STON fromString: string!

dictionaryUpdate: presenter
	!

displayString

	^self name!

filterUpdate: presenter
	!

frameListUpdate: aPresenter!

globalsUpdate: presenter!

initializeRemoteService
	remoteService := transport executeString: self remoteServiceName , ' new'!

inspectDefinition
	| definition |
	definition := transport send: #definition to: remoteService.
	JadeInspector showOn: definition session: transport!

isClassService

	^false!

isDirty
	^isDirty!

isDirty: anObject
	isDirty := anObject!

isMethodService

	^false!

isPackageService

	^false!

isProjectService

	^false!

isTestCase

	self rowanFixMe. "need to support running tests" 
	^false!

methodFilterUpdate: presenter
	!

methodSourceUpdate: presenter!

methodsRemoved: presenter
	!

methodsUpdate: presenter
	!

name
	^name!

name: aString
	name := aString.
	transport send: #name: to: remoteService withAll: (Array with: aString)!

packagesUpdate: presenter!

printOn: aStream

	super printOn: aStream. 
	aStream nextPut: $:. 
	aStream nextPutAll: (name ifNil: [nil printString])!

processListUpdate: aPresenter!

projectPackagesUpdate: aPresenter
!

projectSelectionUpdate: aPresenter
!

projectsUpdate: aPresenter!

releaseReferencedServices
	"Give the service a chance to clean up unnecessary objects after replication"

	!

remoteService
	^remoteService!

remoteServiceName
	self subclassResponsibility!

removeSampleSymbolDictionary

	transport send: #removeSampleSymbolDictionary to: remoteService!

replicateFrom: newService
	newService name = name
		ifTrue: 
			[1 to: self class instSize do: [:index | self instVarAt: index put: (newService instVarAt: index)]]!

sampleDefinitions
	| anArrayOop |
	anArrayOop := transport send: #sampleDefinitions to: remoteService.
	^self createClientFrom: anArrayOop!

setRemoteName

	transport send: #name: to: remoteService withAll: (Array with: name)!

superclassListUpdate: presenter
	!

toolTip
	^self displayString
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

updateSuperclassSelectionFor: presenter!

updateType
	^updateType!

updateType: anObject
	updateType := anObject!

variableDataUpdate: aPresenter!

variableListUpdate: aPresenter! !
!RowanService categoriesFor: #classCategoryUpdate:!public!updating! !
!RowanService categoriesFor: #classCommentUpdate:!public!updating! !
!RowanService categoriesFor: #classDefinitionUpdate:!public!updating! !
!RowanService categoriesFor: #classesHierarchyUpdate:!public!updating! !
!RowanService categoriesFor: #classesUpdate:!public!updating! !
!RowanService categoriesFor: #classHierarchyUpdate:!public!updating! !
!RowanService categoriesFor: #classMethodsUpdate:!public!updating! !
!RowanService categoriesFor: #classNameFrom:!public! !
!RowanService categoriesFor: #classUpdate:!public!updating! !
!RowanService categoriesFor: #clearCommand!Init / Release!public! !
!RowanService categoriesFor: #clientDefinitionClass!private! !
!RowanService categoriesFor: #command!accessing!public! !
!RowanService categoriesFor: #command:!accessing!public! !
!RowanService categoriesFor: #commandArgs!accessing!private! !
!RowanService categoriesFor: #commandArgs:!accessing!private! !
!RowanService categoriesFor: #createClientFrom:!private! !
!RowanService categoriesFor: #dictionaryUpdate:!public!updating! !
!RowanService categoriesFor: #displayString!displaying!public! !
!RowanService categoriesFor: #filterUpdate:!public!updating! !
!RowanService categoriesFor: #frameListUpdate:!Debugger!public!updating! !
!RowanService categoriesFor: #globalsUpdate:!public!updating! !
!RowanService categoriesFor: #initializeRemoteService!Init / Release!private! !
!RowanService categoriesFor: #inspectDefinition!public!rowan! !
!RowanService categoriesFor: #isClassService!public!testing! !
!RowanService categoriesFor: #isDirty!accessing!private! !
!RowanService categoriesFor: #isDirty:!accessing!private! !
!RowanService categoriesFor: #isMethodService!public!testing! !
!RowanService categoriesFor: #isPackageService!public!testing! !
!RowanService categoriesFor: #isProjectService!public!testing! !
!RowanService categoriesFor: #isTestCase!public!testing! !
!RowanService categoriesFor: #methodFilterUpdate:!public!updating! !
!RowanService categoriesFor: #methodSourceUpdate:!public!updating! !
!RowanService categoriesFor: #methodsRemoved:!public!updating! !
!RowanService categoriesFor: #methodsUpdate:!public!updating! !
!RowanService categoriesFor: #name!accessing!private! !
!RowanService categoriesFor: #name:!accessing!private! !
!RowanService categoriesFor: #packagesUpdate:!public!updating! !
!RowanService categoriesFor: #printOn:!printing!public! !
!RowanService categoriesFor: #processListUpdate:!Debugger!public!updating! !
!RowanService categoriesFor: #projectPackagesUpdate:!public!updating! !
!RowanService categoriesFor: #projectSelectionUpdate:!public!updating! !
!RowanService categoriesFor: #projectsUpdate:!public!updating! !
!RowanService categoriesFor: #releaseReferencedServices!Init / Release!public! !
!RowanService categoriesFor: #remoteService!public! !
!RowanService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanService categoriesFor: #removeSampleSymbolDictionary!public! !
!RowanService categoriesFor: #replicateFrom:!public!updating! !
!RowanService categoriesFor: #sampleDefinitions!examples!public! !
!RowanService categoriesFor: #setRemoteName!accessing!public! !
!RowanService categoriesFor: #superclassListUpdate:!public!updating! !
!RowanService categoriesFor: #toolTip!printing!public! !
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
!RowanService categoriesFor: #updateType!accessing!private! !
!RowanService categoriesFor: #updateType:!accessing!private! !
!RowanService categoriesFor: #variableDataUpdate:!Debugger!public!updating! !
!RowanService categoriesFor: #variableListUpdate:!Debugger!public!updating! !

!RowanService class methodsFor!

command: symbol withArgs: array
	| inst |
	inst := self new.
	inst
		command: symbol;
		commandArgs: array.
	^inst!

defaultIcon
	"Answer a suitable default icon to use for this class. Not all classes use their 
	default icon; one must define an appropriate #icon method in each class where the 
	icon required differs from that of the superclass."

	^(self environment at: #Icon) fromId: self defaultIconName.
!

defaultIconName
	"Answer a filename to use for an icon of this class."

	^File composeStem: 'Magic' extension: 'ico'.!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon!

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
!RowanService class categoriesFor: #command:withArgs:!instance creation!public! !
!RowanService class categoriesFor: #defaultIcon!private! !
!RowanService class categoriesFor: #defaultIconName!private! !
!RowanService class categoriesFor: #icon!private! !
!RowanService class categoriesFor: #named:!instance creation!public! !
!RowanService class categoriesFor: #new!initialization!public! !
!RowanService class categoriesFor: #on:!initialization!public! !
!RowanService class categoriesFor: #sampleDefinitions!examples!public! !

