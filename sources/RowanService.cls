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

classesUpdate: presenter browser: anObject
	!

classHierarchyUpdate: presenter
	!

classHierarchyUpdate: presenter browser: browser
!

classMethodsUpdate: presenter browser: anObject
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

emptyFilterListsIn: browser
	self initializePresenterList: browser categoryListPresenter.
	self initializePresenterList: browser variableListPresenter!

filterUpdate: presenter browser: anObject
	!

frameListUpdate: aPresenter!

globalsUpdate: presenter!

initialize

	commandArgs := Array new. !

initializePresenterList: presenter
	presenter list: (SortedCollection sortBlock: [:x :y | x sortAspect < y sortAspect])!

initializeRemoteService
	remoteService := transport executeString: self remoteServiceName , ' new'!

inspectDefinition
	| definition |
	definition := transport send: #definition to: remoteService.
	JadeInspector showOn: definition session: transport!

isBrowserService

	^false!

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

methodSourceUpdate: presenter browser: anObject
	!

methodsRemoved: presenter
	!

methodsUpdate: presenter
	!

methodUpdate: presenter
	!

name
	^name!

name: aString
	name := aString.!

packagesUpdate: presenter!

prepareForReplication
	"only ston replicate what is absolutely necessary. Each
	service knows how to do that. Make this method prepares
	each command arg as well"

	commandArgs ifNotNil: [commandArgs do: [:arg | arg prepareForReplication]]!

printOn: aStream

	super printOn: aStream. 
	aStream nextPut: $:. 
	aStream nextPutAll: (name ifNil: [nil printString])!

processListUpdate: aPresenter!

projectPackagesUpdate: aPresenter browser: anObject
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

removedClass: aPresenter!

removedProject: presenter!

removeSampleSymbolDictionary

	transport send: #removeSampleSymbolDictionary to: remoteService!

replicateFrom: newService
	newService name = self name
		ifTrue: 
			[1 to: self class instSize do: [:index | self instVarAt: index put: (newService instVarAt: index)]]!

sampleDefinitions
	| anArrayOop |
	anArrayOop := transport send: #sampleDefinitions to: remoteService.
	^self createClientFrom: anArrayOop!

setRemoteName

	transport send: #name: to: remoteService withAll: (Array with: name)!

sortAspect

	^name!

stonOn: anObject

	^super stonOn: anObject   "Dolphin strips out the #stonOn: in Object. try to save it"!

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

updateList: presenter whilePreservingSelections: updates
	| replicate |
	presenter list copy do: 
			[:service |
			replicate := updates detect: [:update | update = service] ifNone: [nil].
			replicate notNil ifTrue: [service replicateFrom: replicate]].
	updates do: [:update | (presenter list includes: update) not ifTrue: [presenter model add: update]]!

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
!RowanService categoriesFor: #classesUpdate:browser:!public!updating! !
!RowanService categoriesFor: #classHierarchyUpdate:!public!updating! !
!RowanService categoriesFor: #classHierarchyUpdate:browser:!public!updating! !
!RowanService categoriesFor: #classMethodsUpdate:browser:!public!updating! !
!RowanService categoriesFor: #classNameFrom:!public! !
!RowanService categoriesFor: #classUpdate:!public!updating! !
!RowanService categoriesFor: #clearCommand!Init / Release!public! !
!RowanService categoriesFor: #clientDefinitionClass!public! !
!RowanService categoriesFor: #command!accessing!public! !
!RowanService categoriesFor: #command:!accessing!public! !
!RowanService categoriesFor: #commandArgs!accessing!public! !
!RowanService categoriesFor: #commandArgs:!accessing!public! !
!RowanService categoriesFor: #createClientFrom:!public! !
!RowanService categoriesFor: #dictionaryUpdate:!public!updating! !
!RowanService categoriesFor: #displayString!displaying!public! !
!RowanService categoriesFor: #emptyFilterListsIn:!Init / Release!private! !
!RowanService categoriesFor: #filterUpdate:browser:!public!updating! !
!RowanService categoriesFor: #frameListUpdate:!Debugger!public!updating! !
!RowanService categoriesFor: #globalsUpdate:!public!updating! !
!RowanService categoriesFor: #initialize!Init / Release!public! !
!RowanService categoriesFor: #initializePresenterList:!Init / Release!private! !
!RowanService categoriesFor: #initializeRemoteService!Init / Release!public! !
!RowanService categoriesFor: #inspectDefinition!public!rowan! !
!RowanService categoriesFor: #isBrowserService!public!testing! !
!RowanService categoriesFor: #isClassService!public!testing! !
!RowanService categoriesFor: #isDirty!accessing!public! !
!RowanService categoriesFor: #isDirty:!accessing!public! !
!RowanService categoriesFor: #isMethodService!public!testing! !
!RowanService categoriesFor: #isPackageService!public!testing! !
!RowanService categoriesFor: #isProjectService!public!testing! !
!RowanService categoriesFor: #isTestCase!public!testing! !
!RowanService categoriesFor: #methodFilterUpdate:!public!updating! !
!RowanService categoriesFor: #methodSourceUpdate:browser:!public!updating! !
!RowanService categoriesFor: #methodsRemoved:!public!updating! !
!RowanService categoriesFor: #methodsUpdate:!public!updating! !
!RowanService categoriesFor: #methodUpdate:!public!updating! !
!RowanService categoriesFor: #name!accessing!public! !
!RowanService categoriesFor: #name:!accessing!public! !
!RowanService categoriesFor: #packagesUpdate:!public!updating! !
!RowanService categoriesFor: #prepareForReplication!public!replication! !
!RowanService categoriesFor: #printOn:!printing!public! !
!RowanService categoriesFor: #processListUpdate:!Debugger!public!updating! !
!RowanService categoriesFor: #projectPackagesUpdate:browser:!public!updating! !
!RowanService categoriesFor: #projectSelectionUpdate:!public!updating! !
!RowanService categoriesFor: #projectsUpdate:!public!updating! !
!RowanService categoriesFor: #releaseReferencedServices!Init / Release!public! !
!RowanService categoriesFor: #remoteService!public! !
!RowanService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanService categoriesFor: #removedClass:!public!updating! !
!RowanService categoriesFor: #removedProject:!public!updating! !
!RowanService categoriesFor: #removeSampleSymbolDictionary!public! !
!RowanService categoriesFor: #replicateFrom:!public!replication!updating! !
!RowanService categoriesFor: #sampleDefinitions!examples!public! !
!RowanService categoriesFor: #setRemoteName!accessing!public! !
!RowanService categoriesFor: #sortAspect!accessing!public! !
!RowanService categoriesFor: #stonOn:!must not strip!public! !
!RowanService categoriesFor: #superclassListUpdate:!public!updating! !
!RowanService categoriesFor: #toolTip!printing!public! !
!RowanService categoriesFor: #transport!accessing!public! !
!RowanService categoriesFor: #transport:!accessing!public! !
!RowanService categoriesFor: #updateClassCategorySelectionsFor:!public!updating! !
!RowanService categoriesFor: #updateClassHierarchySelectionFor:!public!updating! !
!RowanService categoriesFor: #updateClassSelectionFor:!public!updating! !
!RowanService categoriesFor: #updateDictionarySelectionsFor:!public!updating! !
!RowanService categoriesFor: #updateList:whilePreservingSelections:!private! !
!RowanService categoriesFor: #updateMethodFilterSelectionFor:!public!updating! !
!RowanService categoriesFor: #updateMethodSelectionFor:!public!updating! !
!RowanService categoriesFor: #updatePackageSelectionsFor:!public!updating! !
!RowanService categoriesFor: #updateSelectorFor:!public!updating! !
!RowanService categoriesFor: #updateSuperclassSelectionFor:!public!updating! !
!RowanService categoriesFor: #updateType!accessing!public! !
!RowanService categoriesFor: #updateType:!accessing!public! !
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
!RowanService class categoriesFor: #new!initialization!public! !
!RowanService class categoriesFor: #on:!initialization!public! !
!RowanService class categoriesFor: #sampleDefinitions!examples!public! !

