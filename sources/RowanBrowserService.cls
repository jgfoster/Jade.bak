"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanBrowserService
	instanceVariableNames: 'projects removedMethods allClasses hierarchyServices'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanBrowserService guid: (GUID fromString: '{9c5e8a88-ca64-403c-8f81-6c70c46926f0}')!
RowanBrowserService comment: ''!
!RowanBrowserService categoriesForClass!Unclassified! !
!RowanBrowserService methodsFor!

= projectService
	^name = projectService name!

addHierarchyService: service to: treeModel withParent: parentService
	treeModel add: service asChildOf: parentService.
	(hierarchyServices at: service ifAbsent: [^self]) do: 
			[:classService |
			self
				addHierarchyService: classService
				to: treeModel
				withParent: service]!

allClasses
	^allClasses!

classHierarchyUpdate: presenter browser: browser
	| treeModel selection |
	hierarchyServices ifNil:[^self]. 
	browser isClassListTabSelected ifTrue: [^self].
	treeModel := TreeModel new
				searchPolicy: SearchPolicy equality;
				reset.
	(hierarchyServices at: #nil) do: 
			[:classService |
			self
				addHierarchyService: classService
				to: treeModel
				withParent: nil].
	presenter model: treeModel.
	presenter view expandAll. 
	selection := (treeModel asBag detect: [:svc | svc name = name asSymbol]). 
	selection hierarchyServices: nil. 
	hierarchyServices := nil. 
	presenter selection: selection
!

excludedInstVars

	^#( 'hierarchyServices' )
!

hash
	^name hash!

isBrowserService

	^true!

methodsRemoved: presenter
	| remainingServices |
	remainingServices := OrderedCollection new.
	presenter list
		do: [:methodService | (removedMethods includes: methodService) ifFalse: [remainingServices add: methodService]].
	presenter list: remainingServices!

name
	"browser services aren't named, but (for convenience) can respond"

	^nil!

projectsUpdate: presenter
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	self updateList: presenter whilePreservingSelections: projects!

releaseReferencedServices
	| copy |
	projects
		ifNotNil: 
			[copy := projects copy.
			projects := Array new.
			copy do: [:service | service releaseReferencedServices]]!

removedMethods
	^removedMethods!

removedMethods: anObject
	removedMethods := anObject!

replicateFrom: newService
	self isBrowserService ifTrue: [super replicateFrom: newService]!

stonOn: stonWriter
	| instanceVariableNames |
	(instanceVariableNames := self class allInstVarNames reject: [:iv | self excludedInstVars includes: iv]) isEmpty
		ifTrue: [stonWriter writeObject: self do: [stonWriter encodeMap: #()]]
		ifFalse: 
			[stonWriter writeObject: self
				streamMap: 
					[:dictionary |
					instanceVariableNames do: 
							[:each |
							(self instVarAt: (instanceVariableNames indexOf: each))
								ifNotNil: [:value | dictionary at: each asSymbol put: value]
								ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]]! !
!RowanBrowserService categoriesFor: #=!comparing!public! !
!RowanBrowserService categoriesFor: #addHierarchyService:to:withParent:!private!updating! !
!RowanBrowserService categoriesFor: #allClasses!accessing!private! !
!RowanBrowserService categoriesFor: #classHierarchyUpdate:browser:!public!updating! !
!RowanBrowserService categoriesFor: #excludedInstVars!public!ston! !
!RowanBrowserService categoriesFor: #hash!comparing!public! !
!RowanBrowserService categoriesFor: #isBrowserService!public!testing! !
!RowanBrowserService categoriesFor: #methodsRemoved:!public!updating! !
!RowanBrowserService categoriesFor: #name!accessing!public! !
!RowanBrowserService categoriesFor: #projectsUpdate:!public!updating! !
!RowanBrowserService categoriesFor: #releaseReferencedServices!public! !
!RowanBrowserService categoriesFor: #removedMethods!accessing!private! !
!RowanBrowserService categoriesFor: #removedMethods:!accessing!private! !
!RowanBrowserService categoriesFor: #replicateFrom:!public!replication!updating! !
!RowanBrowserService categoriesFor: #stonOn:!must not strip!public!ston! !

