"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanBrowserService
	instanceVariableNames: 'projects removedMethods allClasses hierarchyServices testPackages testCount'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanBrowserService guid: (GUID fromString: '{9c5e8a88-ca64-403c-8f81-6c70c46926f0}')!
RowanBrowserService comment: ''!
!RowanBrowserService categoriesForClass!Unclassified! !
!RowanBrowserService methodsFor!

= projectService
	^name = projectService name!

allClasses
	^allClasses!

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

prepareForReplication
	super prepareForReplication.	
	removedMethods
		ifNotNil: [removedMethods do: [:methodService | methodService prepareForReplication]]!

projects
	^projects!

projectsUpdate: presenter
	| removals |
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	removals := presenter list select:[:projectService | (projects includes: projectService) not]. 
	presenter model removeAll: removals. 
	self updateList: presenter whilePreservingSelections: projects!

releaseReferencedServices
	| copy |
	projects
		ifNotNil: 
			[copy := projects copy.
			projects := Array new.
			copy do: [:service | service releaseReferencedServices]]!

reloadProjects: selections presenter: presenter
	self
		command: #reloadProjects:;
		commandArgs: (Array with: selections).
	presenter issueCommand: (Array with: self)!

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
								ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]]!

testCount
	^testCount!

testCount: anObject
	testCount := anObject!

testPackages: presenter
	self initializePresenterList: presenter.
	presenter model addAll: testPackages! !
!RowanBrowserService categoriesFor: #=!comparing!public! !
!RowanBrowserService categoriesFor: #allClasses!accessing!private! !
!RowanBrowserService categoriesFor: #excludedInstVars!public!ston! !
!RowanBrowserService categoriesFor: #hash!comparing!public! !
!RowanBrowserService categoriesFor: #isBrowserService!public!testing! !
!RowanBrowserService categoriesFor: #methodsRemoved:!public!updating! !
!RowanBrowserService categoriesFor: #name!accessing!public! !
!RowanBrowserService categoriesFor: #prepareForReplication!public!replication! !
!RowanBrowserService categoriesFor: #projects!accessing!public! !
!RowanBrowserService categoriesFor: #projectsUpdate:!public!updating! !
!RowanBrowserService categoriesFor: #releaseReferencedServices!public! !
!RowanBrowserService categoriesFor: #reloadProjects:presenter:!commands!public! !
!RowanBrowserService categoriesFor: #removedMethods!accessing!private! !
!RowanBrowserService categoriesFor: #removedMethods:!accessing!private! !
!RowanBrowserService categoriesFor: #replicateFrom:!public!replication!updating! !
!RowanBrowserService categoriesFor: #stonOn:!must not strip!public!ston! !
!RowanBrowserService categoriesFor: #testCount!accessing!private! !
!RowanBrowserService categoriesFor: #testCount:!accessing!private! !
!RowanBrowserService categoriesFor: #testPackages:!public!updating! !

