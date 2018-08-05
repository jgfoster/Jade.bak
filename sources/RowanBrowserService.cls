"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanBrowserService
	instanceVariableNames: 'projects removedMethods allClasses'
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

classHierarchyUpdate: presenter browser: browser!

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
	self isBrowserService ifTrue: [super replicateFrom: newService]! !
!RowanBrowserService categoriesFor: #=!comparing!public! !
!RowanBrowserService categoriesFor: #allClasses!accessing!private! !
!RowanBrowserService categoriesFor: #classHierarchyUpdate:browser:!public!updating! !
!RowanBrowserService categoriesFor: #hash!comparing!public! !
!RowanBrowserService categoriesFor: #isBrowserService!public!testing! !
!RowanBrowserService categoriesFor: #methodsRemoved:!public!updating! !
!RowanBrowserService categoriesFor: #name!accessing!public! !
!RowanBrowserService categoriesFor: #projectsUpdate:!public!updating! !
!RowanBrowserService categoriesFor: #releaseReferencedServices!public! !
!RowanBrowserService categoriesFor: #removedMethods!accessing!private! !
!RowanBrowserService categoriesFor: #removedMethods:!accessing!private! !
!RowanBrowserService categoriesFor: #replicateFrom:!public!replication!updating! !

