"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanBrowserService
	instanceVariableNames: 'projects removedMethods'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanBrowserService guid: (GUID fromString: '{9c5e8a88-ca64-403c-8f81-6c70c46926f0}')!
RowanBrowserService comment: ''!
!RowanBrowserService categoriesForClass!Unclassified! !
!RowanBrowserService methodsFor!

= projectService
	^name = projectService name!

hash
	^name hash!

methodsRemoved: presenter
	| remainingServices |
	remainingServices := OrderedCollection new.
	presenter list
		do: [:methodService | (removedMethods includes: methodService) ifFalse: [remainingServices add: methodService]].
	presenter list: remainingServices!

projectsUpdate: presenter
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
	"browser services don't need replication"! !
!RowanBrowserService categoriesFor: #=!comparing!public! !
!RowanBrowserService categoriesFor: #hash!comparing!public! !
!RowanBrowserService categoriesFor: #methodsRemoved:!public!updating! !
!RowanBrowserService categoriesFor: #projectsUpdate:!public!updating! !
!RowanBrowserService categoriesFor: #releaseReferencedServices!public! !
!RowanBrowserService categoriesFor: #removedMethods!accessing!private! !
!RowanBrowserService categoriesFor: #removedMethods:!accessing!private! !
!RowanBrowserService categoriesFor: #replicateFrom:!public!replication!updating! !

