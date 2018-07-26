"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanBrowserService
	instanceVariableNames: 'projects'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanBrowserService guid: (GUID fromString: '{9c5e8a88-ca64-403c-8f81-6c70c46926f0}')!
RowanBrowserService comment: ''!
!RowanBrowserService categoriesForClass!Unclassified! !
!RowanBrowserService methodsFor!

projectsUpdate: presenter
	presenter list: projects.!

releaseReferencedServices
	| copy |
	projects
		ifNotNil: 
			[copy := projects copy.
			projects := Array new.
			copy do: [:service | service releaseReferencedServices]]! !
!RowanBrowserService categoriesFor: #projectsUpdate:!public! !
!RowanBrowserService categoriesFor: #releaseReferencedServices!public! !

