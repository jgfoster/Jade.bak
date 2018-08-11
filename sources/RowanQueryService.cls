"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanQueryService
	instanceVariableNames: 'queryResults'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanQueryService guid: (GUID fromString: '{b3af0738-b123-4ff6-8610-4344673a459d}')!
RowanQueryService comment: ''!
!RowanQueryService categoriesForClass!Unclassified! !
!RowanQueryService methodsFor!

answer
	^queryResults	!

queryResults
	^queryResults!

queryResults: anObject
	queryResults := anObject!

releaseReferencedServices
	"don't release the services until the gui has used them"! !
!RowanQueryService categoriesFor: #answer!accessing!private! !
!RowanQueryService categoriesFor: #queryResults!accessing!private! !
!RowanQueryService categoriesFor: #queryResults:!accessing!private! !
!RowanQueryService categoriesFor: #releaseReferencedServices!Init / Release!public! !

