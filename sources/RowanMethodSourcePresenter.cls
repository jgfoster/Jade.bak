"Filed out from Dolphin Smalltalk 7"!

MethodSourcePresenter subclass: #RowanMethodSourcePresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodSourcePresenter guid: (GUID fromString: '{cfc0ca1c-9636-45a9-bc23-ceca124a04cd}')!
RowanMethodSourcePresenter comment: ''!
!RowanMethodSourcePresenter categoriesForClass!Unclassified! !
!RowanMethodSourcePresenter methodsFor!

browseSelectedClass
	| range string shell |
	range := documentPresenter view selectionRange.
	string := documentPresenter value copyFrom: range start to: range stop.
	shell := RowanSystemBrowser showOnSession: self gciSession.
	shell selectClass:string selector: String new.
	!

updateMethodStepPoints

	self rowanFixMe.
	! !
!RowanMethodSourcePresenter categoriesFor: #browseSelectedClass!public! !
!RowanMethodSourcePresenter categoriesFor: #updateMethodStepPoints!public! !

