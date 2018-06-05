"Filed out from Dolphin Smalltalk 7"!

RowanCommandResult subclass: #RowanGlobalsUpdate
	instanceVariableNames: 'globals'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanGlobalsUpdate guid: (GUID fromString: '{5814b892-8cb7-4648-a140-d028cf234155}')!
RowanGlobalsUpdate comment: ''!
!RowanGlobalsUpdate categoriesForClass!Unclassified! !
!RowanGlobalsUpdate methodsFor!

globalsUpdate: aPresenter
	self halt. 
	aPresenter list: globals.! !
!RowanGlobalsUpdate categoriesFor: #globalsUpdate:!public! !

