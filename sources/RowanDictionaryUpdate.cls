"Filed out from Dolphin Smalltalk 7"!

RowanCommandResult subclass: #RowanDictionaryUpdate
	instanceVariableNames: 'dictionaries selections'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanDictionaryUpdate guid: (GUID fromString: '{83fbbd49-2c42-4bef-8463-1e3264cbcc43}')!
RowanDictionaryUpdate comment: ''!
!RowanDictionaryUpdate categoriesForClass!Unclassified! !
!RowanDictionaryUpdate methodsFor!

dictionaryUpdate: aPresenter
	self rowanFixMe. "Selections should only be done on the requesting window"
	aPresenter list: (dictionaries collect:[:dictName | dictName -> false]). 
	"aPresenter selections: selections.
	aPresenter view ensureSelectionVisible"!

updateMethod

	^#dictionaryUpdate:! !
!RowanDictionaryUpdate categoriesFor: #dictionaryUpdate:!public!updating! !
!RowanDictionaryUpdate categoriesFor: #updateMethod!public! !

