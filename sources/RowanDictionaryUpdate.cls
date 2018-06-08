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
	aPresenter list: (dictionaries collect:[:dictName | dictName -> false]). !

updateDictionarySelectionsFor: presenter
	| sels |
	sels := presenter list select:[:sel | selections includes: sel key].
	presenter selections: sels!

updateMethod

	^#dictionaryUpdate:! !
!RowanDictionaryUpdate categoriesFor: #dictionaryUpdate:!public!updating! !
!RowanDictionaryUpdate categoriesFor: #updateDictionarySelectionsFor:!public!updating! !
!RowanDictionaryUpdate categoriesFor: #updateMethod!public!updating! !

