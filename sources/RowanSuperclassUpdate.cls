"Filed out from Dolphin Smalltalk 7"!

RowanClassesUpdate subclass: #RowanSuperclassUpdate
	instanceVariableNames: 'tabName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanSuperclassUpdate guid: (GUID fromString: '{1ac5ff42-a163-44ef-b7b3-f8830aa25edf}')!
RowanSuperclassUpdate comment: 'List of superclasses in the method pane'!
!RowanSuperclassUpdate categoriesForClass!Unclassified! !
!RowanSuperclassUpdate methodsFor!

classesUpdate: presenter browser: anObject
	!

superclassListUpdate: presenter
	presenter list: (classNames reverse collect:[:name | name asSymbol])!

updateClassSelectionFor: presenter!

updateSuperclassSelectionFor: presenter
	
	presenter selections: (selectionNames collect: [:name | name asSymbol])! !
!RowanSuperclassUpdate categoriesFor: #classesUpdate:browser:!public!updating! !
!RowanSuperclassUpdate categoriesFor: #superclassListUpdate:!public!updating! !
!RowanSuperclassUpdate categoriesFor: #updateClassSelectionFor:!public!updating! !
!RowanSuperclassUpdate categoriesFor: #updateSuperclassSelectionFor:!public!updating! !

