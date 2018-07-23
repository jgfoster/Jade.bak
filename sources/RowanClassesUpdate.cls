"Filed out from Dolphin Smalltalk 7"!

RowanCommandResult subclass: #RowanClassesUpdate
	instanceVariableNames: 'classNames selectionNames'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanClassesUpdate guid: (GUID fromString: '{25ac5151-b39b-4ede-b826-a3d41a790bf4}')!
RowanClassesUpdate comment: ''!
!RowanClassesUpdate categoriesForClass!Unclassified! !
!RowanClassesUpdate methodsFor!

classesUpdate: presenter

	presenter list: classNames!

updateClassSelectionFor: presenter
	| symbols |
	symbols := selectionNames collect:[:name | name asSymbol].
	presenter selections: symbols! !
!RowanClassesUpdate categoriesFor: #classesUpdate:!public!updating! !
!RowanClassesUpdate categoriesFor: #updateClassSelectionFor:!public!updating! !

