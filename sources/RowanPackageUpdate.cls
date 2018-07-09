"Filed out from Dolphin Smalltalk 7"!

RowanCommandResult subclass: #RowanPackageUpdate
	instanceVariableNames: 'packages modifiedPackages selections'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanPackageUpdate guid: (GUID fromString: '{4c15535a-fac8-439c-bf9d-abf04a2949f4}')!
RowanPackageUpdate comment: ''!
!RowanPackageUpdate categoriesForClass!Unclassified! !
!RowanPackageUpdate methodsFor!

packagesUpdate: aPresenter
	aPresenter list: packages asOrderedCollection. 
	!

updatePackageSelectionsFor: presenter
	| selectionAssociations |
	selectionAssociations := OrderedCollection new.
	selections do:[:sel |
		| selAssoc |
		selAssoc := presenter model detect:[:assoc | assoc key = sel] ifNone:[].
		selAssoc ifNotNil: [:value | selectionAssociations  add: value]
		].
	presenter selections: selectionAssociations! !
!RowanPackageUpdate categoriesFor: #packagesUpdate:!public!updating! !
!RowanPackageUpdate categoriesFor: #updatePackageSelectionsFor:!public!updating! !

