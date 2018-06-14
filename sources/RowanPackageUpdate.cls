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
	self rowanFixMe. "selections need to be handled separately"
	aPresenter list: (packages collect: [:package | package -> false]).
	modifiedPackages
		do: [:mod | aPresenter list do: [:assoc | assoc key = mod ifTrue: [assoc value: true]]].
	"aPresenter selections: (selections collect: [:x | aPresenter list detect: [:y | x = y key]]).
	aPresenter view ensureSelectionVisible"!

update: presenter
	presenter ensureVisible.
	presenter list: (packages collect: [:package | package -> false]).
	modifiedPackages
		do: [:mod | presenter list do: [:assoc | assoc key = mod ifTrue: [assoc value: true]]].
	presenter selections: (selections collect: [:x | presenter list detect: [:y | x = y key]]).
	presenter view ensureSelectionVisible!

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
!RowanPackageUpdate categoriesFor: #update:!public!updating! !
!RowanPackageUpdate categoriesFor: #updatePackageSelectionsFor:!public!updating! !

