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

update: aPresenter
	super update: aPresenter.
	aPresenter list: (packages collect: [:package | package -> false]).
	modifiedPackages
		do: [:mod | aPresenter list do: [:assoc | assoc key = mod ifTrue: [assoc value: true]]].
	aPresenter selections: (selections collect: [:x | aPresenter list detect: [:y | x = y key]]).
	aPresenter view ensureSelectionVisible! !
!RowanPackageUpdate categoriesFor: #update:!public!updating! !

