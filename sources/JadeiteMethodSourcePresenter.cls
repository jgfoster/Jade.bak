"Filed out from Dolphin Smalltalk 7"!

RowanMethodSourcePresenter subclass: #JadeiteMethodSourcePresenter
	instanceVariableNames: 'methodService'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteMethodSourcePresenter guid: (GUID fromString: '{c31ba898-ca43-466f-9627-726fb3531dca}')!
JadeiteMethodSourcePresenter comment: ''!
!JadeiteMethodSourcePresenter categoriesForClass!Unclassified! !
!JadeiteMethodSourcePresenter methodsFor!

methodService
	^methodService!

methodService: anObject
	methodService := anObject!

update
	methodService isNil ifTrue: [^self setDefaultMethod].
	documentPresenter
		value: methodService source;
		isModified: false.
	documentPresenter view
		backcolor: JadeTextStyles default colorForNoEdits;
		isEnabled: true;
		yourself!

value: string

	documentPresenter value: string! !
!JadeiteMethodSourcePresenter categoriesFor: #methodService!accessing!private! !
!JadeiteMethodSourcePresenter categoriesFor: #methodService:!accessing!private! !
!JadeiteMethodSourcePresenter categoriesFor: #update!public!updating! !
!JadeiteMethodSourcePresenter categoriesFor: #value:!public! !

