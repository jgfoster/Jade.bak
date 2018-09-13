"Filed out from Dolphin Smalltalk 7"!

JadeInspector subclass: #JadeNavigationInspector
	instanceVariableNames: 'oopsCollection objectIndex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeNavigationInspector guid: (GUID fromString: '{fd0e32b8-c1ae-427c-9ba2-ae30b7d8cff9}')!
JadeNavigationInspector comment: ''!
!JadeNavigationInspector categoriesForClass!Unclassified! !
!JadeNavigationInspector methodsFor!

addOop: aGsOop

	aGsOop isNil ifTrue: [^self].
	(aGsOop isKindOf: String) ifTrue: [^self].
	(oopsCollection includes: aGsOop value) ifTrue: [^self].
	oopsCollection add: aGsOop value.
	objectIndex := objectIndex + 1.
!

getNextObject

	objectIndex := (objectIndex >= oopsCollection size) 
				ifTrue: [1]
				ifFalse:[objectIndex + 1].

	self setNewInspectedObject.
!

getPreviousObject
	objectIndex := (objectIndex <= 1) 
				ifTrue: [oopsCollection size]
				ifFalse:[objectIndex - 1].

	self setNewInspectedObject.!

initialize

	super initialize.

	oopsCollection := OrderedCollection new.

	objectIndex := 0.!

inspectInstVar
	| oop |
	oop := instVarListPresenter selection value.	"instVarListPresenter selection is <anAssociation>"
	objectIndex = oopsCollection size
		ifFalse: [(oopsCollection at: objectIndex + 1) = oop value ifTrue: [^self getNextObject]].
	(oop isKindOf: String) ifTrue: [^self].
	self model: oop.
	self addOop: oop.
	self setDocumentPresenterLabel.
	self displayObject.
	self updateCaption!

objectIndex
	^objectIndex!

objectIndex: anObject
	objectIndex := anObject!

onViewOpened

	super onViewOpened.

	self addOop: self model.!

oopsCollection
	^oopsCollection!

oopsCollection: anObject
	oopsCollection := anObject!

openNormalInspector

	instVarListPresenter hasSelection ifFalse: [^JadeInspector showOn: (self gciSession oopTypeWithOop: oopsCollection first) session: gciSession].

	^(instVarListPresenter selection key = 'self')
		ifTrue: [JadeInspector showOn: (self gciSession oopTypeWithOop: oopsCollection first) session: gciSession] 
		ifFalse: [JadeInspector showOn: instVarListPresenter selection value session: gciSession]
!

queryCommand: query
	query command == #getPreviousObject
		ifTrue: 
			[query isEnabled: objectIndex ~= 1.
			^true].
	query command == #getNextObject
		ifTrue: 
			[query isEnabled: objectIndex < oopsCollection size.
			^true].
	^false!

removeObject

	(oopsCollection size > 1) ifFalse: [^self].

	oopsCollection remove: self model value ifAbsent: [^self].

	objectIndex := (objectIndex <= 1) 
				ifTrue: [oopsCollection size]
				ifFalse:[objectIndex - 1].

	self setNewInspectedObject.
!

selectedInstVar

	instVarListPresenter hasSelection ifFalse: [^self].

	super selectedInstVar.! !
!JadeNavigationInspector categoriesFor: #addOop:!public! !
!JadeNavigationInspector categoriesFor: #getNextObject!public! !
!JadeNavigationInspector categoriesFor: #getPreviousObject!public! !
!JadeNavigationInspector categoriesFor: #initialize!public! !
!JadeNavigationInspector categoriesFor: #inspectInstVar!public! !
!JadeNavigationInspector categoriesFor: #objectIndex!accessing!private! !
!JadeNavigationInspector categoriesFor: #objectIndex:!accessing!private! !
!JadeNavigationInspector categoriesFor: #onViewOpened!public! !
!JadeNavigationInspector categoriesFor: #oopsCollection!accessing!private! !
!JadeNavigationInspector categoriesFor: #oopsCollection:!accessing!private! !
!JadeNavigationInspector categoriesFor: #openNormalInspector!public! !
!JadeNavigationInspector categoriesFor: #queryCommand:!menu handlers support!public! !
!JadeNavigationInspector categoriesFor: #removeObject!public! !
!JadeNavigationInspector categoriesFor: #selectedInstVar!public! !

