"Filed out from Dolphin Smalltalk 7"!

JadeTextDocument subclass: #JadeInspector
	instanceVariableNames: 'instVarListPresenter className'
	classVariableNames: 'DefaultInspectorClass'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeInspector guid: (GUID fromString: '{7cade1f1-0352-4ff0-a34d-888a4de0cd14}')!
JadeInspector comment: ''!
!JadeInspector categoriesForClass!Unclassified! !
!JadeInspector methodsFor!

contextObject

	^model.
!

createComponents

	super createComponents.
	instVarListPresenter := self add: ListPresenter new name: 'instVarList'.
!

createSchematicWiring

	super createSchematicWiring.
	instVarListPresenter 	when: #'selectionChanged' 	send: #'selectedInstVar' 	to: self.
	instVarListPresenter 	when: #'actionPerformed' 	send: #'inspectInstVar' 	to: self.
!

inspectInstVar

	JadeInspector 
		showOn: instVarListPresenter selection value 
		session: gciSession.
!

isModified

	^false.
!

model: anObject
	"often a GsObject, but could be an Integer, String, etc."

	model := anObject.
	codePane model: anObject.
!

selectedInstVar

	| string |
	instVarListPresenter selection key = 'self' ifTrue: [
		string := instVarListPresenter selection value.
		documentPresenter value: string.
		^self.
	].
	instVarListPresenter selection key = '-.asOop' ifTrue: [
		documentPresenter value: instVarListPresenter selection value.
		^self.
	].
	string := [
		gciSession
			serverPerform: #'printStringOf:to:' 
			with: instVarListPresenter selection value
			with: 500.
	] on: GsRuntimeError do: [:ex | 
		ex errorReport number == 2106	ifTrue: [	"Forward reference error"
			ex return: 'an invalid or hidden object (perhaps a LargeObjectNode)'.
		].
		ex pass.
	].
	documentPresenter value: string.
! !
!JadeInspector categoriesFor: #contextObject!private! !
!JadeInspector categoriesFor: #createComponents!accessing!private! !
!JadeInspector categoriesFor: #createSchematicWiring!accessing!private! !
!JadeInspector categoriesFor: #inspectInstVar!accessing!private! !
!JadeInspector categoriesFor: #isModified!private! !
!JadeInspector categoriesFor: #model:!private! !
!JadeInspector categoriesFor: #selectedInstVar!accessing!private! !

!JadeInspector class methodsFor!

defaultInspectorClass

	DefaultInspectorClass isNil ifTrue: [DefaultInspectorClass := JadeInspector].

	^DefaultInspectorClass!

setDefaultInspectorClass: aJadeInspectorClass

	DefaultInspectorClass := aJadeInspectorClass
!

setNormalInspector

	self setDefaultInspectorClass: JadeInspector!

showOn: anObject session: aGciSession

	| class view |
	(anObject isKindOf: ExternalInteger) ifTrue: [	"OopType"
		class := self defaultInspectorClass.
		view := self defaultView.
	] ifFalse: [	"Special object (Boolean, SmallInteger, Character, etc."
		class := JadeInspector.
		view := 'Special view'.
	].
	^class new 
		gciSession: aGciSession;
		model: anObject;
		createView: view;
		showShell;
		yourself.
! !
!JadeInspector class categoriesFor: #defaultInspectorClass!public! !
!JadeInspector class categoriesFor: #setDefaultInspectorClass:!public! !
!JadeInspector class categoriesFor: #setNormalInspector!public! !
!JadeInspector class categoriesFor: #showOn:session:!public! !

