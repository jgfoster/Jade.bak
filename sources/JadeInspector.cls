"Filed out from Dolphin Smalltalk 7"!

JadeTextDocument subclass: #JadeInspector
	instanceVariableNames: 'instVarListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeInspector guid: (GUID fromString: '{7CADE1F1-0352-4FF0-A34D-888A4DE0CD14}')!
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

displayObject

	| result stream className oop size instVarNames |
	result := gciSession 
		serverPerform: #'inspect:' 
		with: model.
	(result isKindOf: ByteArray) ifTrue: [result := result asString].
	(result isKindOf: String) ifFalse: [self error: 'Result of #inspect: on ' , model printString , ' should be a String but is ' , model class name].
	stream := ReadStream on: result.
	className := stream upTo: Character tab.
	self setLastGsShape: className.
	oop := stream nextLine.
	size := stream nextLine asNumber.
	instVarNames := OrderedCollection with: '-.asOop' -> oop.
	size timesRepeat: [
		| name |
		name := '-' , (stream upTo: Character tab).
		oop := stream upTo: Character lf.
		instVarNames add: (name -> (gciSession oopTypeWithOop: oop asNumber)).
	].
	instVarNames addFirst: 'self' -> stream upToEnd.
	instVarListPresenter 
		list: instVarNames;
		selection: instVarNames first;
		yourself.
	self view caption: className.!

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

	super model: anObject.
	codePane model: anObject.
!

onViewOpened

	super onViewOpened.
	(model isKindOf: String) ifTrue: [
		documentPresenter value: model.
		self view caption: 'String (' , model size printString , ' characters)'.
		^self.
	].
	(model isKindOf: ByteArray) ifTrue: [
		documentPresenter value: model printString.
		self view caption: model class name , ' (' , model size printString , ' bytes)'.
		^self.
	].
	(model isKindOf: Integer) ifTrue: [
		documentPresenter value: model printString.
		self view caption: 'Integer'.
		^self.
	].
	model isNil ifTrue: [
		documentPresenter value: model printString.
		self view caption: 'UndefinedObject'.
		^self.
	].
	(model isKindOf: Boolean) ifTrue: [
		documentPresenter value: model printString.
		self view caption: 'Boolean'.
		^self.
	]. 
	(model isKindOf: Character) ifTrue: [
		documentPresenter value: model printString.
		self view caption: 'Character'.
		^self.
	]. 
	(model isKindOf: Float) ifTrue: [
		documentPresenter value: model printString.
		self view caption: 'Float'.
		^self.
	].
	(model isKindOf: Fraction) ifTrue: [
		documentPresenter value: model printString.
		self view caption: 'Fraction'.
		^self.
	].
	self displayObject.
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
!

setLastGsShape: className

	codePane setDocumentPresenterWith: (self registry getClass: className)! !
!JadeInspector categoriesFor: #contextObject!private! !
!JadeInspector categoriesFor: #createComponents!accessing!private! !
!JadeInspector categoriesFor: #createSchematicWiring!accessing!private! !
!JadeInspector categoriesFor: #displayObject!accessing!private! !
!JadeInspector categoriesFor: #inspectInstVar!accessing!private! !
!JadeInspector categoriesFor: #isModified!private! !
!JadeInspector categoriesFor: #model:!private! !
!JadeInspector categoriesFor: #onViewOpened!accessing!private! !
!JadeInspector categoriesFor: #selectedInstVar!accessing!private! !
!JadeInspector categoriesFor: #setLastGsShape:!accessing!private! !

!JadeInspector class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  801 601 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 6 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 624 0 482 8 4278190080 0 519 0 0 0 624 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 241 1 530 19 483 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 120 0 0 0 0 0 0 0 129 0 0 0 241 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 241 1010 8 #fixedViewLeft 19 1010 8 #fixedParentTop 1 1010 8 #fixedParentBottom 1 410 8 ##(Smalltalk.ListBox)  98 17 0 416 98 2 8 1144062209 1025 1152 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 1152 0 8 4294903235 459270 ##(Smalltalk.Message)  8 #key 98 0 1280 32 738 202 208 98 2 802 832 98 2 530 1 1 530 241 483 1152 802 8 #horizontalExtent: 98 1 1 1152 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 120 0 0 0 241 0 0 0] 98 0 960 0 27 978 1024 1 1010 8 #fixedPreviousLeft 1 1088 1 1120 1 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 1696 0 482 8 4278190080 0 7 0 0 0 1696 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.CodeSourcePresenter)  8 #resource_Default_view 0 738 202 208 98 1 802 832 98 2 530 259 1 530 511 483 1696 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 129 0 0 0 0 0 0 0 128 1 0 0 241 0 0 0] 1280 960 0 27 978 1010 8 #fixedPreviousRight 19 1010 8 #fixedParentRight 1 1088 1 1120 1 234 256 98 4 1152 8 'instVarList' 1696 8 'codePane' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #fileNew 8 '&New Workspace' 9373 1 0 0 0 2258 1 2290 8 #fileOpen 8 '&Open Workspace...' 9375 1 0 0 0 2258 1 2290 8 #fileSave 8 '&Save' 9383 1 0 0 0 2258 1 2290 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 2258 1 2290 8 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 2258 1 2290 8 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 37617 0 0 2210 0 16 98 15 2258 1 2290 8 #undo 8 '&Undo' 9397 1 0 0 0 2258 1 2290 8 #redo 8 'R&edo' 9395 1 0 0 0 2610 4097 2258 1 2290 8 #editCut 8 'Cu&t' 9393 1 0 0 0 2258 1 2290 8 #editCopy 8 '&Copy' 9351 1 0 0 0 2258 1 2290 8 #editPaste 8 '&Paste' 9389 1 0 0 0 2258 1 2290 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 2258 1 2290 8 #editDelete 8 '&Delete' 1629 1 0 0 0 2610 4097 2258 1 2290 8 #editFind 8 '&Find...' 9357 1 0 0 0 2258 1 2290 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 2258 1 2290 8 #editReplace 8 '&Replace...' 9361 1 0 0 0 2610 4097 2258 1 2290 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 2258 1 2290 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 37643 0 0 2210 0 16 98 9 2258 1 2290 8 #abortTransaction 8 '&Abort Transaction' 1 1 0 0 0 2258 1 2290 8 #commitTransaction 8 '&Commit Transaction' 1 1 0 0 0 2610 4097 2258 1 2290 8 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 2258 1 2290 8 #jadeDisplay 8 '&Display' 9353 1 0 0 0 2258 1 2290 8 #jadeExecute 8 '&Execute' 9355 1 0 0 0 2258 1 2290 8 #fileIn 8 'Fi&le In' 1 1 0 0 0 2610 4097 2258 1 2290 8 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 0 0 0 8 '&Jade' 0 1 0 0 37659 0 0 2210 0 16 98 1 2258 1 2290 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 37663 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 738 202 208 98 3 802 832 98 2 530 2879 21 530 801 601 416 802 8 #text: 98 1 8 'Jade Object Inspector' 416 802 8 #updateMenuBar 1280 416 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 47 7 0 0 54 1 0 0] 98 3 624 1152 1696 960 0 27 )!

resource_Special_view
	"Answer the literal data from which the 'Special view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Special_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  801 601 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 2 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 416 98 2 8 1143017796 1025 656 0 482 8 4278190080 0 7 0 0 0 656 0 8 4294903017 852486 ##(Smalltalk.NullConverter)  0 0 11 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 769 485 656 882 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 656 882 8 #isTextModified: 98 1 32 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 242 0 0 0] 98 0 530 193 193 0 27 8 'document' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #fileNew 8 '&New Workspace' 9373 1 0 0 0 1298 1 1330 8 #fileOpen 8 '&Open Workspace...' 9375 1 0 0 0 1298 1 1330 8 #fileSave 8 '&Save' 9383 1 0 0 0 1298 1 1330 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 1298 1 1330 8 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 1298 1 1330 8 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 27279 0 0 1250 0 16 98 15 1298 1 1330 8 #undo 8 '&Undo' 9397 1 0 0 0 1298 1 1330 8 #redo 8 'R&edo' 9395 1 0 0 0 1650 4097 1298 1 1330 8 #editCut 8 'Cu&t' 9393 1 0 0 0 1298 1 1330 8 #editCopy 8 '&Copy' 9351 1 0 0 0 1298 1 1330 8 #editPaste 8 '&Paste' 9389 1 0 0 0 1298 1 1330 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 1298 1 1330 8 #editDelete 8 '&Delete' 1629 1 0 0 0 1650 4097 1298 1 1330 8 #editFind 8 '&Find...' 9357 1 0 0 0 1298 1 1330 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 1298 1 1330 8 #editReplace 8 '&Replace...' 9361 1 0 0 0 1650 4097 1298 1 1330 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 1298 1 1330 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 27305 0 0 1250 0 16 98 9 1298 1 1330 8 #abortTransaction 8 '&Abort Transaction' 1 1 0 0 0 1298 1 1330 8 #commitTransaction 8 '&Commit Transaction' 1 1 0 0 0 1650 4097 1298 1 1330 8 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 1298 1 1330 8 #jadeDisplay 8 '&Display' 9353 1 0 0 0 1298 1 1330 8 #jadeExecute 8 '&Execute' 9355 1 0 0 0 1298 1 1330 8 #fileIn 8 'Fi&le In' 1 1 0 0 0 1650 4097 1298 1 1330 8 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 0 0 0 8 '&Jade' 0 1 0 0 27321 0 0 1250 0 16 98 1 1298 1 1330 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 27325 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 818 202 208 98 3 882 912 98 2 530 2879 21 530 801 601 416 882 8 #text: 98 1 8 'Jade Object Inspector' 416 882 8 #updateMenuBar 608 416 1106 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 47 7 0 0 54 1 0 0] 98 1 656 1168 0 27 )!

showOn: anObject session: aGciSession

	^self new 
		gciSession: aGciSession;
		model: anObject;
		createView: ((anObject isKindOf: ExternalInteger "OopType") ifTrue: [self defaultView] ifFalse: ['Special view']);
		showShell;
		yourself.
! !
!JadeInspector class categoriesFor: #resource_Default_view!public!resources-views! !
!JadeInspector class categoriesFor: #resource_Special_view!public!resources-views! !
!JadeInspector class categoriesFor: #showOn:session:!public! !

