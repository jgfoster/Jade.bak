"Filed out from Dolphin Smalltalk 7"!

JadeTextDocument subclass: #JadeInspector
	instanceVariableNames: 'instVarListPresenter'
	classVariableNames: 'DefaultInspectorClass'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeInspector guid: (GUID fromString: '{7cade1f1-0352-4ff0-a34d-888a4de0cd14}')!
JadeInspector comment: ''!
!JadeInspector categoriesForClass!Unclassified! !
!JadeInspector methodsFor!

contextObject

    	instVarListPresenter hasSelection ifFalse: [^model].

	((instVarListPresenter selection key = 'self') or: [instVarListPresenter selection key = '-.asOop']) ifTrue: [^model].
 
	^instVarListPresenter selection value!

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

	| result stream className oop size instVarNames |	(model isKindOf: ExternalInteger) ifFalse: [^self].
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

isJadeInspector

	^true!

isModified

	^false.
!

model: anObject
	"often a GsObject, but could be an Integer, String, etc."

	model := anObject.
	codePane model: anObject.
!

onViewOpened

	super onViewOpened.
	self setDocumentPresenterLabel.
	self displayObject.
!

openNavigationInspector

	instVarListPresenter hasSelection ifFalse: [^JadeNavigationInspector showOn: self model session: gciSession].

	^(instVarListPresenter selection key = 'self')
		ifTrue: [JadeNavigationInspector showOn: self model session: gciSession] 
		ifFalse: [JadeNavigationInspector showOn: instVarListPresenter selection value session: gciSession]!

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

setDocumentPresenterLabel

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
!

setLastGsShape: className

	codePane setDocumentPresenterWith: (self registry getClass: className)! !
!JadeInspector categoriesFor: #contextObject!private! !
!JadeInspector categoriesFor: #createComponents!accessing!private! !
!JadeInspector categoriesFor: #createSchematicWiring!accessing!private! !
!JadeInspector categoriesFor: #displayObject!accessing!private! !
!JadeInspector categoriesFor: #inspectInstVar!accessing!private! !
!JadeInspector categoriesFor: #isJadeInspector!private! !
!JadeInspector categoriesFor: #isModified!private! !
!JadeInspector categoriesFor: #model:!private! !
!JadeInspector categoriesFor: #onViewOpened!accessing!private! !
!JadeInspector categoriesFor: #openNavigationInspector!accessing!private! !
!JadeInspector categoriesFor: #selectedInstVar!accessing!private! !
!JadeInspector categoriesFor: #setDocumentPresenterLabel!accessing!private! !
!JadeInspector categoriesFor: #setLastGsShape:!accessing!private! !

!JadeInspector class methodsFor!

defaultInspectorClass

	DefaultInspectorClass isNil ifTrue: [DefaultInspectorClass := JadeInspector].

	^DefaultInspectorClass!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13565952 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 801 601 551 0 0 0 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 576 0 721158 ##(Smalltalk.SystemColor) 31 0 7 0 0 0 576 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 720 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 769 49 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 24 0 0 0] 34 1 410 ##(Smalltalk.Toolbar) 34 25 0 576 34 2 8 1409288972 131137 960 0 466 8 4278190080 0 519 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 0 960 466 1040 8 4294905247 170 192 720 170 192 34 2 51315 1115910 ##(Smalltalk.ToolbarIconButton) 51315 0 960 1 1180998 4 ##(Smalltalk.CommandDescription) #openNavigationInspector 8 'Open Navigation Inspector' 1 1 263494 3 ##(Smalltalk.Icon) 0 true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'BasicInspector.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 395334 3 ##(Smalltalk.Bitmap) 0 true 0 0 0 0 3 514 33 33 1 34 1 1248 170 176 34 2 1456 1 0 1 0 514 33 33 514 45 45 0 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 754 138 144 34 2 818 #createAt:extent: 34 2 514 1 1 514 769 49 960 818 #updateSizePosted 720 960 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 24 0 0 0] 8 #() 514 193 193 0 27 1776 0 27 0 0 0 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 1792 0 0 0 7 0 0 0 1792 674 170 176 720 false 170 192 34 4 410 ##(Smalltalk.ReferenceView) 34 14 0 1792 34 2 8 1140850688 131073 1920 0 466 8 4278190080 0 7 0 0 0 1920 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.CodeSourcePresenter) #resource_Default_view 0 754 138 144 34 1 818 #createAt:extent: 34 2 514 389 1 514 381 435 1920 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 194 0 0 0 0 0 0 0 128 1 0 0 217 0 0 0] 720 1776 0 27 8 'codePane' 410 ##(Smalltalk.ListBox) 34 17 0 1792 34 2 8 1144062209 1025 2208 590662 2 ##(Smalltalk.ListModel) 138 144 720 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 0 7 0 0 0 2208 0 8 4294905191 459270 ##(Smalltalk.Message) #key 8 #() 720 false 754 138 144 34 2 818 #createAt:extent: 34 2 514 1 1 514 379 435 2208 818 #horizontalExtent: 8 #(0) 2208 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 189 0 0 0 217 0 0 0] 8 #() 1776 0 27 8 'instVarList' 0 754 138 144 34 1 818 #createAt:extent: 34 2 514 1 49 514 769 435 1792 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 24 0 0 0 128 1 0 0 241 0 0 0] 34 3 2208 410 ##(Smalltalk.Splitter) 34 12 0 1792 34 2 8 1140850688 1 2816 0 466 8 4278190080 0 519 0 0 0 2816 754 138 144 34 1 818 #createAt:extent: 34 2 514 379 1 514 11 435 2816 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 189 0 0 0 0 0 0 0 194 0 0 0 217 0 0 0] 8 #() 1776 0 27 1920 1776 0 27 170 192 720 0 461638 4 ##(Smalltalk.MenuBar) 0 true 34 4 265030 4 ##(Smalltalk.Menu) 0 true 34 7 984134 2 ##(Smalltalk.CommandMenuItem) 1 1266 #fileNew 8 '&New Workspace' 9373 1 0 0 0 3186 1 1266 #fileOpen 8 '&Open Workspace...' 9375 1 0 0 0 3186 1 1266 #fileSave 8 '&Save' 9383 1 0 0 0 3186 1 1266 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 3186 1 1266 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 3186 1 1266 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 51267 0 0 3138 0 true 34 15 3186 1 1266 #undo 8 '&Undo' 9397 1 0 0 0 3186 1 1266 #redo 8 'R&edo' 9395 1 0 0 0 3442 4097 3186 1 1266 #editCut 8 'Cu&t' 9393 1 0 0 0 3186 1 1266 #editCopy 8 '&Copy' 9351 1 0 0 0 3186 1 1266 #editPaste 8 '&Paste' 9389 1 0 0 0 3186 1 1266 #editSelectAll 8 'Select &All' 9347 1 0 0 0 3186 1 1266 #editDelete 8 '&Delete' 1629 1 0 0 0 3442 4097 3186 1 1266 #editFind 8 '&Find...' 9357 1 0 0 0 3186 1 1266 #editFindNext 8 'Find &Next' 9359 1 0 0 0 3186 1 1266 #editReplace 8 '&Replace...' 9361 1 0 0 0 3442 4097 3186 1 1266 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 3186 1 1266 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 51293 0 0 3138 0 true 34 9 3186 1 1266 #abortTransaction 8 '&Abort Transaction' 1 1 0 0 0 3186 1 1266 #commitTransaction 8 '&Commit Transaction' 1 1 0 0 0 3442 4097 3186 1 1266 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 3186 1 1266 #jadeDisplay 8 '&Display' 9353 1 0 0 0 3186 1 1266 #jadeExecute 8 '&Execute' 9355 1 0 0 0 3186 1 1266 #fileIn 8 'Fi&le In' 1 1 0 0 0 3442 4097 3186 1 1266 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 0 0 0 8 '&Jade' 0 1 0 0 51309 0 0 3138 0 true 34 1 3186 1 1266 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 51313 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 514 2719 21 514 801 601 416 818 #text: 34 1 8 'Jade Object Inspector' 416 818 #updateMenuBar 720 416 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 79 5 0 0 10 0 0 0 223 6 0 0 54 1 0 0] 34 2 576 1792 1776 0 27 )
!

resource_Special_view
	"Answer the literal data from which the 'Special view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Special_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  801 601 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 2 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 416 98 2 8 1143017796 1025 656 0 482 8 4278190080 0 7 0 0 0 656 0 8 4294903017 852486 ##(Smalltalk.NullConverter)  0 0 11 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 769 485 656 882 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 656 882 8 #isTextModified: 98 1 32 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 242 0 0 0] 98 0 530 193 193 0 27 8 'document' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #fileNew 8 '&New Workspace' 9373 1 0 0 0 1298 1 1330 8 #fileOpen 8 '&Open Workspace...' 9375 1 0 0 0 1298 1 1330 8 #fileSave 8 '&Save' 9383 1 0 0 0 1298 1 1330 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 1298 1 1330 8 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 1298 1 1330 8 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 27279 0 0 1250 0 16 98 15 1298 1 1330 8 #undo 8 '&Undo' 9397 1 0 0 0 1298 1 1330 8 #redo 8 'R&edo' 9395 1 0 0 0 1650 4097 1298 1 1330 8 #editCut 8 'Cu&t' 9393 1 0 0 0 1298 1 1330 8 #editCopy 8 '&Copy' 9351 1 0 0 0 1298 1 1330 8 #editPaste 8 '&Paste' 9389 1 0 0 0 1298 1 1330 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 1298 1 1330 8 #editDelete 8 '&Delete' 1629 1 0 0 0 1650 4097 1298 1 1330 8 #editFind 8 '&Find...' 9357 1 0 0 0 1298 1 1330 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 1298 1 1330 8 #editReplace 8 '&Replace...' 9361 1 0 0 0 1650 4097 1298 1 1330 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 1298 1 1330 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 27305 0 0 1250 0 16 98 9 1298 1 1330 8 #abortTransaction 8 '&Abort Transaction' 1 1 0 0 0 1298 1 1330 8 #commitTransaction 8 '&Commit Transaction' 1 1 0 0 0 1650 4097 1298 1 1330 8 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 1298 1 1330 8 #jadeDisplay 8 '&Display' 9353 1 0 0 0 1298 1 1330 8 #jadeExecute 8 '&Execute' 9355 1 0 0 0 1298 1 1330 8 #fileIn 8 'Fi&le In' 1 1 0 0 0 1650 4097 1298 1 1330 8 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 0 0 0 8 '&Jade' 0 1 0 0 27321 0 0 1250 0 16 98 1 1298 1 1330 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 27325 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 818 202 208 98 3 882 912 98 2 530 2879 21 530 801 601 416 882 8 #text: 98 1 8 'Jade Object Inspector' 416 882 8 #updateMenuBar 608 416 1106 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 47 7 0 0 54 1 0 0] 98 1 656 1168 0 27 )!

setDefaultInspectorClass: aJadeInspectorClass

	DefaultInspectorClass := aJadeInspectorClass
!

setNavigationInspector

	self setDefaultInspectorClass: JadeNavigationInspector!

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
!JadeInspector class categoriesFor: #resource_Default_view!public! !
!JadeInspector class categoriesFor: #resource_Special_view!public!resources-views! !
!JadeInspector class categoriesFor: #setDefaultInspectorClass:!public! !
!JadeInspector class categoriesFor: #setNavigationInspector!public! !
!JadeInspector class categoriesFor: #setNormalInspector!public! !
!JadeInspector class categoriesFor: #showOn:session:!public! !

