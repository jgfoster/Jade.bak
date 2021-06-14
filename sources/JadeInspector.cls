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
	^instVarListPresenter selection value
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
	(model isKindOf: ExternalInteger) ifFalse: [^self].
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
	(#(Boolean Character Date DateAndTime Integer Float Fraction UndefinedObject) includes: model class name) ifTrue: [
		self view caption: model class name.
		documentPresenter value: model printString.
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 801 601 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 2 410 ##(Smalltalk.ScintillaView) 34 50 nil 416 34 2 8 1174475012 1025 608 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 480 nil 7 nil nil nil 608 nil 8 1575865961 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 8 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 818 77 786694 ##(Smalltalk.IndexedColor) 33554457 866 33554471 1 nil nil nil nil #callTip nil nil nil #lineNumber 818 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #indentGuide 818 75 nil nil 1 nil nil nil nil #indentGuide nil nil nil 34 256 832 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 912 nil nil nil 928 848 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) nil nil nil nil 608 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 8 #() nil 170 176 1072 nil nil nil nil nil 866 33554447 nil nil nil nil nil nil nil 3 170 192 1072 nil nil nil nil 130003 nil nil nil nil 170 192 34 6 #Notification 818 1027 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 818 1029 1186 60239 1186 31326207 1 nil nil nil nil #Warning nil nil nil #Error 818 1031 1186 335 1186 31317503 1 nil nil nil nil #Error nil nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 9 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 769 483 608 1378 #textLimit: 8 #(1073741823) 608 1378 #modificationEventMask: 8 #(3912191) 608 1378 #caretPeriod: 8 #(530) 608 1378 #isCurrentLineHighlighted: 8 #(true) 608 1378 #wordWrap: 8 #(true) 608 1378 #margins: 34 1 34 5 985158 2 ##(Smalltalk.ScintillaMargin) 1 608 65 3 nil nil nil nil 1666 3 608 1 nil nil 67108863 nil nil 1666 5 608 1 nil nil nil nil nil 1666 7 608 1 nil nil nil nil nil 1666 9 608 1 nil nil nil nil nil 608 1378 #canHScroll: 8 #(false) 608 1378 #maxCompletionListHeight: 8 #(9) 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 241 0 0 0] 8 #() 514 193 193 nil 29 3 false 170 192 34 2 608 8 'document' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 4 265030 4 ##(Smalltalk.Menu) nil true 34 7 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #fileNew 8 '&New Workspace' 9373 1 nil nil nil 2050 1 2082 #fileOpen 8 '&Open Workspace...' 9375 1 nil nil nil 2050 1 2082 #fileSave 8 '&Save' 9383 1 nil nil nil 2050 1 2082 #fileSaveAs 8 'Save &As...' 1 1 nil nil nil 2050 1 2082 #fileRevert 8 '&Revert' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 2050 1 2082 #exit 8 'E&xit Jade' 17639 1 nil nil nil 8 '&File' nil 1 nil nil 36591 nil nil 2002 nil true 34 15 2050 1 2082 #undo 8 '&Undo' 9397 1 nil nil nil 2050 1 2082 #redo 8 'R&edo' 9395 1 nil nil nil 2322 4097 2050 1 2082 #editCut 8 'Cu&t' 9393 1 nil nil nil 2050 1 2082 #editCopy 8 '&Copy' 9351 1 nil nil nil 2050 1 2082 #editPaste 8 '&Paste' 9389 1 nil nil nil 2050 1 2082 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 2050 1 2082 #editDelete 8 '&Delete' 1629 1 nil nil nil 2322 4097 2050 1 2082 #editFind 8 '&Find...' 9357 1 nil nil nil 2050 1 2082 #editFindNext 8 'Find &Next' 9359 1 nil nil nil 2050 1 2082 #editReplace 8 '&Replace...' 9361 1 nil nil nil 2322 4097 2050 1 2082 #addQuotesToSelection 8 'Add &Quotes' 1 1 nil nil nil 2050 1 2082 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 nil nil nil 8 '&Edit' nil 1 nil nil 36617 nil nil 2002 nil true 34 9 2050 1 2082 #abortTransaction 8 '&Abort Transaction' 1 1 nil nil nil 2050 1 2082 #commitTransaction 8 '&Commit Transaction' 1 1 nil nil nil 2322 4097 2050 1 2082 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 2050 1 2082 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 2050 1 2082 #jadeExecute 8 '&Execute' 9355 1 nil nil nil 2050 1 2082 #fileIn 8 'Fi&le In' 1 1 nil nil nil 2322 4097 2050 1 2082 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 nil nil nil 8 '&Jade' nil 1 nil nil 36633 nil nil 2002 nil true 34 1 2050 1 2082 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 1 nil nil 36637 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1314 138 144 34 3 1378 #createAt:extent: 34 2 514 3839 21 514 801 601 416 1378 #text: 34 1 8 'Jade Object Inspector' 416 1378 #updateMenuBar 1072 416 1826 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 15 9 0 0 54 1 0 0] 34 1 608 1888 nil 27 )!

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

