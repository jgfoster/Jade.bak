"Filed out from Dolphin Smalltalk 7"!

JadeTextDocument subclass: #JadeWorkspace
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeWorkspace guid: (GUID fromString: '{5337e391-2108-4a4d-ae2f-c6c1ad3e4c1c}')!
JadeWorkspace comment: 'model is an instance of GciSession'!
!JadeWorkspace categoriesForClass!Unclassified! !
!JadeWorkspace methodsFor!

onViewOpened

	super onViewOpened.
	codePane setDocumentPresenterWith: (self registry getClass: 'Object').

	"documentPresenter view styler: JadeExpressionStyler new."!

queryCommand: aCommandQuery

	| cmd |
	cmd := aCommandQuery commandSymbol.
	(cmd == #'setEnvironment0' and: [environment == 0]) ifTrue: [aCommandQuery isChecked: true; isEnabled: false. ^true].
	(cmd == #'setEnvironment1' and: [environment == 1]) ifTrue: [aCommandQuery isChecked: true; isEnabled: false. ^true].
	(cmd == #'setEnvironment2' and: [environment == 2]) ifTrue: [aCommandQuery isChecked: true; isEnabled: false. ^true].
	^super queryCommand: aCommandQuery!

setEnvironment: anInteger

	environment := anInteger.
	documentPresenter parentPresenter environment: anInteger.
	self updateCaption.
!

setEnvironment0

	self setEnvironment: 0.!

setEnvironment1

	self setEnvironment: 1.!

setEnvironment2

	self setEnvironment: 2.!

updateCaption

	| string |
	string := self filename isNil
		ifTrue: ['(Untitled)']
		ifFalse: [self filename].
	string := gciSession titleBarFor: string.
	environment ~~ 0 ifTrue: [string := string , ' @env' , environment printString].
	self caption: string.
! !
!JadeWorkspace categoriesFor: #onViewOpened!private! !
!JadeWorkspace categoriesFor: #queryCommand:!public! !
!JadeWorkspace categoriesFor: #setEnvironment:!public! !
!JadeWorkspace categoriesFor: #setEnvironment0!public! !
!JadeWorkspace categoriesFor: #setEnvironment1!public! !
!JadeWorkspace categoriesFor: #setEnvironment2!public! !
!JadeWorkspace categoriesFor: #updateCaption!private!updating! !

!JadeWorkspace class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 551 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 640 nil 466 8 4278190080 nil 7 nil nil nil 640 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.CodeSourcePresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 1169 683 640 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 85 1 0 0] 592 514 193 193 nil 27 8 'codePane' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 5 265030 4 ##(Smalltalk.Menu) nil true 34 7 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #fileNew 8 '&New Workspace' 9373 1 nil nil nil 1090 1 1122 #fileOpen 8 '&Open Workspace...' 9375 1 nil nil nil 1090 1 1122 #fileSave 8 '&Save' 9383 1 nil nil nil 1090 1 1122 #fileSaveAs 8 'Save &As...' 1 1 nil nil nil 1090 1 1122 #fileRevert 8 '&Revert' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 1090 1 1122 #exit 8 'E&xit Jade' 17639 1 nil nil nil 8 '&File' nil 1 nil nil 25847 nil nil 1042 nil true 34 15 1090 1 1122 #undo 8 '&Undo' 9397 1 nil nil nil 1090 1 1122 #redo 8 'R&edo' 9395 1 nil nil nil 1362 4097 1090 1 1122 #editCut 8 'Cu&t' 9393 1 nil nil nil 1090 1 1122 #editCopy 8 '&Copy' 9351 1 nil nil nil 1090 1 1122 #editPaste 8 '&Paste' 9389 1 nil nil nil 1090 1 1122 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 1090 1 1122 #editDelete 8 '&Delete' 1629 1 nil nil nil 1362 4097 1090 1 1122 #editFind 8 '&Find...' 9357 1 nil nil nil 1090 1 1122 #editFindNext 8 'Find &Next' 9359 1 nil nil nil 1090 1 1122 #editReplace 8 '&Replace...' 9361 1 nil nil nil 1362 4097 1090 1 1122 #addQuotesToSelection 8 'Add &Quotes' 1 1 nil nil nil 1090 1 1122 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 nil nil nil 8 '&Edit' nil 1 nil nil 25963 nil nil 1042 nil true 34 10 1090 1 1122 #abortTransaction 8 '&Abort Transaction' 1 1 nil nil nil 1090 1 1122 #commitTransaction 8 '&Commit Transaction' 1 1 nil nil nil 1362 4097 1090 1 1122 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 1090 1 1122 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 1090 1 1122 #jadeExecute 8 '&Execute' 9355 1 nil nil nil 1090 1 1122 #jadeDebug 8 'Debu&g' 9359 1 nil nil nil 1090 1 1122 #fileIn 8 'Fi&le In' 1 1 nil nil nil 1362 4097 1090 1 1122 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 nil nil nil 8 '&Jade' nil 1 nil nil 26009 nil nil 1042 nil true 34 3 1090 1 1122 #setEnvironment0 8 '&0' 1 1 nil nil nil 1090 1 1122 #setEnvironment1 8 '&1' 1 1 nil nil nil 1090 1 1122 #setEnvironment2 8 '&2' 1 1 nil nil nil 8 'En&vironment' nil 134217729 nil nil 26039 nil nil 1042 nil true 34 1 1090 1 1122 #aboutJadeite 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 1 nil nil 26043 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'icons\GS32.ico' nil 2898 nil true 2944 8 'icons\GS16.ico' nil nil nil 1 nil nil 770 138 144 34 3 834 #createAt:extent: 34 2 514 3839 21 514 1201 801 416 834 #text: 34 1 8 'Jade Workspace' 416 834 #updateMenuBar 592 416 914 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 215 9 0 0 154 1 0 0] 34 1 640 960 nil 27)! !
!JadeWorkspace class categoriesFor: #resource_Default_view!public!resources-views! !

