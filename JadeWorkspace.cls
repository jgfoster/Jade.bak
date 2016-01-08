"Filed out from Dolphin Smalltalk 7"!

JadeTextDocument subclass: #JadeWorkspace
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeWorkspace guid: (GUID fromString: '{5337E391-2108-4A4D-AE2F-C6C1AD3E4C1C}')!
JadeWorkspace comment: 'model is an instance of GciSession'!
!JadeWorkspace categoriesForClass!Unclassified! !
!JadeWorkspace methodsFor!

gciSession
	^gciSession!

onViewOpened

	super onViewOpened.

	codePane setDocumentPresenterWith: (self registry getClass: 'Object')!

updateCaption

	| string |
	string := self filename isNil
		ifTrue: ['(Untitled)']
		ifFalse: [self filename].
	self caption: (gciSession titleBarFor: string).
! !
!JadeWorkspace categoriesFor: #gciSession!public! !
!JadeWorkspace categoriesFor: #onViewOpened!public! !
!JadeWorkspace categoriesFor: #updateCaption!private!updating! !

!JadeWorkspace class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 2 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 656 0 482 8 4278190080 0 7 0 0 0 656 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.CodeSourcePresenter)  8 #resource_Default_view 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 1169 683 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 85 1 0 0] 608 530 193 193 0 27 8 'codePane' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #fileNew 8 '&New Workspace' 9373 1 0 0 0 1170 1 1202 8 #fileOpen 8 '&Open Workspace...' 9375 1 0 0 0 1170 1 1202 8 #fileSave 8 '&Save' 9383 1 0 0 0 1170 1 1202 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 1170 1 1202 8 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 1170 1 1202 8 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 38145 0 0 1122 0 16 98 15 1170 1 1202 8 #undo 8 '&Undo' 9397 1 0 0 0 1170 1 1202 8 #redo 8 'R&edo' 9395 1 0 0 0 1522 4097 1170 1 1202 8 #editCut 8 'Cu&t' 9393 1 0 0 0 1170 1 1202 8 #editCopy 8 '&Copy' 9351 1 0 0 0 1170 1 1202 8 #editPaste 8 '&Paste' 9389 1 0 0 0 1170 1 1202 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 1170 1 1202 8 #editDelete 8 '&Delete' 1629 1 0 0 0 1522 4097 1170 1 1202 8 #editFind 8 '&Find...' 9357 1 0 0 0 1170 1 1202 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 1170 1 1202 8 #editReplace 8 '&Replace...' 9361 1 0 0 0 1522 4097 1170 1 1202 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 1170 1 1202 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 38171 0 0 1122 0 16 98 9 1170 1 1202 8 #abortTransaction 8 '&Abort Transaction' 1 1 0 0 0 1170 1 1202 8 #commitTransaction 8 '&Commit Transaction' 1 1 0 0 0 1522 4097 1170 1 1202 8 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 1170 1 1202 8 #jadeDisplay 8 '&Display' 9353 1 0 0 0 1170 1 1202 8 #jadeExecute 8 '&Execute' 9355 1 0 0 0 1170 1 1202 8 #fileIn 8 'Fi&le In' 1 1 0 0 0 1522 4097 1170 1 1202 8 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 0 0 0 8 '&Jade' 0 1 0 0 38187 0 0 1122 0 16 98 1 1170 1 1202 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 38191 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 263494 3 ##(Smalltalk.Icon)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'icons\GS32x32.ico' 0 3154 0 16 3200 8 'icons\GS16x16.ico' 0 0 0 1 0 0 834 202 208 98 3 898 928 98 2 530 2879 21 530 1201 801 416 898 8 #text: 98 1 8 'Jade Workspace' 416 898 8 #updateMenuBar 608 416 994 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 98 1 656 1040 0 27 )! !
!JadeWorkspace class categoriesFor: #resource_Default_view!public!resources-views! !

