| package |
package := Package name: 'Jade UI Base'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.053'.


package classNames
	add: #JadeBrowserPresenter;
	add: #JadeCodePresenter;
	add: #JadePresenter;
	add: #JadeShell;
	add: #JadeTextDocument;
	add: #JadeTextPresenter;
	add: #JadeValueDialog;
	add: #JadeWorkspace;
	yourself.

package methodNames
	add: #JadeServer -> #fileInClass:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Views\Scintilla\Dolphin Scintilla View';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'GemStone Session';
	yourself).

package!

"Class Definitions"!

Presenter subclass: #JadeCodePresenter
	instanceVariableNames: 'documentPresenter parent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #JadePresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter subclass: #JadeBrowserPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #JadeShell
	instanceVariableNames: 'myPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ValueDialog subclass: #JadeValueDialog
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TextDocument subclass: #JadeTextDocument
	instanceVariableNames: 'gciSession codePane'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeTextDocument subclass: #JadeWorkspace
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TextPresenter subclass: #JadeTextPresenter
	instanceVariableNames: ''
	classVariableNames: 'ColorForCompileError ColorForNoEdits ColorForUnsavedEdits JadeTextStyles'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JadeServer methodsFor!

fileInClass: aString

	| list className index dictionaryName dictionary oldClass oldString |
	list := aString subStrings.
	className := list at: 3.
	className first = $' ifFalse: [self error: 'Class name ' , className printString , ' expected to begin and end with a quote!!'].
	className last = $' ifFalse: [self error: 'Class name ' , className printString , ' expected to begin and end with a quote!!'].
	className := className copyFrom: 2 to: className size - 1.
	index := list indexOf: 'inDictionary:'.
	dictionaryName := list at: index + 1.
	dictionary := self objectNamed: dictionaryName.
	oldClass := dictionary at: className ifAbsent: [nil].
	oldClass notNil ifTrue: [
		oldString := (oldClass _modifiableDefinitionInDictionary: dictionary named: dictionaryName) , '.'.
	].
	oldString = aString ifFalse: [aString evaluate].
! !
!JadeServer categoriesFor: #fileInClass:!Classes!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

JadeCodePresenter guid: (GUID fromString: '{549A5009-CDD1-42B3-8907-C3C5C1C9E532}')!
JadeCodePresenter comment: ''!
!JadeCodePresenter categoriesForClass!Unclassified! !
!JadeCodePresenter methodsFor!

codePresenterIsMethod

	^parentPresenter codePresenterIsMethod!

createSchematicWiring

	super createSchematicWiring.
	documentPresenter
		when: #'marginClicked:'
		send: #'marginClicked:'
		to: self.
!

documentPresenter

	^documentPresenter.
!

marginClicked: anSCNotification

	| scintilla line |
	scintilla := documentPresenter view.
	line := scintilla lineFromPosition: anSCNotification position.
	scintilla selectLine: line.
	"It would be nice to toggle breakpoints here..."!

parent
	^parent!

parent: anObject

	parent := anObject!

setDocumentPresenterWith: aJadeGsClassShape

	documentPresenter lastGsShape: aJadeGsClassShape!

updateMenuBar: aMenuBar
! !
!JadeCodePresenter categoriesFor: #codePresenterIsMethod!public! !
!JadeCodePresenter categoriesFor: #createSchematicWiring!public! !
!JadeCodePresenter categoriesFor: #documentPresenter!public! !
!JadeCodePresenter categoriesFor: #marginClicked:!public! !
!JadeCodePresenter categoriesFor: #parent!accessing!public! !
!JadeCodePresenter categoriesFor: #parent:!accessing!public! !
!JadeCodePresenter categoriesFor: #setDocumentPresenterWith:!public! !
!JadeCodePresenter categoriesFor: #updateMenuBar:!public! !

!JadeCodePresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1140851500 131137 560 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 517 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point)  193 193 0 560 642 672 8 4294902879 234 256 98 0 234 256 98 38 57363 1246982 ##(Smalltalk.ToolbarSystemButton)  57363 0 560 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #fileOpen 8 'Open Workspace' 1 1 0 1 15 57365 898 57365 0 560 1 930 8 #fileSave 8 'Save' 1 1 0 1 17 57367 898 57367 0 560 1 930 8 #editCut 8 'Cut' 1 1 0 1 1 57369 898 57369 0 560 1 930 8 #editCopy 8 'Copy' 1 1 0 1 3 57371 898 57371 0 560 1 930 8 #editPaste 8 'Paste' 1 1 0 1 5 57373 898 57373 0 560 1 930 8 #editDelete 8 'Delete' 1 1 0 1 11 57375 898 57375 0 560 1 930 8 #undo 8 'Undo' 1 1 0 1 7 57377 898 57377 0 560 1 930 8 #redo 8 'Redo' 1 1 0 1 9 57379 898 57379 0 560 1 930 8 #editFind 8 'Find' 1 1 0 1 25 57381 898 57381 0 560 1 930 8 #editReplace 8 'Replace' 1 1 0 1 27 57345 853766 ##(Smalltalk.ToolbarButton)  57345 0 560 1 930 8 #abortTransaction 8 'Abort Transaction' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 770 1857 33 1 57347 1570 57347 0 560 1 930 8 #commitTransaction 8 'Commit Transaction' 1 1 0 1664 27 57349 1570 57349 0 560 1 930 8 #jadeBrowseUsers 8 'Browse Users' 1 1 0 1664 75 57351 1570 57351 0 560 1 930 8 #jadeBrowseClasses 8 'Open System Browser' 1 1 0 1664 17 57353 1570 57353 0 560 1 930 8 #jadeBrowseMonticello 8 'Open Monticello Browser' 1 1 0 1664 3 57355 1570 57355 0 560 1 930 8 #jadeDisplay 8 'Print Result of Selection or Line' 1 1 0 1664 55 57357 1570 57357 0 560 1 930 8 #jadeExecute 8 'Evaluate Selection or Line' 1 1 0 1664 57 57359 1570 57359 0 560 1 930 8 #jadeInspect 8 'Inspect Selection or Line' 1 1 0 1664 59 57361 898 57361 0 560 1 930 8 #fileNew 8 'New Workspace' 1 1 0 1 13 98 24 1584 1792 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 560 3 0 1 1856 1920 1984 2322 0 0 560 3 0 1 2048 2112 2176 2322 0 0 560 3 0 1 2240 912 992 2322 0 0 560 3 0 1 1056 1120 1184 1248 1312 1376 2322 0 0 560 3 0 1 1440 1504 234 240 98 4 1 117 1664 1 0 1 0 770 33 33 770 45 45 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 770 1 1 770 1001 51 560 2546 8 #updateSize 848 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 244 1 0 0 25 0 0 0] 98 0 770 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 2786 8 #fixedParentRight 1 2786 8 #fixedParentTop 1 2786 8 #fixedViewTop 51 410 8 ##(Smalltalk.ScintillaView)  98 46 0 416 98 2 8 1176571972 1025 2928 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 196934 1 ##(Smalltalk.RGB)  27387381 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 21 984134 2 ##(Smalltalk.CommandMenuItem)  1 930 1344 8 '&Undo' 9397 1 0 0 0 3154 1 930 1408 8 'R&edo' 9395 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 3154 1 930 1088 8 'Cu&t' 9393 1 0 0 0 3154 1 930 1152 8 '&Copy' 9351 1 0 0 0 3154 1 930 1216 8 '&Paste' 9389 1 0 0 0 3154 1 930 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 3154 1 930 1280 8 'De&lete' 1629 1 0 0 0 3266 4097 3154 1 930 1472 8 '&Find...' 9357 1 0 0 0 3154 1 930 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 3154 1 930 1536 8 '&Replace...' 9361 1 0 0 0 3266 4097 3154 1 930 2208 8 '&Inspect' 9379 1 0 0 0 3154 1 930 2080 8 '&Display' 9353 1 0 0 0 3154 1 930 2144 8 'Execute' 9355 1 0 0 0 3266 4097 3154 1 930 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 3154 1 930 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 3266 4097 3154 1 930 8 #fileIn 8 'File In' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 690 0 16 722 8 #[244 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 770 193 193 0 2928 0 8 4294903491 852486 ##(Smalltalk.NullConverter)  0 0 9 0 234 256 98 42 8 #specialSelector 1182726 ##(Smalltalk.ScintillaTextStyle)  33 3074 16646145 0 3 0 0 0 0 4272 0 0 0 8 #lineNumber 4290 67 0 0 1 0 0 0 0 4336 0 0 0 8 #global 4290 21 0 0 3 0 0 0 0 4368 0 0 0 8 #normal 4290 1 0 0 1 0 0 0 0 4400 0 0 0 8 #boolean 4290 13 4320 0 3 0 0 0 0 4432 0 0 0 8 #special 4290 25 0 0 3 0 0 0 0 4464 0 0 0 8 #number 4290 5 3074 16711169 0 1 0 0 0 0 4496 0 0 0 8 #nil 4290 19 4320 0 3 0 0 0 0 4544 0 0 0 8 #character 4290 31 3074 16646399 0 3 0 0 0 0 4576 0 0 0 8 #indentGuide 4290 75 786694 ##(Smalltalk.IndexedColor)  33554447 0 1 0 0 0 0 4624 0 0 0 8 #braceHighlight 4290 69 4658 33554465 0 3 0 0 0 0 4688 0 0 0 8 #string 4290 3 3074 16646399 0 129 0 0 0 0 4736 0 0 0 8 #symbol 4290 9 4658 33554443 0 1 0 0 0 0 4784 0 0 0 8 #super 4290 17 4320 0 3 0 0 0 0 4832 0 0 0 8 #comment 4290 7 3074 65025 0 1 0 0 0 0 4864 0 0 0 8 #binary 4290 11 4658 33554433 0 1 0 0 0 0 4912 0 0 0 8 #assignment 4290 29 0 0 3 0 0 0 0 4960 0 0 0 8 #keywordSend 4290 27 4658 33554437 0 3 0 0 0 0 4992 0 0 0 8 #return 4290 23 3074 321 0 3 0 0 0 0 5040 0 0 0 8 #braceMismatch 4290 71 4658 33554459 0 3 0 0 0 0 5088 0 0 0 8 #self 4290 15 4320 0 3 0 0 0 0 5136 0 0 0 98 40 4416 4752 4512 4880 4800 4928 4448 5152 4848 4560 4384 5056 4480 5008 4976 4592 4304 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4352 4704 5104 0 4640 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  4400 234 256 98 16 8 #folderTail 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  57 11 4944 4944 2928 5248 8 #folderOpenMid 5266 53 11 4658 33554471 4944 2928 5296 8 #folderSub 5266 59 11 4944 4944 2928 5344 8 #folderMidTail 5266 55 11 5328 4944 2928 5376 8 #folder 5266 61 5 4944 4944 2928 5408 8 #folderOpen 5266 63 13 4944 4944 2928 5440 8 #folderEnd 5266 51 11 5328 4944 2928 5472 8 #breakpoint 5266 1 1 4944 5120 2928 5504 202 208 848 0 63 9215 0 0 0 0 4672 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 234 256 98 2 4400 4290 1 0 0 1 0 0 0 0 4400 0 0 0 0 0 8 #arrows 0 1 0 234 256 98 6 8 'indicator2' 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  5 2928 511 1 32 0 0 8 'indicator0' 5730 1 2928 65025 3 32 0 0 8 'indicator1' 5730 3 2928 33423361 5 32 0 0 2482 202 208 98 10 2546 2576 98 2 770 1 51 770 1001 551 2928 2546 8 #contextMenu: 98 1 3120 2928 2546 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 2928 2546 8 #isTextModified: 98 1 32 2928 2546 8 #modificationEventMask: 98 1 9215 2928 2546 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 2928 1 3 32 1 6226 3 2928 33 1 16 67108863 6226 5 2928 1 1 16 -67108863 2928 2546 8 #indentationGuides: 98 1 8 #real 2928 2546 8 #tabIndents: 98 1 16 2928 2546 8 #tabWidth: 98 1 9 2928 2546 8 #setLexerLanguage: 98 1 8 #smalltalk 2928 2674 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 244 1 0 0 44 1 0 0] 98 0 2736 0 27 2754 2800 1 2832 1 2864 51 2786 8 #fixedParentBottom 1 234 256 98 4 560 8 'toolbar' 2928 8 'document' 0 2482 202 208 98 1 2546 2576 98 2 770 6239 21 770 1001 601 416 2674 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 47 12 0 0 10 0 0 0 35 14 0 0 54 1 0 0] 98 2 560 2928 2736 0 27 )! !
!JadeCodePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

JadePresenter guid: (GUID fromString: '{CCC9B223-B3B5-44FD-BF88-60F48B65DC99}')!
JadePresenter comment: ''!
!JadePresenter categoriesForClass!Unclassified! !
!JadePresenter methodsFor!

aboutToEditLabel: anObject accept: aValueHolder
	"Is it okay to start editing a label?"

	aValueHolder value: (self canEditLabel: anObject).
!

canEditLabel: anObject

	^true.
!

createDragDropSchematicWiringFor: aPresenter

	aPresenter
		when: #dragOver: 	send: #onDragOver: 	to: self;
		when: #drop:				send: #onDragDrop:	to: self;
		when: #drag:				send: #onDrag:			to: self;
		when: #dragCut:		send: #onDragCut:	to: self;
		yourself.

!

createLabelEditSchematicWiringFor: aPresenter

	aPresenter
		when: #labelOf:changedTo: 			send: #labelOf:changedTo:				to: self;
		when: #aboutToEditLabel:accept: 	send: #aboutToEditLabel:accept:	to: self;
		when: #labelOf:editedTo:accept: 	send: #labelOf:editedTo:accept:		to: self;
		yourself.
!

createSchematicWiring

	super createSchematicWiring.
	self dragDropPresenters do: [:each | 
		self createDragDropSchematicWiringFor: each.
	].
	self labelEditPresenters do: [:each | 
		self createLabelEditSchematicWiringFor: each.
	].
!

dragDropPresenter

	^self primaryPresenter.
!

dragDropPresenters

	^Set with: self dragDropPresenter.
!

drop: sourceObject on: targetObject description: aString

	MessageBox 
		warning: 'Sorry, drop ' , aString , ' ' , sourceObject printString , ' on ' , targetObject printString , ' in ' , self class name , ' not yet supported.'
		caption: 'Jade System Browser'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

dropClass: anOopType on: aTarget

	self
		drop: anOopType 
		on: aTarget 
		description: 'class'.
!

dropClassCategory: anOopType on: aTarget

	self
		drop: anOopType 
		on: aTarget 
		description: 'class category'.
!

dropMethod: anOopType on: aTarget

	self
		drop: anOopType 
		on: aTarget 
		description: 'method'.
!

dropMethodCategory: anOopType on: aTarget

	self
		drop: anOopType 
		on: aTarget 
		description: 'method category'.
!

dropSymbolDictionary: anOopType on: aTarget

	self
		drop: anOopType 
		on: aTarget 
		description: 'symbol dictionary'.
!

gciSession

	^self model.
!

labelEditPresenter

	^self primaryPresenter.
!

labelEditPresenters

	^Set with: self labelEditPresenter.
!

labelOf: a changedTo: b
	"Letting us know that the rename occurred"!

labelOf: oldString editedTo: newString accept: aValueHolder
	"Is it okay to do the rename?"

	MessageBox 
		warning: 'Sorry. Item rename not yet supported.' 
		caption: 'Jade System Browser'.
	aValueHolder value: false.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onDrag: anInternalDragDropSession 

	self subclassResponsibility.
!

onDragClassCategoriesOver: aSession 

	self
		onDragOver: aSession 
		operations: self supportedClassCategoryDropOperations.
!

onDragClassesOver: aSession 

	self
		onDragOver: aSession 
		operations: self supportedClassDropOperations.
!

onDragCut: aSession 
!

onDragDrop: aSession 

	(aSession isFormatAvailable: #'method') 				ifTrue: [^self onDragDropMethods: aSession].
	(aSession isFormatAvailable: #'methodCategory') 	ifTrue: [^self onDragDropMethodCategories: aSession].
	(aSession isFormatAvailable: #'class') 					ifTrue: [^self onDragDropClasses: aSession].
	(aSession isFormatAvailable: #'classCategory')		ifTrue: [^self onDragDropClassCategories: aSession].
	(aSession isFormatAvailable: #'symbolDictionary')	ifTrue: [^self onDragDropSymbolDictionaries: aSession].
	MessageBox notify: 'Sorry, we are not yet able to drag/drop ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onDragDropClassCategories: aSession 

	aSession dragObjects do: [:each | 
		self
			dropClassCategory: (each format: #classCategory)
			on: aSession suggestedTarget.
	].
!

onDragDropClasses: aSession 

	aSession dragObjects do: [:each | 
		self
			dropClass: (each format: #class)
			on: aSession suggestedTarget.
	].
!

onDragDropMethodCategories: aSession

	aSession dragObjects do: [:each | 
		self
			dropMethodCategory: (each format: #methodCategory)
			on: aSession suggestedTarget.
	].
!

onDragDropMethods: aSession

	aSession dragObjects do: [:each | 
		self
			dropMethod: (each format: #method)
			on: aSession suggestedTarget.
	].
!

onDragDropSymbolDictionaries: aSession 

	aSession dragObjects do: [:each | 
		self
			dropSymbolDictionary: (each format: #symbolDictionary)
			on: aSession suggestedTarget.
	].
!

onDragDropSymbolLists: aSession 

	aSession dragObjects do: [:each | 
		self
			dropSymbolList: (each format: #symbolList)
			on: aSession suggestedTarget.
	].
!

onDragMethodCategoriesOver: aSession

	self
		onDragOver: aSession 
		operations: self supportedMethodCategoryDropOperations.
!

onDragMethodsOver: aSession

	self
		onDragOver: aSession 
		operations: self supportedMethodDropOperations.
!

onDragOver: aSession

	aSession operation: nil.
	aSession dragObjects isEmpty ifTrue: [^self].
	aSession suggestedTarget isNil ifTrue: [^self].
	(aSession isFormatAvailable: #'method') 				ifTrue: [^self onDragMethodsOver: aSession].
	(aSession isFormatAvailable: #'methodCategory') 	ifTrue: [^self onDragMethodCategoriesOver: aSession].
	(aSession isFormatAvailable: #'class') 					ifTrue: [^self onDragClassesOver: aSession].
	(aSession isFormatAvailable: #'classCategory')		ifTrue: [^self onDragClassCategoriesOver: aSession].
	(aSession isFormatAvailable: #'symbolDictionary')	ifTrue: [^self onDragSymbolDictionariesOver: aSession].
	MessageBox notify: 'Sorry, we are not yet able to drag over ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onDragOver: aSession operations: aList

	aList isEmpty ifTrue: [^self].
	aSession
		supportedOperations: aList;
		operation: aList first;
		yourself.
!

onDragSymbolDictionariesOver: aSession 

	self
		onDragOver: aSession 
		operations: self supportedSymbolDictionaryDropOperations.
!

primaryPresenter

	^nil.
!

selectionChanging: aSelectionChangingEvent 

	self 
		trigger: #'selectionChanging:'
		with: aSelectionChangingEvent.
!

statusBarText: aString

	self topShell statusBarText: aString.
!

subMenuName

	^nil.
!

subMenuPresenter

	^nil.
!

supportedClassCategoryDropOperations

	^#().
!

supportedClassDropOperations

	^#().
!

supportedMethodCategoryDropOperations

	^#().
!

supportedMethodDropOperations

	^#().
!

supportedSymbolListDropOperations

	^#().
!

updateMenuBar: aMenuBar

	self
		updateMenuBar: aMenuBar 
		withName: self subMenuName 
		itemsFrom: self subMenuPresenter.
!

updateMenuBar: aMenuBar withName: aString itemsFrom: aPresenter

	| contextMenu commandsToRemove items newMenu |
	aString isNil ifTrue: [^self].
	(contextMenu := aPresenter view contextMenu) isNil ifTrue: [^self].
	commandsToRemove := #(#'rename').
	items := contextMenu items reject: [:each | commandsToRemove includes: each command].
	items isEmpty ifTrue: [^self].
	newMenu := aMenuBar addSubmenu: aString.
	1 to: items size do: [:i | 
		| item description |
		item := (items at: i) copy.
		(item isKindOf: CommandMenuItem) ifTrue: [
			description := item commandDescription.
			description := ClosedCommandDescription new
				command: 				description command;
				description: 				description description;
				acceleratorKey: 		description acceleratorKey;
				isModalCommand: 	description isModalCommand;
				image: 						description image;
				receiver:					self;
				queryBlock: 				[:aCommandQuery | self queryCommand: aCommandQuery];
				yourself.
			item commandDescription: description.
		].
		newMenu 
			insertItem: item
			at: i.
	].
!

userSelection

	^nil.
! !
!JadePresenter categoriesFor: #aboutToEditLabel:accept:!label edit!public! !
!JadePresenter categoriesFor: #canEditLabel:!label edit!public! !
!JadePresenter categoriesFor: #createDragDropSchematicWiringFor:!drag & drop!public! !
!JadePresenter categoriesFor: #createLabelEditSchematicWiringFor:!label edit!public! !
!JadePresenter categoriesFor: #createSchematicWiring!drag & drop!label edit!public! !
!JadePresenter categoriesFor: #dragDropPresenter!drag & drop!public! !
!JadePresenter categoriesFor: #dragDropPresenters!drag & drop!public! !
!JadePresenter categoriesFor: #drop:on:description:!drag & drop!public! !
!JadePresenter categoriesFor: #dropClass:on:!drag & drop!public! !
!JadePresenter categoriesFor: #dropClassCategory:on:!drag & drop!public! !
!JadePresenter categoriesFor: #dropMethod:on:!drag & drop!public! !
!JadePresenter categoriesFor: #dropMethodCategory:on:!drag & drop!public! !
!JadePresenter categoriesFor: #dropSymbolDictionary:on:!drag & drop!public! !
!JadePresenter categoriesFor: #gciSession!public! !
!JadePresenter categoriesFor: #labelEditPresenter!label edit!public! !
!JadePresenter categoriesFor: #labelEditPresenters!label edit!public! !
!JadePresenter categoriesFor: #labelOf:changedTo:!label edit!public! !
!JadePresenter categoriesFor: #labelOf:editedTo:accept:!label edit!public! !
!JadePresenter categoriesFor: #onDrag:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragClassCategoriesOver:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragClassesOver:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragCut:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragDrop:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragDropClassCategories:!public! !
!JadePresenter categoriesFor: #onDragDropClasses:!public! !
!JadePresenter categoriesFor: #onDragDropMethodCategories:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragDropMethods:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragDropSymbolDictionaries:!public! !
!JadePresenter categoriesFor: #onDragDropSymbolLists:!public! !
!JadePresenter categoriesFor: #onDragMethodCategoriesOver:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragMethodsOver:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragOver:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragOver:operations:!drag & drop!public! !
!JadePresenter categoriesFor: #onDragSymbolDictionariesOver:!drag & drop!public! !
!JadePresenter categoriesFor: #primaryPresenter!drag & drop!label edit!public! !
!JadePresenter categoriesFor: #selectionChanging:!public! !
!JadePresenter categoriesFor: #statusBarText:!public! !
!JadePresenter categoriesFor: #subMenuName!menus!public! !
!JadePresenter categoriesFor: #subMenuPresenter!menus!public! !
!JadePresenter categoriesFor: #supportedClassCategoryDropOperations!drag & drop!public! !
!JadePresenter categoriesFor: #supportedClassDropOperations!drag & drop!public! !
!JadePresenter categoriesFor: #supportedMethodCategoryDropOperations!drag & drop!public! !
!JadePresenter categoriesFor: #supportedMethodDropOperations!drag & drop!public! !
!JadePresenter categoriesFor: #supportedSymbolListDropOperations!drag & drop!public! !
!JadePresenter categoriesFor: #updateMenuBar:!menus!public! !
!JadePresenter categoriesFor: #updateMenuBar:withName:itemsFrom:!menus!public! !
!JadePresenter categoriesFor: #userSelection!public! !

JadeBrowserPresenter guid: (GUID fromString: '{D80D9891-A83E-456B-8849-D67E57E2A490}')!
JadeBrowserPresenter comment: ''!
!JadeBrowserPresenter categoriesForClass!Unclassified! !
!JadeBrowserPresenter methodsFor!

updateMenuBar: aMenuBar

! !
!JadeBrowserPresenter categoriesFor: #updateMenuBar:!public! !

JadeShell guid: (GUID fromString: '{D1D000EE-0B26-4AF9-8C27-396116CB864D}')!
JadeShell comment: ''!
!JadeShell categoriesForClass!Unclassified! !
!JadeShell methodsFor!

createComponents

	super createComponents.
	self presenterClass ifNotNil: [:class | myPresenter := self add: class new name: 'myPresenter'].
!

logoutRequested: aValueHolder
	"Private - Opportunity to save changes."

	aValueHolder value: true.
!

model: aGciSession

	super model: aGciSession.
	myPresenter notNil ifTrue: [myPresenter model: aGciSession].
	model
		when: #'logoutRequested:'	send: #'logoutRequested:'	to: self;
		when: #'logoutPending'		send: #'exit'						to: self;
		yourself.
!

onViewClosed

	model notNil ifTrue: [
		| temp |
		temp := model.
		model := nil.
		temp removeEventsTriggeredFor: self.
	].
	super onViewClosed.
!

onViewOpened

	[
		| menuBar |
		super onViewOpened.
		menuBar := self view menuBar.
		menuBar isNil ifTrue: [menuBar := MenuBar new].
		myPresenter notNil ifTrue: [myPresenter updateMenuBar: menuBar].
		self view menuBar: menuBar.
	] on: Error do: [:ex | 
		SessionManager current logError: ex.
		MessageBox
			notify: ex description 
			caption: 'Unable to Open ' , self class name.
		Keyboard default isShiftDown ifTrue: [ex halt].
		self view close.
	].

!

presenterClass

	^nil.
!

shellName

	^'Jade Shell'.
!

updateCaption

	self caption: (model titleBarFor: self shellName).
! !
!JadeShell categoriesFor: #createComponents!private! !
!JadeShell categoriesFor: #logoutRequested:!private! !
!JadeShell categoriesFor: #model:!private! !
!JadeShell categoriesFor: #onViewClosed!private! !
!JadeShell categoriesFor: #onViewOpened!private! !
!JadeShell categoriesFor: #presenterClass!overrides!private! !
!JadeShell categoriesFor: #shellName!overrides!private! !
!JadeShell categoriesFor: #updateCaption!overrides!private! !

!JadeShell class methodsFor!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
! !
!JadeShell class categoriesFor: #icon!public! !

JadeValueDialog guid: (GUID fromString: '{C0B22B98-D493-48B6-A13A-1DE92E902CC5}')!
JadeValueDialog comment: ''!
!JadeValueDialog categoriesForClass!Unclassified! !
!JadeValueDialog class methodsFor!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
! !
!JadeValueDialog class categoriesFor: #icon!public! !

JadeTextDocument guid: (GUID fromString: '{B31562B7-43A8-4F81-AE6B-ADD2DE2F1C0B}')!
JadeTextDocument comment: 'This class probably should not have any subclasses except Workspace (and Transcript). The CodeBrowser subclasses should be elsewhere since we don''t want to treat the code as an external text file that can be loaded and saved. '!
!JadeTextDocument categoriesForClass!Unclassified! !
!JadeTextDocument methodsFor!

abortTransaction

	gciSession abort.
	Sound informationBeep.
!

aboutJade

	| stream version |
	version := [SessionManager current version] on: Error do: [:ex | ex return: ex description printString].
	stream := WriteStream on: String new.
	stream
		nextPutAll: 'Jade for GemStone/S ('; 
		nextPutAll: version;
		nextPutAll: ')';
		cr;
		nextPutAll: 'GCI Version: ' , gciSession gciVersion;
		yourself.
	MessageBox 
		notify: stream contents
		caption: 'About Jade'.
!

activeTextEdit 

	^View focus.
!

addQuotesToSelection

	self activeTextEdit replaceSelection: self activeTextEdit selection printString.
!

codePresenterIsMethod

	^false!

commitTransaction

	gciSession commit ifTrue: [
		Sound informationBeep.
	] ifFalse: [
		MessageBox warning: 'Commit failed!!'.
	].
!

contextObject

	^nil.
!

continueTransaction

	MessageBox notify: 'Sorry, we are not yet prepared to handle this feature!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

createComponents

	super createComponents.
	codePane :=  self add: JadeCodePresenter new name: 'codePane'.
	documentPresenter := codePane documentPresenter.
	codePane parent: self.
	self updateCodeFont.
!

currentSelectionOrLine

	self activeTextEdit hasSelection ifFalse: [self activeTextEdit selectCurrentLine].
	^self activeTextEdit selection replaceCrLfWithLf.
!

defineClass: aString inPackageNamed: anUndefinedObject 

	[
		gciSession 
			serverPerform: #'fileInClass:' 
			with: aString.
	] on: GsCompileError do: [:ex | 
		(JadeWorkspace showOn: gciSession)
			caption: 'Jade Workspace - Compile Error';
			showError: ex list on: aString.
	].
!

defineClassMethod: methodString inClassNamed: className inPackageNamed: packageName inCategory: categoryName 

	self
		defineMethod: methodString 
		inClassNamed: className , ' class'
		inPackageNamed: packageName 
		inCategory: categoryName.

!

defineMethod: methodString inClassNamed: className inPackageNamed: packageName inCategory: categoryName 
 
	| string result |
	string := className , 
		' compileMethod: ' , methodString printString , '
		dictionaries: System myUserProfile symbolList
		category: ' , categoryName printString.
	[
		(result := gciSession executeString: string) isNil ifTrue: [^self].
	] on: GsCompileError do: [:ex | 
		(JadeWorkspace showOn: gciSession)
			caption: 'Jade Workspace - Compile Error';
			showError: ex list on: string.
		^self.
	].
	result := GsCompileError
		errorListFor: result
		inSession: gciSession.
	(JadeWorkspace showOn: gciSession)
		caption: 'Jade Workspace - Compile Error on ' , className;
		showError: result on: methodString.
!

doIt: aString

	[
		gciSession executeString: aString.
	] on: GsCompileError do: [:ex | 
		(JadeWorkspace showOn: gciSession)
			caption: 'Jade Workspace - Compile Error';
			showError: ex list on: aString.
	].
!

editCopy

	self activeTextEdit 
		copySelection;
		updateModel;
		yourself.
!

editCut

	self activeTextEdit 
		cutSelection;
		updateModel;
		yourself.
!

editDelete

	self selectLfIfEndingOnCr.
	self activeTextEdit
		clearSelection;
		updateModel;
		yourself.
!

editFind
	"I'm not sure how it works, but this method isn't called!! 
	Somehow, the command is sent directly to the text widget."

self error: 'Do we ever get here!!?'.
	"self activeTextEdit editFind."
!

editFindNext

	self activeTextEdit findNext.
	self showSelection.

!

editPaste

	self activeTextEdit 
		pasteClipboard;
		updateModel;
		yourself.
!

editReplace

	self activeTextEdit 
		findReplace;
		updateModel;
		yourself.
!

editSelectAll

	self activeTextEdit selectAll.
!

executeSelectionOrLine

	[
		^true -> (gciSession 
			executeString: self currentSelectionOrLine 
			fromContext: self contextObject).
	] on: GsCompileError do: [:ex | 
		ex list notEmpty ifTrue: [^false -> ex list].
		^nil -> ex description.
	].
	^false -> #(nil).
!

fileIn: aString

	self setDocumentData: aString.
	self activeTextEdit selectAll.
	self 
		fileIn;
		isModified: false;
		yourself.
!

fileInEnd: aPackage
!

fileInStart: aPackage
!

fileNew

	JadeWorkspace showOn: gciSession.
!

fileOpen
	"Prompts for a file to open into the receiver"

	| openFilename |
	openFilename := self class getFilename.
	openFilename isNil ifTrue: [^self].
	(JadeWorkspace showOn: gciSession)
		openOn: openFilename;
		yourself.
!

getDocumentData

	^documentPresenter value asString.
!

help

	self aboutJade.
!

jadeDisplay

	self jadeExecuteAndDisplay: true.

!

jadeExecute

	self jadeExecuteAndDisplay: false.
!

jadeExecuteAndDisplay: showResult 

	| textView result value selectionRange offset |
	textView := self activeTextEdit.
	(result := self executeSelectionOrLine) key isNil ifTrue: [
		MessageBox notify: result value.
		^self.
	].
	result key ifTrue: [
		value := result value.
		result := ''.
		showResult ifTrue: [
			(gciSession isOopType: value) ifFalse: [
				result := ' ' , value printString.
			] ifTrue: [
				result := ' ' , (gciSession printString: value).
			].
		].
		selectionRange := textView selectionRange.
		result := result "replaceLfWithCrLf".
		textView
			caretPosition: selectionRange stop + 1;
			replaceSelection: result;
			selectionStart: textView caretPosition - result size length: result size.
	] ifFalse: [
		| string count |
		(result := result value first) isNil ifTrue: [^self].
		offset := result at: 2.
		result := result at: 3.
		selectionRange := textView selectionRange.
		(string := textView text) size < selectionRange stop ifTrue: [
			self error: 'Selection range is beyond text size!!?'.
			string := documentPresenter value.
		].
		string := string copyFrom: selectionRange start to: selectionRange stop.
		string := string replaceCrLfWithLf copyFrom: 1 to: offset - 1.
		count := (string select: [:each | each = Character lf]) size.
		offset := offset + count.
		textView
			caretPosition: selectionRange start + offset - 1;
			replaceSelection: result;
			selectionStart: textView caretPosition - result size length: result size.
	].
	^value.
!

logoutRequested: aValueHolder
	"Opportunity to save changes."

	aValueHolder value: true.
!

on: aGciSession

	gciSession := aGciSession.
	gciSession
		when: #'logoutRequested:'	send: #'logoutRequested:'	to: self;
		when: #'logoutPending'		send: #'exit'						to: self;
		yourself.
	super on: self class defaultModel.
!

onCloseRequested: boolValueHolder

	super onCloseRequested: boolValueHolder.
"
	boolValueHolder value ifTrue: [
		| start |
		self view hide.
		start := Time millisecondClockValue.
		SessionManager inputState loopWhile: [(Time millisecondClockValue - start) abs < 100].
	].
"!

onPromptToSaveChanges: aBooleanValue 

	self getDocumentData isEmpty ifTrue: [
		self isModified: false.
	].
	^super onPromptToSaveChanges: aBooleanValue.
!

onViewClosed

	gciSession notNil ifTrue: [
		| temp |
		temp := gciSession.
		gciSession := nil.
		temp removeEventsTriggeredFor: self.
	].
	super onViewClosed.


!

onViewOpened

	super onViewOpened.
	self updateStatusBar.
	documentPresenter view 
		restyleAll;
		backcolor: (RGB red: 250 green: 242 blue: 208);
		yourself.
	self updateCodeFont.
!

print!

queryCommand: query

	| textEdit |
	textEdit := self activeTextEdit.
	(#(#editCut #editCopy #fileIn) includes: query commandSymbol) ifTrue: [
		query isEnabled: (textEdit notNil and: [(textEdit isKindOf: TextEdit) and: [textEdit hasSelection]]).
		^true.
	].
	(query commandSymbol = #editPaste) ifTrue: [
		query isEnabled: (textEdit notNil and: [(textEdit isKindOf: TextEdit) and: [textEdit canPaste]]).
		^true.
	].
	^super queryCommand: query.
!

removeQuotesFromSelection

	| string |
	string := self activeTextEdit selection trimBlanks.
	(string size >= 2 and: [string first = $' and: [string last = $']]) ifFalse: [
		^MessageBox notify: 'Selection must begin and end with quote'.
	].
	string := string copyFrom: 2 to: string size - 1.
	string := string 
		copyReplaceAll: ''''''
		with: ''''.
	self activeTextEdit replaceSelection: string.
!

saveDocument

	| result |
	result := super saveDocument.
	result ifTrue: [self isModified: false].
	^result.
!

selectLfIfEndingOnCr
	"deleting a CR without the subsequent LF can leave things somewhat confused"

	| textEdit text text1 text2 selectionRange |
	textEdit := self activeTextEdit.
	selectionRange := textEdit selectionRange.
	text := textEdit view "hide; show;" value.			"somehow the value gets out of synch"
	selectionRange stop < selectionRange start 				ifFalse: [^self ].
	selectionRange start < textEdit value size 					ifFalse: [^self ].
	(text at: selectionRange start) = Character cr 			ifFalse: [^self ].
	(text at: selectionRange start + 1) = Character lf 		ifFalse: [^self ].
	textEdit selectionRange: (selectionRange start to: selectionRange start + 1).
!

setDocumentData: aString

	documentPresenter value: aString.
!

showError: aList on: aString

	self setDocumentData: aString.
	aList do: [:each | 
		| offset error |
		offset := (each at: 2) asNumber.
		error := (each at: 3).
		self activeTextEdit 
			caretPosition: offset;
			replaceSelection: error;
			selectionStart: self activeTextEdit caretPosition - error size length: error size.
	].
	self isModified: false.
	(MessageBox confirm: 'Continue?' caption: 'Compile Error Found') ifTrue: [^self].
	TerminateProcess signal.
	Processor terminateActive.
!

showSelection

	| myView range lineNumber |
	myView := self activeTextEdit.
	(range := myView selectionRange) isEmpty ifTrue: [^self].
	lineNumber := myView lineFromPosition: range first.
	lineNumber := lineNumber - 4 max: 1.
	myView lineScroll: lineNumber.
!

showText: aString

	self setDocumentData: aString.
	self isModified: false.
!

updateCodeFont

	codePane updateCodeFont.
!

updateStatusBar
!

updateStatusBarItem: aString with: anObject

	| item |
	item := self view 
		viewNamed: aString
		ifNone: [self error: 'statusBar item ' , aString printString , ' not found'].
	item model: (ValueHolder with: anObject).
!

validateUserInterface

	[
		super validateUserInterface.
	] on: Error do: [:ex | 
		view == DeafObject current ifTrue: [ex return: nil].
		ex pass.
	].! !
!JadeTextDocument categoriesFor: #abortTransaction!Jade!private! !
!JadeTextDocument categoriesFor: #aboutJade!private! !
!JadeTextDocument categoriesFor: #activeTextEdit!private! !
!JadeTextDocument categoriesFor: #addQuotesToSelection!edit!private! !
!JadeTextDocument categoriesFor: #codePresenterIsMethod!public! !
!JadeTextDocument categoriesFor: #commitTransaction!Jade!private! !
!JadeTextDocument categoriesFor: #contextObject!Jade!private! !
!JadeTextDocument categoriesFor: #continueTransaction!Jade!private! !
!JadeTextDocument categoriesFor: #createComponents!public! !
!JadeTextDocument categoriesFor: #currentSelectionOrLine!Jade!private! !
!JadeTextDocument categoriesFor: #defineClass:inPackageNamed:!public! !
!JadeTextDocument categoriesFor: #defineClassMethod:inClassNamed:inPackageNamed:inCategory:!public! !
!JadeTextDocument categoriesFor: #defineMethod:inClassNamed:inPackageNamed:inCategory:!public! !
!JadeTextDocument categoriesFor: #doIt:!public! !
!JadeTextDocument categoriesFor: #editCopy!edit!private! !
!JadeTextDocument categoriesFor: #editCut!edit!private! !
!JadeTextDocument categoriesFor: #editDelete!edit!private! !
!JadeTextDocument categoriesFor: #editFind!edit!private! !
!JadeTextDocument categoriesFor: #editFindNext!edit!private! !
!JadeTextDocument categoriesFor: #editPaste!edit!private! !
!JadeTextDocument categoriesFor: #editReplace!edit!private! !
!JadeTextDocument categoriesFor: #editSelectAll!edit!private! !
!JadeTextDocument categoriesFor: #executeSelectionOrLine!Jade!private! !
!JadeTextDocument categoriesFor: #fileIn:!public! !
!JadeTextDocument categoriesFor: #fileInEnd:!public! !
!JadeTextDocument categoriesFor: #fileInStart:!public! !
!JadeTextDocument categoriesFor: #fileNew!private! !
!JadeTextDocument categoriesFor: #fileOpen!private! !
!JadeTextDocument categoriesFor: #getDocumentData!accessing!private! !
!JadeTextDocument categoriesFor: #help!private! !
!JadeTextDocument categoriesFor: #jadeDisplay!Jade!private! !
!JadeTextDocument categoriesFor: #jadeExecute!Jade!private! !
!JadeTextDocument categoriesFor: #jadeExecuteAndDisplay:!Jade!public! !
!JadeTextDocument categoriesFor: #logoutRequested:!private! !
!JadeTextDocument categoriesFor: #on:!private! !
!JadeTextDocument categoriesFor: #onCloseRequested:!private! !
!JadeTextDocument categoriesFor: #onPromptToSaveChanges:!private! !
!JadeTextDocument categoriesFor: #onViewClosed!private! !
!JadeTextDocument categoriesFor: #onViewOpened!private! !
!JadeTextDocument categoriesFor: #print!private! !
!JadeTextDocument categoriesFor: #queryCommand:!commands!private! !
!JadeTextDocument categoriesFor: #removeQuotesFromSelection!edit!private! !
!JadeTextDocument categoriesFor: #saveDocument!public! !
!JadeTextDocument categoriesFor: #selectLfIfEndingOnCr!edit!private! !
!JadeTextDocument categoriesFor: #setDocumentData:!accessing!private! !
!JadeTextDocument categoriesFor: #showError:on:!public! !
!JadeTextDocument categoriesFor: #showSelection!edit!private! !
!JadeTextDocument categoriesFor: #showText:!public! !
!JadeTextDocument categoriesFor: #updateCodeFont!public! !
!JadeTextDocument categoriesFor: #updateStatusBar!private! !
!JadeTextDocument categoriesFor: #updateStatusBarItem:with:!private! !
!JadeTextDocument categoriesFor: #validateUserInterface!overrides!private! !

!JadeTextDocument class methodsFor!

defaultFileExtension

	^'gs'.
!

fileTypes

	^Array
		with: #('GemStone Files (*.gs)' '*.gs')
		with: #('Smalltalk Files (*.st)' '*.st')
		with: FileDialog allFilesType.
!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
!

shutdownOnExit

	^false.
! !
!JadeTextDocument class categoriesFor: #defaultFileExtension!public! !
!JadeTextDocument class categoriesFor: #fileTypes!public! !
!JadeTextDocument class categoriesFor: #icon!public! !
!JadeTextDocument class categoriesFor: #shutdownOnExit!public! !

JadeWorkspace guid: (GUID fromString: '{5337E391-2108-4A4D-AE2F-C6C1AD3E4C1C}')!
JadeWorkspace comment: ''!
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 2 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 656 0 482 8 4278190080 0 7 0 0 0 656 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.JadeCodePresenter)  8 #resource_Default_view 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 1185 693 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 80 2 0 0 90 1 0 0] 608 530 193 193 0 27 8 'codePane' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 265030 4 ##(Smalltalk.Menu)  0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #fileNew 8 '&New Workspace' 9373 1 0 0 0 1170 1 1202 8 #fileOpen 8 '&Open Workspace...' 9375 1 0 0 0 1170 1 1202 8 #fileSave 8 '&Save' 9383 1 0 0 0 1170 1 1202 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 1170 1 1202 8 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 1170 1 1202 8 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 16199 0 0 1122 0 16 98 15 1170 1 1202 8 #undo 8 '&Undo' 9397 1 0 0 0 1170 1 1202 8 #redo 8 'R&edo' 9395 1 0 0 0 1522 4097 1170 1 1202 8 #editCut 8 'Cu&t' 9393 1 0 0 0 1170 1 1202 8 #editCopy 8 '&Copy' 9351 1 0 0 0 1170 1 1202 8 #editPaste 8 '&Paste' 9389 1 0 0 0 1170 1 1202 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 1170 1 1202 8 #editDelete 8 '&Delete' 1629 1 0 0 0 1522 4097 1170 1 1202 8 #editFind 8 '&Find...' 9357 1 0 0 0 1170 1 1202 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 1170 1 1202 8 #editReplace 8 '&Replace...' 9361 1 0 0 0 1522 4097 1170 1 1202 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 1170 1 1202 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 16225 0 0 1122 0 16 98 9 1170 1 1202 8 #abortTransaction 8 '&Abort Transaction' 1 1 0 0 0 1170 1 1202 8 #commitTransaction 8 '&Commit Transaction' 1 1 0 0 0 1522 4097 1170 1 1202 8 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 1170 1 1202 8 #jadeDisplay 8 '&Display' 9353 1 0 0 0 1170 1 1202 8 #jadeExecute 8 '&Execute' 9355 1 0 0 0 1170 1 1202 8 #fileIn 8 'Fi&le In' 1 1 0 0 0 1522 4097 1170 1 1202 8 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 0 0 0 8 '&Jade' 0 1 0 0 16241 0 0 1122 0 16 98 1 1170 1 1202 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 16245 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 263494 3 ##(Smalltalk.Icon)  0 16 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.ImageRelativeFileLocator)  8 #current 8 'icons\GS32x32.ico' 0 3154 0 16 3200 8 'icons\GS16x16.ico' 0 0 0 1 0 0 834 202 208 98 3 898 928 98 2 530 2799 21 530 1201 801 416 898 8 #text: 98 1 8 'Jade Workspace' 416 898 8 #updateMenuBar 608 416 994 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 5 0 0 10 0 0 0 207 7 0 0 154 1 0 0] 98 1 656 1040 0 27 )! !
!JadeWorkspace class categoriesFor: #resource_Default_view!public!resources-views! !

JadeTextPresenter guid: (GUID fromString: '{483A5019-CE38-4F6A-B12D-D39804FC00F4}')!
JadeTextPresenter comment: ''!
!JadeTextPresenter categoriesForClass!Unclassified! !
!JadeTextPresenter class methodsFor!

colorForCompileError

	ColorForCompileError ifNil: [ColorForCompileError := Color red: 255 green: 220 blue: 255].
	^ColorForCompileError.!

colorForCompileError: aColor

	ColorForCompileError := aColor.
!

colorForNoEdits

	ColorForNoEdits ifNil: [ColorForNoEdits := Color red: 253 green: 222 blue: 181].
	^ColorForNoEdits.
!

colorForNoEdits: aColor

	ColorForNoEdits := aColor.
!

colorForUnsavedEdits

	ColorForUnsavedEdits ifNil: [ColorForUnsavedEdits := Color red: 255 green: 255 blue: 180].
	^ColorForUnsavedEdits.!

colorForUnsavedEdits: aColor

	ColorForUnsavedEdits := aColor.!

defaultTextStyles
	"Copied from SmalltalkWorkspace class>>#'defaultTextStyles' with permission from Object-Arts."

	| answer |
	answer := Set new.
	answer
		add: ((ScintillaTextStyle name: #illegal)
					description: 'Illegal characters, e.g. £';
					forecolor: Color red;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #comment)
					description: 'Comments in method source';
					forecolor: Color darkGreen;
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: #identifier)
					description: 'Variables references (instance, temporary and class)';
					yourself);
		add: ((ScintillaTextStyle name: #argDecl)
					description: 'Argument declaration in method signature';
					isItalic: true;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #blockArgDecl)
					description: 'Block argument declaration';
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: #tempDecl)
					description: 'Temporary variable declaration';
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: #unarySelector)
					description: 'Unary selectors (method signature)';
					forecolor: Color blue;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #unaryMessage)
					description: 'Unary (no argument) messages';
					forecolor: Color blue;
					yourself);
		add: ((ScintillaTextStyle name: #binarySelector)
					description: 'Binary in-fix selectors such as + and - (method signature)';
					forecolor: Color blue;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #binaryMessage)
					description: 'Binary in-fix messages such as + and -';
					forecolor: Color blue;
					yourself);
		add: ((ScintillaTextStyle name: #keywordSelector)
					description: 'Components of multi-keyword message selectors';
					forecolor: Color blue;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #keywordMessage)
					description: 'Components of multi-keyword message selectors';
					forecolor: Color blue;
					yourself);
		add: ((ScintillaTextStyle name: #literalPseudo)
					description: 'The literal constants true, false and nil';
					forecolor: Color darkCyan;
					yourself);
		add: ((ScintillaTextStyle name: #literalNumber)
					description: 'Numeric literal constants, e.g. 1.2e6';
					forecolor: Color darkRed;
					yourself);
		add: ((ScintillaTextStyle name: #literalString)
					description: 'Literal string constants, e.g. ''abc''';
					forecolor: Color darkMagenta;
					yourself);
		add: ((ScintillaTextStyle name: #literalSymbol)
					description: 'Literal symbol constants, e.g. #abc';
					forecolor: Color darkBlue;
					yourself);
		add: ((ScintillaTextStyle name: #literalCharacter)
					description: 'Literal character constants, e.g. $A';
					forecolor: Color darkBlue;
					yourself);
		add: ((ScintillaTextStyle name: #literalBytes)
					description: 'Literal byte arrays, e.g. #[0 1 2]';
					forecolor: Color brown;
					yourself);
		add: ((ScintillaTextStyle name: #assignment)
					description: 'Assignment operation, i.e. :=';
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: #tempOpenBar)
					description: 'Temporary declarations opening bar';
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: #tempCloseBar)
					description: 'Temporary declarations closing bar';
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: #specialCharacter)
					description: 'Special characters, e.g. normal and block parentheses';
					yourself);
		add: ((ScintillaTextStyle name: #literalArray)
					description: 'Opening/closing token of literal array, i.e. #()';
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #tag)
					description: 'Primitive or external call tag, e.g. <primitive: 1>';
					forecolor: Color darkGray;
					isItalic: true;
					yourself);
		yourself.
	self assert: [answer size < 32].
	"Scintilla pre-defined styles - note how #normal style inherits font of the view (which in turn should be the default system font)"
	answer
		add: ScintillaTextStyle normal yourself;
		add: ((ScintillaTextStyle name: #indentGuide)
					description: 'Indentation guides, when visible';
					forecolor: Color gray;
					yourself);
		add: ((ScintillaTextStyle name: #braceHighlight)
					description: 'Matching brace, when brace highlighting enabled';
					forecolor: Color blue;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #braceMismatch)
					description: 'Mismatched brace, when brace highlighting enabled';
					forecolor: Color red;
					isBold: true;
					yourself);
		yourself.
	^answer!

resetColors
"
	JadeTextPresenter resetColors.
	ColorDialog showModalOn: JadeTextPresenter colorForNoEdits.
"
	ColorForCompileError := nil.
	ColorForNoEdits := nil.
	ColorForUnsavedEdits := nil.
!

textStyles

	JadeTextStyles ifNil: [JadeTextStyles := self defaultTextStyles].
	^JadeTextStyles.


!

textStyles: aSortedCollection

	JadeTextStyles := aSortedCollection.
! !
!JadeTextPresenter class categoriesFor: #colorForCompileError!public! !
!JadeTextPresenter class categoriesFor: #colorForCompileError:!public! !
!JadeTextPresenter class categoriesFor: #colorForNoEdits!public! !
!JadeTextPresenter class categoriesFor: #colorForNoEdits:!public! !
!JadeTextPresenter class categoriesFor: #colorForUnsavedEdits!public! !
!JadeTextPresenter class categoriesFor: #colorForUnsavedEdits:!public! !
!JadeTextPresenter class categoriesFor: #defaultTextStyles!public! !
!JadeTextPresenter class categoriesFor: #resetColors!public! !
!JadeTextPresenter class categoriesFor: #textStyles!public! !
!JadeTextPresenter class categoriesFor: #textStyles:!public! !

"Binary Globals"!

