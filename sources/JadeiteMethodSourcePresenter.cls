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

browseImplementorsOf: string
	| service |
	service := RowanQueryService command: #implementorsOf: withArgs: (Array with: string asSymbol).
	self issueCommand: (Array with: service).
	self openMethodListOn: service selecting: string!

browseSendersOf: string
	| service |
	service := RowanQueryService command: #sendersOf: withArgs: (Array with: string asSymbol).
	self issueCommand: (Array with: service).
	self openMethodListOn: service selecting: string!

editMenuStrings

	false ifTrue: [
		self editSaveMethod; undo; redo; cutSelection; copySelection; pasteClipboard; clearSelection; selectAll; editFind; editFindNext; editReplace; 
			jadeDisplay; jadeExecute; jadeInspect; browseSelectedClass.
	].
	^#(
		'&Edit'
		'&Save/Ctrl+S/editSaveMethod' 
		'-'
		'&Undo/Ctrl+Z/undo'
		'&Redo/Ctrl+Y/redo'
		'-'
		'&Reformat Source/Ctrl+R/reformatSource'
		'-'
		'&Cu&t/Ctrl+X/cutSelection'
		'&Copy/Ctrl+C/copySelection'
		'&Paste/Ctrl+V/pasteClipboard'
		'&Delete/Delete/clearSelection'
		'-'
		'Select &All/Ctrl+A/selectAll'
		'-'
		'&Find/Ctrl+F/editFind'
		'Find &Next/F3/editFindNext'
		'&Replace/Ctrl+H/editReplace'
		'-'
		'Display/Ctrl+D/jadeDisplay'
		'Execute/Ctrl+E/jadeExecute'
		'Inspect/Ctrl+Q/jadeInspect'
		'Browse Class/Ctrl+B/browseSelectedClass'
	).
!

editSaveMethod
	| classService |
	classService := RowanClassService named: methodService className.
	classService meta: methodService meta.
	self
		editSaveMethod: classService
		in: documentPresenter
		category: methodService category.!

fileSave
	self editSaveMethod!

methodListSelection

	^methodService!

methodService
	^methodService!

methodService: anObject
	methodService := anObject!

onViewOpened
	super onViewOpened.
	self registerPresentersForUpdates. !

openMethodListOn: service selecting: string
	(JadeiteMethodListBrowser showOnSession: self gciSession) primaryPresenter
		methods: service queryResults
		selecting: string!

registerPresentersForUpdates
	BrowserUpdate current
		register: documentPresenter
		selector: #methodSourceUpdate:browser:
		browser: self!

update
	^self updateMethodSource!

updateMethodSource
	methodService isNil ifTrue: [^self setDefaultMethod].
	documentPresenter
		value: methodService source;
		isModified: false.
	documentPresenter view
		backcolor: JadeTextStyles default colorForNoEdits;
		isEnabled: true;
		yourself.
	self refreshBreakPointsIn: documentPresenter!

value: string

	documentPresenter value: string! !
!JadeiteMethodSourcePresenter categoriesFor: #browseImplementorsOf:!menu handlers!public! !
!JadeiteMethodSourcePresenter categoriesFor: #browseSendersOf:!menu handlers!public! !
!JadeiteMethodSourcePresenter categoriesFor: #editMenuStrings!menu handlers!private! !
!JadeiteMethodSourcePresenter categoriesFor: #editSaveMethod!menu handlers!public! !
!JadeiteMethodSourcePresenter categoriesFor: #fileSave!menu handlers!public! !
!JadeiteMethodSourcePresenter categoriesFor: #methodListSelection!accessing!public! !
!JadeiteMethodSourcePresenter categoriesFor: #methodService!accessing!private! !
!JadeiteMethodSourcePresenter categoriesFor: #methodService:!accessing!private! !
!JadeiteMethodSourcePresenter categoriesFor: #onViewOpened!event handlers!public! !
!JadeiteMethodSourcePresenter categoriesFor: #openMethodListOn:selecting:!menu handlers!private! !
!JadeiteMethodSourcePresenter categoriesFor: #registerPresentersForUpdates!public!register presenters! !
!JadeiteMethodSourcePresenter categoriesFor: #update!public!updating! !
!JadeiteMethodSourcePresenter categoriesFor: #updateMethodSource!public!updating! !
!JadeiteMethodSourcePresenter categoriesFor: #value:!public! !

!JadeiteMethodSourcePresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ScintillaView) 34 50 nil 416 34 2 8 1445007428 1025 544 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 22 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #undo 8 '&Undo' 9397 1 nil nil nil 770 1 802 #redo 8 'R&edo' 9395 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 770 1 802 #editCut 8 'Cu&t' 9393 1 nil nil nil 770 1 802 #editCopy 8 '&Copy' 9351 1 nil nil nil 770 1 802 #editPaste 8 '&Paste' 9389 1 nil nil nil 770 1 802 #editDelete 8 'De&lete' 1629 1 nil nil nil 770 1 802 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 898 4097 770 1 802 #editFind 8 '&Find...' 9357 1 nil nil nil 770 1 802 #editFindNext 8 'Find &Next' 9359 1 nil nil nil 770 1 802 #editReplace 8 '&Replace...' 9361 1 nil nil nil 898 4097 770 1 802 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 770 1 802 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 770 1 802 #jadeExecute 8 'Execute' 9355 1 nil nil nil 770 1 802 #jadeDebug 8 'De&bug' 9349 1 nil nil nil 898 4097 770 1 802 #addQuotesToSelection 8 'Add &Quotes' 1 1 nil nil nil 770 1 802 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 nil nil nil 898 4097 770 1 802 #fileIn 8 'File In' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 nil 544 nil 8 4294905347 852486 ##(Smalltalk.NullConverter) nil nil 9 nil 170 192 34 4 #callTip 1182790 1 ##(Smalltalk.ScintillaTextStyle) 77 786694 ##(Smalltalk.IndexedColor) 33554457 1954 33554471 1 nil nil nil nil #callTip nil nil nil #normal 1922 1 nil nil 1 nil nil nil nil #normal nil nil nil 34 256 2000 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1936 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 8 #() 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 544 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 2080 nil 170 176 2080 9215 nil nil nil nil 1954 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 4 #callTip 1936 #normal 2000 nil nil nil nil 1 nil 170 192 34 12 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 544 65025 3 false 1 nil nil nil nil nil nil 3 2402 3 544 33423361 5 false 3 nil nil nil nil nil nil 5 2402 5 544 511 1 false 5 nil nil nil nil nil nil 8 'indicator10' 2402 21 544 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator9' 2402 19 544 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator8' 2402 17 544 33554447 1 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 1922 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 1922 1027 2610 60239 2610 31326207 1 nil nil nil nil #Warning nil nil nil #Error 1922 1031 2610 335 2610 31317503 1 nil nil nil nil #Error nil nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 9 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 1810 1 51 1810 1001 551 544 2802 #contextMenu: 34 1 736 544 2802 #modificationEventMask: 8 #(4607) 544 2802 #hoverTime: 8 #(200) 544 2802 #caretPeriod: 8 #(530) 544 2802 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 544 61 3 nil nil nil nil 3058 3 544 1 nil 3 67108863 nil nil 3058 5 544 1 nil 3 -67108863 nil nil 544 2802 #canHScroll: 8 #(false) 544 2802 #tabWidth: 8 #(4) 544 2802 #maxCompletionListHeight: 8 #(9) 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 244 1 0 0 44 1 0 0] 8 #() 1810 193 193 nil 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 3330 #fixedParentRight 1 3330 #fixedPreviousBottom 1 3330 #fixedParentBottom 1 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1140851500 131137 3408 nil 674 8 4278190080 nil 517 nil 1730 nil true 1762 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1810 193 193 nil 3408 674 3488 8 4294905713 170 192 2080 170 192 34 24 20793 1246982 ##(Smalltalk.ToolbarSystemButton) 20793 nil 3408 1 802 #editReplace 8 'Replace' 1 1 nil 1 27 20771 853766 ##(Smalltalk.ToolbarButton) 20771 nil 3408 1 802 #jadeDisplay 8 'Display' 1 1 nil 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 1810 1857 33 55 20773 3714 20773 nil 3408 1 802 #jadeExecute 8 'Execute' 1 1 nil 3792 57 20775 3714 20775 nil 3408 1 802 #jadeInspect 8 'Inspect' 1 1 nil 3792 59 20777 3650 20777 nil 3408 1 802 #editSaveMethod 8 'Save' 9383 1 nil 1 17 20779 3650 20779 nil 3408 1 802 #editCut 8 'Cut' 1 1 nil 1 1 20781 3650 20781 nil 3408 1 802 #copySelection 8 'Copy' 1 1 nil 1 3 20783 3650 20783 nil 3408 1 802 #editPaste 8 'Paste' 1 1 nil 1 5 20785 3650 20785 nil 3408 1 802 #editDelete 8 'Delete' 1 1 nil 1 11 20787 3650 20787 nil 3408 1 802 #undo 8 'Undo' 1 1 nil 1 7 20789 3650 20789 nil 3408 1 802 #redo 8 'Redo' 1 1 nil 1 9 20791 3650 20791 nil 3408 1 802 #editFind 8 'Find' 1 1 nil 1 25 34 15 3728 3920 3968 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 3408 3 nil 1 4016 4418 nil nil 3408 3 nil 1 4064 4112 4160 4208 4256 4304 4418 nil nil 3408 3 nil 1 4352 3664 170 176 34 4 1 117 3792 1 nil 1 nil 1810 33 33 1810 45 45 nil nil 2738 138 144 34 2 2802 #createAt:extent: 34 2 1810 1 1 1810 1001 51 3408 2802 #updateSizePosted 2080 3408 3218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 244 1 0 0 25 0 0 0] 8 #() 3280 nil 27 3298 3344 1 3360 1 3330 #fixedParentTop 1 3330 #fixedViewTop 51 170 192 34 2 544 8 'document' nil 2738 138 144 34 1 2802 #createAt:extent: 34 2 1810 7679 21 1810 1001 601 416 3218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 243 16 0 0 54 1 0 0] 34 2 3408 544 3280 nil 27 )! !
!JadeiteMethodSourcePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

