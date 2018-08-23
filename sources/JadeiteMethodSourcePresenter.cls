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
!JadeiteMethodSourcePresenter categoriesFor: #methodListSelection!accessing!public! !
!JadeiteMethodSourcePresenter categoriesFor: #methodService!accessing!private! !
!JadeiteMethodSourcePresenter categoriesFor: #methodService:!accessing!private! !
!JadeiteMethodSourcePresenter categoriesFor: #onViewOpened!event handlers!public! !
!JadeiteMethodSourcePresenter categoriesFor: #openMethodListOn:selecting:!menu handlers!private! !
!JadeiteMethodSourcePresenter categoriesFor: #registerPresentersForUpdates!public!register presenters! !
!JadeiteMethodSourcePresenter categoriesFor: #update!public!updating! !
!JadeiteMethodSourcePresenter categoriesFor: #updateMethodSource!public!updating! !
!JadeiteMethodSourcePresenter categoriesFor: #value:!public! !

