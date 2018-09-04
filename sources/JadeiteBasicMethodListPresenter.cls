"Filed out from Dolphin Smalltalk 7"!

PackageMethodListPresenter subclass: #JadeiteBasicMethodListPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteBasicMethodListPresenter guid: (GUID fromString: '{a7382908-5775-44be-81de-c643c1215fb3}')!
JadeiteBasicMethodListPresenter comment: ''!
!JadeiteBasicMethodListPresenter categoriesForClass!Unclassified! !
!JadeiteBasicMethodListPresenter methodsFor!

basicRemoveMethods
	| services |
	services := methodListPresenter selections.
	services isEmpty ifTrue:[^self].
	self removeMethods: (Array with: services)!

browseClass

	| selection browserClass |
	selection := methodListPresenter selection.
	browserClass := Smalltalk at: #RowanSystemBrowser.		"Avoid circular dependencies on load order"
	(browserClass showOnSession: self gciSession)
		selectClass: selection className asString
		selector: selection selector.!

browseImplementorsOf

	self withSelectorDo: 
			[:aSelector |
			| methodService |
			methodService := RowanMethodService new selector: aSelector.
			self browseImplementorsOf: methodService]!

browseImplementorsOf: methodService 
	| service |
	service := RowanQueryService command: #implementorsOf: withArgs: (Array with: methodService selector).
	self issueCommand: (Array with: service).
	self openMethodListOn: service selecting: methodService selector!

browseMethodsContaining

	| string service |
	(string := Prompter prompt: 'Enter string:') isNil ifTrue: [^self].
	service := RowanQueryService command: #methodsContaining:
				withArgs: (Array with: string).
	self issueCommand: (Array with: service).
	self openMethodListOn: service selecting: string!

browseSendersOf
	self withSelectorDo: 
			[:aSelector |
			| methodService |
			methodService := RowanMethodService new selector: aSelector.
			self browseSendersOf: methodService]!

browseSendersOf: methodService
	| service |
	service := RowanQueryService command: #sendersOf: withArgs: (Array with: methodService selector).
	self issueCommand: (Array with: service).
	self openMethodListOn: service selecting: methodService selector!

methodListPresenter

	^methodListPresenter!

onViewOpened
	super onViewOpened.
	self registerPresentersForUpdates!

openMethodListOn: service selecting: string
	(JadeiteMethodListBrowser showOnSession: self gciSession) primaryPresenter
		methods: service queryResults
		selecting: string!

registerPresentersForUpdates
	BrowserUpdate current register: methodListPresenter selector: #methodsRemoved:.
	BrowserUpdate current register: methodListPresenter selector: #methodUpdate:!

removeFromList
	methodListPresenter list: (methodListPresenter list copyWithoutAll: methodListPresenter selections)!

removeMethods
	| result confirmationString |
	confirmationString := self removeSelectorsStringUsing: self methodListPresenter.
	result := MessageBox confirm: confirmationString caption: 'Remove the following method(s)?'.
	result ifFalse: [^self].
	self basicRemoveMethods!

removeMethods: services
	| browserService remainingServices |
	browserService := RowanBrowserService new.
	browserService
		command: #removeMethods:;
		commandArgs: (Array with: methodListPresenter selections).
	self issueCommand: (Array with: browserService).
	!

selection
	^methodListPresenter selection!

selections

	^methodListPresenter selections! !
!JadeiteBasicMethodListPresenter categoriesFor: #basicRemoveMethods!private! !
!JadeiteBasicMethodListPresenter categoriesFor: #browseClass!public! !
!JadeiteBasicMethodListPresenter categoriesFor: #browseImplementorsOf!public! !
!JadeiteBasicMethodListPresenter categoriesFor: #browseImplementorsOf:!public!queries! !
!JadeiteBasicMethodListPresenter categoriesFor: #browseMethodsContaining!public!queries! !
!JadeiteBasicMethodListPresenter categoriesFor: #browseSendersOf!public! !
!JadeiteBasicMethodListPresenter categoriesFor: #browseSendersOf:!public!queries! !
!JadeiteBasicMethodListPresenter categoriesFor: #methodListPresenter!accessing!public! !
!JadeiteBasicMethodListPresenter categoriesFor: #onViewOpened!event handlers!public! !
!JadeiteBasicMethodListPresenter categoriesFor: #openMethodListOn:selecting:!private!queries! !
!JadeiteBasicMethodListPresenter categoriesFor: #registerPresentersForUpdates!public!register presenters! !
!JadeiteBasicMethodListPresenter categoriesFor: #removeFromList!menu handlers!public! !
!JadeiteBasicMethodListPresenter categoriesFor: #removeMethods!menu handlers!public! !
!JadeiteBasicMethodListPresenter categoriesFor: #removeMethods:!menu handlers!private! !
!JadeiteBasicMethodListPresenter categoriesFor: #selection!public! !
!JadeiteBasicMethodListPresenter categoriesFor: #selections!public! !

!JadeiteBasicMethodListPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 2 410 ##(Smalltalk.ListView) 34 30 nil 416 34 2 8 1140920649 1025 544 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 13 265030 4 ##(Smalltalk.Menu) nil true 34 12 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #removeFromList 8 'Remove from &List' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 802 1 834 #browseClass 8 'Browse Class' 1 1 nil nil nil 802 1 834 #browseImplementors 8 'Browse &Implementors' 1 1 nil nil nil 802 1 834 #browseImplementorsOf 8 'Browse Implementors of ...' 1 1 nil nil nil 802 1 834 #browseSenders 8 'Browse &Senders' 1 1 nil nil nil 802 1 834 #browseSendersOf 8 'Browse Senders of ...' 1 1 nil nil nil 802 1 834 #browseMethodsContaining 8 'Browse Methods Containing ...' 1 1 nil nil nil 882 4097 802 1 834 #removeMethods 8 'Remove Method(s) ...' 1 1 nil nil nil 882 4097 802 1 834 #runTests 8 'Run &Test(s)' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 544 nil 8 4294906105 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 5 920646 5 ##(Smalltalk.ListViewColumn) 8 'Project' 201 #left 1362 #displayString 8 #() 1362 #<= 1536 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each projectName ]' 8 #[30 105 226 0 106] #projectName 1584 7 257 nil nil 544 nil 1 nil nil 1474 8 'Package' 251 #left 1362 #displayString 8 #() 1362 #<= 1728 1570 0 nil 1602 2 1 1568 8 'doIt' 8 '[:each | each packageName ]' 8 #[30 105 226 0 106] #packageName 1760 7 257 nil nil 544 nil 1 nil nil 1474 8 'Class' 315 #left 1362 #displayString 8 #() 1362 #<= 1888 1570 0 nil 1602 2 1 1568 8 'doIt' 8 '[:each | each displayClassName ]' 8 #[30 105 226 0 106] #displayClassName 1920 7 257 nil nil 544 nil 3 nil nil 1474 8 'Protocol' 315 #left 1362 #displayString 8 #() 1362 #<= 2048 1570 0 nil 1602 2 1 1568 8 'doIt' 8 '[:each | each category]' 8 #[30 105 226 0 106] #category 2080 7 257 nil nil 544 nil 3 nil nil 1474 8 'Method' 315 #left 1362 #displayString 1392 ##(Smalltalk.SortedCollection) 1570 0 nil 1602 2 1 1568 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 2208 7 257 nil nil 544 nil 3 nil nil #report 656 nil 131169 328198 ##(Smalltalk.Point) 129 129 34 4 nil nil 2290 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 2290 1 1 2290 1401 401 544 2418 #contextMenu: 34 1 768 544 2418 #text: 34 1 8 'Project' 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 188 2 0 0 200 0 0 0] 8 #() 2290 193 193 nil 27 3 false 170 192 34 2 544 8 'methodList' nil 2354 138 144 34 1 2418 #createAt:extent: 34 2 2290 7679 21 2290 1401 401 416 2578 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 187 17 0 0 210 0 0 0] 34 1 544 2640 nil 27 )! !
!JadeiteBasicMethodListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

