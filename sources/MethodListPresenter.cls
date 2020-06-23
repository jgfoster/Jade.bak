"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #MethodListPresenter
	instanceVariableNames: 'methodListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MethodListPresenter guid: (GUID fromString: '{d704c96d-6d80-444e-8a76-ac5b0124bc86}')!
MethodListPresenter comment: ''!
!MethodListPresenter categoriesForClass!Unclassified! !
!MethodListPresenter methodsFor!

anyMethod

	^methodListPresenter list anyOne.
!

browseClass

	| selection browserClass |
	selection := methodListPresenter selection.
	browserClass := Smalltalk at: #JadeSystemBrowser.		"Avoid circular dependencies on load order"
	JadeLog log: 'MethodListPresenter>>browseClass - ' , selection gsClass name , '>>' , selection name.
	(browserClass showOnSession: self gciSession)
		selectClass: selection gsClass name
		selector: selection name.
!

browseImplementors

	self browseImplementorsOf: methodListPresenter selection.!

browseImplementorsOf

	self withSelectorDo: [:aSelector | self browseImplementorsOf: aSelector].
!

browseMethodsContaining

	| string |
	(string := Prompter prompt: 'Enter substring:') isNil ifTrue: [^self].
	(JadeMethodListBrowser showOnSession: self gciSession) primaryPresenter browseMethodsContaining: string.
!

browseSenders

	self browseSendersOf: methodListPresenter selection.!

browseSendersOf

	self withSelectorDo: [:aSelector | self browseSendersOf: aSelector].
!

createComponents

	super createComponents.
	methodListPresenter :=  self add: ListPresenter new name: 'methodList'.
!

createSchematicWiring

	super createSchematicWiring.
	methodListPresenter	
		when: #actionPerformed	 		send: #browseClass				to: self;
		when: #selectionChanged			send: #methodSelectionChanged	to: self;
		when: #selectionChanging: 		send: #selectionChanging:			to: self;
		when: #labelOf:editedTo:accept: 	send: #labelOf:editedTo:accept: 	to: self;
		when: #'leftButtonPressed:'		send: #'onLeftButtonPressed:'	to: self;
		yourself.
	self createDragDropSchematicWiringFor: methodListPresenter.
!

deleteMethods

	| methods stream result |
	stream := WriteStream on: String new.
	methods := methodListPresenter selections.
	methods do: [:each | stream print: each; cr].
	result := MessageBox 
		confirm: stream contents 
		caption: 'Remove the following method(s)?'.
	result ifFalse: [^self].
	methods do: [:each | self gciSession serverPerform: #'deleteMethod:' with: each].
	self removeFromList.
	JadeLog log: 'MethodListPresenter>>removeMethods'.
!

list: aList

	methodListPresenter list: aList.
!

methodSelectionChanged

	self trigger: #'methodSelectionChanged'.
!

onDrag: aSession 

	| list |
	list := methodListPresenter selections collect: [:each | 
		(aSession newDragObject: each)
			format: #method data: each;
			yourself.
	].
	aSession 
		dragObjects: list;
		supportedOperations: #(#move #copy);
		defaultOperation: #copy;
		yourself.
!

onLeftButtonPressed: aMouseEvent

	self trigger: #'leftButtonPressed:' with: aMouseEvent.
!

primaryPresenter

	^methodListPresenter.
!

queryCommand: aCommandQuery

	(#(#'browseClass' #'browseImplementors' #'browseSenders' #'showHistory') includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery 
			isEnabled: methodListPresenter selections size = 1;
			receiver: self;
			yourself.
		^true.
	].
	(#(#'runTests') includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: (methodListPresenter selections notEmpty and: [
			methodListPresenter selections allSatisfy: [:each | each isTestCase] ]).
		^true.
	].
	(#(#'deleteMethods' #'removeFromList') includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: methodListPresenter selections notEmpty.
		^true.
	].
	^super queryCommand: aCommandQuery.
!

removeFromList

	methodListPresenter selections copy do: [:each | 
		methodListPresenter model removeAtIndex: (methodListPresenter model list indexOf: each).
	].
!

runTests

	| list |
	list := methodListPresenter selections.
	list do: [:each | 
		self gciSession 
			serverPerform: #'runAsTest:'
			with: each.
	].
	MessageBox notify: list size printString , ' test(s) passed!!'.
!

selectedClasses

	^nil!

selectedMethod

	^methodListPresenter selectionOrNil.
!

selectFirstMethod

	methodListPresenter list notEmpty ifTrue: [
		methodListPresenter selectionByIndex: 1.
	].
!

selectionOrNil

	^methodListPresenter selectionOrNil.
!

selectMethod: aGsMethod

	methodListPresenter selection: aGsMethod ifAbsent: []!

showHistory

	self halt.!

subMenuName

	^'&Methods'.
!

subMenuPresenter

	^methodListPresenter.
!

withSelectorDo: aBlock

	| selector result list |
	(selector := Prompter prompt: 'Enter selector:') isNil ifTrue: [^self].
	selector := selector reject: [:each | each = Character space].
	(selector includes: $*) ifFalse: [
		aBlock value: selector.
		^self.
	].
	result := gciSession
		serverPerform: #selectorsMatching:
		with: (gciSession encode: selector).
	result isNil ifTrue: [^self].
	list := result subStrings: Character lf.
	(selector := ChoicePrompter choices: list) isNil ifTrue: [^self].
	aBlock value: selector.
! !
!MethodListPresenter categoriesFor: #anyMethod!public! !
!MethodListPresenter categoriesFor: #browseClass!public! !
!MethodListPresenter categoriesFor: #browseImplementors!public! !
!MethodListPresenter categoriesFor: #browseImplementorsOf!public! !
!MethodListPresenter categoriesFor: #browseMethodsContaining!public! !
!MethodListPresenter categoriesFor: #browseSenders!public! !
!MethodListPresenter categoriesFor: #browseSendersOf!public! !
!MethodListPresenter categoriesFor: #createComponents!public! !
!MethodListPresenter categoriesFor: #createSchematicWiring!drag & drop!public! !
!MethodListPresenter categoriesFor: #deleteMethods!menus!public! !
!MethodListPresenter categoriesFor: #list:!public! !
!MethodListPresenter categoriesFor: #methodSelectionChanged!drag & drop!public! !
!MethodListPresenter categoriesFor: #onDrag:!drag & drop!public! !
!MethodListPresenter categoriesFor: #onLeftButtonPressed:!drag & drop!public! !
!MethodListPresenter categoriesFor: #primaryPresenter!drag & drop!label edit!public! !
!MethodListPresenter categoriesFor: #queryCommand:!public! !
!MethodListPresenter categoriesFor: #removeFromList!menus!public! !
!MethodListPresenter categoriesFor: #runTests!public! !
!MethodListPresenter categoriesFor: #selectedClasses!public! !
!MethodListPresenter categoriesFor: #selectedMethod!public! !
!MethodListPresenter categoriesFor: #selectFirstMethod!public! !
!MethodListPresenter categoriesFor: #selectionOrNil!public! !
!MethodListPresenter categoriesFor: #selectMethod:!public! !
!MethodListPresenter categoriesFor: #showHistory!menus!public! !
!MethodListPresenter categoriesFor: #subMenuName!menus!public! !
!MethodListPresenter categoriesFor: #subMenuPresenter!menus!public! !
!MethodListPresenter categoriesFor: #withSelectorDo:!public! !

!MethodListPresenter class methodsFor!

publishedEventsOfInstances
    
    	^super publishedEventsOfInstances
			add: #'classSelectionChanged';
			add: #'methodSelectionChanged';
			add: #'needFilterList';
			add: #'needIsMeta';
			add: #'needIsVariables';
			add: #'needClassList';
			yourself.
!

resource_MultiClass_view
	"Answer the literal data from which the 'MultiClass view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_MultiClass_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 2 410 ##(Smalltalk.ListView) 34 45 nil 416 34 2 8 1140920905 1025 576 590662 2 ##(Smalltalk.ListModel) 138 144 528 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 327686 ##(Smalltalk.Color) #default nil 13 265030 4 ##(Smalltalk.Menu) nil true 34 12 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #removeFromList 8 'Remove from &List' 1 1 nil nil nil 802 1 834 #browseClass 8 'Browse &Class' 1 1 nil nil nil 802 1 834 #deleteMethods 8 '&Delete method(s)' 1 1 nil nil nil 802 1 834 #showHistory 8 'Show &History' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 802 1 834 #browseImplementors 8 'Browse &Implementors' 1 1 nil nil nil 802 1 834 #browseImplementorsOf 8 'Browse Implementors of ...' 1 1 nil nil nil 802 1 834 #browseSenders 8 'Browse &Senders' 1 1 nil nil nil 802 1 834 #browseSendersOf 8 'Browse Senders of ...' 1 1 nil nil nil 802 1 834 #browseMethodsContaining 8 'Browse Methods Containing ...' 1 1 nil nil nil 1026 4097 802 1 834 #runTests 8 'Run &Test(s)' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 576 nil 8 1943883040 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 5 920646 5 ##(Smalltalk.ListViewColumn) 8 'Dictionary' 201 #left 1394 #displayString 8 #() 1394 #<= 1568 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 3 1 1600 8 'doIt' 8 '[:each | each gsClass dictionary]' 8 #[31 105 226 0 159 106] #gsClass #dictionary 1616 7 257 nil nil 576 nil 1 nil nil 1506 8 'Class Category' 241 #left 1394 #displayString 8 #() 1394 #<= 1760 1602 0 nil 1634 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each gsClass category]' 8 #[31 105 226 0 159 106] #gsClass #category 1792 7 257 nil nil 576 nil 3 nil nil 1506 8 'Class' 241 #left 1394 #displayString 8 #() 1394 #<= 1920 1602 0 nil 1634 3 1 1600 8 'doIt' 8 '[:each | each gsClass name]' 8 #[31 105 226 0 159 106] #gsClass #name 1952 7 257 nil nil 576 nil 3 nil nil 1506 8 'Method Category' 241 #left 1394 #displayString 1760 1394 #<= 1760 1602 0 nil 1634 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each category]' 8 #[30 105 226 0 106] #category 2096 7 257 nil nil 576 nil 3 nil nil 1506 8 'Method' 241 #left 1394 #displayString 1424 ##(Smalltalk.SortedCollection) 1602 0 nil 1634 2 1 1600 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 2224 7 257 nil nil 576 nil 3 nil nil #report 528 nil 131169 nil 1 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 1 2418 1201 401 576 2370 #contextMenu: 34 1 768 576 2370 #text: 34 1 8 'Dictionary' 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 200 0 0 0] 8 #() 2418 193 193 nil 35 8 'methodList' nil 2306 138 144 34 1 2370 #createAt:extent: 34 2 2418 5119 21 2418 1201 401 416 2546 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 210 0 0 0] 34 1 576 2608 nil 27 )! !
!MethodListPresenter class categoriesFor: #publishedEventsOfInstances!public! !
!MethodListPresenter class categoriesFor: #resource_MultiClass_view!public!resources-views! !

