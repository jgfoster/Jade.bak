"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #MethodListPresenter
	instanceVariableNames: 'classListPresenter methodListPresenter inheritanceListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MethodListPresenter guid: (GUID fromString: '{d704c96d-6d80-444e-8a76-ac5b0124bc86}')!
MethodListPresenter comment: ''!
!MethodListPresenter categoriesForClass!Unclassified! !
!MethodListPresenter methodsFor!

allowedToDeleteMethods

	^methodListPresenter selections notEmpty and: [classListPresenter view ~= DeafObject new and: [classListPresenter selection == classListPresenter list last]].
!

anyMethod

	^methodListPresenter list anyOne.
!

browseClass

	| selection browserClass |
	selection := methodListPresenter selection.
	browserClass := Smalltalk at: #JadeSystemBrowser.		"Avoid circular dependencies on load order"
	(browserClass showOnSession: self gciSession)
		selectClass: selection gsClass name
		selector: selection name.
!

browseImplementors

	super browseImplementorsOf: methodListPresenter selection.

	"parentPresenter browseImplementorsOf: methodListPresenter selection"
!

browseImplementorsOf

	self withSelectorDo: [:aSelector | self browseImplementorsOf: aSelector].
!

browseMethodsContaining

	| string |
	(string := Prompter prompt: 'Enter substring:') isNil ifTrue: [^self].
	(JadeMethodListBrowser showOnSession: self gciSession) primaryPresenter browseMethodsContaining: string.
!

browseSenders

	super browseSendersOf: methodListPresenter selection.

	"parentPresenter browseImplementorsOf: methodListPresenter selection"
!

browseSendersOf

	self withSelectorDo: [:aSelector | self browseSendersOf: aSelector].
!

createComponents

	super createComponents.
	classListPresenter 		:=  self add: ListPresenter		new name: 'classList'.
	methodListPresenter 	:=  self add: ListPresenter		new name: 'methodList'.
	inheritanceListPresenter 	:=  self add: ListPresenter		new name: 'inheritanceList'.
!

createSchematicWiring

	super createSchematicWiring.
	methodListPresenter		when: #actionPerformed	 		send: #browseClass				to: self.

	classListPresenter		when: #selectionChanging: 		send: #selectionChanging:			to: self.
	methodListPresenter		when: #selectionChanging: 		send: #selectionChanging:			to: self.
	inheritanceListPresenter	when: #selectionChanging: 		send: #selectionChanging:			to: self.

	classListPresenter 		when: #'selectionChanged' 			send: #'trigger:'					to: self 	with: #'classSelectionChanged'.
	methodListPresenter 	when: #'selectionChanged' 			send: #'updateInheritanceList'		to: self.
	inheritanceListPresenter 	when: #'selectionChanged' 			send: #'methodSelectionChanged' 	to: self .

	methodListPresenter		when: #labelOf:editedTo:accept: 	send: #labelOf:editedTo:accept:	to: self.

	self createDragDropSchematicWiringFor: methodListPresenter.
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

primaryPresenter

	^methodListPresenter.
!

queryCommand: aCommandQuery

	(#(#'browseClass' #'browseImplementors' #'browseSenders') includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery 
			isEnabled: methodListPresenter selections size = 1;
			receiver: self;
			yourself.
		^true.
	].
	(#(#'removeMethods' ) includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: self allowedToDeleteMethods.
		^true.
	].
	(#(#'runTests') includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: (methodListPresenter selections notEmpty and: [
			methodListPresenter selections allSatisfy: [:each | each isTestCase] ]).
		^true.
	].
	(#(#'removeFromList') includes: aCommandQuery commandSymbol) ifTrue: [
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

removeMethods

	methodListPresenter selections do: [:each | 
		self gciSession
			serverPerform: #'removeMethod:'
			with: each.
	].
	self update.
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

selectedClass

	| myClass |
	(myClass := inheritanceListPresenter selectionOrNil) notNil ifTrue: [^myClass].
	(myClass 	:= self trigger: #'needClass') isNil ifTrue: [^nil].
	(self trigger: #'needIsMeta') ifFalse: [^myClass].
	^(myClass superclassListForMeta: true) last.
!

selectedClasses

	| index list |
	classListPresenter view = DeafObject new ifTrue: [^nil].
	classListPresenter hasSelection ifFalse: [^nil].
	index := classListPresenter selectionByIndex.
	list := classListPresenter list copyFrom: index to: classListPresenter list size.
	^list.
!

selectedMethod

	^methodListPresenter selectionOrNil.
!

selectedMethodClass

	^inheritanceListPresenter selectionOrNil.
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

subMenuName

	^'&Methods'.
!

subMenuPresenter

	^methodListPresenter.
!

update

	self 
		updateSelecting: nil 
		inClass: nil.
!

updateClassList

	| myClass isMeta list |
	classListPresenter list: #().
	(myClass 	:= self trigger: #'needClass') isNil ifTrue: [^self].
	isMeta 	:= self trigger: #'needIsMeta'.
	list := myClass superclassListForMeta: isMeta.
	classListPresenter 
		list: list;
		selection: list last;
		yourself.
!

updateInheritanceList

	| myClass gsMethod list |
	inheritanceListPresenter list: #().
	(gsMethod := methodListPresenter selectionOrNil) isNil ifTrue: [
		^self methodSelectionChanged.
	].
	(list := classListPresenter list) isEmpty ifTrue: [
		^self methodSelectionChanged.
	].
	myClass := list last.
	list := myClass implementorsOf: gsMethod.
	list := list reverse.
	inheritanceListPresenter
		list: list;
		selection: list last;
		yourself.
!

updateSelecting: aSymbol

	| classList isVariables filterList list newMethod |
	(classList := self selectedClasses) isNil ifTrue: [
		methodListPresenter selectionOrNil: nil.
		^self.
	].
	methodListPresenter list: #().
	inheritanceListPresenter list: #().
	isVariables 	:= self trigger: #'needIsVariables'.
	filterList		:= self trigger: #'needFilterList'.
	list := classList last
		methodsUpTo: classList first 
		filterList: filterList 
		isVariables: isVariables.
	newMethod := list 
		detect: [:each | each name = aSymbol]
		ifNone: [nil].
	methodListPresenter 
		list: list asSortedCollection;
		selectionOrNil: newMethod;
		yourself.
!

updateSelecting: aSymbol inClass: aClass

	| classList newMethod isVariables filterList list |
	methodListPresenter list: #().
	inheritanceListPresenter list: #().
	(classList := self selectedClasses) isNil ifTrue: [
		newMethod := methodListPresenter list
			detect: [:each | each gsClass = aClass and: [each name = aSymbol]]
			ifNone: [nil].
		methodListPresenter selectionOrNil: newMethod.
		^self.
	].
	isVariables 	:= self trigger: #'needIsVariables'.
	filterList		:= self trigger: #'needFilterList'.
	list := classList last
		methodsUpTo: classList first 
		filterList: filterList 
		isVariables: isVariables.
	newMethod := list 
		detect: [:each | each name = aSymbol]
		ifNone: [nil].
	methodListPresenter 
		list: list asSortedCollection;
		selectionOrNil: newMethod;
		yourself.
	newMethod isNil ifTrue: [self trigger: #methodSelectionChanged].
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
		with: selector.
	result isNil ifTrue: [^self].
	list := result subStrings: Character lf.
	(selector := ChoicePrompter choices: list) isNil ifTrue: [^self].
	aBlock value: selector.
! !
!MethodListPresenter categoriesFor: #allowedToDeleteMethods!public! !
!MethodListPresenter categoriesFor: #anyMethod!public! !
!MethodListPresenter categoriesFor: #browseClass!public! !
!MethodListPresenter categoriesFor: #browseImplementors!public! !
!MethodListPresenter categoriesFor: #browseImplementorsOf!public! !
!MethodListPresenter categoriesFor: #browseMethodsContaining!public! !
!MethodListPresenter categoriesFor: #browseSenders!public! !
!MethodListPresenter categoriesFor: #browseSendersOf!public! !
!MethodListPresenter categoriesFor: #createComponents!public! !
!MethodListPresenter categoriesFor: #createSchematicWiring!drag & drop!public! !
!MethodListPresenter categoriesFor: #list:!public! !
!MethodListPresenter categoriesFor: #methodSelectionChanged!drag & drop!public! !
!MethodListPresenter categoriesFor: #onDrag:!drag & drop!public! !
!MethodListPresenter categoriesFor: #primaryPresenter!drag & drop!label edit!public! !
!MethodListPresenter categoriesFor: #queryCommand:!public! !
!MethodListPresenter categoriesFor: #removeFromList!menus!public! !
!MethodListPresenter categoriesFor: #removeMethods!public! !
!MethodListPresenter categoriesFor: #runTests!public! !
!MethodListPresenter categoriesFor: #selectedClass!public! !
!MethodListPresenter categoriesFor: #selectedClasses!public! !
!MethodListPresenter categoriesFor: #selectedMethod!public! !
!MethodListPresenter categoriesFor: #selectedMethodClass!public! !
!MethodListPresenter categoriesFor: #selectFirstMethod!public! !
!MethodListPresenter categoriesFor: #selectionOrNil!public! !
!MethodListPresenter categoriesFor: #selectMethod:!public! !
!MethodListPresenter categoriesFor: #subMenuName!menus!public! !
!MethodListPresenter categoriesFor: #subMenuPresenter!menus!public! !
!MethodListPresenter categoriesFor: #update!public! !
!MethodListPresenter categoriesFor: #updateClassList!public! !
!MethodListPresenter categoriesFor: #updateInheritanceList!public! !
!MethodListPresenter categoriesFor: #updateSelecting:!public! !
!MethodListPresenter categoriesFor: #updateSelecting:inClass:!public! !
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 2 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920905 1025 592 590662 2 ##(Smalltalk.ListModel)  202 208 544 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  524550 ##(Smalltalk.ColorRef)  8 4278190080 0 13 265030 4 ##(Smalltalk.Menu)  0 16 98 12 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #removeFromList 8 'Remove from &List' 1 1 0 0 0 850 1 882 8 #browseClass 8 'Browse Class' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 850 1 882 8 #browseImplementors 8 'Browse &Implementors' 1 1 0 0 0 850 1 882 8 #browseImplementorsOf 8 'Browse Implementors of ...' 1 1 0 0 0 850 1 882 8 #browseSenders 8 'Browse &Senders' 1 1 0 0 0 850 1 882 8 #browseSendersOf 8 'Browse Senders of ...' 1 1 0 0 0 850 1 882 8 #browseMethodsContaining 8 'Browse Methods Containing ...' 1 1 0 0 0 1010 4097 850 1 882 8 #removeMethods 8 '&Delete Method(s)' 1 1 0 0 0 850 1 882 8 #rename 8 '&Rename' 1251 1 0 0 0 850 1 882 8 #runTests 8 'Run &Test(s)' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 592 0 8 4294902785 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 4 920646 5 ##(Smalltalk.ListViewColumn)  8 'Class Category' 299 8 #left 1602 1632 98 0 1602 8 #<= 1808 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  3 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each gsClass category]' 8 #[31 105 226 0 159 106] 8 #gsClass 8 #category 1872 7 257 0 0 592 0 3 0 0 1730 8 'Class' 299 1776 1602 1632 98 0 1602 1840 2064 1858 0 0 1890 3 1 1856 8 'doIt' 8 '[:each | each gsClass name]' 8 #[31 105 226 0 159 106] 1984 8 #name 2096 7 257 0 0 592 0 3 0 0 1730 8 'Method Category' 299 1776 1602 1632 1808 1602 1840 1808 1858 0 0 1890 2 1 1920 8 'doIt' 8 '[:each | each category]' 8 #[30 105 226 0 106] 2000 2256 7 257 0 0 592 0 3 0 0 1730 8 'Method' 299 1776 1602 1632 1648 8 ##(Smalltalk.SortedCollection)  1858 0 0 1890 2 1 1856 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] 2176 2400 7 257 0 0 592 0 3 0 0 8 #report 544 0 131169 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 2626 1201 401 592 2562 8 #contextMenu: 98 1 816 592 2562 8 #text: 98 1 8 'Class Category' 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 200 0 0 0] 98 0 2626 193 193 0 27 8 'methodList' 0 2498 202 208 98 1 2562 2592 98 2 2626 5119 21 2626 1201 401 416 2786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 210 0 0 0] 98 1 592 2848 0 27 )! !
!MethodListPresenter class categoriesFor: #publishedEventsOfInstances!public! !
!MethodListPresenter class categoriesFor: #resource_MultiClass_view!public!resources-views! !

