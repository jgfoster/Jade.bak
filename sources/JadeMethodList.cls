"Filed out from Dolphin Smalltalk 7"!

JadeCodeBrowser subclass: #JadeMethodList
	instanceVariableNames: 'methodListPresenter searchString captionString'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMethodList guid: (GUID fromString: '{82986EA3-B8FB-45C8-A0BA-7A389D8F9DFC}')!
JadeMethodList comment: ''!
!JadeMethodList categoriesForClass!Unclassified! !
!JadeMethodList methodsFor!

captionString: aString

	captionString := 'Method(s) ' , aString.
	self updateCaption.
!

countOf: subString in: string

	| index next count |
	count := 0.
	index := 1.
	[
		next := string 
			indexOfSubCollection: subString 
			startingAt: index.
		next > 0.
	] whileTrue: [
		index := next + 1.
		((next = 1 or: [(string at: next - 1) isAlphaNumeric not]) and: [
		next + subString size > string size or: [(string at: next + subString size) isAlphaNumeric not]]) ifTrue: [
				count := count + 1.
		].
	].
	^count.
!

createComponents

	super createComponents.
	methodListPresenter 	:= self add: ListPresenter		new name: 'methodList'.
!

createSchematicWiring

	super createSchematicWiring.
	methodListPresenter 	
		when: #selectionChanged 	send: #selectedInMethodList 	to: self;
		when: #leftButtonPressed:	send: #doSearchFork					to: self;
		yourself.
!

doSearch

	| myView foundString lineNumber string |
	searchString isNil ifTrue: [^self].
	myView := documentPresenter view.
	string := searchString.
	(string includes: $:) ifTrue: [string := (string subStrings: $:) first , ':'].
	foundString := myView 
		findNextWrappedEx: string 
		down: true 
		wholeWord: true 
		matchCase: true.
	foundString ifFalse: [
		foundString := myView 
			findNextWrappedEx: string 
			down: true 
			wholeWord: false  
			matchCase: true.
	].
	foundString ifFalse: [^self].
	lineNumber := myView lineFromPosition: myView selectionRange first.
	lineNumber := lineNumber - 4 max: 1.
	myView lineScroll: lineNumber.
	self updateCaption.
!

doSearchFork

	searchString isNil ifTrue: [^self].
	[ 
		self doSearch.
	] forkAt: Processor userBackgroundPriority.
!

filename: aString

	(JadeWorkspace showOnSession: gciSession)
		filename: aString;
		fileLoad.
!

methodList: aList

	methodListPresenter list: aList.
!

removeFromList

	| oldList selections newList |
	oldList := methodListPresenter list asOrderedCollection.
	selections := methodListPresenter selections.
	newList := oldList copyWithoutAll: selections.
	methodListPresenter 
		resetSelection;
		list: newList.
!

searchFor: aString

	searchString := aString.
!

selectedInMethodList 

	| method gsCode result |
	documentPresenter clear.
	self caption: captionString.
	(method := methodListPresenter selectionOrNil) isNil ifTrue: [^self].
	gsCode := method gsBehavior , ' sourceCodeAt: #' , method name printString.
	(result := gciSession executeString: gsCode) isNil ifTrue: [^self].
	documentPresenter
		value: result "replaceLfWithCrLf";
		isModified: false;
		yourself.
	self doSearch.
!

updateCaption

	| selectionRange index next beforeCount afterCount string |
	captionString notNil ifTrue: [self caption: captionString].
	(selectionRange := documentPresenter view selectionRange) isEmpty ifTrue: [^self].
	string := documentPresenter value  
		copyFrom: 1
		to: selectionRange start.
	beforeCount := self
		countOf: searchString 
		in: string.
	string := documentPresenter value  
		copyFrom: selectionRange stop
		to: documentPresenter value size.
	afterCount := self
		countOf: searchString 
		in: string.
	self caption: captionString , ' (' , (beforeCount + 1) printString , ' of ' , (beforeCount + afterCount + 1) printString , ')'.
! !
!JadeMethodList categoriesFor: #captionString:!public! !
!JadeMethodList categoriesFor: #countOf:in:!public! !
!JadeMethodList categoriesFor: #createComponents!public! !
!JadeMethodList categoriesFor: #createSchematicWiring!public! !
!JadeMethodList categoriesFor: #doSearch!public! !
!JadeMethodList categoriesFor: #doSearchFork!public! !
!JadeMethodList categoriesFor: #filename:!public! !
!JadeMethodList categoriesFor: #methodList:!public! !
!JadeMethodList categoriesFor: #removeFromList!public! !
!JadeMethodList categoriesFor: #searchFor:!public! !
!JadeMethodList categoriesFor: #selectedInMethodList!public! !
!JadeMethodList categoriesFor: #updateCaption!public! !

!JadeMethodList class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 98 4 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 656 0 482 8 4278190080 0 7 0 0 0 656 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.MethodSourcePresenter)  8 #resource_Default_view 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 371 530 1169 353 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 185 0 0 0 72 2 0 0 105 1 0 0] 608 530 193 193 0 27 8 'codePane' 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920393 1025 1072 590662 2 ##(Smalltalk.ListModel)  202 208 608 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 512 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #removeFromList 8 'Remove from list' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 1072 0 8 4294903309 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 3 920646 5 ##(Smalltalk.ListViewColumn)  8 'Dictionary' 241 8 #left 459270 ##(Smalltalk.Message)  8 #displayString 98 0 1586 8 #<= 1632 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 1680 8 'doIt' 8 '[:each | each symbolDictionaryName]' 8 #[30 105 226 0 106] 8 #symbolDictionaryName 1696 7 257 0 0 1072 0 1 0 0 1522 8 'Class' 301 1568 1424 8 ##(Smalltalk.SortedCollection)  1682 0 459302 ##(Smalltalk.Context)  1 1 0 0 1714 1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each gsBehavior]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  608 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #gsBehavior 17 257 0 0 1072 0 1 0 0 1522 8 'Method' 601 1568 1424 1840 1682 0 1874 1 1 0 0 1714 1 9 1920 8 'doIt' 98 2 8 '[:each | each name]' 98 1 202 2016 608 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #name 17 257 0 0 1072 0 1 0 0 8 #report 608 0 131169 0 0 834 202 208 98 3 898 928 98 2 530 1 1 530 1169 353 1072 898 8 #contextMenu: 98 1 1264 1072 898 8 #text: 98 1 8 'Dictionary' 1072 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 176 0 0 0] 98 0 1040 0 27 8 'methodList' 0 0 0 0 0 1 263494 3 ##(Smalltalk.Icon)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'icons\GS32x32.ico' 0 2562 0 16 2608 8 'icons\GS16x16.ico' 0 0 0 1 0 0 834 202 208 98 3 898 928 98 2 530 2879 21 530 1201 801 416 898 2448 98 1 8 'Jade Method List' 416 898 8 #updateMenuBar 608 416 994 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 98 3 1072 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2912 0 482 512 0 519 0 0 0 2912 834 202 208 98 1 898 928 98 2 530 1 353 530 1169 19 2912 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 176 0 0 0 72 2 0 0 185 0 0 0] 98 0 1040 0 27 656 1040 0 27 )! !
!JadeMethodList class categoriesFor: #resource_Default_view!public!resources-views! !

