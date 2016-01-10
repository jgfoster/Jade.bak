"Filed out from Dolphin Smalltalk 7"!

CodeSourcePresenter subclass: #MethodSourcePresenter
	instanceVariableNames: 'currentSelector'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MethodSourcePresenter guid: (GUID fromString: '{4EDA4C47-5B55-436D-B9E2-EF2042660A98}')!
MethodSourcePresenter comment: '
| newClass ymbolList oldClass source category result |
symbolList := System myUserProfile symbolList.
newClass := SSPAffanRecord.
oldClass := newClass classHistory at: 1.
oldClass selectors do: [:each | 
	source := oldClass sourceCodeAt: each.
	category := oldClass categoryOfSelector: each.
	result := newClass
		compileMethod: source
		dictionaries: symbolList
		category: category.
	result notNil ifTrue: [result halt].
].
'!
!MethodSourcePresenter categoriesForClass!Unclassified! !
!MethodSourcePresenter methodsFor!

canSetBreakpoints

	^true.
!

codePresenterIsMethod

	^true!

defaultMethod

^'methodSelector
	"method comment"

	| temps |
	^self yourself.
'.
!

fileSave
		"Answer whether the save succeeded (false means to stay on the window and cancel any attempt to leave)"

	| user theClass newSelector category string stream list warnings errors index a b c methodExists |
	(theClass := self trigger: #'needClass') isNil ifTrue: [^true].
	newSelector := self newSelector.
	currentSelector = newSelector ifFalse: [
		methodExists := self model
			serverPerform: #'class:includesSelector:'
			with: theClass
			with: newSelector.
		methodExists ifTrue: [
			(MessageBox confirm: 'Replace method?' caption: 'Method already exists!!') ifFalse: [^self].
		].
	].
	user := self trigger: #'needUser'.
	(category := self trigger: #'needMethodCategory') isNil ifTrue: [self error: 'We need a method category!!?'].
	string := self model
		serverPerform: #'compileMethod:behavior:user:inCategory:'
		with: documentPresenter value replaceCrLfWithLf
		with: theClass 
		with: user 
		with: category.

	stream := ReadStream on: string.
	(newSelector := stream nextLine) notEmpty ifTrue: [
		documentPresenter isModified: false.
		self 
			trigger: #'savedMethod:inClass:' 
				with: newSelector
				with: theClass;
			yourself.
	].
	(list := stream upToEnd subStrings: Character lf) isEmpty ifTrue: [^true].
	warnings := list select: [:each | each beginsWith: 'WARNING:'].
	warnings := warnings collect: [:each | each copyFrom: 10 to: each size].
	warnings notEmpty ifTrue: [
		ChoicePrompter
			choices: warnings
			caption: 'Compile warnings'.
	].
	errors := list select: [:each | each beginsWith: 'ERROR:'].
	errors isEmpty ifTrue: [^true].
	list := errors first subStrings: Character tab.
	string := documentPresenter value replaceCrLfWithLf.
	index := (list at: 3) asNumber - 1.
	a := (string copyFrom: 1 to: index) "replaceLfWithCrLf".
	b := (list at: 4) "replaceLfWithCrLf".
	c := (string copyFrom: index + 1 to: string size) "replaceLfWithCrLf".
	index := a size + 1.
	string := a , b , c.
	documentPresenter value: string.
	documentPresenter view
		selectionStart: index 
		length: b size.
	^false.
!

methodSource

	^documentPresenter value.
!

newSelector

	| string index list stream |
	string := documentPresenter value.
	#($" $| $. $;) do: [:each | 
		index := string indexOf: each.
		2 < index ifTrue: [string := string copyFrom: 1 to: index - 1].
	].
	list := string subStrings.
	string first isPunctuation ifTrue: [^list first].
	list first last = $: ifFalse: [^list first].
	stream := WriteStream on: String new.
	index := 1.
	[
		index < list size and: [(list at: index) last = $:].
	] whileTrue: [
		stream nextPutAll: (list at: index).
		index := index + 2.
	].
	^stream contents.


!

onViewOpened

	super onViewOpened.
	(documentPresenter view margins at: 2) isSensitive: false.
	self 
		updateCodeFont;
		setDefaultMethod;
		yourself.
!

setDefaultMethod

	documentPresenter 
		value: self defaultMethod;
		isModified: false;
		isReadOnly: false;
		yourself.
!

setEmptyMethod

	self statusBarText: ''.
	documentPresenter 
		value: '';
		isModified: false;
		isReadOnly: true;
		selectionRange: (1 to: 0);
		yourself.
!

update

	| theClass method string stream |
	self setEmptyMethod.
	(self trigger: #'needClass') isNil ifTrue: [^self].
	self setDefaultMethod.
	(theClass := self trigger: #'needMethodClass') isNil ifTrue: [^self].
	(method := self trigger: #'needMethod') isNil ifTrue: [^self].
	string := theClass sourceFor: method.
	stream := ReadStream on: string.
	string := (stream upTo: Character tab) , ' -- ' , (stream upTo: Character tab) , ' -- ' , stream nextLine.
	self statusBarText: string.
	documentPresenter 
		value: stream upToEnd;
		isModified: false;
		isReadOnly: false;
		yourself.
	documentPresenter view isEnabled: true.
	currentSelector := self newSelector.
! !
!MethodSourcePresenter categoriesFor: #canSetBreakpoints!Breakpoints!public! !
!MethodSourcePresenter categoriesFor: #codePresenterIsMethod!public! !
!MethodSourcePresenter categoriesFor: #defaultMethod!public! !
!MethodSourcePresenter categoriesFor: #fileSave!public! !
!MethodSourcePresenter categoriesFor: #methodSource!public! !
!MethodSourcePresenter categoriesFor: #newSelector!public! !
!MethodSourcePresenter categoriesFor: #onViewOpened!public! !
!MethodSourcePresenter categoriesFor: #setDefaultMethod!public! !
!MethodSourcePresenter categoriesFor: #setEmptyMethod!public! !
!MethodSourcePresenter categoriesFor: #update!public! !

!MethodSourcePresenter class methodsFor!

publishedEventsOfInstances
    
    	^super publishedEventsOfInstances
			add: #'needClass';
			add: #'needMethodClass';
			add: #'needMethod';
			add: #'needUser';
			add: #'savedMethod:inClass:';
			yourself.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1140851500 131137 560 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 517 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point)  193 193 0 560 642 672 8 4294903133 234 256 98 0 234 256 98 24 16241 1246982 ##(Smalltalk.ToolbarSystemButton)  16241 0 560 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #redo 8 'Redo' 1 1 0 1 9 16243 898 16243 0 560 1 930 8 #editFind 8 'Find' 1 1 0 1 25 16245 898 16245 0 560 1 930 8 #editReplace 8 'Replace' 1 1 0 1 27 16247 853766 ##(Smalltalk.ToolbarButton)  16247 0 560 1 930 8 #jadeDisplay 8 'Print Result of Selection or Line' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.ImageRelativeFileLocator)  8 #current 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 770 1857 33 55 16249 1122 16249 0 560 1 930 8 #jadeExecute 8 'Evaluate Selection or Line' 1 1 0 1216 57 16251 1122 16251 0 560 1 930 8 #jadeInspect 8 'Inspect Selection or Line' 1 1 0 1216 59 16229 898 16229 0 560 1 930 8 #fileSave 8 'Save' 1 1 0 1 17 16231 898 16231 0 560 1 930 8 #editCut 8 'Cut' 1 1 0 1 1 16233 898 16233 0 560 1 930 8 #editCopy 8 'Copy' 1 1 0 1 3 16235 898 16235 0 560 1 930 8 #editPaste 8 'Paste' 1 1 0 1 5 16237 898 16237 0 560 1 930 8 #editDelete 8 'Delete' 1 1 0 1 11 16239 898 16239 0 560 1 930 8 #undo 8 'Undo' 1 1 0 1 7 98 15 1504 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 560 3 0 1 1568 1632 1696 1760 1824 912 1906 0 0 560 3 0 1 992 1056 1906 0 0 560 3 0 1 1136 1376 1440 234 240 98 4 1 1 1216 31 0 1 0 770 33 33 770 45 45 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 770 1 1 770 1001 51 560 2098 8 #updateSize 848 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 244 1 0 0 25 0 0 0] 98 0 770 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1242 8 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 1242 2352 8 #fixedParentRight 1 1242 2352 8 #fixedParentTop 1 1242 2352 8 #fixedViewTop 51 410 8 ##(Smalltalk.ScintillaView)  98 46 0 416 98 2 8 1176571972 1025 2480 721990 2 ##(Smalltalk.ValueHolder)  0 32 1242 8 ##(Smalltalk.SearchPolicy)  8 #equality 0 196934 1 ##(Smalltalk.RGB)  27387381 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 5 2674 0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 930 8 #browseImplementors 8 'Browse &Implementors' 1 1 0 0 0 2754 1 930 8 #browseSenders 8 'Browse &Senders' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 2754 1 930 8 #browseReferences 8 'Browse &References' 1 1 0 0 0 2754 1 930 8 #browseClass 8 'Browse &Class' 1 1 0 0 0 2898 4097 2754 1 930 8 #browseMethodsWithString 8 'Browse &Methods with String' 1 1 0 0 0 8 '&Browse' 0 134217729 0 0 0 0 0 2674 0 16 98 10 2754 1 930 1536 8 '&Save' 9383 1 0 0 0 2898 4097 2754 1 930 1856 8 '&Undo' 9397 1 0 0 0 2754 1 930 960 8 'Redo' 9395 1 0 0 0 2898 4097 2754 1 930 1600 8 'Cu&t' 9393 1 0 0 0 2754 1 930 1664 8 '&Copy' 9351 1 0 0 0 2754 1 930 1728 8 '&Paste' 9389 1 0 0 0 2754 1 930 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 2754 1 930 1792 8 'De&lete' 1629 1 0 0 0 8 '&Edit' 0 134217729 0 0 0 0 0 2674 0 16 98 6 2754 1 930 1168 8 '&Display' 9353 1 0 0 0 2754 1 930 1408 8 '&Execute' 9355 1 0 0 0 2754 1 930 1472 8 '&Inspect' 9363 1 0 0 0 2754 1 930 1472 8 '&Query' 9379 1 0 0 0 2898 4097 2754 1 930 8 #fileIn 8 'File In' 1 1 0 0 0 8 'E&xecute' 0 134217729 0 0 0 0 0 2674 0 16 98 3 2754 1 930 1024 8 '&Find...' 9357 1 0 0 0 2754 1 930 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 2754 1 930 1088 8 '&Replace...' 9361 1 0 0 0 8 '&Find' 0 134217729 0 0 0 0 0 2674 0 16 98 2 2754 1 930 8 #addQuotesToSelection 8 '&Add' 1 1 0 0 0 2754 1 930 8 #removeQuotesFromSelection 8 '&Remove' 1 1 0 0 0 8 '&Quotes' 0 134217729 0 0 0 0 0 8 '' 0 1 0 0 0 0 0 690 0 16 722 8 #[244 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 770 193 193 0 2480 0 61692557 852486 ##(Smalltalk.NullConverter)  0 0 9 0 234 256 98 42 8 #braceMismatch 1182726 ##(Smalltalk.ScintillaTextStyle)  71 786694 ##(Smalltalk.IndexedColor)  33554459 0 3 0 0 0 0 4480 0 0 0 8 #character 4498 31 2642 16646399 0 3 0 0 0 0 4560 0 0 0 8 #indentGuide 4498 75 4530 33554447 0 1 0 0 0 0 4608 0 0 0 8 #string 4498 3 2642 16646399 0 129 0 0 0 0 4656 0 0 0 8 #global 4498 21 0 0 3 0 0 0 0 4704 0 0 0 8 #keywordSend 4498 27 4530 33554437 0 3 0 0 0 0 4736 0 0 0 8 #boolean 4498 13 2642 16646145 0 3 0 0 0 0 4784 0 0 0 8 #nil 4498 19 4816 0 3 0 0 0 0 4832 0 0 0 8 #number 4498 5 2642 16711169 0 1 0 0 0 0 4864 0 0 0 8 #binary 4498 11 4530 33554433 0 1 0 0 0 0 4912 0 0 0 8 #assignment 4498 29 0 0 3 0 0 0 0 4960 0 0 0 8 #symbol 4498 9 4530 33554443 0 1 0 0 0 0 4992 0 0 0 8 #self 4498 15 4816 0 3 0 0 0 0 5040 0 0 0 8 #return 4498 23 2642 321 0 3 0 0 0 0 5072 0 0 0 8 #super 4498 17 4816 0 3 0 0 0 0 5120 0 0 0 8 #specialSelector 4498 33 4816 0 3 0 0 0 0 5152 0 0 0 8 #special 4498 25 0 0 3 0 0 0 0 5184 0 0 0 8 #lineNumber 4498 67 0 0 1 0 0 0 0 5216 0 0 0 8 #normal 4498 1 0 0 1 0 0 0 0 5248 0 0 0 8 #comment 4498 7 2642 65025 0 1 0 0 0 0 5280 0 0 0 8 #braceHighlight 4498 69 4530 33554465 0 3 0 0 0 0 5328 0 0 0 98 40 5264 4672 4880 5296 5008 4928 4800 5056 5136 4848 4720 5088 5200 4752 4976 4576 5168 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 5232 5344 4512 0 4624 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  5248 234 256 98 16 8 #folderTail 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  57 11 4944 4944 2480 5456 8 #folder 5474 61 5 4944 4944 2480 5504 8 #folderOpen 5474 63 13 4944 4944 2480 5536 8 #folderOpenMid 5474 53 11 4530 33554471 4944 2480 5568 8 #folderEnd 5474 51 11 5600 4944 2480 5616 8 #folderMidTail 5474 55 11 5600 4944 2480 5648 8 #circle 5474 1 1 4944 4544 2480 5680 8 #folderSub 5474 59 11 4944 4944 2480 5712 202 208 848 0 63 0 0 0 0 0 4640 0 0 0 0 0 0 8 '' 7 234 256 98 2 8 #container 234 256 98 2 5248 4498 1 0 0 1 0 0 0 0 5248 0 0 0 0 0 8 #arrows 0 1 0 0 2034 202 208 98 11 2098 2128 98 2 770 1 51 770 1001 551 2480 2098 8 #contextMenu: 98 1 2688 2480 2098 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 2480 2098 8 #isTextModified: 98 1 32 2480 2098 8 #modificationEventMask: 98 1 9215 2480 2098 8 #indicatorDefinitions: 98 1 98 3 1836038 ##(Smalltalk.ScintillaIndicatorDefinition)  1 2480 65025 3 6290 3 2480 33423361 5 6290 5 2480 511 1 2480 2098 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 2480 41 3 32 1 6418 3 2480 33 1 16 67108863 6418 5 2480 1 1 16 -67108863 2480 2098 8 #hasIndentationGuides: 98 1 16 2480 2098 8 #tabIndents: 98 1 16 2480 2098 8 #tabWidth: 98 1 9 2480 2098 8 #setLexerLanguage: 98 1 8 #smalltalk 2480 2226 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 244 1 0 0 44 1 0 0] 98 0 2288 0 27 2306 2336 1 2384 1 2416 51 1242 2352 8 #fixedParentBottom 1 234 256 98 2 2480 8 'document' 0 2034 202 208 98 1 2098 2128 98 2 770 2799 21 770 1001 601 416 2226 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 5 0 0 10 0 0 0 107 7 0 0 54 1 0 0] 98 2 560 2480 2288 0 27 )! !
!MethodSourcePresenter class categoriesFor: #publishedEventsOfInstances!public! !
!MethodSourcePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

