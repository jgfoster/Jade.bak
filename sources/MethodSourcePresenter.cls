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
		methodExists := gciSession
			serverPerform: #'class:includesSelector:'
			with: theClass
			with: newSelector.
		methodExists ifTrue: [
			(MessageBox confirm: 'Replace method?' caption: 'Method already exists!!') ifFalse: [^self].
		].
	].
	user := self trigger: #'needUser'.
	(category := self trigger: #'needMethodCategory') isNil ifTrue: [self error: 'We need a method category!!?'].
	string := gciSession
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ContainerView) 98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 234 240 98 4 410 8 ##(Smalltalk.Toolbar) 98 25 0 416 98 2 8 1140851500 131137 560 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 517 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 560 642 672 8 4294905905 234 256 98 0 234 256 98 24 11787 1246982 ##(Smalltalk.ToolbarSystemButton) 11787 0 560 1 1180998 4 ##(Smalltalk.CommandDescription) 8 #fileSave 8 'Save' 1 1 0 1 17 11789 898 11789 0 560 1 930 8 #editCut 8 'Cut' 1 1 0 1 1 11791 898 11791 0 560 1 930 8 #editCopy 8 'Copy' 1 1 0 1 3 11793 898 11793 0 560 1 930 8 #editPaste 8 'Paste' 1 1 0 1 5 11795 898 11795 0 560 1 930 8 #editDelete 8 'Delete' 1 1 0 1 11 11797 898 11797 0 560 1 930 8 #undo 8 'Undo' 1 1 0 1 7 11799 898 11799 0 560 1 930 8 #redo 8 'Redo' 1 1 0 1 9 11801 898 11801 0 560 1 930 8 #editFind 8 'Find' 1 1 0 1 25 11803 898 11803 0 560 1 930 8 #editReplace 8 'Replace' 1 1 0 1 27 11805 853766 ##(Smalltalk.ToolbarButton) 11805 0 560 1 930 8 #jadeDisplay 8 'Print Result of Selection or Line' 1 1 0 395334 3 ##(Smalltalk.Bitmap) 0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 0 7 770 1857 33 55 11807 1506 11807 0 560 1 930 8 #jadeExecute 8 'Evaluate Selection or Line' 1 1 0 1600 57 11809 1506 11809 0 560 1 930 8 #jadeInspect 8 'Inspect Selection or Line' 1 1 0 1600 59 98 15 912 1050118 ##(Smalltalk.ToolbarSeparator) 0 0 560 3 0 1 992 1056 1120 1184 1248 1312 1874 0 0 560 3 0 1 1376 1440 1874 0 0 560 3 0 1 1520 1728 1792 234 240 98 4 1 1 1600 31 0 1 0 770 33 33 770 45 45 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 770 1 1 770 1001 51 560 2066 8 #updateSizePosted 848 560 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 244 1 0 0 25 0 0 0] 98 0 770 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) 8 #fixedParentLeft 1 2306 8 #fixedParentRight 1 2306 8 #fixedParentTop 1 2306 8 #fixedViewTop 51 410 8 ##(Smalltalk.ScintillaView) 98 50 0 416 98 2 8 1176571972 1025 2448 721990 2 ##(Smalltalk.ValueHolder) 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 196934 1 ##(Smalltalk.RGB) 27387381 0 5 265030 4 ##(Smalltalk.Menu) 0 16 98 5 2626 0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem) 1 930 8 #browseImplementors 8 'Browse &Implementors' 1 1 0 0 0 2706 1 930 8 #browseSenders 8 'Browse &Senders' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 2706 1 930 8 #browseReferences 8 'Browse &References' 1 1 0 0 0 2706 1 930 8 #browseClass 8 'Browse &Class' 1 1 0 0 0 2850 4097 2706 1 930 8 #browseMethodsWithString 8 'Browse &Methods with String' 1 1 0 0 0 8 '&Browse' 0 134217729 0 0 0 0 0 2626 0 16 98 10 2706 1 930 960 8 '&Save' 9383 1 0 0 0 2850 4097 2706 1 930 1280 8 '&Undo' 9397 1 0 0 0 2706 1 930 1344 8 'Redo' 9395 1 0 0 0 2850 4097 2706 1 930 1024 8 'Cu&t' 9393 1 0 0 0 2706 1 930 1088 8 '&Copy' 9351 1 0 0 0 2706 1 930 1152 8 '&Paste' 9389 1 0 0 0 2706 1 930 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 2706 1 930 1216 8 'De&lete' 1629 1 0 0 0 8 '&Edit' 0 134217729 0 0 0 0 0 2626 0 16 98 6 2706 1 930 1552 8 '&Display' 9353 1 0 0 0 2706 1 930 1760 8 '&Execute' 9355 1 0 0 0 2706 1 930 1824 8 '&Inspect' 9363 1 0 0 0 2706 1 930 1824 8 '&Query' 9379 1 0 0 0 2850 4097 2706 1 930 8 #fileIn 8 'File In' 1 1 0 0 0 8 'E&xecute' 0 134217729 0 0 0 0 0 2626 0 16 98 3 2706 1 930 1408 8 '&Find...' 9357 1 0 0 0 2706 1 930 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 2706 1 930 1472 8 '&Replace...' 9361 1 0 0 0 8 '&Find' 0 134217729 0 0 0 0 0 2626 0 16 98 2 2706 1 930 8 #addQuotesToSelection 8 '&Add' 1 1 0 0 0 2706 1 930 8 #removeQuotesFromSelection 8 '&Remove' 1 1 0 0 0 8 '&Quotes' 0 134217729 0 0 0 0 0 8 '' 0 1 0 0 0 0 0 690 0 16 722 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 770 193 193 0 2448 0 8 4294903335 852486 ##(Smalltalk.NullConverter) 0 0 9 0 234 256 98 44 8 #braceMismatch 1182790 1 ##(Smalltalk.ScintillaTextStyle) 71 786694 ##(Smalltalk.IndexedColor) 33554459 0 3 0 0 0 0 4448 0 1401 0 8 #normal 4466 1 0 0 1 0 0 0 0 4528 0 0 0 8 #specialSelector 4466 33 2594 16646145 0 3 0 0 0 0 4560 0 1401 0 8 #global 4466 21 0 0 3 0 0 0 0 4608 0 1401 0 8 #symbol 4466 9 4498 33554443 0 1 0 0 0 0 4640 0 0 0 8 #binary 4466 11 4498 33554433 0 1 0 0 0 0 4688 0 0 0 8 #string 4466 3 2594 16646399 0 129 0 0 0 0 4736 0 0 0 8 #nil 4466 19 4592 0 3 0 0 0 0 4784 0 1401 0 8 #assignment 4466 29 0 0 3 0 0 0 0 4816 0 1401 0 8 #callTip 4466 77 4498 33554457 4498 33554471 1 0 0 0 0 4848 0 0 0 8 #self 4466 15 4592 0 3 0 0 0 0 4912 0 1401 0 8 #character 4466 31 2594 16646399 0 3 0 0 0 0 4944 0 1401 0 8 #super 4466 17 4592 0 3 0 0 0 0 4992 0 1401 0 8 #keywordSend 4466 27 4498 33554437 0 3 0 0 0 0 5024 0 1401 0 8 #braceHighlight 4466 69 4498 33554465 0 3 0 0 0 0 5072 0 1401 0 8 #special 4466 25 0 0 3 0 0 0 0 5120 0 1401 0 8 #lineNumber 4466 67 0 0 1 0 0 0 0 5152 0 0 0 8 #indentGuide 4466 75 4498 33554447 0 1 0 0 0 0 5184 0 0 0 8 #boolean 4466 13 4592 0 3 0 0 0 0 5232 0 1401 0 8 #comment 4466 7 2594 65025 0 1 0 0 0 0 5264 0 0 0 8 #number 4466 5 2594 16711169 0 1 0 0 0 0 5312 0 0 0 8 #return 4466 23 2594 321 0 3 0 0 0 0 5360 0 1401 0 98 40 4544 4752 5328 5280 4656 4704 5248 4928 5008 4800 4624 5376 5136 5040 4832 4960 4576 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 5168 5088 4480 0 5200 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler) 4528 234 256 98 16 8 #folderMidTail 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 55 11 33554471 33554471 2448 5488 0 0 0 0 0 0 8 #folderSub 5506 59 11 0 0 2448 5536 0 0 0 0 0 0 8 #folderTail 5506 57 11 0 0 2448 5568 0 0 0 0 0 0 8 #circle 5506 1 0 0 0 2448 5600 0 0 0 0 0 0 8 #folder 5506 61 5 0 0 2448 5632 0 0 0 0 0 0 8 #folderOpen 5506 63 13 0 0 2448 5664 0 0 0 0 0 0 8 #folderEnd 5506 51 11 33554471 33554471 2448 5696 0 0 0 0 0 0 8 #folderOpenMid 5506 53 11 33554471 33554471 2448 5728 0 0 0 0 0 0 202 208 848 0 63 9215 0 0 0 0 5216 0 0 65 0 0 0 8 '' 7 234 256 98 2 8 #container 234 256 98 2 4528 4466 1 0 0 1 0 0 0 0 4528 0 0 0 0 0 8 #arrows 0 1 0 234 256 98 6 8 'indicator2' 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 5 2448 511 1 32 0 0 0 0 0 0 0 8 'indicator1' 5954 3 2448 33423361 5 32 0 0 0 0 0 0 0 8 'indicator0' 5954 1 2448 65025 3 32 0 0 0 0 0 0 0 0 0 234 256 98 6 8 #Notification 4466 1029 0 2594 31185883 1 0 0 0 0 6080 0 0 0 8 #Error 4466 1031 2594 335 2594 31317503 1 0 0 0 0 6128 0 0 0 8 #Warning 4466 1027 2594 60239 2594 31326207 1 0 0 0 0 6192 0 0 0 0 2002 202 208 98 13 2066 2096 98 2 770 1 51 770 1001 551 2448 2066 8 #contextMenu: 98 1 2640 2448 2066 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval) 3 1 3 2448 2066 8 #isTextModified: 98 1 32 2448 2066 8 #modificationEventMask: 98 1 9215 2448 2066 8 #secondaryCaretForecolor: 98 1 2594 16711423 2448 2066 8 #caretPeriod: 98 1 1061 2448 2066 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin) 1 2448 41 3 32 1 15 6770 3 2448 33 1 16 67108863 15 6770 5 2448 1 1 16 -67108863 15 2448 2066 8 #sciSetIndentationGuides: 98 1 3 2448 2066 8 #tabIndents: 98 1 16 2448 2066 8 #tabWidth: 98 1 9 2448 2066 8 #setLexerLanguage: 98 1 8 #smalltalk 2448 2066 8 #areAdditionalCaretsVisible: 98 1 16 2448 2194 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 244 1 0 0 44 1 0 0] 98 0 2256 0 29 2274 2320 1 2352 1 2384 51 2306 8 #fixedParentBottom 1 234 256 98 2 2448 8 'document' 0 2002 202 208 98 1 2066 2096 98 2 770 5119 21 770 1001 601 416 2194 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 243 11 0 0 54 1 0 0] 98 2 560 2448 2256 0 27 )! !
!MethodSourcePresenter class categoriesFor: #publishedEventsOfInstances!public! !
!MethodSourcePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

