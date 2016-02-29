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
! !
!MethodSourcePresenter class categoriesFor: #publishedEventsOfInstances!public! !

