"Filed out from Dolphin Smalltalk 7"!

CodeSourcePresenter subclass: #MethodSourcePresenter
	instanceVariableNames: 'currentSelector gsMethod originalSource'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MethodSourcePresenter guid: (GUID fromString: '{4eda4c47-5b55-436d-b9e2-ef2042660a98}')!
MethodSourcePresenter comment: ''!
!MethodSourcePresenter categoriesForClass!Unclassified! !
!MethodSourcePresenter methodsFor!

canSetBreakpoints

	^true.
!

clearBreakAtStepPoint: anInteger

	gsMethod clearBreakAtStepPoint: anInteger session: gciSession.
	self updateMethodStepPoints.
!

codePresenterIsMethod

	^true!

createSchematicWiring

	super createSchematicWiring.
	documentPresenter
		when: #'hoverStart:'				send: #'hoverStart:'			to: self;
		when: #'hoverEnd:'				send: #'hoverEnd:'			to: self;
		when: #'aboutToDisplayMenu:'		send: #'methodMenu:'		to: self;
		"when: #'leftButtonDoubleClicked:'	send: #'methodDoubleClicked:'	to: self;
		when: #'valueChanged'			send: #'methodValueChanged'	to: self;"
		when: #'focusLost'				send: #'cancelCallTip'			to: documentPresenter view;
		yourself.
!

defaultMethod

^'methodSelector
	"method comment"

	| temps |
	^self yourself.
'.
!

gsMethod: aGsMethod
	gsMethod := aGsMethod.
	self update.!

hoverEnd: anObject

	documentPresenter view cancelCallTip.!

hoverStart: aPoint

	| charIndex indicator |
	charIndex := documentPresenter view charNearestPosition: aPoint.
	indicator := documentPresenter view indicators
		detect: [:each | each range includes: charIndex]
		ifNone: [^self].
	documentPresenter view 
		showCallTip: indicator tag
		at: charIndex.
!

methodDoubleClicked: anObject

	| range string |
	range := documentPresenter view selectionRange.
	string := documentPresenter value.
	string size = range stop ifTrue: [^self].
	(string at: range stop + 1) = $: ifFalse: [^self].
	range stop: range stop + 1.
	documentPresenter view selectionRange: range.
!

methodMenu: aMenu

	| properSize |
	properSize := self editMenuStrings size - 1.
	[
		properSize < aMenu items size.
	] whileTrue: [
		aMenu removeItemAt: aMenu items size.
	].
	self methodMenuBreak: aMenu.
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
	string first isPunctuation ifTrue: [^list first asSymbol].
	list first last = $: ifFalse: [^list first asSymbol].
	stream := WriteStream on: String new.
	index := 1.
	[
		index < list size and: [(list at: index) last = $:].
	] whileTrue: [
		stream nextPutAll: (list at: index).
		index := index + 2.
	].
	^stream contents asSymbol.


!

onViewOpened
	super onViewOpened.
	documentPresenter view contextMenu: (Menu fromStrings: self editMenuStrings).
	self 
		updateCodeFont;
		setDefaultMethod;
		yourself.
!

setBreakAtStepPoint: anInteger

	gsMethod setBreakAtStepPoint: anInteger session: gciSession.
	self updateMethodStepPoints.
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

showStepPoint: anInteger

	| lineNumber range stepPoint |
	stepPoint := gsMethod stepPoints at: anInteger.
	documentPresenter view selectionRange: stepPoint key.
	(range := documentPresenter view selectionRange) isEmpty ifTrue: [^self].
	lineNumber := documentPresenter view lineFromPosition: range first.
	lineNumber := lineNumber - 4 max: 1.
	documentPresenter view lineScroll: lineNumber.
!

stepPointAt: aPoint

	| charIndex stepPoints |
	stepPoints := gsMethod stepPoints.
	charIndex := documentPresenter view charNearestPosition: aPoint.
	stepPoints size to: 1 by: -1 do: [:stepPoint | 
		| range |
		range := (stepPoints at: stepPoint) key.
		(range start <= charIndex and: [charIndex <= range stop]) ifTrue: [
			^stepPoint.
		].
	].
	^nil.
!

update
	| newSource string |
	documentPresenter view cancelCallTip.
	newSource := gsMethod source.
	documentPresenter view isModified
		ifTrue: 
			[newSource = originalSource ifTrue: [^self].
			(MessageBox confirm: 'Copy changes to clipboard?' caption: 'Method has unsaved changes!!')
				ifTrue: 
					[documentPresenter value isString
						ifTrue: [documentPresenter value copyToClipboard	"come in through debugger"]
						ifFalse: [documentPresenter value value copyToClipboard]]].
	self setEmptyMethod.
	gsMethod ifNil: [^self].
	string := ' -- ' , gsMethod category , ' -- '.	"package"	"mcTimestamp"
	self statusBarText: string.
	originalSource := newSource.
	documentPresenter
		value: originalSource;
		isModified: false;
		isReadOnly: gsMethod isReadOnly;
		yourself.
	documentPresenter view
		backcolor: JadeTextStyles default colorForNoEdits;
		isEnabled: true;
		yourself.
	self updateMethodStepPoints.
	currentSelector := gsMethod selector!

updateMethodStepPoints

	| breakPoints indicators stepPoints unimplementedSelectors |
	breakPoints := gsMethod breakPoints.
	stepPoints := gsMethod stepPoints.
	unimplementedSelectors := gsMethod unimplementedSelectors.
	documentPresenter view clearContainerIndicators.
	indicators := OrderedCollection new.
	1 to: stepPoints size do: [:stepPoint |
		| range string styleName |
		range := (stepPoints at: stepPoint) key.
		(unimplementedSelectors at: range start ifAbsent: [nil]) ifNotNil: [:value | 
			styleName := 10.
			string := 'No implementors of #' , value printString , ' (found at step point #' , stepPoint printString , ')'.
		] ifNil: [
			styleName := (breakPoints includes: stepPoint) ifTrue: [9] ifFalse: [8].
			string := ((breakPoints includes: stepPoint) ifTrue: ['Break at '] ifFalse: ['']) , 'step point #' , stepPoint printString.
		].
		indicators add: (ScintillaIndicator
			styleName: styleName 
			range: range 
			tag: string).
	].
	documentPresenter view indicators: indicators.
! !
!MethodSourcePresenter categoriesFor: #canSetBreakpoints!Breakpoints!public! !
!MethodSourcePresenter categoriesFor: #clearBreakAtStepPoint:!public! !
!MethodSourcePresenter categoriesFor: #codePresenterIsMethod!public! !
!MethodSourcePresenter categoriesFor: #createSchematicWiring!Jade!private! !
!MethodSourcePresenter categoriesFor: #defaultMethod!public! !
!MethodSourcePresenter categoriesFor: #gsMethod:!public! !
!MethodSourcePresenter categoriesFor: #hoverEnd:!Jade!private! !
!MethodSourcePresenter categoriesFor: #hoverStart:!Jade!private! !
!MethodSourcePresenter categoriesFor: #methodDoubleClicked:!Jade!private! !
!MethodSourcePresenter categoriesFor: #methodMenu:!Jade!private! !
!MethodSourcePresenter categoriesFor: #methodSource!public! !
!MethodSourcePresenter categoriesFor: #newSelector!public! !
!MethodSourcePresenter categoriesFor: #onViewOpened!public! !
!MethodSourcePresenter categoriesFor: #setBreakAtStepPoint:!public! !
!MethodSourcePresenter categoriesFor: #setDefaultMethod!public! !
!MethodSourcePresenter categoriesFor: #setEmptyMethod!public! !
!MethodSourcePresenter categoriesFor: #showStepPoint:!public! !
!MethodSourcePresenter categoriesFor: #stepPointAt:!Jade!private! !
!MethodSourcePresenter categoriesFor: #update!public! !
!MethodSourcePresenter categoriesFor: #updateMethodStepPoints!public! !

!MethodSourcePresenter class methodsFor!

publishedEventsOfInstances
    
    	^super publishedEventsOfInstances
			add: #'needUser';
			add: #'savedMethod:inClass:';
			yourself.
! !
!MethodSourcePresenter class categoriesFor: #publishedEventsOfInstances!public! !

