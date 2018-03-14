"Filed out from Dolphin Smalltalk 7"!

MethodSourcePresenter subclass: #RowanMethodSourcePresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodSourcePresenter guid: (GUID fromString: '{cfc0ca1c-9636-45a9-bc23-ceca124a04cd}')!
RowanMethodSourcePresenter comment: ''!
!RowanMethodSourcePresenter categoriesForClass!Unclassified! !
!RowanMethodSourcePresenter methodsFor!

fileSave
	"Answer whether the save succeeded (false means to stay on the window and cancel any attempt to leave)"

	| user theClass newSelector category string stream list warnings errors index a b c methodExists line newOop service |
	(theClass := gsMethod behavior) isNil ifTrue: [^true].
	newSelector := self newSelector.
	currentSelector = newSelector
		ifFalse: 
			[methodExists := gciSession
						serverPerform: #class:includesSelector:
						with: theClass
						with: newSelector.
			methodExists
				ifTrue: [(MessageBox confirm: 'Replace method?' caption: 'Method already exists!!') ifFalse: [^self]]].
	user := self trigger: #needUser.
	(category := gsMethod category) isNil ifTrue: [self error: 'We need a method category!!?'].
	self halt. 
	service := gsMethod methodDefinitionService. 
	service source: documentPresenter value replaceCrLfWithLf; 
	className: theClass name;
	user: user; 
	category: category. 
	service packageName: 'foo'.
	string := service compileMethod: theClass user: user.
	stream := ReadStream on: string.
	(line := stream nextLine) notEmpty
		ifTrue: 
			[| pieces |
			pieces := line subStrings: Character tab.
			newOop := (pieces at: 1) asNumber.
			newSelector := pieces at: 2.
			documentPresenter isModified: false.
			self
				trigger: #savedMethod:inClass:
				with: newSelector
				with: theClass].
	parentPresenter updateSaveMethodOop: newOop.
	(list := stream upToEnd subStrings: Character lf) isEmpty ifTrue: [^true].
	warnings := list select: [:each | each beginsWith: 'WARNING:'].
	warnings := warnings collect: [:each | each copyFrom: 10 to: each size].
	warnings notEmpty ifTrue: [ChoicePrompter choices: warnings caption: 'Compile warnings'].
	errors := list select: [:each | each beginsWith: 'ERROR:'].
	errors isEmpty ifTrue: [^true].
	list := errors first subStrings: Character tab.
	string := documentPresenter value replaceCrLfWithLf.
	index := (list at: 3) asNumber - 1.
	a := string copyFrom: 1 to: index.	"replaceLfWithCrLf"
	b := list at: 4.	"replaceLfWithCrLf"
	c := string copyFrom: index + 1 to: string size.	"replaceLfWithCrLf"
	index := a size + 1.
	string := a , b , c.
	documentPresenter value: string.
	documentPresenter view selectionStart: index length: b size.
	^false!

updateMethodBreakPoints

	self rowanFixMe.
	!

updateMethodStepPoints

	self rowanFixMe.
	! !
!RowanMethodSourcePresenter categoriesFor: #fileSave!public! !
!RowanMethodSourcePresenter categoriesFor: #updateMethodBreakPoints!public! !
!RowanMethodSourcePresenter categoriesFor: #updateMethodStepPoints!public! !

