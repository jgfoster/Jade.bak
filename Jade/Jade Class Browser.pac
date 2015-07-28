| package |
package := Package name: 'Jade Class Browser'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.069'.


package classNames
	add: #CodeSourcePresenter;
	add: #GsClass;
	add: #JadeMigrateClassDialog;
	yourself.

package methodNames
	add: #JadeCodePresenter -> #updateCodeFont;
	add: #JadeServer -> #_addClass:toStream:;
	add: #JadeServer -> #_addClass:toStream:isVisible:fromDictionary:;
	add: #JadeServer -> #addCategory:to:;
	add: #JadeServer -> #addMissingAccessorsFor:;
	add: #JadeServer -> #classOrganizer;
	add: #JadeServer -> #defectiveTestsIn:;
	add: #JadeServer -> #definitionOfClass:;
	add: #JadeServer -> #definitionOfClass:forUser:;
	add: #JadeServer -> #dictionaryForClass:forUser:;
	add: #JadeServer -> #does:replace:;
	add: #JadeServer -> #fileOutForClass:;
	add: #JadeServer -> #nameForSharedPool:forUser:;
	add: #JadeServer -> #postSaveClass:activities:;
	add: #JadeServer -> #removeClass:from:;
	add: #JadeServer -> #removePriorVersionsOf:;
	add: #JadeServer -> #stringForClassList:;
	add: #JadeServer -> #subclassSelectorForClass:;
	add: #JadeServer -> #superclassesOf:isMeta:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Boolean\Dolphin Boolean Presenter';
	add: '..\Object Arts\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Views\Scintilla\Dolphin Scintilla View';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'GemStone Objects';
	add: 'GemStone Session';
	add: 'Jade Autocompletation';
	add: 'Jade Inspector';
	add: 'Jade UI Base';
	yourself).

package!

"Class Definitions"!

GsObject subclass: #GsClass
	instanceVariableNames: 'category parent children isVisible classHistory isTestCase'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter subclass: #CodeSourcePresenter
	instanceVariableNames: 'documentPresenter menuTitle'
	classVariableNames: 'CodeFont'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeValueDialog subclass: #JadeMigrateClassDialog
	instanceVariableNames: 'copyMethodsPresenter recompileSubclassesPresenter migrateInstancesPresenter removeFromClassHistoryPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JadeCodePresenter methodsFor!

updateCodeFont

	CodeSourcePresenter codeFont notNil ifTrue: [
		documentPresenter view font: CodeSourcePresenter codeFont.
	].
! !
!JadeCodePresenter categoriesFor: #updateCodeFont!public! !

!JadeServer methodsFor!

_addClass: each toStream: stream 

	self
		_addClass: each 
		toStream: stream 
		isVisible: true
		fromDictionary: nil.
!

_addClass: aClass toStream: aStream isVisible: aBoolean fromDictionary: aDictionary
	"1. OOP; 2. key; 3. category; 4. superclass OOP; 5. children; 6. Visible/Inherited; 7. Class History; 8. isTestCase"

	| testCaseClass history |
	(self oopOf: aClass) printOn: aStream.
	aStream tab; nextPutAll: (aDictionary  isNil ifTrue: [aClass name] ifFalse: [aDictionary keyAtValue: aClass ifAbsent: [aClass name]]); tab.
	aClass category notNil ifTrue: [aStream nextPutAll: aClass category].
	aStream tab. (self oopOf: aClass superclass) printOn: aStream.
	aStream 
		tab; "let client build children list"
		tab; 
		nextPut: (aBoolean ifTrue: [$V] ifFalse: [$I]);
		tab.
	(history := self historyOf: aClass) isNil ifTrue: [history := Array with: aClass].
	(history indexOf: aClass) printOn: aStream.
	aStream nextPut: $/.
	history size printOn: aStream.
	aStream tab.
	testCaseClass := Globals
		at: #'TestCase'
		ifAbsent: [nil].
	(testCaseClass notNil and: [aClass isSubclassOf: testCaseClass]) printOn: aStream.
	aStream lf.
!

addCategory: aString to: aClass 

	aClass addCategory: aString.
!

addMissingAccessorsFor: aClass

	aClass compileMissingAccessingMethods.
!

classOrganizer

	classOrganizer isNil ifTrue: [classOrganizer := ClassOrganizer new].
	^classOrganizer.
!

defectiveTestsIn: aClass

	| testClass results stream |
	testClass := aClass thisClass.
	results := testClass suite run.
	stream := WriteStream on: String new.
	stream nextPutAll: results printString; lf.
	results defects asSet do: [:each | 
		| selector class |
		selector := each selector asSymbol.
		class := each class whichClassIncludesSelector: selector.
		stream nextPutAll: class name , ' debug: ' , each selector printString; lf.
	].
	^stream contents.
!

definitionOfClass: aClass

	^aClass definition.
!

definitionOfClass: aClass forUser: aUserProfile

			| stream |
			stream := WriteStream on: String new.
"1"		aClass superclass printOn: stream.
			stream 
"2"			lf; nextPutAll: (self subclassSelectorForClass: aClass);
"3"			lf; nextPutAll: aClass name;
"4"			lf; nextPutAll: (self dictionaryForClass: aClass forUser: aUserProfile);
				yourself.
"5"		stream lf. aClass instancesInvariant printOn: stream.
"6"		stream lf. aClass isModifiable printOn: stream.
"7"		stream lf. ((aClass class canUnderstand: #'instancesDbTransient') and: [aClass instancesDbTransient]) printOn: stream.
"8"		stream lf. ((aClass class canUnderstand: #'instancesNonPersistent') and: [aClass instancesNonPersistent]) printOn: stream.
			stream lf.
		aClass instVarNames do: [:each |
				stream 
"9.*.1"		nextPutAll: each; 
					space;
"9.*.2"		nextPutAll: (aClass constraintOfInstVar: each) name;
					tab.
			].
			stream lf.
			aClass class instVarNames do: [:each | 
"10.*"		stream nextPutAll: each; tab.
			].
			stream lf.
			aClass classVarNames asSortedCollection do: [:each | 
"11.*"		stream nextPutAll: each; tab.
			].
			stream lf.
			aClass sharedPools asSortedCollection do: [:each | 
"12.*"		stream nextPutAll: (self nameForSharedPool: each forUser: aUserProfile); tab.
			].
			^stream 
"13"		lf; nextPutAll: aClass userId;
"14"		lf; nextPutAll: (aClass timeStamp asStringUsingFormat: #(3 2 1 $- 1 1 $: true true false));
				lf; 
				contents.
!

dictionaryForClass: aClass forUser: aUserProfile

	| anArray |
	anArray := self dictionaryAndSymbolOf: aClass forUser: aUserProfile.
	anArray isNil ifTrue: [^''].
	anArray := self dictionaryAndSymbolOf: (anArray at: 1) forUser: aUserProfile.
	anArray isNil ifTrue: [^''].
	^(anArray at: 2)
!

does: newClass replace: oldClass

	^newClass name = oldClass name.
!

fileOutForClass: aClass

	^aClass thisClass fileOutClass.
 !

nameForSharedPool: anObject forUser: aUserProfile

	| anArray dict sharedPoolClass |
	anArray := self dictionaryAndSymbolOf: anObject forUser: aUserProfile.
	anArray notNil ifTrue: [^anArray at: 2].
	(dict := aUserProfile objectNamed: anObject name) isNil ifTrue: [^'???'].
	(sharedPoolClass := self objectNamed: 'SharedPool') isNil ifTrue: [^'???'].
	((dict isKindOf: Class) and: [dict isSubclassOf: sharedPoolClass]) ifTrue: [^anObject name , ' _classVars'].
	^'???'.
!

postSaveClass: aGsClass activities: aString 

	| gsClass copyMethods migrateInstances recompileSubclasses removeFromClassHistory symbolList list index key oldClass newClass oldNewList stream |
	gsClass := (self historyOf: aGsClass) last.
	list := aString subStrings: Character tab.
	list := list collect: [:each | each = 'true'].
	symbolList := self symbolList.
	copyMethods := list at: 1.
	recompileSubclasses := list at: 2.
	migrateInstances := list at: 3.
	removeFromClassHistory := list at: 4.
	oldNewList := OrderedCollection new.
	stream := WriteStream on: String new.
	oldClass := (self historyOf: gsClass) asArray reverse at: 2.
	oldNewList add: oldClass -> gsClass.
	recompileSubclasses ifTrue: [
		(self classOrganizer allSubclassesOf: oldClass) do: [:each | 
			gsClass := GsSession currentSession execute: each definition.
			oldNewList add: each -> gsClass.
		].
	].
	copyMethods ifTrue: [
		oldNewList do: [:eachAssoc | 
			oldClass := eachAssoc key.
			newClass := eachAssoc value.
			index := symbolList findFirst: [:eachDict | eachDict includes: newClass].
			index = 0 ifTrue: [self error: 'Where did the class go?'].
			key := (symbolList at: index) keyAtValue: newClass.
			list := newClass copyMethodsFrom: oldClass dictionaries: symbolList.
			list do: [:eachMethod | 
				stream
					nextPutAll: 'method'; tab;
					nextPutAll: index printString; tab;
					nextPutAll: key; tab;
					nextPutAll: eachMethod selector; lf;
					yourself]]].
	migrateInstances ifTrue: [
		System commitTransaction ifFalse: [self error: 'commit failed!!'].
		oldNewList do: [:eachAssoc | 
			oldClass := eachAssoc key.
			newClass := eachAssoc value.
			list := oldClass migrateInstancesTo: newClass.
			list do: [:each | 
				each notEmpty ifTrue: [
					stream
						nextPutAll: 'migrate'; tab;
						nextPutAll: newClass name; tab;
						nextPutAll: each size printString; lf;
						yourself.
				].
			].
		].
	].
	removeFromClassHistory ifTrue: [
		oldNewList do: [:eachAssoc | 
			newClass := eachAssoc value.
			((self historyOf: newClass) asArray copyFrom: 1 to: (self historyOf: newClass) size - 1) do: [:each | 
				(self historyOf: newClass) removeVersion: each.
			].
		].
	].
	^stream contents.
!

removeClass: aClass from: aDictionary

	| key |
	key := aDictionary
		keyAtValue: aClass
		ifAbsent: [^false].
	aDictionary removeKey: key.
	^true.
!

removePriorVersionsOf: aClass

	[
		1 < (self historyOf: aClass) size.
	] whileTrue: [
		(self historyOf: aClass) removeVersion: (self historyOf: aClass) first.
	].
!

stringForClassList: aList

	| stream |
	stream := WriteStream on: String new.
	aList do: [:each | 
		self 
			_addClass: each 
			toStream: stream.
	].
	^stream contents.
!

subclassSelectorForClass: aClass

	(aClass isBytes and: [aClass superclass notNil and: [aClass superclass isBytes not]]) ifTrue: [
		^'byteSubclass:'.
	].
	(aClass isIndexable and: [aClass superclass notNil and: [aClass superclass isIndexable not]]) ifTrue: [
		^'indexableSubclass:'.
	].
	((aClass class canUnderstand: #'isTransientDB') and: [aClass isTransientDB]) ifTrue: [
		^'transientSubclass:'.
	].
	^'subclass:'.
!

superclassesOf: aClass isMeta: aBoolean

	| myClass list |
	myClass := aBoolean ifTrue: [aClass class] ifFalse: [aClass].
	list := myClass _allSuperList , (Array with: myClass).
	^self stringForClassList: list.
! !
!JadeServer categoriesFor: #_addClass:toStream:!Classes!public! !
!JadeServer categoriesFor: #_addClass:toStream:isVisible:fromDictionary:!Classes!public! !
!JadeServer categoriesFor: #addCategory:to:!Classes!public! !
!JadeServer categoriesFor: #addMissingAccessorsFor:!Classes!public! !
!JadeServer categoriesFor: #classOrganizer!Classes!public! !
!JadeServer categoriesFor: #defectiveTestsIn:!Classes!public! !
!JadeServer categoriesFor: #definitionOfClass:!Classes!public! !
!JadeServer categoriesFor: #definitionOfClass:forUser:!Classes!public! !
!JadeServer categoriesFor: #dictionaryForClass:forUser:!Classes!public! !
!JadeServer categoriesFor: #does:replace:!Classes!public! !
!JadeServer categoriesFor: #fileOutForClass:!Classes!public! !
!JadeServer categoriesFor: #nameForSharedPool:forUser:!Classes!public! !
!JadeServer categoriesFor: #postSaveClass:activities:!Classes!public! !
!JadeServer categoriesFor: #removeClass:from:!Classes!public! !
!JadeServer categoriesFor: #removePriorVersionsOf:!Classes!public! !
!JadeServer categoriesFor: #stringForClassList:!Classes!public! !
!JadeServer categoriesFor: #subclassSelectorForClass:!Classes!public! !
!JadeServer categoriesFor: #superclassesOf:isMeta:!Classes!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

GsClass guid: (GUID fromString: '{B3021ACC-6EB1-40F5-93E7-6353E8DB8A14}')!
GsClass comment: ''!
!GsClass categoriesForClass!Unclassified! !
!GsClass methodsFor!

addChild: aGsClass2

	children add: aGsClass2.
!

addToDictionary: aDictionary

	isVisible ifFalse: [
		| other |
		other := aDictionary
			at: oopType value
			ifAbsent: [nil].
		isVisible := other notNil and: [other isVisible].
	].
	aDictionary
		at: oopType value
		put: self.
!

category

	^category.
!

children
	^children!

classHistory
	^classHistory!

implementorsOf: aGsMethod

	| string |
	string := gciSession
		serverPerform: #'implementorsOf:startingAt:'
		with: aGsMethod
		with: self.
	^self class
		listFromString: string 
		session: gciSession.
!

initialize: aList
	"See JadeServer>>#_addClass: aClass toStream: aStream isVisible: aBoolean fromDictionary: aDictionary
	1. OOP; 2. key; 3. category; 4. superclass OOP; 5. children; 6. Visible/Inherited; 7. Class History; 8. isTestCase"

	super initialize: aList.
	category := aList at: 3.
	aList size < 4 ifTrue: [^self].	"When we get a Class as the holder of a GsMethod, then we don't get all the details"
	parent := gciSession oopTypeWithOop: (aList at: 4) asNumber.
	children := SortedCollection sortBlock: [:a :b | a name <= b name].
	isVisible := (aList at: 6) = 'V'.
	classHistory := aList at: 7.
	classHistory := classHistory = '1/1' 
	  ifTrue: ['']
	  ifFalse: [' [' , classHistory , ']'].
	isTestCase := (aList at: 8) = 'true'.
!

isTestCase

	^isTestCase.
!

isVisible

	^isVisible.
!

nameWithVersion

	^name , classHistory.
!

parent

	^parent.
!

superclassListForMeta: aBoolean

	| string |
	string := gciSession
		serverPerform: #'superclassesOf:isMeta:'
		with: self
		with: aBoolean.
	^self class
		listFromString: string 
		session: gciSession.
!

updateFrom: aGsClass2

	isVisible := isVisible or: [aGsClass2 isVisible].
!

updateParentInDictionary: aDictionary

	parent := aDictionary
		at: parent value
		ifAbsent: [nil].
	parent notNil ifTrue: [
		parent addChild: self.
	].
! !
!GsClass categoriesFor: #addChild:!public! !
!GsClass categoriesFor: #addToDictionary:!public! !
!GsClass categoriesFor: #category!public! !
!GsClass categoriesFor: #children!accessing!public! !
!GsClass categoriesFor: #classHistory!accessing!public! !
!GsClass categoriesFor: #implementorsOf:!public! !
!GsClass categoriesFor: #initialize:!public! !
!GsClass categoriesFor: #isTestCase!public! !
!GsClass categoriesFor: #isVisible!public! !
!GsClass categoriesFor: #nameWithVersion!public! !
!GsClass categoriesFor: #parent!accessing!public! !
!GsClass categoriesFor: #superclassListForMeta:!public! !
!GsClass categoriesFor: #updateFrom:!public! !
!GsClass categoriesFor: #updateParentInDictionary:!public! !

CodeSourcePresenter guid: (GUID fromString: '{549A5009-CDD1-42B3-8907-C3C5C1C9E532}')!
CodeSourcePresenter comment: ''!
!CodeSourcePresenter categoriesForClass!Unclassified! !
!CodeSourcePresenter methodsFor!

addBreakpointAtLine: anInteger

	| stepPoints stepPoint |
	stepPoints := self stepPointsForLine: anInteger.
	stepPoints isEmpty ifTrue: [
		documentPresenter view selectLine: anInteger.
		^self.
	].
	stepPoint := stepPoints asSortedCollection first.
	self setBreakAtStepPoint: stepPoint.
!

addMarkerAtLine: anInteger

	(self markerAtLine: anInteger) isNil ifTrue: [
		documentPresenter view
			addMarkerType: #'breakpoint' 
			at: anInteger.
	].
!

addMenu

	| menuBar |
	menuBar := self view topShell view menuBar.
	self updateMenuBar: menuBar.
	self view topShell view menuBar: menuBar.
!

addMenuTo: aMenuBar

	self updateMenuBar: aMenuBar.
!

addQuotesToSelection

	documentPresenter view replaceSelection: documentPresenter selection printString.
!

browseImplementors

	self browseImplementorsOf: documentPresenter view selection.!

browseSenders

	self browseSendersOf: documentPresenter view selection.!

canSetBreakpoints

	^false.
!

clearBreakAtStepPoint: anInteger

	| method |
	(method := self trigger: #'needMethod') isNil ifTrue: [^self].
	method clearBreakAtStepPoint: anInteger.
!

codeFont: aFont

	documentPresenter view font: aFont.
!

codePresenterIsMethod

	^false!

createComponents

	super createComponents.
	documentPresenter := self add: JadeAutoTextPresenter new name: 'document'.
	self updateCodeFont.
!

createSchematicWiring

	super createSchematicWiring.
	self view topShell 
		when: #'closeRequested:' 
		send: #'selectionChanging:' 
		to: self.
	documentPresenter
		when: #'marginClicked:'
		send: #'marginClicked:'
		to: self.
!

currentSelectionOrLine

	| range |
	documentPresenter hasSelection ifFalse: [documentPresenter view selectCurrentLine].
	range := documentPresenter view selectionRange.
	^(documentPresenter value copyFrom: range start to: range stop) replaceCrLfWithLf.
!

documentPresenter

	^documentPresenter!

editCopy

	documentPresenter view
		copySelection;
		updateModel;
		yourself.
!

editCut

	documentPresenter view
		cutSelection;
		updateModel;
		yourself.
!

editDelete

	self selectLfIfEndingOnCr.
	documentPresenter view
		clearSelection;
		updateModel;
		yourself.
!

editFind
	"I'm not sure how it works, but this method isn't called!! 
	Somehow, the command is sent directly to the text widget."

self error: 'Do we ever see this?'.
	"self activeTextEdit editFind."
!

editFindNext

	documentPresenter view findNext.
	self showSelection.

!

editPaste

	documentPresenter view
		pasteClipboard;
		updateModel;
		yourself.
!

editReplace

	documentPresenter view
		findReplace;
		updateModel;
		yourself.
!

editSelectAll

	documentPresenter view selectAll.
!

executeSelectionOrLine

	[
		^true -> (self model executeString: self currentSelectionOrLine fromContext: nil).
	] on: GsCompileError do: [:ex | 
		^false -> ex list.
	].
	self error: 'How did we get here?'.
	^false -> #(nil).
!

fileSave
		"Answer whether the save succeeded (false means to stay on the window and cancel any attempt to leave)"

	self subclassResponsibility.
!

hasBreakpointOnLine: anInteger

	^(self markerAtLine: anInteger) notNil.
!

jadeDisplay

	self jadeExecuteAndDisplay: true.
!

jadeExecute

	self jadeExecuteAndDisplay: false.
!

jadeExecuteAndDisplay: showResult 

	| result |
	result := self executeSelectionOrLine.
	result key ifTrue: [
		showResult ifTrue: [
			self showResult: result value.
		] ifFalse: [
			self setCaretToEndOfSelection.
		].
		^result value.
	] ifFalse: [
		self showCompileError: result value first.
	].
!

jadeInspect

	| result |
	result := self jadeExecuteAndDisplay: false.
	JadeInspector showOn: self gciSession -> result.
!

marginClicked: anSCNotification

	| scintilla line |
	scintilla := documentPresenter view.
	line := scintilla lineFromPosition: anSCNotification position.
	self canSetBreakpoints ifFalse: [
		scintilla selectLine: line.
		^self.
	].
	(self hasBreakpointOnLine: line) ifTrue: [
		self removeBreakpointsOnLine: line.
	] ifFalse: [
		self addBreakpointAtLine: line.
	].
!

markerAtLine: anInteger

	^documentPresenter view markers 
		detect: [:each | each line = anInteger]
		ifNone: [nil].
!

menuTitle: aString

	menuTitle := aString.
!

queryCommand: query

	(#(#'fileSave') includes: query commandSymbol) ifTrue: [
		query isEnabled: documentPresenter isModified.
		^true.
	].
	(#(#editCut #editCopy) includes: query commandSymbol) ifTrue: [
		query isEnabled: documentPresenter hasSelection.
		^true.
	].
	(query commandSymbol = #editPaste) ifTrue: [
		query isEnabled: documentPresenter view canPaste.
		^true.
	].
	^super queryCommand: query.
!

removeBreakpointsOnLine: anInteger

	(self stepPointsForLine: anInteger) do: [:each | 
		self clearBreakAtStepPoint: each abs.
	].
	self trigger: #'breaksChanged'.
!

removeMenu

	| menuBar item |
	(menuBar := self view topShell view menuBar) isNil ifTrue: [^self].
	item := menuBar items 
		detect: [:each | each text = self subMenuName]
		ifNone: [^self].
	menuBar removeItem: item.
	self view topShell view menuBar: menuBar.
!

removeQuotesFromSelection

	| string |
	string := documentPresenter view selection trimBlanks.
	(string size >= 2 and: [string first = $' and: [string last = $']]) ifFalse: [
		^MessageBox notify: 'Selection must begin and end with quote'.
	].
	string := string copyFrom: 2 to: string size - 1.
	string := string 
		copyReplaceAll: ''''''
		with: ''''.
	documentPresenter view replaceSelection: string.
!

selectionChanging: aSelectionChangingEvent 

	(documentPresenter view isKindOf: DeafObject) ifTrue: [^self].
	documentPresenter view isModified ifFalse: [^self].
	documentPresenter view ensureVisible.
	MessageBox 
		confirm: 'Save changes?' 
		onYes: 			[aSelectionChangingEvent value: self fileSave] 
		onNo: 			[documentPresenter view isModified: false] 
		onCancel: 	[aSelectionChangingEvent value: false].
!

selectLfIfEndingOnCr
	"deleting a CR without the subsequent LF can leave things somewhat confused"

	| text selectionRange |
	selectionRange := documentPresenter view selectionRange.
	text := documentPresenter view "hide; show;" value.			"somehow the value gets out of synch"
	selectionRange stop < selectionRange start 						ifFalse: [^self ].
	selectionRange start < documentPresenter view value size 	ifFalse: [^self ].
	(text at: selectionRange start) = Character cr 						ifFalse: [^self ].
	(text at: selectionRange start + 1) = Character lf 				ifFalse: [^self ].
	documentPresenter view selectionRange: (selectionRange start to: selectionRange start + 1).
!

setBreakAtStepPoint: anInteger

	| method |
	(method := self trigger: #'needMethod') isNil ifTrue: [^self].
	method setBreakAtStepPoint: anInteger.
	self trigger: #'breaksChanged'.
!

setBreakpointOnLine: lineInteger atStepPoint: stepPointInteger

	self setBreakAtStepPoint: stepPointInteger.
!

setCaretToEndOfSelection

	| textView |
	textView := documentPresenter view.
	textView caretPosition: textView selectionRange stop + 1.
!

setFont

	| font |
	font := CodeFont notNil
		ifTrue: [FontDialog showModalOn: CodeFont]
		ifFalse: [FontDialog showModal].
	font notNil ifTrue: [
		self class codeFont: font.
	]
!

showCompileError: anArray

	| result string count textView selectionRange offset |
	textView := documentPresenter view.
	offset := anArray at: 2.
	result := anArray at: 3.
	selectionRange := textView selectionRange.
	string := textView value.
	string := string copyFrom: selectionRange start to: selectionRange stop.
	string := string replaceCrLfWithLf copyFrom: 1 to: offset - 1.
	count := (string select: [:each | each = Character lf]) size.
	offset := offset + count.
	textView
		caretPosition: selectionRange start + offset - 1;
		replaceSelection: result;
		selectionStart: textView caretPosition - result size length: result size.
!

showResult: anObject

	| result textView |
	(self gciSession isOopType: anObject) ifFalse: [
		result := ' ' , anObject printString.
	] ifTrue: [
		result := ' ' , (self model printString: anObject).
	].
	"result := result replaceLfWithCrLf."
	self setCaretToEndOfSelection.
	(textView := documentPresenter view)
		replaceSelection: result;
		selectionStart: textView caretPosition - result size length: result size.
!

showSelection

	| range lineNumber |
	(range := documentPresenter view selectionRange) isEmpty ifTrue: [^self].
	lineNumber := documentPresenter view lineFromPosition: range first.
	lineNumber := lineNumber - 4 max: 1.
	documentPresenter view lineScroll: lineNumber.
!

stepPointsForLine: anInteger

	| methodSource lineBegin lineEnd stepPointOffsets assoc |
	methodSource := self trigger: #'needMethodSource'.
	lineBegin := 0.
	anInteger - 1 timesRepeat: [
		lineBegin := methodSource nextIndexOf: Character lf from: lineBegin + 1 to: methodSource size.
	].
	lineEnd := methodSource nextIndexOf: Character lf from: lineBegin + 1 to: methodSource size.
	0 = lineEnd ifTrue: [lineEnd := methodSource size + 1].
	stepPointOffsets := self trigger: #'needStepPointOffsets'.
	stepPointOffsets := stepPointOffsets select: [:each | lineBegin <= each key and: [each key <= lineEnd]].
	^stepPointOffsets collect: [:each | each value].
!

subMenuName

	^menuTitle.
!

subMenuPresenter

	^documentPresenter.
!

update

	self subclassResponsibility.
!

updateCodeFont

	CodeFont notNil ifTrue: [
		documentPresenter view font: CodeFont.
	].
! !
!CodeSourcePresenter categoriesFor: #addBreakpointAtLine:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #addMarkerAtLine:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #addMenu!menus!public! !
!CodeSourcePresenter categoriesFor: #addMenuTo:!menus!public! !
!CodeSourcePresenter categoriesFor: #addQuotesToSelection!edit!private! !
!CodeSourcePresenter categoriesFor: #browseImplementors!public! !
!CodeSourcePresenter categoriesFor: #browseSenders!public! !
!CodeSourcePresenter categoriesFor: #canSetBreakpoints!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #clearBreakAtStepPoint:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #codeFont:!public! !
!CodeSourcePresenter categoriesFor: #codePresenterIsMethod!public! !
!CodeSourcePresenter categoriesFor: #createComponents!public! !
!CodeSourcePresenter categoriesFor: #createSchematicWiring!public! !
!CodeSourcePresenter categoriesFor: #currentSelectionOrLine!Jade!private! !
!CodeSourcePresenter categoriesFor: #documentPresenter!public! !
!CodeSourcePresenter categoriesFor: #editCopy!edit!private! !
!CodeSourcePresenter categoriesFor: #editCut!edit!private! !
!CodeSourcePresenter categoriesFor: #editDelete!edit!private! !
!CodeSourcePresenter categoriesFor: #editFind!edit!private! !
!CodeSourcePresenter categoriesFor: #editFindNext!edit!private! !
!CodeSourcePresenter categoriesFor: #editPaste!edit!private! !
!CodeSourcePresenter categoriesFor: #editReplace!edit!private! !
!CodeSourcePresenter categoriesFor: #editSelectAll!edit!private! !
!CodeSourcePresenter categoriesFor: #executeSelectionOrLine!Jade!private! !
!CodeSourcePresenter categoriesFor: #fileSave!public! !
!CodeSourcePresenter categoriesFor: #hasBreakpointOnLine:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #jadeDisplay!Jade!private! !
!CodeSourcePresenter categoriesFor: #jadeExecute!Jade!private! !
!CodeSourcePresenter categoriesFor: #jadeExecuteAndDisplay:!Jade!private! !
!CodeSourcePresenter categoriesFor: #jadeInspect!Jade!private! !
!CodeSourcePresenter categoriesFor: #marginClicked:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #markerAtLine:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #menuTitle:!menus!public! !
!CodeSourcePresenter categoriesFor: #queryCommand:!public! !
!CodeSourcePresenter categoriesFor: #removeBreakpointsOnLine:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #removeMenu!menus!public! !
!CodeSourcePresenter categoriesFor: #removeQuotesFromSelection!edit!private! !
!CodeSourcePresenter categoriesFor: #selectionChanging:!public! !
!CodeSourcePresenter categoriesFor: #selectLfIfEndingOnCr!edit!private! !
!CodeSourcePresenter categoriesFor: #setBreakAtStepPoint:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #setBreakpointOnLine:atStepPoint:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #setCaretToEndOfSelection!Jade!private! !
!CodeSourcePresenter categoriesFor: #setFont!private! !
!CodeSourcePresenter categoriesFor: #showCompileError:!Jade!private! !
!CodeSourcePresenter categoriesFor: #showResult:!Jade!private! !
!CodeSourcePresenter categoriesFor: #showSelection!edit!private! !
!CodeSourcePresenter categoriesFor: #stepPointsForLine:!Breakpoints!public! !
!CodeSourcePresenter categoriesFor: #subMenuName!menus!public! !
!CodeSourcePresenter categoriesFor: #subMenuPresenter!menus!public! !
!CodeSourcePresenter categoriesFor: #update!public! !
!CodeSourcePresenter categoriesFor: #updateCodeFont!public! !

!CodeSourcePresenter class methodsFor!

codeFont

	^CodeFont.
!

codeFont: aFont

	CodeFont := aFont.
	self withAllSubclassesDo: [:eachClass | 
		eachClass allInstances do: [:each | 
			each codeFont: aFont.
		].
	].
	JadeTextDocument withAllSubclassesDo: [:eachClass | 
		eachClass allInstances do: [:each | 
			each updateCodeFont.
		].
	].
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1140851500 131137 560 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 517 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point)  193 193 0 560 642 672 8 4294902325 234 256 98 0 234 256 98 24 20127 1246982 ##(Smalltalk.ToolbarSystemButton)  20127 0 560 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #editCopy 8 'Copy' 1 1 0 1 3 20129 898 20129 0 560 1 930 8 #editPaste 8 'Paste' 1 1 0 1 5 20131 898 20131 0 560 1 930 8 #editDelete 8 'Delete' 1 1 0 1 11 20133 898 20133 0 560 1 930 8 #undo 8 'Undo' 1 1 0 1 7 20135 898 20135 0 560 1 930 8 #redo 8 'Redo' 1 1 0 1 9 20137 898 20137 0 560 1 930 8 #editFind 8 'Find' 1 1 0 1 25 20139 898 20139 0 560 1 930 8 #editReplace 8 'Replace' 1 1 0 1 27 20141 853766 ##(Smalltalk.ToolbarButton)  20141 0 560 1 930 8 #jadeDisplay 8 'Print Result of Selection or Line' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 770 1857 33 55 20143 1378 20143 0 560 1 930 8 #jadeExecute 8 'Evaluate Selection or Line' 1 1 0 1472 57 20145 1378 20145 0 560 1 930 8 #jadeInspect 8 'Inspect Selection or Line' 1 1 0 1472 59 20123 898 20123 0 560 1 930 8 #fileSave 8 'Save' 1 1 0 1 17 20125 898 20125 0 560 1 930 8 #editCut 8 'Cut' 1 1 0 1 1 98 15 1728 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 560 3 0 1 1792 912 992 1056 1120 1184 1874 0 0 560 3 0 1 1248 1312 1874 0 0 560 3 0 1 1392 1600 1664 234 240 98 4 1 1 1472 31 0 1 0 770 33 33 770 45 45 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 770 1 1 770 1001 51 560 2066 8 #updateSize 848 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 244 1 0 0 25 0 0 0] 98 0 770 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 2306 8 #fixedParentRight 1 2306 8 #fixedParentTop 1 2306 8 #fixedViewTop 51 410 8 ##(Smalltalk.ScintillaView)  98 46 0 416 98 2 8 1176571972 1025 2448 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 196934 1 ##(Smalltalk.RGB)  27387381 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 6 2626 0 16 98 7 984134 2 ##(Smalltalk.CommandMenuItem)  1 930 8 #browseImplementors 8 'Browse &Implementors' 1 1 0 0 0 2706 1 930 8 #browseSenders 8 'Browse &Senders' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 2706 1 930 8 #browseReferences 8 'Browse &References' 1 1 0 0 0 2706 1 930 8 #browseClass 8 'Browse &Class' 1 1 0 0 0 2850 4097 2706 1 930 8 #browseMethodsWithString 8 'Browse &Methods with String' 1 1 0 0 0 8 '&Browse' 0 134217729 0 0 0 0 0 2626 0 16 98 12 2706 1 930 1760 8 '&Save' 9383 1 0 0 0 2850 4097 2706 1 930 1152 8 '&Undo' 9397 1 0 0 0 2706 1 930 1216 8 'Redo' 9395 1 0 0 0 2850 4097 2706 1 930 1824 8 'Cu&t' 9393 1 0 0 0 2706 1 930 960 8 '&Copy' 9351 1 0 0 0 2706 1 930 1024 8 '&Paste' 9389 1 0 0 0 2706 1 930 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 2706 1 930 1088 8 'De&lete' 1629 1 0 0 0 2850 4097 2706 1 930 8 #setFont 8 'Font...' 1025 1 0 0 0 8 '&Edit' 0 134217729 0 0 0 0 0 2626 0 16 98 6 2706 1 930 1424 8 '&Display' 9353 1 0 0 0 2706 1 930 1632 8 '&Execute' 9355 1 0 0 0 2706 1 930 1696 8 '&Inspect' 9363 1 0 0 0 2706 1 930 1696 8 '&Query' 9379 1 0 0 0 2850 4097 2706 1 930 8 #fileIn 8 'File In' 1 1 0 0 0 8 'E&xecute' 0 134217729 0 0 0 0 0 2626 0 16 98 3 2706 1 930 1280 8 '&Find...' 9357 1 0 0 0 2706 1 930 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 2706 1 930 1344 8 '&Replace...' 9361 1 0 0 0 8 '&Find' 0 134217729 0 0 0 0 0 2626 0 16 98 2 2706 1 930 8 #addQuotesToSelection 8 '&Add' 1 1 0 0 0 2706 1 930 8 #removeQuotesFromSelection 8 '&Remove' 1 1 0 0 0 8 '&Quotes' 0 134217729 0 0 0 0 0 2706 1 930 8 #setBreakpoint 8 'Set Brea&kpoint' 1025 1 0 0 0 8 '' 0 1 0 0 0 0 0 690 0 16 722 8 #[244 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 770 193 193 0 2448 0 8 4294902235 852486 ##(Smalltalk.NullConverter)  0 0 9 0 234 256 98 42 8 #lineNumber 1182726 ##(Smalltalk.ScintillaTextStyle)  67 0 0 1 0 0 0 0 4592 0 0 0 8 #specialSelector 4610 33 2594 16646145 0 3 0 0 0 0 4640 0 0 0 8 #global 4610 21 0 0 3 0 0 0 0 4688 0 0 0 8 #normal 4610 1 0 0 1 0 0 0 0 4720 0 0 0 8 #boolean 4610 13 4672 0 3 0 0 0 0 4752 0 0 0 8 #special 4610 25 0 0 3 0 0 0 0 4784 0 0 0 8 #number 4610 5 2594 16711169 0 1 0 0 0 0 4816 0 0 0 8 #nil 4610 19 4672 0 3 0 0 0 0 4864 0 0 0 8 #character 4610 31 2594 16646399 0 3 0 0 0 0 4896 0 0 0 8 #braceHighlight 4610 69 786694 ##(Smalltalk.IndexedColor)  33554465 0 3 0 0 0 0 4944 0 0 0 8 #indentGuide 4610 75 4978 33554447 0 1 0 0 0 0 5008 0 0 0 8 #string 4610 3 2594 16646399 0 129 0 0 0 0 5056 0 0 0 8 #symbol 4610 9 4978 33554443 0 1 0 0 0 0 5104 0 0 0 8 #super 4610 17 4672 0 3 0 0 0 0 5152 0 0 0 8 #comment 4610 7 2594 65025 0 1 0 0 0 0 5184 0 0 0 8 #binary 4610 11 4978 33554433 0 1 0 0 0 0 5232 0 0 0 8 #assignment 4610 29 0 0 3 0 0 0 0 5280 0 0 0 8 #keywordSend 4610 27 4978 33554437 0 3 0 0 0 0 5312 0 0 0 8 #return 4610 23 2594 321 0 3 0 0 0 0 5360 0 0 0 8 #braceMismatch 4610 71 4978 33554459 0 3 0 0 0 0 5408 0 0 0 8 #self 4610 15 4672 0 3 0 0 0 0 5456 0 0 0 98 40 4736 5072 4832 5200 5120 5248 4768 5472 5168 4880 4704 5376 4800 5328 5296 4912 4656 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4624 4960 5424 0 5024 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  4720 234 256 98 16 8 #folderTail 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  57 11 5264 5264 2448 5568 8 #breakpoint 5586 1 1 4978 33554433 4978 33554459 2448 5616 8 #folderOpenMid 5586 53 11 4978 33554471 5264 2448 5680 8 #folderSub 5586 59 11 5264 5264 2448 5728 8 #folderMidTail 5586 55 11 5712 5264 2448 5760 8 #folder 5586 61 5 5264 5264 2448 5792 8 #folderOpen 5586 63 13 5264 5264 2448 5824 8 #folderEnd 5586 51 11 5712 5264 2448 5856 202 208 848 0 63 9215 0 0 0 0 5040 0 0 0 0 0 0 8 '' 7 234 256 98 2 8 #container 234 256 98 2 4720 4610 1 0 0 1 0 0 0 0 4720 0 0 0 0 0 8 #arrows 0 1 0 234 256 98 6 8 'indicator1' 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  3 2448 33423361 5 32 0 0 8 'indicator0' 6082 1 2448 65025 3 32 0 0 8 'indicator2' 6082 5 2448 511 1 32 0 0 2002 202 208 98 11 2066 2096 98 2 770 1 51 770 1001 551 2448 2066 8 #contextMenu: 98 1 2640 2448 2066 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 2448 2066 8 #isTextModified: 98 1 32 2448 2066 8 #modificationEventMask: 98 1 9215 2448 2066 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 2448 41 3 32 1 6578 3 2448 1 1 16 67108863 6578 5 2448 33 1 16 67108863 2448 2066 8 #indentationGuides: 98 1 8 #real 2448 2066 8 #tabIndents: 98 1 16 2448 2066 8 #tabWidth: 98 1 9 2448 2066 8 #setLexerLanguage: 98 1 8 #smalltalk 2448 2066 8 #positionCacheSize: 98 1 1 2448 2194 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 244 1 0 0 44 1 0 0] 98 0 2256 0 27 2274 2320 1 2352 1 2384 51 2306 8 #fixedParentBottom 1 234 256 98 2 2448 8 'document' 0 2002 202 208 98 1 2066 2096 98 2 770 2879 21 770 1001 601 416 2194 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 147 7 0 0 54 1 0 0] 98 2 560 2448 2256 0 27 )! !
!CodeSourcePresenter class categoriesFor: #codeFont!public! !
!CodeSourcePresenter class categoriesFor: #codeFont:!public! !
!CodeSourcePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

JadeMigrateClassDialog guid: (GUID fromString: '{F275BC44-A982-4EC0-A934-3D9C87A9296D}')!
JadeMigrateClassDialog comment: ''!
!JadeMigrateClassDialog categoriesForClass!Unclassified! !
!JadeMigrateClassDialog methodsFor!

createComponents

	super createComponents.
	copyMethodsPresenter 				:= self add: BooleanPresenter	new name: 'copyMethods'.
	recompileSubclassesPresenter 			:= self add: BooleanPresenter	new name: 'recompileSubclasses'.
	migrateInstancesPresenter 			:= self add: BooleanPresenter	new name: 'migrateInstances'.
	removeFromClassHistoryPresenter	:= self add: BooleanPresenter	new name: 'removeFromClassHistory'.
!

ok

	self model value: (Dictionary new
		at: #copyMethods 						put: copyMethodsPresenter value;
		at: #recompileSubclasses			put: recompileSubclassesPresenter value;
		at: #migrateInstances 				put: migrateInstancesPresenter value;
		at: #removeFromClassHistory	put: removeFromClassHistoryPresenter value;
		yourself).
	super ok.
!

onViewOpened

	super onViewOpened.
	copyMethodsPresenter 				value: true.
	recompileSubclassesPresenter 			value: true.
	migrateInstancesPresenter 			value: false.
	removeFromClassHistoryPresenter value: false.
! !
!JadeMigrateClassDialog categoriesFor: #createComponents!public! !
!JadeMigrateClassDialog categoriesFor: #ok!public! !
!JadeMigrateClassDialog categoriesFor: #onViewOpened!public! !

!JadeMigrateClassDialog class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 0 0 0 0 234 256 98 8 410 8 ##(Smalltalk.CheckBox)  98 16 0 416 98 2 8 1140924419 1 592 721990 2 ##(Smalltalk.ValueHolder)  0 32 1114118 ##(Smalltalk.NeverSearchPolicy)  16 482 512 0 7 0 0 0 592 0 8 4294903163 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  11 251 930 491 61 592 866 8 #text: 98 1 8 'Migrate all instances (requires Commit)' 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 125 0 0 0 250 0 0 0 155 0 0 0] 98 0 930 193 193 0 27 8 'migrateInstances' 410 608 98 16 0 416 98 2 8 1140924419 1 1136 674 0 32 720 16 482 512 0 7 0 0 0 1136 0 8 4294903163 770 0 0 0 802 202 208 98 2 866 896 98 2 930 11 301 930 511 61 1136 866 992 98 1 8 'Remove old classes from history' 1136 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 150 0 0 0 4 1 0 0 180 0 0 0] 98 0 1104 0 27 8 'removeFromClassHistory' 410 608 98 16 0 416 98 2 8 1140924419 1 1488 674 0 32 720 16 482 512 0 7 0 0 0 1488 0 8 4294903163 770 0 0 0 802 202 208 98 2 866 896 98 2 930 11 151 930 451 61 1488 866 992 98 1 8 'Copy methods from old class' 1488 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 75 0 0 0 230 0 0 0 105 0 0 0] 98 0 1104 0 27 8 'copyMethods' 410 608 98 16 0 416 98 2 8 1140924419 1 1840 674 0 32 720 16 482 512 0 7 0 0 0 1840 0 8 4294903163 770 0 0 0 802 202 208 98 2 866 896 98 2 930 11 201 930 441 61 1840 866 992 98 1 8 'Recompile all subclasses' 1840 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 100 0 0 0 225 0 0 0 130 0 0 0] 98 0 1104 0 27 8 'recompileSubclasses' 590342 ##(Smalltalk.Rectangle)  930 21 21 930 21 21 0 0 0 0 10627 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 1976922596 802 202 208 98 3 866 896 98 2 930 5119 21 930 541 591 416 866 992 98 1 8 'ClassHistory' 416 866 8 #updateMenuBar 98 0 416 1042 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 13 11 0 0 49 1 0 0] 98 7 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2560 0 0 0 7 0 0 0 2560 0 8 4294903337 770 0 0 0 802 202 208 98 2 866 896 98 2 930 11 11 930 481 131 2560 866 992 98 1 8 'This new class replaces a pre-existing class with the same name. What additional actions should be taken?' 2560 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 245 0 0 0 70 0 0 0] 98 0 1104 0 27 1488 1840 592 1136 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 2880 0 0 0 7 0 0 0 2880 0 8 4294903163 1180998 4 ##(Smalltalk.CommandDescription)  8 #ok 8 'Proceed' 1 1 0 0 16 0 0 0 802 202 208 98 3 866 896 98 2 930 211 461 930 141 51 2880 866 8 #isEnabled: 98 1 32 2880 866 992 98 1 8 'Proceed' 2880 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 230 0 0 0 175 0 0 0 255 0 0 0] 98 0 1104 0 29 410 2896 98 20 0 416 98 2 8 1140924416 1 3296 0 0 0 7 0 0 0 3296 0 8 4294903163 2978 8 #cancel 8 'Revert' 1 1 0 0 32 0 0 0 802 202 208 98 3 866 896 98 2 930 371 461 930 141 51 3296 866 3168 98 1 32 3296 866 992 98 1 8 'Revert' 3296 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 230 0 0 0 255 0 0 0 255 0 0 0] 98 0 1104 0 29 1104 0 27 )! !
!JadeMigrateClassDialog class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

