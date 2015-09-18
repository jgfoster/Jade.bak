| package |
package := Package name: 'Jade Class Browser'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.071'.


package classNames
	add: #GsClass;
	add: #JadeMigrateClassDialog;
	yourself.

package methodNames
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
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'GemStone Objects';
	add: 'GemStone Session';
	add: 'Jade UI Base';
	yourself).

package!

"Class Definitions"!

GsObject subclass: #GsClass
	instanceVariableNames: 'category parent children isVisible classHistory isTestCase'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeValueDialog subclass: #JadeMigrateClassDialog
	instanceVariableNames: 'copyMethodsPresenter recompileSubclassesPresenter migrateInstancesPresenter removeFromClassHistoryPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

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

