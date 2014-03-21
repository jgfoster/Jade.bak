| package |
package := Package name: 'VisualWorks Component'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.011'.

package basicScriptAt: #postinstall put: '''Loaded: VisualWorks Component'' yourself.'.

package classNames
	add: #VWClass;
	add: #VWComponent;
	add: #VWMethods;
	add: #VWNameSpace;
	add: #VWSourceFile;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Steve Waring\Yaxo\YAXO DOM';
	yourself).

package!

"Class Definitions"!

Object subclass: #VWComponent
	instanceVariableNames: 'element name environment category package isPrivate sharedVariables'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #VWMethods
	instanceVariableNames: 'package className isMeta category methods'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #VWSourceFile
	instanceVariableNames: 'timeStamp classes methods nameSpaces properties initializers stream'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VWComponent subclass: #VWClass
	instanceVariableNames: 'superClass indexedType instVars classInstVars imports comment methods'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VWComponent subclass: #VWNameSpace
	instanceVariableNames: 'imports'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

VWComponent guid: (GUID fromString: '{408B2BD9-180C-481F-970B-1EA93BA60CDD}')!
VWComponent comment: ''!
!VWComponent categoriesForClass!Unclassified! !
!VWComponent methodsFor!

addSharedVar: nameString initializer: codeString

	sharedVariables 
		at: nameString 
		put: codeString.
!

category

	^category.
!

environment

	^environment.
!

initialize: anElement

	element := anElement.
	self 
		readCommon;
		readOther;
		yourself.
	element := nil.
!

initializerFor: anArray

	| stream period |
	stream := WriteStream on: String new.
	period := ''.
	anArray do: [:each | 
		stream 
			nextPutAll: period;
			nextPutAll: each;
			yourself.
		period := '.'.
	].
	stream
		nextPutAll: ' := ';
		nextPutAll: (sharedVariables at: anArray last);
		nextPut: $.;
		yourself.
	^stream contents.
!

name

	^name.
!

package

	^package.
!

printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: ': ' , name.
!

readAttributes

	| attributes |
	attributes := element entityAt: #attributes.
	attributes entitiesDo: [:key :value | 
		key = #'package' ifTrue: [
			package := value elements first string.
		] ifFalse: [
			MessageBox notify: 'Sorry, we are not yet prepared to handle ' , key printString , ' (we only support the #package attribute)!!'.
			Keyboard default isShiftDown ifTrue: [self halt].
		].
	].
!

readCommon

	name 		:= (element entityAt: #name		) elements first string.
	environment 	:= (element entityAt: #environment 	) elements first string.
	category 		:= (element entityAt: #category 	) elements first string.
	isPrivate 		:= (element entityAt: #private		) elements first string = 'true'.
	sharedVariables := Dictionary new.
	self readAttributes.
! !
!VWComponent categoriesFor: #addSharedVar:initializer:!public! !
!VWComponent categoriesFor: #category!public! !
!VWComponent categoriesFor: #environment!public! !
!VWComponent categoriesFor: #initialize:!public! !
!VWComponent categoriesFor: #initializerFor:!public! !
!VWComponent categoriesFor: #name!public! !
!VWComponent categoriesFor: #package!public! !
!VWComponent categoriesFor: #printOn:!public! !
!VWComponent categoriesFor: #readAttributes!public! !
!VWComponent categoriesFor: #readCommon!public! !

!VWComponent class methodsFor!

from: anElement

	^self new
		initialize: anElement;
		yourself.
! !
!VWComponent class categoriesFor: #from:!public! !

VWMethods guid: (GUID fromString: '{2AD4F4DE-E467-49DE-BBB0-15E24ABEE6DF}')!
VWMethods comment: ''!
!VWMethods categoriesForClass!Unclassified! !
!VWMethods methodsFor!

className
	^className!

initialize: anElement

	methods := OrderedCollection new.
	anElement entitiesDo: [:key :value |
		self processElement: key value: value.
	].
!

package

	^package.
!

printTopazFileInOn: aStream

	aStream
		nextPutAll: 'category: ';
		nextPutAll: category printString; cr;
		yourself.
	methods do: [:each | 
		isMeta ifTrue: [aStream nextPutAll: 'class'].
		aStream
			nextPutAll: 'method: ';
			nextPutAll: className; cr;
			nextPutAll: each; cr;
			nextPut: $%; cr;
			yourself.
	].
!

processBody: anElement

	| text |
	package := anElement attributes at: 'package' ifAbsent: [nil].
	text := anElement elements first string.
	methods add: text.
!

processCategory: anElement

	category isNil ifFalse: [self error: 'Category isNil!!?'].
	category := anElement elements first string.
!

processClassName: anElement

	className isNil ifFalse: [self error: 'Class name is nil!!?'].
	className := anElement elements first string.
	(isMeta := className endsWith: ' class') ifTrue: [
		className := className copyFrom: 1 to: className size - 6.
	].
	(className includes: $.) ifFalse: [^self].
	className := (className subStrings: $.) last.
!

processElement: aKey value: aValue

	aKey = #'class-id' 	ifTrue: [^self processClassName: 	aValue].
	aKey = #'category' 	ifTrue: [^self processCategory: 		aValue].
	aKey = #'body'			ifTrue: [^self processBody:			aValue].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aKey printString , ' (we only support the #''class-id'', #''category'', and #''body'' keys)!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
! !
!VWMethods categoriesFor: #className!public! !
!VWMethods categoriesFor: #initialize:!public! !
!VWMethods categoriesFor: #package!public! !
!VWMethods categoriesFor: #printTopazFileInOn:!public! !
!VWMethods categoriesFor: #processBody:!public! !
!VWMethods categoriesFor: #processCategory:!public! !
!VWMethods categoriesFor: #processClassName:!public! !
!VWMethods categoriesFor: #processElement:value:!public! !

VWSourceFile guid: (GUID fromString: '{E9CA297E-6F08-4DBD-AFC9-5DE48A98E4C0}')!
VWSourceFile comment: ''!
!VWSourceFile categoriesForClass!Unclassified! !
!VWSourceFile methodsFor!

addClass: anElement

	classes add: (VWClass from: anElement).
!

addComment: anElement

	| globalName text vwGlobal |
	globalName 	:= (anElement entityAt: #'class-id' 	) elements first string.
	text 			:= (anElement entityAt: #'body'		) elements.
	text isEmpty ifTrue: [^self].
	text := text first string.
	vwGlobal := self globalNamed: globalName.
	vwGlobal comment: text.
!

addComponentProperty: anElement

	| name type property value dict |
	name 		:= (anElement entityAt: #name 		ifAbsent: [self error: anElement printString , ' doees not have a name!!?']) elements first string.
	type 			:= (anElement entityAt: #type 		ifAbsent: [self error: anElement printString , ' doees not have a type!!?']) elements first string.
	property 	:= (anElement entityAt: #property 	ifAbsent: [self error: anElement printString , ' doees not have a property!!?']) elements first string.
	value 		:= (anElement entityAt: #value 		ifAbsent: [self error: anElement printString , ' doees not have a value!!?']) elements first string.
	dict := properties 
		at: type
		ifAbsentPut: [Dictionary new].
	dict := dict
		at: name
		ifAbsentPut: [Dictionary new].
	dict
		at: property
		put: value.
!

addInitialize: anElement

	anElement entitiesDo: [:key :value |
		self addInitialize: key value: value.
	].
!

addInitialize: aKey value: aValue

	aKey = #'class-id' ifTrue: [^self addInitializeClass: aValue].
	aKey = #'variable-id' ifTrue: [^self addInitializeVariable: aValue].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aKey printString , ' (we only support the #''class-id'' and #''variable-id'' keys)!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

addInitializeClass: anElement

	| globalName vwGlobal |
	globalName := anElement elements first string.
	vwGlobal := self globalNamed: globalName.
	initializers add: (vwGlobal initializer).
!

addInitializeVariable: anElement 

	| variableName list globalName vwGlobal initializer |
	variableName := anElement elements first string.
	list := variableName subStrings: $..
	globalName := list at: list size - 1.
	(vwGlobal := self globalNamed: globalName) isNil ifTrue: [self error: globalName printString , ' not found!!?'. ^self].
	(initializer := vwGlobal initializerFor: list) isNil ifTrue: [^self].
	initializers add: initializer.
!

addMethods: anElement 

	| newMethods className vwClass |
	newMethods := VWMethods new initialize: anElement.
	className := newMethods className.
	vwClass := classes 
		detect: [:each | each name = className]
		ifNone: [nil].
	vwClass isNil ifTrue: [
		methods add: newMethods.
	] ifFalse: [
		vwClass addMethods: newMethods.
	].
!

addNameSpace: anElement

	nameSpaces add: (VWNameSpace from: anElement).
!

addSharedVariable: anElement

	| globalName variableName vwGlobal initializer |
	globalName	 	:= (anElement entityAt: #'environment') elements first string.
	variableName 	:= (anElement entityAt: #'name') elements first string.
	initializer 			:= anElement entityAt: #'initializer' ifAbsent: [nil].
	initializer notNil ifTrue: [initializer := initializer elements first string].
	(vwGlobal			:= self globalNamed: globalName) isNil ifTrue: [self error: globalName printString , ' not found!!?'. ^self].
	vwGlobal 
		addSharedVar: variableName
		initializer: initializer.
!

addTimeStamp: anElement

	timeStamp := anElement elements first string.
!

asTopazFileIn

	stream := WriteStream on: String new.
	stream nextPut: $!!; space; nextPutAll: timeStamp; cr.
	self
		createSymbolDictionaries;
		createPackageComments;
		createClasses;
		createMethods;
		createInitializers;
		yourself.
	^stream contents.
!

createClasses

	classes do: [:each | each printTopazFileInOn: stream].
!

createInitializers

	stream nextPutAll: 'doit'; cr.
	initializers do: [:each | stream nextPutAll: each; cr].
	stream nextPutAll: '%'; cr.
!

createMethods

	| list |
	list := OrderedCollection  new.
	classes do: [:each | list addAll: each methods].
	list addAll: methods.
	list do: [:each | each printTopazFileInOn: stream].
!

createPackageComments

	(properties at: 'package' ifAbsent: [^self]) keysAndValuesDo: [:key :value |
		| string |
		stream 
			nextPutAll: 'run'; cr;
			nextPutAll: '| dict |'; cr;
			nextPutAll: 'dict := System myUserProfile symbolList objectNamed: ';
			nextPutAll: key printString;
			nextPut: $.; cr;
			nextPutAll: 'dict isNil ifTrue: [^nil].'; cr;
			yourself.
		string := value at: 'comment' ifAbsent: [nil].
		string notNil ifTrue: [
			stream 
				nextPutAll: 'dict at: #COMMENT put: '; cr;
				nextPutAll: string; 
				nextPut: $.; cr;
				yourself.
		].
		string := value at: 'version' ifAbsent: [nil].
		string notNil ifTrue: [
			stream 
				nextPutAll: 'dict at: #VERSION put: ';
				nextPutAll: string; 
				nextPut: $.; cr;
				yourself.
		].
		stream nextPut: $%; cr.
	].
!

createSymbolDictionaries

	stream nextPutAll: 'run
| existingSymbolList newSymbols newDictionary |
existingSymbolList := System myUserProfile symbolList.
newSymbols := #('.
	 self packages reverseDo: [:each | stream space; nextPut: $#; nextPutAll: each printString].
	stream nextPutAll: ' ).
newSymbols do: [:eachSymbol |
	existingSymbolList
		detect: [:each | each includesKey: eachSymbol]
		ifNone: [
			| newDict |
			newDict := SymbolDictionary new.
			newDict at: eachSymbol put: newDict.
			System myUserProfile insertDictionary: newDict at: 1.
		].
].
%
'.
!

globalNamed: aString 

	| pieces |
	pieces := aString subStrings: $..
	classes do: [:each | 
		each name = pieces last ifTrue: [^each].
	].
	nameSpaces do: [:each | 
		each name = pieces last ifTrue: [^each].
	].
	^nil.
!

initializeFromFilePath: aString

	| document |
	document := XMLDOMParser parseDocumentFromFileNamed: aString.
	self initializeFromXMLDocument: document.
!

initializeFromString: aString

	| document |
	document := XMLDOMParser parseDocumentFrom: (ReadStream on: aString).
	self initializeFromXMLDocument: document.
!

initializeFromXMLDocument: anXMLDocument

	properties 	:= Dictionary new.
	classes 		:= OrderedCollection new.
	methods 		:= OrderedCollection new.
	nameSpaces 	:= OrderedCollection new.
	initializers		:= OrderedCollection  new.
	(anXMLDocument entityAt: #'st-source') entitiesDo: [:key :value | self processElement: key value: value].
!

packages

	| list |
	list := Set new.
	nameSpaces , classes do: [:each | list add: each package].
	^list asSortedCollection.
!

processElement: aKey value: aValue

	aKey = #'time-stamp' 				ifTrue: [^self addTimeStamp: 				aValue].
	aKey = #'component-property' 	ifTrue: [^self addComponentProperty: 	aValue].
	aKey = #'class'							ifTrue: [^self addClass:							aValue].
	aKey = #'methods'					ifTrue: [^self addMethods:						aValue].
	aKey = #'comment'					ifTrue: [^self addComment:					aValue].
	aKey = #'initialize'						ifTrue: [^self addInitialize:						aValue].
	aKey = #'shared-variable'			ifTrue: [^self addSharedVariable: 			aValue].
	aKey = #'name-space'				ifTrue: [^self addNameSpace:				aValue].
	aKey = #'component-created'	ifTrue: [^self].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aKey printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

removeAllInPackage: aString

	classes := classes reject: [:each | each package = aString].
	methods := methods reject: [:each | each package = aString].
!

removeSPort

	| list |
	list := self packages.
	list := list select: [:each | each beginsWith: 'Sp'].
	list do: [:each | self removeAllInPackage: each].
! !
!VWSourceFile categoriesFor: #addClass:!public! !
!VWSourceFile categoriesFor: #addComment:!public! !
!VWSourceFile categoriesFor: #addComponentProperty:!public! !
!VWSourceFile categoriesFor: #addInitialize:!public! !
!VWSourceFile categoriesFor: #addInitialize:value:!public! !
!VWSourceFile categoriesFor: #addInitializeClass:!public! !
!VWSourceFile categoriesFor: #addInitializeVariable:!public! !
!VWSourceFile categoriesFor: #addMethods:!public! !
!VWSourceFile categoriesFor: #addNameSpace:!public! !
!VWSourceFile categoriesFor: #addSharedVariable:!public! !
!VWSourceFile categoriesFor: #addTimeStamp:!public! !
!VWSourceFile categoriesFor: #asTopazFileIn!public! !
!VWSourceFile categoriesFor: #createClasses!public! !
!VWSourceFile categoriesFor: #createInitializers!public! !
!VWSourceFile categoriesFor: #createMethods!public! !
!VWSourceFile categoriesFor: #createPackageComments!public! !
!VWSourceFile categoriesFor: #createSymbolDictionaries!public! !
!VWSourceFile categoriesFor: #globalNamed:!public! !
!VWSourceFile categoriesFor: #initializeFromFilePath:!public! !
!VWSourceFile categoriesFor: #initializeFromString:!public! !
!VWSourceFile categoriesFor: #initializeFromXMLDocument:!public! !
!VWSourceFile categoriesFor: #packages!public! !
!VWSourceFile categoriesFor: #processElement:value:!public! !
!VWSourceFile categoriesFor: #removeAllInPackage:!public! !
!VWSourceFile categoriesFor: #removeSPort!public! !

!VWSourceFile class methodsFor!

fromDocument: anXMLDocument

	^self new
		initializeFromXMLDocument: anXMLDocument;
		yourself.
!

fromPath: aString

	^self new
		initializeFromFilePath: aString;
		yourself.

!

fromString: aString

	^self new
		initializeFromString: aString;
		yourself.

! !
!VWSourceFile class categoriesFor: #fromDocument:!public! !
!VWSourceFile class categoriesFor: #fromPath:!public! !
!VWSourceFile class categoriesFor: #fromString:!public! !

VWClass guid: (GUID fromString: '{148C4A80-9D4D-4757-AE9C-49A17AEF60C5}')!
VWClass comment: ''!
!VWClass categoriesForClass!Unclassified! !
!VWClass methodsFor!

addMethod: aMethod 

	methods add: methods.
!

addMethods: aVWMethods 

	methods add: aVWMethods.
!

comment: aString

	comment := aString.
!

initializer

	^name , ' initialize.'.
!

methods

	^methods.
!

printBytesTopazFileInOn: aStream

	aStream 
		nextPutAll: '!! Category: ';
		nextPutAll: category; cr;
		nextPutAll: 'run'; cr;
		nextPutAll: superClass last; cr; 
		tab; nextPutAll: 'byteSubclass: ';
		nextPutAll: name printString; cr; 
		tab; nextPutAll: 'classVars: #(';
		yourself.
	sharedVariables keys do: [:each | 
		aStream cr; tab; tab; nextPutAll: each.
	].
	aStream  
		nextPutAll: ')'; cr;
		tab; nextPutAll: 'classInstVars: #(';
		yourself.
	classInstVars do: [:each | 
		aStream cr; tab; tab; nextPutAll: each.
	].
	aStream
		nextPutAll: ')'; cr;
		tab; nextPutAll: 'poolDictionaries: #()'; cr;
		tab; nextPutAll: 'inDictionary: (System myUserProfile symbolList objectNamed: ';
		nextPutAll: package printString; 
		nextPut: $); cr;
		tab; nextPutAll: 'instancesInvariant: false.'; cr;
		nextPut: $%; cr;
		yourself.
	self printCommentOn: aStream.
!

printCommentOn: aStream

	comment isNil ifTrue: [^self].
	aStream
		nextPutAll: 'run'; cr;
		nextPutAll: '| doc txt |'; cr; 
		nextPutAll: 'doc := GsClassDocumentation newForClass: ';
		nextPutAll: name;
		nextPutAll: '.'; cr;
		nextPutAll: 'txt := GsDocText new details: '; cr;
		nextPutAll: comment printString;
		nextPutAll: '.'; cr;
		nextPutAll: 'doc documentClassWith: txt.'; cr;
		nextPutAll: name;
		nextPutAll: ' description: doc.'; cr;
		nextPut: $%; cr;
		yourself.
!

printObjectsTopazFileInOn: aStream

	aStream 
		nextPutAll: '!! Category: ';
		nextPutAll: category; cr;
		nextPutAll: 'run'; cr;
		nextPutAll: superClass last; cr; 
		tab; nextPutAll: 'indexableSubclass: ';
		nextPutAll: name printString; cr; 
		tab; nextPutAll: 'instVarNames: #(';
		yourself.
	instVars do: [:each | 
		aStream cr; tab; tab; nextPutAll: each.
	].
	aStream
		nextPutAll: ')'; cr;
		tab; nextPutAll: 'classVars: #(';
		yourself.
	sharedVariables keys do: [:each | 
		aStream cr; tab; tab; nextPutAll: each.
	].
	aStream  
		nextPutAll: ')'; cr;
		tab; nextPutAll: 'classInstVars: #(';
		yourself.
	classInstVars do: [:each | 
		aStream cr; tab; tab; nextPutAll: each.
	].
	aStream
		nextPutAll: ')'; cr;
		tab; nextPutAll: 'poolDictionaries: #()'; cr;
		tab; nextPutAll: 'inDictionary: (System myUserProfile symbolList objectNamed: ';
		nextPutAll: package printString; 
		nextPut: $); cr;
		tab; nextPutAll: 'constraints: #()'; cr;
		tab; nextPutAll: 'instancesInvariant: false'; cr;
		tab; nextPutAll: 'isModifiable: false.'; cr;
		nextPut: $%; cr;
		yourself.
	self printCommentOn: aStream.
!

printTopazFileInOn: aStream

	indexedType = 'bytes' ifTrue: [^self printBytesTopazFileInOn: aStream].
	indexedType = 'objects' ifTrue: [^self printObjectsTopazFileInOn: aStream].
	aStream 
		nextPutAll: '!! Category: ';
		nextPutAll: category; cr;
		nextPutAll: 'run'; cr;
		nextPutAll: superClass last; cr; 
		yourself.
	indexedType = 'none' ifTrue: [
		aStream tab; nextPutAll: 'subclass: '.
	] ifFalse: [
		MessageBox notify: 'Sorry, we are not yet prepared to handle ' , indexedType printString , '!!'.
		Keyboard default isShiftDown ifTrue: [self halt].
	].
	aStream 
		nextPutAll: name printString; cr; 
		tab; nextPutAll: 'instVarNames: #(';
		yourself.
	instVars do: [:each | 
		aStream cr; tab; tab; nextPutAll: each.
	].
	aStream
		nextPutAll: ')'; cr;
		tab; nextPutAll: 'classVars: #(';
		yourself.
	sharedVariables keys do: [:each | 
		aStream cr; tab; tab; nextPutAll: each.
	].
	aStream  
		nextPutAll: ')'; cr;
		tab; nextPutAll: 'classInstVars: #(';
		yourself.
	classInstVars do: [:each | 
		aStream cr; tab; tab; nextPutAll: each.
	].
	aStream
		nextPutAll: ')'; cr;
		tab; nextPutAll: 'poolDictionaries: #()'; cr;
		tab; nextPutAll: 'inDictionary: (System myUserProfile symbolList objectNamed: ';
		nextPutAll: package printString; 
		nextPut: $); cr;
		tab; nextPutAll: 'constraints: #()'; cr;
		tab; nextPutAll: 'instancesInvariant: false'; cr;
		tab; nextPutAll: 'isModifiable: false.'; cr;
		nextPut: $%; cr;
		yourself.
	self printCommentOn: aStream.
!

readOther

	superClass 	:= (element entityAt: #super 			) elements first string subStrings: $..
	indexedType 	:= (element entityAt: #'indexed-type' 		) elements first string.
	instVars 		:= (element entityAt: #'inst-vars' 		) elements.
	classInstVars 	:= (element entityAt: #'class-inst-vars' 	) elements.
	imports 		:= (element entityAt: #imports 			) elements.
	methods		:= OrderedCollection new.
	superClass first = 'Core' ifTrue: [superClass := superClass copyFrom: 2 to: superClass size].

	instVars 		notEmpty ifTrue: [instVars 		:= instVars 		first string subStrings].
	classInstVars 	notEmpty ifTrue: [classInstVars 	:= classInstVars 	first string subStrings].
! !
!VWClass categoriesFor: #addMethod:!public! !
!VWClass categoriesFor: #addMethods:!public! !
!VWClass categoriesFor: #comment:!public! !
!VWClass categoriesFor: #initializer!public! !
!VWClass categoriesFor: #methods!public! !
!VWClass categoriesFor: #printBytesTopazFileInOn:!public! !
!VWClass categoriesFor: #printCommentOn:!public! !
!VWClass categoriesFor: #printObjectsTopazFileInOn:!public! !
!VWClass categoriesFor: #printTopazFileInOn:!public! !
!VWClass categoriesFor: #readOther!public! !

VWNameSpace guid: (GUID fromString: '{131258E5-5281-42D3-A12C-501CAE32FBE4}')!
VWNameSpace comment: ''!
!VWNameSpace categoriesForClass!Unclassified! !
!VWNameSpace methodsFor!

readOther

	imports := (element entityAt: #'imports') elements first string trimBlanks.
! !
!VWNameSpace categoriesFor: #readOther!public! !

"Binary Globals"!

