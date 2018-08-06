"Filed out from Dolphin Smalltalk 7"!

Object subclass: #VWSourceFile
	instanceVariableNames: 'timeStamp classes methods nameSpaces properties initializers stream'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VWSourceFile guid: (GUID fromString: '{e9ca297e-6f08-4dbd-afc9-5de48a98e4c0}')!
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

