"Filed out from Dolphin Smalltalk 7"!

VWComponent subclass: #VWClass
	instanceVariableNames: 'superClass indexedType instVars classInstVars imports comment methods'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

