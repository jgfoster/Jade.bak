| package |
package := Package name: 'Jade Method Browser'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.086'.


package classNames
	add: #GsMethod2;
	add: #JadeMethodHistoryBrowser;
	add: #JadeMethodListBrowser;
	add: #JadeMethodListPresenter;
	add: #MethodListPresenter;
	add: #MethodSourcePresenter;
	yourself.

package methodNames
	add: #GsClass -> #methodsUpTo:filterList:isVariables:;
	add: #GsClass -> #sourceFor:;
	add: #GsClass -> #stepPointsFor:;
	add: #JadePresenter -> #browseImplementorsOf:;
	add: #JadePresenter -> #browseSendersOf:;
	add: #JadeServer -> #_addMethod:toStream:;
	add: #JadeServer -> #_methodsFor:categories:;
	add: #JadeServer -> #_methodsFor:filter:isVariables:;
	add: #JadeServer -> #_methodsFor:variables:;
	add: #JadeServer -> #_packageNameFor:;
	add: #JadeServer -> #behaviorFor:in:;
	add: #JadeServer -> #compileMethod:behavior:symbolList:inCategory:;
	add: #JadeServer -> #compileMethod:behavior:user:inCategory:;
	add: #JadeServer -> #implementorsOf:;
	add: #JadeServer -> #implementorsOf:startingAt:;
	add: #JadeServer -> #methodsContaining:;
	add: #JadeServer -> #methodsFor:upTo:filter:isVariables:;
	add: #JadeServer -> #moveMethod:toCategory:;
	add: #JadeServer -> #referencesToObject:;
	add: #JadeServer -> #removeCategory:fromBehavior:;
	add: #JadeServer -> #removeMethod:;
	add: #JadeServer -> #renameCategory:to:inBehavior:;
	add: #JadeServer -> #runAsTest:;
	add: #JadeServer -> #sbSaveMethod:;
	add: #JadeServer -> #selectorsMatching:;
	add: #JadeServer -> #sendersOf:;
	add: #JadeServer -> #sourceFor:in:;
	add: #JadeServer -> #stepPointsFor:in:;
	add: #JadeServer -> #streamOfMethods:;
	add: #JadeServer -> #stringOfLineNumbersWithBreaksIn:;
	add: #JadeServer64bit3x -> #_methodsFor:categories:;
	add: #JadeServer64bit3x -> #compileMethod:behavior:symbolList:inCategory:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter';
	add: '..\Object Arts\Dolphin\MVP\Views\Scintilla\Dolphin Scintilla View';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'GemStone Objects';
	add: 'GemStone Session';
	add: 'Jade Class Browser';
	add: 'Jade UI Base';
	yourself).

package!

"Class Definitions"!

GsObject subclass: #GsMethod2
	instanceVariableNames: 'gsClass category isTestCase'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter subclass: #MethodListPresenter
	instanceVariableNames: 'classListPresenter methodListPresenter inheritanceListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CodeSourcePresenter subclass: #MethodSourcePresenter
	instanceVariableNames: 'currentSelector'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeBrowserPresenter subclass: #JadeMethodListPresenter
	instanceVariableNames: 'methodListPresenter methodSourcePresenter searchBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeShell subclass: #JadeMethodHistoryBrowser
	instanceVariableNames: 'versionListPresenter editorPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeShell subclass: #JadeMethodListBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!GsClass methodsFor!

methodsUpTo: aClass filterList: aList isVariables: aBoolean

	|  stream string |
	stream := WriteStream on: String new.
	aList do: [:each | stream nextPutAll: each; tab].
	string := gciSession
		serverPerform: #'methodsFor:upTo:filter:isVariables:' 
		with: self
		with: aClass
		with: stream contents 
		with: aBoolean.
	^GsMethod2
		listFromString: string 
		session: gciSession.
!

sourceFor: anObject

	^gciSession
		serverPerform: #'sourceFor:in:'
		with: anObject
		with: self.
!

stepPointsFor: aGsMethod

	^gciSession
		serverPerform: #'stepPointsFor:in:'
		with: aGsMethod
		with: self.
! !
!GsClass categoriesFor: #methodsUpTo:filterList:isVariables:!Methods!public! !
!GsClass categoriesFor: #sourceFor:!Methods!public! !
!GsClass categoriesFor: #stepPointsFor:!Methods!public! !

!JadePresenter methodsFor!

browseImplementorsOf: aGsMethodOrString

	(JadeMethodListBrowser showOn: self gciSession)
		browseImplementorsOf: aGsMethodOrString;
		yourself.
!

browseSendersOf: aGsMethodOrString

	(JadeMethodListBrowser showOn: self gciSession)
		browseSendersOf: aGsMethodOrString;
		yourself.
! !
!JadePresenter categoriesFor: #browseImplementorsOf:!public! !
!JadePresenter categoriesFor: #browseSendersOf:!public! !

!JadeServer methodsFor!

_addMethod: aGsMethod toStream: aStream

	| inClass testCaseClass |
	inClass := aGsMethod inClass.
	(self oopOf: aGsMethod) printOn: aStream.
	aStream 
		tab; nextPutAll: aGsMethod selector; 
		tab; nextPutAll: (inClass categoryOfSelector: aGsMethod selector);
		tab.
	(self oopOf: inClass) printOn: aStream.
	aStream tab; nextPutAll: inClass name; tab.
	inClass category notNil ifTrue: [aStream nextPutAll: inClass category].
	aStream tab.
	testCaseClass := Globals
		at: #'TestCase'
		ifAbsent: [nil].
	((testCaseClass  notNil and: [inClass isSubclassOf: testCaseClass]) and: [inClass testSelectors includes: aGsMethod selector]) printOn: aStream.
	aStream lf.
!

_methodsFor: aClass categories: aList

	| methods |
	methods := IdentitySet new.
	aList do: [:eachCategory | 
		(aClass _includesCategory: eachCategory) ifTrue: [
			(aClass selectorsIn: eachCategory) do: [:eachSelector |
				methods add: (aClass compiledMethodAt: eachSelector).
			].
		].
	].
	^methods.

!

_methodsFor: aClass filter: aList isVariables: aBoolean

	^aBoolean 
		ifTrue:	[self _methodsFor: aClass variables: 	aList]
		ifFalse:	[self _methodsFor: aClass categories: aList].
!

_methodsFor: aClass variables: aList

	| methods |
	aList isEmpty ifTrue: [^aClass selectors collect: [:each | aClass compiledMethodAt: each]].
	methods := IdentitySet new.
	aClass selectors do: [:each | 
		| method intersect |
		method := aClass compiledMethodAt: each.
		intersect := method instVarsAccessed * aList.
		intersect notEmpty ifTrue: [methods add: method].
	].
	^methods.
!

_packageNameFor: aCategoryName

	| string mcWorkingCopyClass list |
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^''].
	string := aCategoryName asUppercase copyFrom: 2 to: aCategoryName size.
	list := mcWorkingCopyClass allManagers collect: [:each | each packageName].
	list := list select: [:each | (string copyFrom: 1 to: (string size min: each size)) = each asUppercase].
	list isEmpty ifTrue: [^''].
	list size = 1 ifTrue: [^list first].
	^(list asSortedCollection: [:a :b | a size <= b size]) last.
!

behaviorFor: selector in: aClass

	| behavior |
	behavior := aClass.
	[
		behavior notNil.
	] whileTrue: [
		(behavior includesSelector: selector) ifTrue: [^behavior].
		behavior := behavior superclass.
	].
	self error: 'Method not found in class or in any superclass'.
!

compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol
	"Returns aGsNMethod (if successful) -> anArrayOfErrorsOrWarnings"

	| result |
	"Method is in GsFoundation, but not in GsBase"
	result := (aBehavior class canUnderstand: #'compileMethod:category:using:environmentId:') ifTrue: [
		[
			aBehavior		"returns self or signals a CompileError"
				compileMethod: methodString
				category: categorySymbol
				using: aSymbolList
				environmentId: 0.
			nil.
		] on: (self objectNamed: #'UndefinedSymbolNotification') do: [:ex | 
			ex resume: false.
		].
	] ifFalse: [(aBehavior class canUnderstand: #'compileMethod:category:using:') ifTrue: [
		[
			aBehavior		"returns self or signals a CompileError"
				compileMethod: methodString
				category: categorySymbol
				using: aSymbolList.
			nil.
		] on: (self objectNamed: #'UndefinedSymbolNotification') do: [:ex | 
			ex resume: false.
		].
	] ifFalse: [
		aBehavior		"returns nil or an Array of error descriptions"
			compileMethod: methodString
			dictionaries: aSymbolList
			category: categorySymbol.
	]].
	result notNil ifTrue: [
		^nil -> result.
	].
	(aBehavior class canUnderstand: #_primitiveCompileMethod:symbolList:category:oldLitVars:intoMethodDict:intoCategories:intoPragmas:) ifTrue: [
		result := aBehavior 
			_primitiveCompileMethod: methodString
			symbolList: aSymbolList
			category: categorySymbol
			oldLitVars: nil
			intoMethodDict: GsMethodDictionary new 
			intoCategories: GsMethodDictionary new
			intoPragmas: nil.
	] ifFalse: [
		(aBehavior class canUnderstand: #_primitiveCompileMethod:symbolList:category:obsoleteClassNames:oldLitVars:) ifTrue: [
			result := aBehavior 
				_primitiveCompileMethod: methodString
				symbolList: aSymbolList
				category: categorySymbol
				obsoleteClassNames: nil
				oldLitVars: nil.
		] ifFalse: [
			result := aBehavior 
				_primitiveCompileMethod: methodString
				symbolList: aSymbolList
				category: categorySymbol
				oldLitVars: nil
				intoMethodDict: GsMethodDictionary new 
				intoCategories: GsMethodDictionary new.
		].
	].
	(result isKindOf: Array) ifTrue: [
		"in 2.3.x: (Array with: compiledMethod with: errors with: warnings)"
		(result at: 2) notNil ifTrue: [^nil -> (result at: 2)].
		^(result at: 1) -> (result at: 3)
	].
	^result -> nil.
!

compileMethod: methodString behavior: aBehavior user: aUserProfileOrNil inCategory: categoryString

	| userProfile result gsMethod stream errDict errorList warnings |
	userProfile := aUserProfileOrNil isNil
		ifTrue: [System myUserProfile]
		ifFalse: [aUserProfileOrNil].
	result := self 		"key: GsNMethod value: ((Array withAll: errors) or aStringOfWarnings)"
		compileMethod: methodString 
		behavior: aBehavior 
		symbolList: userProfile symbolList 
		inCategory: categoryString asSymbol.
	(gsMethod := result key) isNil ifTrue: [
		errorList := result value.
		warnings := ''.
	] ifFalse: [
		errorList := #().
		warnings := result value.
	].
	stream := WriteStream on: String new.
	gsMethod notNil ifTrue: [
		stream nextPutAll: gsMethod selector.
	].
	errDict := GemStoneError at: System myUserProfile nativeLanguage.
	errorList do: [:each |
		stream lf; 
			nextPutAll: 'ERROR:'; tab;
			nextPutAll: (each at: 1) printString; tab;
			nextPutAll: (each at: 2) printString; tab;
			yourself.
		(each size >= 3 and: [(each at: 3) notNil]) ifTrue: [
			stream nextPutAll: (each at: 3); tab.
		] ifFalse: [
			(each at: 1) > errDict size ifTrue: [
				stream nextPutAll: '(unknown error number)'; tab.
			] ifFalse: [
				stream nextPutAll: (errDict at: (each at: 1)) asString; tab.
			].
		].
	].
	warnings isNil ifTrue: [warnings := ''].
	stream lf; nextPutAll: warnings.
	^stream contents.
!

implementorsOf: anObject

	| symbol |
	symbol := (anObject isKindOf: String)
		ifTrue: [anObject asSymbol]
		ifFalse: [anObject selector].
	^self streamOfMethods: (self classOrganizer implementorsOf: symbol).
!

implementorsOf: aGsMethod startingAt: aClass

	| selector myClass list |
	selector := aGsMethod selector.
	myClass := aClass.
	list := OrderedCollection new.
	[
		(myClass includesSelector: selector) ifTrue: [list add: myClass].
		(myClass := myClass superclass) notNil.
	] whileTrue: [].
	^self stringForClassList: list.
!

methodsContaining: aString

	^self streamOfMethods: (self classOrganizer substringSearch: aString) first.
!

methodsFor: childClass upTo: parentClass filter: aString isVariables: aBoolean 

	| filterList answerList aClass stream selectors |
	filterList := (aString subStrings: Character tab) reject: [:each | each isEmpty].
	aBoolean ifTrue: [filterList := (filterList collect: [:each | each asSymbol]) asIdentitySet].
	aClass := childClass.
	answerList := IdentitySet new.
	selectors := IdentitySet new.
	[
		| methods |
		methods := self 
			_methodsFor: aClass
			filter: filterList
			isVariables: aBoolean.
		methods do: [:each | 
			(selectors includes: each selector) ifFalse: [
				answerList add: each.
				selectors add: each selector.
			].
		].
		aClass = parentClass.
	] whileFalse: [
		aClass := aClass superclass.
	].
	stream := WriteStream on: String new.
	answerList do: [:each | self _addMethod: each toStream: stream].
	^stream contents!

moveMethod: aGsMethod toCategory: aString

	aGsMethod inClass
		moveMethod: aGsMethod selector
		toCategory: aString.
!

referencesToObject: anObject

	^self streamOfMethods: (self classOrganizer referencesToObject: anObject).
!

removeCategory: aString fromBehavior: aBehavior

	aBehavior removeCategory: aString.
!

removeMethod: aGsMethod

	aGsMethod inClass removeSelector: aGsMethod selector.
!

renameCategory: oldString to: newString inBehavior: aBehavior

	aBehavior
		renameCategory: oldString asSymbol
		to: newString.
!

runAsTest: aGsMethod

	aGsMethod inClass debug: aGsMethod selector.
	^true.
!

sbSaveMethod: anOrderedCollection
	"Save in method editor"

	| behavior category string association gsMethod |
	behavior := self sbClassFrom: anOrderedCollection.
	category := anOrderedCollection notEmpty ifTrue: [anOrderedCollection removeFirst] ifFalse: ['other'].
	string := self sbNextParagraph.
	association := self		"key: GsNMethod value: (Array withAll: errors and warnings)"
		compileMethod: string 
		behavior: behavior 
		symbolList: self symbolList 
		inCategory: category asSymbol.
	(gsMethod := association key) isNil ifTrue: [
		System
			signal: 1001 
			args: (Array with: association value)
			signalDictionary: GemStoneError.
	].
	selections 
		at: #'methodCategory' 	put: (gsMethod inClass categoryOfSelector: gsMethod selector) asString;
		at: #'method'					put: gsMethod selector asString;
		at: #'methodWarnings'	put: association value;
		yourself.
	self systemBrowserUpdate.
!

selectorsMatching: aString

	| user stream list |
	list := (aString subStrings: $*) asOrderedCollection collect: [:each | each asUppercase].
	list size - 1 to: 1 do: [:i | list add: $* afterIndex: i].
	aString last = $* ifTrue: [list addLast: $*].
	stream := WriteStream on: String new.
	user := AllUsers 
		userWithId: #SymbolUser 
		ifAbsent: [AllUsers userWithId: #DataCurator].
	list := list asArray.
	list := (user resolveSymbol: #AllSymbols) value select: [:each |each asUppercase matchPattern: list].
	list := list select: [:each | (self classOrganizer implementorsOf: each) notEmpty].
	list := list asSortedCollection.
	list do: [:each | stream nextPutAll: each; nextPut: Character lf; yourself].
	^stream contents.
!

sendersOf: anObject

	| symbol |
	symbol := (anObject isKindOf: String)
		ifTrue: [anObject asSymbol]
		ifFalse: [anObject selector].
	^self streamOfMethods: (self classOrganizer sendersOf: symbol) first.
!

sourceFor: anObject in: aClass

	| behavior selector packageName category mcTimestamp dict source |
	selector := (anObject isKindOf: String) 
		ifTrue: [anObject asSymbol]
		ifFalse: [anObject selector].
	behavior := self
		behaviorFor: selector 
		in: aClass.
	category := behavior categoryOfSelector: selector.
	packageName := category first = $*
		ifTrue: [self _packageNameFor: category]
		ifFalse: [behavior thisClass _classCategory].
	packageName isNil ifTrue: [packageName := ''].
	mcTimestamp := ''.
	dict := behavior extraDict.
	dict notNil ifTrue: [
		dict := dict at: #'GSMethodStampDict' ifAbsent: [nil].
		dict notNil ifTrue: [
			mcTimestamp := dict
				at: selector
				ifAbsent: [''].
		].
	].
	source := behavior sourceCodeAt: selector.
	^(WriteStream on: String new)
		nextPutAll: packageName; tab;
		nextPutAll: category; tab;
		nextPutAll: mcTimestamp; lf;
		nextPutAll: source;
		contents.
!

stepPointsFor: aGsMethod in: aClass

	| behavior method source breakStepPoints stepPoint stream |
	behavior := self
		behaviorFor: aGsMethod selector
		in: aClass.
	source := behavior sourceCodeAt: aGsMethod selector.
	method := behavior compiledMethodAt: aGsMethod selector.
	stream := WriteStream on: String new.
	breakStepPoints := (aGsMethod class canUnderstand: #'_breakpointIpOffsets')
		ifTrue: [aGsMethod _stepPointsFromBreakIpOffsets: aGsMethod _breakpointIpOffsets]
		ifFalse: [#()].
	stepPoint := 0.
	method _sourceOffsets do: [:each | 
		stepPoint := stepPoint + 1.
		(breakStepPoints includes: stepPoint) ifTrue: [stream nextPut: $B].
		each printOn: stream.
		stream nextPut: Character space.
	].
	stream lf; 
		nextPutAll: (self stringOfLineNumbersWithBreaksIn: method); lf;
		nextPutAll: source;
		yourself.
	^stream contents.
!

streamOfMethods: aList

	| stream |
	stream := WriteStream on: String new.
	aList do: [:each | 
		self
			_addMethod: each 
			toStream: stream.
	].
	^stream contents.
!

stringOfLineNumbersWithBreaksIn: aGsMethod

	| stepPoints offsets lines stream |
	stepPoints := (aGsMethod class canUnderstand: #'_breakpointIpOffsets')
		ifTrue: [aGsMethod _stepPointsFromBreakIpOffsets: aGsMethod _breakpointIpOffsets]
		ifFalse: [#()].
	offsets := stepPoints collect: [:each | aGsMethod _sourceOffsetsAt: each].
	lines := offsets collect: [:each | 
		((aGsMethod sourceString copyFrom: 1 to: each) select: [:char | char = Character lf]) size + 1.
	].
	stream := WriteStream on: String new.
	lines do: [:each | each printOn: stream. stream space].
	^stream contents.
! !
!JadeServer categoriesFor: #_addMethod:toStream:!Methods!public! !
!JadeServer categoriesFor: #_methodsFor:categories:!Methods!public! !
!JadeServer categoriesFor: #_methodsFor:filter:isVariables:!Methods!public! !
!JadeServer categoriesFor: #_methodsFor:variables:!Methods!public! !
!JadeServer categoriesFor: #_packageNameFor:!Methods!public! !
!JadeServer categoriesFor: #behaviorFor:in:!Methods!public! !
!JadeServer categoriesFor: #compileMethod:behavior:symbolList:inCategory:!Methods!public!System Browser! !
!JadeServer categoriesFor: #compileMethod:behavior:user:inCategory:!Methods!public! !
!JadeServer categoriesFor: #implementorsOf:!Methods!public! !
!JadeServer categoriesFor: #implementorsOf:startingAt:!public! !
!JadeServer categoriesFor: #methodsContaining:!Methods!public! !
!JadeServer categoriesFor: #methodsFor:upTo:filter:isVariables:!Methods!public! !
!JadeServer categoriesFor: #moveMethod:toCategory:!Methods!public! !
!JadeServer categoriesFor: #referencesToObject:!Methods!public! !
!JadeServer categoriesFor: #removeCategory:fromBehavior:!Methods!public! !
!JadeServer categoriesFor: #removeMethod:!Methods!public! !
!JadeServer categoriesFor: #renameCategory:to:inBehavior:!Methods!public! !
!JadeServer categoriesFor: #runAsTest:!Methods!public! !
!JadeServer categoriesFor: #sbSaveMethod:!public!System Browser! !
!JadeServer categoriesFor: #selectorsMatching:!Methods!public! !
!JadeServer categoriesFor: #sendersOf:!Methods!public! !
!JadeServer categoriesFor: #sourceFor:in:!Methods!public! !
!JadeServer categoriesFor: #stepPointsFor:in:!Methods!public! !
!JadeServer categoriesFor: #streamOfMethods:!Methods!public! !
!JadeServer categoriesFor: #stringOfLineNumbersWithBreaksIn:!Methods!public! !

!JadeServer64bit3x methodsFor!

_methodsFor: aClass categories: aList

	| methods |
	methods := IdentitySet new.
	aList do: [:eachCategory | 
		(aClass includesCategory: eachCategory) ifTrue: [
			(aClass selectorsIn: eachCategory) do: [:eachSelector |
				methods add: (aClass compiledMethodAt: eachSelector).
			].
		].
	].
	^methods.

!

compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol
	"returns (nil -> anArrayOfErrors) or (aGsNMethod -> compilerWarnings) or (aGsNMethod -> nil)"

	| method warnings |
	[[
		method := aBehavior
			compileMethod: methodString
			dictionaries: aSymbolList
			category: categorySymbol
			environmentId: 0.
	] on: CompileError do: [:ex |
		^nil -> (ex gsArguments at: 1)
	]] on: CompileWarning do: [:ex |
		warnings := ex gsArguments at: 1.
		ex resume.
	].
	^[	"Above method does not notify package of changes. 
		Repeat compile if possible using method that broadcasts changes (but does not signal warnings).
		See https://github.com/jgfoster/Jade/issues/3."
		aBehavior
			compileMethod: methodString 
			category: categorySymbol 
			using: aSymbolList 
			environmentId: 0.
		(aBehavior compiledMethodAt: method key selector) -> warnings.
	] on: Error do: [:ex | 
		ex return: method -> warnings.
	].
! !
!JadeServer64bit3x categoriesFor: #_methodsFor:categories:!Methods!public! !
!JadeServer64bit3x categoriesFor: #compileMethod:behavior:symbolList:inCategory:!Methods!public!System Browser! !

"End of package definition"!

"Source Globals"!

"Classes"!

GsMethod2 guid: (GUID fromString: '{D21971C6-FA84-4D7B-A684-D1DE5B9F0519}')!
GsMethod2 comment: ''!
!GsMethod2 categoriesForClass!Unclassified! !
!GsMethod2 methodsFor!

<= aGsMethod

	"self classCategory ~= aGsMethod classCategory ifTrue: [^self classCategory <= aGsMethod classCategory].
	self gsClass name ~= aGsMethod gsClass name ifTrue: [^self gsClass name ~= aGsMethod gsClass name].
	self category ~= aGsMethod category ifTrue: [^self category <= aGsMethod category]."
	^self name <= aGsMethod name.
!

category
	^category!

classCategory

	^gsClass category.
!

clearBreakAtStepPoint: anInteger

	gciSession
		send: #'clearBreakAtStepPoint:'
		to: oopType	
		with: anInteger.
!

gsClass
	^gsClass!

initialize: aList 

	| string |
	category := aList at: 3.
	string := (aList at: 4) , Character tab asString , (aList at: 5) , Character tab asString , (aList at: 6).
	gsClass := GsClass 
		fromString: string 
		session: gciSession.
	isTestCase := (aList at: 7) = 'true'.
!

isTestCase

	^isTestCase.
!

printOn: aStream

	gsClass printOn: aStream.
	aStream nextPutAll: '>>'.
	super printOn: aStream.
!

setBreakAtStepPoint: anInteger

	gciSession
		send: #'setBreakAtStepPoint:'
		to: oopType	
		with: anInteger.
! !
!GsMethod2 categoriesFor: #<=!public! !
!GsMethod2 categoriesFor: #category!public! !
!GsMethod2 categoriesFor: #classCategory!public! !
!GsMethod2 categoriesFor: #clearBreakAtStepPoint:!Breakpoints!public! !
!GsMethod2 categoriesFor: #gsClass!public! !
!GsMethod2 categoriesFor: #initialize:!public! !
!GsMethod2 categoriesFor: #isTestCase!public! !
!GsMethod2 categoriesFor: #printOn:!public! !
!GsMethod2 categoriesFor: #setBreakAtStepPoint:!Breakpoints!public! !

MethodListPresenter guid: (GUID fromString: '{D704C96D-6D80-444E-8A76-AC5B0124BC86}')!
MethodListPresenter comment: ''!
!MethodListPresenter categoriesForClass!Unclassified! !
!MethodListPresenter methodsFor!

allowedToDeleteMethods

	^methodListPresenter selections notEmpty and: [classListPresenter view ~= DeafObject new and: [classListPresenter selection == classListPresenter list last]].
!

anyMethod

	^methodListPresenter list any.
!

browseClass

	| selection browserClass |
	selection := methodListPresenter selection.
	browserClass := Smalltalk at: #JadeSystemBrowser.		"Avoid circular dependencies on load order"
	(browserClass showOn: self gciSession)
		selectClass: selection gsClass name
		selector: selection name.
!

browseImplementors

	super browseImplementorsOf: methodListPresenter selection.
!

browseImplementorsOf

	self withSelectorDo: [:aSelector | self browseImplementorsOf: aSelector].
!

browseMethodsContaining

	| string |
	(string := Prompter prompt: 'Enter substring:') isNil ifTrue: [^self].
	(JadeMethodListBrowser showOn: self gciSession) browseMethodsContaining: string.
!

browseSenders

	super browseSendersOf: methodListPresenter selection.
!

browseSendersOf

	self withSelectorDo: [:aSelector | self browseSendersOf: aSelector].
!

createComponents

	super createComponents.
	classListPresenter 			:=  self add: ListPresenter		new name: 'classList'.
	methodListPresenter 		:=  self add: ListPresenter		new name: 'methodList'.
	inheritanceListPresenter 	:=  self add: ListPresenter		new name: 'inheritanceList'.
!

createSchematicWiring

	super createSchematicWiring.
	methodListPresenter			when: #actionPerformed	 	send: #browseClass			to: self.

	classListPresenter				when: #selectionChanging: 	send: #selectionChanging:	to: self.
	methodListPresenter			when: #selectionChanging: 	send: #selectionChanging:	to: self.
	inheritanceListPresenter	when: #selectionChanging: 	send: #selectionChanging:	to: self.

	classListPresenter 			when: #'selectionChanged' 	send: #'trigger:'								to: self 	with: #'classSelectionChanged'.
	methodListPresenter 		when: #'selectionChanged' 	send: #'updateInheritanceList'		to: self.
	inheritanceListPresenter 	when: #'selectionChanged' 	send: #'methodSelectionChanged' 	to: self .

	methodListPresenter			when: #labelOf:editedTo:accept: 	send: #labelOf:editedTo:accept:	to: self.

	self createDragDropSchematicWiringFor: methodListPresenter.
!

list: aList

	methodListPresenter list: aList.
!

methodSelectionChanged

	self trigger: #'methodSelectionChanged'.
!

onDrag: aSession 

	| list |
	list := methodListPresenter selections collect: [:each | 
		(aSession newDragObject: each)
			format: #method data: each;
			yourself.
	].
	aSession 
		dragObjects: list;
		supportedOperations: #(#move #copy);
		defaultOperation: #copy;
		yourself.
!

primaryPresenter

	^methodListPresenter.
!

queryCommand: aCommandQuery

	(#(#'browseClass' #'browseImplementors' #'browseSenders') includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery 
			isEnabled: methodListPresenter selections size = 1;
			receiver: self;
			yourself.
		^true.
	].
	(#(#'removeMethods' ) includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: self allowedToDeleteMethods.
		^true.
	].
	(#(#'runTests') includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: (methodListPresenter selections notEmpty and: [
			methodListPresenter selections allSatisfy: [:each | each isTestCase] ]).
		^true.
	].
	(#(#'removeFromList') includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: methodListPresenter selections notEmpty.
		^true.
	].
	^super queryCommand: aCommandQuery.
!

removeFromList

	methodListPresenter selections copy do: [:each | 
		methodListPresenter model removeAtIndex: (methodListPresenter model list indexOf: each).
	].
!

removeMethods

	Cursor wait showWhile: [ 
		methodListPresenter selections do: [:each | 
			self gciSession
				serverPerform: #'removeMethod:'
				with: each.
		].
	].
	self update.
!

runTests

	| list |
	list := methodListPresenter selections.
	list do: [:each | 
		self gciSession 
			serverPerform: #'runAsTest:'
			with: each.
	].
	MessageBox notify: list size printString , ' test(s) passed!!'.
!

selectedClass

	| myClass |
	(myClass := inheritanceListPresenter selectionOrNil) notNil ifTrue: [^myClass].
	(myClass 	:= self trigger: #'needClass') isNil ifTrue: [^nil].
	(self trigger: #'needIsMeta') ifFalse: [^myClass].
	^(myClass superclassListForMeta: true) last.
!

selectedClasses

	| index list |
	classListPresenter view = DeafObject new ifTrue: [^nil].
	classListPresenter hasSelection ifFalse: [^nil].
	index := classListPresenter selectionByIndex.
	list := classListPresenter list copyFrom: index to: classListPresenter list size.
	^list.
!

selectedMethod

	^methodListPresenter selectionOrNil.
!

selectedMethodClass

	^inheritanceListPresenter selectionOrNil.
!

selectFirstMethod

	methodListPresenter list notEmpty ifTrue: [
		methodListPresenter selectionByIndex: 1.
	].
!

selectionOrNil

	^methodListPresenter selectionOrNil.
!

subMenuName

	^'&Methods'.
!

subMenuPresenter

	^methodListPresenter.
!

update

	self 
		updateSelecting: nil 
		inClass: nil.
!

updateClassList

	Cursor wait showWhile: [
		| myClass isMeta list |
		classListPresenter list: #().
		(myClass 	:= self trigger: #'needClass') isNil ifTrue: [^self].
		isMeta 		:= self trigger: #'needIsMeta'.
		list := myClass superclassListForMeta: isMeta.
		classListPresenter 
			list: list;
			selection: list last;
			yourself.
	].
!

updateInheritanceList

	| myClass gsMethod list |
	inheritanceListPresenter list: #().
	(gsMethod := methodListPresenter selectionOrNil) isNil ifTrue: [
		^self methodSelectionChanged.
	].
	(list := classListPresenter list) isEmpty ifTrue: [
		^self methodSelectionChanged.
	].
	myClass := list last.
	list := myClass implementorsOf: gsMethod.
	list := list reverse.
	inheritanceListPresenter
		list: list;
		selection: list last;
		yourself.
!

updateSelecting: aSymbol

	| classList |
	(classList := self selectedClasses) isNil ifTrue: [
		methodListPresenter selectionOrNil: nil.
		^self.
	].
	Cursor wait showWhile: [ 
		| isVariables filterList list newMethod |
		methodListPresenter list: #().
		inheritanceListPresenter list: #().
		isVariables 	:= self trigger: #'needIsVariables'.
		filterList		:= self trigger: #'needFilterList'.
		list := classList last
			methodsUpTo: classList first 
			filterList: filterList 
			isVariables: isVariables.
		newMethod := list 
			detect: [:each | each name = aSymbol]
			ifNone: [nil].
		methodListPresenter 
			list: list asSortedCollection;
			selectionOrNil: newMethod;
			yourself.
	].
!

updateSelecting: aSymbol inClass: aClass

	| classList newMethod |
	methodListPresenter list: #().
	inheritanceListPresenter list: #().
	(classList := self selectedClasses) isNil ifTrue: [
		newMethod := methodListPresenter list
			detect: [:each | each gsClass = aClass and: [each name = aSymbol]]
			ifNone: [nil].
		methodListPresenter selectionOrNil: newMethod.
		^self.
	].
	Cursor wait showWhile: [ 
		| isVariables filterList list |
		isVariables 	:= self trigger: #'needIsVariables'.
		filterList		:= self trigger: #'needFilterList'.
		list := classList last
			methodsUpTo: classList first 
			filterList: filterList 
			isVariables: isVariables.
		newMethod := list 
			detect: [:each | each name = aSymbol]
			ifNone: [nil].
		methodListPresenter 
			list: list asSortedCollection;
			selectionOrNil: newMethod;
			yourself.
		newMethod isNil ifTrue: [self trigger: #methodSelectionChanged].
	].
!

withSelectorDo: aBlock

	| selector result list |
	(selector := Prompter prompt: 'Enter selector:') isNil ifTrue: [^self].
	selector := selector reject: [:each | each = Character space].
	(selector includes: $*) ifFalse: [
		aBlock value: selector.
		^self.
	].
	result := self model 
		serverPerform: #selectorsMatching:
		with: selector.
	result isNil ifTrue: [^self].
	list := result subStrings: Character lf.
	(selector := ChoicePrompter choices: list) isNil ifTrue: [^self].
	aBlock value: selector.
! !
!MethodListPresenter categoriesFor: #allowedToDeleteMethods!public! !
!MethodListPresenter categoriesFor: #anyMethod!public! !
!MethodListPresenter categoriesFor: #browseClass!public! !
!MethodListPresenter categoriesFor: #browseImplementors!public! !
!MethodListPresenter categoriesFor: #browseImplementorsOf!public! !
!MethodListPresenter categoriesFor: #browseMethodsContaining!public! !
!MethodListPresenter categoriesFor: #browseSenders!public! !
!MethodListPresenter categoriesFor: #browseSendersOf!public! !
!MethodListPresenter categoriesFor: #createComponents!public! !
!MethodListPresenter categoriesFor: #createSchematicWiring!drag & drop!public! !
!MethodListPresenter categoriesFor: #list:!public! !
!MethodListPresenter categoriesFor: #methodSelectionChanged!drag & drop!public! !
!MethodListPresenter categoriesFor: #onDrag:!drag & drop!public! !
!MethodListPresenter categoriesFor: #primaryPresenter!drag & drop!label edit!public! !
!MethodListPresenter categoriesFor: #queryCommand:!public! !
!MethodListPresenter categoriesFor: #removeFromList!menus!public! !
!MethodListPresenter categoriesFor: #removeMethods!public! !
!MethodListPresenter categoriesFor: #runTests!public! !
!MethodListPresenter categoriesFor: #selectedClass!public! !
!MethodListPresenter categoriesFor: #selectedClasses!public! !
!MethodListPresenter categoriesFor: #selectedMethod!public! !
!MethodListPresenter categoriesFor: #selectedMethodClass!public! !
!MethodListPresenter categoriesFor: #selectFirstMethod!public! !
!MethodListPresenter categoriesFor: #selectionOrNil!public! !
!MethodListPresenter categoriesFor: #subMenuName!menus!public! !
!MethodListPresenter categoriesFor: #subMenuPresenter!menus!public! !
!MethodListPresenter categoriesFor: #update!public! !
!MethodListPresenter categoriesFor: #updateClassList!public! !
!MethodListPresenter categoriesFor: #updateInheritanceList!public! !
!MethodListPresenter categoriesFor: #updateSelecting:!public! !
!MethodListPresenter categoriesFor: #updateSelecting:inClass:!public! !
!MethodListPresenter categoriesFor: #withSelectorDo:!public! !

!MethodListPresenter class methodsFor!

publishedEventsOfInstances
    
    	^super publishedEventsOfInstances
			add: #'classSelectionChanged';
			add: #'methodSelectionChanged';
			add: #'needFilterList';
			add: #'needIsMeta';
			add: #'needIsVariables';
			add: #'needClassList';
			yourself.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 6 410 8 ##(Smalltalk.ComboBox)  98 17 0 416 98 2 8 1144063491 1025 560 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 560 0 8 4294903045 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 800 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] 8 #name 816 7 257 0 688 401 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 455 1058 701 47 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 227 0 0 0 94 1 0 0 250 0 0 0] 98 0 1058 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 1218 8 #fixedParentRight 1 1218 8 #fixedParentBottom -45 1218 8 #fixedViewTop 47 410 576 98 17 0 416 98 2 8 1144063491 1025 1360 642 202 208 688 0 720 738 768 0 5 0 0 0 1360 0 8 4294903045 802 0 0 834 2 1 800 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] 912 1488 7 257 0 688 401 930 202 208 98 1 994 1024 98 2 1058 1 1 1058 701 47 1360 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 23 0 0 0] 98 0 1168 0 27 1186 1232 1 1264 1 1218 8 #fixedParentTop 1 1328 47 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140970057 1025 1776 642 202 208 688 0 720 738 8 4278190080 0 13 265030 4 ##(Smalltalk.Menu)  0 16 98 9 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #browseImplementors 8 'Browse &Implementors' 1 1 0 0 0 1970 1 2002 8 #browseImplementorsOf 8 'Browse Implementors of ...' 1 1 0 0 0 1970 1 2002 8 #browseSenders 8 'Browse &Senders' 1 1 0 0 0 1970 1 2002 8 #browseSendersOf 8 'Browse Senders of ...' 1 1 0 0 0 1970 1 2002 8 #browseMethodsContaining 8 'Browse Methods Containing ...' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 1970 1 2002 8 #removeMethods 8 '&Delete Method(s)' 1 1 0 0 0 1970 1 2002 8 #rename 8 '&Rename' 1251 1 0 0 0 1970 1 2002 8 #runTests 8 'Run &Test(s)' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 1776 0 8 4294903113 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 'Methods' 693 8 #left 2578 2608 2624 8 ##(Smalltalk.SortedCollection)  802 0 0 834 2 1 800 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] 912 2800 7 257 0 0 1776 0 3 0 0 8 #report 688 0 131137 0 0 930 202 208 98 3 994 1024 98 2 1058 1 47 1058 701 409 1776 994 8 #contextMenu: 98 1 1936 1776 994 8 #text: 98 1 8 'Methods' 1776 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 94 1 0 0 227 0 0 0] 98 0 1168 0 27 1186 1232 1 1264 1 1744 47 1296 -45 234 256 98 6 560 8 'inheritanceList' 1360 8 'classList' 1776 8 'methodList' 0 930 202 208 98 1 994 1024 98 2 1058 3359 21 1058 701 501 416 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 143 6 0 0 10 0 0 0 237 7 0 0 4 1 0 0] 98 3 1360 1776 560 1168 0 27 )!

resource_MultiClass_view
	"Answer the literal data from which the 'MultiClass view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_MultiClass_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 2 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920905 1025 592 590662 2 ##(Smalltalk.ListModel)  202 208 544 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  524550 ##(Smalltalk.ColorRef)  8 4278190080 0 13 265030 4 ##(Smalltalk.Menu)  0 16 98 12 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #removeFromList 8 'Remove from &List' 1 1 0 0 0 850 1 882 8 #browseClass 8 'Browse Class' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 850 1 882 8 #browseImplementors 8 'Browse &Implementors' 1 1 0 0 0 850 1 882 8 #browseImplementorsOf 8 'Browse Implementors of ...' 1 1 0 0 0 850 1 882 8 #browseSenders 8 'Browse &Senders' 1 1 0 0 0 850 1 882 8 #browseSendersOf 8 'Browse Senders of ...' 1 1 0 0 0 850 1 882 8 #browseMethodsContaining 8 'Browse Methods Containing ...' 1 1 0 0 0 1010 4097 850 1 882 8 #removeMethods 8 '&Delete Method(s)' 1 1 0 0 0 850 1 882 8 #rename 8 '&Rename' 1251 1 0 0 0 850 1 882 8 #runTests 8 'Run &Test(s)' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 592 0 8 4294902785 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 4 920646 5 ##(Smalltalk.ListViewColumn)  8 'Class Category' 299 8 #left 1602 1632 98 0 1602 8 #<= 1808 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  3 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each gsClass category]' 8 #[31 105 226 0 159 106] 8 #gsClass 8 #category 1872 7 257 0 0 592 0 3 0 0 1730 8 'Class' 299 1776 1602 1632 98 0 1602 1840 2064 1858 0 0 1890 3 1 1856 8 'doIt' 8 '[:each | each gsClass name]' 8 #[31 105 226 0 159 106] 1984 8 #name 2096 7 257 0 0 592 0 3 0 0 1730 8 'Method Category' 299 1776 1602 1632 1808 1602 1840 1808 1858 0 0 1890 2 1 1920 8 'doIt' 8 '[:each | each category]' 8 #[30 105 226 0 106] 2000 2256 7 257 0 0 592 0 3 0 0 1730 8 'Method' 299 1776 1602 1632 1648 8 ##(Smalltalk.SortedCollection)  1858 0 0 1890 2 1 1856 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] 2176 2400 7 257 0 0 592 0 3 0 0 8 #report 544 0 131169 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 2626 1201 401 592 2562 8 #contextMenu: 98 1 816 592 2562 8 #text: 98 1 8 'Class Category' 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 200 0 0 0] 98 0 2626 193 193 0 27 8 'methodList' 0 2498 202 208 98 1 2562 2592 98 2 2626 5119 21 2626 1201 401 416 2786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 210 0 0 0] 98 1 592 2848 0 27 )! !
!MethodListPresenter class categoriesFor: #publishedEventsOfInstances!public! !
!MethodListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !
!MethodListPresenter class categoriesFor: #resource_MultiClass_view!public!resources-views! !

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

JadeMethodListPresenter guid: (GUID fromString: '{BCF7EDB6-114A-4E67-84D3-6FECD6C5AE0C}')!
JadeMethodListPresenter comment: ''!
!JadeMethodListPresenter categoriesForClass!Unclassified! !
!JadeMethodListPresenter methodsFor!

browse: performSelector gsMethod: aGsMethod

	| string |
	string := self gciSession 
		serverPerform: performSelector
		with: aGsMethod.
	self browseMethodsFromString: string.
!

browse: performSelector method: aGsMethodOrString

	| string |
	(aGsMethodOrString isKindOf: String) ifTrue: [
		self browse: performSelector methodSelector: aGsMethodOrString.
		string := aGsMethodOrString.
	] ifFalse: [
		self browse: performSelector gsMethod: aGsMethodOrString.
		string := aGsMethodOrString name.
	].
	self selecting: string.
!

browse: performSelector methodSelector: aString

	| string |
	string := self gciSession 
		serverPerform: performSelector
		with: aString.
	self browseMethodsFromString: string.
!

browseImplementorsOf: aGsMethod

	self
		browse: #'implementorsOf:' 
		method: aGsMethod.
!

browseMethodsContaining: aString

	self
		browse: #'methodsContaining:' 
		method: aString.
!

browseMethodsFromString: aString

	| list |
	list := GsMethod2
		listFromString: aString
		session: self gciSession.
	methodListPresenter list: list asSortedCollection.
	list notEmpty ifTrue: [
		methodListPresenter selectFirstMethod.
	].
!

browseSendersOf: aGsMethodOrString

	self
		browse: #'sendersOf:' 
		method: aGsMethodOrString.
!

codePresenterIsMethod

	^true!

createComponents

	super createComponents.
	methodListPresenter 				:= self add: MethodListPresenter					new name: 'methodList'.
	methodSourcePresenter				:= (self add: MethodSourcePresenter				new name: 'methodSource') 			menuTitle: '&Method Source'; yourself.
!

createSchematicWiring

	super createSchematicWiring.
	methodListPresenter
		when: #'methodSelectionChanged'		send: #'updateSource'			to: self;
		when: #'selectionChanging:'				send: #'selectionChanging:'		to: self;
		yourself.
	methodSourcePresenter
		when: #'needMethodCategory'			send: #'selectedCategory'		to: self;
		when: #'needClass'							send: #'selectedClass'			to: self;
		when: #'needMethodClass'				send: #'selectedClass'			to: self;
		when: #'needMethod'						send: #'selectedMethod'			to: methodListPresenter;
		when: #'needUser'							send: #'userSelection'			to: self;
		when: #'savedMethod:inClass:' 			send: #'doSearch'					to: self;
		yourself.
!

doSearch

	searchBlock notNil ifTrue: [searchBlock value].
!

jadeMenuStrings

	^#(
		'&Jade'
		'&Abort Transaction//abortTransaction'
		'&CommitTransaction//commitTransaction'
		'-'
		'&New Worspace/Ctrl+N/newWorkspace'
		'&Open System Browser/Ctrl+B/openSystemBrowser'
		'-'
		'E&xit/Shift+F4/exit'
	).
!

model: aGciSession

	super model: aGciSession.
	methodListPresenter model: aGciSession .
	methodSourcePresenter model: aGciSession.
!

onViewOpened

	super onViewOpened.
	[methodSourcePresenter addMenu] forkAt: Processor userBackgroundPriority.
!

searchFor: aString

	| sourceView selectionRange textRange found |
	sourceView := methodSourcePresenter view viewNamed: 'document'.
	selectionRange := sourceView selectionRange.
	textRange := sourceView textRange.
	found := sourceView
		find: aString
		range: (selectionRange stop + 1 to: textRange stop)
		flags: 4 + 2. "SCFIND_MATCHCASE + SCFIND_WHOLEWORD"
	0 < found stop ifTrue: [
		sourceView
			selectionStart: found start 
			end: found stop.
		sourceView ensureVisible: found stop.
	].
!

selectedCategory

	| method |
	(method := methodListPresenter selectedMethod) notNil ifTrue: [
		^method category.
	].
	^'as yet unclassified'.
!

selectedClass

	| gsMethod |
	(gsMethod := methodListPresenter selectionOrNil) isNil ifTrue: [^nil].
	^gsMethod gsClass.
!

selectFirstMethod

	methodListPresenter selectFirstMethod.
!

selecting: aString

	searchBlock := [self searchFor: aString].
	self doSearch.
!

updateMenuBar: aMenuBar

	| menu |
	menu := Menu fromStrings: self jadeMenuStrings.
	aMenuBar addItem: menu.
	methodListPresenter updateMenuBar: aMenuBar.
!

updateSource

	methodSourcePresenter update.
	self doSearch.

	methodListPresenter selectedMethod ifNil: [^self].
	methodSourcePresenter documentPresenter lastGsShape: (self registry getClass: methodListPresenter selectedMethod gsClass name).! !
!JadeMethodListPresenter categoriesFor: #browse:gsMethod:!public! !
!JadeMethodListPresenter categoriesFor: #browse:method:!public! !
!JadeMethodListPresenter categoriesFor: #browse:methodSelector:!public! !
!JadeMethodListPresenter categoriesFor: #browseImplementorsOf:!public! !
!JadeMethodListPresenter categoriesFor: #browseMethodsContaining:!public! !
!JadeMethodListPresenter categoriesFor: #browseMethodsFromString:!public! !
!JadeMethodListPresenter categoriesFor: #browseSendersOf:!public! !
!JadeMethodListPresenter categoriesFor: #codePresenterIsMethod!public! !
!JadeMethodListPresenter categoriesFor: #createComponents!public! !
!JadeMethodListPresenter categoriesFor: #createSchematicWiring!public! !
!JadeMethodListPresenter categoriesFor: #doSearch!public! !
!JadeMethodListPresenter categoriesFor: #jadeMenuStrings!public! !
!JadeMethodListPresenter categoriesFor: #model:!public! !
!JadeMethodListPresenter categoriesFor: #onViewOpened!public! !
!JadeMethodListPresenter categoriesFor: #searchFor:!public! !
!JadeMethodListPresenter categoriesFor: #selectedCategory!public! !
!JadeMethodListPresenter categoriesFor: #selectedClass!public! !
!JadeMethodListPresenter categoriesFor: #selectFirstMethod!public! !
!JadeMethodListPresenter categoriesFor: #selecting:!public! !
!JadeMethodListPresenter categoriesFor: #updateMenuBar:!public! !
!JadeMethodListPresenter categoriesFor: #updateSource!public! !

!JadeMethodListPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 4 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 560 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 560 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.MethodListPresenter)  8 #resource_MultiClass_view 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 882 1201 351 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 175 0 0 0] 98 0 882 193 193 0 27 9 410 576 98 14 0 416 98 2 8 1140850688 131073 1008 0 642 672 0 5 0 0 0 1008 690 8 ##(Smalltalk.MethodSourcePresenter)  8 #resource_Default_view 0 754 202 208 98 1 818 848 98 2 882 1 361 882 1201 441 1008 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 180 0 0 0 88 2 0 0 144 1 0 0] 976 992 0 27 11 16 234 256 98 4 560 8 'methodList' 1008 8 'methodSource' 0 754 202 208 98 1 818 848 98 2 882 2799 21 882 1201 801 416 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 5 0 0 10 0 0 0 207 7 0 0 154 1 0 0] 98 3 560 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 1504 0 642 8 4278190080 0 517 0 0 0 1504 754 202 208 98 1 818 848 98 2 882 1 351 882 1201 11 1504 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 175 0 0 0 88 2 0 0 180 0 0 0] 98 0 992 0 27 1008 992 0 27 )! !
!JadeMethodListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

JadeMethodHistoryBrowser guid: (GUID fromString: '{1A2BEFC8-8219-4AE2-BD65-318E08DAC9BF}')!
JadeMethodHistoryBrowser comment: ''!
!JadeMethodHistoryBrowser categoriesForClass!Unclassified! !
!JadeMethodHistoryBrowser methodsFor!

createComponents

	super createComponents.
	versionListPresenter 		:= self add: ListPresenter		new name: 'versionList'.
	editorPresenter				:= self add: TextPresenter		new name: 'editor'.

!

createSchematicWiring

	super createSchematicWiring.
	versionListPresenter when: #'selectionChanged' send: #'fillMethodSource' to: self.
!

fillMethodSource

	editorPresenter value: (versionListPresenter selection at: 3).
!

setContents: aReadStream

	| versions | 
	versions := OrderedCollection new.
	[
		aReadStream atEnd not.
	] whileTrue: [
		| category timeStamp writeStream |
		category := aReadStream upTo: Character tab.
		timeStamp := aReadStream upTo: Character tab.
		writeStream := WriteStream on: String new.
		writeStream nextPutAll: aReadStream nextLine; lf.
		[
			aReadStream peekFor: $%.
		] whileFalse: [
			writeStream nextPutAll: aReadStream nextLine; lf.
		].
		aReadStream nextLine.
		versions add: (Array with: category with: timeStamp with: writeStream contents).
	].
	versionListPresenter 
		list: versions;
		selection: versions first;
		yourself.
!

shellName

	^'Jade Method History Browser'.
! !
!JadeMethodHistoryBrowser categoriesFor: #createComponents!public! !
!JadeMethodHistoryBrowser categoriesFor: #createSchematicWiring!public! !
!JadeMethodHistoryBrowser categoriesFor: #fillMethodSource!public! !
!JadeMethodHistoryBrowser categoriesFor: #setContents:!public! !
!JadeMethodHistoryBrowser categoriesFor: #shellName!public! !

!JadeMethodHistoryBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 98 4 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1409355853 1025 656 590662 2 ##(Smalltalk.ListModel)  202 208 608 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 656 0 8 4294903981 459270 ##(Smalltalk.Message)  8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Category' 501 8 #left 866 896 912 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 1120 7 257 0 0 656 0 1 0 0 1010 8 'Timestamp' 501 1056 866 896 608 866 8 #<= 608 1106 0 0 1138 1 83886081 1168 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 1312 7 257 0 0 656 0 1 0 0 8 #report 608 0 131169 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 1169 353 656 1474 8 #text: 98 1 8 'Category' 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 176 0 0 0] 98 0 530 193 193 0 27 8 'versionList' 410 8 ##(Smalltalk.ScintillaView)  98 46 0 416 98 2 8 1445007428 1025 1728 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 482 8 4278190080 0 7 0 0 0 1728 0 8 4294904083 852486 ##(Smalltalk.NullConverter)  0 0 11 0 234 256 98 2 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 1984 0 0 0 98 40 2016 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  1984 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 786694 ##(Smalltalk.IndexedColor)  33554433 2162 33554471 1728 8 #circle 202 208 608 0 63 9215 0 0 0 0 2162 33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 1952 0 0 0 0 1 0 234 256 98 6 1 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  1 1728 65025 3 32 1 0 3 2354 3 1728 33423361 5 32 3 0 5 2354 5 1728 511 1 32 5 0 1410 202 208 98 8 1474 1504 98 2 530 1 371 530 1169 355 1728 1474 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1728 1474 8 #isTextModified: 98 1 32 1728 1474 8 #modificationEventMask: 98 1 9215 1728 1474 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 1728 1 3 32 1 2770 3 1728 33 1 16 67108863 2770 5 1728 1 1 16 -67108863 1728 1474 8 #indentationGuides: 98 1 0 1728 1474 8 #tabIndents: 98 1 16 1728 1474 8 #tabWidth: 98 1 9 1728 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 185 0 0 0 72 2 0 0 106 1 0 0] 98 0 1696 0 27 8 'editor' 0 0 0 0 0 1 0 0 0 0 1 0 0 1410 202 208 98 2 1474 1504 98 2 530 2879 21 530 1201 801 416 1474 8 #updateMenuBar 608 416 1634 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 98 3 656 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 3232 0 482 8 4278190080 0 519 0 0 0 3232 1410 202 208 98 1 1474 1504 98 2 530 1 353 530 1169 19 3232 1634 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 176 0 0 0 72 2 0 0 185 0 0 0] 98 0 1696 0 27 1728 1696 0 27 )! !
!JadeMethodHistoryBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

JadeMethodListBrowser guid: (GUID fromString: '{046CEBD5-F142-4382-99FF-B3FD8B8032B9}')!
JadeMethodListBrowser comment: ''!
!JadeMethodListBrowser categoriesForClass!Unclassified! !
!JadeMethodListBrowser methodsFor!

browseImplementorsOf: aGsMethod

	myPresenter browseImplementorsOf: aGsMethod.
!

browseMethodsContaining: aString

	myPresenter browseMethodsContaining: aString.
!

browseMethodsFromString: aString

	myPresenter browseMethodsFromString: aString.
!

browseSendersOf: aGsMethodOrString

	myPresenter browseSendersOf: aGsMethodOrString.
!

presenterClass

	^JadeMethodListPresenter.
!

selecting: aString

	myPresenter selecting: aString.
!

shellName

	^'Method List Browser'.
!

statusBarText: aString

	(self view viewNamed: 'statusBarField') model: (ValueHolder with: aString).
! !
!JadeMethodListBrowser categoriesFor: #browseImplementorsOf:!private! !
!JadeMethodListBrowser categoriesFor: #browseMethodsContaining:!private! !
!JadeMethodListBrowser categoriesFor: #browseMethodsFromString:!private! !
!JadeMethodListBrowser categoriesFor: #browseSendersOf:!private! !
!JadeMethodListBrowser categoriesFor: #presenterClass!overrides!private! !
!JadeMethodListBrowser categoriesFor: #selecting:!public! !
!JadeMethodListBrowser categoriesFor: #shellName!overrides!private! !
!JadeMethodListBrowser categoriesFor: #statusBarText:!private! !

!JadeMethodListBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.StatusBar)  98 18 0 416 98 2 8 1409288460 1 624 0 482 8 4278190080 0 7 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 530 193 193 0 624 0 8 4294902483 234 256 98 2 853766 ##(Smalltalk.StatusBarItem)  1 -1 624 0 459270 ##(Smalltalk.Message)  8 #displayString 98 0 914 8 #iconImageIndex 98 0 1049670 1 ##(Smalltalk.IconImageManager)  8 'statusBarField' 98 1 896 1115142 ##(Smalltalk.StatusBarNullItem)  513 1 624 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 689 530 1185 45 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 88 1 0 0 80 2 0 0 110 1 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 1394 8 #fixedParentRight 1 1394 8 #fixedParentBottom -43 1394 8 #fixedViewTop 45 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 1536 0 482 8 4278190080 0 7 0 0 0 1536 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.JadeMethodListPresenter)  8 #resource_Default_view 0 1122 202 208 98 1 1186 1216 98 2 530 1 1 530 1185 689 1536 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 80 2 0 0 88 1 0 0] 98 0 1344 0 27 1362 1408 1 1440 1 1394 8 #fixedParentTop 1 1472 -43 234 256 98 2 624 8 'statusBar' 0 0 0 0 0 1 0 0 0 0 1 0 0 1122 202 208 98 3 1186 1216 98 2 530 2879 21 530 1201 801 416 1186 8 #text: 98 1 8 'Jade Method List Browser' 416 1186 8 #updateMenuBar 1856 416 1282 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 98 2 1536 624 1344 0 27 )! !
!JadeMethodListBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

