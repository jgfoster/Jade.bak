| package |
package := Package name: 'Jade System Browser'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.259'.


package classNames
	add: #JadeAutoSystemBrowserPresenter;
	add: #JadeFindClassDialog;
	add: #JadeSystemBrowser;
	add: #JadeSystemBrowserPresenter;
	add: #SUnitResultDialog;
	yourself.

package methodNames
	add: #JadeBrowserPresenter -> #newMethodPresenter;
	add: #JadeServer -> #addAccessorsFor:inBehavior:;
	add: #JadeServer -> #assignClass:toCategory:;
	add: #JadeServer -> #class:includesSelector:;
	add: #JadeServer -> #classesForUser:;
	add: #JadeServer -> #commentFor:;
	add: #JadeServer -> #currentUserMayEditMethod:;
	add: #JadeServer -> #dictionaryAndSymbolOf:;
	add: #JadeServer -> #historyOf:;
	add: #JadeServer -> #isPackagePolicyEnabled;
	add: #JadeServer -> #millisecondsElapsedTime:;
	add: #JadeServer -> #moveClassesInDictionary:category:to:;
	add: #JadeServer -> #moveDictionary:toBefore:forUser:;
	add: #JadeServer -> #nextLine;
	add: #JadeServer -> #nextLineAsList;
	add: #JadeServer -> #objectSecurityPolicyFor:;
	add: #JadeServer -> #removeDictionary:fromUser:;
	add: #JadeServer -> #removeKey:fromSymbolDictionary:;
	add: #JadeServer -> #sbAddDictionary:;
	add: #JadeServer -> #sbAddMethodCategory:;
	add: #JadeServer -> #sbAddMissingAccessors:;
	add: #JadeServer -> #sbAddNameOf:;
	add: #JadeServer -> #sbAddPackage:;
	add: #JadeServer -> #sbAddRepository:;
	add: #JadeServer -> #sbBreak:;
	add: #JadeServer -> #sbBrowseClassReferences:;
	add: #JadeServer -> #sbBrowseGlobalReferences:;
	add: #JadeServer -> #sbBrowseImplementors:;
	add: #JadeServer -> #sbBrowseMethodHistory:;
	add: #JadeServer -> #sbBrowseMethodsContaining:;
	add: #JadeServer -> #sbBrowseSenders:;
	add: #JadeServer -> #sbChangeClassName:;
	add: #JadeServer -> #sbChangesInPackage:;
	add: #JadeServer -> #sbCheckUniqueClassName:;
	add: #JadeServer -> #sbClass:;
	add: #JadeServer -> #sbClassCategory:;
	add: #JadeServer -> #sbClassComment:;
	add: #JadeServer -> #sbClassesToDictionary:;
	add: #JadeServer -> #sbClassFrom:;
	add: #JadeServer -> #sbClassTemplate;
	add: #JadeServer -> #sbComparePackages:;
	add: #JadeServer -> #sbCopyMethodsFor:;
	add: #JadeServer -> #sbFileOutClass:;
	add: #JadeServer -> #sbFileOutDictionary:;
	add: #JadeServer -> #sbFindClass;
	add: #JadeServer -> #sbFindClassPackageMap;
	add: #JadeServer -> #sbFindSelectors:;
	add: #JadeServer -> #sbInstVarsOldParent:newParent:oldChild:;
	add: #JadeServer -> #sbLoadLatestVersionOfConfiguration:;
	add: #JadeServer -> #sbMethodCategory:;
	add: #JadeServer -> #sbMethodClass:;
	add: #JadeServer -> #sbMigrateAll:;
	add: #JadeServer -> #sbNextParagraph;
	add: #JadeServer -> #sbPostSaveClass:;
	add: #JadeServer -> #sbRecompileSubclassesOf:andCopyMethods:;
	add: #JadeServer -> #sbRemoveClasses;
	add: #JadeServer -> #sbRemoveDictionaries:;
	add: #JadeServer -> #sbRemoveGlobals;
	add: #JadeServer -> #sbRemoveHistory:;
	add: #JadeServer -> #sbRemoveMethodCategories:;
	add: #JadeServer -> #sbRemoveMethods:;
	add: #JadeServer -> #sbRemovePriorVersions;
	add: #JadeServer -> #sbRemoveRepository:;
	add: #JadeServer -> #sbRevertClass;
	add: #JadeServer -> #sbRunClassTests:;
	add: #JadeServer -> #sbRunMethodTests:;
	add: #JadeServer -> #sbSavePackage:;
	add: #JadeServer -> #sbSetHomeDictionary:;
	add: #JadeServer -> #sbUniqueVersionName:;
	add: #JadeServer -> #sbUnloadPackage:;
	add: #JadeServer -> #sbUpdateClassCategories;
	add: #JadeServer -> #sbUpdateClasses;
	add: #JadeServer -> #sbUpdateClassHierarchy;
	add: #JadeServer -> #sbUpdateClassInfo;
	add: #JadeServer -> #sbUpdateClassList;
	add: #JadeServer -> #sbUpdateDictionaries;
	add: #JadeServer -> #sbUpdateMethod:;
	add: #JadeServer -> #sbUpdateMethodBreakPointsFor:;
	add: #JadeServer -> #sbUpdateMethodCategories;
	add: #JadeServer -> #sbUpdateMethodFilter;
	add: #JadeServer -> #sbUpdateMethodFilterSelections;
	add: #JadeServer -> #sbUpdateMethodInheritedImplementationsOf:;
	add: #JadeServer -> #sbUpdateMethods;
	add: #JadeServer -> #sbUpdateMethodsByCategories;
	add: #JadeServer -> #sbUpdateMethodsByVariables;
	add: #JadeServer -> #sbUpdateMethodSelectionsIn:;
	add: #JadeServer -> #sbUpdateMethodStepPointsFor:;
	add: #JadeServer -> #sbUpdateMethodVariables;
	add: #JadeServer -> #sbUpdatePackage:;
	add: #JadeServer -> #sbUpdatePackages;
	add: #JadeServer -> #sbUpdatePackagesOrDictionaries;
	add: #JadeServer -> #sbUpdateSuperclass;
	add: #JadeServer -> #symbolList;
	add: #JadeServer -> #systemBrowser:;
	add: #JadeServer -> #systemBrowserA:;
	add: #JadeServer -> #systemBrowserCommand;
	add: #JadeServer -> #systemBrowserUpdate;
	add: #JadeServer -> #writeList:;
	add: #JadeServer32bit -> #systemBrowser:;
	add: #JadeServer64bit -> #systemBrowser:;
	add: #JadeServer64bit32 -> #dictionaryAndSymbolOf:;
	add: #JadeServer64bit3x -> #objectSecurityPolicyFor:;
	add: #JadeServer64bit3x -> #sbClassComment:;
	add: #JadeServer64bit3x -> #sbMethod:;
	add: #JadeServer64bit3x -> #sbUpdateMethodBreakPointsFor:;
	add: #JadeServer64bit3x -> #sbUpdateMethodStepPointsFor:;
	add: #JadeTextDocument -> #jadeBrowseClasses;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Views\Cards\Dolphin Card Containers';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter';
	add: '..\Object Arts\Dolphin\MVP\Views\Scintilla\Dolphin Scintilla View';
	add: '..\Object Arts\Dolphin\MVP\Views\Sliding Tray\Dolphin Slidey-Inney-Outey Thing';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Tree\Dolphin Tree Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'GemStone Session';
	add: 'Jade Autocompletation';
	add: 'Jade Class Browser';
	add: 'Jade Inspector';
	add: 'Jade Method Browser';
	add: 'Jade UI';
	add: 'Jade UI Base';
	add: 'Monticello';
	yourself).

package!

"Class Definitions"!

JadeBrowserPresenter subclass: #JadeSystemBrowserPresenter
	instanceVariableNames: 'ancestorListPresenter breakPoints categoryListPresenter categoryVariableTabs classCategoryPresenter classCommentPresenter classDefinition classDefinitionPresenter classHierarchyPresenter classHierarchyTabs classListPresenter dictionaryListPresenter eventCount globalsPresenter ignoreNextSetFocusEvent instanceClassTabs inUpdate keystrokeTime methodCategory methodListPresenter methodSource methodSourcePresenter originalSourcePresenter overrideListPresenter packageDictionaryTabs packageInfoTab packageListPresenter readStream repositoryListPresenter selectedClassChanged selectedClassesAreTestCases selectedClassName selectedClassOop stepPoints superclassListPresenter textAreaTabs updateCount updateProcess variableListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSystemBrowserPresenter subclass: #JadeAutoSystemBrowserPresenter
	instanceVariableNames: 'lastGsShape'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeValueDialog subclass: #JadeFindClassDialog
	instanceVariableNames: 'classListPresenter nameEntryPresenter availableClasses'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeValueDialog subclass: #SUnitResultDialog
	instanceVariableNames: 'listPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeShell subclass: #JadeSystemBrowser
	instanceVariableNames: 'cardsPresenter roundTripCount'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JadeBrowserPresenter methodsFor!

newMethodPresenter

	^JadeTextPresenter new.
! !
!JadeBrowserPresenter categoriesFor: #newMethodPresenter!public! !

!JadeServer methodsFor!

addAccessorsFor: aString inBehavior: aBehavior

	aBehavior compileAccessingMethodsFor: (Array with: aString asSymbol).
!

assignClass: aClass toCategory: aString

	aClass thisClass category: aString.
!

class: aClass includesSelector: aSelector

	^aClass includesSelector: aSelector asSymbol.
!

classesForUser: aUserProfile

	| stream |
	stream := WriteStream on: String new.
	aUserProfile symbolList do: [:eachDict |
		eachDict keysAndValuesDo: [:key :value |
			value isBehavior ifTrue: [
				stream nextPutAll: key; space; nextPutAll: value category asString; tab.
			].
		].
		stream lf.
	].
	^stream contents.
!

commentFor: aClass

	| description |
	(Class canUnderstand: #'classComment') ifTrue: [
		^aClass classComment.
	].
	(description := aClass description) isNil ifTrue: [^nil].
	(description class name = #'GsClassDocumentation') ifTrue: [^description detailsAboutClass].
	^description printString.
!

currentUserMayEditMethod: aMethod

	| objectSecurityPolicy |
	objectSecurityPolicy := self objectSecurityPolicyFor: aMethod inClass.
	^self isPackagePolicyEnabled or: [objectSecurityPolicy isNil or: [objectSecurityPolicy currentUserCanWrite]].
!

dictionaryAndSymbolOf: aClass

	^self symbolList dictionaryAndSymbolOf: aClass.
!

historyOf: aClass

	| history |
	(history := aClass classHistory) isNil ifTrue: [
		history := Array with: aClass.
	].
	^history.
!

isPackagePolicyEnabled

	^self gsPackagePolicy notNil!

millisecondsElapsedTime: aBlock

	^Time millisecondsElapsedTime: aBlock.
!

moveClassesInDictionary: sourceDictionary category: aString to: destinationDictionary

	sourceDictionary copy keysAndValuesDo: [:eachKey :eachValue | 
		(eachValue isBehavior and: [eachValue category = aString]) ifTrue: [
			sourceDictionary removeKey: eachKey.
			destinationDictionary
				at: eachKey
				put: eachValue.
		].
	].
!

moveDictionary: source toBefore: target forUser: aUserProfile

	| list |
	list := aUserProfile symbolList.
	list remove: source.
	target notNil ifTrue: [
		list
			add: source 
			before: target.
	] ifFalse: [
		list addLast: source.
	].

!

nextLine

	^readStream upTo: Character lf.
!

nextLineAsList

	^(self nextLine subStrings: Character tab) reject: [:each | each isEmpty].
!

objectSecurityPolicyFor: anObject

	^anObject segment.
!

removeDictionary: aDictionary fromUser: aUserProfile

	| symbolList index |
	symbolList := aUserProfile symbolList.
	index := symbolList indexOf: aDictionary.
	aUserProfile removeDictionaryAt: index.
!

removeKey: aString fromSymbolDictionary: aSymbolDictionary

	aSymbolDictionary removeKey: aString asSymbol.

!

sbAddDictionary: anOrderedCollection

	| currentName newName symbolList index |
	symbolList := self symbolList.
	newName := anOrderedCollection removeFirst.
	anOrderedCollection notEmpty ifTrue: [
		currentName := anOrderedCollection removeFirst asSymbol.
		index := symbolList findFirst: [:each | each name = currentName].
	] ifFalse: [
		index := symbolList size + 1.
	].
	symbolList
		createDictionaryNamed: newName
		at: index.
	self refreshSymbolList.
	selections at: #'dictionary' put: newName.
	self systemBrowserUpdate.
!

sbAddMethodCategory: anOrderedCollection

	(self sbClassFrom: anOrderedCollection) addCategory: anOrderedCollection first.
	selections at: #'methodCategory' put: anOrderedCollection first.
	self systemBrowserUpdate.
!

sbAddMissingAccessors: anOrderedCollection

	(self sbClassFrom: anOrderedCollection) compileMissingAccessingMethods.
	self systemBrowserUpdate.
!

sbAddNameOf: aClass

	writeStream nextPutAll: aClass name.
	1 < (self historyOf: aClass) size ifTrue: [
		writeStream nextPutAll: ' ('.
		((self historyOf: aClass) indexOf: aClass) printOn: writeStream.
		writeStream nextPut: $/.
		(self historyOf: aClass) size printOn: writeStream.
		writeStream nextPut: $).
	].
	writeStream tab.
!

sbAddPackage: anOrderedCollection

	| string |
	string := anOrderedCollection removeFirst.
	selections at: #'package' put: string.
	self mcWorkingCopyClass forPackage: (self mcPackageClass named: string).
	self systemBrowserUpdate.
!

sbAddRepository: list

	| description repository packages |
	description := list removeFirst.
	repository := self mcRepositoryGroup repositories detect: [:each | each description = description].
	packages := self mcWorkingCopyClass allManagers select: [:each | list includes: each package name].
	packages do: [:each | each repositoryGroup addRepository: repository].
	self systemBrowserUpdate.
!

sbBreak: anOrderedCollection

	| myClass gsMethod stepPoint |
	myClass := self sbClassFrom: anOrderedCollection.
	gsMethod := myClass compiledMethodAt: anOrderedCollection removeFirst asSymbol.
	stepPoint := anOrderedCollection removeFirst asNumber.
	anOrderedCollection removeFirst = 'set' ifTrue: [
		gsMethod setBreakAtStepPoint: stepPoint.
	] ifFalse: [
		gsMethod clearBreakAtStepPoint: stepPoint.
	].
	self systemBrowserUpdate.
!

sbBrowseClassReferences: anOrderedCollection

	| class |
	class := (self sbClassFrom: anOrderedCollection) thisClass.
	writeStream 
		nextPutAll: 'browseClassReferences'; lf;
		nextPutAll: (self referencesToObject: class); 
		yourself.
!

sbBrowseGlobalReferences: anOrderedCollection

	| global |
	global := self objectForOop: anOrderedCollection removeFirst asNumber.
	writeStream 
		nextPutAll: 'browseGlobalReferences'; lf;
		nextPutAll: (self referencesToObject: global); 
		yourself.
!

sbBrowseImplementors: anOrderedCollection

	writeStream 
		nextPutAll: 'browseImplementors'; lf;
		nextPutAll: (self implementorsOf: anOrderedCollection removeFirst);
		yourself.
!

sbBrowseMethodHistory: anOrderedCollection

	| behavior selector historyClass historyList |
	historyClass := self objectNamed: #'MethodVersionHistory'.
	historyClass isNil ifTrue: [^self].
	behavior := self sbClassFrom: anOrderedCollection.
	selector := anOrderedCollection removeFirst asSymbol.
	historyList := historyClass uniqueInstance 
		versionsOfMethod: selector 
		in: behavior.
	writeStream nextPutAll: 'browseMethodHistory'; nextPut: Character lf.
	historyList do: [:each | " behavior selector changeStamp category source"
		writeStream
			nextPutAll: each category; tab;
			nextPutAll: each changeStamp; tab;
			nextPutAll: each source;
			nextPut: Character lf;
			nextPut: $%;
			nextPut: Character lf;
			yourself.
	].
!

sbBrowseMethodsContaining: anOrderedCollection

	writeStream 
		nextPutAll: 'browseMethodsContaining'; lf;
		nextPutAll: (self methodsContaining: anOrderedCollection removeFirst);
		yourself.
!

sbBrowseSenders: anOrderedCollection

	writeStream 
		nextPutAll: 'browseSenders'; lf;
		nextPutAll: (self sendersOf: anOrderedCollection removeFirst);
		yourself.
!

sbChangeClassName: aList

	| oldName class newName changedIn |
	oldName := aList removeFirst asSymbol.
	class := self objectForOop: aList removeFirst asNumber.
	class name == oldName ifFalse: [self error: 'Current name is ' , class name printString].
	newName := aList removeFirst asSymbol.
	class changeNameTo: newName.
	changedIn := OrderedCollection new.
	self symbolList do: [:each | 
		(each includes: class) ifTrue: [
			(each at: oldName ifAbsent: [nil]) == class ifFalse: [self error: 'Class not at name!!'].
			(each includesKey: newName) ifTrue: [self error: 'Key already in use!!'].
			each
				removeKey: oldName;
				at: newName put: class;
				yourself.
			changedIn add: each.
		].
	].
	self systemBrowserUpdate.
!

sbChangesInPackage: anOrderedCollection

	| current repository patch string |
	current := self mcWorkingCopyClass forPackage: (self mcPackageClass named: anOrderedCollection removeFirst).
	repository := anOrderedCollection removeFirst.
	repository := current repositoryGroup repositories detect: [:each | each description = repository].
	patch := current changesRelativeToRepository: repository.
	patch operations isEmpty ifTrue: [current modified: false].
	string := self 
		_mcDescriptionOfPatch: patch
		baseName: 'closest ancestor'
		alternateName: nil.
	writeStream 
		nextPutAll: 'changesInPackage'; lf;
		nextPutAll: string;
		yourself.
!

sbCheckUniqueClassName: aList

	| oldName class newName |
	oldName := aList removeFirst asSymbol.
	class := self objectForOop: aList removeFirst asNumber.
	class name == oldName ifFalse: [
		writeStream nextPutAll: 'Current name is ' , class name printString. 
		^self.
	].
	newName := aList removeFirst asSymbol.
	self symbolList do: [:each | 
		((each includes: class) and: [each includesKey: newName]) ifTrue: [
			writeStream nextPutAll: 'Dictionary '.
			each name printOn: writeStream.
			writeStream nextPutAll: ' already has a global with name '.
			newName printOn: writeStream.
			^self.
		].
	].
	!

sbClass: aList

	| string newClass mcWorkingCopyClass packages dictName |
	string := aList first.
	newClass := string evaluate.
	self classOrganizer update.
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [
		packages := Array with: nil.
	] ifFalse: [
		packages := mcWorkingCopyClass allManagers collect: [:each | each package name].
		packages := packages select: [:each | (newClass category copyFrom: 1 to: (newClass category size min: each size)) = each].
		packages isEmpty ifTrue: [
			packages := Array with: nil.
		].
	].
	dictName := (newClass class canUnderstand: #'symbolDictionaryName')
		ifTrue: [newClass symbolDictionaryName]
		ifFalse: [
			| array |
			array := self dictionaryAndSymbolOf: newClass.
			array isNil
				ifTrue: ['UserGlobals']
				ifFalse: [array first name]].
	selections 
		at: #'package' 		put: packages first;
		at: #'dictionary' 		put: dictName asString;
		at: #'category' 		put: newClass category;
		at: #'className'	put: newClass name;
		at: #'class'				put: newClass;
		yourself.
	selectedClass := newClass.
	self systemBrowserUpdate.
!

sbClassCategory: aList

	| category classes |
	category := aList removeFirst.
	category := category copyFrom: 1 to: category size - 1.
	classes := aList removeFirst subStrings reject: [:each | each isEmpty].
	classes := classes collect: [:each | System myUserProfile symbolList objectNamed: each asSymbol].
	classes := classes collect: [:each | each thisClass].
	classes do: [:each | each category: category].
	self systemBrowserUpdate.
!

sbClassComment: anOrderedCollection

	| class doc txt |
	class := (self sbClassFrom: anOrderedCollection) thisClass.
	doc := (self objectNamed: #'GsClassDocumentation') newForClass: self.
	txt := (self objectNamed: #'GsDocText') new details: self sbNextParagraph trimSeparators.
	doc documentClassWith: txt.
	class description: doc.
	self systemBrowserUpdate.
!

sbClassesToDictionary: anOrderedCollection

	| action targetName target sourceNames sources classNames |
	action := anOrderedCollection removeFirst.
	targetName := anOrderedCollection removeFirst asSymbol.
	target := self symbolList detect: [:each | each name = targetName].
	sourceNames := self nextLineAsList collect: [:each | each asSymbol].
	sources := sourceNames collect: [:eachName | self symbolList detect: [:eachDictionary | eachDictionary name = eachName]].
	classNames := self nextLineAsList collect: [:each | each asSymbol].
	classNames do: [:eachName | 
		| source class |
		source := sources detect: [:eachDict | 
			class := eachDict detect: [:eachGlobal | eachGlobal isBehavior and: [eachGlobal name = eachName]] ifNone: [nil].
			class notNil.
		].
		target at: class name put: class.
		action = 'move' ifTrue: [source removeKey: class name].
	].
	self systemBrowserUpdate.
!

sbClassFrom: anOrderedCollection

	| selectedClassName selectedClassOop set myClass |
	selectedClassName := (anOrderedCollection removeFirst subStrings: Character space) first.
	selectedClassOop := anOrderedCollection removeFirst asNumber.
	set := IdentitySet new.
	self symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal |
			eachGlobal isBehavior ifTrue: [
				set addAll: (self historyOf: eachGlobal).
			].
		].
	].
	myClass := set detect: [:each | (self oopOf: each) = selectedClassOop].
	myClass name asString = selectedClassName ifFalse: [self error: 'Class not found!!'].
	anOrderedCollection removeFirst = 'classTab' ifTrue: [myClass := myClass class].
	^myClass.
!

sbClassTemplate

	| string index |
	string := Stream definition.
	index := string findPattern: #('Stream') startingAt: 1.
	writeStream 
		nextPutAll: '0'; lf;	"selectedClassOop"
		nextPut: $(; lf;
		nextPutAll: (string copyFrom: 1 to: index - 1);
		nextPutAll: 'MyNewClass';
		nextPutAll: (string copyFrom: index + 6 to: string size);
		nextPutAll: ')';
		nextPutAll: ((string includesString: 'category: ''')
			ifTrue: ['']
			ifFalse: [' category: ' , Boolean category printString]); lf;
		nextPut: $%; lf;
		nextPut: $%; lf;
		yourself.
!

sbComparePackages: anOrderedCollection

	| current ancestor repository patch string |
	current := self mcWorkingCopyClass forPackage: (self mcPackageClass named: anOrderedCollection removeFirst).
	ancestor := anOrderedCollection removeFirst.
	repository := anOrderedCollection removeFirst.
	repository := current repositoryGroup repositories detect: [:each | each description = repository].
	ancestor := repository class name = #'MCDictionaryRepository'
		ifTrue: [repository versionFromVersionNamed: ancestor]
		ifFalse: [repository versionFromFileNamed: ancestor , '.mcz'].
	patch := current package snapshot patchRelativeToBase: ancestor snapshot.
	string := self 
		_mcDescriptionOfPatch: patch
		baseName: ancestor info name
		alternateName: nil.
	writeStream 
		nextPutAll: 'comparePackages'; lf;
		nextPutAll: string;
		yourself.
!

sbCopyMethodsFor: newClass

	| history oldClass symbolList |
	newClass isMeta ifFalse: [self sbCopyMethodsFor: newClass class].
	history := self historyOf: newClass thisClass.
	oldClass := history at: history size - 1.
	newClass isMeta ifTrue: [oldClass := oldClass class].
	symbolList := self symbolList.
	oldClass selectors do: [:each | 
		| source category errors |
		source := (oldClass compiledMethodAt: each) sourceString.
		category := oldClass categoryOfSelector: each.
		errors := newClass 
			compileMethod: source
			dictionaries: symbolList
			category: category.
		errors notNil ifTrue: [
			writeStream
				nextPutAll: 'compileError'; lf;
				nextPutAll: newClass name; tab;
				nextPutAll: category; lf;
				nextPutAll: source; lf;
				nextPut: $%; lf;
				yourself.
			newClass removeSelector: each ifAbsent: [].
		].
	].
!

sbFileOutClass: anOrderedCollection

	writeStream nextPutAll: (self sbClassFrom: anOrderedCollection) thisClass fileOutClass.
!

sbFileOutDictionary: anOrderedCollection

	self classOrganizer
		fileOutClassesAndMethodsInDictionary: (self objectNamed: anOrderedCollection first)
		on: writeStream.
!

sbFindClass

	| classToPackageMap |
	classToPackageMap := self sbFindClassPackageMap.
	self symbolList do: [:eachDict | 
		| name |
		name := eachDict name.
		eachDict do: [:eachGlobal | 
			eachGlobal isBehavior ifTrue: [
				| category |
				category := eachGlobal category.
				category isNil ifTrue: [category := ''].
"1"			self sbAddNameOf: eachGlobal.
				writeStream
"2"				nextPutAll: name; tab;
"3"				nextPutAll: category; tab;		"Class category"
"4"				nextPutAll: (classToPackageMap at: eachGlobal ifAbsent: ['']); tab;		"Package name if available"
					lf.
			].
		].
	].
!

sbFindClassPackageMap

	| systemOrganizerClass mcWorkingCopyClass dictionary packageInfoList |
	dictionary := Dictionary new.
	(systemOrganizerClass := self objectNamed: #'SystemOrganizer') isNil ifTrue: [^dictionary].
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^dictionary].
	packageInfoList := mcWorkingCopyClass allManagers collect: [:each | each packageInfo].
	systemOrganizerClass new categoryDict keysAndValuesDo: [:catName :classes |
		| symbol packageInfo |
		symbol := catName asSymbol.
		packageInfo := packageInfoList detect: [:each | each includesSystemCategory: symbol] ifNone: [nil].
		packageInfo notNil ifTrue: [
			| name |
			name := packageInfo name.
			classes do: [:each | dictionary at: each put: name].
		].
	].
	^dictionary.
!

sbFindSelectors: anOrderedCollection

	| allSymbols pattern |
	pattern := (anOrderedCollection collect: [:each | each = '*' ifTrue: [$*] ifFalse: [each]]) asArray.
	allSymbols := ((AllUsers userWithId: #SymbolUser ifAbsent: [AllUsers userWithId: #DataCurator]) resolveSymbol: #AllSymbols) value.
	allSymbols := allSymbols select: [:each |each asUppercase matchPattern: pattern].
	allSymbols := allSymbols select: [:each | (self classOrganizer implementorsOf: each) notEmpty].
	allSymbols := allSymbols asSortedCollection.
	allSymbols do: [:each | writeStream nextPutAll: each; nextPut: Character lf; yourself].
!

sbInstVarsOldParent: oldParent newParent: newParent oldChild: oldChild

	| added removed newList used missing |
	added := newParent allInstVarNames asIdentitySet - oldParent allInstVarNames asIdentitySet.
	removed := oldParent allInstVarNames asIdentitySet - newParent allInstVarNames asIdentitySet.
	newList := oldChild instVarNames.
	used := IdentitySet new.
	(oldChild class canUnderstand: #'_methodDict') ifTrue: [
		oldChild _methodDict do: [:each | used addAll: each instVarsAccessed].
	].
	(oldChild class canUnderstand: #'persistentMethodDictsDo:') ifTrue: [
		oldChild persistentMethodDictsDo: [:eachDict | 
			eachDict do: [:eachMethod | 
				used addAll: eachMethod instVarsAccessed.
			].
		].
	].
	used := used * removed.	"Only interested in things that have been removed."
	missing := (used - newList asIdentitySet) asSortedCollection asArray.
	newList := newList , missing.
	newList := newList reject: [:each | added includes: each].
	^newList.
!

sbLoadLatestVersionOfConfiguration: anOrderedCollection

	anOrderedCollection do: [:each | 
		(self objectNamed: each) project latestVersion load.
	].
	self systemBrowserUpdate.
!

sbMethodCategory: anOrderedCollection

	| behavior category |
	behavior := self sbClassFrom: anOrderedCollection.
	category := anOrderedCollection removeFirst.
	anOrderedCollection do: [:each | 
		behavior 
			moveMethod: each asSymbol
			toCategory: category.
	].
	self systemBrowserUpdate.
!

sbMethodClass: anOrderedCollection
	"Drag/drop method onto class"

	| sourceBehavior action targetName set target |
	sourceBehavior := self sbClassFrom: anOrderedCollection.
	action := anOrderedCollection removeFirst.
	targetName := anOrderedCollection removeFirst asSymbol.
	set := IdentitySet new.
	self symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal | 
			(eachGlobal isBehavior and: [eachGlobal name = targetName]) ifTrue: [set add: eachGlobal].
		].
	].
	1 < set size ifTrue: [self error: 'Target name is ambiguous!!'].
	1 = set size ifFalse: [self error: 'Target not found!!'].
	target := set asArray first.
	sourceBehavior isMeta ifTrue: [target := target class].
	anOrderedCollection do: [:each | 
		| gsMethod result |
		gsMethod := sourceBehavior compiledMethodAt: each asSymbol.
		result := self		"key: GsNMethod value: (Array withAll: errors and warnings)"
				compileMethod: gsMethod sourceString
				behavior: target
				symbolList: self symbolList 
				inCategory: (sourceBehavior categoryOfSelector: gsMethod selector).
		(result key notNil and: [action = 'move']) ifTrue: [
			sourceBehavior removeSelector: gsMethod selector.
		].
	].
	self systemBrowserUpdate.
!

sbMigrateAll: aClass

	| mcPlatformSupport classes instances |
	((mcPlatformSupport := self objectNamed: #'MCPlatformSupport') notNil and: [mcPlatformSupport autoMigrate]) ifTrue: [^self].
	System commitTransaction ifFalse: [self error: 'commit failed!!'].
	classes := (ClassOrganizer new allSubclassesOf: aClass) 
		inject: (IdentitySet withAll: (self historyOf: aClass))
		into: [:set :each | set addAll: (self historyOf: each); yourself].
	classes := classes asArray.
	instances := (self objectNamed: #'SystemRepository') listInstances: classes.
	1 to: classes size do: [:i | 
		| class |
		class := classes at: i.
		class 
			migrateInstances: (instances at: i) 
			to: (self historyOf: class) last.
		System commitTransaction ifFalse: [self error: 'commit failed!!'].
	].
!

sbNextParagraph

	| stream |
	stream := WriteStream on: String new.
	[
		readStream peek = $%.
	] whileFalse: [
		stream nextPutAll: self nextLine; lf.
	].
	self nextLine.
	^stream contents.
!

sbPostSaveClass: anOrderedCollection

	| newClass copyMethods recompileSubclasses migrateInstances removeFromClassHistory |
	newClass := (self sbClassFrom: anOrderedCollection) thisClass.
	copyMethods := anOrderedCollection removeFirst = 'true'.
	recompileSubclasses := anOrderedCollection removeFirst = 'true'.
	migrateInstances := anOrderedCollection removeFirst = 'true'.
	removeFromClassHistory := anOrderedCollection removeFirst = 'true'.
	copyMethods ifTrue: [self sbCopyMethodsFor: newClass].
	recompileSubclasses ifTrue: [self sbRecompileSubclassesOf: newClass andCopyMethods: copyMethods].
	migrateInstances ifTrue: [self sbMigrateAll: newClass].
	removeFromClassHistory ifTrue: [self sbRemoveHistory: newClass].
	self systemBrowserUpdate.
!

sbRecompileSubclassesOf: newClass andCopyMethods: aBoolean

	| history oldClass symbolList list |
	history := self historyOf: newClass thisClass.
	oldClass := history at: history size - 1.
	symbolList := self symbolList.
	list := self classOrganizer subclassesOf: oldClass.
	list do: [:oldSubclass |
		| instVars classInstVars newSubclass |
		instVars := self sbInstVarsOldParent: oldClass newParent: newClass oldChild: oldSubclass.
		classInstVars := self sbInstVarsOldParent: oldClass class newParent: newClass class oldChild: oldSubclass class.
		newSubclass := newClass
			subclass: oldSubclass name
			instVarNames: instVars
			classVars: oldSubclass classVarNames
			classInstVars: classInstVars
			poolDictionaries: oldSubclass sharedPools
			inDictionary: (System myUserProfile dictionaryAndSymbolOf: oldSubclass) first
			instancesInvariant: oldSubclass instancesInvariant
			isModifiable: oldSubclass isModifiable.
		aBoolean ifTrue: [self sbCopyMethodsFor: newSubclass].
		self classOrganizer update.
	].
!

sbRemoveClasses

	| isPackages containers classNames |
	isPackages := self nextLine = 'packageList'.
	containers := self nextLineAsList.
	classNames := (self nextLineAsList reject: [:each | each isEmpty]) collect: [:each | (each subStrings: Character space) first asSymbol].
	self symbolList do: [:eachDictionary | 
		| dictionaryName |
		dictionaryName := eachDictionary name asString.
		classNames do: [:eachName |
			| class flag |
			(class := eachDictionary at: eachName ifAbsent: [nil]) notNil ifTrue: [
				selections removeAllKeys: selections keys.
				isPackages ifTrue: [
					flag := false. 
					containers do: [:each |
						flag := flag or: [(class category copyFrom: 1 to: (class category size min: each size)) = each].
						flag ifTrue: [
							selections at: #'package' put: each.
						].
					].
				] ifFalse: [
					flag := containers includes: dictionaryName.
					flag ifTrue: [
						selections at: #'dictionary' put: dictionaryName.
					].
				].
				flag ifTrue: [
					selectedClass := (eachDictionary at: eachName) superclass.
					selections 
						at: #'category' 		put: selectedClass category;
						at: #'className'	put: selectedClass name;
						at: #'class'				put: selectedClass;
						yourself.
					eachDictionary removeKey: eachName.
				].
			].
		].
	].
	self systemBrowserUpdate.
!

sbRemoveDictionaries: anOrderedCollection

	anOrderedCollection do: [:each | 
		self symbolList removeDictionaryNamed: each asSymbol.
	].
	self refreshSymbolList.
	self systemBrowserUpdate.
!

sbRemoveGlobals

	| symbolList dictionaries globals |
	symbolList := self symbolList.
	dictionaries := self nextLineAsList collect: [:each | each asSymbol].
	dictionaries := dictionaries collect: [:eachName | symbolList detect: [:eachDict | eachDict name = eachName]].
	globals := self nextLineAsList collect: [:each | each asSymbol].
	dictionaries do: [:eachDict | 
		globals do: [:eachKey | 
			eachDict removeKey: eachKey ifAbsent: [].
		].
	].
	self systemBrowserUpdate.
!

sbRemoveHistory: aClass

	(ClassOrganizer new allSubclassesOf: aClass) asArray , (Array with: aClass) do: [:eachNewClass | 
		(self historyOf: eachNewClass) asArray do: [:eachClass | 
			eachClass ~~ eachNewClass ifTrue: [
				(self historyOf: eachNewClass) removeVersion: eachClass.
			].
		].
	].
!

sbRemoveMethodCategories: anOrderedCollection

	| behavior |
	behavior := self sbClassFrom: anOrderedCollection.
	anOrderedCollection do: [:each | behavior removeCategory: each].
	self systemBrowserUpdate.
!

sbRemoveMethods: anOrderedCollection

	| behavior |
	behavior := self sbClassFrom: anOrderedCollection.
	anOrderedCollection do: [:each | behavior removeSelector: each asSymbol].
	self systemBrowserUpdate.
!

sbRemovePriorVersions

	| isPackages containers classNames |
	isPackages := self nextLine = 'packageList'.
	containers := self nextLineAsList.
	classNames := (self nextLineAsList reject: [:each | each isEmpty]) collect: [:each | (each subStrings: Character space) first asSymbol].
	self symbolList do: [:eachDictionary | 
		| dictionaryName |
		dictionaryName := eachDictionary name asString.
		classNames do: [:eachName |
			| class flag |
			(class := eachDictionary at: eachName ifAbsent: [nil]) notNil ifTrue: [
				isPackages ifTrue: [
					flag := false.
					containers do: [:each | flag := flag or: [(class category copyFrom: 1 to: (class category size min: each size)) = each]].
				] ifFalse: [
					flag := containers includes: dictionaryName.
				].
				flag ifTrue: [
					| classHistory |
					classHistory := self historyOf: class.
					classHistory size - 1 timesRepeat: [
						classHistory removeVersion: classHistory first.
					].
				].
			].
		].
	].
	self systemBrowserUpdate.
!

sbRemoveRepository: list

	| description repository packages |
	description := list removeFirst.
	repository := self mcRepositoryGroup repositories detect: [:each | each description = description].
	packages := self mcWorkingCopyClass allManagers select: [:each | list includes: each package name].
	packages do: [:each | each repositoryGroup removeRepository: repository].
	self systemBrowserUpdate.
!

sbRevertClass

	| isPackages container className |
	isPackages := self nextLine = 'packageList'.
	container := self nextLine trimSeparators.
	className := self nextLine trimSeparators.
	self symbolList do: [:eachDictionary | 
		| dictionaryName class flag |
		dictionaryName := eachDictionary name asString.
		(class := eachDictionary at: className ifAbsent: [nil]) notNil ifTrue: [
			isPackages ifTrue: [
				flag := (class category copyFrom: 1 to: (class category size min: container size)) = container.
			] ifFalse: [
				flag := container = dictionaryName.
			].
			flag ifTrue: [
				| history |
				history := class classHistory.
				(class == history last and: [1 < history size]) ifFalse: [self error: 'Unexpected class history!!'].
				history removeVersion: class.
				class := history last.
				eachDictionary at: class name put: class.
			].
		].
	].
	self systemBrowserUpdate.
!

sbRunClassTests: aString

	| behavior |
	behavior := self sbClassFrom: (aString subStrings: Character tab).
	^self defectiveTestsIn: behavior.
!

sbRunMethodTests: aString

	| list class |
	list := aString subStrings: Character tab.
	class := (self sbClassFrom: list) thisClass.
	list do: [:each | class debug: each asSymbol].
	^true.
!

sbSavePackage: list

	| packageName package repositoryDescription repository versionName httpUser httpPassword comment |
	packageName := list removeFirst.
	package := self mcWorkingCopyClass allManagers detect: [:each | each package name = packageName].
	repositoryDescription := list removeFirst.
	repository := self mcRepositoryGroup repositories detect: [:each | each description = repositoryDescription].
	versionName := list removeFirst.
	list notEmpty ifTrue: [httpUser := list removeFirst].
	list notEmpty ifTrue: [httpPassword := list removeFirst].
	comment := self sbNextParagraph.
	[
		comment notEmpty and: [comment last asciiValue <= 32].
	] whileTrue: [
		comment := comment copyFrom: 1 to: comment size - 1.
	].
	(repository class name = #'MCHttpRepository') ifTrue: [
		repository
			user: httpUser;
			password: httpPassword;
			yourself.
	].
	self 
		mcStore: package 
		name: versionName 
		message: comment 
		repository: repository.
	self systemBrowserUpdate.
!

sbSetHomeDictionary: list

	| name dictionary packagePolicy |
	name := list removeFirst asSymbol.
	dictionary := self symbolList detect: [:each | each name = name].
	(packagePolicy := self gsPackagePolicy) notNil ifTrue: [
		packagePolicy homeSymbolDict: dictionary.
	].
	self systemBrowserUpdate.
!

sbUniqueVersionName: aList

	| packageName package |
	packageName := aList removeFirst.
	package := self mcWorkingCopyClass allManagers detect: [:each | each package name = packageName].
	writeStream
		nextPutAll: 'uniqueVersionName'; lf;
		nextPutAll: package uniqueVersionName;
		yourself.

!

sbUnloadPackage: anOrderedCollection

	| string |
	string := anOrderedCollection removeFirst.
	(self mcWorkingCopyClass forPackage: (self mcPackageClass named: string)) unload.
	self systemBrowserUpdate.
!

sbUpdateClassCategories

	| categories selection override |
	categories := Set new.
	classList do: [:each | categories add: each category].
	categories asSortedCollection do: [:each | 
		writeStream nextPutAll: each; tab.
	].
	writeStream lf.
	selection := self nextLine.
	(override := selections at: #'category' ifAbsent: [nil]) notNil ifTrue: [selection := override , '-'].
	selection := 1 < selection size 
		ifTrue: [selection copyFrom: 1 to: selection size - 1]
		ifFalse: [''].
	selection notEmpty ifTrue: [
		categories := categories select: [:each | (each copyFrom: 1 to: (each size min: selection size)) = selection].
		categories isEmpty ifTrue: [selection := ''].
		categories notEmpty ifTrue: [
			classList := classList select: [:each | categories includes: each category].
		].
	].
	writeStream nextPutAll: selection; lf.
!

sbUpdateClasses

	| tabName |
	tabName := self nextLine.
	writeStream nextPutAll: tabName; lf.
	tabName = 'classList' ifTrue: [^self sbUpdateClassList].
	tabName = 'classHierarchy' ifTrue: [^self sbUpdateClassHierarchy].
	self error: 'Unexpected token!!'.
!

sbUpdateClassHierarchy

	| currentSelection currentClass allClasses override testCaseClass |
	allClasses := IdentitySet new.
	"Add each class in dictionary/package to stream on a line with the superclass chain"
	classList do: [:each | 
		currentClass := each.
		[
			currentClass notNil.
		] whileTrue: [
			self sbAddNameOf: currentClass.
			allClasses add: currentClass.
			currentClass := currentClass superclass.
		].
		writeStream lf.
	].
	writeStream nextPut: $%; lf.
	"now figure out which class to select"
	currentSelection := self nextLineAsList.
	(override := selections at: #'className' ifAbsent: [nil]) notNil ifTrue: [currentSelection := Array with: override].
	currentSelection isEmpty ifTrue: [
		selectedClass := nil.
	] ifFalse: [
		(currentClass := selections at: #'class' ifAbsent: [nil]) ifNil: [
			| className |
			className := (currentSelection last subStrings: Character space) first asSymbol.
			selectedClass := allClasses detect: [:each | each name = className] ifNone: [nil].
		] ifNotNil: [
			selectedClass := nil.
			[
				selectedClass isNil and: [currentClass notNil].
			] whileTrue: [
				selectedClass := allClasses detect: [:each | each == currentClass] ifNone: [nil].
				currentClass := currentClass superclass.
			].
		].
	].
	currentClass := selectedClass.
	[
		currentClass notNil.
	] whileTrue: [
		self sbAddNameOf: currentClass.
		currentClass := currentClass superclass.
	].
	writeStream lf;
		nextPutAll: (selectedClass notNil and: [(testCaseClass := self objectNamed: #'TestCase') notNil and: [selectedClass inheritsFrom: testCaseClass]]) printString; tab;
		lf.
!

sbUpdateClassInfo

	| definition classComment |
	selectedClass isNil ifTrue: [^self sbClassTemplate].
	writeStream 
		nextPutAll: (self oopOf: selectedClass) printString; lf;
		nextPut: $(; lf;
		nextPutAll: (definition := selectedClass definition);
		nextPutAll: ')';
		nextPutAll: ((definition includesString: 'category: ''')
			ifTrue: ['']
			ifFalse: [' category: ' , selectedClass category printString]); lf;
		nextPut: $%; lf;
		yourself.
	(Class includesSelector: #'comment') ifTrue: [
		classComment := selectedClass comment.
	] ifFalse: [
	(Class includesSelector: #'classComment') ifTrue: [
		classComment := selectedClass classComment.
	] ifFalse: [
		| description |
		description := selectedClass description.
		description class name = #'GsClassDocumentation' ifTrue: [
			classComment := description detailsAboutClass.
		].
	]].
	classComment isNil ifTrue: [classComment := ''].
	writeStream 
		nextPutAll: classComment; lf;
		nextPut: $%; lf;
		yourself.
!

sbUpdateClassList

	| mySelections override testCaseClass |
	(classList asSortedCollection: [:a :b | a name <= b name]) do: [:eachClass | 
		self sbAddNameOf: eachClass.
	].
	writeStream lf.
	mySelections := self nextLineAsList collect: [:each | (each subStrings: Character space) first asSymbol].
	(override := selections at: #'className' ifAbsent: [nil]) notNil ifTrue: [mySelections := Array with: override].
	mySelections := classList select: [:eachClass | mySelections includes: eachClass name].
	mySelections do: [:each | self sbAddNameOf: each].
	writeStream lf.
	(testCaseClass := self objectNamed: #'TestCase') notNil ifTrue: [
		(mySelections allSatisfy: [:each | each  inheritsFrom: testCaseClass]) printOn: writeStream.
	].
	writeStream lf.
	selectedClass := mySelections size = 1
		ifTrue: [mySelections first]
		ifFalse: [nil].
!

sbUpdateDictionaries

	| override packagePolicy home symbolList oldSelections newSelections fullList globals |
	oldSelections := self nextLineAsList.
	(override := selections at: #'dictionary' ifAbsent: [nil]) notNil ifTrue: [oldSelections := Array with: override].
	symbolList := self symbolList.
	(packagePolicy := self gsPackagePolicy) notNil  ifTrue: [
		home := packagePolicy homeSymbolDict.
	].
	fullList := symbolList collect: [:each | (each == home ifTrue: ['H'] ifFalse: ['V']) , each name].
	self writeList: fullList.
	fullList := fullList collect: [:each | each copyFrom: 2 to: each size].
	newSelections := oldSelections select: [:each | fullList includes: each].
	globals := Dictionary new.
	newSelections do: [:eachName | 
		| symbolDictionary globalKeyPrefix |
		globalKeyPrefix := 1 < newSelections size ifTrue: [eachName , '.'] ifFalse: [''].
		writeStream nextPutAll: eachName; tab.
		symbolDictionary := symbolList at: (fullList indexOf: eachName).
		symbolDictionary keys asSortedCollection do: [:eachKey |
			| eachGlobal |
			eachGlobal := symbolDictionary at: eachKey.
			(eachGlobal isBehavior and: [eachGlobal name == eachKey])
				ifTrue: [classList add: eachGlobal]
				ifFalse: [globals at: globalKeyPrefix , eachKey put: (symbolDictionary associationAt: eachKey)].
		].
	].
	writeStream lf.
	globals keys asSortedCollection do: [:eachKey | 
		| eachAssociation eachValue string |
		eachAssociation := globals at: eachKey.
		eachValue := eachAssociation value.
		string := self asString: eachValue.
		100 < string size ifTrue: [string := string copyFrom: 1 to: 100].
		string := string collect: [:char | (char asciiValue < 32 or: [127 < char asciiValue]) ifTrue: [$.] ifFalse: [char]].
		string := String withAll: string asArray.
		writeStream 
"1"		nextPutAll: eachKey; tab; 
"2"		nextPutAll: eachValue class name; tab; 
"3"		nextPutAll: string; tab; 
"4"		nextPutAll: (self oopOf: eachValue) printString; tab; 
"5"		nextPutAll: (self oopOf: eachAssociation) printString; lf.
	].
	writeStream nextPut: $%; lf.
!

sbUpdateMethod: aSymbol

	| selection classes names method string list oldGsMethod |

	"Inherited implimentors"
	classes := self sbUpdateMethodInheritedImplementationsOf: aSymbol.
	names := classes collect: [:each | each name asString].
	self writeList: names.	"Line 1"

	"Which inherited implementation is selected?"
	selection := self nextLine.
	(names includes: selection) ifFalse: [selection := names last].
	writeStream nextPutAll: selection; lf.	"Line 2"

	"May user edit method?"
	method := (classes detect: [:each | each name asString = selection]) compiledMethodAt: aSymbol.
	writeStream nextPutAll: (self currentUserMayEditMethod: method) asString; lf.	"Line 3"

	"Method source"
	writeStream nextPutAll: (string := method sourceString).
	string last = Character lf ifFalse: [writeStream lf].
	writeStream nextPut: $%; lf.	"Lines 4-N"

	"Array of Associations (offset -> selector) indexed by step points"
	list := self sbUpdateMethodStepPointsFor: method.
	list := list collect: [:each | each key printString , ' ' , each value].
	self writeList: list.	"Line N+1"

	"breaks"
	list := self  sbUpdateMethodBreakPointsFor: method.
	self writeList: (list collect: [:each | each printString]).	"Line N+2"

	"Method category"
	writeStream nextPutAll: (method inClass categoryOfSelector: method selector); lf.	"Line N+3"

	oldGsMethod := (method inClass class canUnderstand: #'persistentMethodDictForEnv:')
		ifTrue: [(method inClass persistentMethodDictForEnv: 0) at: aSymbol ifAbsent: [method]]
		ifFalse: [(method inClass class canUnderstand: #'_rawMethodDict')
			ifTrue: [method inClass _rawMethodDict at: aSymbol ifAbsent: [method]]
			ifFalse: [method]].
	method ~~ oldGsMethod ifTrue: [
		string := oldGsMethod sourceString.
		writeStream nextPutAll: string.
		(string notEmpty and: [string last = Character lf]) ifFalse: [writeStream lf].
	].
	writeStream nextPut: $%; lf.
!

sbUpdateMethodBreakPointsFor: aMethod
	"Answers an Array of step points"

	^aMethod _stepPointsFromBreakIpOffsets: aMethod _breakpointIpOffsets.		"at least as far back as 32-bit 6.3.0 and 64-bit 2.3.0, but not in 64-bit 3.0"
!

sbUpdateMethodCategories

	methodFilters := Set new.
	classList do: [:each | methodFilters addAll: each categoryNames].
	methodFilters isEmpty ifTrue: [methodFilters := #(#'other')].
	"Reverse order to be consistent with variables, where we add superclasses to the end"
	self writeList: methodFilters asSortedCollection asArray reverse.
	self sbUpdateMethodFilterSelections.
!

sbUpdateMethodFilter

	methodFilterType := self nextLine.
	writeStream nextPutAll: methodFilterType; lf.
	selectedClass isNil ifTrue: [^self].
	methodFilterType = 'categoryList' ifTrue: [^self sbUpdateMethodCategories].
	methodFilterType = 'variableList' ifTrue: [^self sbUpdateMethodVariables].
	self error: 'Unexpected token!!'.
!

sbUpdateMethodFilterSelections

	| mySelections override |
	mySelections := self nextLineAsList.
	(override := selections at: #'methodCategory' ifAbsent: [nil]) notNil ifTrue: [mySelections := Array with: override].
	mySelections := methodFilters select: [:each | mySelections includes: each asString].
	mySelections notEmpty ifTrue: [methodFilters := mySelections].
	self writeList: mySelections.
!

sbUpdateMethodInheritedImplementationsOf: aSymbol

	| classes currentClass |
	classes := OrderedCollection new.
	currentClass := classList last.
	[
		currentClass notNil.
	] whileTrue: [
		(currentClass includesSelector: aSymbol) ifTrue: [classes add: currentClass].
		currentClass := currentClass superclass.
	].
	^classes reverse.
!

sbUpdateMethods

	| selectors isTestClass gsPackagePolicy anySatisfy |
	selectedClass isNil ifTrue: [^self].
	anySatisfy := false.
	selectedClass _allSuperList do: [:each | 
		anySatisfy := anySatisfy or: [each name = #'TestCase'].
	].
	isTestClass := anySatisfy ifTrue: [$T] ifFalse: [$F].
	gsPackagePolicy := self gsPackagePolicy.
	methodFilterType = 'categoryList' ifTrue: [selectors := self sbUpdateMethodsByCategories].
	methodFilterType = 'variableList' ifTrue: [selectors := self sbUpdateMethodsByVariables].
	selectors isNil ifTrue: [self error: 'Unrecognized method filter type'].
	selectors := selectors asSortedCollection.
	selectors do: [:each | 
		writeStream 
"1"		nextPutAll: each; tab;
"2"		nextPut: ((selectedClass superclass notNil and: [selectedClass superclass canUnderstand: each]) ifTrue: [$T] ifFalse: [$F]); tab;
"3"		nextPut: isTestClass; tab;
"4"		nextPut: ((gsPackagePolicy notNil and: [gsPackagePolicy includesSelector: each for: selectedClass]) ifTrue: [$T] ifFalse: [$F]); tab;
			lf.
	].
	writeStream nextPut: $%; lf.
	self sbUpdateMethodSelectionsIn: selectors.
!

sbUpdateMethodsByCategories

	| selectors |
	selectors := IdentitySet new.
	classList do: [:eachClass |
		eachClass selectors do: [:eachSelector |
			(methodFilters includes: (eachClass categoryOfSelector: eachSelector)) ifTrue: [selectors add: eachSelector].
		].
	].
	^selectors.
!

sbUpdateMethodsByVariables

	| selectors filters |
	selectors := IdentitySet new.
	filters := IdentitySet withAll: (methodFilters select: [:each | each isSymbol]).
	selectedClass selectors do: [:eachSelector | 
		| gsMethod |
		gsMethod := selectedClass compiledMethodAt: eachSelector.
		(gsMethod instVarsAccessed * filters) notEmpty ifTrue: [selectors add: eachSelector].
	].
	^selectors.
!

sbUpdateMethodSelectionsIn: aList

	| priorSelections override newSelections |
	priorSelections := self nextLineAsList.
	(override := selections at: #'method' ifAbsent: [nil]) notNil ifTrue: [priorSelections := Array with: override].
	newSelections := aList select: [:each | priorSelections includes: each asString].
	self writeList: newSelections.
	newSelections size = 1 ifTrue: [self sbUpdateMethod: newSelections first].
!

sbUpdateMethodStepPointsFor: aMethod
	"Answers an Array of Associations (offset -> selector) indexed by step point"

	| offsets selectors |
	offsets := aMethod _sourceOffsets.
	selectors := Array new.
	1 to: offsets size do: [:i | 		"exists as far back as 32-bit 6.3.0"
		| offset ip association |
		offset := offsets at: i.
		ip := (aMethod _ipForStepPoint: i) + 2.		"dropped in 64-bit 3.0"
		association := offset -> ''.
		ip <= aMethod size ifTrue: [
			| literal |
			((literal := aMethod at: ip) isKindOf: Symbol) ifTrue: [
				association value: literal.
			].
		].
		selectors add: association.
	].
	^selectors.
!

sbUpdateMethodVariables

	| currentClass |
	currentClass := classList notEmpty ifTrue: [classList last] ifFalse: [nil].
	methodFilters := OrderedCollection new.
	[
		currentClass notNil.
	] whileTrue: [
		methodFilters
			addAll: currentClass instVarNames reverse;
			add: ' ' , currentClass name;
			yourself.
		currentClass := currentClass superclass.
	].
	self writeList: methodFilters.
	self sbUpdateMethodFilterSelections.
!

sbUpdatePackage: aString

	| package workingCopy list index |
	package := self mcPackageClass named: aString.
	workingCopy := self mcWorkingCopyClass forPackage: package.
	list := workingCopy ancestors collect: [:each | 0 -> each].
	index := 1.
	[
		list size < 4 and: [index <= list size].
	] whileTrue: [
		| assoc |
		assoc := list at: index.
		assoc value ancestors do: [:parent | list add: assoc key + 1 -> parent].
		index := index + 1.
	].
	list do: [:each | 
		| date time |
		date := each value date isNil 
			ifTrue: ['']
			ifFalse: [each value date asStringUsingFormat: #(3 2 1 $- 1 1)].
		time := each value time isNil
			ifTrue: ['']
			ifFalse: [each value time asStringUsingFormat: #($: true false)].
		writeStream 
			nextPutAll: each key printString; tab;
			nextPutAll: each value name; tab;
			nextPutAll: date; 
			nextPut: $T;
			nextPutAll: time; tab;
			nextPutAll: (each value message collect: [:char |(char asciiValue < 32 or: [127 < char asciiValue]) ifTrue: [$.] ifFalse: [char]]);
			lf.
	].
	writeStream nextPut: $%; lf.
	workingCopy repositoryGroup repositories do: [:each | 
		writeStream 
			nextPutAll: each class name; tab;
			nextPutAll: each description; tab;
			yourself.
		(each class name = #'MCHttpRepository') ifTrue: [
			writeStream
				nextPutAll: each user; tab;
				nextPutAll: each password;
				yourself.
		] ifFalse: [
			writeStream tab; tab.
		].
		writeStream lf.
	].
	writeStream nextPut: $%; lf.!

sbUpdatePackages

	| override modifiedList oldSelections newSelections fullList |
	oldSelections := self nextLineAsList.
	(override := selections at: #'package' ifAbsent: [nil]) notNil ifTrue: [oldSelections := Array with: override].
	fullList := self mcLoadedVersionNames subStrings: Character lf.
	fullList := fullList reject: [:each | each isEmpty].
	fullList := fullList asSortedCollection asArray.
	fullList := fullList collect: [:each | each subStrings: Character tab].
	fullList := fullList reject: [:each | each isEmpty].
	modifiedList := (fullList select: [:each | (each at: 2) = 'Y']) collect: [:each | each at: 3].
	fullList := fullList collect: [:each | each at: 3].
	self 
		writeList: fullList;
		writeList: modifiedList;
		yourself.
	newSelections := oldSelections select: [:each | fullList includes: each].
	(newSelections isEmpty and: [oldSelections size = 1]) ifTrue: [
			newSelections := fullList select: [:each | (oldSelections first copyFrom: 1 to: (oldSelections first size min: each size)) = each].
	].
	newSelections do: [:each | 
		writeStream nextPutAll: each; tab.
		classList addAll: (self mcPackageClass named: each) packageInfo classes.
	].
	writeStream lf.
	newSelections size = 1 ifTrue: [self sbUpdatePackage: newSelections first].
!

sbUpdatePackagesOrDictionaries

	| selectedTab |
	selectedTab := self nextLine.
	(self mcWorkingCopyClass isNil or: [self gsPackagePolicy isNil]) ifTrue: [selectedTab := 'dictionaryList'].
	writeStream nextPutAll: selectedTab; lf.
	classList := OrderedCollection new.
	selectedTab = 'dictionaryList' ifTrue: [^self sbUpdateDictionaries].
	selectedTab = 'packageList' ifTrue: [^self sbUpdatePackages].
	self error: 'unexpected token'.
!

sbUpdateSuperclass

	| tabName selected class index |
	tabName := self nextLine.
	(#('instanceTab' 'classTab') includes: tabName) ifFalse: [self error: 'Unexpected token!!'].
	writeStream nextPutAll: tabName; lf.
	selectedClass notNil ifTrue: [
		selectedClass := tabName = 'instanceTab'
			ifTrue: [selectedClass]
			ifFalse: [selectedClass class].
	].
	class := selectedClass.
	classList := OrderedCollection new.
	[
		class notNil.
	] whileTrue: [
		classList add: class.
		self sbAddNameOf: class.
		class := class superclass.
	].
	classList := classList reverse.
	selected := self nextLine.
	index := classList findFirst: [:each | each name asString = selected].
	index = 0 ifTrue: [index := classList size].
	0 < index ifTrue: [classList := classList copyFrom: index to: classList size].
	writeStream lf.
	classList notEmpty ifTrue: [
		selectedClass := classList first.
		self sbAddNameOf: selectedClass.
	].
	writeStream lf.
!

symbolList

	^System myUserProfile symbolList.
!

systemBrowser: aString

	^self copy systemBrowserA: aString.
!

systemBrowserA: aString

	| time |
	time := self millisecondsElapsedTime: [
		selections := Dictionary new.
		readStream := ReadStream on: aString.
		writeStream := WriteStream on: String new.
		writeStream lf.
		self systemBrowserCommand.
	].
	^time printString , writeStream contents.
!

systemBrowserCommand

	| list command |
	list := self nextLineAsList asOrderedCollection.
	command := list removeFirst.
	command = 'addDictionary'					ifTrue: [^self sbAddDictionary: list].
	command = 'addMethodCategory' 			ifTrue: [^self sbAddMethodCategory: list].
	command = 'addMissingAccessors'		ifTrue: [^self sbAddMissingAccessors: list].
	command = 'addPackage' 						ifTrue: [^self sbAddPackage: list].
	command = 'addRepository'					ifTrue: [^self sbAddRepository: list].

	command = 'break' 								ifTrue: [^self sbBreak: list].
	command = 'browseClassReferences'	ifTrue: [^self sbBrowseClassReferences: list].
	command = 'browseGlobalReferences'	ifTrue: [^self sbBrowseGlobalReferences: list].
	command = 'browseImplementors'			ifTrue: [^self sbBrowseImplementors: list].
	command = 'browseMethodHistory'		ifTrue: [^self sbBrowseMethodHistory: list].
	command = 'browseMethodsContaining'	ifTrue: [^self sbBrowseMethodsContaining: list].
	command = 'browseSenders'					ifTrue: [^self sbBrowseSenders: list].

	command = 'changeClassName'				ifTrue: [^self sbChangeClassName: list].
	command = 'changesInPackage'			ifTrue: [^self sbChangesInPackage: list].
	command = 'checkUniqueClassName'	ifTrue: [^self sbCheckUniqueClassName: list].
	command = 'class' 								ifTrue: [^self sbClass: list].
	command = 'classCategory'					ifTrue: [^self sbClassCategory: list].
	command = 'classComment'					ifTrue: [^self sbClassComment: list].
	command = 'classesToDictionary'			ifTrue: [^self sbClassesToDictionary: list].
	command = 'comparePackages'				ifTrue: [^self sbComparePackages: list].

	command = 'fileOutClass'						ifTrue: [^self sbFileOutClass: list].
	command = 'fileOutDictionary'				ifTrue: [^self sbFileOutDictionary: list].
	command = 'findClass' 							ifTrue: [^self sbFindClass].
	command = 'findSelectors'						ifTrue: [^self sbFindSelectors: list].

	command = 'loadLatestVersion'				ifTrue: [^self sbLoadLatestVersionOfConfiguration: list].

	command = 'method' 							ifTrue: [^self sbSaveMethod: list].
	command = 'methodCategory'				ifTrue: [^self sbMethodCategory: list].
	command = 'methodClass'						ifTrue: [^self sbMethodClass: list].

	command = 'objectLog'							ifTrue: [^self sbObjectLog: list].

	command = 'postSaveClass'					ifTrue: [^self sbPostSaveClass: list].

	command = 'removeClasses'					ifTrue: [^self sbRemoveClasses].
	command = 'removeDictionaries'			ifTrue: [^self sbRemoveDictionaries: list].
	command = 'removeGlobals'					ifTrue: [^self sbRemoveGlobals].
	command = 'removeMethodCategories' 	ifTrue: [^self sbRemoveMethodCategories: list].
	command = 'removeMethods'				ifTrue: [^self sbRemoveMethods: list].
	command = 'removePriorVersions'			ifTrue: [^self sbRemovePriorVersions].
	command = 'removeRepository'				ifTrue: [^self sbRemoveRepository: list].
	command = 'revertClass'						ifTrue: [^self sbRevertClass].

	command = 'savePackage'						ifTrue: [^self sbSavePackage: list].
	command = 'setHomeDictionary'			ifTrue: [^self sbSetHomeDictionary: list].

	command = 'uniqueVersionName'			ifTrue: [^self sbUniqueVersionName: list].
	command = 'unloadPackage'					ifTrue: [^self sbUnloadPackage: list].
	command = 'update' 								ifTrue: [^self systemBrowserUpdate].

	self error: 'Unknown command: ' , command printString.
!

systemBrowserUpdate

	writeStream nextPutAll: 'update'; lf.
	self 
		sbUpdatePackagesOrDictionaries;
		sbUpdateClassCategories;
		sbUpdateClasses;
		sbUpdateClassInfo;
		sbUpdateSuperclass;
		sbUpdateMethodFilter;
		sbUpdateMethods;
		yourself.
!

writeList: aList

	aList do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.
! !
!JadeServer categoriesFor: #addAccessorsFor:inBehavior:!public! !
!JadeServer categoriesFor: #assignClass:toCategory:!Classes!public! !
!JadeServer categoriesFor: #class:includesSelector:!Classes!public! !
!JadeServer categoriesFor: #classesForUser:!Classes!public! !
!JadeServer categoriesFor: #commentFor:!public! !
!JadeServer categoriesFor: #currentUserMayEditMethod:!public!System Browser! !
!JadeServer categoriesFor: #dictionaryAndSymbolOf:!public!System Browser! !
!JadeServer categoriesFor: #historyOf:!private! !
!JadeServer categoriesFor: #isPackagePolicyEnabled!public!System Browser! !
!JadeServer categoriesFor: #millisecondsElapsedTime:!public!System Browser! !
!JadeServer categoriesFor: #moveClassesInDictionary:category:to:!Classes!public! !
!JadeServer categoriesFor: #moveDictionary:toBefore:forUser:!public!SymbolDictionary! !
!JadeServer categoriesFor: #nextLine!public!System Browser! !
!JadeServer categoriesFor: #nextLineAsList!public!System Browser! !
!JadeServer categoriesFor: #objectSecurityPolicyFor:!public!System Browser! !
!JadeServer categoriesFor: #removeDictionary:fromUser:!public! !
!JadeServer categoriesFor: #removeKey:fromSymbolDictionary:!public!SymbolDictionary! !
!JadeServer categoriesFor: #sbAddDictionary:!public! !
!JadeServer categoriesFor: #sbAddMethodCategory:!public!System Browser! !
!JadeServer categoriesFor: #sbAddMissingAccessors:!public!System Browser! !
!JadeServer categoriesFor: #sbAddNameOf:!public!System Browser! !
!JadeServer categoriesFor: #sbAddPackage:!public!System Browser! !
!JadeServer categoriesFor: #sbAddRepository:!public!System Browser! !
!JadeServer categoriesFor: #sbBreak:!public!System Browser! !
!JadeServer categoriesFor: #sbBrowseClassReferences:!public!System Browser! !
!JadeServer categoriesFor: #sbBrowseGlobalReferences:!public!System Browser! !
!JadeServer categoriesFor: #sbBrowseImplementors:!public!System Browser! !
!JadeServer categoriesFor: #sbBrowseMethodHistory:!public!System Browser! !
!JadeServer categoriesFor: #sbBrowseMethodsContaining:!public!System Browser! !
!JadeServer categoriesFor: #sbBrowseSenders:!public!System Browser! !
!JadeServer categoriesFor: #sbChangeClassName:!public!System Browser! !
!JadeServer categoriesFor: #sbChangesInPackage:!public!System Browser! !
!JadeServer categoriesFor: #sbCheckUniqueClassName:!public!System Browser! !
!JadeServer categoriesFor: #sbClass:!public!System Browser! !
!JadeServer categoriesFor: #sbClassCategory:!public!System Browser! !
!JadeServer categoriesFor: #sbClassComment:!public!System Browser! !
!JadeServer categoriesFor: #sbClassesToDictionary:!public!System Browser! !
!JadeServer categoriesFor: #sbClassFrom:!public!System Browser! !
!JadeServer categoriesFor: #sbClassTemplate!public!System Browser! !
!JadeServer categoriesFor: #sbComparePackages:!public!System Browser! !
!JadeServer categoriesFor: #sbCopyMethodsFor:!public!System Browser! !
!JadeServer categoriesFor: #sbFileOutClass:!public!System Browser! !
!JadeServer categoriesFor: #sbFileOutDictionary:!public!System Browser! !
!JadeServer categoriesFor: #sbFindClass!public!System Browser! !
!JadeServer categoriesFor: #sbFindClassPackageMap!public!System Browser! !
!JadeServer categoriesFor: #sbFindSelectors:!public!System Browser! !
!JadeServer categoriesFor: #sbInstVarsOldParent:newParent:oldChild:!public!System Browser! !
!JadeServer categoriesFor: #sbLoadLatestVersionOfConfiguration:!public!System Browser! !
!JadeServer categoriesFor: #sbMethodCategory:!public!System Browser! !
!JadeServer categoriesFor: #sbMethodClass:!public!System Browser! !
!JadeServer categoriesFor: #sbMigrateAll:!public!System Browser! !
!JadeServer categoriesFor: #sbNextParagraph!public!System Browser! !
!JadeServer categoriesFor: #sbPostSaveClass:!public!System Browser! !
!JadeServer categoriesFor: #sbRecompileSubclassesOf:andCopyMethods:!public!System Browser! !
!JadeServer categoriesFor: #sbRemoveClasses!public!System Browser! !
!JadeServer categoriesFor: #sbRemoveDictionaries:!public!System Browser! !
!JadeServer categoriesFor: #sbRemoveGlobals!public!System Browser! !
!JadeServer categoriesFor: #sbRemoveHistory:!public!System Browser! !
!JadeServer categoriesFor: #sbRemoveMethodCategories:!public!System Browser! !
!JadeServer categoriesFor: #sbRemoveMethods:!public!System Browser! !
!JadeServer categoriesFor: #sbRemovePriorVersions!public!System Browser! !
!JadeServer categoriesFor: #sbRemoveRepository:!public!System Browser! !
!JadeServer categoriesFor: #sbRevertClass!public!System Browser! !
!JadeServer categoriesFor: #sbRunClassTests:!public!System Browser! !
!JadeServer categoriesFor: #sbRunMethodTests:!public!System Browser! !
!JadeServer categoriesFor: #sbSavePackage:!public!System Browser! !
!JadeServer categoriesFor: #sbSetHomeDictionary:!public!System Browser! !
!JadeServer categoriesFor: #sbUniqueVersionName:!public!System Browser! !
!JadeServer categoriesFor: #sbUnloadPackage:!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateClassCategories!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateClasses!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateClassHierarchy!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateClassInfo!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateClassList!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateDictionaries!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateMethod:!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateMethodBreakPointsFor:!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateMethodCategories!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateMethodFilter!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateMethodFilterSelections!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateMethodInheritedImplementationsOf:!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateMethods!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateMethodsByCategories!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateMethodsByVariables!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateMethodSelectionsIn:!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateMethodStepPointsFor:!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateMethodVariables!public!System Browser! !
!JadeServer categoriesFor: #sbUpdatePackage:!public!System Browser! !
!JadeServer categoriesFor: #sbUpdatePackages!public!System Browser! !
!JadeServer categoriesFor: #sbUpdatePackagesOrDictionaries!public!System Browser! !
!JadeServer categoriesFor: #sbUpdateSuperclass!public!System Browser! !
!JadeServer categoriesFor: #symbolList!public!System Browser! !
!JadeServer categoriesFor: #systemBrowser:!public!System Browser! !
!JadeServer categoriesFor: #systemBrowserA:!public!System Browser! !
!JadeServer categoriesFor: #systemBrowserCommand!public!System Browser! !
!JadeServer categoriesFor: #systemBrowserUpdate!public!System Browser! !
!JadeServer categoriesFor: #writeList:!public!System Browser! !

!JadeServer32bit methodsFor!

systemBrowser: aString

	Exception
		category: nil 
		number: nil 
		do: [:ex :cat :num :args | 
			readStream := nil.
			ex resignal: cat number: num args: args.
			self error: 'Should not return!!'.
		].
	^super systemBrowser: aString.
! !
!JadeServer32bit categoriesFor: #systemBrowser:!public!System Browser! !

!JadeServer64bit methodsFor!

systemBrowser: aString

	[
		^super systemBrowser: aString.
	] on: Error do: [:ex |
		readStream := nil.
		ex pass.
	].
! !
!JadeServer64bit categoriesFor: #systemBrowser:!public!System Browser! !

!JadeServer64bit32 methodsFor!

dictionaryAndSymbolOf: aClass

	| array |
	array := self symbolList dictionariesAndSymbolsOf: aClass.
	^array isEmpty
		ifTrue: [nil]
		ifFalse: [array first].
! !
!JadeServer64bit32 categoriesFor: #dictionaryAndSymbolOf:!public! !

!JadeServer64bit3x methodsFor!

objectSecurityPolicyFor: anObject

	^anObject objectSecurityPolicy.
!

sbClassComment: anOrderedCollection

	(self sbClassFrom: anOrderedCollection) thisClass comment: self sbNextParagraph trimSeparators.
	self systemBrowserUpdate.
!

sbMethod: anOrderedCollection

	| behavior category string gsMethod |
	behavior := self sbClassFrom: anOrderedCollection.
	category := anOrderedCollection notEmpty ifTrue: [anOrderedCollection removeFirst] ifFalse: ['other'].
	string := self sbNextParagraph.
	gsMethod := behavior
		compileMethod: string 
		dictionaries: self symbolList 
		category: category asSymbol 
		environmentId: 0.
	selections 
		at: #'methodCategory' 	put: (gsMethod inClass categoryOfSelector: gsMethod selector) asString;
		at: #'method'					put: gsMethod selector asString;
		yourself.
	self systemBrowserUpdate.
!

sbUpdateMethodBreakPointsFor: aMethod
	"Answers an Array of step points"

	| list array |
	(array := aMethod _allBreakpoints) ifNil: [^#()].      "{ breakpointNumber1 . method . ipOffset1 . ... }"
	list := Array new.
	1 to: array size by: 3 do:[:k |
		list add: (aMethod
			_stepPointForMeth: (array at: k + 1)
			ip: (array at: k + 2)).
	].
	^list.
!

sbUpdateMethodStepPointsFor: aMethod
	"Answers an Array of Associations (offset -> selector) indexed by step point"

	|  selectors list |
	selectors := aMethod _allDebugInfo: 10.
	list := aMethod _sourceOffsets collect: [:each | 		"exists as far back as 32-bit 6.3.0"
		| index selector |
		selector := ''.
		index := selectors indexOf: each.
		0 < index ifTrue: [selector := selectors at: index + 1].
		each -> selector.
	].
	^list.

! !
!JadeServer64bit3x categoriesFor: #objectSecurityPolicyFor:!public!System Browser! !
!JadeServer64bit3x categoriesFor: #sbClassComment:!public!System Browser! !
!JadeServer64bit3x categoriesFor: #sbMethod:!public!System Browser! !
!JadeServer64bit3x categoriesFor: #sbUpdateMethodBreakPointsFor:!public!System Browser! !
!JadeServer64bit3x categoriesFor: #sbUpdateMethodStepPointsFor:!public!System Browser! !

!JadeTextDocument methodsFor!

jadeBrowseClasses

	gciSession hasServer ifTrue: [
		^JadeSystemBrowser showOn: gciSession.
	].
	MessageBox
		warning: 'Server initialization failed at login.'
		caption: 'Unable to Open Browser'.
! !
!JadeTextDocument categoriesFor: #jadeBrowseClasses!Jade!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

JadeSystemBrowserPresenter guid: (GUID fromString: '{AA74F365-5E98-46B2-AF4C-19A7BF1E6E97}')!
JadeSystemBrowserPresenter comment: ''!
!JadeSystemBrowserPresenter categoriesForClass!Unclassified! !
!JadeSystemBrowserPresenter methodsFor!

aboutToChange: aSelectionChangingEvent

	aSelectionChangingEvent value ifTrue: [
		aSelectionChangingEvent value: self isOkayToChange.
	].
!

aboutToEditClassLabel: oldName accept: aValueHolder

	aValueHolder value: true.
!

addClassCategoryInfoTo: aStream

	| category |
	category := (classCategoryPresenter selectionIfNone: [#() -> nil]) key.
	category isEmpty ifTrue: [category := #('')].
	category do: [:each | 
		aStream nextPutAll: each; nextPut: $-.
	].
	aStream lf.
!

addClassHierarchyInfoTo: aStream

	| selections |
	aStream nextPutAll: classHierarchyTabs currentCard name; lf.
	self isClassListTabSelected ifTrue: [
		(selections := classListPresenter selections) isEmpty ifTrue: [
			selections := Array with: self selectedClassNameWithoutVersion.
		].
	] ifFalse: [
		selections := Array with: (classHierarchyPresenter selectionIfNone: [Array with: self selectedClassNameWithoutVersion]) last.
	].
	selections do: [:each | aStream nextPutAll: each; tab].
	aStream lf.
!

addMethodCategory

	| string stream |
	(string := Prompter prompt: 'New method category?') isNil ifTrue: [^self].
	stream := (WriteStream on: String new)
		nextPutAll: 'addMethodCategory'; tab;
		nextPutAll: self selectedClassNameWithoutVersion; tab;
		nextPutAll: selectedClassOop printString; tab;
		nextPutAll: instanceClassTabs currentCard name; tab;
		nextPutAll: string; tab; lf;
		yourself.
	self 
		updateCommand: stream contents , self requestString
		onSuccessDo: [methodSourcePresenter ensureVisible].

!

addMethodInfoTo: aStream

	| string |
	selectedClassChanged ifTrue: [
		aStream 
			nextPutAll: instanceClassTabs currentCard name; lf;
			lf;	"superclassList selection"
			nextPutAll: 'categoryList'; lf;
			lf;	"method filter selections"
			lf; "overrideList selection"
			yourself.
		^self.
	].
	string := superclassListPresenter selectionIfNone: [''].
	aStream 
		nextPutAll: instanceClassTabs currentCard name; lf;			"instanceTab or classTab"
		nextPutAll: string; lf;															"selected superclass"
		nextPutAll: categoryVariableTabs currentCard name; lf;		"categoryList or variableList"
		yourself.
	self methodFilterListPresenter selections do: [:each | aStream nextPutAll: each trimBlanks; tab].
	aStream lf.
	methodListPresenter selections do: [:each | aStream nextPutAll: each first; tab].
	string := overrideListPresenter selectionIfNone: [''].
	(string includes: Character space) ifTrue: [string := string subStrings first].
	aStream 
		lf;
		nextPutAll: string; lf;
		yourself.
!

addMissingAccessors

	| string |
	string := 'addMissingAccessors' , Character tab asString , self behaviorIdentifier , Character lf asString , self requestString.
	self updateCommand: string.
!

addPackage

	| string stream |
	(string := Prompter prompt: 'New package name?') isNil ifTrue: [^self].
	stream := WriteStream on: String new.
	stream
		nextPutAll: 'addPackage'; tab;
		nextPutAll: string; tab;
		lf.
	self updateCommand: stream contents , self requestString.
!

addPackageDictionaryInfoTo: aStream

	| tabName |
	tabName := packageDictionaryTabs currentCard name.
	aStream nextPutAll: tabName; lf.
	tabName = 'packageList' ifTrue: [
		packageListPresenter selections do: [:each | aStream nextPutAll: each key; tab].
	] ifFalse: [
		dictionaryListPresenter selections do: [:each | aStream nextPutAll: each key; tab].
	].
	aStream lf.
!

addRepository

	| string list selection stream |
	string := self gciSession serverPerform: #'mcRepositoryList'.
	list := (string subStrings: Character lf) reject: [:each | each isEmpty].
	list := list collect: [:each | (each subStrings: Character tab) at: 2].
	list := list asSortedCollection.
	selection := ChoicePrompter 
		choices: list 
		caption: 'Select Repository'.
	selection isNil ifTrue: [^self].
	stream := (WriteStream on: String new)
		nextPutAll: 'addRepository'; tab;
		nextPutAll: selection;
		yourself.
	packageListPresenter selections do: [:each | stream tab; nextPutAll: each key].
	stream lf.
	self updateCommand: stream contents , self requestString.
!

addSubclass

	| i j superclass string |
	self isOkayToChange ifFalse: [^self].
	i := classDefinition indexOfSubCollection: '''' startingAt: 1.
	j := classDefinition indexOfSubCollection: '''' startingAt: i + 1.
	superclass := classDefinition copyFrom: i + 1 to: j - 1.
	string := '(
' , superclass , ' subclass: ''MyNewSubclass''' , (classDefinition copyFrom: j + 1 to: classDefinition size).
	j := 1.
	[
		i := string indexOfSubCollection: '#(' startingAt: j.
		0 < i.
	] whileTrue: [
		j := string indexOfSubCollection: ')' startingAt: i + 1.
		string := (string copyFrom: 1 to: i + 1) , (string copyFrom: j to: string size).
	].
	j := 1.
	[
		i := string indexOfSubCollection: '#[' startingAt: j.
		0 < i.
	] whileTrue: [
		j := string indexOfSubCollection: ']' startingAt: i + 1.
		string := (string copyFrom: 1 to: i + 1) , (string copyFrom: j to: string size).
	].
	classDefinitionPresenter
		ensureVisible;
		value: string;
		yourself.
!

behaviorIdentifier

	^(WriteStream on: String new)
		nextPutAll: self selectedClassNameWithoutVersion; tab;
		nextPutAll: selectedClassOop printString; tab;
		nextPutAll: instanceClassTabs currentCard name;
		contents.
!

breakAt: anInteger operation: aString

	| stream |
	stream := WriteStream on: String new.
	stream
		nextPutAll: 'break'; tab;
		nextPutAll: self selectedClassNameWithoutVersion; tab;
		nextPutAll: selectedClassOop printString; tab;
		nextPutAll: instanceClassTabs currentCard name; tab;
		nextPutAll: methodListPresenter selection first; tab;
		nextPutAll: anInteger printString; tab;
		nextPutAll: aString; tab;
		lf.
	self updateCommand: stream contents , self requestString.
!

browseClassReferences

	| string |
	string := 'browseClassReferences' , Character tab asString , self behaviorIdentifier , Character lf asString.
	(string := self updateCommand: string) = 'browseClassReferences' ifFalse: [self error: 'Unrecognized response'].
	self browseMethodsAndSelect: selectedClassName.!

browseGlobalReferences

	| string |
	string := 'browseGlobalReferences' , Character tab asString , (globalsPresenter selection at: 5) , Character lf asString.
	(string := self updateCommand: string) = 'browseGlobalReferences' ifFalse: [self error: 'unrecognized response'].
	self browseMethodsAndSelect: (globalsPresenter selection at: 1).
!

browseImplementors

	self browseImplementorsOf: methodListPresenter selections first first.
!

browseImplementorsOf

	| selector list stream string |
	(selector := self promptForSelector) isNil ifTrue: [^self].
	selector := selector reject: [:each | each = Character space].
	(selector includes: $*) ifFalse: [
		self browseImplementorsOf: selector.
		^self.
	].
	list := (selector subStrings: $*) asOrderedCollection collect: [:each | each asUppercase].
	list size - 1 to: 1 do: [:i | list add: '*' afterIndex: i].
	selector last = $* ifTrue: [list addLast: '*'].
	stream := (WriteStream on: String new)
		nextPutAll: 'findSelectors';
		yourself.
	list do: [:each | stream tab; nextPutAll: each].

	string := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: stream contents.
	list := string subStrings: Character lf.
	list := list copyFrom: 2 to: list size.
	(selector := ChoicePrompter choices: list) isNil ifTrue: [^self].
	self browseImplementorsOf: selector.
!

browseImplementorsOf: aString

	| string |
	string := 'browseImplementors' , Character tab asString , aString , Character lf asString.
	(self updateCommand: string) = 'browseImplementors' ifFalse: [self error: 'unexpected response'].
	self browseMethodsAndSelect: aString.
!

browseMethodHistory

	| string |
	string := 'browseMethodHistory' , Character tab asString , self methodsIdentifier , Character lf asString.
	(self updateCommand: string) = 'browseMethodHistory' ifFalse: [self error: 'unexpected response'].
	readStream atEnd ifTrue: [
		MessageBox notify: 'No history available!!'.
		^self.
	].
	(JadeMethodHistoryBrowser showOn: model)
		setContents: readStream;
		yourself.
!

browseMethodsAndSelect: aString

	(JadeMethodListBrowser showOn: self gciSession) 
		browseMethodsFromString: readStream upToEnd;
		selecting: aString.
!

browseMethodsContaining

	| searchString commandString |
	(searchString := Prompter prompt: 'Enter string:') isNil ifTrue: [^self].
	commandString := 'browseMethodsContaining' , Character tab asString , searchString , Character lf asString.
	(self updateCommand: commandString) = 'browseMethodsContaining' ifFalse: [self error: 'unexpected response'].
	self browseMethodsAndSelect: searchString.
!

browseSenders

	self browseSendersOf: methodListPresenter selections first first.
!

browseSendersOf

	| string |
	(string := self promptForSelector) isNil ifTrue: [^self].
	self browseSendersOf: string.
!

browseSendersOf: aString

	| string |
	string := 'browseSenders' , Character tab asString , aString , Character lf asString.
	(self updateCommand: string) = 'browseSenders' ifFalse: [self error: 'unexpected response'].
	self browseMethodsAndSelect: aString.!

categoriesMenuStrings

	false ifTrue: [
		self addMethodCategory; removeMethodCategories.
	].
	^#(
		'&Categories'
		'&Add Method Category//addMethodCategory'
		'&Remove Method Categories//removeMethodCategories'
	).
!

classDefChanged

	inUpdate ifTrue: [^self].
	classDefinitionPresenter value = classDefinition ifTrue: [
		classDefinitionPresenter view 
			backcolor: Color white;
			isModified: false;
			yourself.
	] ifFalse: [
		classDefinitionPresenter view 
			backcolor: (Color red: 255 green: 240 blue: 240);
			isModified: true;
			yourself.
	].
!

classesMenuStrings

	false ifTrue: [
		self browseClassReferences; fileOutClass; addSubclass; addMissingAccessors; removeClass; removePriorVersions; runClassTests.
	].
	^#(
		'&Classes'
		'&Browse References//browseClassReferences'
		'&File Out Class//fileOutClass'
		'Add &Subclass//addSubclass'
		'Add &Missing Accessors//addMissingAccessors'
		'&Remove//removeClass'
		'Remove Prior &Versions//removePriorVersions'
		'-'
		'Run &Tests//runClassTests'
	).
!

clearPackageInfo

	ancestorListPresenter list: #().
	repositoryListPresenter list: #().

!

closeRequested: anAssociation

	anAssociation value ifTrue: [
		anAssociation value: self isOkayToChange.
	].

!

compareAncestor

	| repository stream patch |
	repository := repositoryListPresenter hasSelection 
		ifTrue: [repositoryListPresenter selection]
		ifFalse: [repositoryListPresenter list first].
	stream := (WriteStream on: String new)
		nextPutAll: 'comparePackages'; tab;
		nextPutAll: packageListPresenter selections first key; tab;
		nextPutAll: (ancestorListPresenter selections first at: 2); tab;
		nextPutAll: (repository at: 2); tab;
		lf.
	(self updateCommand: stream contents) = 'comparePackages' ifFalse: [self error: 'Unexpected response!!'].
	patch := MCPatch
		fromString: readStream upToEnd
		session: self gciSession.
	patch operations isEmpty ifTrue: [
		MessageBox notify: 'No changes!!'.
		^self.
	].
	MCPatchBrowser showOn: patch.
!

contextObject

	^nil.
!

createComponents

	ancestorListPresenter 		:= self add: ListPresenter			new 		name: 'ancestorList'.
	categoryListPresenter		:= self add: ListPresenter			new 		name: 'categoryList'.
	classCategoryPresenter		:= self add: TreePresenter			new 		name: 'classCategoryList'.
	classCommentPresenter 	:= self add: TextPresenter 		new 		name: 'classDocumentation'.
	classDefinitionPresenter	:= self add: JadeTextPresenter 	new 		name: 'classDefinition'.
	classHierarchyPresenter	:= self add: TreePresenter			new 		name: 'classHierarchy'.
	classListPresenter				:= self add: ListPresenter			new 		name: 'classList'.
	dictionaryListPresenter 		:= self add: ListPresenter 			new 		name: 'dictionaryList'.
	globalsPresenter				:= self add: ListPresenter			new 		name: 'globals'.
	methodListPresenter			:= self add: ListPresenter			new 		name: 'methodList'.
	methodSourcePresenter		:= self add: self newMethodPresenter 	name: 'methodSource'.
	originalSourcePresenter		:= self add: self newMethodPresenter 	name: 'originalSource'.
	overrideListPresenter		:= self add: ListPresenter			new 		name: 'overrideList'.
	packageListPresenter 		:= self add: ListPresenter			new 		name: 'packageList'.
	repositoryListPresenter		:= self add: ListPresenter			new 		name: 'repositoryList'.
	superclassListPresenter		:= self add: ListPresenter			new 		name: 'superclassList'.
	variableListPresenter			:= self add: ListPresenter			new 		name: 'variableList'.
!

createSchematicWiring

	super createSchematicWiring.
	self 
		getViews;
		createSchematicWiringForClassList;
		createSchematicWiringForClassHierarchy;
		createSchematicWiringForMethodList;
		createSchematicWiringForDictionaryList;
		createSchematicWiringForPackageList;
		createSchematicWiringForPackageDictionaryTabs;
		createSchematicWiringForMethodSource;
		createSchematicWiringForClassDefinition;
		createSchematicWiringForMethodCategoryList;
		createSchematicWiringForVariableList;
		createSchematicWiringForClassCategoryList;
		createSchematicWiringForSuperClassList;
		createSchematicWiringForOverrideList;
		createSchematicWiringForInstanceClassTabs;
		yourself.
	textAreaTabs					when: #'currentCardChanged'	send: #'textTabChanged'		to: self.
	classHierarchyTabs			when: #'currentCardChanging:'	send: #'aboutToChange:'		to: self;		when: #'currentCardChanged'	send: #'updateAndSelect:'		to: self	with: classDefinitionPresenter.
	categoryVariableTabs		when: #'currentCardChanging:'	send: #'aboutToChange:'		to: self;		when: #'currentCardChanged'	send: #'updateAndSelect:'		to: self	with: classDefinitionPresenter.
	globalsPresenter				when: #'actionPerformed'			send: #'inspectGlobal'			to: self.
!

createSchematicWiringForClassCategoryList

	classCategoryPresenter		
		when: #'dragOver:'				send: #'onDragOverClassCategory:'	to: self;
		when: #'drop:'						send: #'onDropOnClassCategory:'		to: self;
		when: #'keyTyped:'				send: #'delayUpdate'							to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'					to: self;		
		when: #'selectionChanged'		send: #'updateAndSelect:'					to: self	with: classDefinitionPresenter;
		yourself.
!

createSchematicWiringForClassDefinition

	classDefinitionPresenter
		when: #'valueChanged'					send: #'classDefChanged'			to: self;
		yourself.
!

createSchematicWiringForClassHierarchy

	classHierarchyPresenter	
		when: #'drag:'						send: #'onDragClassHierarchy:'			to: self;
		when: #'dragCut:'					send: #'onDragCutClassHierarchy:'	to: self;
		when: #'dragOver:'				send: #'onDragOverClassHierarchy:'	to: self;
		when: #'drop:'						send: #'onDropOnClassHierarchy:'		to: self;
		when: #'keyTyped:'				send: #'delayUpdate'							to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'					to: self;
		when: #'selectionChanged'		send: #'selectedClassChanged:'			to: self	with: true;
		when: #'selectionChanged'		send: #'updateAndSelect:'					to: self	with: classDefinitionPresenter;
		yourself.
!

createSchematicWiringForClassList

	classListPresenter				
		when: #'drag:'								send: #'onDragClassList:'						to: self;
		when: #'dragCut:'							send: #'onDragCutClassList:'					to: self;
		when: #'dragOver:'						send: #'onDragOverClassList:'				to: self;
		when: #'drop:'								send: #'onDropOnClassList:'					to: self;
		when: #'keyTyped:'						send: #'delayUpdate'								to: self;
		when: #'selectionChanging:'			send: #'aboutToChange:'						to: self;
		when: #'selectionChanged'				send: #'selectedClassChanged:'				to: self	with: true;
		when: #'selectionChanged'				send: #'updateAndSelect:'						to: self	with: classDefinitionPresenter;

		when: #labelOf:changedTo: 			send: #labelOfClass:changedTo:				to: self;
		when: #aboutToEditLabel:accept: 	send: #aboutToEditClassLabel:accept:	to: self;
		when: #labelOf:editedTo:accept: 	send: #labelOfClass:editedTo:accept:		to: self;

		yourself.
!

createSchematicWiringForDictionaryList

	dictionaryListPresenter
		when: #'dragOver:'				send: #'onDragOverDictionary:'		to: self;
		when: #'drop:'						send: #'onDropOnDictionary:'		to: self;
		when: #'keyTyped:'				send: #'delayUpdate'						to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'				to: self;
		when: #'selectionChanged'		send: #'updateAndSelect:'				to: self	with: globalsPresenter;
		yourself.
!

createSchematicWiringForInstanceClassTabs

	instanceClassTabs
		when: #'currentCardChanging:'	send: #'aboutToChange:'			to: self;
		when: #'currentCardChanged'	send: #'selectedClassChanged:'	to: self	with: true;
		when: #'currentCardChanged'	send: #'updateAndSelect:'			to: self	with: classDefinitionPresenter;
		yourself.
!

createSchematicWiringForMethodCategoryList

	categoryListPresenter
		when: #'dragOver:'				send: #'onDragOverMethodCategory:'	to: self;
		when: #'drop:'						send: #'onDropOnMethodCategory:'		to: self;
		when: #'keyTyped:'				send: #'delayUpdate'								to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'						to: self;
		when: #'selectionChanged'		send: #'updateAndSelect:'						to: self	with: methodSourcePresenter;
		when: #'selectionChanged'		send: #'updateTabLabel'						to: self;
		yourself.
!

createSchematicWiringForMethodList

	methodListPresenter			
		when: #'drag:'						send: #'onDragMethod:'			to: self;
		when: #'dragCut:'					send: #'onDragCutMethod:'	to: self;
		when: #'keyTyped:'				send: #'delayUpdate'				to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'		to: self;
		when: #'selectionChanged'		send: #'updateAndSelect:'		to: self	with: methodSourcePresenter;
		yourself.
!

createSchematicWiringForMethodSource

	methodSourcePresenter 	
		when: #'hoverStart:'						send: #'methodHoverStart:'		to: self;
		when: #'hoverEnd:'						send: #'methodHoverEnd:'			to: self;
		when: #'aboutToDisplayMenu:'		send: #'methodMenu:'				to: self;
		when: #'leftButtonDoubleClicked:'	send: #'methodDoubleClicked:'	to: self;
		when: #'valueChanged'					send: #'methodChanged'			to: self;
		when: #'focusLost'						send: #'cancelCallTip'				to: methodSourcePresenter view;
		yourself.
!

createSchematicWiringForOverrideList

	overrideListPresenter		
		when: #'keyTyped:'					send: #'delayUpdate'				to: self;
		when: #'selectionChanging:'		send: #'aboutToChange:'		to: self;		
		when: #'selectionChanged'			send: #'updateAndSelect:'		to: self	with: methodSourcePresenter;
		yourself.
!

createSchematicWiringForPackageDictionaryTabs

	packageDictionaryTabs
		when: #'currentCardChanging:'	send: #'aboutToChange:'		to: self;
		when: #'currentCardChanged'	send: #'pkgDictChanged'		to: self;
		when: #'currentCardChanged'	send: #'updateAndSelect:'		to: self	with: packageInfoTab;
		yourself.
!

createSchematicWiringForPackageList

	packageListPresenter
		when: #'drag:'						send: #'onDragPackageList:'			to: self;
		when: #'dragCut:'					send: #'onDragCutPackageList:'		to: self;
		when: #'dragOver:'				send: #'onDragOverPackageList:'	to: self;
		when: #'drop:'						send: #'onDropOnPackageList:'		to: self;
		when: #'keyTyped:'				send: #'delayUpdate'						to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'				to: self;
		when: #'selectionChanged' 	send: #'ensureVisible'					to: packageInfoTab;
		when: #'selectionChanged' 	send: #'list:' 									to: globalsPresenter 	with: #();
		when: #'selectionChanged'		send: #'updateAndSelect:'				to: self						with: packageInfoTab;
		yourself.
!

createSchematicWiringForSuperClassList

	superclassListPresenter		
		when: #'keyTyped:'					send: #'delayUpdate'				to: self;
		when: #'selectionChanging:'		send: #'aboutToChange:'		to: self;		
		when: #'selectionChanged'			send: #'updateAndSelect:'		to: self	with: methodSourcePresenter;
		yourself.
!

createSchematicWiringForVariableList

	variableListPresenter
		when: #'keyTyped:'				send: #'delayUpdate'							to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'					to: self;
		when: #'selectionChanged'		send: #'updateAndSelect:'					to: self	with: methodSourcePresenter;
		when: #'selectionChanged'		send: #'updateTabLabel'					to: self;
		yourself.
!

currentMethodSource

	| source fromStream writeStream |
	fromStream := ReadStream on: methodSourcePresenter value.
	writeStream := WriteStream on: String new.
	[
		fromStream atEnd not.
	] whileTrue: [
		| char |
		(char := fromStream next) == Character cr ifTrue: [
			fromStream peek ~~ Character lf ifTrue: [
				writeStream nextPut: Character lf.
			].
		] ifFalse: [
			writeStream nextPut: char.
		].
	].
	source := writeStream contents.
	[
		source last codePoint <= 32.
	] whileTrue: [
		source := source copyFrom: 1 to: source size - 1.
	].
	^source.
!

currentSelectionOrLine

	View focus hasSelection ifFalse: [View focus selectCurrentLine].
	^View focus selection replaceCrLfWithLf.
!

defaultFileExtension

	^'gs'.
!

delayUpdate
	"Sent by various key-press events. If someone is typing in a list, the list will update
	to the letters typed. If someone is typing several keys in in a row, we don't want to
	go to the server for every keystroke."

	keystrokeTime := Time millisecondClockValue + 500.
!

dictsMenuStrings

	false ifTrue: [
		self browseDictionaryReferences; fileOutDictionary; findClass; insertDictionary; removeDictionary; setHomeDictionary.
	].
	^#(
		'&Dictionaries'
		'&Browse References//browseDictionaryReferences'
		'&File Out Dictionary//fileOutDictionary'
		'&Find Class/Ctrl+Shift+F/findClass'
		'&Insert Dictionary//insertDictionary'
		'&Remove Dictionary//removeDictionary'
		'&Set Home Dictionary//setHomeDictionary'
	).
!

editCopy

	View focus copySelection.
!

editCut

	View focus cutSelection.
!

editDelete

	View focus clearSelection.
	self methodChanged.
!

editFind
	"I'm not sure how it works, but this method isn't called!! 
	Somehow, the command is sent directly to the text widget."

self error: 'Do we get here?'.
	View focus editFind.
!

editFindNext

	View focus findNext.
!

editMenuStrings

	false ifTrue: [
		self editSave; editUndo; editRedo; editCut; editCopy; editPaste; editDelete; editSelectAll; editFind; editFindNext; editReplace; jadeDisplay; jadeExecute; jadeInspect.
	].
	^#(
		'&Edit'
		'&Save/Ctrl+S/editSave'
		'-'
		'&Undo/Ctrl+Z/editUndo'
		'&Redo/Ctrl+Y/editRedo'
		'-'
		'&Reformat Source/Ctrl+R/reformatSource'
		'-'
		'&Cu&t/Ctrl+X/editCut'
		'&Copy/Ctrl+C/editCopy'
		'&Paste/Ctrl+V/editPaste'
		'&Delete/Delete/editDelete'
		'-'
		'Select &All/Ctrl+A/editSelectAll'
		'-'
		'&Find/Ctrl+F/editFind'
		'Find &Next/F3/editFindNext'
		'&Replace/Ctrl+H/editReplace'
		'-'
		'Display/Ctrl+D/jadeDisplay'
		'Execute/Ctrl+E/jadeExecute'
		'Inspect/Ctrl+Q/jadeInspect'
	).
!

editPaste

	View focus 
		pasteClipboard;
		updateModel;
		yourself.
!

editRedo

	View focus redo.
!

editReplace

	View focus findReplace.
!

editSave

	| currentCard |
	currentCard := textAreaTabs view currentCard.
	currentCard = classDefinitionPresenter view ifTrue: [^self editSaveClass].
	currentCard = methodSourcePresenter view ifTrue: [^self editSaveMethod].
	currentCard = classCommentPresenter view ifTrue: [^self editSaveClassComment].
	MessageBox notify: 'Save menu not effective for this text area tab'.
!

editSaveClass

	| string stream flag |
	string := classDefinitionPresenter value collect: [:each | each codePoint < 32 ifTrue: [Character space] ifFalse: [each]].
	stream := (WriteStream on: String new)
		nextPutAll: 'class'; tab;
		nextPutAll: string; lf;
		yourself.
	flag := false.
	[
		self 
			updateCommand: stream contents , self requestString
			onSuccessDo: [
				classDefinitionPresenter view isModified: false.
				flag := true.
			].
	] on: GsCompileError do: [:ex | 
		| offset explanation |
		offset := ex list first at: 2.
		explanation := ex list first at: 3.
		string := classDefinitionPresenter value.
		string := (string copyFrom: 1 to: offset - 1) , ' ' , explanation , ' ' , (string copyFrom: offset to: string size).
		classDefinitionPresenter 
			value: string;
			selectionRange: (offset to: offset + explanation size + 1);
			yourself.
		^self.
	].
	flag ifTrue: [self editSaveClassA].
!

editSaveClassA

	| dict stream answer |
	(selectedClassName includes: Character space) ifFalse: [^self].		"This is a check to see if an earlier version exists"
	(dict := JadeMigrateClassDialog showModal) isNil ifTrue: [^self revertNewClass].
	(dict allSatisfy: [:each | each not]) ifTrue: [^self].
	(stream := WriteStream on: String new)
		nextPutAll: 'postSaveClass'; tab;
		nextPutAll: self behaviorIdentifier; tab;
		yourself.
	(dict at: #copyMethods) printOn: stream. 					stream tab.
	(dict at: #recompileSubclasses) printOn: stream. 			stream tab.
	(dict at: #migrateInstances) printOn: stream. 				stream tab.
	(dict at: #removeFromClassHistory) printOn: stream. 	stream lf.
	(answer := self updateCommand: stream contents , self requestString) isNil ifTrue: [^self].
	[
		answer = 'compileError'.
	] whileTrue: [
		(JadeWorkspace showOn: self gciSession)
			caption: 'Jade Workspace - Compile errors found when compiling class';
			showText: self nextParagraph.
		answer := self nextLine.
	].
	answer = 'update' ifFalse: [self error: 'Unexpected token'].
	self updatePresenters.
!

editSaveClassComment

	| stream |
	stream := (WriteStream on: String new)
		nextPutAll: 'classComment'; tab;
		nextPutAll: self selectedClassNameWithoutVersion; tab;
		nextPutAll: selectedClassOop printString; tab;
		nextPutAll: instanceClassTabs currentCard name; lf;
		nextPutAll: classCommentPresenter value trimBlanks; lf;
		nextPut: $%; lf;
		yourself.
	self 
		updateCommand: stream contents , self requestString
		onSuccessDo: [classCommentPresenter view isModified: false].
!

editSaveMethod

	| stream |
	(self isCategoriesTabSelected and: [categoryListPresenter hasSelection]) ifTrue: [
		methodCategory := categoryListPresenter selection trimBlanks.
	].
	stream := (WriteStream on: String new)
		nextPutAll: 'method'; tab;
		nextPutAll: self selectedClassNameWithoutVersion; tab;
		nextPutAll: selectedClassOop printString; tab;
		nextPutAll: instanceClassTabs currentCard name; tab;
		nextPutAll: methodCategory; tab; 
		lf;
		nextPutAll: self currentMethodSource; lf;
		nextPut: $%; lf;
		yourself.
	[
		self 
			updateCommand: stream contents , self requestString
			onSuccessDo: [methodSourcePresenter view isModified: false; ensureVisible].
	] on: GsCompileError do: [:ex | 
		self reportCompileError: ex list.
	].
!

editSelectAll

	View focus selectAll.
!

editUndo

	View focus undo.
!

executeSelectionOrLine

	[
		^true -> (self gciSession executeString: self currentSelectionOrLine fromContext: self contextObject).
	] on: GsCompileError do: [:ex | 
		^false -> ex list.
	].
	^false -> #(nil).
!

fileOutClass

	| path string |
	((string := self selectedClassNameWithoutVersion) endsWith: 'TestCase') ifTrue: [
		string := string copyFrom: 1 to: string size - 8.
	].
	path := FileSaveDialog new
		caption: 'File Out ' , self selectedClassNameWithoutVersion;
		fileTypes: self fileTypes;
		defaultExtension: self defaultFileExtension;
		value: string;
		overwritePrompt;
		showModal.
	path ifNotNil: [:value | self fileOutClassOnPath: value].
!

fileOutClassOnPath: aString

	| className header file newSource index |
	className := self selectedClassNameWithoutVersion.
	header := self stuffToKeepFromOldFileForClass: className onPath: aString.
	newSource := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: 'fileOutClass' , Character tab asString , self behaviorIdentifier.
	index := newSource indexOf: Character lf.
	newSource := newSource copyFrom: index + 1 to: newSource size.
	file := FileStream write: aString.
	[
		file nextPutAll: header; nextPutAll: newSource.
	] ensure: [
		file close.
	].
!

fileOutDictionary

	| dictionaryName path string file index |
	dictionaryName :=  dictionaryListPresenter selection key.
	path := FileSaveDialog new
		caption: 'File Out ' , dictionaryName;
		fileTypes: self fileTypes;
		defaultExtension: self defaultFileExtension;
		value: dictionaryName , '.gs';
		overwritePrompt;
		showModal.
	path isNil ifTrue: [^self].
	string := 'fileOutDictionary' , Character tab asString , dictionaryName.
	string := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: string.
	index := string indexOf: Character lf.
	file := FileStream write: path.
	[
		file nextPutAll: (string copyFrom: index + 1 to: string size).
	] ensure: [
		file close.
	].
!

fileTypes

	^Array
		with: #('GemStone Files (*.gs)' '*.gs')
		with: #('Smalltalk Files (*.st)' '*.st')
		with: FileDialog allFilesType.
!

findClass
"
	Array with: className with: dictionaryName with: catetory with: packageName.
"
	| string find list |
	string := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: 'findClass'.
	list := (string subStrings: Character lf) collect: [:each | each subStrings: Character tab].
	list := list copyFrom: 2 to: list size.
	list := list collect: [:each | each size < 3 ifTrue: [each , #('' '' '')] ifFalse: [each]].
	list := list collect: [:each | (each at: 1) -> each].
	ignoreNextSetFocusEvent := true.
	find := JadeFindClassDialog showModal: 'ThreeColumnView' on: list.
	find ifNil: [^self].
	self 
		updateAfterFindClass: find value
		isMeta: (find = ('System' -> #('System' 'Globals' 'User Classes' ''))) 
		selector: ''.
!

getViews

	packageDictionaryTabs := self view viewNamed: 'packageDictionaryTabs'.
	classHierarchyTabs 		:= self view viewNamed: 'classHierarchyTabs'.
	categoryVariableTabs 	:= self view viewNamed: 'categoryVariableTabs'.
	instanceClassTabs			:= self view viewNamed: 'instanceClassTabs'.
	textAreaTabs				:= self view viewNamed: 'textAreaTabs'.
	packageInfoTab			:= self view viewNamed: 'packageInfo'.
!

globalsMenuStrings

	false ifTrue: [
		self inspectGlobal; browseGlobalReferences; removeGlobals.
	].
	^#(
		'&Globals'
		'&Inspect//inspectGlobal'
		'&Browse References//browseGlobalReferences'
		'&Remove//removeGlobals'
	).
!

handleInvalidSession

	| hadDialog |
	hadDialog := false.
	inUpdate := true.
	methodSourcePresenter view isModified ifTrue: [
		self ensureVisible.
		methodSourcePresenter ensureVisible.
		methodSourcePresenter view isModified: false.
		(MessageBox confirm: 'Copy unsaved method to clipboard?' caption: 'Invalid Session!!') ifTrue: [
			methodSourcePresenter view selectAll; copySelection.
		].
		hadDialog := true.
	].
	classDefinitionPresenter view isModified ifTrue: [
		self ensureVisible.
		classDefinitionPresenter ensureVisible.
		classDefinitionPresenter view isModified: false.
		(MessageBox confirm: 'Copy unsaved class definition to clipboard?' caption: 'Invalid Session!!') ifTrue: [
			classDefinitionPresenter view selectAll; copySelection.
		].
		hadDialog := true.
	].
	^hadDialog.
!

horizontalSplitter

	^view 
		viewNamed: 'splitter' 
		ifNone: [self error: 'splitter is missing!!?'].
!

initialize

	super initialize.
	ignoreNextSetFocusEvent := false.
	inUpdate := false.
	selectedClassName := ''.
	eventCount := 0.
	selectedClassChanged := false.
	selectedClassesAreTestCases := false.
	keystrokeTime := 0.
	updateCount := 0.
!

insertDictionary

	| newName currentName stream |
	(newName := Prompter prompt: 'New dictionary name?') isNil ifTrue: [^self].
	currentName := dictionaryListPresenter selections notEmpty
		ifTrue: [dictionaryListPresenter selections first key]
		ifFalse: [''].
	stream := (WriteStream on: String new)
		nextPutAll: 'addDictionary'; tab;
		nextPutAll: newName; tab;
		nextPutAll: currentName; tab;
		lf;
		nextPutAll: self requestString;
		yourself.
	self updateCommand: stream contents.
!

inspectGlobal

	| oopType |
	oopType := self gciSession oopTypeWithOop: (globalsPresenter selection at: 4) asNumber.
	(Smalltalk at: #'JadeInspector' ifAbsent: [^self]) showOn: self gciSession -> oopType.
!

isCategoriesTabSelected

	^categoryVariableTabs currentCard name = 'categoryList'.
!

isClassListTabSelected

	^classHierarchyTabs currentCard name = 'classList'.
!

isDictionariesTabSelected

	^packageDictionaryTabs currentCard name = 'dictionaryList'.
!

isGlobalsTabSelected

	^textAreaTabs currentCard name = 'globals'.
!

isOkayToChange

	methodSourcePresenter view isModified ifTrue: [
		methodSourcePresenter ensureVisible.
		(MessageBox confirm: 'Stay on unsaved method?' caption: 'Method has unsaved changes!!') ifTrue: [^false].
		methodSourcePresenter view isModified: false.
	].
	classDefinitionPresenter view isModified ifTrue: [
		classDefinitionPresenter ensureVisible.
		(MessageBox confirm: 'Stay on unsaved class definition?' caption: 'Class has unsaved changes!!') ifTrue: [^false].
		classDefinitionPresenter view isModified: false.
	].
	^true.
!

isPackageListTabSelected

	^packageDictionaryTabs currentCard name = 'packageList'.
!

isTreeModel: treeModelA equivalentTo: treeModelB

	| listA listB |
	listA := (treeModelA asBag collect: [:each | each key printString]) asSortedCollection asArray.
	listB := (treeModelB asBag collect: [:each | each key printString]) asSortedCollection asArray.
	^listA = listB.
!

isVariablesTabSelected

	^categoryVariableTabs currentCard name = 'variableList'.
!

jadeDisplay

	self jadeExecuteAndDisplay: true.
!

jadeExecute

	self jadeExecuteAndDisplay: false.
!

jadeExecuteAndDisplay: aBoolean


	| textView result value selectionRange offset |
	textView := View focus.
	result := self executeSelectionOrLine.
	result key ifTrue: [
		value := result value.
		result := ''.
		aBoolean ifTrue: [
			(self gciSession isOopType: value) ifFalse: [
				result := ' ' , value printString.
			] ifTrue: [
				result := ' ' , (self gciSession printString: value).
			].
		].
		selectionRange := textView selectionRange.
		textView
			caretPosition: selectionRange stop + 1;
			replaceSelection: result;
			selectionStart: textView caretPosition - result size length: result size.
	] ifFalse: [
		| string count |
		(result := result value first) isNil ifTrue: [^self].
		offset := result at: 2.
		result := result at: 3.
		selectionRange := textView selectionRange.
		(string := textView text) size < selectionRange stop ifTrue: [
			self error: 'Select beyond the end of the text!!?'.
			"string := documentPresenter value."
		].
		string := string copyFrom: selectionRange start to: selectionRange stop.
		string := string replaceCrLfWithLf copyFrom: 1 to: offset - 1.
		count := (string select: [:each | each = Character lf]) size.
		offset := offset + count.
		textView
			caretPosition: selectionRange start + offset - 1;
			replaceSelection: result;
			selectionStart: textView caretPosition - result size length: result size.
	].
	^value.
!

jadeInspect

	| result |
	result := self jadeExecuteAndDisplay: false.
	JadeInspector showOn: self gciSession -> result.
!

jadeMenuStrings

	false ifTrue: [
		self abortTransaction; commitTransaction; newWorkspace; openSystemBrowser; rename; exit.
	].
	^#(
		'&Jade'
		'&Abort Transaction//abortTransaction'
		'&CommitTransaction//commitTransaction'
		'-'
		'&New Worspace/Ctrl+N/newWorkspace'
		'&Open System Browser/Ctrl+B/openSystemBrowser'
		'-'
		'&Rename Selection/F2/rename'
		'-'
		'E&xit/Shift+F4/exit'
	).
!

labelOfClass: oldName changedTo: newName

	| stream |
	oldName = self selectedClassNameWithoutVersion.
	stream := (WriteStream on: String new)
		nextPutAll: 'changeClassName'; tab;
		nextPutAll: self selectedClassNameWithoutVersion; tab;
		nextPutAll: selectedClassOop printString; tab;
		nextPutAll: newName; tab;
		lf; nextPutAll: self requestString;
		yourself.
	self selectedClassName: newName.
	self updateCommand: stream contents.
!

labelOfClass: oldName editedTo: newName accept: aValueHolder

	| stream string |
	oldName = self selectedClassNameWithoutVersion.
	stream := (WriteStream on: String new)
		nextPutAll: 'checkUniqueClassName'; tab;
		nextPutAll: oldName; tab;
		nextPutAll: selectedClassOop printString; tab;
		nextPutAll: newName; tab; lf;
		yourself.
	(string := self updateCommand: stream contents) notEmpty ifTrue: [
		MessageBox warning: string.
	] ifFalse: [
		aValueHolder value: true.
	].
!

layoutInfo

	^OrderedCollection new
		add: self requestStringForUpdate;
		add: self horizontalSplitter position;
		asArray.
!

layoutInfo: anArray

	view ensureVisible.
	anArray isNil ifTrue: [
		self updateAndSelect: nil.
		^self.
	].
	self 
		updateCommand: (anArray at: 1);
		updateMenuBar;
		yourself.
	methodListPresenter hasSelection ifTrue: [
		methodSourcePresenter ensureVisible.
	].
	[
		view layoutManager reposition: self horizontalSplitter to: (anArray at: 2).
		view layout.
	] postToInputQueue.
!

loadLatestVersion

	| stream |
	stream := WriteStream on: String new.
	stream nextPutAll: 'loadLatestVersion'.
	packageListPresenter selections do: [:each | stream tab; nextPutAll: each key].
	stream lf; nextPutAll: self requestString.
	self updateCommand: stream contents.
!

methodChanged

	inUpdate ifTrue: [^self].
	methodSourcePresenter value = methodSource ifTrue: [
		methodSourcePresenter view 
			backcolor: JadeTextPresenter colorForNoEdits;
			isModified: false;
			yourself.
		self updateMethodStepPoints.
		self statusBarText: ''.
	] ifFalse: [
		methodSourcePresenter view 
			backcolor: JadeTextPresenter colorForUnsavedEdits;
			clearContainerIndicators;
			yourself.
	].
!

methodDoubleClicked: anObject

	| range string |
	range := methodSourcePresenter view selectionRange.
	string := methodSourcePresenter value.
	string size = range stop ifTrue: [^self].
	(string at: range stop + 1) = $: ifFalse: [^self].
	range stop: range stop + 1.
	methodSourcePresenter view selectionRange: range.
!

methodFilterListPresenter

	^categoryVariableTabs currentCard name = 'categoryList'
		ifTrue: [categoryListPresenter]
		ifFalse: [variableListPresenter].
!

methodHoverEnd: anObject

	methodSourcePresenter view cancelCallTip.!

methodHoverStart: aPoint

	| charIndex indicator |
	charIndex := methodSourcePresenter view charNearestPosition: aPoint.
	indicator := methodSourcePresenter view indicators
		detect: [:each | each range includes: charIndex]
		ifNone: [^self].
	methodSourcePresenter view 
		showCallTip: indicator tag
		at: charIndex.
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

methodMenuBreak: aMenu

	| stepPoint args desc command message |
	methodSourcePresenter view isModified ifTrue: [^self].
	stepPoint := self stepPointAt: methodSourcePresenter view cursorPosition.
	stepPoint isNil ifTrue: [^self].
	(breakPoints includes: stepPoint) ifTrue: [
		args := Array with: stepPoint with: 'clear'.
		desc := 'Clear break at step point ' , stepPoint printString.
	] ifFalse: [
		args := Array with: stepPoint with: 'set'.
		desc := 'Break at step point ' , stepPoint printString.
	].
	command := MessageSend 
		receiver: self 
		selector: #'breakAt:operation:' 
		arguments: args.
	aMenu 
		addSeparator;
		addCommand: command description: desc;
		yourself.
	(message := (stepPoints at: stepPoint) value) isEmpty ifTrue: [^self].
	command := MessageSend
		receiver: self 
		selector: #'browseImplementorsOf:' 
		argument: message.
	aMenu
		addCommand: command
		description: 'Browse Implementors of #' , message printString.
	command := MessageSend
		receiver: self 
		selector: #'browseSendersOf:' 
		argument: message.
	aMenu
		addCommand: command
		description: 'Browse Senders of #' , message printString.
!

methodsIdentifier

	| stream |
	stream := (WriteStream on: String new)
		nextPutAll: self behaviorIdentifier;
		yourself.
	methodListPresenter selections do: [:each | stream tab; nextPutAll: each first].
	^stream contents.
!

methodsMenuStrings

	false ifTrue: [
		self browseImplementors; browseImplementorsOf; browseSenders; browseSendersOf; browseMethodsContaining; removeMethods; runMethodTests.
	].
	^#(
		'&Methods'
		'Browse &Implementors//browseImplementors'
		'Browse Implementors of ...//browseImplementorsOf'
		'Browse &Senders//browseSenders'
		'Browse Senders of ...//browseSendersOf'
		'Browse Methods &Containing ...//browseMethodsContaining'
		'Browse &History//browseMethodHistory'
		'-'
		'&Delete Method(s)//removeMethods'
		'Run &Tests//runMethodTests'
	).
!

methodSourcePresenter

	^methodSourcePresenter.
!

nextLine

	^readStream upTo: Character lf.
!

nextLineAsList

	^(self nextLine subStrings: Character tab) reject: [:each | each isEmpty].

!

nextList

	| list line writeStream |
	list := OrderedCollection new.
	[
		line := self nextLineAsList.
		line notEmpty and: [line first = '%'].
	] whileFalse: [
		list add: line.
	].
	^list.
!

nextParagraph

	| line writeStream |
	writeStream := WriteStream on: String new.
	[
		line := readStream upTo: Character lf.
		readStream atEnd or: [line = '%'].
	] whileFalse: [
		writeStream nextPutAll: line; lf.
	].
	^writeStream contents.
!

onDragClassesOverDictionary: aSession 

	(dictionaryListPresenter selections includes: aSession suggestedTarget) ifTrue: [^self].
	aSession
		supportedOperations: #(#'copy' #'move');
		operation: #'move';
		yourself.
!

onDragClassesOverPackageList: aSession

	aSession
		supportedOperations: #(#'move');
		operation: #'move';
		yourself.
!

onDragClassHierarchy: aSession 

	| class list |
	class := classHierarchyPresenter selection last.
	list := Array with: (
		(aSession newDragObject: class)
			format: #class data: class;
			yourself).
	aSession 
		dragObjects: list;
		supportedOperations: #(#move #copy);
		defaultOperation: #move;
		yourself.
!

onDragClassList: aSession 

	| list |
	list := classListPresenter selections collect: [:each | 
		(aSession newDragObject: each)
			format: #class data: each;
			yourself.
	].
	aSession 
		dragObjects: list;
		supportedOperations: #(#move #copy);
		defaultOperation: #move;
		yourself.
!

onDragCutClassHierarchy: aSession 
!

onDragCutClassList: aSession 
!

onDragCutMethod: aSession 

	"self halt."!

onDragMethod: aSession 

	| list |
	list := methodListPresenter selections collect: [:each | 
		(aSession newDragObject: each first)
			format: #method data: each first;
			yourself.
	].
	aSession 
		dragObjects: list;
		supportedOperations: #(#move #copy);
		defaultOperation: #copy;
		yourself.
!

onDragMethodsOverClassHierarchy: aSession

	aSession
		supportedOperations: #(#'copy' #'move');
		operation: #'copy';
		yourself.
!

onDragMethodsOverClassList: aSession

	aSession
		supportedOperations: #(#'copy' #'move');
		operation: #'copy';
		yourself.
!

onDragOverClassCategory: aSession 

	aSession operation: nil.
	aSession dragObjects isEmpty ifTrue: [^self].
	aSession suggestedTarget isNil ifTrue: [^self].
	(aSession isFormatAvailable: #'class') ifFalse: [^self].
	aSession
		supportedOperations: #(#'move');
		operation: #'move';
		yourself.
!

onDragOverClassHierarchy: aSession 

	aSession operation: nil.
	aSession dragObjects isEmpty 				ifTrue: [^self].
	aSession suggestedTarget isNil 				ifTrue: [^self].
	(aSession isFormatAvailable: #'class') 	ifTrue: [^self].
	(aSession isFormatAvailable: #'method') ifTrue: [^self onDragMethodsOverClassHierarchy: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onDragOverClassList: aSession 

	aSession operation: nil.
	aSession dragObjects isEmpty ifTrue: [^self].
	aSession suggestedTarget isNil ifTrue: [^self].
	(aSession isFormatAvailable: #'class')			ifTrue: [^self].
	(aSession isFormatAvailable: #'method') 	ifTrue: [^self onDragMethodsOverClassList: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onDragOverDictionary: aSession 

	aSession operation: nil.
	aSession dragObjects isEmpty ifTrue: [^self].
	aSession suggestedTarget isNil ifTrue: [^self].
	(aSession isFormatAvailable: #'class') 				ifTrue: [^self onDragClassesOverDictionary: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onDragOverMethodCategory: aSession 

	aSession operation: nil.
	aSession dragObjects isEmpty ifTrue: [^self].
	aSession suggestedTarget isNil ifTrue: [^self].
	(aSession isFormatAvailable: #'method') ifFalse: [^self].
	aSession
		supportedOperations: #(#'move');
		operation: #'move';
		yourself.
!

onDragOverPackageList: aSession 

	aSession operation: nil.
	aSession dragObjects isEmpty ifTrue: [^self].
	aSession suggestedTarget isNil ifTrue: [^self].
	(aSession isFormatAvailable: #'package') ifTrue: [^self].
	(aSession isFormatAvailable: #'class') 	ifTrue: [^self onDragClassesOverPackageList: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onDropClassesOnDictionary: aSession 

	| classNames stream |
	classNames := aSession dragObjects collect: [:each | each format: #'class'].
	stream := (WriteStream on: String new)
		nextPutAll: 'classesToDictionary'; tab;
		nextPutAll: aSession operation; tab;
		nextPutAll: aSession suggestedTarget key;
		lf.
	dictionaryListPresenter selections do: [:each | stream nextPutAll: each key; tab].
	stream lf.
	classNames do: [:each | stream nextPutAll: each; tab].
	stream lf; nextPutAll: self requestString.
	self updateCommand: stream contents.
!

onDropClassesOnPackage: aString session: aSession 

	| classes stream |
	classes := aSession dragObjects collect: [:each | each format: #'class'].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
"
	stream := (WriteStream on: String new)
		nextPutAll: 'methodClass'; tab;
		nextPutAll: self behaviorIdentifier; tab;
		nextPutAll: aSession operation; tab;
		nextPutAll: aString;
		yourself.
	classes do: [:each | stream tab; nextPutAll: each].
	stream lf; nextPutAll: self requestString.
	self updateCommand: stream contents.
"!

onDropClassesOnPackageList: aSession 

	self
		onDropClassesOnPackage: aSession suggestedTarget 
		session: aSession.!

onDropMethodsOnClass: aString session: aSession 

	| selectors stream |
	selectors := aSession dragObjects collect: [:each | each format: #'method'].
	stream := (WriteStream on: String new)
		nextPutAll: 'methodClass'; tab;
		nextPutAll: self behaviorIdentifier; tab;
		nextPutAll: aSession operation; tab;
		nextPutAll: aString;
		yourself.
	selectors do: [:each | stream tab; nextPutAll: each].
	stream lf; nextPutAll: self requestString.
	self updateCommand: stream contents.
!

onDropMethodsOnClassHierarchy: aSession 

	self
		onDropMethodsOnClass: aSession suggestedTarget last
		session: aSession.!

onDropMethodsOnClassList: aSession 

	self
		onDropMethodsOnClass: aSession suggestedTarget 
		session: aSession.!

onDropOnClassCategory: aSession 

	| classNames stream |
	classNames := aSession dragObjects collect: [:each | each format: #'class'].
	stream := (WriteStream on: String new)
		nextPutAll: 'classCategory'; tab;
		yourself.
	aSession suggestedTarget key do: [:each | stream nextPutAll: each; nextPut: $-].
	stream tab.
	classNames do: [:each | stream nextPutAll: each; space].
	stream lf; nextPutAll: self requestString.
	self updateCommand: stream contents.
!

onDropOnClassHierarchy: aSession 

	(aSession isFormatAvailable: #'method') 				ifTrue: [^self onDropMethodsOnClassHierarchy: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onDropOnClassList: aSession 

	(aSession isFormatAvailable: #'method') ifTrue: [^self onDropMethodsOnClassList: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onDropOnDictionary: aSession 

	(aSession isFormatAvailable: #'class') 				ifTrue: [^self onDropClassesOnDictionary: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onDropOnMethodCategory: aSession 

	| selectors stream |
	selectors := aSession dragObjects collect: [:each | each format: #'method'].
	stream := (WriteStream on: String new)
		nextPutAll: 'methodCategory'; tab;
		nextPutAll: self behaviorIdentifier; tab;
		nextPutAll: aSession suggestedTarget trimBlanks;
		yourself.
	selectors do: [:each | stream tab; nextPutAll: each].
	stream lf; nextPutAll: self requestString.
	self updateCommand: stream contents.
!

onDropOnPackageList: aSession 

	(aSession isFormatAvailable: #'class') 	ifTrue: [^self onDropClassesOnPackageList: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

onSetFocus

	| activeView stack |
	ignoreNextSetFocusEvent ifTrue: [
		ignoreNextSetFocusEvent := false.
		^self.
	].
	updateProcess ifNotNil: [updateProcess terminate].
	activeView := View active.
	stack := Processor activeProcess stackTrace: 40.
	updateProcess := [self onSetFocus: activeView stack: stack] forkAt: Processor userBackgroundPriority.
!

onSetFocus: aView stack: aString

	[
		| startTime activeView |
		startTime := TimeStamp current.
		(Delay forMilliseconds: 50) wait.
		aView ~~ (activeView := View active) ifTrue: [^self].		"If view changed then there is no point in updating it!!"
		view = DeafObject current ifTrue: [^self].
		eventCount = self gciSession eventCount ifTrue: [^self].
		super onSetFocus.
		[
			self updateAndSelect: nil.
		] on: Error do: [:ex | 
			SessionManager current logError: ex.
			MessageBox 
				errorMsg: ex description
				caption: 'Jade Error'.
			Keyboard default isShiftDown ifTrue: [
				| stream |
				SessionManager current isRuntime ifFalse: [ | x | x := TimeStamp current asMilliseconds - startTime asMilliseconds. (x -> aView -> activeView -> aString) halt].
				stream := WriteStream on: String new.
				ex printTraceOn: stream.
				(JadeWorkspace showOn: model) showText: stream contents.
			].
		].
	] ensure: [
		updateProcess := nil.
	].
!

onViewOpened

	super onViewOpened.
	self model: parentPresenter model.
	categoryListPresenter		view contextMenu: (Menu fromStrings: self categoriesMenuStrings	).
	classCommentPresenter	view isReadOnly: false.
	classHierarchyPresenter	view contextMenu: (Menu fromStrings: self classesMenuStrings		).
	classListPresenter				view contextMenu: (Menu fromStrings: self classesMenuStrings		).
	dictionaryListPresenter 		view contextMenu: (Menu fromStrings: self dictsMenuStrings			).
	globalsPresenter				view contextMenu: (Menu fromStrings: self globalsMenuStrings		).
	methodListPresenter			view contextMenu: (Menu fromStrings: self methodsMenuStrings	).
	methodSourcePresenter		view contextMenu: (Menu fromStrings: self editMenuStrings			);
		isBackgroundDwellEnabled: true;
		isBraceHighlightingEnabled: true;
		yourself.
	packageListPresenter 		view contextMenu: (Menu fromStrings: self pkgsMenuStrings			).
	variableListPresenter			view contextMenu: (Menu fromStrings: self variablesMenuStrings	).
	self setSearchPolicy.
!

openSourceStyler

	| textStyles |
	textStyles := methodSourcePresenter view editStyles textStyles.
	JadeTextPresenter textStyles: textStyles.
	classDefinitionPresenter view textStyles: textStyles.
	originalSourcePresenter view textStyles: textStyles.
!

parseContext

	^nil!

pkgDictChanged

	self isPackageListTabSelected
		ifTrue: [packageInfoTab ensureVisible]
		ifFalse: [globalsPresenter ensureVisible].
!

pkgsMenuStrings

	0 == 1 ifTrue: [
		self findClass; addPackage; loadLatestVersion; addRepository; removeRepository; savePackage; showPackageChanges; unloadPackage.
	].
	^#(
		'&Packages'
		'&Find Class.../Ctrl+Shift+F/findClass'
		'&Add Package...//addPackage'
		'-'
		'&Load Latest Version//loadLatestVersion'
		'Add &Repository...//addRepository'
		'Remo&ve Repository...//removeRepository'
		'&Save Package..//savePackage'
		'Show &Changes//showPackageChanges'
		'&Unload Package...//unloadPackage'
	).
!

preferencesMenuStrings

	false ifTrue: [		"adding explicit senders ensures that packaging will not drop methods and allows us to find references by browsing senders"
		self openSourceStyler; setColorForCompileErrors; setColorForNoEdits; setColorForUnsavedEdits.
	].
	^#(
		'&Preferences'
		'&Source Styler...//openSourceStyler'
		'-'
		'&Reset Colors//resetColors'
		'Color for &Compile Errors...//setColorForCompileErrors'
		'Color for &No Edits...//setColorForNoEdits'
		'Color for &Unsaved Edits...//setColorForUnsavedEdits'
	).
!

promptForSelector

	| string list stream |
	(string := Prompter prompt: 'Enter selector:') isNil ifTrue: [^nil].
	((string includes: $:) and: [string includes: Character space]) ifTrue: [
		list := string subStrings: Character space.
		list := list select: [:each | each last = $:].
		stream := WriteStream on: String new.
		list do: [:each | stream nextPutAll: each].
		string := stream contents.
	].
	string := string reject: [:each | each = Character space].
	^string.!

queryCommand: aCommandQuery

	| command focusView isTextEdit |
	command := aCommandQuery command.
	focusView := View focus.
	isTextEdit := focusView isKindOf: TextEdit.
	(#(#'editSave') includes: command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [focusView isModified]). ^true].
	(#(#'editUndo') includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [focusView canUndo]). ^true].
	(#(#'editRedo') includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [focusView canRedo]). ^true].
	(#(#'editCut' #'editCopy' "#'editDelete'" ) includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [focusView hasSelection]). ^true].
	(#(#'editPaste') includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [Clipboard current isTextAvailable]). ^true].
	(#(#'addRepository') includes: command) ifTrue: [aCommandQuery isEnabled: packageListPresenter selections notEmpty. ^true].
	(#(#'showPackageChanges') includes: command) ifTrue: [
		aCommandQuery isEnabled: (packageListPresenter selections size = 1 and: [packageListPresenter selection value]). ^true].
	(#(#'savePackage' #'removeRepository') includes: command) ifTrue: [
		aCommandQuery isEnabled: (repositoryListPresenter hasSelection and: [packageListPresenter selections size = 1]). ^true].
	(#(#'compareAncestor') includes: command) ifTrue: [
		aCommandQuery isEnabled: (packageListPresenter selections size = 1 and: [ancestorListPresenter hasSelection]). ^true].
	(#(#'browseMethodHistory' #'browseImplementors' #'browseSenders') includes: command) ifTrue: [aCommandQuery isEnabled: methodListPresenter selections size = 1. ^true].
	(#(#'inspectGlobal' #'browseGlobalReferences') includes: command) ifTrue: [aCommandQuery isEnabled: globalsPresenter selections size = 1. ^true].
	(#(#'removeGlobals') includes: command) ifTrue: [aCommandQuery isEnabled: globalsPresenter selections notEmpty. ^true].
	(#(#'fileOutDictionary' ) includes: command) ifTrue: [aCommandQuery isEnabled: dictionaryListPresenter selections size = 1. ^true].
	(#(#'setHomeDictionary') includes: command) ifTrue: [aCommandQuery isEnabled: (dictionaryListPresenter selections size = 1 and: [dictionaryListPresenter selection value not]). ^true].
	(#(#'runMethodTests') includes: command) ifTrue: [aCommandQuery isEnabled: (methodListPresenter selections notEmpty and: [methodListPresenter selections first at: 3]). ^true].
	(#(#'loadLatestVersion') includes: command) ifTrue: [
		aCommandQuery isEnabled: (packageListPresenter selections notEmpty and: [packageListPresenter selections allSatisfy: [:each | each key beginsWith: 'ConfigurationOf']]). ^true].
	(#(#'browseClassReferences' #'fileOutClass' #'addSubclass' #'addMissingAccessors' #'removeClass' #'removePriorVersions') includes: command) ifTrue: [
		aCommandQuery isEnabled: self selectedClasses size == 1. ^true.
	].
	(#(#'runClassTests') includes: command) ifTrue: [aCommandQuery isEnabled: selectedClassesAreTestCases. ^true].
	^super queryCommand: aCommandQuery.
!

removeClass

	| list stream result |
	list := self selectedClasses.
	stream := WriteStream on: String new.
	list do: [:each | stream nextPutAll: each; cr].
	result := MessageBox 
		confirm: stream contents 
		caption: 'Remove the following class(s)?'.
	result ifFalse: [^self].
	stream := WriteStream on: String new.
	stream nextPutAll: 'removeClasses'; lf.
	self addPackageDictionaryInfoTo: stream.
	list do: [:each | stream nextPutAll: each; tab].
	stream lf; nextPutAll: self requestString.
	self updateCommand: stream contents.

!

removeDictionary

	| list stream result |
	list := dictionaryListPresenter selections collect: [:each | each key].
	stream := WriteStream on: String new.
	list do: [:each | stream nextPutAll: each; cr].
	result := MessageBox 
		confirm: stream contents 
		caption: 'Remove the following dictionary(s)?'.
	result ifFalse: [^self].
	stream := (WriteStream on: String new)
		nextPutAll: 'removeDictionaries'; tab;
		yourself.
	list do: [:each | 
		stream nextPutAll: each; tab.
	].
	stream lf; nextPutAll: self requestString.
	self updateCommand: stream contents.
!

removeGlobals

	| stream |
	stream := (WriteStream on: String new)
		nextPutAll: 'removeGlobals'; lf;
		yourself.
	dictionaryListPresenter selections do: [:each | 
		stream nextPutAll: each key; tab.
	].
	stream lf.
	globalsPresenter selections do: [:each | 
		stream nextPutAll: each first; tab.
	].
	stream lf; nextPutAll: self requestString.
	self updateCommand: stream contents.
!

removeMethodCategories

	| stream |
	stream := WriteStream on: String new.
	stream
		nextPutAll: 'removeMethodCategories'; tab;
		nextPutAll: self selectedClassNameWithoutVersion; tab;
		nextPutAll: selectedClassOop printString; tab;
		nextPutAll: instanceClassTabs currentCard name;
		yourself.
	categoryListPresenter selections do: [:each | stream tab; nextPutAll: each trimBlanks].
	stream lf.
	self updateCommand: stream contents , self requestString.
!

removeMethods

	| stream result string |
	stream := WriteStream on: String new.
	methodListPresenter selections do: [:each | stream nextPutAll: each first; cr].
	result := MessageBox 
		confirm: stream contents 
		caption: 'Remove the following method(s)?'.
	result ifFalse: [^self].
	string := 'removeMethods' , Character tab asString , self methodsIdentifier , Character lf asString , self requestString.
	self updateCommand: string.
!

removePriorVersions

	| stream |
	stream := WriteStream on: String new.
	stream nextPutAll: 'removePriorVersions'; lf.
	self addPackageDictionaryInfoTo: stream.
	self selectedClasses do: [:each | stream nextPutAll: each; tab].
	stream lf; nextPutAll: self requestString.
	self updateCommand: stream contents.
!

removeRepository

	|stream |
	stream := (WriteStream on: String new)
		nextPutAll: 'removeRepository'; tab;
		nextPutAll: (repositoryListPresenter selection at: 2);
		yourself.
	packageListPresenter selections do: [:each | stream tab; nextPutAll: each key].
	stream lf.
	self updateCommand: stream contents , self requestString.
!

reportCompileError: anArrayOfArray 

	| source position stream string indicators |
	indicators := OrderedCollection new.
	methodSourcePresenter view clearContainerIndicators.
	source := self currentMethodSource , Character lf asString.
	position := methodSourcePresenter view caretPosition.
	methodSourcePresenter value: source.
	methodSourcePresenter view caretPosition: position.
	stream := WriteStream on: String new.
	anArrayOfArray do: [:eachArray | 
		| start |
		start := eachArray at: 2.
		string := eachArray at: 3.
		stream nextPutAll: string , '; '.
		indicators add: (ScintillaIndicator
			styleName: 10 
			range: (start to: (start + 10 min: source size)) 
			tag: string).
	].
	methodSourcePresenter view backcolor: JadeTextPresenter colorForCompileError.
	methodSourcePresenter view indicators: indicators.
	string := stream contents.
	self statusBarText: (string copyFrom: 1 to: string size - 2).
!

requestString

	| stream |
	stream := WriteStream on: String new.
	self
		addPackageDictionaryInfoTo: stream;
		addClassCategoryInfoTo: stream;
		addClassHierarchyInfoTo: stream;
		addMethodInfoTo: stream;
		yourself.
	^stream contents.!

requestStringForUpdate

	^'update' , Character lf asString , self requestString.
!

resetColors

	JadeTextPresenter resetColors.
!

revertNewClass

	| stream |
	stream := WriteStream on: String new.
	stream nextPutAll: 'revertClass'; lf.
	self addPackageDictionaryInfoTo: stream.
	stream nextPutAll: selectedClassName subStrings first.
	stream lf; nextPutAll: self requestString.
	self updateCommand: stream contents.
!

runClassTests

	| stream string list caption selection |
	stream := (WriteStream on: String new)
		nextPutAll: self behaviorIdentifier;
		yourself.
	string := self gciSession 
		serverPerform: #'sbRunClassTests:' 
		with: stream contents.
	string isNil ifTrue: [self error: 'Test run did not return expected value!!'. ^self].
	list := string subStrings: Character lf.
	list size = 1 ifTrue: [
		MessageBox notify: list first.
		^self.
	].
	caption := list first.
	list := list copyFrom: 2 to: list size.
	list := list asSortedCollection.
	selection := SUnitResultDialog showModalOn: (Array with: self gciSession with: caption with: list).
	selection isNil ifTrue: [^self].
	[
		self gciSession executeString: selection.
	] on: GsError do: [:ex | 
		JadeDebugger openDebuggerOnException: ex.
	].
!

runMethodTests

	| result |
	result := self gciSession 
		serverPerform: #'sbRunMethodTests:' 
		with: self methodsIdentifier.
	result == true ifFalse: [self error: 'unexpected result!!'].
	MessageBox notify: 'Ran ' , methodListPresenter selections size printString , ' test(s)'.
!

savePackage

	| package string array dict stream |
	package := packageListPresenter selections first.
	string := self updateCommand: 'uniqueVersionName' , Character tab asString , package key.
	string = 'uniqueVersionName' ifFalse: [self error: 'Unrecognized response'].
	string := readStream nextLine.
	array := ancestorListPresenter list.
	array := array isEmpty 
		ifTrue: [#('' '' '' '')]
		ifFalse: [array first].
	dict := Dictionary new
		at: #'httpPassword'			put: nil;
		at: #'httpUser'					put: nil;
		at: #'name' 						put: package key;
		at: #'isModified'					put: package value;
		at: #'uniqueVersionName'	put: string;
		at: #'versionName'			put: (array at: 2);
		at: #'versionMessage'		put: (array at: 4);
		at: #'repositoryList'			put: repositoryListPresenter list;
		at: #'repository'					put: repositoryListPresenter selection;
		yourself.
	(MCVersionDialog showModalOn: dict) isNil ifTrue: [^self].
	stream := (WriteStream on: String new)
		nextPutAll: 'savePackage'; tab;
		nextPutAll: package key; tab;
		nextPutAll: ((dict at: #'repository') at: 2); tab;
		nextPutAll: (dict at: #'versionName'); tab;
		nextPutAll: (dict at: #'httpUser'); tab;
		nextPutAll: (dict at: #'httpPassword'); lf;
		nextPutAll: ((dict at: #'versionMessage') reject: [:char | char = Character cr]); lf;
		nextPut: $%; lf;
		nextPutAll: self requestString;
		yourself.
	self updateCommand: stream contents.
!

selectClass: aString selector: methodString
"
	Array with: className with: dictionaryName with: catetory with: packageName.
"
	| string list className isMeta array |
	string := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: 'findClass'.
	list := (string subStrings: Character lf) collect: [:each | each subStrings: Character tab].
	list := list copyFrom: 2 to: list size.
	list := list collect: [:each | each size < 3 ifTrue: [each , #('' '' '')] ifFalse: [each]].
	className := aString.
	(isMeta := className endsWith: ' class') ifTrue: [
		className := className copyFrom: 1 to: className size - 6.
	].
	array := list detect: [:each | each first subStrings first = className].
	self 
		updateAfterFindClass: array
		isMeta: isMeta 
		selector: methodString.
!

selectedClassChanged: aBoolean
		"We don't want the previous class' method filter to apply to new class"

	selectedClassChanged := aBoolean.
!

selectedClasses

	| list |
	(list := classListPresenter selections) isEmpty ifTrue: [
		(list := classHierarchyPresenter selections) notEmpty ifTrue: [
			list := Array with: list last last.
		].
	].
	^list.
!

selectedClassName: aString

	selectedClassName := aString.
	self updateTabLabel.
!

selectedClassNameWithoutVersion

	^(selectedClassName includes: Character space)
		ifTrue: [selectedClassName subStrings first]
		ifFalse: [selectedClassName].
!

setColorForCompileErrors

	| newColor |
	newColor := ColorDialog showModalOn: JadeTextPresenter colorForNoEdits.
	JadeTextPresenter colorForCompileError: newColor.
!

setColorForNoEdits

	| newColor |
	newColor := ColorDialog showModalOn: JadeTextPresenter colorForNoEdits.
	JadeTextPresenter colorForNoEdits: newColor.
!

setColorForUnsavedEdits

	| newColor |
	newColor := ColorDialog showModalOn: JadeTextPresenter colorForUnsavedEdits.
	JadeTextPresenter colorForUnsavedEdits: newColor.
	!

setHomeDictionary

	| string |
	string := 'setHomeDictionary' , Character tab asString , dictionaryListPresenter selection key , Character lf asString , self requestString.
	self updateCommand: string.
!

setSearchPolicy

	packageListPresenter 			view model searchPolicy: SearchPolicy equality.
	dictionaryListPresenter 			view model searchPolicy: SearchPolicy equality.
	classCategoryPresenter			view model searchPolicy: SearchPolicy equality.
	classListPresenter					view model searchPolicy: SearchPolicy equality.
	classHierarchyPresenter		view model searchPolicy: SearchPolicy equality.
	categoryListPresenter 			view model searchPolicy: SearchPolicy equality.
	variableListPresenter	 			view model searchPolicy: SearchPolicy equality.
	superclassListPresenter			view model searchPolicy: SearchPolicy equality.
	methodListPresenter				view model searchPolicy: SearchPolicy equality.
	overrideListPresenter			view model searchPolicy: SearchPolicy equality.
	ancestorListPresenter 			view model searchPolicy: SearchPolicy equality.
	repositoryListPresenter			view model searchPolicy: SearchPolicy equality.
	globalsPresenter					view model searchPolicy: SearchPolicy equality.
!

showPackageChanges

	| repository stream patch |
	repository := repositoryListPresenter hasSelection 
		ifTrue: [repositoryListPresenter selection]
		ifFalse: [repositoryListPresenter list first].
	stream := (WriteStream on: String new)
		nextPutAll: 'changesInPackage'; tab;
		nextPutAll: packageListPresenter selections first key; tab;	"package name"
		nextPutAll: (repository at: 2); tab;		"repository name"
		lf.
	(self updateCommand: stream contents) = 'changesInPackage' ifFalse: [self error: 'Unexpected response!!'].
	patch := MCPatch
		fromString: readStream upToEnd
		session: self gciSession.
	patch operations isEmpty ifTrue: [
		MessageBox notify: 'No changes!!'.
		^self.
	].
	MCPatchBrowser showOn: patch.
!

splitterPosition: aPoint

	[
		| splitter |
		splitter := view viewNamed: 'splitter' ifNone: [self error: 'splitter is missing!!?'].
		view layoutManager reposition: splitter to: aPoint.
		view layout.
	] postToInputQueue.

!

statusBarServerRequestText: aString

	self parentPresenter parentPresenter statusBarServerRequestText: aString.
!

statusBarText: aString

	self parentPresenter parentPresenter statusBarText: aString.
!

stepPointAt: aPoint

	| charIndex |
	charIndex := methodSourcePresenter view charNearestPosition: aPoint.
	stepPoints size to: 1 by: -1 do: [:stepPoint | 
		| range |
		range := (stepPoints at: stepPoint) key.
		(range start <= charIndex and: [charIndex <= range stop]) ifTrue: [
			^stepPoint.
		].
	].
	^nil.
!

stuffToKeepFromOldFileForClass: nameString onPath: pathString

	| file source lf i j k l x |
	[
		file := FileStream
			read: pathString
			text: true.
	] on: Exception do: [:ex | ^''].
	source := file contents.
	file close.
	l := source indexOfSubCollection: ' subclass: ''' , nameString , ''''.
	0 == l ifTrue: [^''].
	k := j := i := 0.
	lf := Character lf asString.
	[
		(k := source indexOfSubCollection: lf startingAt: k + 1) < l.
	] whileTrue: [
		i := j. j := k.
	].
	x := source copyFrom: i + 1 to: j - 1.
	x = 'doit' ifFalse: [^''].
	x := source copyFrom: j + 1 to: l - 1.
	(x includes: Character space) ifTrue: [^''].
	^source copyFrom: 1 to: i.!

textTabChanged

	methodSourcePresenter view cancelCallTip.
	(textAreaTabs currentCard name = 'globals' 			and: [self isPackageListTabSelected	]) ifTrue: [packageInfoTab 	ensureVisible].
	(textAreaTabs currentCard name = 'packageInfo' 	and: [self isDictionariesTabSelected	]) ifTrue: [globalsPresenter 	ensureVisible].
	self updateMenuBar.
!

unloadPackage

	| packageName |
	packageName := packageListPresenter selections first key.
	(MessageBox confirm: 'Do you want to unload ''' , packageName , '''?' caption: 'Confirm Unload') ifFalse: [^self].
	self updateCommand: 'unloadPackage' , Character tab asString , packageName , Character lf asString , self requestString.
!

updateAfterFindClass: anArray isMeta: aBoolean selector: aString
"
	Array with: className with: dictionaryName with: catetory with: packageName.

	packageDictionaryTabs currentCard name
	package or dictionary selections (tab-delimited)
	category-subcategory-subsubcategory-
	classHierarchyTabs currentCard name
	selected classes (tab-delimited)
	instanceClassTabs currentCard name
	superclassList selection
	categoryVariableTabs currentCard name
	methodFilterList selections (tab-delimited)
	methodList selections (tab-delimited)
	overrideList selection
"
	| stream tabName |
	anArray isNil ifTrue: [^self].
	stream := WriteStream on: String new.
	tabName := (anArray at: 4) isEmpty 
		ifTrue: ['dictionaryList']
		ifFalse: [packageDictionaryTabs currentCard name].
	stream 
		nextPutAll: 'update'; lf;
		nextPutAll: tabName; lf;	"[ packageList | dictionaryList ]"
		nextPutAll: (tabName = 'packageList' ifTrue: [anArray at: 4] ifFalse: [anArray at: 2]); lf; 	"Package or Dictionary name"
		nextPutAll: (anArray at: 3); nextPut: $-; lf;	"class category"
		nextPutAll: 'classList'; lf;	"not hierarchy"
		nextPutAll: (anArray at: 1); lf;	"className"
		nextPutAll: (aBoolean ifTrue: ['classTab'] ifFalse: ['instanceTab']); lf;
		lf; 	"superclass"
		nextPutAll: 'categoryList'; lf;	"not variables"
		lf;		"methodFilter (category or variable)"
		nextPutAll: aString; lf; 	"method names"
		lf;		"override"
		yourself.
	self updateCommand: stream contents.
!

updateAndSelect: aView

	updateCount := updateCount + 1.
	keystrokeTime < Time millisecondClockValue ifTrue: [
		self updateAndSelectA: aView.
	] ifFalse: [
		[self updateAndSelectB: aView] fork.
	].
!

updateAndSelectA: aView

	inUpdate ifTrue: [^self].
	self updateCommand: self requestStringForUpdate.
	self updateMenuBar.
	aView notNil ifTrue: [aView ensureVisible].
!

updateAndSelectB: aView

	| oldUpdateCount |
	oldUpdateCount := updateCount.
	(Delay forMilliseconds: keystrokeTime - Time millisecondClockValue) wait.
	oldUpdateCount = updateCount ifTrue: [
		self updateAndSelectA: aView.
	].
!

updateClassCategoryTree

	| root treeModel cache listA listB existingSelection newSelection |
	root := #() -> '--Categories--'.
	treeModel := TreeModel new
		searchPolicy: SearchPolicy equality;
		reset;
		add: root asChildOf: nil;
		yourself.
	cache := Dictionary new
		at: root key put: root;
		yourself.
	self nextLineAsList asSortedCollection do: [:each | 
		| array |
		array := each subStrings: $-.
		1 to: array size do: [:i | 
			| childName parentName child parent |
			childName := array copyFrom: 1 to: i.
			parentName := array copyFrom: 1 to: i - 1.
			parent := cache at: parentName.
			(cache includesKey: childName) ifFalse: [
				cache 
					at: childName
					put: (child := childName -> each).
				treeModel 
					add: child
					asChildOf: parent.
			].
		].
	].
	listA := (treeModel asBag collect: [:each | each key printString]) asSortedCollection asArray.
	listB := (classCategoryPresenter model asBag collect: [:each | each key printString]) asSortedCollection asArray.
	listA = listB ifFalse: [
		classCategoryPresenter
			model: treeModel;
			yourself.
	].
	newSelection := self nextLine subStrings: $-.
	newSelection isEmpty ifTrue: [newSelection := root key].
	existingSelection := (classCategoryPresenter selectionIfNone: [nil -> nil]) key.
	newSelection = existingSelection ifFalse: [
		| items item |
		items := classCategoryPresenter model asBag asArray.
		0 to: newSelection size do: [:i | 
			| key |
			key := newSelection copyFrom: 1 to: i.
			item := items detect: [:each | each key = key].
			classCategoryPresenter expand: item.
		].
		classCategoryPresenter selection: item.
	].
	classCategoryPresenter view ensureSelectionVisible.
!

updateClassHierarchy

	| list paths treeModel x y currentSelection newSelection flags |
	list := self nextList.
	newSelection := self nextLineAsList reverse.
	flags := self nextLineAsList.

	classHierarchyPresenter ensureVisible.
	list := list collect: [:each | each reverse].
	paths := Set new.
	list do: [:eachClass | 
		| string |
		string := ''.
		eachClass do: [:each | 
			string := string , each.
			paths add: string.
			string := string , Character tab asString.
		].
	].
	paths := paths asSortedCollection asArray.
	treeModel := TreeModel new
		searchPolicy: SearchPolicy equality;
		reset;
		yourself.
	paths do: [:each | 
		| path parent |
		path := each subStrings: Character tab.
		parent := path copyFrom: 1 to: path size - 1.
		parent isEmpty ifTrue: [parent := nil].
		treeModel add: path asChildOf: parent.
	].
	x := (treeModel asBag collect: [:each | each printString]) asSortedCollection asArray.
	y := (classHierarchyPresenter model asBag collect: [:each | each printString]) asSortedCollection asArray.
	x = y ifFalse: [
		classHierarchyPresenter
			model: treeModel;
			yourself.
	].
	currentSelection := classHierarchyPresenter selectionIfNone: [#()].
	(currentSelection isEmpty and: [newSelection isEmpty and: [classListPresenter selections size = 1]]) ifTrue: [
		x := classListPresenter selection.
		newSelection := classHierarchyPresenter model asBag asArray
			detect: [:each | each last = x]
			ifNone: [nil].
	].
	(newSelection notEmpty and: [currentSelection ~= newSelection]) ifTrue: [
		classHierarchyPresenter 
			selection: newSelection;
			expand: newSelection;
			yourself.
	].
	self selectedClassName: (newSelection notEmpty ifTrue: [newSelection last] ifFalse: ['']).
	classListPresenter selectionOrNil: nil.

	selectedClassesAreTestCases := (flags at: 1) = 'true'.!

updateClassInfo

	| index newClassDefinition |
	selectedClassOop := self nextLine asNumber.
	newClassDefinition := self nextParagraph.
	classCommentPresenter value: self nextParagraph.

	classDefinitionPresenter view isModified ifTrue: [
		newClassDefinition = classDefinition ifTrue: [^self].
		(MessageBox confirm: 'Copy changes to clipboard?' caption: 'Class has unsaved changes!!') ifTrue: [
			classDefinitionPresenter value copyToClipboard.
		].
	].
	classDefinition := newClassDefinition.
	0 < (classDefinition indexOfSubCollection: 'MyNewClass') ifTrue: [
		index := classDefinition indexOfSubCollection: 'Globals'.
		(classDefinition copyFrom: index - 4 to: index - 1) ~= 'User' ifTrue: [
			classDefinition := (classDefinition copyFrom: 1 to: index - 1) , 
				(dictionaryListPresenter selectionIfNone: ['UserGlobals' -> false]) key , 
				(classDefinition copyFrom: index + 7 to: classDefinition size).
			index := classDefinition indexOfSubCollection: 'User Classes'.
			0 < index ifTrue: [
				classDefinition := (classDefinition copyFrom: 1 to: index - 1) , 
					(packageListPresenter selectionIfNone: ['User Classes' -> nil]) key , 
					(classDefinition copyFrom: index + 12 to: classDefinition size).
			].
		].
	].
	classDefinitionPresenter value: classDefinition.
	classDefinitionPresenter view 
		backcolor: Color white;
		isModified: false;
		yourself.
!

updateClassList

	| fullList newSelections flags |
	classListPresenter ensureVisible.
	fullList := self nextLineAsList.
	newSelections := self nextLineAsList.
	flags := self nextLineAsList.

	fullList = classListPresenter list ifFalse: [
		classListPresenter list: fullList.
	].

	newSelections := classListPresenter list select: [:each | newSelections includes: each].
	newSelections = classListPresenter selections ifFalse: [
		classListPresenter selections: newSelections.
	].
	self selectedClassName: (newSelections size = 1 ifTrue: [newSelections first] ifFalse: ['']).
	classHierarchyPresenter selectionOrNil: nil.
	newSelections notEmpty ifTrue: [
		classListPresenter view ensureSelectionVisible.
	] ifFalse: [
		fullList notEmpty ifTrue: [
			classListPresenter view ensureVisible: 1.
		].
	].

	selectedClassesAreTestCases := (flags at: 1) = 'true'.!

updateClassListOrHierarchy

	| tab |
	tab := self nextLine.
	tab = 'classHierarchy' ifTrue: [^self updateClassHierarchy].
	tab = 'classList' ifTrue: [^self updateClassList].
	self error: 'Unexpected token!!'.
!

updateCommand: aString

	^self 
		updateCommand: aString 
		onSuccessDo: [].
!

updateCommand: aString onSuccessDo: aBlock

	[
		| time1 time2 time3 string |
		time1 := Time millisecondsToRun: [
			string := self gciSession 
				serverPerform: #'systemBrowser:' 
				with: aString.
			eventCount := self gciSession eventCount.
		].
		time2 := Time millisecondsToRun: [
			| responseType |
			readStream := ReadStream on: string.
			time3 := self nextLine asNumber.
			aBlock value.
			(responseType := self nextLine) = 'update' ifFalse: [^responseType].
			self updatePresenters.
		].
		self statusBarServerRequestText:
			'server took ' , time3 printString , 'ms; ' , 
			'network took ' , (time1 - time3) printString , 'ms; ' , 
			'client took ' , time2 printString , 'ms; ' , 
			'total of ' , (time1 + time2) printString , 'ms'.
	] ensure: [
		selectedClassChanged := false.
	].
	^nil.
!

updateDictionaryList

	| fullList selections lines |
	dictionaryListPresenter ensureVisible.
	fullList := self nextLineAsList collect: [:each | (each copyFrom: 2 to: each size) -> (each first = $H)].
	dictionaryListPresenter list = fullList ifFalse: [
		dictionaryListPresenter list: fullList.
	].
	selections := self nextLineAsList collect: [:x | dictionaryListPresenter list detect: [:y | x = y key]].
	dictionaryListPresenter selections = selections ifFalse: [
		dictionaryListPresenter selections: selections.
	].
	dictionaryListPresenter view ensureSelectionVisible.
	lines := self nextList.
	globalsPresenter list: lines.
!

updateMenuBar

	| shellView menuBar dictsPkgsMenu methodFilterMenu textAreaMenu |
	dictsPkgsMenu := Menu fromStrings: (self isDictionariesTabSelected ifTrue: [self dictsMenuStrings] ifFalse: [self pkgsMenuStrings]).
	methodFilterMenu := Menu fromStrings: (self isCategoriesTabSelected ifTrue: [self categoriesMenuStrings] ifFalse: [self variablesMenuStrings]).
	textAreaMenu := Menu fromStrings: (self isGlobalsTabSelected ifTrue: [self globalsMenuStrings] ifFalse: [self editMenuStrings]).
	shellView := self parentPresenter parentPresenter view.
	menuBar := shellView menuBar
		clear;
		addItem: (Menu fromStrings: self jadeMenuStrings);
		addItem: dictsPkgsMenu;
		addItem: (Menu fromStrings: self classesMenuStrings);
		addItem: methodFilterMenu;
		addItem: (Menu fromStrings: self methodsMenuStrings);
		addItem: textAreaMenu;
		addItem: (Menu fromStrings: self preferencesMenuStrings);
		yourself.
	shellView menuBar: menuBar.
!

updateMethod

	| isReadOnly newSource |
	methodSourcePresenter view cancelCallTip.
	isReadOnly := self nextLine = 'false'.	"current user has write permission for the class"
	(newSource := self nextParagraph) isEmpty ifTrue: [
		newSource := 
'newMethod: argument
		"Method comment"

	^self yourself.
'.
	].
	methodSourcePresenter view isModified ifTrue: [
		newSource = methodSource ifTrue: [
			self nextLine; nextLine.
			^self.
		].
		(MessageBox confirm: 'Copy changes to clipboard?' caption: 'Method has unsaved changes!!') ifTrue: [
			methodSourcePresenter value copyToClipboard.
		].
	].
	methodSource := newSource.
	methodSourcePresenter value: methodSource.
	stepPoints := self nextLineAsList collect: [:each |
		| pieces offset selector | 
		pieces := each subStrings.
		offset := pieces first asNumber.
		selector := (2 <= pieces size ifTrue: [pieces at: 2] ifFalse: ['']).
		(offset to: 0) -> selector.
	].
	breakPoints := self nextLineAsList collect: [:each | each asNumber].
	1 to: stepPoints size do: [:stepPoint |
		| range start char length |
		range := (stepPoints at: stepPoint) key.
		start := range start.
		char := methodSource at: start.
		length := (char isAlphaNumeric or: [char = $_])
			ifTrue: [(methodSource copyFrom: start + 1 to: methodSource size) findFirst: [:eachChar | (eachChar isAlphaNumeric or: [eachChar = $_ or: [eachChar = $:]]) not]]
			ifFalse: [2].
		length = 0 ifTrue: [length := methodSource size - start].
		[
			2 < length and: [(methodSource at: start) = $_].
		] whileTrue: [
			start := start + 1.
			length := length - 1.
		].
		range 
			start: start;
			stop: start + length - 1;
			yourself.
	].
	self updateMethodStepPoints.
	methodSourcePresenter view 
		backcolor: JadeTextPresenter colorForNoEdits;
		isModified: false;
		isReadOnly: isReadOnly;
		yourself.
	(overrideListPresenter list notEmpty and: [overrideListPresenter selectionOrNil ~= overrideListPresenter list last]) ifTrue: [
		methodSourcePresenter view
			backcolor: nil;
			isReadOnly: true;
			yourself.
	].
	((methodCategory := self nextLine) notEmpty and: [self isCategoriesTabSelected]) ifTrue: [
		| fullList selections index newName |
		fullList := categoryListPresenter list.
		selections := categoryListPresenter selections.
		0 < (index := fullList indexOf: methodCategory) ifTrue: [
			fullList at: index put: (newName := ' ' , methodCategory).
			categoryListPresenter list: fullList.
			0 < (index := selections indexOf: methodCategory) ifTrue: [
				selections at: index put: newName.
				categoryListPresenter selections: selections.
			].
			categoryListPresenter view invalidate.
		].
	].
	originalSourcePresenter value: self nextParagraph.
!

updateMethodFilter

	| listPresenter type tabs tab filters selections |
	listPresenter := self methodFilterListPresenter.
	type := self nextLine.
	(tabs := categoryVariableTabs cards) isEmpty ifTrue: [^self].
	tab := tabs detect: [:each | each name = type].
	tab ensureVisible.
	filters := self nextLineAsList reverse.
	filters = listPresenter list ifFalse: [
		listPresenter list: filters.
	].
	selections := self nextLineAsList.
	selections := listPresenter list select: [:each | selections includes: each].
	selections = listPresenter selections ifFalse: [
		listPresenter selections: selections.
	].
	selections notEmpty ifTrue: [
		listPresenter view ensureSelectionVisible.
	] ifFalse: [
		filters notEmpty ifTrue: [
			listPresenter view ensureVisible: 1.
		].
	].
!

updateMethodList

	| fullList selections |
	fullList := (self nextParagraph subStrings: Character lf) collect: [:each | (each subStrings: Character tab) , #('' '' '' '')].
	fullList := fullList do: [:each | 
		each 
			at: 2 put: (each at: 2) = 'T';		"has a superclass implementation"
			at: 3 put: (each at: 3) = 'T';		"is in a TestCase class"
			at: 4 put: (each at: 4) = 'T';		"method replaced by GsPackagePolicy"
			yourself.
	].
	fullList = methodListPresenter list ifFalse: [
		methodListPresenter list: fullList.
	].
	selections := self nextLineAsList.
	selections := methodListPresenter list select: [:eachArray | selections includes: eachArray first].
	selections = methodListPresenter selections ifFalse: [
		methodListPresenter selections: selections.
	].
	selections notEmpty ifTrue: [
		methodListPresenter view ensureSelectionVisible.
	] ifFalse: [
		fullList notEmpty ifTrue: [
			methodListPresenter view ensureVisible: 1.
		].
	].

!

updateMethodStepPoints

	| indicators |
	methodSourcePresenter view clearContainerIndicators.
	indicators := OrderedCollection new.
	1 to: stepPoints size do: [:stepPoint |
		| range string styleName |
		range := (stepPoints at: stepPoint) key.
		styleName := (breakPoints includes: stepPoint) ifTrue: [9] ifFalse: [8].
		string := ((breakPoints includes: stepPoint) ifTrue: ['Break at '] ifFalse: ['']) , 'step point #' , stepPoint printString.
		indicators add: (ScintillaIndicator
			styleName: styleName 
			range: range 
			tag: string).
	].
	methodSourcePresenter view indicators: indicators.
!

updateOverrideList

	| list selection |
	list := self nextLineAsList.
	list = overrideListPresenter list ifFalse: [
		overrideListPresenter list: list.
	].
	selection := self nextLine.
	selection isEmpty ifTrue: [selection := nil].
	selection notNil ifTrue: [
		selection := overrideListPresenter list
			detect: [:each | each = selection]
			ifNone: [nil].
	].
	selection = overrideListPresenter selectionOrNil ifFalse: [
		overrideListPresenter selectionOrNil: selection.
	].
!

updatePackageDictionaryList

	| next |
	next := self nextLine.
	next = 'dictionaryList' ifTrue: [^self updateDictionaryList].
	next = 'packageList' ifTrue: [^self updatePackageList].
	self error: 'Unexpected token'.
!

updatePackageInfo

	| list |
	list := self nextList collect: [:each | each , #('' '' '' '')].
	list = ancestorListPresenter list ifFalse: [
		ancestorListPresenter list: list.
	].
	list := self nextList collect: [:each | each , #('' '')].
	list = repositoryListPresenter list ifFalse: [
		repositoryListPresenter list: list.
	].
!

updatePackageList

	| dictionary fullList selections old new |
	packageListPresenter ensureVisible.
	dictionary := Dictionary new.
	self nextLineAsList do: [:each | dictionary at: each put: false].
	self nextLineAsList do: [:each | dictionary at: each put: true].
	fullList := dictionary associations asSortedCollection asArray.
	old := packageListPresenter list collect: [:each | each key].
	new := fullList collect: [:each | each key].
	old = new ifTrue: [
		packageListPresenter list do: [:each | 
			each value: (dictionary at: each key).
		].
		packageListPresenter view updateAll.
	] ifFalse: [
		packageListPresenter list: fullList.
	].
	selections :=  self nextLineAsList.
	selections := selections collect: [:x | packageListPresenter list detect: [:y | x = y key]].
	packageListPresenter selections = selections ifFalse: [
		packageListPresenter selections: selections.
	].
	selections size = 1 
		ifTrue: [self updatePackageInfo]
		ifFalse: [self clearPackageInfo].
	packageListPresenter view ensureSelectionVisible.
!

updatePresenters

	[
		inUpdate := true.
		self
			updatePackageDictionaryList;
			updateClassCategoryTree;
			updateClassListOrHierarchy;
			updateClassInfo;
			updateSuperclassList;
			updateMethodFilter;
			updateMethodList;
			updateOverrideList;
			updateMethod;
			yourself.
	] ensure: [
		inUpdate := false.
	].
!

updateSuperclassList

	| tabs tabName tab list selected |
	tabName := self nextLine.
	(tabs := instanceClassTabs cards) isEmpty ifTrue: [^self].
	tab := tabs 
		detect: [:each | each name = tabName]
		ifNone: [self error: 'None of ' , tabs printString , ' match ' , tabName printString].
	tab ensureVisible.
	list := self nextLineAsList reverse.
	list = superclassListPresenter list ifFalse: [
		superclassListPresenter list: list.
	].
	(selected := self nextLine) notEmpty ifTrue: [
		selected := (selected subStrings: Character tab) first.
	].
	selected := superclassListPresenter list
		detect: [:each | each = selected]
		ifNone: [list notEmpty ifTrue: [list last] ifFalse: [nil]].
	(selected = superclassListPresenter selectionOrNil) ifFalse: [
		superclassListPresenter selection: selected.
	].
!

updateTabLabel

	| cardNumber newLabel |
	cardNumber := self parentPresenter view cards indexOf: self view.
	(newLabel := self selectedClassNameWithoutVersion) notEmpty ifTrue: [
		instanceClassTabs currentCard name = 'classTab' ifTrue: [
			newLabel := newLabel , ' class'.
		].
	] ifFalse: [
		| tabName selections |
		tabName := packageDictionaryTabs currentCard name.
		tabName = 'packageList' ifTrue: [
			newLabel := 'Packages'.
			selections := packageListPresenter selections.
		] ifFalse: [
			newLabel := 'Dictionaries'.
			selections := dictionaryListPresenter selections.
		].
		selections size = 1 ifTrue: [newLabel := selections first key].
	].
	self view arrangement: cardNumber printString , ' ' , newLabel.
	self parentPresenter view updateTabs.
!

variablesMenuStrings

	false ifTrue: [
		self addVariableAccessors.
	].
	^#(
		'&Variables'
		'&Add Accessors//addVariableAccessors'
	).
!

viewActivated

	Transcript cr; show: 'viewActivated'.

	"self update."
! !
!JadeSystemBrowserPresenter categoriesFor: #aboutToChange:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #aboutToEditClassLabel:accept:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #addClassCategoryInfoTo:!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #addClassHierarchyInfoTo:!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #addMethodCategory!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #addMethodInfoTo:!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #addMissingAccessors!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #addPackage!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #addPackageDictionaryInfoTo:!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #addRepository!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #addSubclass!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #behaviorIdentifier!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #breakAt:operation:!event handlers!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseClassReferences!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseGlobalReferences!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseImplementors!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseImplementorsOf!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseImplementorsOf:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseMethodHistory!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseMethodsAndSelect:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseMethodsContaining!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseSenders!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseSendersOf!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseSendersOf:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #categoriesMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #classDefChanged!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #classesMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #clearPackageInfo!public! !
!JadeSystemBrowserPresenter categoriesFor: #closeRequested:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #compareAncestor!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #contextObject!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #createComponents!overrides!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiring!create schemantic wiring!overrides!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForClassCategoryList!create schemantic wiring!overrides!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForClassDefinition!create schemantic wiring!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForClassHierarchy!create schemantic wiring!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForClassList!create schemantic wiring!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForDictionaryList!create schemantic wiring!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForInstanceClassTabs!create schemantic wiring!overrides!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForMethodCategoryList!create schemantic wiring!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForMethodList!create schemantic wiring!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForMethodSource!create schemantic wiring!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForOverrideList!create schemantic wiring!overrides!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForPackageDictionaryTabs!create schemantic wiring!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForPackageList!create schemantic wiring!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForSuperClassList!create schemantic wiring!overrides!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForVariableList!create schemantic wiring!public! !
!JadeSystemBrowserPresenter categoriesFor: #currentMethodSource!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #currentSelectionOrLine!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #defaultFileExtension!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #delayUpdate!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #dictsMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #editCopy!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editCut!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editDelete!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editFind!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editFindNext!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #editPaste!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editRedo!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editReplace!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editSave!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editSaveClass!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editSaveClassA!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editSaveClassComment!public! !
!JadeSystemBrowserPresenter categoriesFor: #editSaveMethod!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editSelectAll!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #editUndo!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #executeSelectionOrLine!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #fileOutClass!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #fileOutClassOnPath:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #fileOutDictionary!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #fileTypes!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #findClass!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #getViews!public! !
!JadeSystemBrowserPresenter categoriesFor: #globalsMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #handleInvalidSession!public! !
!JadeSystemBrowserPresenter categoriesFor: #horizontalSplitter!public! !
!JadeSystemBrowserPresenter categoriesFor: #initialize!public! !
!JadeSystemBrowserPresenter categoriesFor: #insertDictionary!public! !
!JadeSystemBrowserPresenter categoriesFor: #inspectGlobal!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isCategoriesTabSelected!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isClassListTabSelected!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #isDictionariesTabSelected!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isGlobalsTabSelected!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isOkayToChange!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isPackageListTabSelected!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isTreeModel:equivalentTo:!public! !
!JadeSystemBrowserPresenter categoriesFor: #isVariablesTabSelected!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #jadeDisplay!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #jadeExecute!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #jadeExecuteAndDisplay:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #jadeInspect!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #jadeMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #labelOfClass:changedTo:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #labelOfClass:editedTo:accept:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #layoutInfo!public! !
!JadeSystemBrowserPresenter categoriesFor: #layoutInfo:!public! !
!JadeSystemBrowserPresenter categoriesFor: #loadLatestVersion!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodChanged!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodDoubleClicked:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodFilterListPresenter!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #methodHoverEnd:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodHoverStart:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodMenu:!event handlers!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodMenuBreak:!event handlers!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodsIdentifier!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodsMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodSourcePresenter!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #nextLine!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #nextLineAsList!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #nextList!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #nextParagraph!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #onDragClassesOverDictionary:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragClassesOverPackageList:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragClassHierarchy:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragClassList:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragCutClassHierarchy:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragCutClassList:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragCutMethod:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragMethod:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragMethodsOverClassHierarchy:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragMethodsOverClassList:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragOverClassCategory:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragOverClassHierarchy:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragOverClassList:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragOverDictionary:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragOverMethodCategory:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDragOverPackageList:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDropClassesOnDictionary:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDropClassesOnPackage:session:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDropClassesOnPackageList:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDropMethodsOnClass:session:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDropMethodsOnClassHierarchy:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDropMethodsOnClassList:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDropOnClassCategory:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDropOnClassHierarchy:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDropOnClassList:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDropOnDictionary:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDropOnMethodCategory:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onDropOnPackageList:!drag & drop!public! !
!JadeSystemBrowserPresenter categoriesFor: #onSetFocus!public! !
!JadeSystemBrowserPresenter categoriesFor: #onSetFocus:stack:!public! !
!JadeSystemBrowserPresenter categoriesFor: #onViewOpened!overrides!public! !
!JadeSystemBrowserPresenter categoriesFor: #openSourceStyler!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #parseContext!public! !
!JadeSystemBrowserPresenter categoriesFor: #pkgDictChanged!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #pkgsMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #preferencesMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #promptForSelector!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #queryCommand:!event handlers!overrides!public! !
!JadeSystemBrowserPresenter categoriesFor: #removeClass!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #removeDictionary!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #removeGlobals!public! !
!JadeSystemBrowserPresenter categoriesFor: #removeMethodCategories!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #removeMethods!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #removePriorVersions!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #removeRepository!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #reportCompileError:!public! !
!JadeSystemBrowserPresenter categoriesFor: #requestString!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #requestStringForUpdate!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #resetColors!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #revertNewClass!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #runClassTests!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #runMethodTests!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #savePackage!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #selectClass:selector:!public! !
!JadeSystemBrowserPresenter categoriesFor: #selectedClassChanged:!public! !
!JadeSystemBrowserPresenter categoriesFor: #selectedClasses!public! !
!JadeSystemBrowserPresenter categoriesFor: #selectedClassName:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #selectedClassNameWithoutVersion!public! !
!JadeSystemBrowserPresenter categoriesFor: #setColorForCompileErrors!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setColorForNoEdits!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setColorForUnsavedEdits!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setHomeDictionary!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setSearchPolicy!overrides!public! !
!JadeSystemBrowserPresenter categoriesFor: #showPackageChanges!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #splitterPosition:!public! !
!JadeSystemBrowserPresenter categoriesFor: #statusBarServerRequestText:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #statusBarText:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #stepPointAt:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #stuffToKeepFromOldFileForClass:onPath:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #textTabChanged!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #unloadPackage!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #updateAfterFindClass:isMeta:selector:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateAndSelect:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateAndSelectA:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateAndSelectB:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateClassCategoryTree!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateClassHierarchy!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateClassInfo!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateClassList!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateClassListOrHierarchy!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateCommand:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateCommand:onSuccessDo:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateDictionaryList!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateMenuBar!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #updateMethod!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateMethodFilter!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateMethodList!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateMethodStepPoints!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateOverrideList!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updatePackageDictionaryList!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updatePackageInfo!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updatePackageList!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updatePresenters!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateSuperclassList!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateTabLabel!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #variablesMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #viewActivated!event handlers!public! !

!JadeSystemBrowserPresenter class methodsFor!

overriddenIcon

	^Icon fromId: 'OVERRIDDEN.ICO'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 
		##(Smalltalk.STBViewProxy)  8 
		##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 265030 4 
		##(Smalltalk.Menu)  0 16 98 1 984134 2 
		##(Smalltalk.CommandMenuItem)  1 1180998 4 
		##(Smalltalk.CommandDescription)  8 #savePackage 8 '&Save' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 416 1180166 
		##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 98 4 410 8 
		##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1409286144 131073 752 0 524550 
		##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 752 655878 
		##(Smalltalk.CardLayout)  202 208 98 6 721414 
		##(Smalltalk.Association)  8 'Class Definition' 410 8 
		##(Smalltalk.ScintillaView)  98 46 0 752 98 2 8 1445007428 1025 992 721990 2 
		##(Smalltalk.ValueHolder)  0 32 1310726 
		##(Smalltalk.EqualitySearchPolicy)  0 834 8 4278190080 0 5 498 0 16 98 11 546 1 578 8 #editSave 8 '&Save' 9383 1 0 0 0 983366 1 
		##(Smalltalk.DividerMenuItem)  4097 546 1 578 8 #editUndo 8 '&Undo' 9397 1 0 0 0 546 1 578 8 #editRedo 8 '&Redo' 9395 1 0 0 0 1266 4097 546 1 578 8 #editCut 8 'Cu&t' 9393 1 0 0 0 546 1 578 8 #editCopy 8 '&Copy' 9351 1 0 0 0 546 1 578 8 #editPaste 8 '&Paste' 9389 1 0 0 0 546 1 578 8 #editDelete 8 '&Delete' 1629 1 0 0 0 1266 4097 546 1 578 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 263174 
		##(Smalltalk.Font)  0 16 459014 
		##(Smalltalk.LOGFONT)  8 #[244 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 
		##(Smalltalk.Point)  193 193 0 992 0 8 4294902249 852486 
		##(Smalltalk.NullConverter)  0 0 13 0 234 256 98 42 8 #lineNumber 1182726 
		##(Smalltalk.ScintillaTextStyle)  67 0 0 1 0 0 0 0 1984 0 0 0 8 #specialSelector 2002 33 196934 1 
		##(Smalltalk.RGB)  16646145 0 3 0 0 0 0 2032 0 0 0 8 #global 2002 21 0 0 3 0 0 0 0 2096 0 0 0 8 #normal 2002 1 0 0 1 0 0 0 0 2128 0 0 0 8 #boolean 2002 13 2080 0 3 0 0 0 0 2160 0 0 0 8 #special 2002 25 0 0 3 0 0 0 0 2192 0 0 0 8 #number 2002 5 2066 16711169 0 1 0 0 0 0 2224 0 0 0 8 #nil 2002 19 2080 0 3 0 0 0 0 2272 0 0 0 8 #character 2002 31 2066 16646399 0 3 0 0 0 0 2304 0 0 0 8 #braceHighlight 2002 69 2066 66047 0 3 0 0 0 0 2352 0 0 0 8 #indentGuide 2002 75 786694 
		##(Smalltalk.IndexedColor)  33554447 0 1 0 0 0 0 2400 0 0 0 8 #string 2002 3 2066 16646399 0 129 0 0 0 0 2464 0 0 0 8 #symbol 2002 9 2434 33554443 0 1 0 0 0 0 2512 0 0 0 8 #super 2002 17 2080 0 3 0 0 0 0 2560 0 0 0 8 #comment 2002 7 2066 65025 0 1 0 0 0 0 2592 0 0 0 8 #binary 2002 11 2434 33554433 0 1 0 0 0 0 2640 0 0 0 8 #assignment 2002 29 0 0 3 0 0 0 0 2688 0 0 0 8 #keywordSend 2002 27 2434 33554437 0 3 0 0 0 0 2720 0 0 0 8 #return 2002 23 2066 321 0 3 0 0 0 0 2768 0 0 0 8 #braceMismatch 2002 71 2434 33554459 0 3 0 0 0 0 2816 0 0 0 8 #self 2002 15 2080 0 3 0 0 0 0 2864 0 0 0 98 40 2144 2480 2240 2608 2528 2656 2176 2880 2576 2288 2112 2784 2208 2736 2704 2320 2048 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2016 2368 2832 0 2416 0 0 1245510 1 
		##(Smalltalk.NullScintillaStyler)  2128 234 256 98 2 8 #default 1639942 
		##(Smalltalk.ScintillaMarkerDefinition)  1 1 2672 2434 33554471 992 8 #circle 202 208 704 0 63 9215 0 0 0 0 2448 0 0 0 0 0 234 240 98 4 2128 8 '()[]{}<>' 2192 8 '()[]{}<>' 8 '' 3 234 256 98 2 8 #container 234 256 98 2 2128 2002 1 0 0 1 0 0 0 0 2128 0 0 0 0 0 0 0 1 0 234 256 98 6 1 1509190 1 
		##(Smalltalk.ScintillaIndicatorStyle)  1 992 65025 3 32 1 0 3 3282 3 992 33423361 5 32 3 0 5 3282 5 992 511 1 32 5 0 983302 
		##(Smalltalk.MessageSequence)  202 208 98 11 721670 
		##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 1874 9 49 1874 1769 481 992 3410 8 #contextMenu: 98 1 1168 992 3410 8 #selectionRange: 98 1 525062 
		##(Smalltalk.Interval)  3 1 3 992 3410 8 #isTextModified: 98 1 32 992 3410 8 #modificationEventMask: 98 1 9215 992 3410 8 #wordWrap: 98 1 16 992 3410 8 #margins: 98 1 98 3 984582 
		##(Smalltalk.ScintillaMargin)  1 992 1 3 32 1 3842 3 992 33 1 16 67108863 3842 5 992 1 1 16 -67108863 992 3410 8 #indentationGuides: 98 1 0 992 3410 8 #tabIndents: 98 1 16 992 3410 8 #tabWidth: 98 1 9 992 3410 8 #setLexerLanguage: 98 1 8 #smalltalk 992 983302 
		##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 120 3 0 0 8 1 0 0] 98 0 1874 193 193 0 27 946 8 'Class Documentation' 410 1008 98 46 0 752 98 2 8 1445007428 1025 4224 1074 0 32 1120 0 834 1152 0 5 0 1794 0 16 1826 8 #[244 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1874 193 193 0 4224 0 8 4294902249 1922 0 0 11 0 234 256 98 2 2128 2002 1 0 0 1 0 0 0 0 2128 0 0 0 98 40 4448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2914 2128 234 256 98 2 2976 2994 1 1 2672 3024 4224 3040 202 208 704 0 63 9215 0 0 0 0 2448 0 0 0 0 0 0 8 '' 3 234 256 98 2 3184 234 256 98 2 2128 4448 0 0 0 0 1 0 234 256 98 6 1 3282 1 4224 65025 3 32 1 0 3 3282 3 4224 33423361 5 32 3 0 5 3282 5 4224 511 1 32 5 0 3346 202 208 98 9 3410 3440 98 2 1874 9 49 1874 1769 481 4224 3410 3568 98 1 3602 3 1 3 4224 3410 3648 98 1 32 4224 3410 3696 98 1 9215 4224 3410 3744 98 1 16 4224 3410 3792 98 1 98 3 3842 1 4224 1 3 32 1 3842 3 4224 33 1 16 67108863 3842 5 4224 1 1 16 -67108863 4224 3410 3920 98 1 0 4224 3410 3968 98 1 16 4224 3410 4016 98 1 9 4224 4114 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 120 3 0 0 8 1 0 0] 98 0 4176 0 27 946 590662 1 
		##(Smalltalk.CardLabel)  8 'Package' 787814 3 
		##(Smalltalk.BlockClosure)  0 0 1180966 
		##(Smalltalk.CompiledExpression)  7 1 5232 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 5232 5264 8 
		##(Smalltalk.Icon)  8 'Package.ico' 8 #fromId: 8 #text:iconBlock: 5296 11 1 0 0 410 432 98 15 0 752 98 2 8 1140850688 131073 5456 0 0 0 5 0 0 0 5456 658 234 240 98 4 410 8 
		##(Smalltalk.ListView)  98 30 0 5456 98 2 8 1409355853 1025 5568 590662 2 
		##(Smalltalk.ListModel)  202 208 704 0 1310726 
		##(Smalltalk.IdentitySearchPolicy)  834 8 4278190080 0 5 498 0 16 98 5 546 1 578 8 #addRepository 8 'Add &Repository' 1 1 0 0 0 546 1 578 8 #removeRepository 8 'Remo&ve Repository' 1025 1 0 0 0 1266 4097 546 1 578 608 8 '&Save Package' 1 1 0 0 0 546 1 578 8 #showPackageChanges 8 'Show &Changes' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 5568 0 8 4294903909 459270 
		##(Smalltalk.Message)  8 #displayString 98 0 0 1049670 1 
		##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 4 920646 5 
		##(Smalltalk.ListViewColumn)  8 'Repository Type' 201 8 #left 6082 6112 6128 8 
		##(Smalltalk.SortedCollection)  5282 0 0 5314 2 1 5280 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] 8 #copyFrom:to: 6304 7 257 0 0 5568 0 1 0 0 6210 8 'Description' 1169 6256 6082 6112 98 0 6082 8 #<= 6448 5282 0 0 5314 1 83886081 8 
		##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 6496 7 257 0 0 5568 0 3 0 0 6210 8 'User' 201 6256 6082 6112 98 0 6082 6480 6640 5282 0 0 5314 1 83886081 6528 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 6672 7 257 0 0 5568 0 1 0 0 6210 8 'Password' 201 6256 6082 6112 6640 6082 6480 6640 5282 0 0 5314 3 1 6528 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] 8 #collect: 6816 5282 0 0 6832 19 257 0 7 257 0 0 5568 0 1 0 0 8 #report 704 0 131169 0 0 3346 202 208 98 3 3410 3440 98 2 1874 1 265 1874 1769 217 5568 3410 3520 98 1 5760 5568 3410 8 #text: 98 1 8 'Repository Type' 5568 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 132 0 0 0 116 3 0 0 240 0 0 0] 98 0 4176 0 27 19 410 5584 98 30 0 5456 98 2 8 1409355853 1025 7200 5650 202 208 704 0 5712 834 5744 0 5 498 0 16 98 1 546 1 578 8 #compareAncestor 8 '&Compare' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 7200 0 8 4294903909 6082 6112 98 0 0 6160 0 0 0 0 0 0 202 208 98 4 6210 8 'Ancestor' 121 6256 6082 6112 6640 6082 6480 6640 5282 0 0 5314 1 83886081 6528 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 7568 7 257 0 0 7200 0 1 0 0 6210 8 'Name' 401 6256 6082 6112 7456 6288 5282 0 0 5314 1 83886081 5280 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 7696 7 257 0 0 7200 0 1 0 0 6210 8 'Timestamp' 301 6256 6082 6112 6448 6082 6480 6448 5282 0 0 5314 1 83886081 5280 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 7840 7 257 0 0 7200 0 1 0 0 6210 8 'Message' 949 6256 6082 6112 6448 6082 6480 6448 5282 0 0 5314 1 83886081 5280 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 7984 7 257 0 0 7200 0 3 0 0 6928 704 0 131169 0 0 3346 202 208 98 3 3410 3440 98 2 1874 1 1 1874 1769 265 7200 3410 3520 98 1 7312 7200 3410 7104 98 1 8 'Ancestor' 7200 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 132 0 0 0] 98 0 4176 0 27 23 16 234 256 98 4 5568 8 'repositoryList' 7200 8 'ancestorList' 0 3346 202 208 98 1 3410 3440 98 2 1874 9 49 1874 1769 481 5456 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 120 3 0 0 8 1 0 0] 98 2 7200 5568 4176 0 27 946 5234 8 'Globals' 5282 0 0 5314 7 1 80 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 5232 8560 5392 8 'Dictionary.ico' 5424 5440 8576 11 1 0 0 410 5584 98 30 0 752 98 2 8 1409355853 1025 8672 5650 202 208 704 0 5712 834 5744 0 5 0 0 0 8672 0 8 4294903909 6082 6112 98 0 0 6160 0 0 0 0 0 0 202 208 98 3 6210 8 'Name' 587 6256 6082 6112 8816 6288 5282 0 0 5314 1 83886081 6528 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 8912 7 257 0 0 8672 0 3 0 0 6210 8 'Class' 589 6256 6082 6112 6448 6082 6480 6448 5282 0 0 5314 1 83886081 6528 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 9056 7 257 0 0 8672 0 3 0 0 6210 8 'Value' 587 6256 6082 6112 6448 6082 6480 6448 5282 0 0 5314 1 83886081 6528 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 9200 7 257 0 0 8672 0 3 0 0 6928 704 0 131169 0 0 3346 202 208 98 2 3410 3440 98 2 1874 9 49 1874 1769 481 8672 3410 7104 98 1 8 'Name' 8672 4114 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 120 3 0 0 8 1 0 0] 98 0 4176 0 27 946 5234 8 'Method Source' 5282 0 0 5314 7 1 80 8 'doIt' 8 '(CardLabel text: ''Method Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PUBLIC.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 5232 9520 5392 8 'COMPILEDMETHOD_PUBLIC.ico' 5424 5440 9536 11 1 0 0 410 1008 98 46 0 752 98 2 8 1445007428 1025 9632 1074 0 32 1120 0 834 1152 0 5 498 0 16 98 11 546 1 578 1232 8 '&Save' 9383 1 0 0 0 1266 4097 546 1 578 1328 8 '&Undo' 9397 1 0 0 0 546 1 578 1392 8 '&Redo' 9395 1 0 0 0 1266 4097 546 1 578 1472 8 'Cu&t' 9393 1 0 0 0 546 1 578 1536 8 '&Copy' 9351 1 0 0 0 546 1 578 1600 8 '&Paste' 9389 1 0 0 0 546 1 578 1664 8 '&Delete' 1629 1 0 0 0 1266 4097 546 1 578 1744 8 'Select &All' 9347 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 1794 0 16 1826 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1874 193 193 0 9632 0 8 4294902249 1922 0 0 13 0 234 256 98 42 2032 2002 33 2080 0 3 0 0 0 0 2032 0 0 0 1984 2002 67 0 0 1 0 0 0 0 1984 0 0 0 2096 2002 21 0 0 3 0 0 0 0 2096 0 0 0 2128 2002 1 0 0 1 0 0 0 0 2128 0 0 0 2160 2002 13 2080 0 3 0 0 0 0 2160 0 0 0 2192 2002 25 0 0 3 0 0 0 0 2192 0 0 0 2224 2002 5 2256 0 1 0 0 0 0 2224 0 0 0 2272 2002 19 2080 0 3 0 0 0 0 2272 0 0 0 2304 2002 31 2336 0 3 0 0 0 0 2304 0 0 0 2352 2002 69 2066 66047 0 3 0 0 0 0 2352 0 0 0 2400 2002 75 2448 0 1 0 0 0 0 2400 0 0 0 2464 2002 3 2496 0 129 0 0 0 0 2464 0 0 0 2512 2002 9 2544 0 1 0 0 0 0 2512 0 0 0 2560 2002 17 2080 0 3 0 0 0 0 2560 0 0 0 2592 2002 7 2624 0 1 0 0 0 0 2592 0 0 0 2640 2002 11 2672 0 1 0 0 0 0 2640 0 0 0 2688 2002 29 0 0 3 0 0 0 0 2688 0 0 0 2720 2002 27 2752 0 3 0 0 0 0 2720 0 0 0 2768 2002 23 2800 0 3 0 0 0 0 2768 0 0 0 2816 2002 71 2848 0 3 0 0 0 0 2816 0 0 0 2864 2002 15 2080 0 3 0 0 0 0 2864 0 0 0 98 40 10384 10528 10432 10576 10544 10592 10400 10672 10560 10448 10368 10640 10416 10624 10608 10464 10336 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10352 10480 10656 0 10512 0 0 2914 2128 234 256 98 2 2976 2994 1 1 2672 3024 9632 3040 202 208 704 0 63 9215 0 0 0 0 2448 0 0 0 0 0 234 240 98 4 2128 3104 2192 8 '()[]{}<>' 8 '' 3 234 256 98 4 4096 10304 3184 234 256 98 2 2128 2002 1 0 0 1 0 0 0 0 2128 0 0 0 0 0 0 0 1 0 234 256 98 12 1 3282 1 9632 65025 3 32 1 0 3 3282 3 9632 33423361 5 32 3 0 5 3282 5 9632 511 1 32 5 0 8 'indicator8' 3282 17 9632 33554447 1 32 0 0 8 'indicator10' 3282 21 9632 511 3 32 0 0 8 'indicator9' 3282 19 9632 33554459 13 32 0 0 3346 202 208 98 12 3410 3440 98 2 1874 9 49 1874 1769 481 9632 3410 3520 98 1 9728 9632 3410 3568 98 1 3602 3 1 3 9632 3410 3648 98 1 32 9632 3410 3696 98 1 9215 9632 3410 8 #hoverTime: 98 1 401 9632 3410 3744 98 1 16 9632 3410 3792 98 1 98 3 3842 1 9632 61 3 32 1 3842 3 9632 1 1 16 67108863 3842 5 9632 1 1 16 -67108863 9632 3410 3920 98 1 0 9632 3410 3968 98 1 16 9632 3410 4016 98 1 9 9632 3410 4064 98 1 4096 9632 4114 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 120 3 0 0 8 1 0 0] 98 0 4176 0 27 946 5234 8 'Original Source' 5282 0 0 5314 7 1 80 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 5232 11744 5392 8 'COMPILEDMETHOD_PRIVATE.ico' 5424 5440 11760 11 1 0 0 410 1008 98 46 0 752 98 2 8 1445007428 1025 11856 1074 0 32 1120 0 834 8 4278190080 0 5 0 1794 0 16 1826 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1874 193 193 0 11856 0 8 4294902249 1922 0 0 11 0 234 256 98 42 1984 2002 67 0 0 1 0 0 0 0 1984 0 0 0 2032 2002 33 2066 16646145 0 3 0 0 0 0 2032 0 0 0 2096 2002 21 0 0 3 0 0 0 0 2096 0 0 0 2128 2002 1 0 0 1 0 0 0 0 2128 0 0 0 2160 2002 13 12128 0 3 0 0 0 0 2160 0 0 0 2192 2002 25 0 0 3 0 0 0 0 2192 0 0 0 2224 2002 5 2066 16711169 0 1 0 0 0 0 2224 0 0 0 2272 2002 19 12128 0 3 0 0 0 0 2272 0 0 0 2304 2002 31 2066 16646399 0 3 0 0 0 0 2304 0 0 0 2352 2002 69 2434 33554465 0 3 0 0 0 0 2352 0 0 0 2400 2002 75 2448 0 1 0 0 0 0 2400 0 0 0 2464 2002 3 2066 16646399 0 129 0 0 0 0 2464 0 0 0 2512 2002 9 2544 0 1 0 0 0 0 2512 0 0 0 2560 2002 17 12128 0 3 0 0 0 0 2560 0 0 0 2592 2002 7 2066 65025 0 1 0 0 0 0 2592 0 0 0 2640 2002 11 2672 0 1 0 0 0 0 2640 0 0 0 2688 2002 29 0 0 3 0 0 0 0 2688 0 0 0 2720 2002 27 2752 0 3 0 0 0 0 2720 0 0 0 2768 2002 23 2066 321 0 3 0 0 0 0 2768 0 0 0 2816 2002 71 2848 0 3 0 0 0 0 2816 0 0 0 2864 2002 15 12128 0 3 0 0 0 0 2864 0 0 0 98 40 12160 12336 12208 12400 12368 12432 12176 12528 12384 12240 12144 12480 12192 12464 12448 12256 12112 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 12096 12288 12512 0 12320 0 0 2914 2128 234 256 98 2 2976 2994 1 1 2672 3024 11856 3040 202 208 704 0 63 9215 0 0 0 0 2448 0 0 0 0 0 0 8 '' 3 234 256 98 2 3184 234 256 98 2 2128 2002 1 0 0 1 0 0 0 0 2128 0 0 0 0 0 0 0 1 0 234 256 98 6 1 3282 1 11856 65025 3 32 1 0 3 3282 3 11856 33423361 5 32 3 0 5 3282 5 11856 511 1 32 5 0 3346 202 208 98 10 3410 3440 98 2 1874 9 49 1874 1769 481 11856 3410 3568 98 1 3602 3 1 3 11856 3410 3648 98 1 32 11856 3410 3696 98 1 9215 11856 3410 3744 98 1 16 11856 3410 3792 98 1 98 3 3842 1 11856 61 3 32 1 3842 3 11856 1 1 16 67108863 3842 5 11856 1 1 16 -67108863 11856 3410 3920 98 1 0 11856 3410 3968 98 1 16 11856 3410 4016 98 1 9 11856 3410 4064 98 1 4096 11856 4114 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 120 3 0 0 8 1 0 0] 98 0 4176 0 27 5456 234 256 98 12 992 8 'classDefinition' 4224 8 'classDocumentation' 8672 8 'globals' 11856 8 'originalSource' 5456 8 'packageInfo' 9632 8 'methodSource' 0 410 8 
		##(Smalltalk.TabViewXP)  98 28 0 752 98 2 8 1140916736 1 13472 5650 202 208 98 6 5248 8544 976 4208 11728 9504 0 5712 0 0 1 0 0 0 13472 0 8 4294902877 5282 0 0 918822 
		##(Smalltalk.CompiledMethod)  2 3 8 
		##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 6112 13616 7 257 0 5282 0 0 13634 2 3 8 
		##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 13712 7 257 0 6160 0 0 0 0 0 8 #smallIcons 0 0 0 0 0 3346 202 208 98 3 3410 3440 98 2 1874 1 1 1874 1785 537 13472 3410 8 #basicSelectionsByIndex: 98 1 98 1 3 13472 3410 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 13472 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 12 1 0 0] 98 0 4176 0 27 3346 202 208 98 1 3410 3440 98 2 1874 1 553 1874 1785 537 752 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 1 0 0 124 3 0 0 32 2 0 0] 98 7 5456 8672 992 4224 11856 9632 13472 4176 0 27 8 'textAreaTabs' 410 8 
		##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 14272 0 834 8 4278190080 0 517 0 0 0 14272 3346 202 208 98 1 3410 3440 98 2 1874 1 535 1874 1785 19 14272 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 11 1 0 0 124 3 0 0 20 1 0 0] 98 0 4176 0 27 8 'splitter' 0 3346 202 208 98 2 3410 3440 98 2 1874 2879 21 1874 1785 1089 416 3410 3520 98 1 512 416 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 27 9 0 0 42 2 0 0] 98 3 410 432 98 15 0 416 98 2 8 1140850688 131073 14752 0 0 0 5 0 0 0 14752 658 234 240 98 4 410 432 98 15 0 14752 98 2 8 1140850688 131073 14864 0 0 0 5 0 0 0 14864 658 234 240 98 6 410 432 98 15 0 14864 98 2 8 1140850688 131073 14976 0 0 0 5 0 0 0 14976 658 234 240 704 32 234 256 98 2 410 8 
		##(Smalltalk.TreeView)  98 27 0 14976 98 2 8 1409352231 1025 15104 590918 3 
		##(Smalltalk.TreeModel)  0 5712 525062 
		##(Smalltalk.TreeNode)  0 0 0 234 256 704 834 8 4278190080 0 21 0 0 0 15104 0 8 4294903469 5282 0 0 5314 5 1 5280 8 'doIt' 8 '[:each | each key isEmpty ifTrue: [''--All Categories--''] ifFalse: [each key last]].' 8 #[33 105 226 0 159 119 31 106 226 0 161 106] 8 #key 8 #isEmpty 8 '--All Categories--' 8 #last 15312 7 257 0 13744 6160 0 0 0 0 0 234 240 704 17 8 #noIcons 1 0 3346 202 208 98 1 3410 3440 98 2 1874 1 1 1874 219 535 15104 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 109 0 0 0 11 1 0 0] 98 0 4176 0 27 8 'classCategoryList' 0 3346 202 208 98 1 3410 3440 98 2 1874 383 1 1874 219 535 14976 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 191 0 0 0 0 0 0 0 44 1 0 0 11 1 0 0] 98 1 15104 4176 0 27 7 410 432 98 15 0 14864 98 2 8 1140850688 131073 15824 0 0 0 5 0 0 0 15824 852230 
		##(Smalltalk.FramingLayout)  234 240 98 4 410 768 98 16 0 15824 98 2 8 1409286144 131073 15952 0 834 8 4278190080 0 5 0 0 0 15952 882 202 208 98 2 946 5234 8 'Instance' 5282 0 0 5314 7 1 80 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 5232 16128 5392 8 'Class.ico' 5424 5440 16144 11 1 0 0 410 432 98 15 0 15952 98 2 8 1140850688 131073 16240 0 0 0 5 0 0 0 16240 0 234 256 704 0 3346 202 208 98 1 3410 3440 98 2 1874 9 9 1874 425 1 16240 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 216 0 0 0 4 0 0 0] 98 0 4176 0 27 946 5234 8 'Class' 5282 0 0 5314 7 1 80 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 5232 16512 5392 8 'Metaclass.ico' 5424 5440 16528 11 1 0 0 410 432 98 15 0 15952 98 2 8 1140850688 131073 16624 0 0 0 5 0 0 0 16624 0 234 256 704 0 3346 202 208 98 1 3410 3440 98 2 1874 9 9 1874 425 1 16624 4114 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 216 0 0 0 4 0 0 0] 98 0 4176 0 27 16240 234 256 98 4 16624 8 'classTab' 16240 8 'instanceTab' 0 410 13488 98 28 0 15952 98 2 8 1140916738 1 16928 5650 202 208 98 2 16112 16496 0 5712 0 0 1 0 0 0 16928 0 8 4294902877 5282 0 0 13634 2 3 13664 13680 575230339 8 #[30 105 226 0 106] 6112 17056 7 257 0 5282 0 0 13634 2 3 13744 13760 579598755 8 #[30 105 226 0 106] 13792 17104 7 257 0 6160 0 0 0 0 0 13808 0 0 0 0 0 3346 202 208 98 3 3410 3440 98 2 1874 1 1 1874 441 51 16928 3410 13952 98 1 98 1 3 16928 3410 14016 98 2 -1 1 16928 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 25 0 0 0] 98 0 4176 0 27 3346 202 208 98 1 3410 3440 98 2 1874 1 485 1874 441 51 15952 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 242 0 0 0 220 0 0 0 11 1 0 0] 98 3 16240 16624 16928 4176 0 27 1181766 2 
		##(Smalltalk.FramingConstraints)  1180678 
		##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 17586 8 #fixedParentRight 1 17586 8 #fixedParentBottom -49 17586 8 #fixedViewTop 51 410 768 98 16 0 15824 98 2 8 1409286144 131073 17728 0 834 864 0 5 0 0 0 17728 882 202 208 98 2 946 5234 8 'Classes' 5282 0 0 5314 7 1 80 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 5232 17888 5392 8 'Class.ico' 5424 5440 17904 11 1 0 0 410 5584 98 30 0 17728 98 2 8 1409372745 1025 18000 5650 202 208 704 0 5712 834 8 4278190080 0 29 0 0 0 18000 0 8 4294903909 6082 6112 98 0 0 6160 0 0 0 0 0 0 202 208 98 2 6210 8 'Column 1' 357 6256 5282 0 0 5314 5 1 5280 8 'doIt' 8 '[:each | (each subStrings: Character space) first]' 8 #[33 105 17 29 159 178 161 106] 8 
		##(Smalltalk.Character)  8 #space 8 #subStrings: 8 #first 18240 7 257 0 6288 0 0 18000 0 3 0 0 6210 8 '' 69 6256 5282 0 0 5314 6 1 5280 8 'doIt' 8 '[:each | | index |
	index := each indexOf: Character space.
	0 < index ifTrue: [each copyFrom: index + 2 to: each size - 1] ifFalse: ['''']]' 8 #[34 105 17 29 159 178 90 62 18 128 221 9 233 1 64 126 17 145 99 193 106 33 106] 18320 18336 8 #indexOf: 6384 18400 18416 7 65793 0 6082 6480 98 0 0 0 18000 0 1 0 0 6928 704 0 131169 0 0 3346 202 208 98 3 3410 3440 98 2 1874 9 49 1874 425 435 18000 3410 7104 98 1 8 'Column 1' 18000 3410 8 #columnOrder: 98 1 98 2 5 3 18000 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 216 0 0 0 241 0 0 0] 98 0 4176 0 27 946 5234 8 'Hierarchy' 5282 0 0 5314 7 1 80 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 5232 18848 5392 8 'ClassHierarchyDiagram.ico' 5424 5440 18864 11 1 0 0 410 15120 98 27 0 17728 98 2 8 1409352231 1025 18960 15186 0 5712 15218 0 0 0 234 256 704 834 8 4278190080 0 29 0 0 0 18960 0 8 4294903469 5282 0 0 5314 2 1 6080 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] 15440 19120 7 257 0 13744 6160 0 0 0 0 0 234 240 704 17 15472 1 0 3346 202 208 98 1 3410 3440 98 2 1874 9 49 1874 425 435 18960 4114 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 216 0 0 0 241 0 0 0] 98 0 4176 0 27 18000 234 256 98 4 18960 8 'classHierarchy' 18000 8 'classList' 0 410 13488 98 28 0 17728 98 2 8 1140916736 1 19440 5650 202 208 98 2 17872 18832 0 5712 0 0 1 0 0 0 19440 0 8 4294902877 5282 0 0 13634 2 3 13664 13680 575230339 8 #[30 105 226 0 106] 6112 19568 7 257 0 5282 0 0 13634 2 3 13744 13760 579598755 8 #[30 105 226 0 106] 13792 19616 7 257 0 6160 0 0 0 0 0 13808 0 0 0 0 0 3346 202 208 98 3 3410 3440 98 2 1874 1 1 1874 441 491 19440 3410 13952 98 1 98 1 3 19440 3410 14016 98 2 -1 1 19440 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 245 0 0 0] 98 0 4176 0 27 3346 202 208 98 1 3410 3440 98 2 1874 1 1 1874 441 491 17728 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 220 0 0 0 245 0 0 0] 98 3 18000 18960 19440 4176 0 27 17554 17600 1 17632 1 17586 8 #fixedParentTop 1 17664 -43 234 256 98 4 15952 8 'instanceClassTabs' 17728 8 'classHierarchyTabs' 0 3346 202 208 98 1 3410 3440 98 2 1874 619 1 1874 441 535 15824 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 53 1 0 0 0 0 0 0 17 2 0 0 11 1 0 0] 98 2 17728 15952 4176 0 27 13 410 8 
		##(Smalltalk.SlideyInneyOuteyThing)  98 23 0 14864 98 2 8 1409286144 131073 20336 0 834 8 4278190080 0 517 0 0 0 20336 882 202 208 98 2 946 5234 8 'Packages' 5282 0 0 5314 7 1 5232 8 'doIt' 8 '(CardLabel text: ''Packages'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 5232 20528 5392 8 'Package.ico' 5424 5440 20544 11 1 0 0 410 5584 98 30 0 410 8 
		##(Smalltalk.SlidingCardTray)  98 22 0 20336 98 2 8 1140850688 131073 20672 0 834 20432 0 5 0 0 0 20672 20448 234 256 98 4 20640 8 'packageList' 410 5584 98 30 0 20672 98 2 8 1409372233 1025 20816 5650 202 208 704 0 5712 834 18112 0 21 0 0 0 20816 0 8 4294903909 6082 6112 98 0 0 6160 0 0 0 0 0 0 202 208 98 1 6210 8 'Column 1' 309 6256 6082 6112 20960 6288 5282 0 0 5314 2 1 5280 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] 15392 21056 7 257 0 0 20816 0 3 0 5282 0 0 5314 5 1 6528 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] 8 #item 8 #font 8 #beBold 8 #font: 21136 7 257 0 6928 704 0 131169 0 0 3346 202 208 98 2 3410 3440 98 2 1874 1 37 1874 309 483 20816 3410 7104 98 1 8 'Column 1' 20816 4114 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 154 0 0 0 3 1 0 0] 98 0 4176 0 27 8 'dictionaryList' 0 410 13488 98 28 0 20336 98 2 8 1140916864 1 21504 5650 202 208 98 2 20512 5234 8 'Dictionaries' 5282 0 0 5314 7 1 80 8 'doIt' 8 '(CardLabel text: ''Dictionaries'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 5232 21632 5392 8 'Dictionary.ico' 5424 5440 21648 11 1 0 0 0 5712 834 20432 0 1 0 0 0 21504 0 8 4294902877 8 
		##(Smalltalk.BasicListAbstract)  13744 6160 0 0 0 0 0 13808 0 0 0 0 0 3346 202 208 98 3 3410 3440 98 2 1874 1 1 1874 365 535 21504 3410 13952 98 1 98 1 3 21504 3410 14016 98 2 -1 1 21504 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 182 0 0 0 11 1 0 0] 98 0 4176 0 27 0 20336 1874 33 33 1049862 
		##(Smalltalk.ButtonInteractor)  20672 0 590342 
		##(Smalltalk.Rectangle)  1874 273 3 1874 305 35 1 578 8 #togglePin 8 'Pin or Unpin the tray' 1 1 0 0 0 3346 202 208 98 1 3410 3440 98 2 1874 49 9 1874 309 519 20672 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 24 0 0 0 4 0 0 0 178 0 0 0 7 1 0 0] 98 2 20640 20816 4176 0 27 98 2 8 1409405001 1025 20640 5650 202 208 704 0 5712 834 18112 0 5 0 0 0 20640 0 8 4294903909 6082 6112 98 0 0 6160 0 0 0 0 0 0 202 208 98 1 6210 8 'Column 1' 309 6256 6082 6112 22464 6288 5282 0 0 5314 2 1 5280 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] 15392 22560 7 257 0 0 20640 0 3 0 5282 0 0 5314 6 1 5280 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[34 105 226 0 142 221 9 17 226 1 100 160 97 161 180 106 60 106] 21216 21232 21248 8 #beItalic 21264 22640 7 257 0 6928 704 0 131169 0 0 3346 202 208 98 2 3410 3440 98 2 1874 1 37 1874 309 483 20640 3410 7104 98 1 8 'Column 1' 20640 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 154 0 0 0 3 1 0 0] 98 0 4176 0 27 946 21616 20816 20640 234 256 98 2 20672 8 'packageDictionaryTabs' 0 21504 20672 1874 201 201 401 1 31 0 0 3346 202 208 98 1 3410 3440 98 2 1874 1 1 1874 365 535 20336 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 182 0 0 0 11 1 0 0] 98 2 20672 21504 4176 0 27 11 32 234 256 704 0 3346 202 208 98 1 3410 3440 98 2 1874 1 1 1874 1059 535 14864 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 17 2 0 0 11 1 0 0] 98 5 20336 410 14288 98 12 0 14864 98 2 8 1140850688 1 23360 0 834 14368 0 517 0 0 0 23360 3346 202 208 98 1 3410 3440 98 2 1874 365 1 1874 19 535 23360 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 182 0 0 0 0 0 0 0 191 0 0 0 11 1 0 0] 98 0 4176 0 27 14976 410 14288 98 12 0 14864 98 2 8 1140850688 1 23600 0 834 14368 0 517 0 0 0 23600 3346 202 208 98 1 3410 3440 98 2 1874 601 1 1874 19 535 23600 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 1 0 0 0 0 0 0 53 1 0 0 11 1 0 0] 98 0 4176 0 27 15824 4176 0 27 7 410 432 98 15 0 14752 98 2 8 1140850688 131073 23840 0 0 0 5 0 0 0 23840 658 234 240 704 32 234 256 704 0 3346 202 208 98 1 3410 3440 98 2 1874 1077 1 1874 709 535 23840 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 2 0 0 0 0 0 0 124 3 0 0 11 1 0 0] 98 3 410 20352 98 23 0 23840 98 2 8 1409286144 131073 24112 0 834 8 4278190080 0 517 0 0 0 24112 882 202 208 98 2 946 8 'Categories' 410 5584 98 30 0 410 20688 98 22 0 24112 98 2 8 1140850688 131073 24320 0 834 24192 0 5 0 0 0 24320 24208 234 256 98 4 24288 8 'categoryList' 410 5584 98 30 0 24320 98 2 8 1409372233 1025 24448 5650 202 208 704 0 5712 834 8 4278190080 0 21 0 0 0 24448 0 8 4294903909 6082 6112 98 0 0 6160 0 0 0 0 0 0 202 208 98 1 6210 8 'Column 1' 289 6256 6082 6112 24608 6288 0 0 24448 0 3 0 5282 0 0 5314 9 1 5280 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] 21216 18368 18320 18336 21232 21248 22720 21264 24704 7 257 0 6928 704 0 131169 0 0 3346 202 208 98 2 3410 3440 98 2 1874 1 37 1874 289 483 24448 3410 7104 98 1 8 'Column 1' 24448 4114 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 144 0 0 0 3 1 0 0] 98 0 4176 0 27 8 'variableList' 0 410 13488 98 28 0 24112 98 2 8 1140916864 1 25008 5650 202 208 98 2 24272 8 'Variables' 0 5712 834 24192 0 1 0 0 0 25008 0 8 4294902877 21776 13744 6160 0 0 0 0 0 15472 0 0 0 0 0 3346 202 208 98 3 3410 3440 98 2 1874 1 1 1874 345 535 25008 3410 13952 98 1 98 1 3 25008 3410 14016 98 2 -1 1 25008 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 0 0 0 11 1 0 0] 98 0 4176 0 27 0 24112 1874 33 33 22050 24320 0 22082 1874 253 3 1874 285 35 1 578 22160 8 'Pin or Unpin the tray' 1 1 0 0 0 3346 202 208 98 1 3410 3440 98 2 1874 49 9 1874 289 519 24320 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 24 0 0 0 4 0 0 0 168 0 0 0 7 1 0 0] 98 2 24288 24448 4176 0 27 98 2 8 1409372233 1025 24288 5650 202 208 704 0 5712 834 24560 0 21 0 0 0 24288 0 8 4294903909 6082 6112 98 0 0 6160 0 0 0 0 0 0 202 208 98 1 6210 8 'Column 1' 289 6256 6082 6112 25792 6288 0 0 24288 0 3 0 5282 0 0 5314 9 1 6528 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] 21216 18368 18320 18336 21232 21248 22720 21264 25888 7 257 0 6928 704 0 131169 0 0 3346 202 208 98 2 3410 3440 98 2 1874 1 37 1874 289 483 24288 3410 7104 98 1 8 'Column 1' 24288 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 144 0 0 0 3 1 0 0] 98 0 4176 0 27 946 25120 24448 24288 234 256 98 2 24320 8 'categoryVariableTabs' 0 25008 24320 1874 201 201 401 1 31 0 0 3346 202 208 98 1 3410 3440 98 2 1874 1 1 1874 345 535 24112 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 0 0 0 11 1 0 0] 98 2 24320 25008 4176 0 27 410 14288 98 12 0 23840 98 2 8 1140850688 1 26416 0 834 14368 0 517 0 0 0 26416 3346 202 208 98 1 3410 3440 98 2 1874 345 1 1874 19 535 26416 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 172 0 0 0 0 0 0 0 181 0 0 0 11 1 0 0] 98 0 4176 0 27 410 432 98 15 0 23840 98 2 8 1140850688 131073 26656 0 0 0 5 0 0 0 26656 15890 234 240 98 6 410 5584 98 30 0 26656 98 2 8 1409372233 1025 26768 5650 202 208 704 0 5712 834 24560 0 13 0 0 0 26768 0 8 4294903909 6082 6112 98 0 13744 6160 0 0 0 0 0 0 202 208 98 2 6210 8 'Column 1' 315 6256 6082 6112 26912 6288 5282 0 0 5314 1 83886081 6528 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 27008 7 257 0 0 26768 0 3 0 5282 0 0 5314 5 1 5280 8 'doIt' 8 '[:each | (each item at: 4) ifTrue: [each font: each font beItalic]]' 8 #[33 105 226 0 214 4 148 123 17 226 1 160 179 106 60 106] 21216 21232 22720 21264 27088 7 257 0 6210 8 '' 33 8 #center 3410 8 #empty 98 0 80 6082 6480 98 0 0 0 26768 5282 0 0 5314 6 1 5280 8 'doIt' 8 '[:each | ((each at: 2) ifTrue: [JadeSystemBrowserPresenter overriddenIcon] ifFalse: [Icon blank]) 
	imageIndex]' 8 #[34 105 17 64 148 120 29 159 111 31 161 162 106] 8 
		##(Smalltalk.JadeSystemBrowserPresenter)  8 #overriddenIcon 5392 8 #blank 8 #imageIndex 27296 7 257 0 1 0 0 6928 704 0 131173 0 0 3346 202 208 98 3 3410 3440 98 2 1874 1 47 1874 347 443 26768 3410 7104 98 1 8 'Column 1' 26768 3410 18720 98 1 98 2 5 3 26768 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 173 0 0 0 244 0 0 0] 98 0 4176 0 27 17554 17600 1 17632 1 20080 47 17664 -45 410 8 
		##(Smalltalk.ComboBox)  98 17 0 26656 98 2 8 1412498947 1025 27712 5650 202 208 704 0 5712 834 8 4278190080 0 5 0 0 0 27712 0 8 4294904171 6082 6112 98 0 704 401 3346 202 208 98 1 3410 3440 98 2 1874 1 489 1874 347 47 27712 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 173 0 0 0 11 1 0 0] 98 0 4176 0 27 17554 17600 1 17632 1 17664 -45 17696 47 410 27728 98 17 0 26656 98 2 8 1412498947 1025 28080 5650 202 208 704 0 5712 834 27840 0 5 0 0 0 28080 0 8 4294904171 6082 6112 98 0 704 401 3346 202 208 98 1 3410 3440 98 2 1874 1 1 1874 347 47 28080 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 173 0 0 0 23 0 0 0] 98 0 4176 0 27 17554 17600 1 17632 1 20080 1 17696 47 234 256 98 6 26768 8 'methodList' 27712 8 'overrideList' 28080 8 'superclassList' 0 3346 202 208 98 1 3410 3440 98 2 1874 363 1 1874 347 535 26656 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 181 0 0 0 0 0 0 0 98 1 0 0 11 1 0 0] 98 3 28080 26768 27712 4176 0 27 4176 0 27 5 32 234 256 704 0 3346 202 208 98 1 3410 3440 98 2 1874 1 1 1874 1785 535 14752 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 11 1 0 0] 98 3 14864 410 14288 98 12 0 14752 98 2 8 1140850688 1 28832 0 834 14368 0 517 0 0 0 28832 3346 202 208 98 1 3410 3440 98 2 1874 1059 1 1874 19 535 28832 4114 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 17 2 0 0 0 0 0 0 26 2 0 0 11 1 0 0] 98 0 4176 0 27 23840 4176 0 27 14272 752 4176 0 27 )! !
!JadeSystemBrowserPresenter class categoriesFor: #overriddenIcon!public! !
!JadeSystemBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

JadeAutoSystemBrowserPresenter guid: (GUID fromString: '{A4B39CE8-DF3A-4C06-A336-25F599463AE0}')!
JadeAutoSystemBrowserPresenter comment: 'This class register information in the registry (JadeAutocompletationRegistry) each time a user select a class in the hierarchy browser.

see:
#registerClassData'!
!JadeAutoSystemBrowserPresenter categoriesForClass!Unclassified! !
!JadeAutoSystemBrowserPresenter methodsFor!

codePresenterIsMethod

	^true!

createArrayFromString: arrayString
	| startParenthesis endParenthesis|

	startParenthesis := (arrayString indexOfSubCollection: '(') + 1.
	endParenthesis := (arrayString indexOfSubCollection: ')')  - 1.

	[^((arrayString copyFrom: startParenthesis to: endParenthesis) subStrings: ' ') select: [:each | each notEmpty]] on: Error do: [:ex | ^Array new]
!

createSchematicWiringForMethodSource

	methodSourcePresenter 	
		when: #'hoverStart:'						send: #'methodHoverStart:'		to: self;
		when: #'hoverEnd:'						send: #'methodHoverEnd:'			to: self;
		when: #'aboutToDisplayMenu:'		send: #'methodMenu:'				to: self;
		when: #'leftButtonDoubleClicked:'	send: #'methodDoubleClicked:'	to: self;
		when: #'valueChanged'					send: #'methodChanged'			to: self;
		when: #'focusLost'						send: #'cancelCallTip'				to: methodSourcePresenter view;
		yourself.
!

getClassHierarchy

	^classHierarchyPresenter selection!

getClassName

	^classHierarchyPresenter selection last!

gsClassMethods

	^methodListPresenter model collect: [:each | each first]!

gsClassPoolDictionaries
	| arrayString |

	classDefinition ifNil: [^self].

	arrayString := (classDefinition copyFrom: (classDefinition indexOfSubCollection: 'poolDictionaries:') to:  (classDefinition indexOfSubCollection: 'inDictionary:') - 1) allButFirst: 17"poolDictionaries:".

	^self createArrayFromString: arrayString.!

gsClassVariables
	| arrayString |

	classDefinition ifNil: [^self].

	arrayString := (classDefinition copyFrom: (classDefinition indexOfSubCollection: 'classVars:') to:  (classDefinition indexOfSubCollection: 'classInstVars:') - 1) allButFirst: 10 "classVars:".

	^self createArrayFromString: arrayString.!

gsInstClassVariables
	| arrayString |

	classDefinition ifNil: [^self].

	arrayString := (classDefinition copyFrom: (classDefinition indexOfSubCollection: 'classInstVars:') to:  (classDefinition indexOfSubCollection: 'poolDictionaries:') - 1) allButFirst: 14 "classInstVars:".

	^self createArrayFromString: arrayString.!

gsInstVariables
	"The receiver get the instances variable names of the selected class from the class definition pane <classDefinition>"
	| instVars arrayString |

	classDefinition ifNil: [^self].
	((classDefinition indexOfSubCollection: 'instVarNames:') = 0) ifTrue: [^#()].
	((classDefinition indexOfSubCollection: 'classVars:') = 0) ifTrue: [^#()].
	arrayString := (classDefinition copyFrom: (classDefinition indexOfSubCollection: 'instVarNames:') to:  (classDefinition indexOfSubCollection: 'classVars:') - 1) allButFirst: 13 "remove: -instVarNames:-".

	instVars := self createArrayFromString: arrayString.

	^instVars!

gsMethods


	^methodListPresenter model collect: [:each | each first]!

hasClassSideSelected

	^instanceClassTabs currentCard name = 'classTab'!

lastGsShape
	^lastGsShape!

lastGsShape: anObject
	lastGsShape := anObject!

newMethodPresenter

	^JadeAutoTextPresenter new.!

registerClassData
	| gsMethods gsClassMethods |

	gsClassMethods := OrderedCollection new.
	gsMethods := OrderedCollection new.
	self hasClassSideSelected ifTrue: [gsClassMethods := self gsMethods] ifFalse: [gsMethods := self gsMethods].
	classHierarchyPresenter hasSelection ifFalse: [^self].
	lastGsShape := JadeGsClassShape new.
	lastGsShape  name: self getClassName;
		gsClassHierarchy: self getClassHierarchy;
		gsInstVariables: self gsInstVariables;
		gsClassVariables: self gsClassVariables;
		gsInstClassVariables: self gsInstClassVariables; 
		gsMethods: gsMethods; 
		gsClassMethods: gsClassMethods;
		gsPoolDictionaries: self gsClassPoolDictionaries;
		yourself.

	lastGsShape update.
	methodSourcePresenter lastGsShape: lastGsShape.!

removeClass
	| list |

	list := self selectedClasses.
	
	super removeClass.

	list do: [:each | self registry unregister: each]!

updateAndSelect: aView

	updateCount := updateCount + 1.
	keystrokeTime < Time millisecondClockValue ifTrue: [
		self updateAndSelectA: aView.
	] ifFalse: [
		[self updateAndSelectB: aView] fork.
	].

	self registerClassData.
!

updateMethodList
    "The receiver must keep update with any add/remove of a method in order to be acurate in the autocompletation"

    super updateMethodList.

    lastGsShape ifNil: [^self].

    self hasClassSideSelected ifTrue: [^lastGsShape gsClassMethods: self gsMethods].

    ^lastGsShape gsMethods: self gsMethods.! !
!JadeAutoSystemBrowserPresenter categoriesFor: #codePresenterIsMethod!public! !
!JadeAutoSystemBrowserPresenter categoriesFor: #createArrayFromString:!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #createSchematicWiringForMethodSource!create schemantic wiring!public! !
!JadeAutoSystemBrowserPresenter categoriesFor: #getClassHierarchy!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #getClassName!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #gsClassMethods!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #gsClassPoolDictionaries!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #gsClassVariables!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #gsInstClassVariables!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #gsInstVariables!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #gsMethods!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #hasClassSideSelected!public! !
!JadeAutoSystemBrowserPresenter categoriesFor: #lastGsShape!accessing!private! !
!JadeAutoSystemBrowserPresenter categoriesFor: #lastGsShape:!accessing!private! !
!JadeAutoSystemBrowserPresenter categoriesFor: #newMethodPresenter!public! !
!JadeAutoSystemBrowserPresenter categoriesFor: #registerClassData!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #removeClass!menu handlers!public! !
!JadeAutoSystemBrowserPresenter categoriesFor: #updateAndSelect:!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #updateMethodList!public!updating! !

JadeFindClassDialog guid: (GUID fromString: '{0681B51C-D226-407A-8BE0-49345512C4F9}')!
JadeFindClassDialog comment: ''!
!JadeFindClassDialog categoriesForClass!Unclassified! !
!JadeFindClassDialog methodsFor!

createComponents

	super createComponents.
	classListPresenter 		:= self add: ListPresenter		new name: 'classList'.
	nameEntryPresenter		:= self add: TextPresenter		new name: 'nameEntry'.
!

createSchematicWiring

	super createSchematicWiring.
	classListPresenter 		when: #actionPerformed send: #ok							to: self.
	nameEntryPresenter		when: #valueChanged 	send: #updateClassList 	to: self.
!

ok

	self model value: classListPresenter selectionOrNil.
	super ok.
!

onViewOpened

	super onViewOpened.
	availableClasses := self model value asSortedCollection.
	self model value: nil.
	nameEntryPresenter value: '*'.
!

updateClassList 

	| list |
	list := availableClasses select: [:each | nameEntryPresenter value , '*' match: each key].
	classListPresenter list: list.
	list size >= 1 ifTrue: [
		classListPresenter selectionByIndex: 1.
	].
! !
!JadeFindClassDialog categoriesFor: #createComponents!private! !
!JadeFindClassDialog categoriesFor: #createSchematicWiring!private! !
!JadeFindClassDialog categoriesFor: #ok!private! !
!JadeFindClassDialog categoriesFor: #onViewOpened!private! !
!JadeFindClassDialog categoriesFor: #updateClassList!private! !

!JadeFindClassDialog class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 560 0 0 0 7 0 0 0 560 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.Presenter)  8 #resource_OK_Cancel_button_block 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  21 437 834 469 71 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 218 0 0 0 244 0 0 0 253 0 0 0] 98 0 834 193 193 0 27 0 0 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 992 0 482 512 0 7 0 0 0 992 0 8 4294902907 852486 ##(Smalltalk.NullConverter)  0 0 5 706 202 208 98 4 770 800 98 2 834 211 1 834 301 41 992 770 8 #text: 98 1 8 '*' 992 770 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 770 8 #isTextModified: 98 1 32 992 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 0 0 0 0 255 0 0 0 20 0 0 0] 98 0 944 0 27 8 'nameEntry' 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 1504 590662 2 ##(Smalltalk.ListModel)  202 208 928 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 512 0 7 0 0 0 1504 0 8 4294902549 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 353 8 #left 1696 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  4 1 1872 8 'doIt' 8 '[:each | each key subStrings first]' 8 #[32 105 226 0 159 160 106] 8 #key 8 #subStrings 8 #first 1888 7 257 0 0 1504 0 3 0 0 1794 8 'Dictionary' 151 1840 1696 1856 1874 0 459302 ##(Smalltalk.Context)  1 1 0 0 1906 0 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  928 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 1 41 834 511 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 9033 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2118378871 706 202 208 98 3 770 800 98 2 834 3359 21 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 143 6 0 0 10 0 0 0 147 7 0 0 49 1 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2848 0 0 0 7 0 0 0 2848 0 8 4294902555 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2848 770 1264 98 1 8 'Partial Name:' 2848 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

resource_ThreeColumnView
	"Answer the literal data from which the 'ThreeColumnView' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_ThreeColumnView)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 560 0 0 0 7 0 0 0 560 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.Presenter)  8 #resource_OK_Cancel_button_block 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  21 445 834 1149 71 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 222 0 0 0 72 2 0 0 1 1 0 0] 98 0 834 193 193 0 27 0 0 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 992 0 482 512 0 7 0 0 0 992 0 8 4294902451 852486 ##(Smalltalk.NullConverter)  0 0 5 706 202 208 98 4 770 800 98 2 834 215 1 834 971 41 992 770 8 #text: 98 1 8 '*' 992 770 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 770 8 #isTextModified: 98 1 32 992 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 107 0 0 0 0 0 0 0 80 2 0 0 20 0 0 0] 98 0 944 0 27 8 'nameEntry' 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 1504 590662 2 ##(Smalltalk.ListModel)  202 208 928 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 512 0 7 0 0 0 1504 0 8 4294902253 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 4 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 401 8 #left 1696 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 1872 8 'doIt' 8 '[:each | each value at: 1]' 8 #[29 105 17 142 63 148 106] 1888 7 257 0 0 1504 0 1 0 0 1794 8 'Dictionary' 191 1840 1696 1856 1874 0 0 1906 1 83886081 1872 8 'doIt' 8 '[:each | each value at: 2]' 8 #[29 105 17 142 64 148 106] 2016 7 257 0 0 1504 0 1 0 0 1794 8 'Category' 271 1840 459270 ##(Smalltalk.Message)  8 #displayString 98 0 1856 1874 0 0 1906 1 83886081 1872 8 'doIt' 8 '[:each | each value at: 3]' 8 #[29 105 17 142 214 3 148 106] 2192 7 257 0 0 1504 0 1 0 0 1794 8 'Package' 271 1840 2130 2160 98 0 2130 8 #<= 2320 1874 0 0 1906 1 83886081 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each value at: 4]' 8 #[29 105 17 142 214 4 148 106] 2368 7 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 5 41 834 1181 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 20 0 0 0 80 2 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 10939 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 1987604964 706 202 208 98 3 770 800 98 2 834 5119 21 834 1201 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 49 1 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 3056 0 0 0 7 0 0 0 3056 0 8 4294902375 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 3056 770 1264 98 1 8 'Partial Name:' 3056 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )! !
!JadeFindClassDialog class categoriesFor: #resource_Default_view!public!resources-views! !
!JadeFindClassDialog class categoriesFor: #resource_ThreeColumnView!public!resources-views! !

SUnitResultDialog guid: (GUID fromString: '{0FEF434A-DCF0-49C0-8D00-A179396ABF7B}')!
SUnitResultDialog comment: ''!
!SUnitResultDialog categoriesForClass!Unclassified! !
!SUnitResultDialog methodsFor!

apply

	self model: listPresenter selection.
	super apply.
!

createComponents

	super createComponents.
	listPresenter := self add: ListPresenter new name: 'list'.
!

createSchematicWiring

	super createSchematicWiring.
	listPresenter when: #'actionPerformed' send: #'ok' to: self.
!

onViewOpened

	super onViewOpened.
	self caption: (self model value at: 2).
	listPresenter list: (self model value at: 3).
!

openWorkspaceWithList

	| stream |
	stream := WriteStream on: String new.
	listPresenter list do: [:each | 
		stream nextPutAll: each; nextPut: $.; cr.
	].
	(JadeWorkspace showOn: (self model value at: 1)) showText: stream contents.
!

queryCommand: aCommandQuery

	(#(#'ok' ) includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: listPresenter hasSelection.
		^true.
	].
	^super queryCommand: aCommandQuery.
! !
!SUnitResultDialog categoriesFor: #apply!public! !
!SUnitResultDialog categoriesFor: #createComponents!public! !
!SUnitResultDialog categoriesFor: #createSchematicWiring!public! !
!SUnitResultDialog categoriesFor: #onViewOpened!public! !
!SUnitResultDialog categoriesFor: #openWorkspaceWithList!public! !
!SUnitResultDialog categoriesFor: #queryCommand:!public! !

!SUnitResultDialog class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 8 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 592 0 0 0 7 0 0 0 592 0 8 4294903629 1180998 4 ##(Smalltalk.CommandDescription)  8 #openWorkspaceWithList 8 'Open Workspace with List' 1 1 0 0 32 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  25 661 882 361 51 592 818 8 #text: 98 1 8 'Open Workspace with List' 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 12 0 0 0 74 1 0 0 192 0 0 0 99 1 0 0] 98 0 882 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 5 1106 8 #fixedViewLeft 361 1106 8 #fixedParentBottom -43 1106 8 #fixedViewTop 51 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1409372237 1025 1248 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 1248 0 8 4294904243 459270 ##(Smalltalk.Message)  8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 'Column 1' 871 8 #left 1474 1504 1520 8 ##(Smalltalk.SortedCollection)  0 0 1248 0 3 0 0 8 #report 1376 0 131169 0 0 754 202 208 98 2 818 848 98 2 882 21 21 882 879 625 1248 818 944 98 1 8 'Column 1' 1248 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 10 0 0 0 193 1 0 0 66 1 0 0] 98 0 1056 0 27 1074 1120 1 1106 8 #fixedParentRight 1 1106 8 #fixedParentTop 1 1184 -59 410 608 98 20 0 416 98 2 8 1140924416 1 2016 0 0 0 7 0 0 0 2016 0 8 4294903629 690 8 #cancel 8 'Cancel' 1025 1 0 0 32 0 0 0 754 202 208 98 2 818 848 98 2 882 759 661 882 141 51 2016 818 944 98 1 8 'Cancel' 2016 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 123 1 0 0 74 1 0 0 193 1 0 0 99 1 0 0] 98 0 1056 0 29 1074 1952 -139 1152 141 1184 -43 1216 51 410 608 98 20 0 416 98 2 8 1140924416 1 2368 0 0 0 7 0 0 0 2368 0 8 4294903629 690 8 #ok 8 'Run in Debugger' 1 1 0 0 16 0 0 0 754 202 208 98 2 818 848 98 2 882 499 661 882 251 51 2368 818 944 98 1 8 'Run in Debugger' 2368 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 0 0 0 74 1 0 0 118 1 0 0 99 1 0 0] 98 0 1056 0 29 1074 1952 -399 1152 251 1184 -43 1216 51 234 256 98 2 1248 8 'list' 590342 ##(Smalltalk.Rectangle)  882 21 21 882 21 21 0 0 0 0 10373 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 1988726999 754 202 208 98 3 818 848 98 2 882 6239 21 882 931 781 416 818 944 98 1 8 'SUnit Results' 416 818 8 #updateMenuBar 1376 416 994 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 47 12 0 0 10 0 0 0 0 14 0 0 144 1 0 0] 98 4 1248 592 2016 2368 1056 0 27 )! !
!SUnitResultDialog class categoriesFor: #resource_Default_view!public!resources-views! !

JadeSystemBrowser guid: (GUID fromString: '{72210941-97B6-4113-8EC8-E23015415DE6}')!
JadeSystemBrowser comment: ''!
!JadeSystemBrowser categoriesForClass!Unclassified! !
!JadeSystemBrowser methodsFor!

abortTransaction

	self isOkayToChange ifTrue: [
		model abort.
		self update.
	].
!

addSystemBrowser

	self addSystemBrowserWithLayoutInfo: (self currentCard ifNotNil: [:currentCard | currentCard layoutInfo]).
!

addSystemBrowserWithLayoutInfo: each

	(JadeAutoSystemBrowserPresenter
		createIn: cardsPresenter 
		on: model)
		layoutInfo: each.
!

addWorkspace

	JadeWorkspace showOn: self model.
!

closeCard

	cardsPresenter view removeSubView: self currentCard view.!

closeRequested: anAssociation

	anAssociation value ifTrue: [
		self isOkayToChange ifTrue: [
			self saveLayoutAndContents.
		] ifFalse: [
			anAssociation value: false.
		].
	].
!

commitTransaction

	self model commit ifTrue: [
		Sound informationBeep.
		self update.
	] ifFalse: [
		MessageBox warning: 'Commit failed!!'.
	].
!

createComponents

	super createComponents.
	cardsPresenter := self add: Presenter new name: 'cardContainer'.
!

createSchematicWiring

	super createSchematicWiring.
	cardsPresenter view 	when: #'currentCardChanged' 	send: #'update' 					to: self.
	self  							when: #'closeRequested:'			send: #'closeRequested:'	to: self.
	self  view 					when: #'viewActivated'				send: #'update'					to: self.
!

currentCard

	^cardsPresenter view currentCard ifNotNil: [:cardView | cardView presenter].
!

handleInvalidSession

	| hadDialog |
	hadDialog := false.
	cardsPresenter view cards do: [:each | 
		(each presenter isKindOf: JadeSystemBrowserPresenter) ifTrue: [
			hadDialog := each presenter handleInvalidSession or: [hadDialog].
		].
	].
	hadDialog ifFalse: [
		MessageBox
			warning: 'All windows for this session will close due to unexpected logout.'
			caption: 'Invalid session!!'.
	].
	model forceLogout.
!

initialize

	super initialize.
	roundTripCount := 0.
!

isOkayToChange

	cardsPresenter view cards do: [:each | 
		each presenter isOkayToChange ifFalse: [^false].
	].
	^true.
!

logoutRequested: aValueHolder
	"Opportunity to save changes."

	self closeRequested: aValueHolder.
!

moveCardLeft

	| cardView nextSibling |
	cardView := self currentCard view.
	nextSibling := cardView previousSiblingView.
	nextSibling notNil ifTrue: [nextSibling := nextSibling previousSiblingView].
	nextSibling notNil ifTrue: [cardView zOrderAfter: nextSibling] ifFalse: [cardView zOrderTop]!

moveCardRight

	| cardView nextSibling |
	cardView := self currentCard view.
	nextSibling := cardView nextSiblingView.
	nextSibling notNil ifTrue: [cardView zOrderAfter: nextSibling].
!

onViewOpened

	super onViewOpened.
	cardsPresenter model: model.
	self restoreLayoutAndContents.
!

queryCommand: aCommandQuery

	(#(#'closeCard' #'moveCardLeft' #'moveCardRight') includes: aCommandQuery command)  ifTrue: [
		aCommandQuery isEnabled: 1 < cardsPresenter view cards size. 
		^true.
	].
	^super queryCommand: aCommandQuery.
!

renameSelection

	MessageBox notify: 'Sorry, we are not yet prepared to handle this feature!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

restoreLayoutAndContents

	self restoreLayoutAndContentsFromFile ifFalse: [
		self view position: 115 @ 70.
		self addSystemBrowserWithLayoutInfo: nil.
	].
!

restoreLayoutAndContentsFromFile

	| path file bytes data |
	path := SessionManager current imageBase , 'Jade System Browser Layout.stb'.
	(File exists: path) ifFalse: [^false].
	file := File 
		open: path
		mode: #read.
	bytes := ByteArray new: file size.
	file read: bytes; close.
	data := Object fromBinaryStoreBytes: bytes.
	(data isKindOf: OrderedCollection) ifFalse: [File delete: path. ^false].
	(data at: 1) == 1 ifFalse: [File delete: path. ^false].
	(data at: 2) top < 0 ifTrue: [File delete: path. ^false].	"Occasionally the position seems to be bad"
	self view rectangle: (data at: 2).
	(data at: 3) do: [:each | 
		self addSystemBrowserWithLayoutInfo: each.
	].
	^true.
!

saveLayoutAndContents

	| path data |
	path := SessionManager current imageBase , 'Jade System Browser Layout.stb' .
	data := OrderedCollection new
		add: 1;	"file version number"
		add: self view rectangle;
		add: (cardsPresenter view cards collect: [:each | each presenter layoutInfo]);
		yourself.
	(File 
		open: path
		mode: #truncate 
		check: false)
		write: data binaryStoreBytes;
		close.
!

selectClass: classString selector: methodString

	self currentCard
		selectClass: classString 
		selector: methodString.
!

shellName

	^'System Browse'.
!

statusBarServerRequestText: aString

	roundTripCount := roundTripCount + 1.
	self statusBarText: 'Server request #' , roundTripCount printString , '; ' , aString.
!

statusBarText: aString

	(self view viewNamed: 'statusBarField') model: (ValueHolder with: aString).
!

update

	self currentCard ifNotNil: [:currentCard | currentCard onSetFocus].
! !
!JadeSystemBrowser categoriesFor: #abortTransaction!public! !
!JadeSystemBrowser categoriesFor: #addSystemBrowser!public! !
!JadeSystemBrowser categoriesFor: #addSystemBrowserWithLayoutInfo:!public! !
!JadeSystemBrowser categoriesFor: #addWorkspace!public! !
!JadeSystemBrowser categoriesFor: #closeCard!public! !
!JadeSystemBrowser categoriesFor: #closeRequested:!public! !
!JadeSystemBrowser categoriesFor: #commitTransaction!public! !
!JadeSystemBrowser categoriesFor: #createComponents!public! !
!JadeSystemBrowser categoriesFor: #createSchematicWiring!public! !
!JadeSystemBrowser categoriesFor: #currentCard!public! !
!JadeSystemBrowser categoriesFor: #handleInvalidSession!public! !
!JadeSystemBrowser categoriesFor: #initialize!public! !
!JadeSystemBrowser categoriesFor: #isOkayToChange!public! !
!JadeSystemBrowser categoriesFor: #logoutRequested:!public! !
!JadeSystemBrowser categoriesFor: #moveCardLeft!public! !
!JadeSystemBrowser categoriesFor: #moveCardRight!public! !
!JadeSystemBrowser categoriesFor: #onViewOpened!public! !
!JadeSystemBrowser categoriesFor: #queryCommand:!public! !
!JadeSystemBrowser categoriesFor: #renameSelection!public! !
!JadeSystemBrowser categoriesFor: #restoreLayoutAndContents!public! !
!JadeSystemBrowser categoriesFor: #restoreLayoutAndContentsFromFile!public! !
!JadeSystemBrowser categoriesFor: #saveLayoutAndContents!public! !
!JadeSystemBrowser categoriesFor: #selectClass:selector:!public! !
!JadeSystemBrowser categoriesFor: #shellName!overrides!private! !
!JadeSystemBrowser categoriesFor: #statusBarServerRequestText:!public! !
!JadeSystemBrowser categoriesFor: #statusBarText:!public! !
!JadeSystemBrowser categoriesFor: #update!public! !

!JadeSystemBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1801 1201 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 8 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1409286144 131073 624 0 482 8 4278190080 0 1031 0 0 0 624 655878 ##(Smalltalk.CardLayout)  202 208 98 0 0 234 256 784 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 624 98 2 8 1140916736 1 816 590662 2 ##(Smalltalk.ListModel)  202 208 784 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  0 0 1 0 0 0 816 0 8 4294903287 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 8 #displayString 1008 7 257 0 994 0 0 1026 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 1120 7 257 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 8 #smallIcons 0 0 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 1785 999 816 1330 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 816 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 243 1 0 0] 98 0 530 193 193 0 27 1266 202 208 98 1 1330 1360 98 2 530 -7 51 530 1785 999 624 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 252 255 255 255 25 0 0 0 120 3 0 0 12 2 0 0] 98 1 816 1536 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft -7 1746 8 #fixedParentRight 9 1746 8 #fixedParentTop 51 1746 8 #fixedParentBottom -35 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1409288972 131137 1888 0 482 8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 1888 482 1984 8 4294903851 234 256 784 234 256 98 6 24993 853766 ##(Smalltalk.ToolbarButton)  24993 0 1888 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #moveCardRight 8 'Move Card Right' 1 1 0 657990 3 ##(Smalltalk.DIBSection)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'IdeaSpaceCardBar.bmp' 0 0 7 530 161 33 17 0 3 24995 2178 24995 0 1888 1 2210 8 #closeCard 8 'Close Card' 1 1 0 2288 5 24991 2178 24991 0 1888 1 2210 8 #moveCardLeft 8 'Move Card Left' 1 1 0 2288 1 98 4 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 1888 3 0 1 2432 2192 2368 234 240 98 2 2288 1 0 1 0 530 33 33 530 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 1266 202 208 98 2 1330 1360 98 2 530 1609 1 530 161 51 1888 1330 8 #updateSize 784 1888 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 36 3 0 0 0 0 0 0 116 3 0 0 25 0 0 0] 98 0 1536 0 27 1714 1792 -159 1746 8 #fixedViewLeft 161 1824 1 1746 8 #fixedViewTop 51 410 8 ##(Smalltalk.StatusBar)  98 18 0 416 98 2 8 1409288460 1 2912 0 482 8 4278190080 0 7 0 2002 0 16 2034 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 530 193 193 0 2912 0 8 4294903489 234 256 98 2 853766 ##(Smalltalk.StatusBarItem)  1 -1 2912 0 459270 ##(Smalltalk.Message)  1104 98 0 3170 1200 98 0 1232 8 'statusBarField' 98 1 3152 1115142 ##(Smalltalk.StatusBarNullItem)  513 1 2912 0 0 1266 202 208 98 1 1330 1360 98 2 530 1 1041 530 1769 45 2912 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 8 2 0 0 116 3 0 0 30 2 0 0] 98 0 1536 0 27 1714 1760 1 1792 1 1856 -43 1856 1 410 1904 98 25 0 416 98 2 8 1409288972 131137 3488 0 482 8 4278190080 0 519 0 2002 0 16 2034 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 3488 482 3568 8 4294903851 234 256 784 234 256 98 10 24981 2178 24981 0 3488 1 2210 8 #abortTransaction 8 'Abort Transaction' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 2320 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 530 1857 33 1 24983 2178 24983 0 3488 1 2210 8 #commitTransaction 8 'Commit Transaction' 1 1 0 3808 27 24985 1246982 ##(Smalltalk.ToolbarSystemButton)  24985 0 3488 1 2210 8 #addWorkspace 8 'Add Workspace' 1 1 0 1 13 24987 2178 24987 0 3488 1 2210 8 #addSystemBrowser 8 'Add System Browser' 1 1 0 3808 17 24989 2178 24989 0 3488 1 2210 8 #browseUsers 8 'Browse Users' 1 1 0 3808 75 98 7 3728 3904 2514 0 0 3488 3 0 1 3984 4048 4112 2514 0 0 3488 3 0 1 234 240 98 4 1 117 3808 1 0 1 0 530 33 33 530 45 45 0 2610 1 1 1 1266 202 208 98 2 1330 1360 98 2 530 -1 1 530 1611 51 3488 1330 2768 784 3488 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 36 3 0 0 25 0 0 0] 98 0 1536 0 27 1714 1760 -1 1792 -159 1824 1 2880 51 234 256 98 8 624 8 'cardContainer' 1888 8 'rightToolbar' 2912 8 'statusBar' 3488 8 'leftToolbar' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 1 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 2210 8 #close 8 'Exit' 1025 1 0 0 0 8 'Jade' 0 134217729 0 0 24979 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1266 202 208 98 3 1330 1360 98 2 530 6239 21 530 1801 1201 416 1330 8 #text: 98 1 8 'Jade System Browser' 416 1330 8 #updateMenuBar 784 416 1474 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 47 12 0 0 10 0 0 0 179 15 0 0 98 2 0 0] 98 4 3488 1888 624 2912 1536 0 27 )! !
!JadeSystemBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

