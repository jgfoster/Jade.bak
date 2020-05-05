"Filed out from Dolphin Smalltalk 7"!

JadeBrowserPresenter subclass: #JadeSystemBrowserPresenter
	instanceVariableNames: 'ancestorListPresenter breakPoints categoryListPresenter categoryVariableTabs classCategoryPresenter classCommentPresenter classDefinition classDefinitionPresenter classHierarchyPresenter classHierarchyTabs classListPresenter dictionaryListPresenter environment eventCount globalsPresenter globalsLabel historyList historyIndex ignoreNextSetFocusEvent instanceClassTabs inUpdate keystrokeTime methodCategory methodFilterTabs methodListPresenter methodSource methodSourcePresenter originalSourceLabel originalSourcePresenter overrideListPresenter packageDictionaryTabs packageInfoTab packageLabel packageListPresenter pragmaListPresenter readStream repositoryListPresenter selectedClassChanged selectedClassesAreTestCases selectedClassName selectedClassOop stepPoints sunitPresenter superclassListPresenter textAreaTabs unimplementedSelectors updateCount updateProcess variableListPresenter previousSelectedMethod'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSystemBrowserPresenter guid: (GUID fromString: '{aa74f365-5e98-46b2-af4c-19a7bf1e6e97}')!
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
	GciSession log: 'JadeSystemBrowserPresenter>>addMethodCategory'.
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
			nextPutAll: 'categoryList'; tab;
			lf;	"method filter selections"
			lf; "overrideList selection"
			yourself.
		^self.
	].
	string := superclassListPresenter selectionIfNone: [''].
	aStream 
		nextPutAll: instanceClassTabs currentCard name; lf;		"instanceTab or classTab"
		nextPutAll: string; lf;								"selected superclass"
		nextPutAll: methodFilterTabs currentCard name; tab;		"categoryList or variableList or pragmaList"
		lf.
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
	GciSession log: 'JadeSystemBrowserPresenter>>addMissingAccessors'.
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
	GciSession log: 'JadeSystemBrowserPresenter>>addPackage'.
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
	GciSession log: 'JadeSystemBrowserPresenter>>addRepository'.
	self updateCommand: stream contents , self requestString.
!

addSubclass

	| i j superclass string |
	self isOkayToChange ifFalse: [^self].
	i := classDefinition indexOfSubCollection: '''' startingAt: 1.
	j := classDefinition indexOfSubCollection: '''' startingAt: i + 1.
	superclass := classDefinition copyFrom: i + 1 to: j - 1.
	string := '(
' asCharacterCollection , superclass , ' subclass: ''MyNewSubclass''' , (classDefinition copyFrom: j + 1 to: classDefinition size).
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
		value: string asUtf8String;
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
	GciSession log: 'JadeSystemBrowserPresenter>>breakAt: ' , anInteger printString , ' operation: ' , aString printString.
	self updateCommand: stream contents , self requestString.
!

browseClassReferences

	| string |
	string := 'browseClassReferences' , Character tab asString , self behaviorIdentifier , Character lf asString.
	GciSession log: 'JadeSystemBrowserPresenter>>browseClassReferences'.
	(string := self updateCommand: string) = 'browseClassReferences' ifFalse: [self error: 'Unrecognized response'].
	self browseMethodsAndSelect: selectedClassName.!

browseGlobalReferences

	| string |
	string := 'browseGlobalReferences' , Character tab asString , (globalsPresenter selection at: 5) , Character lf asString.
	GciSession log: 'JadeSystemBrowserPresenter>>browseGlobalReferences'.
	(string := self updateCommand: string) = 'browseGlobalReferences' ifFalse: [self error: 'unrecognized response'].
	self browseMethodsAndSelect: (globalsPresenter selection at: 1).
!

browseImplementors

	self browseImplementorsOf: self selectedMethodName.
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
		print: environment;
		nextPutAll: ' findSelectors';
		yourself.
	list do: [:each | stream tab; nextPutAll: each].

	string := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: (self gciSession encode: stream contents).
	string := self gciSession decode: string.
	list := string subStrings: Character lf.
	list := list copyFrom: 2 to: list size.
	(selector := ChoicePrompter choices: list) isNil ifTrue: [^self].
	self browseImplementorsOf: selector.
!

browseImplementorsOf: aString

	| string |
	string := 'browseImplementors' , Character tab asString , aString , Character lf asString.
	GciSession log: 'JadeSystemBrowserPresenter>>browseImplementorsOf: ' , aString printString.
	(self updateCommand: string) = 'browseImplementors' ifFalse: [self error: 'unexpected response'].
	self browseMethodsAndSelect: aString type: #definitions.
!

browseMethodHistory

	| string |
	string := 'browseMethodHistory' , Character tab asString , self methodsIdentifier , Character lf asString.
	GciSession log: 'JadeSystemBrowserPresenter>>browseMethodHistory'.
	(self updateCommand: string) = 'browseMethodHistory' ifFalse: [self error: 'unexpected response'].
	readStream atEnd ifTrue: [
		MessageBox notify: 'No history available!!'.
		^self.
	].
	(JadeMethodHistoryBrowser showOnSession: gciSession)
		setContents: readStream;
		yourself.
!

browseMethodPragmas

	| commandString keyword keywords responseString |
	GciSession log: 'JadeSystemBrowserPresenter>>browseMethodPragmas'.
	responseString:= self updateCommand: 'listMethodPragmas'.
	keywords := responseString subStrings: Character tab.
	(keyword := ChoicePrompter choices: keywords) ifNil: [^self].
	commandString := 'browseMethodsWithPragma' , Character tab asString , keyword , Character lf asString.
	responseString := self updateCommand: commandString.
	responseString = 'browseMethodsWithPragma' ifFalse: [self error: 'unexpected response'].
	self browseMethodsAndSelect: keyword.
!

browseMethodsAndSelect: aString

	(JadeMethodListBrowser showOnSession: self gciSession) primaryPresenter
		browseMethodsFromString: readStream upToEnd methodName: aString;
		selecting: aString.
!

browseMethodsAndSelect: aString type: browseType

	(JadeMethodListBrowser showOnSession: self gciSession) primaryPresenter
		browseMethodsFromString: readStream upToEnd methodName: aString type: browseType;
		selecting: aString.
!

browseMethodsContaining

	| searchString commandString |
	(searchString := Prompter prompt: 'Enter string:') isNil ifTrue: [^self].
	commandString := 'browseMethodsContaining' , Character tab asString , searchString , Character lf asString.
	GciSession log: 'JadeSystemBrowserPresenter>>browseMethodsContaining - ' , searchString printString.
	(self updateCommand: commandString) = 'browseMethodsContaining' ifFalse: [self error: 'unexpected response'].
	self browseMethodsAndSelect: searchString.
!

browseSelectedClass

	| range string list assoc |
	range := methodSourcePresenter view selectionRange.
	string := (methodSourcePresenter value copyFrom: range start to: range stop) asCharacterCollection.
	list := self findClassList.
	assoc := list 
		detect: [:each | each key asCharacterCollection = string]
		ifNone: [^MessageBox warning: 'Class ' , string printString , ' not found!!' caption: 'Jade'].
	parentPresenter parentPresenter addSystemBrowserForClass: assoc value.
!

browseSenders

	self browseSendersOf: self selectedMethodName.
!

browseSendersOf

	| string |
	(string := self promptForSelector) isNil ifTrue: [^self].
	self browseSendersOf: string.
!

browseSendersOf: aString

	| string |
	string := 'browseSenders' , Character tab asString , aString , Character lf asString.
	GciSession log: 'JadeSystemBrowserPresenter>>browseSendersOf: ' , aString printString.
	(self updateCommand: string) = 'browseSenders' ifFalse: [self error: 'unexpected response'].
	self browseMethodsAndSelect: aString type: #references.
!

buildHistoryMenuNamed: aString from: startIndex to: endIndex by: increment

	| currentClassName popup subMenu |
	popup := Menu new.
	currentClassName := nil.	"avoid warning and make explicit that we expect comparison to fail the first time so we get a new submenu"
	startIndex to: endIndex by: increment do: [:i | 
		| className historyEntry methodName |
		historyEntry := historyList at: i.
		className := historyEntry at: 1.
		methodName := historyEntry at: 2.
		className ~= currentClassName ifTrue: [
			currentClassName := className.
			subMenu := popup addSubmenu: currentClassName.
		].
		subMenu
			addCommand: (Message selector: #'jumpToHistoryIndex:' argument: i)
			description: methodName.
		subMenu setDefault: 1.
	].
	popup setDefault: 1.
	^popup!

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

categoryListPresenter

	^categoryListPresenter!

classCategoriesMenuStrings

	false ifTrue: [
		self saveTonelPackage.
	].
	^#(
		'Class Categories'
		'&Save as Tonel Package//saveTonelPackage'
	).
!

classCategoryPresenter

	^classCategoryPresenter!

classDefChanged

	inUpdate ifTrue: [^self].
	classDefinitionPresenter value asCharacterCollection = classDefinition ifTrue: [
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
		'Set Compiler &Environment ...//setEnvironment'
		'Run &Tests//runClassTests'
	).
!

classHierarchyPresenter

	^classHierarchyPresenter!

classListPresenter

	^classListPresenter!

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
	GciSession log: 'JadeSystemBrowserPresenter>>compareAncestor'.
	(self updateCommand: stream contents) = 'comparePackages' ifFalse: [self error: 'Unexpected response!!'].
	patch := MCPatch
		fromString: readStream upToEnd
		session: self gciSession.
	patch operations isEmpty ifTrue: [
		MessageBox notify: 'No changes!!'.
		^self.
	].
	MCPatchDiffBrowser showOn: patch.
!

contextObject

	| oopType |
	selectedClassOop == 0 ifTrue: [^nil].
	oopType := self gciSession oopTypeWithOop: selectedClassOop.
	self isClassSideSelected ifTrue: [
		oopType := self gciSession send: #'class' to: oopType.
	].
	^oopType
!

createComponents

	ancestorListPresenter 	:= self add: ListPresenter		new 		name: 'ancestorList'.
	categoryListPresenter		:= self add: ListPresenter		new 		name: 'categoryList'.
	classCategoryPresenter	:= self add: TreePresenter		new 		name: 'classCategoryList'.
	classCommentPresenter 	:= self add: TextPresenter 		new 		name: 'classDocumentation'.
	classDefinitionPresenter	:= self add: JadeTextPresenter 	new 		name: 'classDefinition'.
	classHierarchyPresenter	:= self add: TreePresenter		new 		name: 'classHierarchy'.
	classListPresenter		:= self add: ListPresenter		new 		name: 'classList'.
	dictionaryListPresenter 	:= self add: ListPresenter 		new 		name: 'dictionaryList'.
	globalsPresenter		:= self add: ListPresenter		new 		name: 'globals'.
	methodListPresenter		:= self add: ListPresenter		new 		name: 'methodList'.
	methodSourcePresenter	:= self add: self newMethodPresenter 	name: 'methodSource'.
	originalSourcePresenter	:= self add: self newMethodPresenter 	name: 'originalSource'.
	overrideListPresenter		:= self add: ListPresenter		new 		name: 'overrideList'.
	packageListPresenter 	:= self add: ListPresenter		new 		name: 'packageList'.
	pragmaListPresenter		:= self add: ListPresenter		new		name: 'pragmaList'.
	repositoryListPresenter	:= self add: ListPresenter		new 		name: 'repositoryList'.
	sunitPresenter 			:= self add: JadeSUnitInClassPresenter new 	name: 'sunit'.
	superclassListPresenter	:= self add: ListPresenter		new 		name: 'superclassList'.
	variableListPresenter		:= self add: ListPresenter		new 		name: 'variableList'.
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
		createSchematicWiringForPragmaList;
		createSchematicWiringForMethodSource;
		createSchematicWiringForClassDefinition;
		createSchematicWiringForMethodCategoryList;
		createSchematicWiringForVariableList;
		createSchematicWiringForClassCategoryList;
		createSchematicWiringForSuperClassList;
		createSchematicWiringForOverrideList;
		createSchematicWiringForInstanceClassTabs;
		yourself.
	textAreaTabs			when: #'currentCardChanged'	send: #'textTabChanged'		to: self.
	classHierarchyTabs		when: #'currentCardChanging:'	send: #'aboutToChange:'		to: self;	when: #'currentCardChanged'	send: #'updateAndSelect_:'	to: self	with: classDefinitionPresenter -> 'classHierarchyTabs currentChardChanged'.
	methodFilterTabs		when: #'currentCardChanging:'	send: #'aboutToChange:'		to: self;	when: #'currentCardChanged'	send: #'updateAndSelect_:'	to: self	with: classDefinitionPresenter -> 'methodFilterTabs currentCardChanged'.
	globalsPresenter		when: #'actionPerformed'		send: #'inspectGlobal'		to: self.
!

createSchematicWiringForClassCategoryList

	classCategoryPresenter		
		when: #'dragOver:'			send: #'onDragOverClassCategory:'	to: self;
		when: #'drop:'				send: #'onDropOnClassCategory:'	to: self;
		when: #'keyTyped:'			send: #'delayUpdate'				to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'			to: self;		
		when: #'selectionChanged'		send: #'updateAndSelect_:'			to: self	with: classDefinitionPresenter -> 'classCategoryPresenter selectionChanged';
		yourself.
!

createSchematicWiringForClassDefinition

	classDefinitionPresenter
		when: #'valueChanged'	send: #'classDefChanged'	to: self;
		yourself.
!

createSchematicWiringForClassHierarchy

	classHierarchyPresenter	
		when: #'drag:'				send: #'onDragClassHierarchy:'		to: self;
		when: #'dragCut:'			send: #'onDragCutClassHierarchy:'	to: self;
		when: #'dragOver:'			send: #'onDragOverClassHierarchy:'	to: self;
		when: #'drop:'				send: #'onDropOnClassHierarchy:'	to: self;
		when: #'keyTyped:'			send: #'delayUpdate'				to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'			to: self;
		when: #'selectionChanged'		send: #'selectedClassChanged:'		to: self	with: true;
		when: #'selectionChanged'		send: #'updateAndSelect_:'			to: self	with: classDefinitionPresenter -> 'classHierarchyPresenter selectionChanged';
		yourself.
!

createSchematicWiringForClassList

	classListPresenter
		when: #'drag:'					send: #'onDragClassList:'				to: self;
		when: #'dragCut:'				send: #'onDragCutClassList:'			to: self;
		when: #'dragOver:'				send: #'onDragOverClassList:'			to: self;
		when: #'drop:'					send: #'onDropOnClassList:'			to: self;
		when: #'keyTyped:'				send: #'delayUpdate'					to: self;
		when: #'selectionChanging:'		send: #'aboutToChange:'				to: self;
		when: #'selectionChanged'			send: #'selectedClassChanged:'			to: self	with: true;
		when: #'selectionChanged'			send: #'updateAndSelect_:'				to: self	with: classDefinitionPresenter -> 'classListPresenter selectionChanged';

		when: #labelOf:changedTo: 		send: #labelOfClass:changedTo:			to: self;
		when: #aboutToEditLabel:accept: 	send: #aboutToEditClassLabel:accept:		to: self;
		when: #labelOf:editedTo:accept: 	send: #labelOfClass:editedTo:accept:		to: self;

		yourself.
!

createSchematicWiringForDictionaryList

	dictionaryListPresenter
		when: #'dragOver:'			send: #'onDragOverDictionary:'		to: self;
		when: #'drop:'				send: #'onDropOnDictionary:'		to: self;
		when: #'keyTyped:'			send: #'delayUpdate'				to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'			to: self;
		when: #'selectionChanged'		send: #'updateAndSelect_:'			to: self	with: globalsPresenter -> 'dictionaryListPresenter selectionChanged';
		yourself.
!

createSchematicWiringForInstanceClassTabs

	instanceClassTabs
		when: #'currentCardChanging:'	send: #'aboutToChange:'		to: self;
		when: #'currentCardChanged'	send: #'selectedClassChanged:'	to: self	with: true;
		when: #'currentCardChanged'	send: #'updateAndSelect_:'		to: self	with: classDefinitionPresenter -> 'instanceClassTabs currentCardChanged';
		yourself.
!

createSchematicWiringForMethodCategoryList

	categoryListPresenter
		when: #'dragOver:'			send: #'onDragOverMethodCategory:'	to: self;
		when: #'drop:'				send: #'onDropOnMethodCategory:'		to: self;
		when: #'keyTyped:'			send: #'delayUpdate'					to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'				to: self;
		when: #'selectionChanged'		send: #'updateAndSelect_:'				to: self	with: methodSourcePresenter -> 'categoryListPresenter selectionChanged';
		when: #'selectionChanged'		send: #'updateTabLabel'				to: self;
		yourself.
!

createSchematicWiringForMethodList

	methodListPresenter			
		when: #'drag:'				send: #'onDragMethod:'			to: self;
		when: #'dragCut:'			send: #'onDragCutMethod:'		to: self;
		when: #'keyTyped:'			send: #'delayUpdate'				to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'			to: self;
		when: #'selectionChanged'		send: #'selectedMethodChanged'	to: self;
		yourself.
!

createSchematicWiringForMethodSource

	methodSourcePresenter 	
		when: #'hoverStart:'				send: #'methodHoverStart:'	to: self;
		when: #'hoverEnd:'				send: #'methodHoverEnd:'		to: self;
		when: #'aboutToDisplayMenu:'		send: #'methodMenu:'		to: self;
		when: #'leftButtonDoubleClicked:'	send: #'methodDoubleClicked:'	to: self;
		when: #'valueChanged'			send: #'methodValueChanged'	to: self;
		when: #'focusLost'				send: #'cancelCallTip'			to: methodSourcePresenter view;
		yourself.
!

createSchematicWiringForOverrideList

	overrideListPresenter		
		when: #'keyTyped:'			send: #'delayUpdate'		to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'	to: self;		
		when: #'selectionChanged'		send: #'updateAndSelect_:'	to: self	with: methodSourcePresenter -> 'overrideListPresenter selectionChanged';
		yourself.
!

createSchematicWiringForPackageDictionaryTabs

	packageDictionaryTabs
		when: #'currentCardChanging:'	send: #'aboutToChange:'	to: self;
		when: #'currentCardChanged'	send: #'pkgDictChanged'	to: self;
		yourself.!

createSchematicWiringForPackageList

	packageListPresenter
		when: #'drag:'				send: #'onDragPackageList:'		to: self;
		when: #'dragCut:'			send: #'onDragCutPackageList:'		to: self;
		when: #'dragOver:'			send: #'onDragOverPackageList:'	to: self;
		when: #'drop:'				send: #'onDropOnPackageList:'		to: self;
		when: #'keyTyped:'			send: #'delayUpdate'				to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'			to: self;
		when: #'selectionChanged' 	send: #'ensureVisible'			to: packageInfoTab;
		when: #'selectionChanged' 	send: #'list:' 					to: globalsPresenter 	with: #();
		when: #'selectionChanged'		send: #'updateAndSelect_:'			to: self			with: packageInfoTab -> 'packageListPresenter selectionChanged';
		yourself.
!

createSchematicWiringForPragmaList

	pragmaListPresenter
		when: #'keyTyped:'			send: #'delayUpdate'		to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'	to: self;
		when: #'selectionChanged'		send: #'updateAndSelect_:'	to: self	with: methodSourcePresenter -> 'pragmaListPresenter selectionChanged';
		when: #'selectionChanged'		send: #'updateTabLabel'	to: self;
		yourself.
!

createSchematicWiringForSuperClassList

	superclassListPresenter		
		when: #'keyTyped:'			send: #'delayUpdate'		to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'	to: self;		
		when: #'selectionChanged'		send: #'updateAndSelect_:'	to: self	with: methodSourcePresenter -> 'superclassListPresenter selectionChanged';
		yourself.
!

createSchematicWiringForVariableList

	variableListPresenter
		when: #'keyTyped:'			send: #'delayUpdate'		to: self;
		when: #'selectionChanging:'	send: #'aboutToChange:'	to: self;
		when: #'selectionChanged'		send: #'updateAndSelect_:'	to: self	with: methodSourcePresenter -> 'variableListPresenter selectionChanged';
		when: #'selectionChanged'		send: #'updateTabLabel'	to: self;
		yourself.
!

currentMethodSource

	| source fromStream writeStream |
	fromStream := ReadStream on: methodSourcePresenter value asCharacterCollection.
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
	source := source , Character lf asString.
	^source.
!

currentSelectionOrLine

	View focus hasSelection ifFalse: [View focus selectCurrentLine].
	^View focus selection asCharacterCollection.
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

dictionaryListPresenter

	^dictionaryListPresenter!

dictsMenuStrings

	false ifTrue: [
		self browseDictionaryReferences; fileOutDictionary; findClass; insertDictionary; removeDictionary; setHomeDictionary; fileInCode.
	].
	^#(
		'&Dictionaries'
		'&Browse References//browseDictionaryReferences'
		'&File Out Dictionary//fileOutDictionary'
		'File In Code//fileInCode'
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
	self methodValueChanged.
!

editFind

	textAreaTabs currentCard setFocus; editFind.!

editFindNext

	textAreaTabs currentCard setFocus; editFind.!

editMenuStrings

	false ifTrue: [
		self editSave; editUndo; editRedo; editCut; editCopy; editPaste; editDelete; editSelectAll; editFind; editFindNext; editReplace; 
			jadeDisplay; jadeExecute; jadeInspect; browseSelectedClass.
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
		'Debug/Ctrl+B/jadeDebug'
		'Display/Ctrl+D/jadeDisplay'
		'Execute/Ctrl+E/jadeExecute'
		'Inspect/Ctrl+Q/jadeInspect'
		'Browse Class//browseSelectedClass'
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
	GciSession log: 'JadeSystemBrowserPresenter>>editSaveClass'.
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
	GciSession log: 'JadeSystemBrowserPresenter>>editSaveClassA'.
	(answer := self updateCommand: stream contents , self requestString) isNil ifTrue: [^self].
	[
		answer = 'compileError'.
	] whileTrue: [
		(JadeWorkspace showOnSession: self gciSession)
			caption: 'Jade Workspace - Compile errors found when compiling class';
			showText: readStream nextParagraph.
		answer := readStream nextLine.
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
	GciSession log: 'JadeSystemBrowserPresenter>>editSaveClassComment'.
	self 
		updateCommand: stream contents , self requestString
		onSuccessDo: [classCommentPresenter view isModified: false].
!

editSaveMethod
	| stream caretPosition firstVisibleLine |

	caretPosition := methodSourcePresenter view caretPosition.
	firstVisibleLine := methodSourcePresenter view lineScroll.
	(self isCategoriesTabSelected and: [categoryListPresenter hasSelection]) ifTrue: [
		methodCategory := categoryListPresenter selection trimBlanks.
	].

	stream := (WriteStream on: CharacterCollection new)
		nextPutAll: 'method'; tab;
		nextPutAll: self selectedClassNameWithoutVersion; tab;
		nextPutAll: selectedClassOop printString; tab;
		nextPutAll: instanceClassTabs currentCard name; tab;
		nextPutAll: methodCategory; tab; 
		lf;
		nextPutAll: self currentMethodSource;
		nextPut: $%; lf;
		yourself.
	GciSession log: 'JadeSystemBrowserPresenter>>editSaveMethod'.
	[
		self 
			updateCommand: stream contents , self requestString
			onSuccessDo: [methodSourcePresenter view isModified: false; ensureVisible].
	] on: GsCompileError do: [:ex | 
		self reportCompileError: ex list.
	].
	methodSourcePresenter view lineScroll: firstVisibleLine.
	methodSourcePresenter view goto: caretPosition.
!

editSelectAll

	View focus selectAll.
!

editUndo

	View focus undo.
!

executeSelectionOrLine

	[
		^true -> (self gciSession 
			executeString: self currentSelectionOrLine
			fromContext: self contextObject 
			environment: environment).
	] on: GsCompileError do: [:ex | 
		^false -> ex list.
	].
	^false -> #(nil).
!

fileInCode

	| path |
	path := FileOpenDialog new
		caption: 'File In Code';
		fileTypes: self fileTypes;
		defaultExtension: self defaultFileExtension;
		showModal.
	path isNil ifTrue: [^self].
	ignoreNextSetFocusEvent := true.
	JadeFileIn new
		fileInPath: path 
		session: gciSession.
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

	| header file newSource index isUTF string |
	string := environment printString , ' fileOutClass' , Character tab asString , self behaviorIdentifier.
	self gciSession encode: string.
	newSource := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: string.
	newSource := self gciSession decode: newSource.
	index := newSource indexOf: Character lf.
	newSource := newSource copyFrom: index + 1 to: newSource size.
	header := self stuffToKeepFromPath: aString andAddTo: newSource.
	newSource := header , newSource.
	isUTF := newSource anySatisfy: [:each | 127 < each codePoint].
	file := FileStream write: aString mode: #'truncate' check: false type: (isUTF ifTrue: [#'utf8'] ifFalse: [#'ansi']).
	[
		file nextPutAll: newSource.
	] ensure: [
		file close.
	].
!

fileOutDictionary

	| dictionaryName file index isUTF path string |
	dictionaryName :=  dictionaryListPresenter selection key.
	path := FileSaveDialog new
		caption: 'File Out ' , dictionaryName;
		fileTypes: self fileTypes;
		defaultExtension: self defaultFileExtension;
		value: dictionaryName , '.gs';
		overwritePrompt;
		showModal.
	path isNil ifTrue: [^self].
	string := environment printString , ' fileOutDictionary' , Character tab asString , dictionaryName.
	string := self gciSession encode: string.
	string := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: string.
	string := self gciSession decode: string.
	index := string indexOf: Character lf.	string := string copyFrom: index + 1 to: string size.
	isUTF := string anySatisfy: [:each | 127 < each codePoint].
	file := FileStream write: path mode: #'truncate' check: false type: (isUTF ifTrue: [#'utf8'] ifFalse: [#'ansi']).
	[
		file nextPutAll: string.
	] ensure: [
		file close.
	].
!

fileOutMethod

	| name path myReadStream writeStream |
	name := self selectedClassNameWithoutVersion.
	self isClassSideSelected ifTrue: [name := name , '-class'].
	name := name , '-' , self selectedMethodName.
	name := name copyReplaceAll: ':' with: '_'.
	myReadStream := ReadStream on: name.
	writeStream := WriteStream on: String new.
	[myReadStream atEnd not] whileTrue: [
		| each |
		each := myReadStream next.
		(each isAlphaNumeric or: [each == $_ or: [each == $-]]) ifTrue: [
			writeStream nextPut: each.
		] ifFalse: [
			writeStream
				nextPut: $-;
				print: each codePoint;
				nextPut: $-;
				yourself.
		].
	].
	name := writeStream contents.
	path := FileSaveDialog new
		caption: 'File Out Method';
		fileTypes: self fileTypes;
		defaultExtension: self defaultFileExtension;
		value: name;
		overwritePrompt;
		showModal.
	path ifNotNil: [:value | self fileOutMethodOnPath: value].
!

fileOutMethodOnPath: aString
	| header file newSource index string |
	string := environment printString , ' fileOutMethod' , Character tab asString
				, self behaviorIdentifier , Character tab asString
				, self selectedMethodName.
	string := self gciSession encode: string.
	newSource := self gciSession serverPerform: #systemBrowser: with: string.
	newSource := self gciSession decode: newSource.
	index := newSource indexOf: Character lf.
	newSource := newSource copyFrom: index + 1 to: newSource size.
	header := self stuffToKeepFromPath: aString andAddTo: newSource.
	file := FileStream
				write: aString
				mode: #truncate
				check: false
				type: JadeFileOutEncodingPreference default jadeFileOutEncoding.
	
	[file
		nextPutAll: header;
		nextPutAll: newSource] ensure: [file close]!

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
	| find list |
	list := self findClassList.
	ignoreNextSetFocusEvent := true.
	find := JadeFindClassDialog showModal: 'ThreeColumnView' on: list.
	find ifNil: [^self].
	self 
		updateAfterFindClass: find value
		isMeta: nil 
		selector: ''.
!

findClassList
	"
	Array with: className with: dictionaryName with: catetory with: packageName.
"

	| string list |
	string := self gciSession serverPerform: #systemBrowser:
				with: environment printString , ' findClass'.
	list := (string subStringsTrimmingTrailingEmptyElement: Character lf)
				collect: [:each | each subStrings: Character tab].
	list := list copyFrom: 2 to: list size.
	list := list collect: [:each | each size < 3 ifTrue: [each , #('' '' '')] ifFalse: [each]].
	list := list collect: [:each | (each at: 1) -> each].
	^list!

gciSession: aGciSession

	super gciSession: aGciSession.
	sunitPresenter gciSession: aGciSession.
!

getSelectedMethod
	| dataArray |

	dataArray := self methodListPresenter selectionOrNil.
	dataArray ifNil: [^nil].

	^dataArray first!

getViews

	packageDictionaryTabs 	:= self view viewNamed: 'packageDictionaryTabs'.
	classHierarchyTabs 		:= self view viewNamed: 'classHierarchyTabs'.
	instanceClassTabs		:= self view viewNamed: 'instanceClassTabs'.
	methodFilterTabs 		:= self view viewNamed: 'methodFilterTabs'.
	textAreaTabs			:= self view viewNamed: 'textAreaTabs'.
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
			methodSourcePresenter value copyToClipboard.
		].
		hadDialog := true.
	].
	classDefinitionPresenter view isModified ifTrue: [
		self ensureVisible.
		classDefinitionPresenter ensureVisible.
		classDefinitionPresenter view isModified: false.
		(MessageBox confirm: 'Copy unsaved class definition to clipboard?' caption: 'Invalid Session!!') ifTrue: [
			classDefinitionPresenter value copyToClipboard.
		].
		hadDialog := true.
	].
	^hadDialog.
!

hasCommandHistoryBack

	^1 < historyIndex!

hasCommandHistoryForward

	^historyIndex < historyList size!

hasHistoryEntry: historyArray
	"The receiver answer whether or not history entry <historyArray> is present.
	index 1 - class name.
	index 2 - method name"

	^historyList anySatisfy: [:eachArray | ((eachArray at: 1) = (historyArray at: 1)) and: [(eachArray at: 2) = (historyArray at: 2)] ]!

historyBack

	self jumpToHistoryIndex: (self moveHistoryIndexTo: historyIndex - 1).
	!

historyBackMenu

	^self 
		buildHistoryMenuNamed: 'Backward'
		from: historyIndex - 1
		to: 1 
		by: -1!

historyForward

	self jumpToHistoryIndex:  (self moveHistoryIndexTo: historyIndex + 1).
	!

historyForwardMenu

	^self 
		buildHistoryMenuNamed: 'Forward'
		from: historyIndex + 1
		to: historyList size 
		by: 1
!

horizontalSplitter

	^view 
		viewNamed: 'splitter' 
		ifNone: [self error: 'splitter is missing!!?'].
!

initialize

	super initialize.
	environment := 0.
	historyIndex := 0.
	historyList := OrderedCollection new.
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
	GciSession log: 'JadeSystemBrowserPresenter>>insertDictionary'.
	self updateCommand: stream contents.
!

inspectGlobal

	| oopType |
	oopType := self gciSession oopTypeWithOop: (globalsPresenter selection at: 4) asNumber.
	(Smalltalk at: #'JadeInspector' ifAbsent: [^self]) showOn: oopType session: self gciSession.
!

isCategoriesTabSelected

	^methodFilterTabs currentCard name = 'categoryList'.
!

isClassListTabSelected

	^classHierarchyTabs currentCard name = 'classList'.
!

isClassSelectedInEditor

	| range string |
	(range := methodSourcePresenter view selectionRange) isEmpty ifTrue: [^false].
	string := (methodSourcePresenter value copyFrom: range start to: range stop) asCharacterCollection.
	^(string allSatisfy: [:each | each isAlphaNumeric]) and: [string first isLetter and: [string first isUppercase]]
!

isClassSideSelected

	^instanceClassTabs currentCard name = 'classTab'.
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
		gciSession isValidSession
			ifTrue: [(MessageBox confirm: 'Stay on unsaved method?' caption: 'Method has unsaved changes!!') ifTrue: [^false]]
			ifFalse: [(MessageBox confirm: 'Copy changes to Clipboard?' caption: 'Method has unsaved changes!!') ifTrue: [methodSourcePresenter selectAll; copySelection]].
		methodSourcePresenter view isModified: false.
	].
	classDefinitionPresenter view isModified ifTrue: [
		classDefinitionPresenter ensureVisible.
		gciSession isValidSession
			ifTrue: [(MessageBox confirm: 'Stay on unsaved class definition?' caption: 'Class has unsaved changes!!') ifTrue: [^false]]
			ifFalse: [(MessageBox confirm: 'Copy changes to Clipboard?' caption: 'Class has unsaved changes!!') ifTrue: [classDefinitionPresenter selectAll; copySelection]].
		classDefinitionPresenter view isModified: false.
	].
	^true.
!

isPackageListTabSelected

	^packageDictionaryTabs currentCard name = 'packageList'.
!

isPragmasTabSelected

	^methodFilterTabs currentCard name = 'pragmaList'.
!

isTreeModel: treeModelA equivalentTo: treeModelB

	| listA listB |
	listA := (treeModelA asBag collect: [:each | each key printString]) asSortedCollection asArray.
	listB := (treeModelB asBag collect: [:each | each key printString]) asSortedCollection asArray.
	^listA = listB.
!

isVariablesTabSelected

	^methodFilterTabs currentCard name = 'variableList'.
!

jadeDebug

	[
		gciSession 
			debugString: (gciSession encode: self currentSelectionOrLine) 
			fromContext: nil 
			environment: environment.
	] on: GsCompileError do: [:ex | 
		self jadeExecute.
	].
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
	result key ifTrue: [	"success"
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
		^value.
	] ifFalse: [		"compile error"
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
		string := string copyFrom: 1 to: (offset - 1 max: string size).
		count := (string select: [:each | each = Character lf]) size.
		offset := offset + count.
		textView
			caretPosition: selectionRange start + offset - 1;
			replaceSelection: result;
			selectionStart: textView caretPosition - result size length: result size.
		^self
	].
!

jadeInspect

	| result |
	result := self jadeExecuteAndDisplay: false.	result == self ifTrue: [^self].	"flag for compile error"
	JadeInspector showOn: result session: gciSession.
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

jumpToHistoryIndex: anInteger

	| commandString historyEntry |

	historyIndex := anInteger.
	historyEntry := historyList at: anInteger.
	commandString := historyEntry at: 3.
	GciSession log: 'JadeSystemBrowserPresenter>>jumpToHistoryIndex: ' , anInteger printString.
	self 
		updateCommand: commandString;
		updateMenuBar;
		yourself.
	methodSourcePresenter ensureVisible.!

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
	GciSession log: 'JadeSystemBrowserPresenter>>labelOfClass: ' , oldName printString , ' changedTo: ' , newName printString.
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
	GciSession log: 'JadeSystemBrowserPresenter>>labelOfClass: ' , oldName printString , ' editedTo: ' , newName printString , ' accept: ' , aValueHolder printString.
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
		add: environment;
		asArray.
!

layoutInfo: anArray

	environment := (anArray size < 3) ifTrue: [0] ifFalse: [anArray at: 3].
	view ensureVisible.
	GciSession log: 'JadeSystemBrowserPresenter>>layoutInfo:'.
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
	GciSession log: 'JadeSystemBrowserPresenter>>loadLatestVersion'.
	self updateCommand: stream contents.
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

	| name |
	(name := methodFilterTabs currentCard name) ifNil: [^self].
	name = 'categoryList' ifTrue: [^categoryListPresenter].
	name = 'variableList' ifTrue: [^variableListPresenter].
	name = 'pragmaList' ifTrue: [^pragmaListPresenter].
	self error: 'Unrecognized method filter name'.
!

methodHoverEnd: anObject

	methodSourcePresenter view cancelCallTip.!

methodHoverStart: aPoint

	| charIndex indicator |
	charIndex := methodSourcePresenter view charNearestPosition: aPoint.
	indicator := methodSourcePresenter view indicators
		detect: [:each | each range includes: charIndex]
		ifNone: [^self].
	indicator tag ifNil: [^self].	"https://github.com/jgfoster/Jade/issues/188"
	methodSourcePresenter view 
		showCallTip: indicator tag
		at: charIndex.
!

methodListPresenter

	^methodListPresenter!

methodMenu: aMenu

	| anEditMenuStrings |
	(anEditMenuStrings := self editMenuStrings) isEmpty
	ifFalse:
		[aMenu text = (anEditMenuStrings at: 1)
		ifTrue: 
			[| properSize |
			properSize := self editMenuStrings size - 1.
			[
				properSize < aMenu items size.
			] whileTrue: [
				aMenu removeItemAt: aMenu items size.
			].
			self methodMenuBreak: aMenu
			]
		].
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
	(message := (stepPoints at: stepPoint) value) isEmpty ifTrue: [
		command := MessageSend
			receiver: self 
			selector: #'optimizedSelector'.
		aMenu
			addCommand: command
			description: 'Optimized Selector'.
	] ifFalse: [
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
	].!

methodsIdentifier

	| stream |
	stream := (WriteStream on: String new)
		nextPutAll: self behaviorIdentifier;
		yourself.
	methodListPresenter selections do: [:each | stream tab; nextPutAll: each first].
	^stream contents.
!

methodsMenuStrings

	false ifTrue: [	"This helps us find senders"
		self browseImplementors; browseImplementorsOf; browseSenders; browseSendersOf; browseMethodsContaining; browseMethodPragmas; removeMethods; runMethodTests; setEnvironment0; setEnvironment1; setEnvironment2; fileOutMethod.
	].
	^#(
		'&Methods'
		'Browse &Implementors//browseImplementors'
		'Browse Implementors of ...//browseImplementorsOf'
		'Browse &Senders//browseSenders'
		'Browse Senders of ...//browseSendersOf'
		'Browse Methods &Containing ...//browseMethodsContaining'
		'Browse &History//browseMethodHistory'
		'BrowseMethod &Pragmas ...//browseMethodPragmas'
		'&File out method ...//fileOutMethod'
		'-'
		'&Delete Method(s)//removeMethods'
		#('&Method Environment' '&0//setEnvironment0' '&1//setEnvironment1' '&2//setEnvironment2')
		'Run &Tests//runMethodTests'
	).
!

methodSourcePresenter

	^methodSourcePresenter.
!

methodValueChanged

	inUpdate ifTrue: [^self].
	methodSourcePresenter value asCharacterCollection = methodSource ifTrue: [
		methodSourcePresenter view 
			backcolor: JadeTextStyles default colorForNoEdits;
			isModified: false;
			yourself.
		self updateMethodStepPoints.
		self statusBarText: ''.
	] ifFalse: [
		methodSourcePresenter view 
			backcolor: JadeTextStyles default colorForUnsavedEdits;
			clearContainerIndicators;
			yourself.
	].
!

moveHistoryIndexTo: anInteger

	(anInteger <= 0) ifTrue: [^historyIndex := historyList  size].

	(anInteger > historyList size) ifTrue: [^historyIndex := 1].

	^historyIndex := anInteger!

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
	aSession dragObjects isEmpty 			ifTrue: [^self].
	aSession suggestedTarget isNil 			ifTrue: [^self].
	(aSession isFormatAvailable: #'class') 	ifTrue: [^self].
	(aSession isFormatAvailable: #'method')	ifTrue: [^self onDragMethodsOverClassHierarchy: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	SessionManager current pause.!

onDragOverClassList: aSession 

	aSession operation: nil.
	aSession dragObjects isEmpty ifTrue: [^self].
	aSession suggestedTarget isNil ifTrue: [^self].
	(aSession isFormatAvailable: #'class')			ifTrue: [^self].
	(aSession isFormatAvailable: #'method') 	ifTrue: [^self onDragMethodsOverClassList: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	SessionManager current pause.
!

onDragOverDictionary: aSession 

	aSession operation: nil.
	aSession dragObjects isEmpty ifTrue: [^self].
	aSession suggestedTarget isNil ifTrue: [^self].
	(aSession isFormatAvailable: #'class') 				ifTrue: [^self onDragClassesOverDictionary: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	SessionManager current pause.
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
	SessionManager current pause.
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
	GciSession log: 'JadeSystemBrowserPresenter>>onDropClassesOnDictionary:'.
	self updateCommand: stream contents.
!

onDropClassesOnPackage: aString session: aSession 

	| classes stream |
	classes := aSession dragObjects collect: [:each | each format: #'class'].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	SessionManager current pause.
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
	GciSession log: 'JadeSystemBrowserPresenter>>onDropMethodsOnClass: ' , aString printString , ' session: ' , aSession printString.
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
	GciSession log: 'JadeSystemBrowserPresenter>>onDropOnClassCategory:'.
	self updateCommand: stream contents.
!

onDropOnClassHierarchy: aSession 

	(aSession isFormatAvailable: #'method') ifTrue: [^self onDropMethodsOnClassHierarchy: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	SessionManager current pause.
!

onDropOnClassList: aSession 

	(aSession isFormatAvailable: #'method') ifTrue: [^self onDropMethodsOnClassList: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	SessionManager current pause.
!

onDropOnDictionary: aSession 

	(aSession isFormatAvailable: #'class') ifTrue: [^self onDropClassesOnDictionary: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	SessionManager current pause.
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
	GciSession log: 'JadeSystemBrowserPresenter>>onDropOnMethodCategory:'.
	self updateCommand: stream contents.
!

onDropOnPackageList: aSession 

	(aSession isFormatAvailable: #'class') 	ifTrue: [^self onDropClassesOnPackageList: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	SessionManager current pause.!

onSetFocus

	| activeView stack |
	ignoreNextSetFocusEvent ifTrue: [	"We have just returned from a Find Class dialog or error and don't need to update just yet"
		ignoreNextSetFocusEvent := false.
		^self.
	].
	updateProcess ifNotNil: [updateProcess terminate].
	activeView := View active.
	stack := Processor activeProcess stackTrace: 40.
	updateProcess := [self onSetFocus: activeView stack: stack] forkAt: Processor userBackgroundPriority.
!

onSetFocus: aView stack: aString

	GciSession log: 'JadeSystemBrowserPresenter>>onSetFocus:stack:'.
	[
		(Delay forMilliseconds: 50) wait.
		aView ~~ View active ifTrue: [^self].		"If view changed then there is no point in updating it!!"
		view = DeafObject current ifTrue: [^self].
		eventCount = gciSession eventCount ifTrue: [^self].
		super onSetFocus.
		[
			[
				self updateAndSelect: nil.
			] on: GsInvalidSessionError do: [:ex | 		"This process was forked and the session might be logging out"
				^self
			].
		] on: Error do: [:ex | 
			SessionManager current logError: ex.
			ignoreNextSetFocusEvent := true.
			MessageBox 
				errorMsg: ex description
				caption: 'Jade Error'.
			Keyboard default isShiftDown ifTrue: [
				| stream |
				stream := WriteStream on: String new.
				ex printTraceOn: stream.
				(JadeWorkspace showOnSession: gciSession) showText: stream contents.
			].
		].
	] ensure: [
		updateProcess := nil.
	].
!

onViewOpened

	super onViewOpened.
	categoryListPresenter		view contextMenu: (Menu fromStrings: self categoriesMenuStrings	).
	classCategoryPresenter	view contextMenu: (Menu fromStrings: self classCategoriesMenuStrings).
	classCommentPresenter	view isReadOnly: false.
	classHierarchyPresenter	view contextMenu: (Menu fromStrings: self classesMenuStrings		).
	classListPresenter		view contextMenu: (Menu fromStrings: self classesMenuStrings		).
	dictionaryListPresenter 	view contextMenu: (Menu fromStrings: self dictsMenuStrings		).
	globalsPresenter		view contextMenu: (Menu fromStrings: self globalsMenuStrings		).
	methodListPresenter		view contextMenu: (Menu fromStrings: self methodsMenuStrings		).
	methodSourcePresenter	view contextMenu: (Menu fromStrings: self editMenuStrings			);
		isBackgroundDwellEnabled: true;
		isBraceHighlightingEnabled: true;
		yourself.
	packageListPresenter 	view contextMenu: (Menu fromStrings: self pkgsMenuStrings		).
	variableListPresenter		view contextMenu: (Menu fromStrings: self variablesMenuStrings		).
	self setSearchPolicy.
	gciSession isPackagePolicyEnabled ifFalse: [
		| aView aModel aLabel |
		aView := packageDictionaryTabs parentView subViews detect: [:each | each isKindOf: TabViewXP].
		aModel := aView model.
		aLabel := aModel detect: [:each | each text = 'Packages'].
		aModel remove: aLabel.	"This seems to trigger a request for information since it changes from one tab to another"
	].
!

openSourceStyler

	| textStyles |
	textStyles := methodSourcePresenter view editStyles textStyles.
	JadeTextStyles default textStyles: textStyles.
	classDefinitionPresenter view textStyles: textStyles.
	originalSourcePresenter view textStyles: textStyles.
!

optimizedSelector
	"https://github.com/jgfoster/Jade/issues/173
	http://kermit.gemtalksystems.com/bug?bug=43830"

	MessageBox notify: 'Because this selector is optimized we do not have adequate information to offer implementors/senders as a shortcut menu here.'.!

packageListPresenter

	^packageListPresenter!

parseContext

	^nil!

pkgDictChanged

	| listModel |
	listModel := (textAreaTabs subViews detect: [:each | each class == TabViewXP]) model.
	globalsLabel ifNil: [globalsLabel := listModel detect: [:each | each text = 'Globals']].
	packageLabel ifNil: [packageLabel := listModel detect: [:each | each text = 'Package']].
	GciSession log: 'JadeSystemBrowserPresenter>>pkgDictChanged'.
	self isPackageListTabSelected ifTrue: [
		(listModel includes: packageLabel) ifFalse: [listModel addFirst: packageLabel].
		packageInfoTab ensureVisible.
		listModel remove: globalsLabel ifAbsent: [].
		self updateAndSelect: packageInfoTab.
	] ifFalse: [
		(listModel includes: globalsLabel ) ifFalse: [listModel addFirst: globalsLabel].
		globalsPresenter ensureVisible.
		listModel remove: packageLabel ifAbsent: [].
		self updateAndSelect: globalsPresenter.
	].
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
	ignoreNextSetFocusEvent := true.
	(string := Prompter prompt: 'Enter a selector:') isNil ifTrue: [^nil].
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
	(#(#'browseMethodPragmas') includes: command) ifTrue: [aCommandQuery isEnabled: gciSession gsHasPragmas. ^true].
	(#(#'editSave') includes: command) ifTrue: [
		aCommandQuery isEnabled: (isTextEdit and: [focusView isModified and: [
			textAreaTabs view currentCard ~= methodSourcePresenter view or: [self selectedClasses size == 1]	"Require a class when saving a method (#149)"
		]
	]). ^true].
	(#(#'editUndo') includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [focusView canUndo]). ^true].
	(#(#'editRedo') includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [focusView canRedo]). ^true].
	(#(#'editCut' #'editCopy' "#'editDelete'" ) includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [focusView hasSelection]). ^true].
	(#(#'editPaste') includes: aCommandQuery command) ifTrue: [aCommandQuery isEnabled: (isTextEdit and: [Clipboard current isTextAvailable]). ^true].
	(#(#'addRepository') includes: command) ifTrue: [aCommandQuery isEnabled: packageListPresenter selections notEmpty. ^true].
	(#(#'showPackageChanges') includes: command) ifTrue: [
		aCommandQuery isEnabled: (packageListPresenter selections size = 1 and: [packageListPresenter selection value]). ^true].
	(#(#'savePackage') includes: command) ifTrue: [aCommandQuery isEnabled: packageListPresenter selections size = 1. ^true].
	(#(#'removeRepository') includes: command) ifTrue: [
		aCommandQuery isEnabled: (repositoryListPresenter hasSelection and: [packageListPresenter selections size = 1]). ^true].
	(#(#'compareAncestor') includes: command) ifTrue: [
		aCommandQuery isEnabled: (packageListPresenter selections size = 1 and: [ancestorListPresenter hasSelection]). ^true].
	(#(#'browseMethodHistory' #'browseImplementors' #'browseSenders' #'fileOutMethod') includes: command) ifTrue: [
		aCommandQuery isEnabled: methodListPresenter selections size = 1. ^true].
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
	(#(#'browseSelectedClass') includes: command) ifTrue: [aCommandQuery isEnabled: self isClassSelectedInEditor. ^true].

	(command == #historyBack) ifTrue: [aCommandQuery isEnabled: self hasCommandHistoryBack. ^true].
	(command == #historyForward) ifTrue: [aCommandQuery isEnabled: self hasCommandHistoryForward. ^true].

	(command == #'setEnvironment0' and: [environment == 0]) ifTrue: [aCommandQuery isChecked: true. ^true].
	(command == #'setEnvironment1' and: [environment == 1]) ifTrue: [aCommandQuery isChecked: true. ^true].
	(command == #'setEnvironment2' and: [environment == 2]) ifTrue: [aCommandQuery isChecked: true. ^true].

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
	GciSession log: 'JadeSystemBrowserPresenter>>removeClass'.
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
	GciSession log: 'JadeSystemBrowserPresenter>>removeDictionary'.
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
	GciSession log: 'JadeSystemBrowserPresenter>>removeGlobals'.
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
	GciSession log: 'JadeSystemBrowserPresenter>>removeMethodCategories'.
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
	GciSession log: 'JadeSystemBrowserPresenter>>removeMethods'.
	self updateCommand: string.
!

removePriorVersions

	| stream |
	stream := WriteStream on: String new.
	stream nextPutAll: 'removePriorVersions'; lf.
	self addPackageDictionaryInfoTo: stream.
	self selectedClasses do: [:each | stream nextPutAll: each; tab].
	stream lf; nextPutAll: self requestString.
	GciSession log: 'JadeSystemBrowserPresenter>>removePriorVersions'.
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
	GciSession log: 'JadeSystemBrowserPresenter>>removeRepository'.
	self updateCommand: stream contents , self requestString.
!

reportCompileError: anArrayOfArray 

	| source position stream string indicators |
	indicators := OrderedCollection new.
	methodSourcePresenter view clearContainerIndicators.
	source := self currentMethodSource.
	position := methodSourcePresenter view caretPosition.
	methodSourcePresenter value: source asUtf8String.
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
	methodSourcePresenter view backcolor: JadeTextStyles default colorForCompileError.
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

	JadeTextStyles default resetColors.
!

revertNewClass

	| stream |
	stream := WriteStream on: String new.
	stream nextPutAll: 'revertClass'; lf.
	self addPackageDictionaryInfoTo: stream.
	stream nextPutAll: selectedClassName subStrings first.
	stream lf; nextPutAll: self requestString.
	GciSession log: 'JadeSystemBrowserPresenter>>revertNewClass'.
	self updateCommand: stream contents.
!

runClassTests

	| stream string list caption selection |
	stream := (WriteStream on: String new)
		nextPutAll: self behaviorIdentifier;
		yourself.
	string := self gciSession 
		serverPerform: #'runClassTests:' 
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
		serverPerform: #'runMethodTests:' 
		with: self methodsIdentifier.
	result == true ifFalse: [self error: 'unexpected result!!'].
	MessageBox notify: 'Ran ' , methodListPresenter selections size printString , ' test(s)'.
!

savePackage

	| package string array dict stream |
	package := packageListPresenter selections first.
	GciSession log: 'JadeSystemBrowserPresenter>>savePackage'.
	string := self updateCommand: 'uniqueVersionName' , Character tab asString , package key.
	string = 'uniqueVersionName' ifFalse: [self error: 'Unrecognized response'].
	string := readStream nextLine.
	array := ancestorListPresenter list.
	array := array isEmpty 
		ifTrue: [#('' '' '' '')]
		ifFalse: [array first].
	dict := Dictionary new
		at: #'httpPassword'		put: '';
		at: #'httpUser'			put: '';
		at: #'name' 			put: package key;
		at: #'isModified'			put: package value;
		at: #'nextVersionName'	put: string;
		at: #'priorVersionName'	put: (array at: 2);
		at: #'versionMessage'		put: (array at: 4);
		at: #'repositoryList'		put: repositoryListPresenter list;
		at: #'repository'			put: (repositoryListPresenter selectionIfNone: [repositoryListPresenter model last]);
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

saveTonelPackageA
"
	| name path repository snapshot |
	name := 'Rowan-Services-Core'.
	path := '/Users/jfoster/Rowan/rowan/src'.
	repository := (CypressTonelRepository on: path) 
		codeFormatProperty: 'Tonel';
		initializeReaderAndWriterClasses;
		yourself.
	snapshot := (CypressPackageDefinition named: name) snapshot.
	repository writer
		writeSnapshot: snapshot
		inPackageNamed: name.
"!

selectClass: aString selector: methodString
"
	Array with: className with: dictionaryName with: catetory with: packageName.
"
	| string list className isMeta array |
	updateProcess ifNotNil: [		"https://github.com/jgfoster/Jade/issues/109"
		updateProcess terminate.
		updateProcess := nil.
	].
	GciSession log: 'JadeSystemBrowserPresenter>>selectClass - ' , aString , ' selector: ' , methodString.
	string := environment printString , ' findClass'.
	string := self gciSession encode: string.
	string := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: string.
	string := self gciSession decode: string.
	GciSession log: 'JadeSystemBrowserPresenter>>selectClass:selector: - string size ' , string size printString.
	list := (string subStrings: Character lf) collect: [:each | each subStrings: Character tab].
	GciSession log: 'JadeSystemBrowserPresenter>>selectClass:selector: - list size ' , list size printString.
	list := list copyFrom: 2 to: list size.
	list := list collect: [:each | each size < 3 ifTrue: [each , #('' '' '')] ifFalse: [each]].
	className := aString.
	(isMeta := className endsWith: ' class') ifTrue: [
		className := className copyFrom: 1 to: className size - 6.
	].
	array := list detect: [:each | each first subStrings first = className].
	GciSession log: 'JadeSystemBrowserPresenter>>selectClass:selector: - found match of ' , array printString.
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

selectedMethodChanged

	| historyEntry |
	GciSession log: 'JadeSystemBrowserPresenter>>selectedMethodChanged'.
	self updateAndSelect: methodSourcePresenter.
	methodListPresenter selections size == 1 ifFalse: [^self].
	"historyList := historyList copyFrom: 1 to: historyIndex."	"trim any forward options on an explicit manual selection"
	historyEntry := Array
		with: selectedClassName , (self isClassSideSelected ifTrue: [' class'] ifFalse: [''])
		with: (methodListPresenter selection at: 1)
		with: self requestStringForUpdate.
	(self hasHistoryEntry: historyEntry) ifFalse: [historyList add: historyEntry].
	"historyIndex := historyList size. "
!

selectedMethodName

	^methodListPresenter selections first first.
!

setColorForCompileErrors

	| newColor |
	newColor := ColorDialog showModalOn: JadeTextStyles default colorForNoEdits.
	JadeTextStyles default colorForCompileError: newColor.
!

setColorForNoEdits

	| newColor |
	newColor := ColorDialog showModalOn: JadeTextStyles default colorForNoEdits.
	JadeTextStyles default colorForNoEdits: newColor.
!

setColorForUnsavedEdits

	| newColor |
	newColor := ColorDialog showModalOn: JadeTextStyles default colorForUnsavedEdits.
	JadeTextStyles default colorForUnsavedEdits: newColor.
	!

setEnvironment: anInteger

	environment := anInteger.
	GciSession log: 'JadeSystemBrowserPresenter>>setEnvironment: ' , anInteger printString.
	self updateCommand: self requestStringForUpdate.
!

setEnvironment0

	self setEnvironment: 0
!

setEnvironment1

	self setEnvironment: 1
!

setEnvironment2

	self setEnvironment: 2
!

setHomeDictionary

	| string |
	string := 'setHomeDictionary' , Character tab asString , dictionaryListPresenter selection key , Character lf asString , self requestString.
	GciSession log: 'JadeSystemBrowserPresenter>>setHomeDictionary'.
	self updateCommand: string.
!

setSearchPolicy

	ancestorListPresenter 	view model searchPolicy: SearchPolicy equality.
	categoryListPresenter 	view model searchPolicy: SearchPolicy equality.
	classCategoryPresenter	view model searchPolicy: SearchPolicy equality.
	classHierarchyPresenter	view model searchPolicy: SearchPolicy equality.
	classListPresenter		view model searchPolicy: SearchPolicy equality.
	dictionaryListPresenter 	view model searchPolicy: SearchPolicy equality.
	globalsPresenter		view model searchPolicy: SearchPolicy equality.
	methodListPresenter		view model searchPolicy: SearchPolicy equality.
	overrideListPresenter		view model searchPolicy: SearchPolicy equality.
	packageListPresenter 	view model searchPolicy: SearchPolicy equality.
	pragmaListPresenter		view model searchPolicy: SearchPolicy equality.
	repositoryListPresenter	view model searchPolicy: SearchPolicy equality.
	superclassListPresenter	view model searchPolicy: SearchPolicy equality.
	variableListPresenter	 	view model searchPolicy: SearchPolicy equality.
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
	GciSession log: 'JadeSystemBrowserPresenter>>showPackageChanges'.
	(self updateCommand: stream contents) = 'changesInPackage' ifFalse: [self error: 'Unexpected response!!'].
	patch := MCPatch
		fromString: readStream upToEnd
		session: self gciSession.
	patch operations isEmpty ifTrue: [
		MessageBox notify: 'No changes!!'.
		^self.
	].
	MCPatchDiffBrowser showOn: patch.
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

stuffToKeepFromPath: pathString andAddTo: newSource

	| file existingSource i j string existingHeader newHeader x |
	[
		file := FileStream
			read: pathString
			type: #'utf8'.
	] on: Exception do: [:ex | ^''].
	existingSource := file contents.
	file close.
	string := 'doit' , Character lf asString.
	(i := existingSource indexOfSubCollection: string) <3 ifTrue: [^''].
	(j := newSource indexOfSubCollection: string) <3 ifTrue: [^''].
	(i == j and: [(existingSource copyFrom: 1 to: i) = (newSource copyFrom: 1 to: j)]) ifTrue: [^''].
	existingHeader := existingSource copyFrom: 1 to: i - 2.
	newHeader := newSource copyFrom: 1 to: j - 2.
	[
		0 < (i := existingHeader size - newHeader size) and: [
			x := existingHeader copyFrom: i + 1 to: existingHeader size.
			x = newHeader.
		].
	] whileTrue: [
		existingHeader = newHeader ifTrue: [^''].
		existingHeader := existingHeader copyFrom: 1 to: i - 1.
	].
	^(MessageBox confirm: 'Add the following header to the fileout?
' , existingHeader caption: 'Existing fileout has extra line(s) at the top') 
		ifTrue: [existingHeader , Character lf asString]
		ifFalse: [''].

!

textTabChanged

	methodSourcePresenter view cancelCallTip.
	(textAreaTabs currentCard name = 'globals' 	and: [self isPackageListTabSelected	]) ifTrue: [packageInfoTab 	ensureVisible].
	(textAreaTabs currentCard name = 'packageInfo' and: [self isDictionariesTabSelected	]) ifTrue: [globalsPresenter 	ensureVisible].
	self updateMenuBar.
!

unloadPackage

	| packageName |
	packageName := packageListPresenter selections first key.
	(MessageBox confirm: 'Do you want to unload ''' , packageName , '''?' caption: 'Confirm Unload') ifFalse: [^self].
	GciSession log: 'JadeSystemBrowserPresenter>>unloadPackage'.
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
		nextPutAll: (aBoolean ifNil: ['default'] ifNotNil: [aBoolean ifTrue: ['classTab'] ifFalse: ['instanceTab']]); lf;
		lf; 	"superclass"
		nextPutAll: 'categoryList'; lf;	"not variables"
		lf;		"methodFilter (category or variable)"
		nextPutAll: aString; lf; 	"method names"
		lf;		"override"
		yourself.
	GciSession log: 'JadeSystemBrowserPresenter>>updateAfterFindClass: ' , anArray printString , ' isMeta: ' , aBoolean printString , ' selector: ' , aString.
	self updateCommand: stream contents.
!

updateAndSelect: aView

	updateCount := updateCount + 1.
	keystrokeTime < Time millisecondClockValue ifTrue: [
		GciSession log: 'JadeSystemBrowserPresenter>>updateAndSelect:'.
		self updateAndSelectA: aView.
	] ifFalse: [
		GciSession log: 'JadeSystemBrowserPresenter>>updateAndSelect: - a'.
		[
			GciSession log: 'JadeSystemBrowserPresenter>>updateAndSelect: - b'.
			self updateAndSelectB: aView.
		] fork.
	].
!

updateAndSelect_: anAssociation

	GciSession log: 'JadeSystemBrowserPresenter>>updateAndSelect_:' , anAssociation printString.
	self updateAndSelect: anAssociation key.
!

updateAndSelectA: aView

	inUpdate ifTrue: [^self].
	GciSession log: 'JadeSystemBrowserPresenter>>updateAndSelectA:'.
	self updateCommand: self requestStringForUpdate.
	self updateMenuBar.
	aView notNil ifTrue: [aView ensureVisible].
!

updateAndSelectB: aView

	| delay oldUpdateCount |
	oldUpdateCount := updateCount.
	delay := keystrokeTime - Time millisecondClockValue.
	GciSession log: 'JadeSystemBrowserPresenter>>updateAndSelectB: - a - ' , delay printString.
	(Delay forMilliseconds: delay) wait.
	oldUpdateCount = updateCount ifTrue: [
		GciSession log: 'JadeSystemBrowserPresenter>>updateAndSelectB: - b'.
		self updateAndSelectA: aView.
	] ifFalse: [
		GciSession log: 'JadeSystemBrowserPresenter>>updateAndSelectB: - c'.
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
	readStream nextLineAsList asSortedCollection do: [:each | 
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
	GciSession log: 'JadeSystemBrowserPresenter>>updateClassCategoryTree - ' , listA printString.
	listB := (classCategoryPresenter model asBag collect: [:each | each key printString]) asSortedCollection asArray.
	listA = listB ifFalse: [
		classCategoryPresenter
			model: treeModel;
			yourself.
	].
	newSelection := readStream nextLine subStrings: $-.
	newSelection isEmpty ifTrue: [newSelection := root key].
	existingSelection := (classCategoryPresenter selectionIfNone: [nil -> nil]) key.
	newSelection = existingSelection ifFalse: [
		| items item |
		items := classCategoryPresenter model asBag asArray.
		0 to: newSelection size do: [:i | 
			| key |
			key := newSelection copyFrom: 1 to: i.
			item := items detect: [:each | each key = key] ifNone: [nil].
			classCategoryPresenter expand: item.
		].
		classCategoryPresenter selection: item ifAbsent: [].
	].
	classCategoryPresenter view ensureSelectionVisible.
!

updateClassHierarchy

	| list paths treeModel x y currentSelection newSelection flags |
	list := readStream nextList.
	newSelection := readStream nextLineAsList reverse.
	flags := readStream nextLineAsList.

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
	(newSelection notNil and: [currentSelection ~= newSelection]) ifTrue: [
		classHierarchyPresenter 
			selection: newSelection;
			expand: newSelection;
			yourself.
	].
	self selectedClassName: ((newSelection notNil and: [newSelection notEmpty]) ifTrue: [newSelection last] ifFalse: ['']).
	classListPresenter selectionOrNil: nil.

	selectedClassesAreTestCases := (flags at: 1) = 'true'.
	sunitPresenter testClassName: (selectedClassesAreTestCases ifTrue: [selectedClassName] ifFalse: ['']).!

updateClassInfo

	| index newClassDefinition |
	selectedClassOop := readStream nextLine asNumber.
	newClassDefinition := readStream nextParagraph asCharacterCollection.
	classCommentPresenter value: readStream nextParagraph asUtf8String.

	classDefinitionPresenter view isModified ifTrue: [
		newClassDefinition = classDefinition ifTrue: [^self].
		(MessageBox confirm: 'Copy changes to clipboard?' caption: 'Class has unsaved changes!!') ifTrue: [
			classDefinitionPresenter value copyToClipboard.
		].
	].
	classDefinition := newClassDefinition.
	0 < (classDefinition indexOfSubCollection: 'MyNewClass') ifTrue: [	"Template is based on Stream class, but we don't want to use 'Globals' in template"
		index := classDefinition indexOfSubCollection: ': Globals'.		
		(0 < index and: [(classDefinition copyFrom: index - 2 to: index + 1) ~= 'User']) ifTrue: [
			classDefinition := (classDefinition copyFrom: 1 to: index + 1) , 
				(dictionaryListPresenter selectionIfNone: ['UserGlobals' -> false]) key , 
				(classDefinition copyFrom: index + 9 to: classDefinition size).
			index := classDefinition indexOfSubCollection: 'User Classes'.
			0 < index ifTrue: [
				classDefinition := (classDefinition copyFrom: 1 to: index - 1) , 
					(packageListPresenter selectionIfNone: ['User Classes' -> nil]) key , 
					(classDefinition copyFrom: index + 12 to: classDefinition size).
			].
		].
	].
	classDefinitionPresenter value: classDefinition asUtf8String.
	classDefinitionPresenter view 
		backcolor: Color white;
		isModified: false;
		yourself.
!

updateClassList

	| fullList newSelections flags |
	classListPresenter ensureVisible.
	fullList := readStream nextLineAsList.
	GciSession log: 'JadeSystemBrowserPresenter>>updateClassList - ' , fullList printString.
	newSelections := readStream nextLineAsList.
	flags := readStream nextLineAsList.

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

	selectedClassesAreTestCases := (flags at: 1) = 'true'.
	sunitPresenter testClassName: (selectedClassesAreTestCases ifTrue: [selectedClassName] ifFalse: ['']).!

updateClassListOrHierarchy

	| tab |
	tab := readStream nextLine.
	tab = 'classHierarchy' ifTrue: [^self updateClassHierarchy].
	tab = 'classList' ifTrue: [^self updateClassList].
	self error: 'Unexpected token!!'.
!

updateCommand: aString

	^self 
		updateCommand: (gciSession encode: aString) 
		onSuccessDo: [].
!

updateCommand: aString onSuccessDo: aBlock

	GciSession log: 'JadeSystemBrowserPresenter>>updateCommand: ' , aString printString , ' onSuccessDo: '.
	[
		| time1 time2 time3 string |
		time1 := Time millisecondsToRun: [
			string := self gciSession encode: environment printString , ' ' , aString.
			string := self gciSession 
				serverPerform: #'systemBrowser:' 
				with: string.
			string := self gciSession decode: string.
			GciSession log: 'JadeSystemBrowserPresenter>>updateCommand:onSuccessDo:  - a - ' , string printString.
			eventCount := self gciSession eventCount.
		].
		time2 := Time millisecondsToRun: [
			| responseType |
			readStream := JadeServerReadStream on: string.
			time3 := readStream nextLine asNumber.
			aBlock value.
			(responseType := readStream nextLine) = 'update' ifFalse: [^responseType].
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
	fullList := readStream nextLineAsList collect: [:each | (each copyFrom: 2 to: each size) -> (each first = $H)].
	dictionaryListPresenter list = fullList ifFalse: [
		dictionaryListPresenter list: fullList.
	].
	selections := readStream nextLineAsList collect: [:x | dictionaryListPresenter list detect: [:y | x = y key]].
	dictionaryListPresenter selections = selections ifFalse: [
		dictionaryListPresenter selections: selections.
	].
	dictionaryListPresenter view ensureSelectionVisible.
	lines := readStream nextList.
	globalsPresenter list: lines.
!

updateMenuBar

	| shellView menuBar dictsPkgsMenu methodFilterMenu textAreaMenu |
	dictsPkgsMenu 		:= Menu fromStrings: (self isDictionariesTabSelected 	ifTrue: [self dictsMenuStrings] 		ifFalse: [self pkgsMenuStrings		]).
	methodFilterMenu 	:= Menu fromStrings: (self isCategoriesTabSelected 	ifTrue: [self categoriesMenuStrings] 	ifFalse: [self variablesMenuStrings	]).
	textAreaMenu 		:= Menu fromStrings: (self isGlobalsTabSelected 		ifTrue: [self globalsMenuStrings] 	ifFalse: [self editMenuStrings		]).
	shellView := self parentPresenter parentPresenter view.
	menuBar := shellView menuBar
		clear;
		addItem: (Menu fromStrings: self jadeMenuStrings);
		addItem: dictsPkgsMenu;
		addItem: (Menu fromStrings: self classCategoriesMenuStrings);
		addItem: (Menu fromStrings: self classesMenuStrings);
		addItem: methodFilterMenu;
		addItem: (Menu fromStrings: self methodsMenuStrings);
		addItem: textAreaMenu;
		addItem: (Menu fromStrings: self preferencesMenuStrings);
		yourself.
	shellView menuBar: menuBar.
!

updateMethod

	| gsMethod isReadOnly listModel methodSourceLabel newSource warnings x |
	methodSourcePresenter view cancelCallTip.
	gsMethod := GsMethod 
		fromStream: readStream
		session: gciSession.
	isReadOnly := gsMethod isReadOnly.	"current user has write permission for the class"
	newSource := gsMethod source.
	methodSourcePresenter view isModified == true ifTrue: [
		newSource = methodSource ifTrue: [^self].
		(MessageBox confirm: 'Copy changes to clipboard?' caption: 'Method has unsaved changes!!') ifTrue: [
			methodSourcePresenter value copyToClipboard.
		].
	].
	methodSource := newSource.
	methodSourcePresenter value: methodSource asUtf8String.
	unimplementedSelectors := gsMethod unimplementedSelectors.
	stepPoints := gsMethod stepPoints.
	breakPoints := gsMethod breakPoints.
	self updateMethodStepPoints.
	methodSourcePresenter view 
		backcolor: JadeTextStyles default colorForNoEdits;
		isModified: false;
		isReadOnly: isReadOnly;
		yourself.
	(overrideListPresenter list notEmpty and: [overrideListPresenter selectionOrNil ~= overrideListPresenter list last]) ifTrue: [
		methodSourcePresenter view
			backcolor: nil;
			isReadOnly: true;
			yourself.
	].
	((methodCategory := gsMethod category) notEmpty and: [self isCategoriesTabSelected]) ifTrue: [
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
	listModel := (textAreaTabs subViews detect: [:each | each class == TabViewXP] ifNone:[^self]) model.
	originalSourceLabel ifNil: [originalSourceLabel := listModel detect: [:each | each text  = 'Original Source']].
	methodSourceLabel := listModel detect: [:each | each text = 'Method Source'].
	(x := gsMethod originalSource) isEmpty ifTrue: [
		listModel remove: originalSourceLabel ifAbsent: [].
	] ifFalse: [
		(listModel includes: originalSourceLabel) ifFalse: [listModel add: originalSourceLabel before: methodSourceLabel].
		originalSourcePresenter value: x asUtf8String.
	].
	(warnings := gsMethod warnings) notEmpty ifTrue: [
		MessageBox warning: warnings caption: 'Jade Compile Warning'.
	].
	!

updateMethodFilter

	| listPresenter pieces type tabs tab filters selections |
	pieces := readStream nextLine subStrings: Character tab.
	type := pieces at: 1.
	1 < pieces size ifTrue: [environment := (pieces at: 2) asNumber].
	(tabs := methodFilterTabs cards) isEmpty ifTrue: [^self].
	tab := tabs detect: [:each | each name = type].
	tab ensureVisible.
	listPresenter := self methodFilterListPresenter.
	filters := readStream nextLineAsList reverse.
	filters = listPresenter list ifFalse: [
		listPresenter list: filters.
	].
	selections := readStream nextLineAsList.
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

	previousSelectedMethod := methodListPresenter selections isEmpty ifTrue: [''] ifFalse: [self selectedMethodName].

	fullList := (readStream nextParagraph subStrings: Character lf) collect: [:each | (each subStrings: Character tab) , #('' '' '' '' '')].
	fullList := fullList do: [:each | 	"JadeServer>>#'sbUpdateMethods' "
		each 
			at: 2 put: (each at: 2) = 'T';		"has a superclass implementation"
			at: 3 put: (each at: 3) = 'T';		"is in a TestCase class"
			at: 4 put: (each at: 4) = 'T';		"method replaced by GsPackagePolicy"
			yourself.
	].
	fullList = methodListPresenter list ifFalse: [
		methodListPresenter list: fullList.
	].
	selections := readStream nextLineAsList.	"JadeServer>>#'sbUpdateMethodSelectionsIn:' "
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

	selectedClassChanged ifTrue: [ | matchMethod |
		matchMethod := methodListPresenter model detect: [:array | (array at: 1) = previousSelectedMethod] ifNone: [^self].
		[methodListPresenter selection: matchMethod ifAbsent: []] forkAt: 4 .
	]!

updateMethodStepPoints

	| indicators |
	methodSourcePresenter view clearContainerIndicators.
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
	methodSourcePresenter view indicators: indicators.
!

updateOverrideList

	| list selection |
	list := readStream nextLineAsList.		"JadeServer>>#'sbUpdateMethod:' "
	list = overrideListPresenter list ifFalse: [
		overrideListPresenter list: list.
	].
	selection := readStream nextLine.
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
	next := readStream nextLine.
	GciSession log: 'JadeSystemBrowserPresenter>>updatePackageDictionaryList'.
	next = 'dictionaryList' ifTrue: [^self updateDictionaryList].
	next = 'packageList' ifTrue: [^self updatePackageList].
	self error: 'Unexpected token'.
!

updatePackageInfo

	| list |
	list := readStream nextList collect: [:each | each , #('' '' '' '')].
	list = ancestorListPresenter list ifFalse: [
		ancestorListPresenter list: list.
	].
	list := readStream nextList collect: [:each | each , #('' '')].
	list = repositoryListPresenter list ifFalse: [
		repositoryListPresenter list: list.
	].
!

updatePackageList

	| dictionary fullList selections old new |
	GciSession log: 'JadeSystemBrowserPresenter>>updatePackageList'.
	packageListPresenter ensureVisible.
	dictionary := Dictionary new.
	readStream nextLineAsList do: [:each | dictionary at: each put: false].
	readStream nextLineAsList do: [:each | dictionary at: each put: true].
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
	selections :=  readStream nextLineAsList.
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
			updateTabLabel;
			yourself.
	] ensure: [
		inUpdate := false.
	].
!

updateSuperclassList

	| tabs tabName tab list selected |
	tabName := readStream nextLine.
	(tabs := instanceClassTabs cards) isEmpty ifTrue: [
		readStream nextLine.
		readStream nextLine.
		^self
	].
	tab := tabs 
		detect: [:each | each name = tabName]
		ifNone: [self error: 'None of ' , tabs printString , ' match ' , tabName printString].
	tab ensureVisible.
	list := readStream nextLineAsList reverse.
	list = superclassListPresenter list ifFalse: [
		superclassListPresenter list: list.
	].
	(selected := readStream nextLine) notEmpty ifTrue: [
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
		0 < environment ifTrue: [newLabel := newLabel , ' [' , environment printString , ']'].
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

variableListPresenter

	^variableListPresenter!

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
!JadeSystemBrowserPresenter categoriesFor: #browseMethodPragmas!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseMethodsAndSelect:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseMethodsAndSelect:type:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseMethodsContaining!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseSelectedClass!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseSenders!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseSendersOf!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #browseSendersOf:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #buildHistoryMenuNamed:from:to:by:!history!public! !
!JadeSystemBrowserPresenter categoriesFor: #categoriesMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #categoryListPresenter!accessing!public! !
!JadeSystemBrowserPresenter categoriesFor: #classCategoriesMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #classCategoryPresenter!accessing!public! !
!JadeSystemBrowserPresenter categoriesFor: #classDefChanged!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #classesMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #classHierarchyPresenter!accessing!public! !
!JadeSystemBrowserPresenter categoriesFor: #classListPresenter!accessing!public! !
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
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForPragmaList!create schemantic wiring!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForSuperClassList!create schemantic wiring!overrides!public! !
!JadeSystemBrowserPresenter categoriesFor: #createSchematicWiringForVariableList!create schemantic wiring!public! !
!JadeSystemBrowserPresenter categoriesFor: #currentMethodSource!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #currentSelectionOrLine!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #defaultFileExtension!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #delayUpdate!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #dictionaryListPresenter!accessing!public! !
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
!JadeSystemBrowserPresenter categoriesFor: #fileInCode!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #fileOutClass!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #fileOutClassOnPath:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #fileOutDictionary!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #fileOutMethod!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #fileOutMethodOnPath:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #fileTypes!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #findClass!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #findClassList!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #gciSession:!overrides!public! !
!JadeSystemBrowserPresenter categoriesFor: #getSelectedMethod!public! !
!JadeSystemBrowserPresenter categoriesFor: #getViews!public! !
!JadeSystemBrowserPresenter categoriesFor: #globalsMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #handleInvalidSession!public! !
!JadeSystemBrowserPresenter categoriesFor: #hasCommandHistoryBack!history!public! !
!JadeSystemBrowserPresenter categoriesFor: #hasCommandHistoryForward!history!public! !
!JadeSystemBrowserPresenter categoriesFor: #hasHistoryEntry:!event handlers!history!public! !
!JadeSystemBrowserPresenter categoriesFor: #historyBack!history!public! !
!JadeSystemBrowserPresenter categoriesFor: #historyBackMenu!history!public! !
!JadeSystemBrowserPresenter categoriesFor: #historyForward!history!public! !
!JadeSystemBrowserPresenter categoriesFor: #historyForwardMenu!history!public! !
!JadeSystemBrowserPresenter categoriesFor: #horizontalSplitter!public! !
!JadeSystemBrowserPresenter categoriesFor: #initialize!history!public! !
!JadeSystemBrowserPresenter categoriesFor: #insertDictionary!public! !
!JadeSystemBrowserPresenter categoriesFor: #inspectGlobal!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isCategoriesTabSelected!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isClassListTabSelected!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #isClassSelectedInEditor!public! !
!JadeSystemBrowserPresenter categoriesFor: #isClassSideSelected!accessing!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #isDictionariesTabSelected!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isGlobalsTabSelected!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isOkayToChange!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isPackageListTabSelected!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isPragmasTabSelected!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isTreeModel:equivalentTo:!public! !
!JadeSystemBrowserPresenter categoriesFor: #isVariablesTabSelected!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #jadeDebug!menu handlers!private! !
!JadeSystemBrowserPresenter categoriesFor: #jadeDisplay!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #jadeExecute!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #jadeExecuteAndDisplay:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #jadeInspect!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #jadeMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #jumpToHistoryIndex:!history!public! !
!JadeSystemBrowserPresenter categoriesFor: #labelOfClass:changedTo:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #labelOfClass:editedTo:accept:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #layoutInfo!public! !
!JadeSystemBrowserPresenter categoriesFor: #layoutInfo:!public! !
!JadeSystemBrowserPresenter categoriesFor: #loadLatestVersion!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodDoubleClicked:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodFilterListPresenter!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #methodHoverEnd:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodHoverStart:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodListPresenter!accessing!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodMenu:!event handlers!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodMenuBreak:!event handlers!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodsIdentifier!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodsMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodSourcePresenter!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #methodValueChanged!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #moveHistoryIndexTo:!history!public! !
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
!JadeSystemBrowserPresenter categoriesFor: #optimizedSelector!event handlers!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #packageListPresenter!accessing!public! !
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
!JadeSystemBrowserPresenter categoriesFor: #saveTonelPackageA!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #selectClass:selector:!public! !
!JadeSystemBrowserPresenter categoriesFor: #selectedClassChanged:!public! !
!JadeSystemBrowserPresenter categoriesFor: #selectedClasses!public! !
!JadeSystemBrowserPresenter categoriesFor: #selectedClassName:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #selectedClassNameWithoutVersion!public! !
!JadeSystemBrowserPresenter categoriesFor: #selectedMethodChanged!event handlers!history!public! !
!JadeSystemBrowserPresenter categoriesFor: #selectedMethodName!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setColorForCompileErrors!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setColorForNoEdits!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setColorForUnsavedEdits!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setEnvironment:!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setEnvironment0!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setEnvironment1!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setEnvironment2!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setHomeDictionary!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #setSearchPolicy!overrides!public! !
!JadeSystemBrowserPresenter categoriesFor: #showPackageChanges!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #splitterPosition:!public! !
!JadeSystemBrowserPresenter categoriesFor: #statusBarServerRequestText:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #statusBarText:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #stepPointAt:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #stuffToKeepFromPath:andAddTo:!public! !
!JadeSystemBrowserPresenter categoriesFor: #textTabChanged!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #unloadPackage!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #updateAfterFindClass:isMeta:selector:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateAndSelect:!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #updateAndSelect_:!public!updating! !
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
!JadeSystemBrowserPresenter categoriesFor: #variableListPresenter!accessing!public! !
!JadeSystemBrowserPresenter categoriesFor: #variablesMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #viewActivated!event handlers!public! !

!JadeSystemBrowserPresenter class methodsFor!

overriddenIcon

	^##((TextTileIcon text: $\x25B4 fontName: 'Segoe UI')
		textcolor: (ARGB named: #dodgerBlue);
		yourself)
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 544 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 nil nil nil 544 655878 ##(Smalltalk.CardLayout) 138 144 34 7 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Package' 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 7 1 752 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 784 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 816 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 544 34 2 8 1140850688 131073 928 nil nil nil 5 nil nil nil 928 482 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 928 34 2 8 1409355853 1025 1040 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 624 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 1040 nil 8 1778896832 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Ancestor' 131 #left 1362 #displayString 8 #() 1362 #<= 1536 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 1568 7 257 nil nil 1040 nil 1 nil nil 1474 8 'Name' 401 #left 1362 #displayString 1392 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 1696 7 257 nil nil 1040 nil 1 nil nil 1474 8 'Timestamp' 301 #left 1362 #displayString 8 #() 1362 #<= 1824 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 1856 7 257 nil nil 1040 nil 1 nil nil 1474 8 'Message' 931 #left 1362 #displayString 1824 1362 #<= 1824 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 2000 7 257 nil nil 1040 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 328198 ##(Smalltalk.Point) 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 2098 1 1 2098 1769 265 1040 2194 #contextMenu: 34 1 1216 1040 2194 #text: 34 1 8 'Ancestor' 1040 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 132 0 0 0] 8 #() 2098 193 193 nil 27 23 410 ##(Smalltalk.ListView) 34 30 nil 928 34 2 8 1409355853 1025 2432 1106 138 144 1152 nil 1184 624 nil 5 1202 nil true 34 5 1250 1 1282 #addRepository 8 'Add &Repository' 1 1 nil nil nil 1250 1 1282 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 1250 1 1282 #savePackage 8 '&Save Package' 1 1 nil nil nil 1250 1 1282 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 2432 nil 8 1778896832 1362 #displayString 8 #() nil 1424 nil nil nil nil nil nil 138 144 34 4 1474 8 'Repository Type' 221 #left 1362 #displayString 2832 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 2928 7 257 nil nil 2432 nil 1 nil nil 1474 8 'Description' 1151 #left 1362 #displayString 1824 1362 #<= 1824 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 3072 7 257 nil nil 2432 nil 3 nil nil 1474 8 'User' 201 #left 1362 #displayString 1536 1362 #<= 1536 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 3216 7 257 nil nil 2432 nil 1 nil nil 1474 8 'Password' 201 #left 1362 #displayString 1536 1362 #<= 1536 802 0 nil 834 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 3360 802 0 nil 3376 19 257 nil 7 257 nil nil 2432 nil 1 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2098 1 1 nil 2130 138 144 34 3 2194 #createAt:extent: 34 2 2098 1 265 2098 1769 217 2432 2194 #contextMenu: 34 1 2528 2432 2194 #text: 34 1 8 'Repository Type' 2432 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 132 0 0 0 116 3 0 0 240 0 0 0] 8 #() 2416 nil 27 19 true 170 192 34 4 1040 8 'ancestorList' 2432 8 'repositoryList' nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 9 53 2098 1769 481 928 2354 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 120 3 0 0 10 1 0 0] 34 2 1040 2432 2416 nil 27 722 754 8 'Globals' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 3984 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 4000 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 544 34 2 8 1409355853 1025 4096 1106 138 144 1152 nil 1184 624 nil 5 nil nil nil 4096 nil 8 1778896832 1362 #displayString 8 #() nil 1424 nil nil nil nil nil nil 138 144 34 3 1474 8 'Name' 587 #left 1362 #displayString 4224 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 4320 7 257 nil nil 4096 nil 3 nil nil 1474 8 'Class' 589 #left 1362 #displayString 1824 1362 #<= 1824 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 4464 7 257 nil nil 4096 nil 3 nil nil 1474 8 'Value' 587 #left 1362 #displayString 1824 1362 #<= 1824 802 0 nil 834 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 4608 7 257 nil nil 4096 nil 3 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2098 1 1 nil 2130 138 144 34 2 2194 #createAt:extent: 34 2 2098 9 53 2098 1769 481 4096 2194 #text: 34 1 8 'Name' 4096 2354 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2416 nil 27 722 754 8 'Original Source' 802 0 nil 834 4 1 800 8 'doIt' 8 '[Icon fromId: ''MethodHistory.ico'']' 8 #[32 105 45 30 178 106] 983558 ##(Smalltalk.VariableBinding) #Icon ##(Smalltalk.Icon) 8 'MethodHistory.ico' #fromId: 4976 7 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 5104 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 624 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2098 193 193 nil 5104 nil 8 1571274276 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5410 77 786694 ##(Smalltalk.IndexedColor) 33554457 5458 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 5424 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 5440 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 1152 170 192 34 2 #default 1641542 2 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 5104 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5458 33554447 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 5424 nil nil nil nil 130003 nil 170 192 34 6 1 1510470 3 ##(Smalltalk.ScintillaIndicatorStyle) 1 5104 196934 1 ##(Smalltalk.RGB) 65025 3 false 1 nil nil nil nil nil nil 3 5874 3 5104 5906 33423361 5 false 3 nil nil nil nil nil nil 5 5874 5 5104 5906 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Warning 5410 1027 5906 60239 5906 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5410 1031 5906 335 5906 31317503 1 nil nil nil nil #Error nil nil nil #Notification 5410 1029 nil 5906 31185883 1 nil nil nil nil #Notification nil nil nil nil 2130 138 144 34 8 2194 #createAt:extent: 34 2 2098 9 53 2098 1769 481 5104 2194 #textLimit: 8 #(1073741823) 5104 2194 #caretPeriod: 8 #(530) 5104 2194 #sciSetEOLMode: 8 #(2) 5104 2194 #wordWrap: 8 #(true) 5104 2194 #margins: 34 1 34 3 985158 3 ##(Smalltalk.ScintillaMargin) 1 5104 61 3 nil nil 5 nil 6450 3 5104 1 nil 3 67108863 5 nil 6450 5 5104 1 nil 3 -67108863 5 nil 5104 2194 #tabWidth: 8 #(4) 5104 2194 #maxCompletionListHeight: 8 #(9) 5104 2354 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2416 nil 29 722 754 8 'Class Definition' 802 0 nil 834 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''ClassDefinition.ico'']' 8 #[32 105 45 30 178 106] 5072 8 'ClassDefinition.ico' #fromId: 6672 7 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 6768 5170 nil false 5216 nil 624 nil 5 1202 nil true 34 11 1250 1 1282 #editSave 8 '&Save' 9383 1 nil nil nil 2658 4097 1250 1 1282 #editUndo 8 '&Undo' 9397 1 nil nil nil 1250 1 1282 #editRedo 8 '&Redo' 9395 1 nil nil nil 2658 4097 1250 1 1282 #editCut 8 'Cu&t' 9393 1 nil nil nil 1250 1 1282 #editCopy 8 '&Copy' 9351 1 nil nil nil 1250 1 1282 #editPaste 8 '&Paste' 9389 1 nil nil nil 1250 1 1282 #editDelete 8 '&Delete' 1629 1 nil nil nil 2658 4097 1250 1 1282 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 5234 nil true 5266 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2098 193 193 nil 6768 nil 8 1571274276 5346 nil nil 13 nil 170 192 34 4 #normal 5410 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5410 77 5472 5488 1 nil nil nil nil #callTip nil nil nil 34 256 7456 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 7472 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 1152 170 192 34 2 #default 5602 1 nil nil nil 6768 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5664 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 7456 nil nil nil nil 130003 nil 170 192 34 6 1 5874 1 6768 5906 65025 3 false 1 nil nil nil nil nil nil 3 5874 3 6768 5906 33423361 5 false 3 nil nil nil nil nil nil 5 5874 5 6768 5906 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Warning 5410 1027 5906 60239 5906 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5410 1031 5906 335 5906 31317503 1 nil nil nil nil #Error nil nil nil #Notification 5410 1029 nil 5906 31185883 1 nil nil nil nil #Notification nil nil nil nil 2130 138 144 34 9 2194 #createAt:extent: 34 2 2098 9 53 2098 1769 481 6768 2194 #contextMenu: 34 1 6848 6768 2194 #textLimit: 8 #(1073741823) 6768 2194 #caretPeriod: 8 #(530) 6768 2194 #sciSetEOLMode: 8 #(2) 6768 2194 #wordWrap: 8 #(true) 6768 2194 #margins: 34 1 34 3 6450 1 6768 1 3 nil nil 5 nil 6450 3 6768 33 nil 3 67108863 5 nil 6450 5 6768 1 nil 3 -67108863 5 nil 6768 2194 #tabWidth: 8 #(4) 6768 2194 #maxCompletionListHeight: 8 #(9) 6768 2354 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2416 nil 29 722 754 8 'Class Documentation' 802 0 nil 834 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''ClassComment.ico'']' 8 #[32 105 45 30 178 106] 5072 8 'ClassComment.ico' #fromId: 8608 7 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 8704 5170 nil false 5216 nil 624 nil 5 nil 5234 nil true 5266 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2098 193 193 nil 8704 nil 8 1571274276 5346 nil nil 11 nil 170 192 34 4 #normal 5410 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5410 77 5472 5488 1 nil nil nil nil #callTip nil nil nil 34 256 8912 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 8928 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 5602 1 nil nil nil 8704 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5664 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 8912 nil nil nil nil 130003 nil 170 192 34 6 1 5874 1 8704 5906 65025 3 false 1 nil nil nil nil nil nil 3 5874 3 8704 5906 33423361 5 false 3 nil nil nil nil nil nil 5 5874 5 8704 5906 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Warning 5410 1027 5906 60239 5906 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5410 1031 5906 335 5906 31317503 1 nil nil nil nil #Error nil nil nil #Notification 5410 1029 nil 5906 31185883 1 nil nil nil nil #Notification nil nil nil nil 2130 138 144 34 8 2194 #createAt:extent: 34 2 2098 9 53 2098 1769 481 8704 2194 #textLimit: 8 #(1073741823) 8704 2194 #caretPeriod: 8 #(530) 8704 2194 #sciSetEOLMode: 8 #(2) 8704 2194 #wordWrap: 8 #(true) 8704 2194 #margins: 34 1 34 3 6450 1 8704 1 3 nil nil 5 nil 6450 3 8704 33 nil 3 67108863 5 nil 6450 5 8704 1 nil 3 -67108863 5 nil 8704 2194 #tabWidth: 8 #(4) 8704 2194 #maxCompletionListHeight: 8 #(9) 8704 2354 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2416 nil 29 722 754 8 'Method Source' 802 0 nil 834 4 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[Icon fromId: ''MethodSource.ico'']' 8 #[32 105 45 30 178 106] 5072 8 'MethodSource.ico' #fromId: 9936 7 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 544 34 2 8 1445007428 1025 10032 5170 nil false 5216 nil 624 nil 5 nil 5234 nil true 5266 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 2098 193 193 nil 10032 nil 8 1571274276 5346 nil nil 13 nil 170 192 34 4 #normal 5410 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 5410 77 5472 5488 1 nil nil nil nil #callTip nil nil nil 34 256 10240 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 10256 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 5522 1 nil nil false 138 144 1152 170 192 34 2 #default 5602 1 nil nil nil 10032 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1152 nil 170 176 1152 9215 nil nil nil nil 5664 nil nil 65 nil nil 7632 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 10240 #callTip 10256 nil nil nil nil 130003 nil 170 192 34 14 1 5874 1 10032 5906 65025 3 false 1 nil nil nil nil nil nil #secondarySelection 5874 23 10032 nil 15 nil #secondarySelection 31 nil nil nil nil nil 3 5874 3 10032 5906 33423361 5 false 3 nil nil nil nil nil nil 8 'indicator10' 5874 21 10032 5458 33554459 3 nil nil nil nil nil nil nil nil 8 'indicator8' 5874 17 10032 5664 1 nil nil nil nil nil nil nil nil 8 'indicator9' 5874 19 10032 10624 13 nil nil nil nil nil nil nil nil 5 5874 5 10032 5906 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Warning 5410 1027 5906 60239 5906 31326207 1 nil nil nil nil #Warning nil nil nil #Error 5410 1031 5906 335 5906 31317503 1 nil nil nil nil #Error nil nil nil #Notification 5410 1029 nil 5906 31185883 1 nil nil nil nil #Notification nil nil nil nil 2130 138 144 34 8 2194 #createAt:extent: 34 2 2098 9 53 2098 1769 481 10032 2194 #textLimit: 8 #(1073741823) 10032 2194 #hoverTime: 8 #(200) 10032 2194 #caretPeriod: 8 #(530) 10032 2194 #sciSetEOLMode: 8 #(2) 10032 2194 #margins: 34 1 34 3 6450 1 10032 61 3 nil nil 5 nil 6450 3 10032 1 nil 3 67108863 nil nil 6450 5 10032 1 nil 3 -67108863 nil nil 10032 2194 #tabWidth: 8 #(4) 10032 2194 #maxCompletionListHeight: 8 #(9) 10032 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2416 nil 29 722 754 8 'SUnit' 802 0 nil 834 7 1 752 8 'doIt' 8 '(CardLabel text: ''SUnit'' iconBlock: [Icon fromId: ''TestRunner.ico''])' 8 #[45 30 35 113 47 32 180 106 195 105] 5058 #CardLabel 752 11376 5058 #Icon ##(Smalltalk.Icon) 8 'TestRunner.ico' #fromId: #text:iconBlock: 11392 11 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 544 34 2 8 1140850688 131073 11520 nil 624 nil 5 nil nil nil 11520 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 9 53 2098 1769 481 11520 2354 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 120 3 0 0 10 1 0 0] 1152 2416 nil 27 10032 170 192 34 14 8704 8 'classDocumentation' 5104 8 'originalSource' 10032 8 'methodSource' 4096 8 'globals' 928 8 'packageInfo' 6768 8 'classDefinition' 11520 8 'sunit' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 544 34 2 8 1140916736 1 11904 1106 138 144 34 7 768 3968 6640 8576 4944 9904 11360 nil 1184 nil nil 1 nil nil nil 11904 nil 8 1778960048 802 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 12032 7 257 nil 802 0 nil 12050 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 12096 7 257 nil 1424 nil nil nil nil nil #smallIcons nil nil nil nil nil 2130 138 144 34 3 2194 #createAt:extent: 34 2 2098 1 1 2098 1785 541 11904 2194 #basicSelectionsByIndex: 34 1 8 #(6) 11904 2194 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 11904 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 14 1 0 0] 8 #() 2416 nil 27 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1 549 2098 1785 541 544 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 1 0 0 124 3 0 0 32 2 0 0] 34 8 928 4096 6768 8704 5104 10032 11520 11904 2416 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 12576 nil nil nil 5 nil nil nil 12576 482 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 12576 34 2 8 1140850688 131073 12688 nil nil nil 5 nil nil nil 12688 482 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 12688 34 2 8 1140850688 131073 12800 nil nil nil 5 nil nil nil 12800 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 12800 34 2 8 1409286144 131073 12928 nil 624 nil 5 nil nil nil 12928 658 138 144 34 2 722 754 8 'Classes' 802 0 nil 834 4 1 800 8 'doIt' 8 '[Icon fromId: ''ClassDefinition.ico'']' 8 #[32 105 45 30 178 106] 5072 8 'ClassDefinition.ico' #fromId: 13088 7 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 12928 34 2 8 1409372745 1025 13184 1106 138 144 1152 nil 1184 624 nil 29 nil nil nil 13184 nil 8 1778896832 1362 #displayString 8 #() nil 1424 nil nil nil nil nil nil 138 144 34 2 1474 8 'Column 1' 359 #left 802 0 nil 834 5 1 800 8 'doIt' 8 '[:each | (each subStrings: Character space) first]' 8 #[33 105 17 29 159 178 161 106] ##(Smalltalk.Character) #space #subStrings: #first 13392 7 257 nil ##(Smalltalk.SortedCollection) nil nil 13184 nil 3 nil nil 1474 8 '' 69 #left 802 0 nil 834 6 1 800 8 'doIt' 8 '[:each | | index |
	index := each indexOf: Character space.
	0 < index ifTrue: [each copyFrom: index + 2 to: each size - 1] ifFalse: ['''']]' 8 #[34 105 17 29 159 178 90 62 18 128 221 9 233 1 64 126 17 145 99 193 106 33 106] ##(Smalltalk.Character) #space #indexOf: #copyFrom:to: 13488 13504 7 65793 nil 1362 #<= 8 #() nil nil 13184 nil 1 nil nil #report 1152 nil 131169 nil 34 4 nil nil 2098 1 1 nil 2130 138 144 34 3 2194 #createAt:extent: 34 2 2098 9 53 2098 435 435 13184 2194 #text: 34 1 8 'Column 1' 13184 2194 #columnOrder: 34 1 8 #(2 1) 13184 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 221 0 0 0 243 0 0 0] 8 #() 2416 nil 27 722 754 8 'Hierarchy' 802 0 nil 834 4 1 800 8 'doIt' 8 '[Icon fromId: ''ClassBrowserShell.ico'']' 8 #[32 105 45 30 178 106] 5072 8 'ClassBrowserShell.ico' #fromId: 13952 7 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 12928 34 2 8 1409352231 1025 14048 590918 3 ##(Smalltalk.TreeModel) nil 1184 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 1152 624 nil 29 nil nil nil 14048 nil 8 1778660272 802 0 nil 834 2 1 1360 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 14208 7 257 nil ##(Smalltalk.IconicListAbstract) 1424 nil nil nil nil nil 170 176 1152 33 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 1152 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 9 53 2098 435 435 14048 2354 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 221 0 0 0 243 0 0 0] 8 #() 2416 nil 27 13184 170 192 34 4 14048 8 'classHierarchy' 13184 8 'classList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 12928 34 2 8 1140916736 1 14544 1106 138 144 34 2 13056 13920 nil 1184 nil nil 1 nil nil nil 14544 nil 8 1778960048 802 0 nil 12050 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 14672 7 257 nil 802 0 nil 12050 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 14720 7 257 nil 1424 nil nil nil nil nil #smallIcons nil nil nil nil nil 2130 138 144 34 3 2194 #createAt:extent: 34 2 2098 1 1 2098 451 495 14544 2194 #basicSelectionsByIndex: 34 1 8 #(1) 14544 2194 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 14544 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 247 0 0 0] 8 #() 2416 nil 27 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1 1 2098 451 495 12928 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 247 0 0 0] 34 3 13184 14048 14544 2416 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 15202 #fixedParentRight 1 15202 #fixedParentTop 1 15202 #fixedParentBottom -43 410 ##(Smalltalk.CardContainer) 34 16 nil 12800 34 2 8 1409286144 131073 15280 nil 624 nil 5 nil nil nil 15280 658 138 144 34 2 722 754 8 'Instance' 802 0 nil 834 4 1 800 8 'doIt' 8 '[Icon fromId: ''Object.ico'']' 8 #[32 105 45 30 178 106] 5072 8 'Object.ico' #fromId: 15440 7 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 15280 34 2 8 1140850688 131073 15536 nil nil nil 5 nil nil nil 15536 nil 170 192 1152 nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 9 9 2098 435 1 15536 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 221 0 0 0 4 0 0 0] 8 #() 2416 nil 27 722 754 8 'Class' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 15808 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 15824 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 15280 34 2 8 1140850688 131073 15920 nil nil nil 5 nil nil nil 15920 nil 170 192 1152 nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 9 9 2098 435 1 15920 2354 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 221 0 0 0 4 0 0 0] 8 #() 2416 nil 27 15536 170 192 34 4 15536 8 'instanceTab' 15920 8 'classTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 15280 34 2 8 1140916738 1 16224 1106 138 144 34 2 15408 15792 nil 1184 nil nil 1 nil nil nil 16224 nil 8 1778960048 802 0 nil 12050 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 16352 7 257 nil 802 0 nil 12050 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 16400 7 257 nil 1424 nil nil nil nil nil #smallIcons nil nil nil nil nil 2130 138 144 34 3 2194 #createAt:extent: 34 2 2098 1 1 2098 451 51 16224 2194 #basicSelectionsByIndex: 34 1 8 #(1) 16224 2194 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 16224 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 25 0 0 0] 8 #() 2416 nil 27 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1 489 2098 451 51 15280 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 225 0 0 0 13 1 0 0] 34 3 15536 15920 16224 2416 nil 27 15170 15216 1 15232 1 15264 -49 15202 #fixedViewTop 51 170 192 34 4 12928 8 'classHierarchyTabs' 15280 8 'instanceClassTabs' nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 615 1 2098 451 539 12800 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 51 1 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 2 12928 15280 2416 nil 27 13 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 12688 34 2 8 1409286144 131073 17104 nil 624 nil 517 nil nil nil 17104 658 138 144 34 2 722 754 8 'Packages' 802 0 nil 834 7 1 752 8 'doIt' 8 '(CardLabel text: ''Packages'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 17248 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 17264 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 17104 34 2 8 1140850688 131073 17392 nil 624 nil 5 nil nil nil 17392 17168 170 192 34 4 17360 8 'packageList' 410 ##(Smalltalk.ListView) 34 30 nil 17392 34 2 8 1409372233 1025 17504 1106 138 144 1152 nil 1184 624 nil 21 nil nil nil 17504 nil 8 1778896832 1362 #displayString 8 #() nil 1424 nil nil nil nil nil nil 138 144 34 1 1474 8 'Column 1' 239 #left 1362 #displayString 17632 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 17728 7 257 nil nil 17504 nil 3 nil 802 0 nil 834 5 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] #item #font #beBold #font: 17808 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2098 1 1 nil 2130 138 144 34 2 2194 #createAt:extent: 34 2 2098 1 37 2098 247 487 17504 2194 #text: 34 1 8 'Column 1' 17504 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 123 0 0 0 5 1 0 0] 8 #() 2416 nil 27 8 'dictionaryList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 17104 34 2 8 1140916864 1 18144 1106 138 144 34 2 17232 754 8 'Dictionaries' 802 0 nil 834 7 1 16 8 'doIt' 8 '(CardLabel text: ''Dictionaries'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 752 18272 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 18288 11 1 nil nil nil 1184 624 nil 1 nil nil nil 18144 nil 8 1778960048 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1424 nil nil nil nil nil #smallIcons nil nil nil nil nil 2130 138 144 34 3 2194 #createAt:extent: 34 2 2098 1 1 2098 311 539 18144 2194 #basicSelectionsByIndex: 34 1 8 #(2) 18144 2194 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 18144 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 155 0 0 0 13 1 0 0] 8 #() 2416 nil 27 nil 17104 2098 33 33 1050182 1 ##(Smalltalk.ButtonInteractor) 17392 nil 1 590342 ##(Smalltalk.Rectangle) 2098 211 3 2098 243 35 nil 1282 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 57 9 2098 247 523 17392 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 28 0 0 0 4 0 0 0 151 0 0 0 9 1 0 0] 34 2 17360 17504 2416 nil 27 34 2 8 1409405001 1025 17360 1106 138 144 1152 nil 1184 624 nil 5 nil nil nil 17360 nil 8 1778896832 1362 #displayString 8 #() nil 1424 nil nil nil nil nil nil 138 144 34 1 1474 8 'Column 1' 239 #left 1362 #displayString 19040 ##(Smalltalk.SortedCollection) 802 0 nil 834 2 1 800 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 19136 7 257 nil nil 17360 nil 3 nil 802 0 nil 834 6 1 800 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[34 105 226 0 142 221 9 17 226 1 100 160 97 161 180 106 60 106] #item #font #beBold #beItalic #font: 19216 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 13632 nil 2130 138 144 34 2 2194 #createAt:extent: 34 2 2098 1 37 2098 247 487 17360 2194 #text: 34 1 8 'Column 1' 17360 2354 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 123 0 0 0 5 1 0 0] 8 #() 2416 nil 27 722 18256 17504 17504 170 192 34 2 17392 8 'packageDictionaryTabs' nil 18144 17392 2098 201 201 401 1 31 nil nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1 1 2098 311 539 17104 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 155 0 0 0 13 1 0 0] 34 2 17392 18144 2416 nil 27 12546 2481 595 410 ##(Smalltalk.ContainerView) 34 15 nil 12688 34 2 8 1140850688 131073 19776 nil nil nil 5 nil nil nil 19776 482 170 176 1152 false 170 192 34 2 410 ##(Smalltalk.TreeView) 34 27 nil 19776 34 2 8 1409352231 1025 19904 14114 nil 1184 14146 nil nil nil 170 192 1152 624 nil 21 nil nil nil 19904 nil 8 1778660272 802 0 nil 834 5 1 800 8 'doIt' 8 '[:each | each key isEmpty ifTrue: [''--All Categories--''] ifFalse: [each key last]].' 8 #[33 105 226 0 159 119 31 106 226 0 161 106] #key #isEmpty 8 '--All Categories--' #last 20032 7 257 nil ##(Smalltalk.IconicListAbstract) 1424 nil nil nil nil nil 170 176 1152 33 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 1152 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1 1 2098 285 539 19904 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 142 0 0 0 13 1 0 0] 8 #() 2416 nil 27 8 'classCategoryList' nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 321 1 2098 285 539 19776 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 0 0 0 0 46 1 0 0 13 1 0 0] 34 1 19904 2416 nil 27 12546 2273 595 false 170 192 1152 nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1 1 2098 1065 539 12688 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 5 17104 410 ##(Smalltalk.Splitter) 34 12 nil 12688 34 2 8 1140850688 1 20688 nil 624 nil 517 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 20688 nil 1 #left nil nil nil 2098 1 1 2098 9 9 nil 20784 nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 311 1 2098 11 539 20688 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 0 0 0 0 0 0 0 160 0 0 0 13 1 0 0] 8 #() 2416 nil 27 19776 410 ##(Smalltalk.Splitter) 34 12 nil 12688 34 2 8 1140850688 1 20976 nil 624 nil 517 nil nil nil 20754 20976 nil 1 #left nil nil nil 20784 20800 nil 20784 nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 605 1 2098 11 539 20976 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 46 1 0 0 0 0 0 0 51 1 0 0 13 1 0 0] 8 #() 2416 nil 27 12800 2416 nil 27 7 410 ##(Smalltalk.ContainerView) 34 15 nil 12576 34 2 8 1140850688 131073 21216 nil nil nil 5 nil nil nil 21216 482 170 176 1152 false 170 192 1152 nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1075 1 2098 711 539 21216 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 2 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 21216 34 2 8 1409286144 131073 21488 nil 624 nil 517 nil nil nil 21488 658 138 144 34 3 722 8 'Variables' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 21488 34 2 8 1140850688 131073 21664 nil 624 nil 5 nil nil nil 21664 21552 170 192 34 6 21632 8 'variableList' 410 ##(Smalltalk.ListView) 34 30 nil 21664 34 2 8 1409372233 1025 21776 1106 138 144 1152 nil 1184 624 nil 21 nil nil nil 21776 nil 8 1778896832 1362 #displayString 8 #() nil 1424 nil nil nil nil nil nil 138 144 34 1 1474 8 'Column 1' 279 #left 1362 #displayString 21904 ##(Smalltalk.SortedCollection) nil nil 21776 nil 3 nil 802 0 nil 834 9 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 22000 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2098 1 1 nil 2130 138 144 34 2 2194 #createAt:extent: 34 2 2098 1 37 2098 287 487 21776 2194 #text: 34 1 8 'Column 1' 21776 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 143 0 0 0 5 1 0 0] 8 #() 2416 nil 27 8 'categoryList' 410 ##(Smalltalk.ListView) 34 30 nil 21664 34 2 8 1409372233 1025 22336 1106 138 144 1152 nil 1184 624 nil 21 nil nil nil 22336 nil 8 1778896832 1362 #displayString 8 #() nil 1424 nil nil nil nil nil nil 138 144 34 1 1474 8 'Column 1' 279 #left 1362 #displayString 22464 ##(Smalltalk.SortedCollection) nil nil 22336 nil 3 nil 802 0 nil 834 9 1 800 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 22560 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2098 1 1 nil 2130 138 144 34 2 2194 #createAt:extent: 34 2 2098 1 37 2098 287 487 22336 2194 #text: 34 1 8 'Column 1' 22336 2354 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 143 0 0 0 5 1 0 0] 8 #() 2416 nil 27 8 'pragmaList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 21488 34 2 8 1140916864 1 22896 1106 138 144 34 3 754 8 'Categories' 802 0 nil 834 7 1 752 8 'doIt' 8 '(CardLabel text: ''Categories'' iconBlock: [Icon fromId: ''Category.ico''])' 8 #[45 30 35 113 47 32 180 106 195 105] 5058 #CardLabel 752 23024 5072 8 'Category.ico' #fromId: #text:iconBlock: 23040 11 1 nil nil 8 'Pragmas' 21616 nil 1184 624 nil 1 nil nil nil 22896 nil 8 1778960048 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1424 nil nil nil nil nil #noIcons nil nil nil nil nil 2130 138 144 34 3 2194 #createAt:extent: 34 2 2098 1 1 2098 351 539 22896 2194 #basicSelectionsByIndex: 34 1 8 #(1) 22896 2194 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 22896 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 8 #() 2416 nil 27 nil 21488 2098 33 33 18658 21664 21664 1 18690 2098 251 3 2098 283 35 nil 1282 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 57 9 2098 287 523 21664 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 28 0 0 0 4 0 0 0 171 0 0 0 9 1 0 0] 34 3 21776 22336 21632 2416 nil 27 34 2 8 1409372233 1025 21632 1106 138 144 1152 nil 1184 624 nil 21 nil nil nil 21632 nil 8 1778896832 1362 #displayString 22464 nil 1424 nil nil nil nil nil nil 138 144 34 1 1474 8 'Column 1' 279 #left 1362 #displayString 22464 ##(Smalltalk.SortedCollection) nil nil 21632 nil 3 nil 802 0 nil 834 9 1 800 22592 22608 22624 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 23872 7 257 nil #report 1152 nil 131169 nil 34 4 nil nil 2098 1 1 nil 2130 138 144 34 2 2194 #createAt:extent: 34 2 2098 1 37 2098 287 487 21632 2194 #text: 34 1 8 'Column 1' 21632 2354 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 143 0 0 0 5 1 0 0] 8 #() 2416 nil 27 722 23152 22336 722 23008 21776 21776 170 192 34 2 21664 8 'methodFilterTabs' nil 22896 21664 2098 201 201 401 1 31 nil nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1 1 2098 351 539 21488 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 34 2 21664 22896 2416 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 21216 34 2 8 1140850688 1 24400 nil 624 nil 517 nil nil nil 20754 24400 nil 1 #left nil nil nil 20784 20800 nil 20784 nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 351 1 2098 11 539 24400 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 0 0 0 0 180 0 0 0 13 1 0 0] 8 #() 2416 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 21216 34 2 8 1140850688 131073 24640 nil nil nil 5 nil nil nil 24640 12866 170 176 34 6 410 ##(Smalltalk.ListView) 34 30 nil 24640 34 2 8 1409372233 1025 24752 1106 138 144 1152 nil 1184 624 nil 13 nil nil nil 24752 nil 8 1778896832 1362 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1424 nil nil nil nil nil nil 138 144 34 2 1474 8 'Column 1' 311 #left 1362 #displayString 24880 ##(Smalltalk.SortedCollection) 802 0 nil 834 1 83886081 800 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 24976 7 257 nil nil 24752 nil 3 nil 802 0 nil 834 5 1 800 8 'doIt' 8 '[:each | (each item at: 4) ifTrue: [each font: each font beItalic]]' 8 #[33 105 226 0 214 4 148 123 17 226 1 160 179 106 60 106] #item #font #beItalic #font: 25056 7 257 nil 1474 8 '' 33 #center 2194 #empty 8 #() 16 1362 #<= 8 #() nil nil 24752 802 0 nil 834 6 1 800 8 'doIt' 8 '[:each | ((each at: 2) ifTrue: [JadeSystemBrowserPresenter overriddenIcon] ifFalse: [Icon blank]) 
	imageIndex]' 8 #[34 105 17 64 148 120 29 159 111 31 161 162 106] ##(Smalltalk.JadeSystemBrowserPresenter) #overriddenIcon ##(Smalltalk.Icon) #blank #imageIndex 25232 7 257 nil 1 nil nil #report 1152 nil 131173 nil 34 4 nil nil 13632 nil 2130 138 144 34 3 2194 #createAt:extent: 34 2 2098 1 47 2098 351 447 24752 2194 #text: 34 1 8 'Column 1' 24752 2194 #columnOrder: 34 1 8 #(2 1) 24752 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 175 0 0 0 246 0 0 0] 8 #() 2416 nil 27 15170 15216 1 15232 1 15248 47 15264 -45 410 ##(Smalltalk.ComboBox) 34 17 nil 24640 34 2 8 1412498947 1025 25600 1106 138 144 1152 nil 1184 624 nil 5 nil nil nil 25600 nil 8 1778687536 1362 #displayString 8 #() 1152 401 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1 1 2098 351 51 25600 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 25 0 0 0] 8 #() 2416 nil 27 15170 15216 1 15232 1 15248 1 16864 47 410 ##(Smalltalk.ComboBox) 34 17 nil 24640 34 2 8 1412498947 1025 25920 1106 138 144 1152 nil 1184 624 nil 5 nil nil nil 25920 nil 8 1778687536 1362 #displayString 8 #() 1152 401 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1 493 2098 351 51 25920 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 246 0 0 0 175 0 0 0 15 1 0 0] 8 #() 2416 nil 27 15170 15216 1 15232 1 15264 -45 16864 47 170 192 34 6 24752 8 'methodList' 25600 8 'superclassList' 25920 8 'overrideList' nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 361 1 2098 351 539 24640 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 99 1 0 0 13 1 0 0] 34 3 25600 24752 25920 2416 nil 27 2416 nil 27 5 false 170 192 1152 nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1 1 2098 1785 539 12576 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 12688 410 ##(Smalltalk.Splitter) 34 12 nil 12576 34 2 8 1140850688 1 26656 nil 624 nil 517 nil nil nil 20754 26656 nil 1 #left nil nil nil 20784 20800 nil 20784 nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1065 1 2098 11 539 26656 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 2 0 0 0 0 0 0 25 2 0 0 13 1 0 0] 8 #() 2416 nil 27 21216 2416 nil 27 12546 3 5 true 170 192 34 4 544 8 'textAreaTabs' 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 26960 nil 624 nil 517 nil nil nil 20754 26960 nil 1 #left nil nil nil 20784 20800 nil 20784 nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 1 539 2098 1785 11 26960 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 13 1 0 0 124 3 0 0 18 1 0 0] 8 #() 2416 nil 27 8 'splitter' nil 2130 138 144 34 1 2194 #createAt:extent: 34 2 2098 3839 21 2098 1785 1089 416 2354 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 251 10 0 0 42 2 0 0] 34 3 12576 26960 544 2416 nil 27 )! !
!JadeSystemBrowserPresenter class categoriesFor: #overriddenIcon!public! !
!JadeSystemBrowserPresenter class categoriesFor: #resource_Default_view!private!resources-views! !

