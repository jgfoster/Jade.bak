"Filed out from Dolphin Smalltalk 7"!

JadeBrowserPresenter subclass: #JadeSystemBrowserPresenter
	instanceVariableNames: 'ancestorListPresenter breakPoints categoryListPresenter categoryVariableTabs classCategoryPresenter classCommentPresenter classDefinition classDefinitionPresenter classHierarchyPresenter classHierarchyTabs classListPresenter dictionaryListPresenter environment eventCount globalsPresenter ignoreNextSetFocusEvent instanceClassTabs inUpdate keystrokeTime methodCategory methodListPresenter methodSource methodSourcePresenter originalSourcePresenter overrideListPresenter packageDictionaryTabs packageInfoTab packageListPresenter readStream repositoryListPresenter selectedClassChanged selectedClassesAreTestCases selectedClassName selectedClassOop stepPoints superclassListPresenter textAreaTabs unimplementedSelectors updateCount updateProcess variableListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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
			nextPutAll: 'categoryList'; tab;
			print: environment; lf;
			lf;	"method filter selections"
			lf; "overrideList selection"
			yourself.
		^self.
	].
	string := superclassListPresenter selectionIfNone: [''].
	aStream 
		nextPutAll: instanceClassTabs currentCard name; lf;			"instanceTab or classTab"
		nextPutAll: string; lf;															"selected superclass"
		nextPutAll: categoryVariableTabs currentCard name; tab;		"categoryList or variableList"
		print: environment; lf;															"environment"
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
	(JadeMethodHistoryBrowser showOnSession: gciSession)
		setContents: readStream;
		yourself.
!

browseMethodsAndSelect: aString

	(JadeMethodListBrowser showOnSession: self gciSession) primaryPresenter
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

browseSelectedClass

	| range string list assoc |
	range := methodSourcePresenter view selectionRange.
	string := methodSourcePresenter value copyFrom: range start to: range stop.
	list := self findClassList.
	assoc := list 
		detect: [:each | each key = string]
		ifNone: [^MessageBox warning: 'Class ' , string printString , ' not found!!' caption: 'Jade'].
	parentPresenter parentPresenter addSystemBrowserForClass: assoc value.
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
		'Set Compiler &Environment ...//setEnvironment'
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

	ancestorListPresenter 	:= self add: ListPresenter		new 		name: 'ancestorList'.
	categoryListPresenter	:= self add: ListPresenter		new 		name: 'categoryList'.
	classCategoryPresenter	:= self add: TreePresenter		new 		name: 'classCategoryList'.
	classCommentPresenter 	:= self add: TextPresenter 	new 		name: 'classDocumentation'.
	classDefinitionPresenter	:= self add: JadeTextPresenter 	new 		name: 'classDefinition'.
	classHierarchyPresenter	:= self add: TreePresenter		new 		name: 'classHierarchy'.
	classListPresenter		:= self add: ListPresenter		new 		name: 'classList'.
	dictionaryListPresenter 	:= self add: ListPresenter 		new 		name: 'dictionaryList'.
	globalsPresenter		:= self add: ListPresenter		new 		name: 'globals'.
	methodListPresenter		:= self add: ListPresenter		new 		name: 'methodList'.
	methodSourcePresenter	:= self add: self newMethodPresenter 	name: 'methodSource'.
	originalSourcePresenter	:= self add: self newMethodPresenter 	name: 'originalSource'.
	overrideListPresenter	:= self add: ListPresenter		new 		name: 'overrideList'.
	packageListPresenter 	:= self add: ListPresenter		new 		name: 'packageList'.
	repositoryListPresenter	:= self add: ListPresenter		new 		name: 'repositoryList'.
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
		when: #'hoverStart:'						send: #'methodHoverStart:'			to: self;
		when: #'hoverEnd:'						send: #'methodHoverEnd:'				to: self;
		when: #'aboutToDisplayMenu:'		send: #'methodMenu:'					to: self;
		when: #'leftButtonDoubleClicked:'	send: #'methodDoubleClicked:'		to: self;
		when: #'valueChanged'					send: #'methodValueChanged'		to: self;
		when: #'focusLost'						send: #'cancelCallTip'					to: methodSourcePresenter view;
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
		'Display/Ctrl+D/jadeDisplay'
		'Execute/Ctrl+E/jadeExecute'
		'Inspect/Ctrl+Q/jadeInspect'
		'Browse Class/Ctrl+B/browseSelectedClass'
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
		(JadeWorkspace showOnSession: self gciSession)
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

	| header file newSource index |
	newSource := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: 'fileOutClass' , Character tab asString , self behaviorIdentifier.
	index := newSource indexOf: Character lf.
	newSource := newSource copyFrom: index + 1 to: newSource size.
	header := self stuffToKeepFromPath: aString andAddTo: newSource.
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
	string := self gciSession 
		serverPerform: #'systemBrowser:' 
		with: 'findClass'.
	list := (string subStrings: Character lf) collect: [:each | each subStrings: Character tab].
	list := list copyFrom: 2 to: list size.
	list := list collect: [:each | each size < 3 ifTrue: [each , #('' '' '')] ifFalse: [each]].
	list := list collect: [:each | (each at: 1) -> each].
	^list
!

getViews

	packageDictionaryTabs 	:= self view viewNamed: 'packageDictionaryTabs'.
	classHierarchyTabs 		:= self view viewNamed: 'classHierarchyTabs'.
	categoryVariableTabs 	:= self view viewNamed: 'categoryVariableTabs'.
	instanceClassTabs		:= self view viewNamed: 'instanceClassTabs'.
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
	environment := 0.
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

isClassSelectedInEditor

	| range string |
	(range := methodSourcePresenter view selectionRange) isEmpty ifTrue: [^false].
	string := methodSourcePresenter value copyFrom: range start to: range stop.
	^(string allSatisfy: [:each | each isAlphaNumeric]) and: [string first isLetter and: [string first isUppercase]]
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
		self browseImplementors; browseImplementorsOf; browseSenders; browseSendersOf; browseMethodsContaining; removeMethods; runMethodTests; setEnvironment.
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
		'Set Compiler &Environment ...//setEnvironment'
		'Run &Tests//runMethodTests'
	).
!

methodSourcePresenter

	^methodSourcePresenter.
!

methodValueChanged

	inUpdate ifTrue: [^self].
	methodSourcePresenter value = methodSource ifTrue: [
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
	self updateCommand: stream contents.
!

onDropOnPackageList: aSession 

	(aSession isFormatAvailable: #'class') 	ifTrue: [^self onDropClassesOnPackageList: aSession].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aSession printString , '!!'.
	SessionManager current pause.!

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
		eventCount = gciSession eventCount ifTrue: [^self].
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
	JadeTextStyles default textStyles: textStyles.
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
	(#(#'browseSelectedClass') includes: command) ifTrue: [aCommandQuery isEnabled: self isClassSelectedInEditor. ^true].
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

setEnvironment

	| answer |
	answer := Prompter 
		on: environment printString
		prompt: 'Set method compiler environment to: '
		caption: 'Jade'.
	answer ifNil: [^self].
	environment := answer asNumber.
	self updateCommand: self requestStringForUpdate.
!

setHomeDictionary

	| string |
	string := 'setHomeDictionary' , Character tab asString , dictionaryListPresenter selection key , Character lf asString , self requestString.
	self updateCommand: string.
!

setSearchPolicy

	packageListPresenter 	view model searchPolicy: SearchPolicy equality.
	dictionaryListPresenter 	view model searchPolicy: SearchPolicy equality.
	classCategoryPresenter	view model searchPolicy: SearchPolicy equality.
	classListPresenter		view model searchPolicy: SearchPolicy equality.
	classHierarchyPresenter	view model searchPolicy: SearchPolicy equality.
	categoryListPresenter 	view model searchPolicy: SearchPolicy equality.
	variableListPresenter	 	view model searchPolicy: SearchPolicy equality.
	superclassListPresenter	view model searchPolicy: SearchPolicy equality.
	methodListPresenter		view model searchPolicy: SearchPolicy equality.
	overrideListPresenter	view model searchPolicy: SearchPolicy equality.
	ancestorListPresenter 	view model searchPolicy: SearchPolicy equality.
	repositoryListPresenter	view model searchPolicy: SearchPolicy equality.
	globalsPresenter		view model searchPolicy: SearchPolicy equality.
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

stuffToKeepFromPath: pathString andAddTo: newSource

	| file existingSource i j string existingHeader newHeader x |
	[
		file := FileStream
			read: pathString
			text: true.
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

	| isReadOnly newSource warnings |
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
	"unimplemented selectors"
	unimplementedSelectors := Dictionary new.
	self nextLineAsList do: [:eachPair | 
		| pieces |
		pieces := eachPair subStrings.
		unimplementedSelectors at: pieces first asNumber put: pieces last.
	].
	"step points"
	stepPoints := self nextLineAsList collect: [:each |
		| pieces offset selector | 
		pieces := each subStrings.
		offset := pieces first asNumber.
		selector := (2 <= pieces size ifTrue: [pieces at: 2] ifFalse: ['']).
		(offset to: 0) -> selector.
	].
	"breaks"
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
	"method category"
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
	"original source"
	originalSourcePresenter value: self nextParagraph.
	"compiler warnings"
	(warnings := self nextParagraph) notEmpty ifTrue: [
		MessageBox warning: warnings caption: 'Jade Compile Warning'.
	].
	!

updateMethodFilter

	| listPresenter pieces type tabs tab filters selections |
	listPresenter := self methodFilterListPresenter.
	pieces := self nextLine subStrings: Character tab.
	type := pieces at: 1.
	1 < pieces size ifTrue: [environment := (pieces at: 2) asNumber].
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
	fullList := (self nextParagraph subStrings: Character lf) collect: [:each | (each subStrings: Character tab) , #('' '' '' '' '')].
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
			updateTabLabel;
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
!JadeSystemBrowserPresenter categoriesFor: #browseSelectedClass!menu handlers!public! !
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
!JadeSystemBrowserPresenter categoriesFor: #findClassList!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #getViews!public! !
!JadeSystemBrowserPresenter categoriesFor: #globalsMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #handleInvalidSession!public! !
!JadeSystemBrowserPresenter categoriesFor: #horizontalSplitter!public! !
!JadeSystemBrowserPresenter categoriesFor: #initialize!public! !
!JadeSystemBrowserPresenter categoriesFor: #insertDictionary!public! !
!JadeSystemBrowserPresenter categoriesFor: #inspectGlobal!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isCategoriesTabSelected!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #isClassListTabSelected!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #isClassSelectedInEditor!public! !
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
!JadeSystemBrowserPresenter categoriesFor: #methodDoubleClicked:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodFilterListPresenter!public!request string! !
!JadeSystemBrowserPresenter categoriesFor: #methodHoverEnd:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodHoverStart:!event handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodMenu:!event handlers!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodMenuBreak:!event handlers!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodsIdentifier!menu handlers!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodsMenuStrings!menus!public! !
!JadeSystemBrowserPresenter categoriesFor: #methodSourcePresenter!public!updating! !
!JadeSystemBrowserPresenter categoriesFor: #methodValueChanged!event handlers!public! !
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
!JadeSystemBrowserPresenter categoriesFor: #setEnvironment!public! !
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ContainerView) 98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 265030 4 ##(Smalltalk.Menu) 0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) 8 #savePackage 8 '&Save' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout) 234 240 98 4 410 432 98 15 0 416 98 2 8 1140850688 131073 720 0 0 0 5 0 0 0 720 658 234 240 98 4 410 432 98 15 0 720 98 2 8 1140850688 131073 832 0 0 0 5 0 0 0 832 658 234 240 98 0 32 234 256 928 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 1 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point) 1075 1 1090 711 539 832 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 2 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 98 3 410 8 ##(Smalltalk.SlideyInneyOuteyThing) 98 23 0 832 98 2 8 1409286144 131073 1200 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 517 0 0 0 1200 655878 ##(Smalltalk.CardLayout) 202 208 98 2 721414 ##(Smalltalk.Association) 8 'Categories' 410 8 ##(Smalltalk.ListView) 98 30 0 410 8 ##(Smalltalk.SlidingCardTray) 98 22 0 1200 98 2 8 1140850688 131073 1488 0 1282 1312 0 5 0 0 0 1488 1344 234 256 98 4 1440 8 'categoryList' 410 1456 98 30 0 1488 98 2 8 1409372233 1025 1632 590662 2 ##(Smalltalk.ListModel) 202 208 928 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 1282 8 4278190080 0 21 0 0 0 1632 0 8 4294904239 459270 ##(Smalltalk.Message) 8 #displayString 98 0 0 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 283 8 #left 1826 1856 1872 8 ##(Smalltalk.SortedCollection) 0 0 1632 0 3 0 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 9 1 2048 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] 8 #item 8 #first 8 ##(Smalltalk.Character) 8 #space 8 #font 8 #beBold 8 #beItalic 8 #font: 2064 7 257 0 8 #report 928 0 131169 0 98 1 0 962 202 208 98 2 1026 1056 98 2 1090 1 37 1090 291 487 1632 1026 8 #text: 98 1 8 'Column 1' 1632 1138 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 98 0 1090 193 193 0 27 8 'variableList' 0 410 8 ##(Smalltalk.TabViewXP) 98 28 0 1200 98 2 8 1140916864 1 2576 1698 202 208 98 2 1424 8 'Variables' 0 1760 1282 1312 0 1 0 0 0 2576 0 8 4294904353 8 ##(Smalltalk.BasicListAbstract) 8 ##(Smalltalk.IconicListAbstract) 1904 0 0 0 0 0 8 #noIcons 0 0 0 0 0 962 202 208 98 3 1026 1056 98 2 1090 1 1 1090 351 539 2576 1026 8 #basicSelectionsByIndex: 98 1 98 1 3 2576 1026 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 2576 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 98 0 2544 0 27 0 1200 1090 33 33 1049862 ##(Smalltalk.ButtonInteractor) 1488 0 590342 ##(Smalltalk.Rectangle) 1090 255 3 1090 287 35 1 578 8 #togglePin 8 'Pin or Unpin the tray' 1 1 0 0 0 962 202 208 98 1 1026 1056 98 2 1090 53 9 1090 291 523 1488 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 171 0 0 0 9 1 0 0] 98 2 1440 1632 2544 0 27 98 2 8 1409372233 1025 1440 1698 202 208 928 0 1760 1282 1792 0 21 0 0 0 1440 0 8 4294904239 1826 1856 98 0 0 1904 0 0 0 0 0 0 202 208 98 1 1954 8 'Column 1' 283 2000 1826 1856 3504 2032 0 0 1440 0 3 0 2050 0 0 2082 9 1 8 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] 2160 2176 2192 2208 2224 2240 2256 2272 3600 7 257 0 2288 928 0 131169 0 98 1 0 962 202 208 98 2 1026 1056 98 2 1090 1 37 1090 291 487 1440 1026 2448 98 1 8 'Column 1' 1440 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 98 0 2544 0 27 1394 2704 1632 1440 234 256 98 2 1488 8 'categoryVariableTabs' 0 2576 1488 1090 201 201 401 1 31 0 0 962 202 208 98 1 1026 1056 98 2 1090 1 1 1090 351 539 1200 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 98 2 1488 2576 2544 0 27 410 8 ##(Smalltalk.Splitter) 98 12 0 832 98 2 8 1140850688 1 4160 0 1282 8 4278190080 0 517 0 0 0 4160 962 202 208 98 1 1026 1056 98 2 1090 351 1 1090 11 539 4160 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 0 0 0 0 180 0 0 0 13 1 0 0] 98 0 2544 0 27 410 432 98 15 0 832 98 2 8 1140850688 131073 4432 0 0 0 5 0 0 0 4432 852230 ##(Smalltalk.FramingLayout) 234 240 98 6 410 8 ##(Smalltalk.ComboBox) 98 17 0 4432 98 2 8 1412498947 1025 4560 1698 202 208 928 0 1760 1282 8 4278190080 0 5 0 0 0 4560 0 8 4294902875 1826 1856 98 0 928 401 962 202 208 98 1 1026 1056 98 2 1090 1 493 1090 351 53 4560 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 246 0 0 0 175 0 0 0 16 1 0 0] 98 0 2544 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) 8 #fixedParentLeft 1 4946 8 #fixedParentRight 1 4946 8 #fixedParentBottom -45 4946 8 #fixedViewTop 47 410 1456 98 30 0 4432 98 2 8 1409372233 1025 5088 1698 202 208 928 0 1760 1282 1792 0 13 0 0 0 5088 0 8 4294904239 1826 1856 98 0 2768 1904 0 0 0 0 0 0 202 208 98 2 1954 8 'Column 1' 311 2000 1826 1856 5232 2032 2050 0 0 2082 1 83886081 2048 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 5328 7 257 0 0 5088 0 3 0 2050 0 0 2082 5 1 2048 8 'doIt' 8 '[:each | (each item at: 4) ifTrue: [each font: each font beItalic]]' 8 #[33 105 226 0 214 4 148 123 17 226 1 160 179 106 60 106] 2160 2224 2256 2272 5408 7 257 0 1954 8 '' 33 8 #center 1026 8 #empty 98 0 80 1826 8 #<= 98 0 0 0 5088 2050 0 0 2082 6 1 2048 8 'doIt' 8 '[:each | ((each at: 2) ifTrue: [JadeSystemBrowserPresenter overriddenIcon] ifFalse: [Icon blank]) 
	imageIndex]' 8 #[34 105 17 64 148 120 29 159 111 31 161 162 106] 8 ##(Smalltalk.JadeSystemBrowserPresenter) 8 #overriddenIcon 8 ##(Smalltalk.Icon) 8 #blank 8 #imageIndex 5632 7 257 0 1 0 0 2288 928 0 131173 0 98 1 0 962 202 208 98 3 1026 1056 98 2 1090 1 47 1090 351 447 5088 1026 2448 98 1 8 'Column 1' 5088 1026 8 #columnOrder: 98 1 98 2 5 3 5088 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 175 0 0 0 246 0 0 0] 98 0 2544 0 27 4914 4960 1 4992 1 4946 8 #fixedParentTop 47 5024 -45 410 4576 98 17 0 4432 98 2 8 1412498947 1025 6128 1698 202 208 928 0 1760 1282 4688 0 5 0 0 0 6128 0 8 4294902875 1826 1856 98 0 928 401 962 202 208 98 1 1026 1056 98 2 1090 1 1 1090 351 53 6128 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 26 0 0 0] 98 0 2544 0 27 4914 4960 1 4992 1 6096 1 5056 47 234 256 98 6 4560 8 'overrideList' 5088 8 'methodList' 6128 8 'superclassList' 0 962 202 208 98 1 1026 1056 98 2 1090 361 1 1090 351 539 4432 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 99 1 0 0 13 1 0 0] 98 3 6128 5088 4560 2544 0 27 2544 0 27 5 410 432 98 15 0 720 98 2 8 1140850688 131073 6704 0 0 0 5 0 0 0 6704 658 234 240 98 6 410 1216 98 23 0 6704 98 2 8 1409286144 131073 6816 0 1282 8 4278190080 0 517 0 0 0 6816 1330 202 208 98 2 1394 590662 1 ##(Smalltalk.CardLabel) 8 'Packages' 2050 0 0 2082 7 1 6976 8 'doIt' 8 '(CardLabel text: ''Packages'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 7008 5744 8 'Package.ico' 8 #fromId: 8 #text:iconBlock: 7024 11 1 0 0 410 1456 98 30 0 410 1504 98 22 0 6816 98 2 8 1140850688 131073 7184 0 1282 6896 0 5 0 0 0 7184 6912 234 256 98 4 410 1456 98 30 0 7184 98 2 8 1409372233 1025 7296 1698 202 208 928 0 1760 1282 8 4278190080 0 21 0 0 0 7296 0 8 4294904239 1826 1856 98 0 0 1904 0 0 0 0 0 0 202 208 98 1 1954 8 'Column 1' 305 2000 1826 1856 7456 2032 2050 0 0 2082 2 1 2048 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] 8 #key 7552 7 257 0 0 7296 0 3 0 2050 0 0 2082 5 1 3632 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] 2160 2224 2240 2272 7648 7 257 0 2288 928 0 131169 0 98 1 0 962 202 208 98 2 1026 1056 98 2 1090 1 37 1090 313 487 7296 1026 2448 98 1 8 'Column 1' 7296 1138 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 156 0 0 0 5 1 0 0] 98 0 2544 0 27 8 'dictionaryList' 7152 8 'packageList' 0 410 2592 98 28 0 6816 98 2 8 1140916864 1 7984 1698 202 208 98 2 6992 6978 8 'Dictionaries' 2050 0 0 2082 7 1 80 8 'doIt' 8 '(CardLabel text: ''Dictionaries'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 8112 5744 8 'Dictionary.ico' 7120 7136 8128 11 1 0 0 0 1760 1282 6896 0 1 0 0 0 7984 0 8 4294904353 2752 2768 1904 0 0 0 0 0 8 #smallIcons 0 0 0 0 0 962 202 208 98 3 1026 1056 98 2 1090 1 1 1090 373 539 7984 1026 2928 98 1 98 1 3 7984 1026 2992 98 2 -1 1 7984 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 186 0 0 0 13 1 0 0] 98 0 2544 0 27 0 6816 1090 33 33 3090 7184 0 3122 1090 277 3 1090 309 35 1 578 3200 8 'Pin or Unpin the tray' 1 1 0 0 0 962 202 208 98 1 1026 1056 98 2 1090 53 9 1090 313 523 7184 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 182 0 0 0 9 1 0 0] 98 2 7152 7296 2544 0 27 98 2 8 1409405001 1025 7152 1698 202 208 928 0 1760 1282 7408 0 5 0 0 0 7152 0 8 4294904239 1826 1856 98 0 0 1904 0 0 0 0 0 0 202 208 98 1 1954 8 'Column 1' 305 2000 1826 1856 8896 2032 2050 0 0 2082 2 1 2048 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] 7632 8992 7 257 0 0 7152 0 3 0 2050 0 0 2082 6 1 2048 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[34 105 226 0 142 221 9 17 226 1 100 160 97 161 180 106 60 106] 2160 2224 2240 2256 2272 9072 7 257 0 2288 928 0 131169 0 98 1 0 962 202 208 98 2 1026 1056 98 2 1090 1 37 1090 313 487 7152 1026 2448 98 1 8 'Column 1' 7152 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 156 0 0 0 5 1 0 0] 98 0 2544 0 27 1394 8096 7296 7152 234 256 98 2 7184 8 'packageDictionaryTabs' 0 7984 7184 1090 201 201 401 1 31 0 0 962 202 208 98 1 1026 1056 98 2 1090 1 1 1090 373 539 6816 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 186 0 0 0 13 1 0 0] 98 2 7184 7984 2544 0 27 11 410 432 98 15 0 6704 98 2 8 1140850688 131073 9616 0 0 0 5 0 0 0 9616 658 234 240 928 32 234 256 98 2 410 8 ##(Smalltalk.TreeView) 98 27 0 9616 98 2 8 1409352231 1025 9744 590918 3 ##(Smalltalk.TreeModel) 0 1760 525062 ##(Smalltalk.TreeNode) 0 0 0 234 256 928 1282 8 4278190080 0 21 0 0 0 9744 0 8 4294902947 2050 0 0 2082 5 1 2048 8 'doIt' 8 '[:each | each key isEmpty ifTrue: [''--All Categories--''] ifFalse: [each key last]].' 8 #[33 105 226 0 159 119 31 106 226 0 161 106] 7632 8 #isEmpty 8 '--All Categories--' 8 #last 9952 7 257 0 2768 1904 0 0 0 0 0 234 240 928 17 2784 1 0 962 202 208 98 1 1026 1056 98 2 1090 1 1 1090 223 539 9744 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 111 0 0 0 13 1 0 0] 98 0 2544 0 27 8 'classCategoryList' 0 962 202 208 98 1 1026 1056 98 2 1090 383 1 1090 223 539 9616 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 191 0 0 0 0 0 0 0 46 1 0 0 13 1 0 0] 98 1 9744 2544 0 27 7 410 432 98 15 0 6704 98 2 8 1140850688 131073 10432 0 0 0 5 0 0 0 10432 4498 234 240 98 4 410 8 ##(Smalltalk.CardContainer) 98 16 0 10432 98 2 8 1409286144 131073 10544 0 1282 8 4278190080 0 5 0 0 0 10544 1330 202 208 98 2 1394 6978 8 'Classes' 2050 0 0 2082 7 1 80 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 10736 5744 8 'Class.ico' 7120 7136 10752 11 1 0 0 410 1456 98 30 0 10544 98 2 8 1409372745 1025 10848 1698 202 208 928 0 1760 1282 7408 0 29 0 0 0 10848 0 8 4294904239 1826 1856 98 0 0 1904 0 0 0 0 0 0 202 208 98 2 1954 8 'Column 1' 359 2000 2050 0 0 2082 5 1 2048 8 'doIt' 8 '[:each | (each subStrings: Character space) first]' 8 #[33 105 17 29 159 178 161 106] 2192 2208 8 #subStrings: 2176 11072 7 257 0 2032 0 0 10848 0 3 0 0 1954 8 '' 69 2000 2050 0 0 2082 6 1 2048 8 'doIt' 8 '[:each | | index |
	index := each indexOf: Character space.
	0 < index ifTrue: [each copyFrom: index + 2 to: each size - 1] ifFalse: ['''']]' 8 #[34 105 17 29 159 178 90 62 18 128 221 9 233 1 64 126 17 145 99 193 106 33 106] 2192 2208 8 #indexOf: 8 #copyFrom:to: 11184 11200 7 65793 0 1826 5600 98 0 0 0 10848 0 1 0 0 2288 928 0 131169 0 98 1 0 962 202 208 98 3 1026 1056 98 2 1090 9 55 1090 435 433 10848 1026 2448 98 1 8 'Column 1' 10848 1026 5984 98 1 98 2 5 3 10848 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 221 0 0 0 243 0 0 0] 98 0 2544 0 27 1394 6978 8 'Hierarchy' 2050 0 0 2082 7 1 80 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 11648 5744 8 'ClassHierarchyDiagram.ico' 7120 7136 11664 11 1 0 0 410 9760 98 27 0 10544 98 2 8 1409352231 1025 11760 9826 0 1760 9858 0 0 0 234 256 928 1282 8 4278190080 0 29 0 0 0 11760 0 8 4294902947 2050 0 0 2082 2 1 1824 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] 10064 11920 7 257 0 2768 1904 0 0 0 0 0 234 240 928 17 2784 1 0 962 202 208 98 1 1026 1056 98 2 1090 9 55 1090 435 433 11760 1138 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 221 0 0 0 243 0 0 0] 98 0 2544 0 27 10848 234 256 98 4 10848 8 'classList' 11760 8 'classHierarchy' 0 410 2592 98 28 0 10544 98 2 8 1140916736 1 12240 1698 202 208 98 2 10720 11632 0 1760 0 0 1 0 0 0 12240 0 8 4294904353 2050 0 0 918822 ##(Smalltalk.CompiledMethod) 2 3 8 ##(Smalltalk.ListControlView) 8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 1856 12368 7 257 0 2050 0 0 12386 2 3 2768 8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 12464 7 257 0 1904 0 0 0 0 0 8256 0 0 0 0 0 962 202 208 98 3 1026 1056 98 2 1090 1 1 1090 451 495 12240 1026 2928 98 1 98 1 3 12240 1026 2992 98 2 -1 1 12240 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 247 0 0 0] 98 0 2544 0 27 962 202 208 98 1 1026 1056 98 2 1090 1 1 1090 451 495 10544 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 247 0 0 0] 98 3 10848 11760 12240 2544 0 27 4914 4960 1 4992 1 6096 1 5024 -43 410 10560 98 16 0 10432 98 2 8 1409286144 131073 12960 0 1282 8 4278190080 0 5 0 0 0 12960 1330 202 208 98 2 1394 6978 8 'Instance' 2050 0 0 2082 7 1 80 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 13136 5744 8 'Class.ico' 7120 7136 13152 11 1 0 0 410 432 98 15 0 12960 98 2 8 1140850688 131073 13248 0 0 0 5 0 0 0 13248 0 234 256 928 0 962 202 208 98 1 1026 1056 98 2 1090 9 9 1090 435 1 13248 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 221 0 0 0 4 0 0 0] 98 0 2544 0 27 1394 6978 8 'Class' 2050 0 0 2082 7 1 80 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 13520 5744 8 'Metaclass.ico' 7120 7136 13536 11 1 0 0 410 432 98 15 0 12960 98 2 8 1140850688 131073 13632 0 0 0 5 0 0 0 13632 0 234 256 928 0 962 202 208 98 1 1026 1056 98 2 1090 9 9 1090 435 1 13632 1138 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 221 0 0 0 4 0 0 0] 98 0 2544 0 27 13248 234 256 98 4 13248 8 'instanceTab' 13632 8 'classTab' 0 410 2592 98 28 0 12960 98 2 8 1140916738 1 13936 1698 202 208 98 2 13120 13504 0 1760 0 0 1 0 0 0 13936 0 8 4294904353 2050 0 0 12386 2 3 12416 12432 575230339 8 #[30 105 226 0 106] 1856 14064 7 257 0 2050 0 0 12386 2 3 2768 12496 579598755 8 #[30 105 226 0 106] 12528 14112 7 257 0 1904 0 0 0 0 0 8256 0 0 0 0 0 962 202 208 98 3 1026 1056 98 2 1090 1 1 1090 451 51 13936 1026 2928 98 1 98 1 3 13936 1026 2992 98 2 -1 1 13936 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 25 0 0 0] 98 0 2544 0 27 962 202 208 98 1 1026 1056 98 2 1090 1 489 1090 451 51 12960 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 225 0 0 0 13 1 0 0] 98 3 13248 13632 13936 2544 0 27 4914 4960 1 4992 1 5024 -49 5056 51 234 256 98 4 10544 8 'classHierarchyTabs' 12960 8 'instanceClassTabs' 0 962 202 208 98 1 1026 1056 98 2 1090 615 1 1090 451 539 10432 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 51 1 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 98 2 10544 12960 2544 0 27 13 32 234 256 928 0 962 202 208 98 1 1026 1056 98 2 1090 1 1 1090 1065 539 6704 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 98 5 6816 410 4176 98 12 0 6704 98 2 8 1140850688 1 14976 0 1282 4256 0 517 0 0 0 14976 962 202 208 98 1 1026 1056 98 2 1090 373 1 1090 11 539 14976 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 186 0 0 0 0 0 0 0 191 0 0 0 13 1 0 0] 98 0 2544 0 27 9616 410 4176 98 12 0 6704 98 2 8 1140850688 1 15216 0 1282 4256 0 517 0 0 0 15216 962 202 208 98 1 1026 1056 98 2 1090 605 1 1090 11 539 15216 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 46 1 0 0 0 0 0 0 51 1 0 0 13 1 0 0] 98 0 2544 0 27 10432 2544 0 27 7 32 234 256 928 0 962 202 208 98 1 1026 1056 98 2 1090 1 1 1090 1785 539 720 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 98 3 6704 410 4176 98 12 0 720 98 2 8 1140850688 1 15632 0 1282 4256 0 517 0 0 0 15632 962 202 208 98 1 1026 1056 98 2 1090 1065 1 1090 11 539 15632 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 2 0 0 0 0 0 0 25 2 0 0 13 1 0 0] 98 0 2544 0 27 832 2544 0 27 524806 ##(Smalltalk.Fraction) 3 5 410 10560 98 16 0 416 98 2 8 1409286144 131073 15904 0 1282 10640 0 5 0 0 0 15904 1330 202 208 98 6 1394 8 'Class Definition' 410 8 ##(Smalltalk.ScintillaView) 98 50 0 15904 98 2 8 1445007428 1025 16064 721990 2 ##(Smalltalk.ValueHolder) 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 1282 8 4278190080 0 5 498 0 16 98 11 546 1 578 8 #editSave 8 '&Save' 9383 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 546 1 578 8 #editUndo 8 '&Undo' 9397 1 0 0 0 546 1 578 8 #editRedo 8 '&Redo' 9395 1 0 0 0 16338 4097 546 1 578 8 #editCut 8 'Cu&t' 9393 1 0 0 0 546 1 578 8 #editCopy 8 '&Copy' 9351 1 0 0 0 546 1 578 8 #editPaste 8 '&Paste' 9389 1 0 0 0 546 1 578 8 #editDelete 8 '&Delete' 1629 1 0 0 0 16338 4097 546 1 578 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[244 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1090 193 193 0 16064 0 8 4294904259 852486 ##(Smalltalk.NullConverter) 0 0 13 0 234 256 98 44 8 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 0 0 1 0 0 0 0 17040 0 0 0 8 #assignment 17058 29 0 0 3 0 0 0 0 17088 0 1401 0 8 #lineNumber 17058 67 0 0 1 0 0 0 0 17120 0 0 0 8 #symbol 17058 9 786694 ##(Smalltalk.IndexedColor) 33554443 0 1 0 0 0 0 17152 0 0 0 8 #global 17058 21 0 0 3 0 0 0 0 17216 0 1401 0 8 #binary 17058 11 17186 33554433 0 1 0 0 0 0 17248 0 0 0 8 #nil 17058 19 196934 1 ##(Smalltalk.RGB) 16646145 0 3 0 0 0 0 17296 0 1401 0 8 #callTip 17058 77 17186 33554457 17186 33554471 1 0 0 0 0 17360 0 0 0 8 #self 17058 15 17344 0 3 0 0 0 0 17424 0 1401 0 8 #string 17058 3 17330 16646399 0 129 0 0 0 0 17456 0 0 0 8 #character 17058 31 17330 16646399 0 3 0 0 0 0 17504 0 1401 0 8 #indentGuide 17058 75 17186 33554447 0 1 0 0 0 0 17552 0 0 0 8 #super 17058 17 17344 0 3 0 0 0 0 17600 0 1401 0 8 #special 17058 25 0 0 3 0 0 0 0 17632 0 1401 0 8 #braceHighlight 17058 69 17330 66047 0 3 0 0 0 0 17664 0 1401 0 8 #boolean 17058 13 17344 0 3 0 0 0 0 17712 0 1401 0 8 #comment 17058 7 17330 65025 0 1 0 0 0 0 17744 0 0 0 8 #specialSelector 17058 33 17344 0 3 0 0 0 0 17792 0 1401 0 8 #braceMismatch 17058 71 17186 33554459 0 3 0 0 0 0 17824 0 1401 0 8 #number 17058 5 17330 16711169 0 1 0 0 0 0 17872 0 0 0 8 #keywordSend 17058 27 17186 33554437 0 3 0 0 0 0 17920 0 1401 0 8 #return 17058 23 17330 321 0 3 0 0 0 0 17968 0 1401 0 98 40 17072 17472 17888 17760 17168 17264 17728 17440 17616 17312 17232 17984 17648 17936 17104 17520 17808 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 17136 17680 17840 0 17568 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler) 17040 234 256 98 2 8 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 0 0 0 16064 8 #circle 0 0 0 0 0 0 202 208 928 0 63 9215 0 0 0 0 17584 0 0 65 0 0 234 240 98 4 17040 8 '()[]{}<>' 17632 8 '()[]{}<>' 8 '' 3 234 256 98 2 8 #container 234 256 98 2 17040 17058 1 0 0 1 0 0 0 0 17040 0 0 0 0 0 0 0 1 0 234 256 98 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 16064 65025 3 32 1 0 0 0 0 0 0 3 18386 3 16064 33423361 5 32 3 0 0 0 0 0 0 5 18386 5 16064 511 1 32 5 0 0 0 0 0 0 0 0 234 256 98 6 8 #Notification 17058 1029 0 17330 31185883 1 0 0 0 0 18480 0 0 0 8 #Error 17058 1031 17330 335 17330 31317503 1 0 0 0 0 18528 0 0 0 8 #Warning 17058 1027 17330 60239 17330 31326207 1 0 0 0 0 18592 0 0 0 0 962 202 208 98 13 1026 1056 98 2 1090 9 55 1090 1769 479 16064 1026 8 #contextMenu: 98 1 16240 16064 1026 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval) 3 1 3 16064 1026 8 #isTextModified: 98 1 32 16064 1026 8 #modificationEventMask: 98 1 9215 16064 1026 8 #secondaryCaretForecolor: 98 1 17330 16711423 16064 1026 8 #caretPeriod: 98 1 1061 16064 1026 8 #wordWrap: 98 1 16 16064 1026 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin) 1 16064 1 3 32 1 5 19218 3 16064 33 1 16 67108863 5 19218 5 16064 1 1 16 -67108863 5 16064 1026 8 #tabIndents: 98 1 16 16064 1026 8 #tabWidth: 98 1 9 16064 1026 8 #setLexerLanguage: 98 1 8 #smalltalk 16064 1026 8 #areAdditionalCaretsVisible: 98 1 16 16064 1138 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 98 0 2544 0 29 1394 8 'Class Documentation' 410 16080 98 50 0 15904 98 2 8 1445007428 1025 19568 16146 0 32 16192 0 1282 16224 0 5 0 16866 0 16 16898 8 #[244 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1090 193 193 0 19568 0 8 4294904259 16978 0 0 11 0 234 256 98 4 17040 17058 1 0 0 1 0 0 0 0 17040 0 0 0 17360 17058 77 17392 17408 1 0 0 0 0 17360 0 0 0 98 40 19792 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18034 17040 234 256 98 2 18096 18114 1 0 0 0 19568 18144 0 0 0 0 0 0 202 208 928 0 63 9215 0 0 0 0 17584 0 0 65 0 0 0 8 '' 3 234 256 98 2 18288 234 256 98 2 17040 19792 0 0 0 0 1 0 234 256 98 6 1 18386 1 19568 65025 3 32 1 0 0 0 0 0 0 3 18386 3 19568 33423361 5 32 3 0 0 0 0 0 0 5 18386 5 19568 511 1 32 5 0 0 0 0 0 0 0 0 234 256 98 6 18480 17058 1029 0 17330 31185883 1 0 0 0 0 18480 0 0 0 18528 17058 1031 17330 335 17330 31317503 1 0 0 0 0 18528 0 0 0 18592 17058 1027 17330 60239 17330 31326207 1 0 0 0 0 18592 0 0 0 0 962 202 208 98 11 1026 1056 98 2 1090 9 55 1090 1769 479 19568 1026 18832 98 1 18866 3 1 3 19568 1026 18912 98 1 32 19568 1026 18960 98 1 9215 19568 1026 19008 98 1 17330 16711423 19568 1026 19072 98 1 1061 19568 1026 19120 98 1 16 19568 1026 19168 98 1 98 3 19218 1 19568 1 3 32 1 5 19218 3 19568 33 1 16 67108863 5 19218 5 19568 1 1 16 -67108863 5 19568 1026 19296 98 1 16 19568 1026 19344 98 1 9 19568 1026 19456 98 1 16 19568 1138 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 98 0 2544 0 29 1394 6978 8 'Package' 2050 0 0 2082 7 1 6976 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 20848 5744 8 'Package.ico' 7120 7136 20864 11 1 0 0 410 432 98 15 0 15904 98 2 8 1140850688 131073 20960 0 0 0 5 0 0 0 20960 658 234 240 98 4 410 1456 98 30 0 20960 98 2 8 1409355853 1025 21072 1698 202 208 928 0 1760 1282 8 4278190080 0 5 498 0 16 98 5 546 1 578 8 #addRepository 8 'Add &Repository' 1 1 0 0 0 546 1 578 8 #removeRepository 8 'Remo&ve Repository' 1025 1 0 0 0 16338 4097 546 1 578 608 8 '&Save Package' 1 1 0 0 0 546 1 578 8 #showPackageChanges 8 'Show &Changes' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 21072 0 8 4294904239 1826 1856 98 0 0 1904 0 0 0 0 0 0 202 208 98 4 1954 8 'Repository Type' 211 2000 1826 1856 21536 2032 2050 0 0 2082 2 1 2048 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] 11296 21632 7 257 0 0 21072 0 1 0 0 1954 8 'Description' 1161 2000 1826 1856 98 0 1826 5600 21760 2050 0 0 2082 1 83886081 3632 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 21792 7 257 0 0 21072 0 3 0 0 1954 8 'User' 201 2000 1826 1856 98 0 1826 5600 21920 2050 0 0 2082 1 83886081 3632 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 21952 7 257 0 0 21072 0 1 0 0 1954 8 'Password' 201 2000 1826 1856 21920 1826 5600 21920 2050 0 0 2082 3 1 3632 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] 8 #collect: 22096 2050 0 0 22112 19 257 0 7 257 0 0 21072 0 1 0 0 2288 928 0 131169 0 98 4 0 0 1090 1 1 0 962 202 208 98 3 1026 1056 98 2 1090 1 263 1090 1769 217 21072 1026 18784 98 1 21200 21072 1026 2448 98 1 8 'Repository Type' 21072 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 98 0 2544 0 27 19 410 1456 98 30 0 20960 98 2 8 1409355853 1025 22480 1698 202 208 928 0 1760 1282 21184 0 5 498 0 16 98 1 546 1 578 8 #compareAncestor 8 '&Compare' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 22480 0 8 4294904239 1826 1856 98 0 0 1904 0 0 0 0 0 0 202 208 98 4 1954 8 'Ancestor' 131 2000 1826 1856 21920 1826 5600 21920 2050 0 0 2082 1 83886081 3632 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 22848 7 257 0 0 22480 0 1 0 0 1954 8 'Name' 401 2000 1826 1856 22736 2032 2050 0 0 2082 1 83886081 2048 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 22976 7 257 0 0 22480 0 1 0 0 1954 8 'Timestamp' 301 2000 1826 1856 21760 1826 5600 21760 2050 0 0 2082 1 83886081 2048 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 23120 7 257 0 0 22480 0 1 0 0 1954 8 'Message' 941 2000 1826 1856 21760 1826 5600 21760 2050 0 0 2082 1 83886081 2048 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 23264 7 257 0 0 22480 0 3 0 0 2288 928 0 131169 0 98 4 0 0 1090 1 1 0 962 202 208 98 3 1026 1056 98 2 1090 1 1 1090 1769 263 22480 1026 18784 98 1 22592 22480 1026 2448 98 1 8 'Ancestor' 22480 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 98 0 2544 0 27 23 16 234 256 98 4 21072 8 'repositoryList' 22480 8 'ancestorList' 0 962 202 208 98 1 1026 1056 98 2 1090 9 55 1090 1769 479 20960 1138 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 98 2 22480 21072 2544 0 27 1394 6978 8 'Globals' 2050 0 0 2082 7 1 80 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 23872 5744 8 'Dictionary.ico' 7120 7136 23888 11 1 0 0 410 1456 98 30 0 15904 98 2 8 1409355853 1025 23984 1698 202 208 928 0 1760 1282 21184 0 5 0 0 0 23984 0 8 4294904239 1826 1856 98 0 0 1904 0 0 0 0 0 0 202 208 98 3 1954 8 'Name' 587 2000 1826 1856 24128 2032 2050 0 0 2082 1 83886081 3632 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 24224 7 257 0 0 23984 0 3 0 0 1954 8 'Class' 589 2000 1826 1856 21760 1826 5600 21760 2050 0 0 2082 1 83886081 3632 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 24368 7 257 0 0 23984 0 3 0 0 1954 8 'Value' 587 2000 1826 1856 21760 1826 5600 21760 2050 0 0 2082 1 83886081 3632 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 24512 7 257 0 0 23984 0 3 0 0 2288 928 0 131169 0 98 4 0 0 1090 1 1 0 962 202 208 98 2 1026 1056 98 2 1090 9 55 1090 1769 479 23984 1026 2448 98 1 8 'Name' 23984 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 98 0 2544 0 27 1394 6978 8 'Method Source' 2050 0 0 2082 7 1 80 8 'doIt' 8 '(CardLabel text: ''Method Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PUBLIC.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 24864 5744 8 'COMPILEDMETHOD_PUBLIC.ico' 7120 7136 24880 11 1 0 0 410 16080 98 50 0 15904 98 2 8 1445007428 1025 24976 16146 0 32 16192 0 1282 16224 0 5 498 0 16 98 21 546 1 578 16304 8 '&Save' 9383 1 0 0 0 16338 4097 546 1 578 16400 8 '&Undo' 9397 1 0 0 0 546 1 578 16464 8 '&Redo' 9395 1 0 0 0 16338 4097 546 1 578 16544 8 'Cu&t' 9393 1 0 0 0 546 1 578 16608 8 '&Copy' 9351 1 0 0 0 546 1 578 16672 8 '&Paste' 9389 1 0 0 0 546 1 578 16736 8 '&Delete' 1629 1 0 0 0 546 1 578 16816 8 'Select &All' 9347 1 0 0 0 16338 4097 546 1 578 8 #editFind 8 '&Find...' 9357 1 0 0 0 546 1 578 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 546 1 578 8 #editReplace 8 '&Replace...' 9361 1 0 0 0 16338 4097 546 1 578 8 #jadeInspect 8 '&Inspect' 9379 1 0 0 0 546 1 578 8 #jadeDisplay 8 '&Display' 9353 1 0 0 0 546 1 578 8 #jadeExecute 8 'Execute' 9355 1 0 0 0 16338 4097 546 1 578 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 546 1 578 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 16866 0 16 16898 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1090 193 193 0 24976 0 8 4294904259 16978 0 0 13 0 234 256 98 44 17040 17058 1 0 0 1 0 0 0 0 17040 0 0 0 17088 17058 29 0 0 3 0 0 0 0 17088 0 1401 0 17120 17058 67 0 0 1 0 0 0 0 17120 0 0 0 17152 17058 9 17200 0 1 0 0 0 0 17152 0 0 0 17216 17058 21 0 0 3 0 0 0 0 17216 0 1401 0 17248 17058 11 17280 0 1 0 0 0 0 17248 0 0 0 17296 17058 19 17344 0 3 0 0 0 0 17296 0 1401 0 17360 17058 77 17392 17408 1 0 0 0 0 17360 0 0 0 17424 17058 15 17344 0 3 0 0 0 0 17424 0 1401 0 17456 17058 3 17488 0 129 0 0 0 0 17456 0 0 0 17504 17058 31 17536 0 3 0 0 0 0 17504 0 1401 0 17552 17058 75 17584 0 1 0 0 0 0 17552 0 0 0 17600 17058 17 17344 0 3 0 0 0 0 17600 0 1401 0 17632 17058 25 0 0 3 0 0 0 0 17632 0 1401 0 17664 17058 69 17330 66047 0 3 0 0 0 0 17664 0 1401 0 17712 17058 13 17344 0 3 0 0 0 0 17712 0 1401 0 17744 17058 7 17776 0 1 0 0 0 0 17744 0 0 0 17792 17058 33 17344 0 3 0 0 0 0 17792 0 1401 0 17824 17058 71 17856 0 3 0 0 0 0 17824 0 1401 0 17872 17058 5 17904 0 1 0 0 0 0 17872 0 0 0 17920 17058 27 17952 0 3 0 0 0 0 17920 0 1401 0 17968 17058 23 18000 0 3 0 0 0 0 17968 0 1401 0 98 40 26224 26368 26544 26496 26272 26304 26480 26352 26416 26320 26288 26576 26432 26560 26240 26384 26512 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 26256 26448 26528 0 26400 0 0 18034 17040 234 256 98 16 8 #folderOpen 18114 63 13 0 0 24976 26656 0 0 0 0 0 0 8 #folderMidTail 18114 55 11 33554471 33554471 24976 26688 0 0 0 0 0 0 8 #folderSub 18114 59 11 0 0 24976 26720 0 0 0 0 0 0 8 #folderTail 18114 57 11 0 0 24976 26752 0 0 0 0 0 0 18096 18114 1 0 0 0 24976 18144 0 0 0 0 0 0 8 #folderOpenMid 18114 53 11 33554471 33554471 24976 26800 0 0 0 0 0 0 8 #folder 18114 61 5 0 0 24976 26832 0 0 0 0 0 0 8 #folderEnd 18114 51 11 33554471 33554471 24976 26864 0 0 0 0 0 0 202 208 928 0 63 9215 0 0 0 0 17584 0 0 65 0 0 234 240 98 4 17040 18208 17632 8 '()[]{}<>' 8 '' 3 234 256 98 4 18288 234 256 98 2 17040 17058 1 0 0 1 0 0 0 0 17040 0 0 0 19424 26192 0 0 8 #arrows 0 1 0 234 256 98 12 1 18386 1 24976 65025 3 32 1 0 0 0 0 0 0 3 18386 3 24976 33423361 5 32 3 0 0 0 0 0 0 5 18386 5 24976 511 1 32 5 0 0 0 0 0 0 8 'indicator9' 18386 19 24976 33554459 13 32 0 0 0 0 0 0 0 8 'indicator8' 18386 17 24976 33554447 1 32 0 0 0 0 0 0 0 8 'indicator10' 18386 21 24976 511 3 32 0 0 0 0 0 0 0 0 0 234 256 98 6 18480 17058 1029 0 17330 31185883 1 0 0 0 0 18480 0 0 0 18528 17058 1031 17330 335 17330 31317503 1 0 0 0 0 18528 0 0 0 18592 17058 1027 17330 60239 17330 31326207 1 0 0 0 0 18592 0 0 0 0 962 202 208 98 14 1026 1056 98 2 1090 9 55 1090 1769 479 24976 1026 18784 98 1 25072 24976 1026 18832 98 1 18866 3 1 3 24976 1026 18912 98 1 32 24976 1026 18960 98 1 9215 24976 1026 8 #hoverTime: 98 1 401 24976 1026 19008 98 1 17330 16711423 24976 1026 19072 98 1 1061 24976 1026 19168 98 1 98 3 19218 1 24976 61 3 32 1 5 19218 3 24976 1 1 16 67108863 5 19218 5 24976 1 1 16 -67108863 5 24976 1026 8 #sciSetIndentationGuides: 98 1 3 24976 1026 19296 98 1 16 24976 1026 19344 98 1 9 24976 1026 19392 98 1 19424 24976 1026 19456 98 1 16 24976 1138 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 98 0 2544 0 29 1394 6978 8 'Original Source' 2050 0 0 2082 7 1 80 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 28144 5744 8 'COMPILEDMETHOD_PRIVATE.ico' 7120 7136 28160 11 1 0 0 410 16080 98 50 0 15904 98 2 8 1445007428 1025 28256 16146 0 32 16192 0 1282 8 4278190080 0 5 0 16866 0 16 16898 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1090 193 193 0 28256 0 8 4294904259 16978 0 0 11 0 234 256 98 44 17040 17058 1 0 0 1 0 0 0 0 17040 0 0 0 17088 17058 29 0 0 3 0 0 0 0 17088 0 1401 0 17120 17058 67 0 0 1 0 0 0 0 17120 0 0 0 17152 17058 9 17200 0 1 0 0 0 0 17152 0 0 0 17216 17058 21 0 0 3 0 0 0 0 17216 0 1401 0 17248 17058 11 17280 0 1 0 0 0 0 17248 0 0 0 17296 17058 19 17330 16646145 0 3 0 0 0 0 17296 0 1401 0 17360 17058 77 17392 17408 1 0 0 0 0 17360 0 0 0 17424 17058 15 28608 0 3 0 0 0 0 17424 0 1401 0 17456 17058 3 17330 16646399 0 129 0 0 0 0 17456 0 0 0 17504 17058 31 17330 16646399 0 3 0 0 0 0 17504 0 1401 0 17552 17058 75 17584 0 1 0 0 0 0 17552 0 0 0 17600 17058 17 28608 0 3 0 0 0 0 17600 0 1401 0 17632 17058 25 0 0 3 0 0 0 0 17632 0 1401 0 17664 17058 69 17186 33554465 0 3 0 0 0 0 17664 0 1401 0 17712 17058 13 28608 0 3 0 0 0 0 17712 0 1401 0 17744 17058 7 17330 65025 0 1 0 0 0 0 17744 0 0 0 17792 17058 33 28608 0 3 0 0 0 0 17792 0 1401 0 17824 17058 71 17856 0 3 0 0 0 0 17824 0 1401 0 17872 17058 5 17330 16711169 0 1 0 0 0 0 17872 0 0 0 17920 17058 27 17952 0 3 0 0 0 0 17920 0 1401 0 17968 17058 23 17330 321 0 3 0 0 0 0 17968 0 1401 0 98 40 28496 28656 28880 28816 28544 28576 28800 28640 28736 28592 28560 28928 28752 28912 28512 28688 28848 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 28528 28768 28864 0 28720 0 0 18034 17040 234 256 98 2 18096 18114 1 0 0 0 28256 18144 0 0 0 0 0 0 202 208 928 0 63 9215 0 0 0 0 17584 0 0 65 0 0 0 8 '' 3 234 256 98 2 18288 234 256 98 2 17040 17058 1 0 0 1 0 0 0 0 17040 0 0 0 0 0 0 0 1 0 234 256 98 6 1 18386 1 28256 65025 3 32 1 0 0 0 0 0 0 3 18386 3 28256 33423361 5 32 3 0 0 0 0 0 0 5 18386 5 28256 511 1 32 5 0 0 0 0 0 0 0 0 234 256 98 6 18480 17058 1029 0 17330 31185883 1 0 0 0 0 18480 0 0 0 18528 17058 1031 17330 335 17330 31317503 1 0 0 0 0 18528 0 0 0 18592 17058 1027 17330 60239 17330 31326207 1 0 0 0 0 18592 0 0 0 0 962 202 208 98 12 1026 1056 98 2 1090 9 55 1090 1769 479 28256 1026 18832 98 1 18866 3 1 3 28256 1026 18912 98 1 32 28256 1026 18960 98 1 9215 28256 1026 19008 98 1 17330 16711423 28256 1026 19072 98 1 1061 28256 1026 19120 98 1 16 28256 1026 19168 98 1 98 3 19218 1 28256 61 3 32 1 5 19218 3 28256 1 1 16 67108863 5 19218 5 28256 1 1 16 -67108863 5 28256 1026 19296 98 1 16 28256 1026 19344 98 1 9 28256 1026 19392 98 1 19424 28256 1026 19456 98 1 16 28256 1138 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 98 0 2544 0 29 23984 234 256 98 12 16064 8 'classDefinition' 28256 8 'originalSource' 23984 8 'globals' 19568 8 'classDocumentation' 24976 8 'methodSource' 20960 8 'packageInfo' 0 410 2592 98 28 0 15904 98 2 8 1140916736 1 30128 1698 202 208 98 6 20832 23856 16048 19552 28128 24848 0 1760 0 0 1 0 0 0 30128 0 8 4294904353 2050 0 0 12386 2 3 12416 12432 575230339 8 #[30 105 226 0 106] 1856 30256 7 257 0 2050 0 0 12386 2 3 2768 12496 579598755 8 #[30 105 226 0 106] 12528 30304 7 257 0 1904 0 0 0 0 0 8256 0 0 0 0 0 962 202 208 98 3 1026 1056 98 2 1090 1 1 1090 1785 541 30128 1026 2928 98 1 98 1 5 30128 1026 2992 98 2 -1 1 30128 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 14 1 0 0] 98 0 2544 0 27 962 202 208 98 1 1026 1056 98 2 1090 1 549 1090 1785 541 15904 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 1 0 0 124 3 0 0 32 2 0 0] 98 7 20960 23984 16064 19568 28256 24976 30128 2544 0 27 15874 3 5 16 234 256 98 4 410 4176 98 12 0 416 98 2 8 1140850688 1 30800 0 1282 4256 0 517 0 0 0 30800 962 202 208 98 1 1026 1056 98 2 1090 1 539 1090 1785 11 30800 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 13 1 0 0 124 3 0 0 18 1 0 0] 98 0 2544 0 27 8 'splitter' 15904 8 'textAreaTabs' 0 962 202 208 98 2 1026 1056 98 2 1090 5119 21 1090 1785 1089 416 1026 18784 98 1 512 416 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 123 13 0 0 42 2 0 0] 98 3 720 30800 15904 2544 0 27 )! !
!JadeSystemBrowserPresenter class categoriesFor: #overriddenIcon!public! !
!JadeSystemBrowserPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

