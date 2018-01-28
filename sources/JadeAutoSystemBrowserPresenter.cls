"Filed out from Dolphin Smalltalk 7"!

JadeSystemBrowserPresenter subclass: #JadeAutoSystemBrowserPresenter
	instanceVariableNames: 'lastGsShape bottomArrangement'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeAutoSystemBrowserPresenter guid: (GUID fromString: '{a4b39ce8-df3a-4c06-a336-25f599463ae0}')!
JadeAutoSystemBrowserPresenter comment: 'This class register information in the registry (JadeAutocompletationRegistry) each time a user select a class in the hierarchy browser.

see:
#registerClassData'!
!JadeAutoSystemBrowserPresenter categoriesForClass!Unclassified! !
!JadeAutoSystemBrowserPresenter methodsFor!

bottomArrangement
	^bottomArrangement!

bottomArrangement: anObject
	bottomArrangement := anObject!

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
		when: #'hoverStart:'						send: #'methodHoverStart:'			to: self;
		when: #'hoverEnd:'						send: #'methodHoverEnd:'				to: self;
		when: #'aboutToDisplayMenu:'		send: #'methodMenu:'					to: self;
		when: #'leftButtonDoubleClicked:'	send: #'methodDoubleClicked:'		to: self;
		when: #'valueChanged'					send: #'methodValueChanged'		to: self;
		when: #'focusLost'						send: #'cancelCallTip'					to: methodSourcePresenter view;
		yourself.
!

createStateEntry

	^packageDictionaryTabs currentCard == packageListPresenter view!

getClassHierarchy
	"
classHierarchyPresenter model anArray of Arrays #(#('Object') #('Object' 'Boolean') #('Object' 'True'))"

	^classHierarchyPresenter model asOrderedCollection detect: [:eachArray | eachArray last = self getClassName] ifNone: [#()]	
"
	^classHierarchyPresenter selection
"!

getClassName

	"classHierarchyPresenter hasSelection ifFalse: [^'none'].

	^classHierarchyPresenter selection last."

	^(self selectedClasses size = 1) 
	ifFalse: ['none']
	ifTrue: [self selectedClasses last]!

gsClassMethods

	^methodListPresenter model collect: [:each | each first]!

gsClassPoolDictionaries
	| arrayString i j |

	classDefinition ifNil: [^#()].
	i := classDefinition indexOfSubCollection: 'poolDictionaries:'.
	j := classDefinition indexOfSubCollection: 'inDictionary:'.
	(i == 0 or: [j == 0]) ifTrue: [^#()].
	arrayString := (classDefinition copyFrom: i to: j - 1) allButFirst: 17"poolDictionaries:".

	^self createArrayFromString: arrayString.!

gsClassVariables
	| arrayString i j |

	classDefinition ifNil: [^#()].
	i := classDefinition indexOfSubCollection: 'classVars:'.
	j := classDefinition indexOfSubCollection: 'classInstVars:'.
	(i == 0 or: [j == 0]) ifTrue: [^#()].
	arrayString := (classDefinition copyFrom: i to: j - 1) allButFirst: 10 "classVars:".

	^self createArrayFromString: arrayString.!

gsInstClassVariables
	| arrayString i j |

	classDefinition ifNil: [^#()].
	i := classDefinition indexOfSubCollection: 'classInstVars:'.
	j := classDefinition indexOfSubCollection: 'poolDictionaries:'.
	(i == 0 or: [j == 0]) ifTrue: [^#()].
	arrayString := (classDefinition copyFrom: i to: j - 1) allButFirst: 14 "classInstVars:".

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

	^JadeAutoTextPresenter3 new.!

registerClassData
	"When a class is selected in the GemStone browser all meta data of that class is registered to be used in the autocompletion system"
	| gsMethods gsClassMethods |

	categoryListPresenter hasSelection ifTrue: [^self]. "if a category is selected then not all methods are displayed --> we only register a class data with ALL methods (no category selected)"
	(self selectedClasses size = 1) ifFalse: [^self]. "with multiple classes we ignore the registration, which class own listed methods ???"
	gsClassMethods := OrderedCollection new.
	gsMethods := OrderedCollection new.
	self hasClassSideSelected ifTrue: [gsClassMethods := self gsClassMethods] ifFalse: [gsMethods := self gsMethods].

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
	lastGsShape updateClassData.
	methodSourcePresenter currentGsShape: lastGsShape.!

removeClass
	| list |

	list := self selectedClasses.
	
	super removeClass.

	list do: [:each | self registry unregister: each]!

restoreFrom: aJadeHistoryEntry

	aJadeHistoryEntry isPackageSelected 
	ifTrue: [self isPackageListTabSelected ifFalse: [packageDictionaryTabs previousCard].
			packageListPresenter selections: aJadeHistoryEntry packageOrDictionary ifAbsent: []] 
	ifFalse: [self isPackageListTabSelected ifTrue: [packageDictionaryTabs nextCard].
			dictionaryListPresenter selections: aJadeHistoryEntry packageOrDictionary ifAbsent: []].

	classCategoryPresenter selection: aJadeHistoryEntry classCategory ifAbsent: [].

	aJadeHistoryEntry isHierarchySelected 
	ifTrue: [self isClassListTabSelected ifTrue: [classHierarchyTabs nextCard].
			classHierarchyPresenter selection: aJadeHistoryEntry className ifAbsent: []] 
	ifFalse: [self isClassListTabSelected ifFalse: [classHierarchyTabs previousCard].
			classListPresenter selection: aJadeHistoryEntry className ifAbsent: []].

	aJadeHistoryEntry isCategoriesSelected 
	ifTrue: [self isCategoriesTabSelected ifFalse: [methodFilterTabs previousCard].
			categoryListPresenter selection: aJadeHistoryEntry categoriesOrVariables ifAbsent: [].] 
	ifFalse: [self isCategoriesTabSelected ifTrue: [methodFilterTabs nextCard].
			variableListPresenter selection: aJadeHistoryEntry categoriesOrVariables ifAbsent: [].].

	methodListPresenter selection: aJadeHistoryEntry methodName ifAbsent: [].

	self resetCursor.
!

updateAndSelect: aView

	super updateAndSelect: aView.

	inUpdate ifTrue: [^self]. "when the browser is updating we avoid to register any data we must wait until the browser has finish"

	self registerClassData.!

updateMethodList
	"The receiver must keep update with any add/remove of a method in order to be acurate in the autocompletation"

	super updateMethodList.

	lastGsShape ifNil: [^self].

	categoryListPresenter hasSelection ifTrue: [^self]. "if a category is selected then not all methods are displayed --> we only register a class data with ALL methods (no category selected)"

	self hasClassSideSelected ifTrue: [^lastGsShape gsClassMethods: self gsClassMethods].

	^lastGsShape gsMethods: self gsMethods.! !
!JadeAutoSystemBrowserPresenter categoriesFor: #bottomArrangement!accessing!private! !
!JadeAutoSystemBrowserPresenter categoriesFor: #bottomArrangement:!accessing!private! !
!JadeAutoSystemBrowserPresenter categoriesFor: #codePresenterIsMethod!public! !
!JadeAutoSystemBrowserPresenter categoriesFor: #createArrayFromString:!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #createSchematicWiringForMethodSource!create schemantic wiring!public! !
!JadeAutoSystemBrowserPresenter categoriesFor: #createStateEntry!accessing!public!updating! !
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
!JadeAutoSystemBrowserPresenter categoriesFor: #restoreFrom:!history!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #updateAndSelect:!public!updating! !
!JadeAutoSystemBrowserPresenter categoriesFor: #updateMethodList!public!updating! !

