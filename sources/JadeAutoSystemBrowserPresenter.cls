"Filed out from Dolphin Smalltalk 7"!

JadeSystemBrowserPresenter subclass: #JadeAutoSystemBrowserPresenter
	instanceVariableNames: 'lastGsShape'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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
		when: #'hoverStart:'						send: #'methodHoverStart:'			to: self;
		when: #'hoverEnd:'						send: #'methodHoverEnd:'				to: self;
		when: #'aboutToDisplayMenu:'		send: #'methodMenu:'					to: self;
		when: #'leftButtonDoubleClicked:'	send: #'methodDoubleClicked:'		to: self;
		when: #'valueChanged'					send: #'methodValueChanged'		to: self;
		when: #'focusLost'						send: #'cancelCallTip'					to: methodSourcePresenter view;
		yourself.
!

getClassHierarchy

	^classHierarchyPresenter selection!

getClassName

	^classHierarchyPresenter selection last!

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

