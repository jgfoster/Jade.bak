| package |
package := Package name: 'Jade Autocompletation'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.014'.


package classNames
	add: #JadeAutocompletationConfiguration;
	add: #JadeAutocompletationConfigurationPresenter;
	add: #JadeAutocompletationConfigurationShell;
	add: #JadeAutocompletationRegistry;
	add: #JadeAutoMap;
	add: #JadeAutoTextPresenter;
	add: #JadeGsClassShape;
	add: #JadeMapAutocompletationPresenter;
	add: #JadeParamAutocompletationPresenter;
	add: #JadeRegistryPresenter;
	yourself.

package methodNames
	add: #CodeSourcePresenter -> #createComponents;
	add: #JadePresenter -> #registry;
	add: #JadeTextDocument -> #registry;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Boolean\Dolphin Boolean Presenter';
	add: '..\Object Arts\Dolphin\MVP\Views\Cards\Dolphin Card Containers';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'Jade UI Base';
	add: '..\Object Arts\Dolphin\System\Compiler\Smalltalk Parser';
	yourself).

package!

"Class Definitions"!

Object subclass: #JadeAutocompletationConfiguration
	instanceVariableNames: 'isEnabled logEnabled filterObjectMethods filterPrimitiveMethods hideClassVars hideInstClassVars hidePoolDictionaries'
	classVariableNames: 'Default'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #JadeAutocompletationRegistry
	instanceVariableNames: 'registry variableMap paramMap'
	classVariableNames: 'Default'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #JadeAutoMap
	instanceVariableNames: 'name gsClassShapeName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #JadeGsClassShape
	instanceVariableNames: 'name gsInstVariables gsClassVariables gsInstClassVariables gsPoolDictionaries gsMethods gsClassMethods gsClassHierarchy'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter subclass: #JadeAutocompletationConfigurationPresenter
	instanceVariableNames: 'isEnabledPresenter logEnabledPresenter filterObjectPresenter filterPrimitivePresenter hideClassVarPresenter hideInstClassVarPresenter hidePoolDictionariesPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter subclass: #JadeMapAutocompletationPresenter
	instanceVariableNames: 'namePresenter gsClassListPresenter mapPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter subclass: #JadeParamAutocompletationPresenter
	instanceVariableNames: 'methodNamePresenter paramPresenter paramsPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter subclass: #JadeRegistryPresenter
	instanceVariableNames: 'registryPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #JadeAutocompletationConfigurationShell
	instanceVariableNames: 'configurationComposite registryComposite mapingComposite paramComposite'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeTextPresenter subclass: #JadeAutoTextPresenter
	instanceVariableNames: 'lastGsShape lastWord lastCharacter currentWord temporaries arguments altMode'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!CodeSourcePresenter methodsFor!

createComponents

	super createComponents.
	documentPresenter := self add: JadeAutoTextPresenter new name: 'document'.
	self updateCodeFont.
! !
!CodeSourcePresenter categoriesFor: #createComponents!public! !

!JadePresenter methodsFor!

registry

	^JadeAutocompletationRegistry default! !
!JadePresenter categoriesFor: #registry!autocompletion!public! !

!JadeTextDocument methodsFor!

registry

	^JadeAutocompletationRegistry default! !
!JadeTextDocument categoriesFor: #registry!autocompletion!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

JadeAutocompletationConfiguration guid: (GUID fromString: '{001E8CDA-0541-471A-9188-4AD4B58206EF}')!
JadeAutocompletationConfiguration comment: ''!
!JadeAutocompletationConfiguration categoriesForClass!Unclassified! !
!JadeAutocompletationConfiguration methodsFor!

filterObjectMethods
	^filterObjectMethods!

filterObjectMethods: anObject
	filterObjectMethods := anObject!

filterPrimitiveMethods
	^filterPrimitiveMethods!

filterPrimitiveMethods: anObject
	filterPrimitiveMethods := anObject!

hideClassVars
	^hideClassVars!

hideClassVars: anObject
	hideClassVars := anObject!

hideInstClassVars
	^hideInstClassVars!

hideInstClassVars: anObject
	hideInstClassVars := anObject!

hidePoolDictionaries
	^hidePoolDictionaries!

hidePoolDictionaries: anObject
	hidePoolDictionaries := anObject!

initialize

	super initialize.

	filterObjectMethods := false.
	filterPrimitiveMethods := false.
	isEnabled := true.
	hideClassVars := false.
	hideInstClassVars := false.
	hidePoolDictionaries := false.

	isEnabled := false.		"disable for initial release"
	logEnabled := false.		"off by default"!

isEnabled
	^isEnabled!

isEnabled: anObject
	isEnabled := anObject!

logEnabled
	^logEnabled!

logEnabled: anObject
	logEnabled := anObject! !
!JadeAutocompletationConfiguration categoriesFor: #filterObjectMethods!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #filterObjectMethods:!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #filterPrimitiveMethods!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #filterPrimitiveMethods:!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #hideClassVars!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #hideClassVars:!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #hideInstClassVars!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #hideInstClassVars:!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #hidePoolDictionaries!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #hidePoolDictionaries:!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #initialize!public! !
!JadeAutocompletationConfiguration categoriesFor: #isEnabled!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #isEnabled:!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #logEnabled!accessing!private! !
!JadeAutocompletationConfiguration categoriesFor: #logEnabled:!accessing!private! !

!JadeAutocompletationConfiguration class methodsFor!

default

	Default ifNil: [Default := super new initialize].

	^Default! !
!JadeAutocompletationConfiguration class categoriesFor: #default!public! !

JadeAutocompletationRegistry guid: (GUID fromString: '{A71207C7-78A5-41B5-8B3C-BE4A42FD676F}')!
JadeAutocompletationRegistry comment: 'This class is used to store JadeGsClassShape.

With a collection of instances of JadeGsClassShape this class can hold information about GemStone/S classes and methods.

Getting information from JadeAutoTextPresenter about the current and last word being typed it can guess the autocompletation.'!
!JadeAutocompletationRegistry categoriesForClass!Unclassified! !
!JadeAutocompletationRegistry methodsFor!

configuration

	^JadeAutocompletationConfiguration default!

defaultFileName

	^SessionManager current imageBase, 'autocompletationInfo.obj'!

fileOut
	| fileStream |

	fileStream := FileStream write: self defaultFileName text: false.
	self binaryStoreOn: fileStream.
	fileStream flush.
	fileStream close.!

getAllClassesNames
	"Answer a collection of all classes name"

	^registry keys!

getAllClassMethods
	"Answer a collection with all class methods in the receiver"

	^(registry inject: OrderedCollection new into: [:all :each | all addAll: each gsClassMethods asSortedCollection. all]) copyWithoutDuplicates!

getAllClassMethodsButObject
	"Answer a collection of all methods in the registry but NOT Object Class methods"
	| keys methods |

	keys := registry keys remove: 'Object'; yourself.
	methods := SortedCollection new.

	keys do: [:eachKey | methods addAll: (registry at: eachKey)  gsClassMethods].

	^methods copyWithoutDuplicates!

getAllClassVarOf: aJadeGsClassShape
	"Answer a collection with all instance variable names of argument <aJadeGsClassShape> including inst var of super classes"
	| vars |

	vars := OrderedCollection new.

	aJadeGsClassShape gsClassHierarchy "iterate over class shape hierarchy to get superclasses inst var names but may be the superclass is NOT in the registry"
		do: [:each | (self includesClassNamed: each) ifTrue: [vars addAll: (self getClass: each) gsClassVariables] ].

	^vars asSortedCollection
!

getAllGlobalMethods
	"Answer all methods in the receiver"
	| methods  | 

	methods := self configuration filterObjectMethods ifTrue: [self getAllMethodsButObject] ifFalse: [self getAllMethods]. 

	self configuration filterPrimitiveMethods ifTrue: [^methods select: [:each | each first ~=  $_] ].

	^methods copyWithoutDuplicates asSortedCollection

!

getAllInstClassVarOf: aJadeGsClassShape
	"Answer a collection with all instance variable names of argument <aJadeGsClassShape> including inst var of super classes"
	| vars |

	vars := OrderedCollection new.

	aJadeGsClassShape gsClassHierarchy "iterate over class shape hierarchy to get superclasses inst var names but may be the superclass is NOT in the registry"
		do: [:each | (self includesClassNamed: each) ifTrue: [vars addAll: (self getClass: each) gsInstClassVariables] ].

	^vars asSortedCollection
!

getAllInstVarNamesOf: aJadeGsClassShape
	"Answer a collection with all instance variable names of argument <aJadeGsClassShape> including inst var of super classes"
	| instVar |

	instVar := OrderedCollection new.

	aJadeGsClassShape gsClassHierarchy "iterate over class shape hierarchy to get superclasses inst var names but may be the superclass is NOT in the registry"
		do: [:each | (self includesClassNamed: each) ifTrue: [instVar addAll: (self getClass: each) gsInstVariables ] ].

	^instVar asSortedCollection
!

getAllMethods
	"Answer of all methods in the receiver"

	^(registry inject: OrderedCollection new into: [:all :each | all addAll: each gsMethods asSortedCollection. all]) copyWithoutDuplicates!

getAllMethodsButObject
	"Answer of all methods in the receiver but NOT Object methods"
	| keys methods |

	keys := registry keys remove: 'Object' ifAbsent:[]; yourself.
	methods := SortedCollection new.

	keys do: [:eachKey | | currentClassMethods |
		currentClassMethods := (registry at: eachKey)  gsMethods select: [:each | each class ==  String or:[each class == Symbol]].
		methods addAll: currentClassMethods].

	^methods copyWithoutDuplicates!

getAllMethodsFor: aJadeGsClassShape
	"Answer a collection of methods of <aJadeGsClassShape> including superclasses methods"
	| methods hierarchy |

	methods := OrderedCollection new.
	hierarchy := aJadeGsClassShape gsClassHierarchy asOrderedCollection.

	self configuration filterObjectMethods ifTrue: [hierarchy := hierarchy removeFirst; yourself ]. "we remove Object class from <hierarchy> Array"

	hierarchy do: [:each | | methodsToDisplay |
		methodsToDisplay := (self includesClassNamed: each) ifTrue: [(self getClass: each) gsMethods] ifFalse: [OrderedCollection new].
		self configuration filterPrimitiveMethods ifTrue: [methodsToDisplay := methodsToDisplay reject: [:eachSymbol | eachSymbol first = $_]].
		methods addAll: methodsToDisplay].

	^methods copyWithoutDuplicates

!

getAllPoolDictionariesOf: aJadeGsClassShape
	"Answer a collection with all instance variable names of argument <aJadeGsClassShape> including inst var of super classes"
	| pools |

	pools := OrderedCollection new.

	aJadeGsClassShape gsClassHierarchy "iterate over class shape hierarchy to get superclasses inst var names but may be the superclass is NOT in the registry"
		do: [:each | (self includesClassNamed: each) ifTrue: [pools addAll: (self getClass: each) gsPoolDictionaries] ].

	^pools asSortedCollection
!

getAllVarNamesOf: aJadeGsClassShape
	"Answer a collection with all instance variable names of argument <aJadeGsClassShape> including inst var of super classes"
	| allVarNames |

	allVarNames := self getAllInstVarNamesOf: aJadeGsClassShape.

	self configuration hideClassVars ifFalse: [allVarNames addAll: (self getAllClassVarOf: aJadeGsClassShape)].
	self configuration hideInstClassVars ifFalse: [allVarNames addAll: (self getAllInstClassVarOf: aJadeGsClassShape)].
	self configuration hidePoolDictionaries ifFalse: [allVarNames addAll: (self getAllPoolDictionariesOf: aJadeGsClassShape)].

	^allVarNames
!

getClass: aString
	"The receiver answer the register JadeGsClassShape for the name <aString> if none answer <nil>"

	^registry at: aString asString ifAbsent: [nil]!

getClassesNamedWith: aString
	"Answer a collection of JadeGsClassShape which it's name includes <aString> as subcollection"

	^self getAllClassesNames reject: [:each | (each indexOfSubCollection: aString trimBlanks) isZero]!

hasMapFor: aString
	"Answer whether the receiver has map for arg <aString>"

	^variableMap includesKey: aString!

hasMapForClassNamed: aString
	"Answer whether the receiver has a map in <variableMap> dictionary for target <aString>"

	^variableMap anySatisfy: [:each | each name = aString]!

includesClassNamed: aString
	"Answer whether the receiver has registered a class named <aString>"

	^registry includesKey: aString!

initialize

	super initialize.

	registry := Dictionary new.
	variableMap := Dictionary new.
	paramMap := Dictionary new.!

mapFor: aString
	"Answer the map (instance of JadeGsClassShape) for the name <aString> "

	^variableMap at: aString ifAbsent: [nil]!

mapToDisplay
	"Format <variableMap> to be displayed in a Presenter"
	| collection |

	collection := OrderedCollection new.
	variableMap keys do: [:eachKey | collection add: (eachKey, '>>', (variableMap at: eachKey) name)].

	^collection!

paramFor: methodName
	"Answer the text for a parameter in the method named <methodName>, if none answer 'param' "

	^paramMap at: methodName ifAbsent: ['param']!

paramFor: methodName index: index
	"Answer the text for a parameter in the method named <methodName>, if none answer 'param' "
	| paramText |

	(paramMap includesKey: methodName) ifFalse: [^'param'].

	paramText := paramMap at: methodName.

	(paramText includes: $,)  ifFalse: [^paramText]. "the name is the same for all parameters"

	((methodName subStrings: ':') size = (paramText subStrings: ',') size) ifFalse: [^paramText]. "the number of arguments in <methodName> must be equel to number of parameter names in <paramText>"

	^(paramText subStrings: ',') at: index "pickup the paramter name "!

paramMap
	^paramMap!

paramMap: anObject
	paramMap := anObject!

paramsToDisplay
	"Format <paramMap> to be displayed in a Presenter"
	| collection |

	collection := SortedCollection new.
	paramMap keys do: [:eachKey | collection add: (eachKey, '>>', (paramMap at: eachKey))].

	^collection!

register: jadeClass
	"The receiver register the JadeGsClassShape instance <jadeClass>.
	- if there is a registered map for JadeGsClassShape instance <jadeClass> then update the class of the map with this new updated instance <jadeClass>.
	- register the class"

	(self hasMapForClassNamed: jadeClass name) ifTrue: [self updateGsClassForMapStrings: jadeClass]. "update the JadeGsClassShape of an existing map"

	^registry at: jadeClass name put: jadeClass "register the class"!

registerMap: mapString gsClassName: aString
	"The receiver register a mapping between a text <mapString> and a JadeGsClassShape named <aString>.
	For example: 'html'>>WAHtmlCanvas.
	Each time 'html' is typed the autocompletation engine will map it to a WAHtmlCanvas showing it's methods (very usefull at development)"

	^variableMap at: mapString put: (self getClass: aString)!

registerParamFor: methodName param: aString
	"The receiver register a parameter text <aString> for the method name <methodName>.
	For example: 'select:'>>'[:each | ]'
	Each time 'select:' is typed the autocompletation engine will put '[:each | each]' as the text for the parameter "

	^paramMap at: methodName put: aString!

registry
	^registry!

registry: anObject
	registry := anObject!

registryAsSortedCollection
	"Answer a SortedCollection with the values of the registry JadeGsClassShape instances"

	^registry asSortedCollection: [:a :b | a name <= b name]!

setAsDefault
	"Set the receiver as the default registry. Use this in the fileOut and fileIn process"

	^self class default: self!

unregister: aString
	"The receiver unregister the class JadeGsClassShape <jadeClass>"

	^registry removeKey: aString ifAbsent: [nil].!

unregisterClassNamed: className
	"The receiver unregister the class named <className>"

	^registry removeKey: className ifAbsent: []!

unregisterMap: mapString 
	"The receiver unregister the map named <mapString>"

	^variableMap removeKey: mapString ifAbsent: []!

unregisterParam: methodName 
	"The receiver unregister the method named <methodName> for parameter substitution"

	^paramMap removeKey: methodName!

updateGsClassForMapStrings: aJadeGsClassShape
	"The receiver update all variables map text that hold a class with same name as <aJadeGsClassShape> with this new update instance <aJadeGsClassShape>"
	| keysToUpdate |
	
	keysToUpdate := variableMap keys select: [:eachKey | (variableMap at: eachKey) name = aJadeGsClassShape name]. "select keys which it's value is equal to <aJadeGsClassShape> name"

	keysToUpdate do: [:eachKey | variableMap at: eachKey put: aJadeGsClassShape] "we update those keys with this new update instance"!

variableMap
	^variableMap!

variableMap: anObject
	variableMap := anObject! !
!JadeAutocompletationRegistry categoriesFor: #configuration!public! !
!JadeAutocompletationRegistry categoriesFor: #defaultFileName!public! !
!JadeAutocompletationRegistry categoriesFor: #fileOut!public! !
!JadeAutocompletationRegistry categoriesFor: #getAllClassesNames!public! !
!JadeAutocompletationRegistry categoriesFor: #getAllClassMethods!public! !
!JadeAutocompletationRegistry categoriesFor: #getAllClassMethodsButObject!public! !
!JadeAutocompletationRegistry categoriesFor: #getAllClassVarOf:!public! !
!JadeAutocompletationRegistry categoriesFor: #getAllGlobalMethods!public! !
!JadeAutocompletationRegistry categoriesFor: #getAllInstClassVarOf:!public! !
!JadeAutocompletationRegistry categoriesFor: #getAllInstVarNamesOf:!public! !
!JadeAutocompletationRegistry categoriesFor: #getAllMethods!public! !
!JadeAutocompletationRegistry categoriesFor: #getAllMethodsButObject!public! !
!JadeAutocompletationRegistry categoriesFor: #getAllMethodsFor:!public! !
!JadeAutocompletationRegistry categoriesFor: #getAllPoolDictionariesOf:!public! !
!JadeAutocompletationRegistry categoriesFor: #getAllVarNamesOf:!public! !
!JadeAutocompletationRegistry categoriesFor: #getClass:!public! !
!JadeAutocompletationRegistry categoriesFor: #getClassesNamedWith:!public! !
!JadeAutocompletationRegistry categoriesFor: #hasMapFor:!public! !
!JadeAutocompletationRegistry categoriesFor: #hasMapForClassNamed:!public! !
!JadeAutocompletationRegistry categoriesFor: #includesClassNamed:!public! !
!JadeAutocompletationRegistry categoriesFor: #initialize!public! !
!JadeAutocompletationRegistry categoriesFor: #mapFor:!public! !
!JadeAutocompletationRegistry categoriesFor: #mapToDisplay!public! !
!JadeAutocompletationRegistry categoriesFor: #paramFor:!public! !
!JadeAutocompletationRegistry categoriesFor: #paramFor:index:!public! !
!JadeAutocompletationRegistry categoriesFor: #paramMap!accessing!private! !
!JadeAutocompletationRegistry categoriesFor: #paramMap:!accessing!private! !
!JadeAutocompletationRegistry categoriesFor: #paramsToDisplay!public! !
!JadeAutocompletationRegistry categoriesFor: #register:!public! !
!JadeAutocompletationRegistry categoriesFor: #registerMap:gsClassName:!public! !
!JadeAutocompletationRegistry categoriesFor: #registerParamFor:param:!public! !
!JadeAutocompletationRegistry categoriesFor: #registry!accessing!private! !
!JadeAutocompletationRegistry categoriesFor: #registry:!accessing!private! !
!JadeAutocompletationRegistry categoriesFor: #registryAsSortedCollection!public! !
!JadeAutocompletationRegistry categoriesFor: #setAsDefault!public! !
!JadeAutocompletationRegistry categoriesFor: #unregister:!public! !
!JadeAutocompletationRegistry categoriesFor: #unregisterClassNamed:!public! !
!JadeAutocompletationRegistry categoriesFor: #unregisterMap:!public! !
!JadeAutocompletationRegistry categoriesFor: #unregisterParam:!public! !
!JadeAutocompletationRegistry categoriesFor: #updateGsClassForMapStrings:!public! !
!JadeAutocompletationRegistry categoriesFor: #variableMap!accessing!private! !
!JadeAutocompletationRegistry categoriesFor: #variableMap:!accessing!private! !

!JadeAutocompletationRegistry class methodsFor!

default

	Default ifNil: [Default := super new initialize].

	^Default!

default: aJadeAutocompletationRegistry

	^Default := aJadeAutocompletationRegistry
!

newFromFile: pathFile
	| fileStream obj |

	fileStream := FileStream read: pathFile text: false.
	obj := self binaryReadFrom: fileStream.
	fileStream close.
	^obj! !
!JadeAutocompletationRegistry class categoriesFor: #default!public! !
!JadeAutocompletationRegistry class categoriesFor: #default:!public! !
!JadeAutocompletationRegistry class categoriesFor: #newFromFile:!public! !

JadeAutoMap guid: (GUID fromString: '{15764AC6-5FD1-4C64-94F6-4DFE155108D4}')!
JadeAutoMap comment: 'This class is used to map <names> with JadeGsClassShape.

The idea is to create instances of this classes to relate names with autocompleation classes.

JadeAutoMap new name: ''html'' map: (JadeAutocompletationRegistry default getClass: ''WAHtmlCanvas'').

Each time ''html'' is used as variable then autocompletation it will assume that <html> is an instances of WAHtmlCanvas.'!
!JadeAutoMap categoriesForClass!Unclassified! !
!JadeAutoMap methodsFor!

gsClassShapeName
	^gsClassShapeName!

gsClassShapeName: anObject
	gsClassShapeName := anObject!

name
	^name!

name: anObject
	name := anObject! !
!JadeAutoMap categoriesFor: #gsClassShapeName!accessing!private! !
!JadeAutoMap categoriesFor: #gsClassShapeName:!accessing!private! !
!JadeAutoMap categoriesFor: #name!accessing!private! !
!JadeAutoMap categoriesFor: #name:!accessing!private! !

JadeGsClassShape guid: (GUID fromString: '{706D0CB1-9735-4DB8-9D5A-45B1CA2C55A3}')!
JadeGsClassShape comment: 'JadeGsClassShape represent individual information about GemStone/S classes.

Instance of this class are only created through the user interface JadeAutoSystemBrowserPresenter.

Each time a class is selected in the hierarchy browser an instance of this class is created and updated in the registry (JadeAutocompletationRegistry).

The Autocompletation engine use instance of this class to show autocompletation options.'!
!JadeGsClassShape categoriesForClass!Unclassified! !
!JadeGsClassShape methodsFor!

configuration
	^JadeAutocompletationConfiguration default!

existInRegistry

	^self registry includesClassNamed: name!

getAllInstVarNames

	^self registry getAllInstVarNamesOf: self

!

getAllMethods

	^self registry getAllMethodsFor: self!

getAllVarNames

	^self registry getAllVarNamesOf: self

!

getNamesAfterAssigment

	^self getAllVarNames
	"^self getAllInstVarNames"

	"^(self getAllInstVarNames, self getAllClassesNames asSortedCollection) asSortedCollection."

	!

gsClassHierarchy
	^gsClassHierarchy!

gsClassHierarchy: anObject
	gsClassHierarchy := anObject!

gsClassMethods
	^gsClassMethods!

gsClassMethods: anObject
	gsClassMethods := anObject!

gsClassVariables
	^gsClassVariables!

gsClassVariables: anObject
	gsClassVariables := anObject!

gsInstClassVariables
	^gsInstClassVariables!

gsInstClassVariables: anObject
	gsInstClassVariables := anObject!

gsInstVariables
	^gsInstVariables!

gsInstVariables: anObject
	gsInstVariables := anObject!

gsMethods
	^gsMethods!

gsMethods: anObject
	gsMethods := anObject!

gsPoolDictionaries
	^gsPoolDictionaries!

gsPoolDictionaries: anObject
	gsPoolDictionaries := anObject!

initialize

	super initialize.

	gsInstVariables := OrderedCollection new.
	gsClassVariables := OrderedCollection new.
	gsInstClassVariables := OrderedCollection new.
	gsPoolDictionaries := OrderedCollection new.
	gsMethods := LookupTable new.
	gsClassMethods := LookupTable new.!

name
	^name!

name: anObject
	name := anObject!

registry

	^JadeAutocompletationRegistry default!

update
	| previousShape |

	self existInRegistry ifFalse: [^self registry register: self].

	previousShape := self registry getClass: name.

	gsMethods isEmpty ifTrue: [gsMethods := previousShape gsMethods].
	gsClassMethods isEmpty ifTrue: [gsClassMethods := previousShape gsClassMethods].

	^self registry register: self! !
!JadeGsClassShape categoriesFor: #configuration!public! !
!JadeGsClassShape categoriesFor: #existInRegistry!public! !
!JadeGsClassShape categoriesFor: #getAllInstVarNames!public! !
!JadeGsClassShape categoriesFor: #getAllMethods!auto methods!public! !
!JadeGsClassShape categoriesFor: #getAllVarNames!public! !
!JadeGsClassShape categoriesFor: #getNamesAfterAssigment!public! !
!JadeGsClassShape categoriesFor: #gsClassHierarchy!accessing!private! !
!JadeGsClassShape categoriesFor: #gsClassHierarchy:!accessing!private! !
!JadeGsClassShape categoriesFor: #gsClassMethods!accessing!private! !
!JadeGsClassShape categoriesFor: #gsClassMethods:!accessing!private! !
!JadeGsClassShape categoriesFor: #gsClassVariables!accessing!private! !
!JadeGsClassShape categoriesFor: #gsClassVariables:!accessing!private! !
!JadeGsClassShape categoriesFor: #gsInstClassVariables!accessing!private! !
!JadeGsClassShape categoriesFor: #gsInstClassVariables:!accessing!private! !
!JadeGsClassShape categoriesFor: #gsInstVariables!accessing!private! !
!JadeGsClassShape categoriesFor: #gsInstVariables:!accessing!private! !
!JadeGsClassShape categoriesFor: #gsMethods!accessing!private! !
!JadeGsClassShape categoriesFor: #gsMethods:!accessing!private! !
!JadeGsClassShape categoriesFor: #gsPoolDictionaries!accessing!private! !
!JadeGsClassShape categoriesFor: #gsPoolDictionaries:!accessing!private! !
!JadeGsClassShape categoriesFor: #initialize!public! !
!JadeGsClassShape categoriesFor: #name!accessing!private! !
!JadeGsClassShape categoriesFor: #name:!accessing!private! !
!JadeGsClassShape categoriesFor: #registry!public! !
!JadeGsClassShape categoriesFor: #update!public! !

JadeAutocompletationConfigurationPresenter guid: (GUID fromString: '{6841F57C-9954-4A74-8098-0BE96F0070ED}')!
JadeAutocompletationConfigurationPresenter comment: ''!
!JadeAutocompletationConfigurationPresenter categoriesForClass!Unclassified! !
!JadeAutocompletationConfigurationPresenter methodsFor!

createComponents

	super createComponents.

	isEnabledPresenter := self add: BooleanPresenter new name: 'isEnabled'.
	filterObjectPresenter := self add: BooleanPresenter new name: 'filterObject'. 
	filterPrimitivePresenter := self add: BooleanPresenter new name: 'filterPrimitive'.
	hideClassVarPresenter := self add: BooleanPresenter new name: 'hideClassVar'.
	hideInstClassVarPresenter := self add: BooleanPresenter new name: 'hideInstClassVar'.
	hidePoolDictionariesPresenter := self add: BooleanPresenter new name: 'hidePool'.
	logEnabledPresenter := self add: BooleanPresenter new name: 'logEnabled'.!

model: aJadeAutocompletationConfiguration

	super model: aJadeAutocompletationConfiguration.

	isEnabledPresenter model: (aJadeAutocompletationConfiguration aspectValue: #isEnabled).
	filterObjectPresenter model: (aJadeAutocompletationConfiguration aspectValue: #filterObjectMethods).
	filterPrimitivePresenter model: (aJadeAutocompletationConfiguration aspectValue: #filterPrimitiveMethods).

	hideClassVarPresenter model: (aJadeAutocompletationConfiguration aspectValue: #hideClassVars).
	hideInstClassVarPresenter model: (aJadeAutocompletationConfiguration aspectValue: #hideInstClassVars).
	hidePoolDictionariesPresenter model: (aJadeAutocompletationConfiguration aspectValue: #hidePoolDictionaries).

	logEnabledPresenter model: (aJadeAutocompletationConfiguration aspectValue: #logEnabled).


	! !
!JadeAutocompletationConfigurationPresenter categoriesFor: #createComponents!public! !
!JadeAutocompletationConfigurationPresenter categoriesFor: #model:!public! !

!JadeAutocompletationConfigurationPresenter class methodsFor!

defaultModel

	^JadeAutocompletationConfiguration default!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 0 5 0 0 0 416 0 234 256 98 14 410 8 ##(Smalltalk.CheckBox)  98 16 0 416 98 2 8 1409363203 1 560 721990 2 ##(Smalltalk.ValueHolder)  0 0 1114118 ##(Smalltalk.NeverSearchPolicy)  32 0 0 5 0 0 0 560 0 8 4294903817 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  31 291 882 411 41 560 818 8 #text: 98 1 8 'Hide class variables' 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 145 0 0 0 220 0 0 0 165 0 0 0] 98 0 882 193 193 0 27 8 'hideClassVar' 410 576 98 16 0 416 98 2 8 1409363203 1 1088 642 0 0 688 32 0 0 5 0 0 0 1088 0 8 4294903817 722 0 0 0 754 202 208 98 2 818 848 98 2 882 31 211 882 411 41 1088 818 944 98 1 8 'Filter Methods starting with "_"' 1088 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 105 0 0 0 220 0 0 0 125 0 0 0] 98 0 1056 0 27 8 'filterPrimitive' 410 576 98 16 0 416 98 2 8 1409363203 1 1424 642 0 0 688 32 0 0 5 0 0 0 1424 0 8 4294903817 722 0 0 0 754 202 208 98 2 818 848 98 2 882 31 441 882 411 41 1424 818 944 98 1 8 'Hide pool dictionaries' 1424 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 220 0 0 0 220 0 0 0 240 0 0 0] 98 0 1056 0 27 8 'hidePoolDictionaries' 410 576 98 16 0 416 98 2 8 1409363203 1 1760 642 0 0 688 32 0 0 5 0 0 0 1760 0 8 4294903817 722 0 0 0 754 202 208 98 2 818 848 98 2 882 31 371 882 411 41 1760 818 944 98 1 8 'Hide instance class variables' 1760 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 185 0 0 0 220 0 0 0 205 0 0 0] 98 0 1056 0 27 8 'hideInstClassVar' 410 576 98 16 0 416 98 2 8 1409363203 1 2096 642 0 0 688 32 0 0 5 0 0 0 2096 0 8 4294903817 722 0 0 0 754 202 208 98 2 818 848 98 2 882 31 51 882 411 41 2096 818 944 98 1 8 'Autocompletation is enabled' 2096 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 25 0 0 0 220 0 0 0 45 0 0 0] 98 0 1056 0 27 8 'isEnabled' 410 576 98 16 0 416 98 2 8 1409363203 1 2432 642 0 0 688 32 0 0 5 0 0 0 2432 0 8 4294903817 722 0 0 0 754 202 208 98 2 818 848 98 2 882 31 131 882 411 41 2432 818 944 98 1 8 'Filter Object Methods' 2432 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 65 0 0 0 220 0 0 0 85 0 0 0] 98 0 1056 0 27 8 'filterObject' 410 576 98 16 0 416 98 2 8 1409363203 1 2768 642 0 0 688 32 0 0 5 0 0 0 2768 0 8 4294903817 722 0 0 0 754 202 208 98 2 818 848 98 2 882 31 521 882 381 41 2768 818 944 98 1 8 'Log is enabled' 2768 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 4 1 0 0 205 0 0 0 24 1 0 0] 98 0 1056 0 27 8 'logEnabled' 0 754 202 208 98 1 818 848 98 2 882 2731 21 882 511 611 416 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 23 5 0 0 10 0 0 0 22 6 0 0 59 1 0 0] 98 7 2096 2432 1088 560 1760 1424 2768 1056 0 27 )! !
!JadeAutocompletationConfigurationPresenter class categoriesFor: #defaultModel!public! !
!JadeAutocompletationConfigurationPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

JadeMapAutocompletationPresenter guid: (GUID fromString: '{17F5F69A-C4A4-4311-BC87-175F9BE8852C}')!
JadeMapAutocompletationPresenter comment: ''!
!JadeMapAutocompletationPresenter categoriesForClass!Unclassified! !
!JadeMapAutocompletationPresenter methodsFor!

addMap

	namePresenter value isEmpty ifTrue: [^self].
	gsClassListPresenter hasSelection ifFalse: [^self].

	self registry registerMap: namePresenter value gsClassName: gsClassListPresenter selection name.

	self onViewOpened.!

createComponents

	super createComponents.

	namePresenter := self add: TextPresenter new name: 'name'.
	gsClassListPresenter := self add: ListPresenter new name: 'gsClassList'.
	mapPresenter := self add: ListPresenter new name: 'map'.!

gsClassListPresenter
	^gsClassListPresenter!

gsClassListPresenter: anObject
	gsClassListPresenter := anObject!

mapPresenter
	^mapPresenter!

mapPresenter: anObject
	mapPresenter := anObject!

namePresenter
	^namePresenter!

namePresenter: anObject
	namePresenter := anObject!

onViewOpened

	super onViewOpened.

	mapPresenter model: (ListModel on: self registry mapToDisplay).

	gsClassListPresenter model: (ListModel on: self registry registryAsSortedCollection)!

removeMap

	mapPresenter hasSelection ifFalse: [^self].

	self registry unregisterMap: (mapPresenter selection subStrings: '>>') first.

	self onViewOpened.! !
!JadeMapAutocompletationPresenter categoriesFor: #addMap!public! !
!JadeMapAutocompletationPresenter categoriesFor: #createComponents!public! !
!JadeMapAutocompletationPresenter categoriesFor: #gsClassListPresenter!accessing!private! !
!JadeMapAutocompletationPresenter categoriesFor: #gsClassListPresenter:!accessing!private! !
!JadeMapAutocompletationPresenter categoriesFor: #mapPresenter!accessing!private! !
!JadeMapAutocompletationPresenter categoriesFor: #mapPresenter:!accessing!private! !
!JadeMapAutocompletationPresenter categoriesFor: #namePresenter!accessing!private! !
!JadeMapAutocompletationPresenter categoriesFor: #namePresenter:!accessing!private! !
!JadeMapAutocompletationPresenter categoriesFor: #onViewOpened!public! !
!JadeMapAutocompletationPresenter categoriesFor: #removeMap!public! !

!JadeMapAutocompletationPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 0 517 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 576 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  2731 21 738 951 791 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 23 5 0 0 10 0 0 0 242 6 0 0 149 1 0 0] 98 2 410 432 98 15 0 416 98 2 8 1140850688 131073 848 0 0 0 5 0 0 0 848 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 848 98 2 8 1140916352 1025 944 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 944 0 8 4294903883 852486 ##(Smalltalk.NullConverter)  0 0 1 610 202 208 98 3 674 704 98 2 738 371 71 738 481 51 944 674 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 944 674 8 #isTextModified: 98 1 32 944 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 35 0 0 0 169 1 0 0 60 0 0 0] 98 0 738 193 193 0 27 8 'name' 410 8 ##(Smalltalk.ComboBox)  98 17 0 848 98 2 8 1412498947 1025 1440 590662 2 ##(Smalltalk.ListModel)  202 208 576 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  1026 8 4278190080 0 5 0 0 0 1440 0 8 4294903885 459270 ##(Smalltalk.Message)  8 #name 98 0 576 401 610 202 208 98 1 674 704 98 2 738 371 151 738 481 47 1440 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 75 0 0 0 169 1 0 0 98 0 0 0] 98 0 1408 0 27 8 'gsClassList' 0 610 202 208 98 1 674 704 98 2 738 1 1 738 951 395 848 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 219 1 0 0 197 0 0 0] 98 6 410 8 ##(Smalltalk.StaticText)  98 16 0 848 98 2 8 1140850944 1 2048 0 0 0 5 0 0 0 2048 0 8 4294903639 1090 0 0 0 610 202 208 98 2 674 704 98 2 738 71 81 738 211 41 2048 674 8 #text: 98 1 8 'Name to map' 2048 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 0 0 0 40 0 0 0 140 0 0 0 60 0 0 0] 98 0 1408 0 27 410 2064 98 16 0 848 98 2 8 1140850944 1 2384 0 0 0 5 0 0 0 2384 0 8 4294903639 1090 0 0 0 610 202 208 98 2 674 704 98 2 738 71 161 738 241 39 2384 674 2288 98 1 8 'GemStone Class' 2384 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 0 0 0 80 0 0 0 155 0 0 0 99 0 0 0] 98 0 1408 0 27 944 1440 410 8 ##(Smalltalk.PushButton)  98 20 0 848 98 2 8 1140924416 1 2688 0 0 0 5 0 0 0 2688 0 8 4294903669 1180998 4 ##(Smalltalk.CommandDescription)  8 #addMap 8 'Add' 1 1 0 0 32 0 0 0 610 202 208 98 3 674 704 98 2 738 711 231 738 141 51 2688 674 8 #isEnabled: 98 1 32 2688 674 2288 98 1 8 'Add' 2688 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 99 1 0 0 115 0 0 0 169 1 0 0 140 0 0 0] 98 0 1408 0 29 410 8 ##(Smalltalk.GroupBox)  98 14 0 848 98 2 8 1140850695 65 3104 0 1026 8 4278190080 0 5 0 0 0 3104 0 8 4294903669 610 202 208 98 2 674 704 98 2 738 41 21 738 841 271 3104 674 2288 98 1 8 'Create New Map' 3104 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 10 0 0 0 184 1 0 0 145 0 0 0] 98 0 1408 0 27 1408 0 27 410 432 98 15 0 416 98 2 8 1140850688 131073 3440 0 0 0 5 0 0 0 3440 530 234 240 576 32 234 256 98 2 410 8 ##(Smalltalk.ListView)  98 30 0 3440 98 2 8 1409355853 1025 3568 1522 202 208 576 0 1584 1026 8 4278190080 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 2786 8 #removeMap 8 'Remove Map' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 3568 0 8 4294903635 1650 8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name to Map' 401 8 #left 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  4 1 1648 8 'doIt' 8 '[:each | (each subStrings: ''>>'') first]' 8 #[32 105 17 29 177 160 106] 8 '>>' 8 #subStrings: 8 #first 4080 7 257 0 8 ##(Smalltalk.SortedCollection)  0 0 3568 0 1 0 0 4002 8 'Gs Class' 401 4048 4066 0 0 4098 4 1 1648 8 'doIt' 8 '[:each | (each subStrings: ''>>'') last]' 8 #[32 105 17 29 177 160 106] 8 '>>' 4192 8 #last 4272 7 257 0 1650 8 #<= 98 0 0 0 3568 0 1 0 0 8 #report 576 0 131169 0 0 610 202 208 98 3 674 704 98 2 738 1 1 738 951 397 3568 674 8 #contextMenu: 98 1 3728 3568 674 2288 98 1 8 'Name to Map' 3568 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 219 1 0 0 198 0 0 0] 98 0 1408 0 27 8 'map' 0 610 202 208 98 1 674 704 98 2 738 1 395 738 951 397 3440 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 197 0 0 0 219 1 0 0 139 1 0 0] 98 1 3568 1408 0 27 1408 0 27 )! !
!JadeMapAutocompletationPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

JadeParamAutocompletationPresenter guid: (GUID fromString: '{C2C838B3-59B2-4713-B655-86630F98210D}')!
JadeParamAutocompletationPresenter comment: ''!
!JadeParamAutocompletationPresenter categoriesForClass!Unclassified! !
!JadeParamAutocompletationPresenter methodsFor!

addParam

	methodNamePresenter value isEmpty ifTrue: [^self].
	paramPresenter value isEmpty ifTrue: [^self].

	self registry registerParamFor: methodNamePresenter value param: paramPresenter value.

	self onViewOpened.!

createComponents

	super createComponents.

	methodNamePresenter := self add: TextPresenter new name: 'methodName'.
	paramPresenter := self add: TextPresenter new name: 'param'.
	paramsPresenter := self add: ListPresenter new name: 'params'.!

onViewOpened

	super onViewOpened.

	paramsPresenter model: (ListModel on: self registry paramsToDisplay).

!

removeParam

	paramsPresenter hasSelection ifFalse: [^self].

	self registry unregisterParam: (paramsPresenter selection subStrings: '>>') first.

	self onViewOpened.! !
!JadeParamAutocompletationPresenter categoriesFor: #addParam!public! !
!JadeParamAutocompletationPresenter categoriesFor: #createComponents!public! !
!JadeParamAutocompletationPresenter categoriesFor: #onViewOpened!public! !
!JadeParamAutocompletationPresenter categoriesFor: #removeParam!public! !

!JadeParamAutocompletationPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 0 517 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 576 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  2731 21 738 951 791 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 23 5 0 0 10 0 0 0 242 6 0 0 149 1 0 0] 98 2 410 432 98 15 0 416 98 2 8 1140850688 131073 848 0 0 0 5 0 0 0 848 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 848 98 2 8 1140916352 1025 944 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 944 0 8 4294903883 852486 ##(Smalltalk.NullConverter)  0 0 1 610 202 208 98 3 674 704 98 2 738 371 161 738 481 51 944 674 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 944 674 8 #isTextModified: 98 1 32 944 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 80 0 0 0 169 1 0 0 105 0 0 0] 98 0 738 193 193 0 27 8 'param' 410 960 98 16 0 848 98 2 8 1140916352 1025 1440 0 1026 8 4278190080 0 5 0 0 0 1440 0 8 4294903883 1090 0 0 1 610 202 208 98 3 674 704 98 2 738 371 71 738 481 51 1440 674 1248 98 1 1282 3 1 3 1440 674 1328 98 1 32 1440 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 35 0 0 0 169 1 0 0 60 0 0 0] 98 0 1408 0 27 8 'methodName' 0 610 202 208 98 1 674 704 98 2 738 1 1 738 951 395 848 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 219 1 0 0 197 0 0 0] 98 6 410 8 ##(Smalltalk.StaticText)  98 16 0 848 98 2 8 1140850944 1 1984 0 0 0 5 0 0 0 1984 0 8 4294903639 1090 0 0 0 610 202 208 98 2 674 704 98 2 738 71 81 738 211 41 1984 674 8 #text: 98 1 8 'Method Name' 1984 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 0 0 0 40 0 0 0 140 0 0 0 60 0 0 0] 98 0 1408 0 27 410 2000 98 16 0 848 98 2 8 1140850944 1 2320 0 0 0 5 0 0 0 2320 0 8 4294903639 1090 0 0 0 610 202 208 98 2 674 704 98 2 738 71 161 738 241 39 2320 674 2224 98 1 8 'Param String' 2320 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 0 0 0 80 0 0 0 155 0 0 0 99 0 0 0] 98 0 1408 0 27 1440 944 410 8 ##(Smalltalk.PushButton)  98 20 0 848 98 2 8 1140924416 1 2624 0 0 0 5 0 0 0 2624 0 8 4294903669 1180998 4 ##(Smalltalk.CommandDescription)  8 #addParam 8 'Add' 1 1 0 0 32 0 0 0 610 202 208 98 3 674 704 98 2 738 711 231 738 141 51 2624 674 8 #isEnabled: 98 1 32 2624 674 2224 98 1 8 'Add' 2624 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 99 1 0 0 115 0 0 0 169 1 0 0 140 0 0 0] 98 0 1408 0 29 410 8 ##(Smalltalk.GroupBox)  98 14 0 848 98 2 8 1140850695 65 3040 0 1026 8 4278190080 0 5 0 0 0 3040 0 8 4294903669 610 202 208 98 2 674 704 98 2 738 41 21 738 841 271 3040 674 2224 98 1 8 'Create New Param' 3040 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 10 0 0 0 184 1 0 0 145 0 0 0] 98 0 1408 0 27 1408 0 27 410 432 98 15 0 416 98 2 8 1140850688 131073 3376 0 0 0 5 0 0 0 3376 530 234 240 576 32 234 256 98 2 410 8 ##(Smalltalk.ListView)  98 30 0 3376 98 2 8 1409355853 1025 3504 590662 2 ##(Smalltalk.ListModel)  202 208 576 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  1026 8 4278190080 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 2722 8 #removeParam 8 'Remove Param' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 3504 0 8 4294903635 459270 ##(Smalltalk.Message)  8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Method Name' 501 8 #left 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  4 1 3856 8 'doIt' 8 '[:each | (each subStrings: ''>>'') first]' 8 #[32 105 17 29 177 160 106] 8 '>>' 8 #subStrings: 8 #first 4080 7 257 0 8 ##(Smalltalk.SortedCollection)  0 0 3504 0 1 0 0 4002 8 'Parameters' 441 4048 4066 0 0 4098 4 1 3856 8 'doIt' 8 '[:each | (each subStrings: ''>>'') last]' 8 #[32 105 17 29 177 160 106] 8 '>>' 4192 8 #last 4272 7 257 0 3858 8 #<= 98 0 0 0 3504 0 1 0 0 8 #report 576 0 131169 0 0 610 202 208 98 3 674 704 98 2 738 1 1 738 951 397 3504 674 8 #contextMenu: 98 1 3712 3504 674 2224 98 1 8 'Method Name' 3504 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 219 1 0 0 198 0 0 0] 98 0 1408 0 27 8 'params' 0 610 202 208 98 1 674 704 98 2 738 1 395 738 951 397 3376 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 197 0 0 0 219 1 0 0 139 1 0 0] 98 1 3504 1408 0 27 1408 0 27 )! !
!JadeParamAutocompletationPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

JadeRegistryPresenter guid: (GUID fromString: '{ADFFFC60-58D5-4425-979F-30FDCEA64884}')!
JadeRegistryPresenter comment: ''!
!JadeRegistryPresenter categoriesForClass!Unclassified! !
!JadeRegistryPresenter methodsFor!

createComponents

	super createComponents.

	registryPresenter := self add: ListPresenter new name: 'registry'.!

deleteGsClassShape

	registryPresenter hasSelection ifFalse: [^self].
	(MessageBox confirm: 'Are sure to delete the selected Gs Class Shape ?') ifFalse: [^self].

	model unregisterClassNamed: registryPresenter selection.

	self model: self model.!

model: aJadeAutocompletationRegistry

	super model: aJadeAutocompletationRegistry.

	registryPresenter model: (ListModel on: aJadeAutocompletationRegistry getAllClassesNames asSortedCollection)

	! !
!JadeRegistryPresenter categoriesFor: #createComponents!public! !
!JadeRegistryPresenter categoriesFor: #deleteGsClassShape!public! !
!JadeRegistryPresenter categoriesFor: #model:!public! !

!JadeRegistryPresenter class methodsFor!

defaultModel

	^JadeAutocompletationRegistry default!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 0 517 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 576 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  2731 21 738 611 791 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 23 5 0 0 10 0 0 0 72 6 0 0 149 1 0 0] 98 1 410 432 98 15 0 416 98 2 8 1140850688 131073 848 0 0 0 5 0 0 0 848 530 234 240 576 32 234 256 98 2 410 8 ##(Smalltalk.ListView)  98 30 0 848 98 2 8 1409355853 1025 976 590662 2 ##(Smalltalk.ListModel)  202 208 576 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #deleteGsClassShape 8 'Delete Gs Class Shape' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 976 0 8 4294902979 459270 ##(Smalltalk.Message)  8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 'Class Name' 601 8 #left 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 1568 8 'doIt' 8 '[:each | each]' 1779525917 1584 7 257 0 8 ##(Smalltalk.SortedCollection)  0 0 976 0 1 0 0 8 #report 576 0 131169 0 0 610 202 208 98 3 674 704 98 2 738 1 1 738 611 791 976 674 8 #contextMenu: 98 1 1200 976 674 8 #text: 98 1 8 'Class Name' 976 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 49 1 0 0 139 1 0 0] 98 0 738 193 193 0 27 8 'registry' 0 610 202 208 98 1 674 704 98 2 738 1 1 738 611 791 848 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 49 1 0 0 139 1 0 0] 98 1 976 1968 0 27 1968 0 27 )! !
!JadeRegistryPresenter class categoriesFor: #defaultModel!public! !
!JadeRegistryPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

JadeAutocompletationConfigurationShell guid: (GUID fromString: '{FC1F72BB-A9F1-47FF-857D-675CF4028FA4}')!
JadeAutocompletationConfigurationShell comment: ''!
!JadeAutocompletationConfigurationShell categoriesForClass!Unclassified! !
!JadeAutocompletationConfigurationShell methodsFor!

createComponents
	super createComponents.

	configurationComposite := self add: JadeAutocompletationConfigurationPresenter new name: 'referee'.

	registryComposite := self add: JadeRegistryPresenter new name: 'registryReferee'.

	mapingComposite := self add: JadeMapAutocompletationPresenter new  name: 'mappingReferee'.

	paramComposite := self add: JadeParamAutocompletationPresenter new name: 'paramReferee'.!

model: anObject

	super model:  anObject.

	configurationComposite model: anObject.

	registryComposite model: self registry
	!

registry

	^JadeAutocompletationRegistry default! !
!JadeAutocompletationConfigurationShell categoriesFor: #createComponents!public! !
!JadeAutocompletationConfigurationShell categoriesFor: #model:!public! !
!JadeAutocompletationConfigurationShell categoriesFor: #registry!public! !

!JadeAutocompletationConfigurationShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 26214401 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 0 39 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 544 0 0 0 7 0 0 0 544 0 234 256 98 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 813 786 949 111 544 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 150 1 0 0 218 1 0 0 205 1 0 0] 98 1 410 8 ##(Smalltalk.PushButton)  98 20 0 544 98 2 8 1140924416 1 896 0 0 0 7 0 0 0 896 0 8 4294903566 1180998 4 ##(Smalltalk.CommandDescription)  8 #exit 8 'Close' 1 1 0 0 32 0 0 0 658 202 208 98 2 722 752 98 2 786 751 21 786 191 81 896 722 8 #text: 98 1 8 'Close' 896 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 1 0 0 10 0 0 0 214 1 0 0 50 0 0 0] 98 0 786 193 193 0 29 1280 0 27 0 0 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1409286144 131073 1296 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 7 0 0 0 1296 655878 ##(Smalltalk.CardLayout)  202 208 98 4 721414 ##(Smalltalk.Association)  8 'Options' 410 560 98 15 0 1296 98 2 8 1140850688 131073 1536 0 0 0 7 0 0 0 1536 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.GroupBox)  98 14 0 1536 98 2 8 1140850695 65 1664 0 1378 8 4278190080 0 7 0 0 0 1664 0 8 4294903566 658 202 208 98 2 722 752 98 2 786 27 31 786 821 681 1664 722 1184 98 1 8 'Autocompletation Options' 1664 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 13 0 0 0 15 0 0 0 167 1 0 0 99 1 0 0] 98 0 1280 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 27 2034 8 #fixedViewLeft 821 2034 8 #fixedParentTop 31 2034 8 #fixedViewTop 681 410 8 ##(Smalltalk.ReferenceView)  98 14 0 1536 98 2 8 1140850688 131073 2176 0 1378 8 4278190080 0 7 0 0 0 2176 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.JadeAutocompletationConfigurationPresenter)  8 #resource_Default_view 0 658 202 208 98 1 722 752 98 2 786 57 81 786 781 621 2176 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 28 0 0 0 40 0 0 0 162 1 0 0 94 1 0 0] 640 1280 0 27 2002 2048 57 2080 781 2112 81 2144 621 234 256 98 2 2176 8 'referee' 0 658 202 208 98 1 722 752 98 2 786 9 49 786 933 757 1536 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 214 1 0 0 146 1 0 0] 98 2 1664 2176 1280 0 27 1490 8 'Params Mapping' 410 2192 98 14 0 1296 98 2 8 1140850688 131073 2752 0 1378 8 4278190080 0 5 0 0 0 2752 2290 8 ##(Smalltalk.JadeParamAutocompletationPresenter)  2336 0 658 202 208 98 1 722 752 98 2 786 9 49 786 933 757 2752 834 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 214 1 0 0 146 1 0 0] 640 1280 0 27 1490 8 'Variable Name Mapping' 410 2192 98 14 0 1296 98 2 8 1140850688 131073 3056 0 1378 8 4278190080 0 5 0 0 0 3056 2290 8 ##(Smalltalk.JadeMapAutocompletationPresenter)  2336 0 658 202 208 98 1 722 752 98 2 786 9 49 786 933 757 3056 834 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 214 1 0 0 146 1 0 0] 640 1280 0 27 1490 8 'Registry' 410 2192 98 14 0 1296 98 2 8 1140850688 131073 3360 0 1378 8 4278190080 0 5 0 0 0 3360 2290 8 ##(Smalltalk.JadeRegistryPresenter)  2336 0 658 202 208 98 1 722 752 98 2 786 9 49 786 933 757 3360 834 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 214 1 0 0 146 1 0 0] 640 1280 0 27 1536 234 256 98 6 3056 8 'mappingReferee' 3360 8 'registryReferee' 2752 8 'paramReferee' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 1296 98 2 8 1140916736 1 3712 590662 2 ##(Smalltalk.ListModel)  202 208 98 4 1520 3344 3040 2736 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  0 0 1 0 0 0 3712 0 8 4294903494 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 8 #displayString 3920 7 257 0 3906 0 0 3938 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 4032 7 257 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 8 #noIcons 0 0 0 0 0 658 202 208 98 3 722 752 98 2 786 1 1 786 949 813 3712 722 8 #basicSelectionsByIndex: 98 1 98 1 3 3712 722 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 3712 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 1 0 0 150 1 0 0] 98 0 1280 0 27 658 202 208 98 1 722 752 98 2 786 1 1 786 949 813 1296 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 1 0 0 150 1 0 0] 98 5 1536 3360 3056 2752 3712 1280 0 27 234 256 640 0 0 0 0 0 1 0 0 0 0 1 0 0 658 202 208 98 3 722 752 98 2 786 2731 21 786 961 981 416 722 1184 98 1 8 'Jade Autocompletation Configuration' 416 722 8 #updateMenuBar 640 416 834 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 23 5 0 0 10 0 0 0 247 6 0 0 244 1 0 0] 98 2 1296 544 1280 0 27 )! !
!JadeAutocompletationConfigurationShell class categoriesFor: #resource_Default_view!public!resources-views! !

JadeAutoTextPresenter guid: (GUID fromString: '{F845E86E-2C61-4193-A936-E7C4D7078136}')!
JadeAutoTextPresenter comment: 'This class use the registry (JadeAutocompletationRegistry)  to get information about the GsClasses and methods.

It uses this information to show autocompletation options for the current text being typed.


see:
#onKeyPressed:'!
!JadeAutoTextPresenter categoriesForClass!Unclassified! !
!JadeAutoTextPresenter methodsFor!

autocompletationConfiguration
	^JadeAutocompletationConfiguration default!

autocompleteForAlphaNumeric
	"This method is triggered when an alpha numeric key is pressed.
	If ALT Mode is enabled then show the autocompletation for the Right Alt key.
	If the current typed word <currentWord> start with an uppercase character --> it is a Class name, a Class Variable name, a Class Inst Var name or a Pool dictionary.
	If NOT (lowercase) --> shows all possible names "

	currentWord trimBlanks isEmpty ifTrue: [^self].

	altMode ifTrue: [^self autocompleteForRightAlt].

	currentWord trimBlanks first isUppercase ifTrue: [	self logMethodName: 'autocompleteForAlphaNumeric - uppercase Classes names' info: #().
		^self showAutoCompletionList: self getClassesName prefixLength: 0 ]. "we have to add class var and class inst var ?"

self logMethodName: 'autocompleteForAlphaNumeric - All Names' info: #().
	^self showAutoCompletionList: self getNamesForAlphaNumeric prefixLength: 0!

autocompleteForEnter
	"This method is triggered when Enter key is pressed.
	The receiver store the current typed word <currentWord> into <lastWord>, reset the ALT Mode"

	self logMethodName: 'autocompleteForEnter' info: #().

	lastWord := currentWord.

	altMode := false.

	self clearAutocompletation.!

autocompleteForRightAlt
	"This method is triggered when Right Alt key is pressed.
	It shows all possible names after an assignment: method arguments, method temporaries and object's inst var, "

	self logMethodName: 'autocompleteForRightAlt' info: #().

	self showAutoCompletionList: self getNamesAfterAssigment prefixLength: 0.

	altMode := true.!

autocompleteForSpace
	"This method is triggered when <space> key is pressed.
	Update the last typed word <lastWord> and clear the autocompleation.
	Check the last typed word <lastWord> to guess what autocompletation list it will fit better for <lastWord>"

	self logMethodName: 'autocompleteForSpace' info: #().
	altMode := false.
	lastWord := currentWord isEmpty ifFalse: [currentWord] ifTrue: [lastWord].
	self clearAutocompletation.
	" -self- typed --> show methods for -self-"
	((lastWord trimBlanks asLowercase = 'self') and: [lastGsShape notNil]) 
		ifTrue: [^self showAutoCompletionList: self getMethodsForSelf prefixLength: 0. ].
	"-uppercase- typed and is a class name --> show class names"
	(lastWord notEmpty and: [((lastWord first isUppercase) and: [self registry includesClassNamed: lastWord trimBlanks])]) 
		ifTrue: [^self showAutoCompletionList: (self getClassMethodsFor: lastWord trimBlanks) prefixLength: 0.].
	" -registered var name- typed --> show the methods of the class related to the map (check JadeAutocompletationRegistry variableMap)"
	(self registry hasMapFor: lastWord trimBlanks) 
		ifTrue: [^self showAutoCompletionList: self getMappedStringToClass prefixLength: 0. ].
	"-assignment- typed --> show possible names after assignment"
	(lastWord trimBlanks notEmpty and: [lastWord trimBlanks = ';=' ]) 
		ifTrue: [^self showAutoCompletionList: self getNamesAfterAssigment prefixLength: 0.].	

	"otherwise it return all possible methods in the system"
	self showAutoCompletionList: self getMethodsForAnyone prefixLength: 0.

!

autocompleteForTab

	self showAutoCompletionList: self getNamesAfterAssigment prefixLength: 0.

!

clearAutocompletation
	"The receiver after character <space> or <enter> reset the current registered word <currentWord>.
	If <currentWord> is not empty --> store it in <lastWord>.
	The clear <currentWord>"

self logMethodName: 'clearAutocompletation ' info: #().

	currentWord notEmpty ifTrue: [lastWord := currentWord].
	currentWord := ''!

codePresenterIsMethod

	^parentPresenter codePresenterIsMethod!

configuration

	^JadeAutocompletationConfiguration default!

controlKeys
	
	^#(
		'ALT'
		'CTRL' 
		'DOWN' 
		'END' 			'ENTER' 			'ESC' 
		'HOME' 
		'LEFT' 			'LEFT ALT'		'LEFT CTRL'		'LEFT SHIFT' 
		'PGDOWN' 	'PGUP' 
		'RIGHT' 		'RIGHT ALT' 	'RIGHT CTRL'		'RIGHT SHIFT' 
		'SHIFT'		'SPACE' 
		'TAB' 
		'UP' 
	).
!

createSchematicWiring

	super createSchematicWiring.

	
	self when: #autoComplete:startingAt:accept: send: #onAutoComplete:startingAt:accept:  to: self.!

currentWord
	^currentWord!

currentWord: anObject
	currentWord := anObject!

getArgumentsNames
   "Get and set receiver's inst var <arguments>"
   | parseResult |

   self codePresenterIsMethod ifFalse: [^SortedCollection new].

   parseResult := SmalltalkParser parseMethod: self value onError: [:err | ^arguments := SortedCollection new].

   ^arguments := parseResult argumentNames asSortedCollection
!

getClassesName
	"The receiver (from registry) get all classes which includes <aString> in it's name"

	^self registry getClassesNamedWith: currentWord!

getClassMethodsFor: className
	"The receiver get all method from class named <className>"

	^(self registry getClass: className) gsClassMethods!

getMappedStringToClass
	"The receiver get and set the class related to var text name <lastWord> in the mapping of the registry.
	JadeAutocompletationRegistry <variableMap>"

	lastGsShape := self registry mapFor: lastWord.

	^self getMethodsForSelf!

getMethodsForAnyone
	"The receiver get all methods stored in the registry"

	^self registry getAllGlobalMethods!

getMethodsForSelf

	^lastGsShape getAllMethods!

getNamesAfterAssigment
   "The receiver get all possible text names after an assigment.
   Class name, method temporary, method argument, inst var, class var, inst class var or pool dictionary"
   | gsNames afterAssignment |

	afterAssignment := lastGsShape ifNotNil: [:value | value getNamesAfterAssigment] ifNil: [SortedCollection new].
   gsNames := afterAssignment, self getTemporaries, self getArgumentsNames. "method args, inst var"

   currentWord isEmpty ifTrue: [^gsNames].

   ^gsNames select: [:each | (each indexOfSubCollection: currentWord) ~=  0]!

getNamesForAlphaNumeric
	"The receiver get all possible names for the current word being typed <currentWord>.
	- All method arguments.
	- All possible methods.
	- All method tempories that match with <currentWord>"

	^(self getArgumentsNames, self getMethodsForAnyone, self getTemporaries) select: [:each | 
		(currentWord size >= each size) ifFalse: [(each first: currentWord size) = currentWord] ifTrue: [false]]!

getTemporaries
   "The receiver get the temporaries of the current method being edited"

   temporaries := self codePresenterIsMethod
               ifTrue: [(SmalltalkParser parseMethod: self value onError: [:err | ^temporaries := SortedCollection new]) body temporaryVariables asSortedCollection]
               ifFalse: [(SmalltalkParser parseExpression: self value onError: [:err | ^temporaries := SortedCollection new]) temporaryVariables asSortedCollection].

   ^temporaries
!

initialize
	super initialize.
	lastWord := ''.
	currentWord := ''.
	altMode := false.
	temporaries := OrderedCollection new.
	arguments := OrderedCollection new.

"	lastGsShape 		- current JadeGsClassShape to get information for autocompletation process.
	lastWord 			- last typed word	
	lastCharacter 		- last typed character
	currentWord 		- current typed word
	temporaries 		- temporaries of the current method being typed
	arguments 		- arguments of the current method being typed
	altMode			- wheter the receiver is in Right Alt mode
" !

insertText: aString at: anInteger
	"The receiver insert <aString> at the position <anInteger>. This method is called when the user select an option in the autocompletation menu"
	| stringWithArgs |

	self logMethodName: 'insert:Text: (string to insert at: )' info: (Array with: aString with: anInteger printString).

	(aString includes: $:) ifFalse: [ | text | "is a string without : --> a var name or a unary selector or a class name"
						text := (aString size >= lastWord size) ifTrue: [aString allButFirst: lastWord size] ifFalse: [aString].
						self logMethodName: 'insert:Text: (text typed + to insert)' info: (Array with: text).
						view insertText: text at: view caretPosition; moveToEndOfWord.
						lastWord := aString.
						^currentWord := ''.].
	stringWithArgs := ''.
	"inserting a binary selector (with :)"
	((aString allButFirst: lastWord size) subStrings: ':') keysAndValuesDo: [:key :value | stringWithArgs := stringWithArgs, value, ': ', (self registry paramFor: aString index: key), ' '].

	currentWord trimBlanks notEmpty ifTrue: [self logMethodName: 'insert:Text: (binary selector1)' info: (Array with: stringWithArgs).
		^view insertText: stringWithArgs  at: anInteger]. "the user has typed some keys of the autocomplation"

	view insertText: stringWithArgs allButLast at: view caretPosition; moveToEndOfWord. "the user has NOT type any key of the selected autocompletation"
	self logMethodName: 'insert:Text: (binary selector2)' info: (Array with: stringWithArgs).
	lastWord := aString.
	currentWord := ''.
	!

lastCharacter
	^lastCharacter!

lastCharacter: anObject
	lastCharacter := anObject!

lastGsShape
	^lastGsShape!

lastGsShape: anObject
	lastGsShape := anObject!

lastWord
	^lastWord!

lastWord: anObject 
	lastWord := anObject!

logMethodName: methodName info: anArray
	| fileStream |

	self configuration logEnabled ifFalse: [^self].

	fileStream := FileStream write: 'autocompletion.log' mode: #'append'.
	fileStream
		nextPutAll: 'time: ', Time now asMilliseconds printString; cr;
		nextPutAll: methodName ; cr; 
		nextPutAll: 'lastCharacter: ', lastCharacter printString; cr;
		nextPutAll: 'currentWord: ', currentWord; cr;
		nextPutAll: 'lastWord: ', lastWord; cr.
	anArray do: [:each | fileStream nextPutAll: ('info: ', each); cr].
	fileStream
		nextPutAll: '----------------------------------------------------------------------'; cr;
		close.!

onAutoComplete: aString startingAt: anInteger accept: aValueHolder 
	"This method is executed when an autocompletation has been entered. The user has selected an option in the autocompletation menu"

	self logMethodName: 'onAutoComplete: (before insertText:)' info: (Array with: aString with: anInteger printString).
	[self insertText: aString at: anInteger. "insert selected option"
	view moveToEndOfLine] forkAt: 4 . 

	aValueHolder value: false.

	lastWord := aString. "we store the selected autocompletation in order to guess the new autocompletation menu for the <lastWord>"

	altMode := false.

	"self transcriptOutput."!

onKeyPressed: aKeyEvent
	"This is the main method for autocompletation, each key stroke is registered and analyzed here"

	"super onKeyPressed: aKeyEvent."

	self autocompletationConfiguration isEnabled ifFalse: [^self]. "autocompletation is disable"

	[self processKeyPressed: aKeyEvent] forkAt: 4!

processKeyPressed: aKeyEvent
   "This is the main method for autocompletation, each key stroke is registered and analyzed here"
   | keyName |

   keyName := Keyboard keyNameFromLParam: aKeyEvent lParam. "get the name of the pressed Key "
   (keyName = 'F2') ifTrue: [^JadeAutocompletationConfigurationShell show].

   self logMethodName: 'processKeyPressed: ' info: (Array with: keyName).

   (keyName size = 1) ifTrue: [self updateAutocompletationTextWith: aKeyEvent]. "we update the <currentWord> with the key pressed if is not a control key"

   aKeyEvent wParam = 32 "space" ifTrue: [self autocompleteForSpace].

   aKeyEvent wParam = 13 "enter" ifTrue: [self autocompleteForEnter].

   (Keyboard default isKeyDown: 165) "right alt" ifTrue: [self autocompleteForRightAlt].

   (Character value: aKeyEvent wParam) isAlphaNumeric ifTrue: [self autocompleteForAlphaNumeric]!

registry

	^JadeAutocompletationRegistry default!

showAutoCompletionList: anOrdedCollection prefixLength: anInteger

	^view showAutoCompletionList: anOrdedCollection prefixLength: anInteger!

transcriptOutput

	Transcript nextPutAll: String lineDelimiter.
	Transcript nextPutAll: lastCharacter asString, ' - CW: ', currentWord , ' - LW: ', lastWord.
	Transcript flush.!

updateAutocompletationTextWith: aKeyEvent 
	"The receiver register <aKeyEvent> and update <currentWord> by adding the pressed key"
	| key |

	key := Keyboard keyNameFromLParam: aKeyEvent lParam. "get the key value"
	key := Keyboard default isShiftDown ifTrue: [key] ifFalse: [key asLowercase]. 
	(key = '-') ifTrue: [key := '_']. "- is the character for _ , so we simply change it"

	aKeyEvent wParam = 16 "shift" ifTrue: [^self].
	lastCharacter := Character value: aKeyEvent wParam. "get Character from <aKeyEvent>"

	Keyboard default isShiftDown ifFalse: [lastCharacter := lastCharacter asLowercase].
	"update the <currentWord> that is being typed"
	self currentWord:  ((lastCharacter == Character backspace) 
					ifTrue: [currentWord notEmpty ifTrue: [currentWord allButLast] ifFalse: ['']]
					ifFalse: [currentWord , key]).
	"self transcriptOutput."! !
!JadeAutoTextPresenter categoriesFor: #autocompletationConfiguration!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #autocompleteForAlphaNumeric!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #autocompleteForEnter!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #autocompleteForRightAlt!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #autocompleteForSpace!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #autocompleteForTab!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #clearAutocompletation!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #codePresenterIsMethod!public! !
!JadeAutoTextPresenter categoriesFor: #configuration!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #controlKeys!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #createSchematicWiring!public! !
!JadeAutoTextPresenter categoriesFor: #currentWord!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #currentWord:!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #getArgumentsNames!autocompletion!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #getClassesName!public! !
!JadeAutoTextPresenter categoriesFor: #getClassMethodsFor:!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #getMappedStringToClass!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #getMethodsForAnyone!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #getMethodsForSelf!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #getNamesAfterAssigment!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #getNamesForAlphaNumeric!autocompletion!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #getTemporaries!autocompletion lists!public! !
!JadeAutoTextPresenter categoriesFor: #initialize!public! !
!JadeAutoTextPresenter categoriesFor: #insertText:at:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #lastCharacter!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #lastCharacter:!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #lastGsShape!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #lastGsShape:!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #lastWord!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #lastWord:!accessing!private! !
!JadeAutoTextPresenter categoriesFor: #logMethodName:info:!public! !
!JadeAutoTextPresenter categoriesFor: #onAutoComplete:startingAt:accept:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #onKeyPressed:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #processKeyPressed:!autocompletation cycle!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #registry!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #showAutoCompletionList:prefixLength:!autocompletation cycle!public! !
!JadeAutoTextPresenter categoriesFor: #transcriptOutput!autocompletion!public! !
!JadeAutoTextPresenter categoriesFor: #updateAutocompletationTextWith:!autocompletation cycle!autocompletion!public! !

"Binary Globals"!

