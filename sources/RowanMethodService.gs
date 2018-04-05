! ------------------- Class definition for RowanMethodService
expectvalue /Class
doit
RowanService subclass: 'RowanMethodService'
	instVarNames: #( source selector methodDefinitions
	                  classService category packageName className
	                  meta)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'Rowan-Services'
	options: #()

%
expectvalue /Class
doit
RowanMethodService category: 'Rowan-Services'
%
! ------------------- Remove existing behavior from RowanMethodService
expectvalue /Metaclass3       
doit
RowanMethodService removeAllMethods.
RowanMethodService class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanMethodService
category: 'instance creation'
classmethod: RowanMethodService
source: source selector: selector category: category className: className packageName: packageName meta: boolString

	| service |
	service := self new. 
	service 
		source: source;
		selector: selector;
		category: category;
		className: className;
		packageName: packageName;
		meta: boolString == true.
	^service
%
! ------------------- Instance methods for RowanMethodService
category: 'rowan'
method: RowanMethodService
addOrUpdateMethod

		self browserTool
                   addOrUpdateMethod: source
                   inProtocol: category
                   forClassNamed: self classService name
                   isMeta: meta
                   inPackageNamed: self classService packageName
%
category: 'rowan'
method: RowanMethodService
browserTool

	^Rowan projectTools browser.
%
category: 'Accessing'
method: RowanMethodService
category
	^category
%
category: 'Updating'
method: RowanMethodService
category: newValue
	category := newValue
%
category: 'Accessing'
method: RowanMethodService
className
	^className
%
category: 'Updating'
method: RowanMethodService
className: newValue
	className := newValue
%
category: 'Accessing'
method: RowanMethodService
classService

	^classService ifNil:[classService := RowanClassService forClassNamed: className package: packageName]
%
category: 'Updating'
method: RowanMethodService
classService: newValue
	classService := newValue
%
category: 'examples'
method: RowanMethodService
createSampleMethod
           
           |   classDefinition | 
		
			classService := RowanClassService new.
			classDefinition := classService createSampleClass. 
			source := self sampleMethodSource.
			category := 'sample'.
			meta := false. 
			^self addOrUpdateMethod
%
category: 'rowan'
method: RowanMethodService
definitionClass

	^RwMethodDefinition
%
category: 'Accessing'
method: RowanMethodService
meta
	^meta printString
%
category: 'Updating'
method: RowanMethodService
meta: newValue
	meta := newValue == 'true'
%
category: 'Accessing'
method: RowanMethodService
methodDefinitions
	^methodDefinitions
%
category: 'Updating'
method: RowanMethodService
methodDefinitions: newValue
	methodDefinitions := newValue
%
category: 'Accessing'
method: RowanMethodService
packageName
	^packageName
%
category: 'Updating'
method: RowanMethodService
packageName: newValue
	packageName := newValue
%
category: 'examples'
method: RowanMethodService
sampleClassInstance

	^classService sampleClassInstance
%
category: 'examples'
method: RowanMethodService
sampleDefinition

	definition := self definitionClass newForSelector: 'sampleMethod' protocol: 'sampleProtocol' source: self sampleMethodSource.
	source := definition source.
	selector := definition selector.
	^definition
%
category: 'examples'
method: RowanMethodService
sampleDefinitions
	
	methodDefinitions := Array with: self sampleDefinition.
	^methodDefinitions
%
category: 'examples'
method: RowanMethodService
sampleMethodSelector

	^'sampleMethod'
%
category: 'examples'
method: RowanMethodService
sampleMethodSource

	^'sampleMethod ^''some text'''.
%
category: 'Accessing'
method: RowanMethodService
selector

	^selector
%
category: 'Accessing'
method: RowanMethodService
selector: aSymbol

	selector := aSymbol
%
category: 'Accessing'
method: RowanMethodService
source

	^source
%
category: 'Accessing'
method: RowanMethodService
source: aString
	
	source := aString
%
