! ------------------- Class definition for RowanMethodDefinitionService
expectvalue /Class
doit
RowanDefinitionService subclass: 'RowanMethodDefinitionService'
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
RowanMethodDefinitionService category: 'Rowan-Services'
%
! ------------------- Remove existing behavior from RowanMethodDefinitionService
expectvalue /Metaclass3       
doit
RowanMethodDefinitionService removeAllMethods.
RowanMethodDefinitionService class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanMethodDefinitionService
category: 'instance creation'
classmethod: RowanMethodDefinitionService
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
! ------------------- Instance methods for RowanMethodDefinitionService
category: 'rowan'
method: RowanMethodDefinitionService
addOrUpdateMethod

		self browserTool
                   addOrUpdateMethod: source
                   inProtocol: category
                   forClassNamed: self classService name
                   isMeta: meta
                   inPackageNamed: self classService packageName
%
category: 'rowan'
method: RowanMethodDefinitionService
browserTool

	^Rowan projectTools browser.
%
category: 'Accessing'
method: RowanMethodDefinitionService
category
	^category
%
category: 'Updating'
method: RowanMethodDefinitionService
category: newValue
	category := newValue
%
category: 'Accessing'
method: RowanMethodDefinitionService
className
	^className
%
category: 'Updating'
method: RowanMethodDefinitionService
className: newValue
	className := newValue
%
category: 'Accessing'
method: RowanMethodDefinitionService
classService

	^classService ifNil:[classService := RowanClassDefinitionService forClassNamed: className package: packageName]
%
category: 'Updating'
method: RowanMethodDefinitionService
classService: newValue
	classService := newValue
%
category: 'examples'
method: RowanMethodDefinitionService
createSampleMethod
           
           |   classDefinition | 
		
			classService := RowanClassDefinitionService new.
			classDefinition := classService createSampleClass. 
			source := self sampleMethodSource.
			category := 'sample'.
			meta := false. 
			^self addOrUpdateMethod
%
category: 'rowan'
method: RowanMethodDefinitionService
definitionClass

	^RwMethodDefinition
%
category: 'Accessing'
method: RowanMethodDefinitionService
meta
	^meta printString
%
category: 'Updating'
method: RowanMethodDefinitionService
meta: newValue
	meta := newValue == 'true'
%
category: 'Accessing'
method: RowanMethodDefinitionService
methodDefinitions
	^methodDefinitions
%
category: 'Updating'
method: RowanMethodDefinitionService
methodDefinitions: newValue
	methodDefinitions := newValue
%
category: 'Accessing'
method: RowanMethodDefinitionService
packageName
	^packageName
%
category: 'Updating'
method: RowanMethodDefinitionService
packageName: newValue
	packageName := newValue
%
category: 'examples'
method: RowanMethodDefinitionService
sampleClassInstance

	^classService sampleClassInstance
%
category: 'examples'
method: RowanMethodDefinitionService
sampleDefinition

	definition := self definitionClass newForSelector: 'sampleMethod' protocol: 'sampleProtocol' source: self sampleMethodSource.
	source := definition source.
	selector := definition selector.
	^definition
%
category: 'examples'
method: RowanMethodDefinitionService
sampleDefinitions
	
	methodDefinitions := Array with: self sampleDefinition.
	^methodDefinitions
%
category: 'examples'
method: RowanMethodDefinitionService
sampleMethodSelector

	^'sampleMethod'
%
category: 'examples'
method: RowanMethodDefinitionService
sampleMethodSource

	^'sampleMethod ^''some text'''.
%
category: 'Accessing'
method: RowanMethodDefinitionService
selector

	^selector
%
category: 'Accessing'
method: RowanMethodDefinitionService
selector: aSymbol

	selector := aSymbol
%
category: 'Accessing'
method: RowanMethodDefinitionService
source

	^source
%
category: 'Accessing'
method: RowanMethodDefinitionService
source: aString
	
	source := aString
%
