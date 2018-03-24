! ------------------- Class definition for RowanDefinitionService
expectvalue /Class
doit
Object subclass: 'RowanDefinitionService'
	instVarNames: #( definition)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'Rowan-Services'
	options: #()

%
expectvalue /Class
doit
RowanDefinitionService category: 'Rowan-Services'
%
! ------------------- Remove existing behavior from RowanDefinitionService
expectvalue /Metaclass3       
doit
RowanDefinitionService removeAllMethods.
RowanDefinitionService class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanDefinitionService
category: 'other'
classmethod: RowanDefinitionService
rowanFixMe

	"send this message to see everywhere that GS_Jade should be fixed"
%
category: 'examples'
classmethod: RowanDefinitionService
sampleService

	^self new sampleService
%
! ------------------- Instance methods for RowanDefinitionService
category: 'rowan'
method: RowanDefinitionService
browserTool

	^self projectTools browser
%
category: 'symbol dictionaries'
method: RowanDefinitionService
createDefaultSymbolDictionary

	^self createSymbolDictionaryNamed: self defaultSymbolDictionaryName
%
category: 'samples'
method: RowanDefinitionService
createSampleSymbolDictionary

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
	self createSymbolDictionaryNamed: self sampleSymbolDictionaryName
%
category: 'symbol dictionaries'
method: RowanDefinitionService
createSymbolDictionaryNamed: aName

	| dictionary size | 
	dictionary := RwGsPackageSymbolDictionary new. 
	dictionary at: aName asSymbol put: dictionary. 
	size := System myUserProfile symbolList size.
	System myUserProfile insertDictionary: dictionary at: size + 1.
	^dictionary
%
category: 'symbol dictionaries'
method: RowanDefinitionService
defaultSymbolDictionary

	^self symbolDictionaryNamed: self defaultSymbolDictionaryName
%
category: 'symbol dictionaries'
method: RowanDefinitionService
defaultSymbolDictionaryName

	^'RowanProjects'
%
category: 'rowan'
method: RowanDefinitionService
definitionClass

	^self subclassResponsibility
%
category: 'rowan'
method: RowanDefinitionService
definitionClassName

	^self definitionClass name
%
category: 'rowan'
method: RowanDefinitionService
projectTools

	^Rowan projectTools
%
category: 'samples'
method: RowanDefinitionService
removeSampleSymbolDictionary

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
%
category: 'symbol dictionaries'
method: RowanDefinitionService
removeSymbolDictionaryNamed: aName

	| index |
	index := System myUserProfile symbolList names indexOf: aName asSymbol.
	index ~= 0 ifTrue:[
		System myUserProfile removeDictionaryAt: index]
%
category: 'samples'
method: RowanDefinitionService
sampleSymbolDictionaryName

	^'SampleSymbolDictionaryName'
%
category: 'symbol dictionaries'
method: RowanDefinitionService
symbolDictionaryNamed: aName

	|  index |
	index := System myUserProfile symbolList names indexOf: aName asSymbol.
	^index ~= 0
		ifTrue:[
			System myUserProfile symbolList at: index]
		ifFalse:[
			self createSymbolDictionaryNamed: aName].
%
