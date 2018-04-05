! ------------------- Class definition for RowanService
expectvalue /Class
doit
Object subclass: 'RowanService'
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
RowanService category: 'Rowan-Services'
%
! ------------------- Remove existing behavior from RowanService
expectvalue /Metaclass3       
doit
RowanService removeAllMethods.
RowanService class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanService
category: 'other'
classmethod: RowanService
rowanFixMe

	"send this message to see everywhere that GS_Jade should be fixed"
%
category: 'examples'
classmethod: RowanService
sampleService

	^self new sampleService
%
! ------------------- Instance methods for RowanService
category: 'rowan'
method: RowanService
browserTool

	^self projectTools browser
%
category: 'symbol dictionaries'
method: RowanService
createDefaultSymbolDictionary

	^self createSymbolDictionaryNamed: self defaultSymbolDictionaryName
%
category: 'samples'
method: RowanService
createSampleSymbolDictionary

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
	self createSymbolDictionaryNamed: self sampleSymbolDictionaryName
%
category: 'symbol dictionaries'
method: RowanService
createSymbolDictionaryNamed: aName

	| dictionary size | 
	dictionary := RwGsPackageSymbolDictionary new. 
	dictionary at: aName asSymbol put: dictionary. 
	size := System myUserProfile symbolList size.
	System myUserProfile insertDictionary: dictionary at: size + 1.
	^dictionary
%
category: 'symbol dictionaries'
method: RowanService
defaultSymbolDictionary

	^self symbolDictionaryNamed: self defaultSymbolDictionaryName
%
category: 'symbol dictionaries'
method: RowanService
defaultSymbolDictionaryName

	^'RowanProjects'
%
category: 'rowan'
method: RowanService
definitionClass

	^self subclassResponsibility
%
category: 'rowan'
method: RowanService
definitionClassName

	^self definitionClass name
%
category: 'rowan'
method: RowanService
projectTools

	^Rowan projectTools
%
category: 'samples'
method: RowanService
removeSampleSymbolDictionary

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
%
category: 'symbol dictionaries'
method: RowanService
removeSymbolDictionaryNamed: aName

	| index |
	index := System myUserProfile symbolList names indexOf: aName asSymbol.
	index ~= 0 ifTrue:[
		System myUserProfile removeDictionaryAt: index]
%
category: 'other'
method: RowanService
rowanFixMe
		
	"marker for all things broken in Rowan"
%
category: 'rowan'
method: RowanService
rowanLoadedPackageNames

	| stream packages |
	self rowanFixMe.	"handle modified package display"
	stream := WriteStream on: String new.
	packages := Rowan packageNames.
	packages do: 
					[:package |
					stream
						nextPutAll: package;
						tab;
						nextPut: $N;
						tab;
						nextPutAll: package;
						lf].
	^stream contents
%
category: 'samples'
method: RowanService
sampleSymbolDictionaryName

	^'SampleSymbolDictionaryName'
%
category: 'symbol dictionaries'
method: RowanService
symbolDictionaryNamed: aName

	|  index |
	index := System myUserProfile symbolList names indexOf: aName asSymbol.
	^index ~= 0
		ifTrue:[
			System myUserProfile symbolList at: index]
		ifFalse:[
			self createSymbolDictionaryNamed: aName].
%
