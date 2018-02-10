! ------- Create dictionary if it is not present
run
| aSymbol names userProfile |
aSymbol := #'RowanServices'.
userProfile := System myUserProfile.
names := userProfile symbolList names.
(names includes: aSymbol) ifFalse: [
	| symbolDictionary |
	symbolDictionary := SymbolDictionary new name: aSymbol; yourself.
	userProfile insertDictionary: symbolDictionary at: names size + 1.
].
%
! ------------------- Class definition for RowanDefinitionService
expectvalue /Class
doit
Object subclass: 'RowanDefinitionService'
	instVarNames: #( definitionOop)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: 'RowanServices'
	category: 'Kernel'
	options: #()

%
expectvalue /Class
doit
RowanDefinitionService category: 'Kernel'
%
! ------------------- Class definition for RowanClassDefinitionService
expectvalue /Class
doit
RowanDefinitionService subclass: 'RowanClassDefinitionService'
	instVarNames: #( name comment instVarNames
	                  classVarNames classInstVarNames superclassName subclassType
	                  poolDictionaryNames classType)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: 'RowanServices'
	category: 'Kernel'
	options: #()

%
expectvalue /Class
doit
RowanClassDefinitionService category: 'Kernel'
%
! ------------------- Class definition for RowanMethodDefinitionService
expectvalue /Class
doit
RowanDefinitionService subclass: 'RowanMethodDefinitionService'
	instVarNames: #( source selector methodDefinition)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: 'RowanServices'
	category: 'Kernel'
	options: #()

%
expectvalue /Class
doit
RowanMethodDefinitionService category: 'Kernel'
%
! ------------------- Class definition for RowanPackageDefinitionService
expectvalue /Class
doit
RowanDefinitionService subclass: 'RowanPackageDefinitionService'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: 'RowanServices'
	category: 'Kernel'
	options: #()

%
expectvalue /Class
doit
RowanPackageDefinitionService category: 'Kernel'
%

! ------------------- Remove existing behavior from RowanDefinitionService
expectvalue /Metaclass3       
doit
RowanDefinitionService removeAllMethods.
RowanDefinitionService class removeAllMethods.
%
! ------------------- Class methods for RowanDefinitionService
set compile_env: 0
category: 'examples'
classmethod: RowanDefinitionService
sampleService

	^self new sampleService
%
! ------------------- Instance methods for RowanDefinitionService

! ------------------- Remove existing behavior from RowanClassDefinitionService
expectvalue /Metaclass3       
doit
RowanClassDefinitionService removeAllMethods.
RowanClassDefinitionService class removeAllMethods.
%
! ------------------- Class methods for RowanClassDefinitionService
! ------------------- Instance methods for RowanClassDefinitionService

! ------------------- Remove existing behavior from RowanMethodDefinitionService
expectvalue /Metaclass3       
doit
RowanMethodDefinitionService removeAllMethods.
RowanMethodDefinitionService class removeAllMethods.
%
! ------------------- Class methods for RowanMethodDefinitionService
! ------------------- Instance methods for RowanMethodDefinitionService
set compile_env: 0
category: 'accessors'
method: RowanMethodDefinitionService
selector

	^selector
%
category: 'accessors'
method: RowanMethodDefinitionService
selector: aSymbol

	selector := aSymbol
%
category: 'accessors'
method: RowanMethodDefinitionService
source

	^source
%
category: 'accessors'
method: RowanMethodDefinitionService
source: aString

	source := aString
%
set compile_env: 0
category: 'examples'
method: RowanMethodDefinitionService
sampleMethodSource

	^'sampleMethod ^''some text'''.
%
category: 'examples'
method: RowanMethodDefinitionService
sampleService

	methodDefinition := RwMethodDefinition newForSelector: 'sampleMethod' protocol: 'sampleProtocol' source: self sampleMethodSource.
	definitionOop := methodDefinition asOop.
	source := methodDefinition source.
	selector := methodDefinition selector.
%

! ------------------- Remove existing behavior from RowanPackageDefinitionService
expectvalue /Metaclass3       
doit
RowanPackageDefinitionService removeAllMethods.
RowanPackageDefinitionService class removeAllMethods.
%
! ------------------- Class methods for RowanPackageDefinitionService
! ------------------- Instance methods for RowanPackageDefinitionService
