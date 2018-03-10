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
	instVarNames: #( definition)
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
	                  poolDictionaryNames classType packageService)
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
	instVarNames: #( source selector methodDefinitions
	                  classService)
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
	instVarNames: #( projectDefinition)
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
! ------------------- Class definition for RowanProjectDefinitionService
expectvalue /Class
doit
RowanDefinitionService subclass: 'RowanProjectDefinitionService'
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
RowanProjectDefinitionService category: 'Kernel'
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
set compile_env: 0
category: 'rowan'
method: RowanDefinitionService
browserTool

	^self projectTools browser
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
set compile_env: 0
category: 'samples'
method: RowanDefinitionService
createSampleSymbolDictionary

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
	self createSymbolDictionaryNamed: self sampleSymbolDictionaryName
%
category: 'samples'
method: RowanDefinitionService
removeSampleSymbolDictionary

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
%
category: 'samples'
method: RowanDefinitionService
sampleSymbolDictionaryName

	^'SampleSymbolDictionaryName'
%
set compile_env: 0
category: 'symbol dictionaries'
method: RowanDefinitionService
createDefaultSymbolDictionary

	^self createSymbolDictionaryNamed: self defaultSymbolDictionaryName
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
category: 'symbol dictionaries'
method: RowanDefinitionService
removeSymbolDictionaryNamed: aName

	| index |
	index := System myUserProfile symbolList names indexOf: aName asSymbol.
	index ~= 0 ifTrue:[
		System myUserProfile removeDictionaryAt: index]
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

! ------------------- Remove existing behavior from RowanClassDefinitionService
expectvalue /Metaclass3       
doit
RowanClassDefinitionService removeAllMethods.
RowanClassDefinitionService class removeAllMethods.
%
! ------------------- Class methods for RowanClassDefinitionService
! ------------------- Instance methods for RowanClassDefinitionService
set compile_env: 0
category: 'examples'
method: RowanClassDefinitionService
createSampleClass

	| classDefinition |
	packageService := RowanPackageDefinitionService new. 
	packageService createSamplePackage.
	classDefinition := RwClassDefinition
		newForClassNamed: self sampleClassName
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: String new
		comment: 'Sample Rowan Class'
		pools: #()
		type: 'normal'.
	self projectTools edit addClass: classDefinition
		inPackageNamed: packageService samplePackageName
		inProject: packageService projectDefinition.
	self projectTools load loadProjectDefinition: packageService projectDefinition.
	^classDefinition
%
category: 'examples'
method: RowanClassDefinitionService
sampleClassName
	
	^'SampleClassName'
%
category: 'examples'
method: RowanClassDefinitionService
samplePackageName
	
	^packageService samplePackageName
%

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
createSampleMethod
           
           |   classDefinition |

           classService := RowanClassDefinitionService new.
           classDefinition := classService createSampleClass. 
           ^self browserTool
                   addOrUpdateMethod: self sampleMethodSource
                   inProtocol: 'sample'
                   forClassNamed: classService sampleClassName
                   isMeta: false
                   inPackageNamed: classService samplePackageName
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
set compile_env: 0
category: 'rowan'
method: RowanMethodDefinitionService
browserTool

	^Rowan projectTools browser.
%
category: 'rowan'
method: RowanMethodDefinitionService
definitionClass

	^RwMethodDefinition
%

! ------------------- Remove existing behavior from RowanPackageDefinitionService
expectvalue /Metaclass3       
doit
RowanPackageDefinitionService removeAllMethods.
RowanPackageDefinitionService class removeAllMethods.
%
! ------------------- Class methods for RowanPackageDefinitionService
! ------------------- Instance methods for RowanPackageDefinitionService
set compile_env: 0
category: 'Accessing'
method: RowanPackageDefinitionService
projectDefinition
	^projectDefinition
%
set compile_env: 0
category: 'examples'
method: RowanPackageDefinitionService
createSamplePackage

	"assume that the sample project & symbol dictionary 
	were already removed"

	| projectService |
	projectService := RowanProjectDefinitionService new.
	projectDefinition := projectService createSampleProject.  
	projectDefinition addPackageNamed: self samplePackageName.
	self projectTools load loadProjectDefinition: projectDefinition.
%
category: 'examples'
method: RowanPackageDefinitionService
samplePackageName
	
	^'SamplePackageName'
%
category: 'examples'
method: RowanPackageDefinitionService
sampleProjectName

	^projectDefinition name
%
set compile_env: 0
category: 'Updating'
method: RowanPackageDefinitionService
projectDefinition: newValue
	projectDefinition := newValue
%

! ------------------- Remove existing behavior from RowanProjectDefinitionService
expectvalue /Metaclass3       
doit
RowanProjectDefinitionService removeAllMethods.
RowanProjectDefinitionService class removeAllMethods.
%
! ------------------- Class methods for RowanProjectDefinitionService
! ------------------- Instance methods for RowanProjectDefinitionService
set compile_env: 0
category: 'examples'
method: RowanProjectDefinitionService
createProjectNamed: projectName 

	| projectDefinition |

	projectDefinition := RwProjectDefinition 
		newForGitPackageProjectNamed: projectName.
	projectDefinition 
		comment: 'Sample Rowan Project';
		defaultSymbolDictName: self sampleSymbolDictionaryName.
	self projectTools load loadProjectDefinition: projectDefinition.
	^projectDefinition
%
category: 'examples'
method: RowanProjectDefinitionService
createProjectNamed: projectName in: symbolDictionaryName

	| projectDefinition |

	projectDefinition := RwProjectDefinition 
		newForGitPackageProjectNamed: projectName.
	projectDefinition 
		comment: 'Sample Rowan Project';
		defaultSymbolDictName: symbolDictionaryName.
	self projectTools load loadProjectDefinition: projectDefinition.
	^projectDefinition
%
category: 'examples'
method: RowanProjectDefinitionService
createSampleProject

	self createSampleSymbolDictionary.
	self removeProjectNamed: self sampleProjectName.
	^self createProjectNamed: self sampleProjectName in: self sampleSymbolDictionaryName
%
category: 'examples'
method: RowanProjectDefinitionService
removeProjectNamed: projectName 
	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | RwGsImage _removeLoadedProject: project ].
%
category: 'examples'
method: RowanProjectDefinitionService
sampleProjectName
	
	^'SampleProjectName'
%
