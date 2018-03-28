! ------------------- Class definition for RowanClassDefinitionService
expectvalue /Class
doit
RowanDefinitionService subclass: 'RowanClassDefinitionService'
	instVarNames: #( name comment instVarNames
	                  classVarNames classInstVarNames superclassName subclassType
	                  poolDictionaryNames classType packageService meta)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'Rowan-Services'
	options: #()

%
expectvalue /Class
doit
RowanClassDefinitionService category: 'Rowan-Services'
%
! ------------------- Remove existing behavior from RowanClassDefinitionService
expectvalue /Metaclass3       
doit
RowanClassDefinitionService removeAllMethods.
RowanClassDefinitionService class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanClassDefinitionService
category: 'instance creation'
classmethod: RowanClassDefinitionService
forClassNamed: className 

	| inst |
	inst := self new. 
	inst name: className.
	^inst
%
category: 'instance creation'
classmethod: RowanClassDefinitionService
forClassNamed: className meta: aBoolean

	| inst |
	inst := self new. 
	inst name: className;
		meta: aBoolean.
	^inst
%
category: 'instance creation'
classmethod: RowanClassDefinitionService
forClassNamed: className package: packageName

	| inst |
	inst := self forClassNamed: className.
	inst packageName: packageName.
	^inst
%
! ------------------- Instance methods for RowanClassDefinitionService
category: 'rowan'
method: RowanClassDefinitionService
classCreationTemplate

	^self browserTool classCreationTemplateForClass: self classFromName hybridBrowser: true.
%
category: 'private'
method: RowanClassDefinitionService
classFromName

	^(System myUserProfile resolveSymbol: name asSymbol) value
%
category: 'Accessing'
method: RowanClassDefinitionService
classInstVarNames
	^classInstVarNames
%
category: 'Updating'
method: RowanClassDefinitionService
classInstVarNames: newValue
	classInstVarNames := newValue
%
category: 'Accessing'
method: RowanClassDefinitionService
classType
	^classType
%
category: 'Updating'
method: RowanClassDefinitionService
classType: newValue
	classType := newValue
%
category: 'Accessing'
method: RowanClassDefinitionService
classVarNames
	^classVarNames
%
category: 'Updating'
method: RowanClassDefinitionService
classVarNames: newValue
	classVarNames := newValue
%
category: 'Accessing'
method: RowanClassDefinitionService
comment
	^comment
%
category: 'Updating'
method: RowanClassDefinitionService
comment: newValue
	comment := newValue
%
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
	name := classDefinition name.
	self projectTools edit addClass: classDefinition
		inPackageNamed: packageService samplePackageName
		inProject: packageService projectDefinition.
	self projectTools load loadProjectDefinition: packageService projectDefinition.
	^classDefinition
%
category: 'Accessing'
method: RowanClassDefinitionService
instVarNames
	^instVarNames
%
category: 'Updating'
method: RowanClassDefinitionService
instVarNames: newValue
	instVarNames := newValue
%
category: 'other'
method: RowanClassDefinitionService
meta

	^meta
%
category: 'other'
method: RowanClassDefinitionService
meta: anObject

	meta := anObject
%
category: 'Accessing'
method: RowanClassDefinitionService
name
	^name
%
category: 'Updating'
method: RowanClassDefinitionService
name: newValue
	name := newValue
%
category: 'examples'
method: RowanClassDefinitionService
packageName
	
	^packageService packageName
%
category: 'examples'
method: RowanClassDefinitionService
packageName: packageName
	
	packageService ifNil:[packageService := RowanPackageDefinitionService new packageName: packageName].
	^packageService packageName
%
category: 'Accessing'
method: RowanClassDefinitionService
packageService
	^packageService
%
category: 'Updating'
method: RowanClassDefinitionService
packageService: newValue
	packageService := newValue
%
category: 'Accessing'
method: RowanClassDefinitionService
poolDictionaryNames
	^poolDictionaryNames
%
category: 'Updating'
method: RowanClassDefinitionService
poolDictionaryNames: newValue
	poolDictionaryNames := newValue
%
category: 'rowan'
method: RowanClassDefinitionService
removeSelector: selector

	self browserTool removeMethod: selector forClassNamed: name isMeta: meta
%
category: 'examples'
method: RowanClassDefinitionService
sampleClass
	"return the actual resolved class"
	
	^(GsSession currentSession resolveSymbol: self sampleClassName) value
%
category: 'examples'
method: RowanClassDefinitionService
sampleClassInstance

	^self sampleClass new
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
category: 'Accessing'
method: RowanClassDefinitionService
subclassType
	^subclassType
%
category: 'Updating'
method: RowanClassDefinitionService
subclassType: newValue
	subclassType := newValue
%
category: 'Accessing'
method: RowanClassDefinitionService
superclassName
	^superclassName
%
category: 'Updating'
method: RowanClassDefinitionService
superclassName: newValue
	superclassName := newValue
%
