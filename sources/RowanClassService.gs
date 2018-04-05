! ------------------- Class definition for RowanClassService
expectvalue /Class
doit
RowanService subclass: 'RowanClassService'
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
RowanClassService category: 'Rowan-Services'
%
! ------------------- Remove existing behavior from RowanClassService
expectvalue /Metaclass3       
doit
RowanClassService removeAllMethods.
RowanClassService class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanClassService
category: 'instance creation'
classmethod: RowanClassService
forClassNamed: className 

	| inst |
	inst := self new. 
	inst name: className.
	^inst
%
category: 'instance creation'
classmethod: RowanClassService
forClassNamed: className meta: aBoolean

	| inst |
	inst := self new. 
	inst name: className;
		meta: aBoolean.
	^inst
%
category: 'instance creation'
classmethod: RowanClassService
forClassNamed: className package: packageName

	| inst |
	inst := self forClassNamed: className.
	inst packageName: packageName.
	^inst
%
! ------------------- Instance methods for RowanClassService
category: 'rowan'
method: RowanClassService
classCreationTemplate
	
	^self browserTool classCreationTemplateForClass: self classFromName hybridBrowser: true.
%
category: 'private'
method: RowanClassService
classFromName

	^(System myUserProfile resolveSymbol: name asSymbol) value
%
category: 'Accessing'
method: RowanClassService
classInstVarNames
	^classInstVarNames
%
category: 'Updating'
method: RowanClassService
classInstVarNames: newValue
	classInstVarNames := newValue
%
category: 'Accessing'
method: RowanClassService
classType
	^classType
%
category: 'Updating'
method: RowanClassService
classType: newValue
	classType := newValue
%
category: 'Accessing'
method: RowanClassService
classVarNames
	^classVarNames
%
category: 'Updating'
method: RowanClassService
classVarNames: newValue
	classVarNames := newValue
%
category: 'Accessing'
method: RowanClassService
comment
	^comment
%
category: 'Updating'
method: RowanClassService
comment: newValue
	comment := newValue
%
category: 'examples'
method: RowanClassService
createSampleClass

	| classDefinition |
	packageService := RowanPackageService new. 
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
method: RowanClassService
instVarNames
	^instVarNames
%
category: 'Updating'
method: RowanClassService
instVarNames: newValue
	instVarNames := newValue
%
category: 'other'
method: RowanClassService
meta

	^meta
%
category: 'other'
method: RowanClassService
meta: anObject

	meta := anObject
%
category: 'Accessing'
method: RowanClassService
name
	^name
%
category: 'Updating'
method: RowanClassService
name: newValue
	name := newValue
%
category: 'examples'
method: RowanClassService
packageName
	
	^packageService packageName
%
category: 'examples'
method: RowanClassService
packageName: packageName
	
	packageService ifNil:[packageService := RowanPackageService new packageName: packageName].
	^packageService packageName
%
category: 'Accessing'
method: RowanClassService
packageService
	^packageService
%
category: 'Updating'
method: RowanClassService
packageService: newValue
	packageService := newValue
%
category: 'Accessing'
method: RowanClassService
poolDictionaryNames
	^poolDictionaryNames
%
category: 'Updating'
method: RowanClassService
poolDictionaryNames: newValue
	poolDictionaryNames := newValue
%
category: 'rowan'
method: RowanClassService
removeSelector: selector

	self browserTool removeMethod: selector forClassNamed: name asString isMeta: meta
%
category: 'examples'
method: RowanClassService
sampleClass
	"return the actual resolved class"
	
	^(GsSession currentSession resolveSymbol: self sampleClassName) value
%
category: 'examples'
method: RowanClassService
sampleClassInstance

	^self sampleClass new
%
category: 'examples'
method: RowanClassService
sampleClassName
	
	^'SampleClassName'
%
category: 'examples'
method: RowanClassService
samplePackageName
	
	^packageService samplePackageName
%
category: 'Accessing'
method: RowanClassService
subclassType
	^subclassType
%
category: 'Updating'
method: RowanClassService
subclassType: newValue
	subclassType := newValue
%
category: 'Accessing'
method: RowanClassService
superclassName
	^superclassName
%
category: 'Updating'
method: RowanClassService
superclassName: newValue
	superclassName := newValue
%
