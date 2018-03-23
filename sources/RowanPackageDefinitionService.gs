! ------------------- Class definition for RowanPackageDefinitionService
expectvalue /Class
doit
RowanDefinitionService subclass: 'RowanPackageDefinitionService'
	instVarNames: #( projectDefinition packageName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'Rowan-Services'
	options: #()

%
expectvalue /Class
doit
RowanPackageDefinitionService category: 'Rowan-Services'
%
! ------------------- Remove existing behavior from RowanPackageDefinitionService
expectvalue /Metaclass3       
doit
RowanPackageDefinitionService removeAllMethods.
RowanPackageDefinitionService class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanPackageDefinitionService
! ------------------- Instance methods for RowanPackageDefinitionService
category: 'rowan'
method: RowanPackageDefinitionService
createPackageNamed: aString inProject: projectName
	| projectService | self halt. 
	packageName := aString.
	projectService := RowanProjectDefinitionService new.
	projectDefinition := projectService createProjectNamed: projectName.  
	projectDefinition addPackageNamed: packageName.
	self projectTools load loadProjectDefinition: projectDefinition.
%
category: 'examples'
method: RowanPackageDefinitionService
createSamplePackage

	"assume that the sample project & symbol dictionary 
	were already removed"

	| projectService |
	projectService := RowanProjectDefinitionService new.
	projectDefinition := projectService createSampleProject.  
	projectDefinition addPackageNamed: self samplePackageName.
	packageName := self samplePackageName.
	self projectTools load loadProjectDefinition: projectDefinition.
%
category: 'Accessing'
method: RowanPackageDefinitionService
packageName
	^packageName
%
category: 'Updating'
method: RowanPackageDefinitionService
packageName: newValue
	packageName := newValue
%
category: 'Accessing'
method: RowanPackageDefinitionService
projectDefinition
	^projectDefinition
%
category: 'Updating'
method: RowanPackageDefinitionService
projectDefinition: newValue
	projectDefinition := newValue
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
