! ------------------- Class definition for RowanPackageDefinitionService
expectvalue /Class
doit
RowanDefinitionService subclass: 'RowanPackageDefinitionService'
	instVarNames: #( projectDefinition packageName name)
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
category: 'instance creation'
classmethod: RowanPackageDefinitionService
forPackageNamed: packageName

	| inst |
	inst := self new. 
	inst packageName: packageName.
	^inst
%
! ------------------- Instance methods for RowanPackageDefinitionService
category: 'rowan'
method: RowanPackageDefinitionService
createPackage
	| projectService default |
	default := RowanProjectDefinitionService defaultProjectName.
	projectService := RowanProjectDefinitionService new.
	projectService createProjectNamed: default.  
	(Rowan packageNames includes: packageName) ifFalse:[
		self browserTool addPackageNamed: packageName toProjectNamed: default].
%
category: 'rowan'
method: RowanPackageDefinitionService
createPackageNamed: aString inProject: projectName
	| projectService | 
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
category: 'rowan'
method: RowanPackageDefinitionService
deletePackage

	self browserTool removePackageNamed: packageName.
%
category: 'rowan'
method: RowanPackageDefinitionService
genericClassCreationTemplate

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: name packageName: nil
%
category: 'rowan'
method: RowanPackageDefinitionService
loadedClassDefinitions

	^self loadedClasses collect:[:loadedClass | loadedClass asDefinition]
%
category: 'rowan'
method: RowanPackageDefinitionService
loadedClasses

	| loadedPackage |
	loadedPackage := Rowan image loadedPackageNamed: packageName ifAbsent:[^Array new].
	^loadedPackage loadedClasses
%
category: 'rowan'
method: RowanPackageDefinitionService
loadedClassHandles

	^self loadedClasses collect:[:loadedClass | loadedClass handle]
%
category: 'rowan'
method: RowanPackageDefinitionService
loadedClassNames

	^self loadedClasses collect:[:loadedClass | loadedClass name]
%
category: 'Accessing'
method: RowanPackageDefinitionService
name
	^name
%
category: 'Updating'
method: RowanPackageDefinitionService
name: newValue
	name := newValue
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
category: 'rowan'
method: RowanPackageDefinitionService
removeClassNamed: className

	self browserTool removeClassNamed: className
%
category: 'examples'
method: RowanPackageDefinitionService
samplePackageName
	
	^'SamplePackageName'
%
category: 'examples'
method: RowanPackageDefinitionService
sampleProjectName

	^'SampleProjectName'
%
