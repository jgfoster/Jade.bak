! ------------------- Class definition for RowanPackageService
expectvalue /Class
doit
RowanService subclass: 'RowanPackageService'
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
RowanPackageService category: 'Rowan-Services'
%
! ------------------- Remove existing behavior from RowanPackageService
expectvalue /Metaclass3       
doit
RowanPackageService removeAllMethods.
RowanPackageService class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanPackageService
category: 'instance creation'
classmethod: RowanPackageService
forPackageNamed: packageName

	| inst |
	inst := self new. 
	inst packageName: packageName.
	^inst
%
! ------------------- Instance methods for RowanPackageService
category: 'rowan'
method: RowanPackageService
createPackage
	| projectService default |
	default := RowanProjectService defaultProjectName.
	projectService := RowanProjectService new.
	projectService createProjectNamed: default.  
	(Rowan packageNames includes: packageName) ifFalse:[
		self browserTool addPackageNamed: packageName toProjectNamed: default].
%
category: 'rowan'
method: RowanPackageService
createPackageNamed: aString inProject: projectName
	| projectService | 
	packageName := aString.
	projectService := RowanProjectService new.
	projectDefinition := projectService createProjectNamed: projectName.  
	projectDefinition addPackageNamed: packageName.
	self projectTools load loadProjectDefinition: projectDefinition.
%
category: 'examples'
method: RowanPackageService
createSamplePackage

	"assume that the sample project & symbol dictionary 
	were already removed"

	| projectService |
	projectService := RowanProjectService new.
	projectDefinition := projectService createSampleProject.  
	projectDefinition addPackageNamed: self samplePackageName.
	packageName := self samplePackageName.
	self projectTools load loadProjectDefinition: projectDefinition.
%
category: 'rowan'
method: RowanPackageService
definition

	^(Rowan image loadedPackageNamed: name) asDefinition
%
category: 'rowan'
method: RowanPackageService
deletePackage

	self browserTool removePackageNamed: packageName.
%
category: 'rowan'
method: RowanPackageService
genericClassCreationTemplate

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: name packageName: nil
%
category: 'rowan'
method: RowanPackageService
loadedClassDefinitions

	^self loadedClasses collect:[:loadedClass | loadedClass asDefinition]
%
category: 'rowan'
method: RowanPackageService
loadedClasses

	| loadedPackage |
	loadedPackage := Rowan image loadedPackageNamed: packageName ifAbsent:[^Array new].
	^loadedPackage loadedClasses
%
category: 'rowan'
method: RowanPackageService
loadedClassHandles

	^self loadedClasses collect:[:loadedClass | loadedClass handle]
%
category: 'rowan'
method: RowanPackageService
loadedClassNames

	^self loadedClasses collect:[:loadedClass | loadedClass name]
%
category: 'Accessing'
method: RowanPackageService
name
	^name
%
category: 'Updating'
method: RowanPackageService
name: newValue
	name := newValue
%
category: 'Accessing'
method: RowanPackageService
packageName
	^packageName
%
category: 'Updating'
method: RowanPackageService
packageName: newValue
	packageName := newValue
%
category: 'Accessing'
method: RowanPackageService
projectDefinition
	^projectDefinition
%
category: 'Updating'
method: RowanPackageService
projectDefinition: newValue
	projectDefinition := newValue
%
category: 'rowan'
method: RowanPackageService
removeClassNamed: className

	self browserTool removeClassNamed: className
%
category: 'examples'
method: RowanPackageService
samplePackageName
	
	^'SamplePackageName'
%
category: 'examples'
method: RowanPackageService
sampleProjectName

	^'SampleProjectName'
%
