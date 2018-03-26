! ------------------- Class definition for RowanProjectDefinitionService
expectvalue /Class
doit
RowanDefinitionService subclass: 'RowanProjectDefinitionService'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'Rowan-Services'
	options: #()

%
expectvalue /Class
doit
RowanProjectDefinitionService category: 'Rowan-Services'
%
! ------------------- Remove existing behavior from RowanProjectDefinitionService
expectvalue /Metaclass3       
doit
RowanProjectDefinitionService removeAllMethods.
RowanProjectDefinitionService class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanProjectDefinitionService
! ------------------- Instance methods for RowanProjectDefinitionService
category: 'examples'
method: RowanProjectDefinitionService
createProjectNamed: projectName 

	^self createProjectNamed: projectName in: self sampleSymbolDictionaryName.
%
category: 'examples'
method: RowanProjectDefinitionService
createProjectNamed: projectName in: symbolDictionaryName

	| projectDefinition |
	projectDefinition := (Rowan loadedProjectNamed: projectName ifAbsent:[
		self projectTools load loadProjectDefinition: (RwProjectDefinition newForGitPackageProjectNamed: projectName)]) asDefinition.
	projectDefinition 
		comment: 'Sample Rowan Project';
		defaultSymbolDictName: symbolDictionaryName.
	^projectDefinition
%
category: 'examples'
method: RowanProjectDefinitionService
createSampleProject

	self removeProjectNamed: self sampleProjectName.
	self createSampleSymbolDictionary.
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
