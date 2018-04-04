! ------------------- Class definition for RowanProjectDefinitionService
expectvalue /Class
doit
RowanDefinitionService subclass: 'RowanProjectDefinitionService'
	instVarNames: #( name sha branch)
	classVars: #( DefaultProjectName)
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
category: 'accessing'
classmethod: RowanProjectDefinitionService
defaultProjectName

	^DefaultProjectName
%
category: 'accessing'
classmethod: RowanProjectDefinitionService
defaultProjectName: aString

	DefaultProjectName := aString
%
! ------------------- Instance methods for RowanProjectDefinitionService
category: 'other'
method: RowanProjectDefinitionService
branch

	^branch
%
category: 'other'
method: RowanProjectDefinitionService
branch: anObject

	branch := anObject
%
category: 'rowan'
method: RowanProjectDefinitionService
changes
	
	^(Rowan projectTools diff diffSpecUrl: name) asString.
%
category: 'examples'
method: RowanProjectDefinitionService
createProjectNamed: projectName 

	^self createProjectNamed: projectName in: self sampleSymbolDictionaryName.
%
category: 'examples'
method: RowanProjectDefinitionService
createProjectNamed: projectName in: symbolDictionaryName

	self rowanFixMe. "Dale doesn't like Rowan projectNames"
	(Rowan projectNames includes: projectName) ifFalse:[
		self browserTool createGitPackageProjectNamed: projectName updateDefinition: [:pd | 
				pd defaultSymbolDictName: symbolDictionaryName; comment:  'Sample Rowan Project'] ].
%
category: 'examples'
method: RowanProjectDefinitionService
createSampleProject

	self removeProjectNamed: self sampleProjectName.
	self createSampleSymbolDictionary.
	^self createProjectNamed: self sampleProjectName in: self sampleSymbolDictionaryName
%
category: 'accessing'
method: RowanProjectDefinitionService
defaultProjectName

	^self class defaultProjectName
%
category: 'accessing'
method: RowanProjectDefinitionService
defaultProjectName: aString

	self class defaultProjectName: aString
%
category: 'other'
method: RowanProjectDefinitionService
name

	^name
%
category: 'other'
method: RowanProjectDefinitionService
name: anObject

	name := anObject
%
category: 'rowan'
method: RowanProjectDefinitionService
projectNames

	^Rowan projectNames collect: [:string | self class new name: string]
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
category: 'other'
method: RowanProjectDefinitionService
sha

	^sha
%
category: 'other'
method: RowanProjectDefinitionService
sha: anObject

	sha := anObject
%
