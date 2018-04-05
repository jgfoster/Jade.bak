! ------------------- Class definition for RowanProjectService
expectvalue /Class
doit
RowanService subclass: 'RowanProjectService'
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
RowanProjectService category: 'Rowan-Services'
%
! ------------------- Remove existing behavior from RowanProjectService
expectvalue /Metaclass3       
doit
RowanProjectService removeAllMethods.
RowanProjectService class removeAllMethods.
%
set compile_env: 0
! ------------------- Class methods for RowanProjectService
category: 'accessing'
classmethod: RowanProjectService
defaultProjectName

	^DefaultProjectName
%
category: 'accessing'
classmethod: RowanProjectService
defaultProjectName: aString

	DefaultProjectName := aString
%
! ------------------- Instance methods for RowanProjectService
category: 'other'
method: RowanProjectService
branch

	^branch
%
category: 'other'
method: RowanProjectService
branch: anObject

	branch := anObject
%
category: 'rowan'
method: RowanProjectService
changes
	
	^(Rowan projectTools diff diffSpecUrl: name) asString.
%
category: 'examples'
method: RowanProjectService
createProjectNamed: projectName 

	^self createProjectNamed: projectName in: self sampleSymbolDictionaryName.
%
category: 'examples'
method: RowanProjectService
createProjectNamed: projectName in: symbolDictionaryName

	self rowanFixMe. "Dale doesn't like Rowan projectNames"
	(Rowan projectNames includes: projectName) ifFalse:[
		self browserTool createGitPackageProjectNamed: projectName updateDefinition: [:pd | 
				pd defaultSymbolDictName: symbolDictionaryName; comment:  'Sample Rowan Project'] ].
%
category: 'examples'
method: RowanProjectService
createSampleProject

	self removeProjectNamed: self sampleProjectName.
	self createSampleSymbolDictionary.
	^self createProjectNamed: self sampleProjectName in: self sampleSymbolDictionaryName
%
category: 'accessing'
method: RowanProjectService
defaultProjectName

	^self class defaultProjectName
%
category: 'accessing'
method: RowanProjectService
defaultProjectName: aString

	self class defaultProjectName: aString
%
category: 'rowan'
method: RowanProjectService
definition

	^(Rowan image loadedProjectNamed: name) asDefinition
%
category: 'other'
method: RowanProjectService
name

	^name
%
category: 'other'
method: RowanProjectService
name: anObject

	name := anObject
%
category: 'rowan'
method: RowanProjectService
projectNames

	^Rowan projectNames collect: [:string | self class new name: string]
%
category: 'examples'
method: RowanProjectService
removeProjectNamed: projectName 
	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | RwGsImage _removeLoadedProject: project ].
%
category: 'examples'
method: RowanProjectService
sampleProjectName
	
	^'SampleProjectName'
%
category: 'other'
method: RowanProjectService
sha

	^sha
%
category: 'other'
method: RowanProjectService
sha: anObject

	sha := anObject
%
