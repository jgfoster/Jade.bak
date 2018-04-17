RowanMethodService newMethod

UserGlobals at: #rowanCompile ifAbsent:[false]  

(System myUserProfile resolveSymbol: #UserGlobals) value at: #rowanCompile put: true true
(System myUserProfile resolveSymbol: #UserGlobals) value at: #rowanCompile put: false

(System myUserProfile resolveSymbol: #UserGlobals) value 

Rowan image loadedProjects


"git pull for Rowan project"
Rowan projectTools pull pullSpecUrl: 'Rowan'.

"write listed packages and commit Rowan project"
Rowan packageTools commit
	commitSpecUrl: 'Rowan'
	packageNames: #('Rowan-Services')
	message: 'checkpoint - Added new project from git and load project to Rowan Project List. Kind of working.'.

"**** RUN TESTS  (eventually in Travis CI) ******"

"git push for Rowan project"
Rowan projectTools push pushSpecUrl: 'Rowan'.

Rowan image loadedPackageNames

RowanDefinitionService subclass: 'RowanPackageDefinitionService'
	instVarNames: #( projectDefinition packageName name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()


"Rudimentary differences tool"
(Rowan projectTools diff diffSpecUrl: 'Rowan') asString.
