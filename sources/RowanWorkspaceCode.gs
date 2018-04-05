UserGlobals at: #rowanCompile ifAbsent:[false]  

UserGlobals at: #rowanCompile put: true
UserGlobals at: #rowanCompile put: false


Rowan image loadedProjects


"git pull for Rowan project"
Rowan projectTools pull pullSpecUrl: 'Rowan'.

"write listed packages and commit Rowan project"
Rowan packageTools commit
	commitSpecUrl: 'Rowan'
	packageNames: #('Rowan-Services')
	message: 'checkpoint - was fixing package display bug. Added changes method for eventual project diff support'.

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
