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
	message: 'Have the Jade system browser showing Rowan Packages. Can also unload packages (kindof) and show different class creation templates correctly'.

"git push for Rowan project"
Rowan projectTools push pushSpecUrl: 'Rowan'.

Rowan image loadedPackageNames

(Rowan projectTools diff diffSpecUrl: 'Rowan') asString.
