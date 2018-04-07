/foos2/users/ewinger/GsDevKit_home/shared/repos/RowanSampler/specs
System gemEnvironmentVariable: 'GS_HOME' '/foos2/users/ewinger/GsDevKit_home'

| useSsh |
useSsh := true.	"set to false to clone using https:"
Rowan projectTools clone
	cloneSpecUrl:
	 'file:/foos2/users/ewinger/GsDevKit_home/shared/repos/RowanSampler/specs/RowanSample1.ston'
	gitRootPath: '$GS_HOME/shared/repos/'
	useSsh: useSsh.

Rowan projectTools load loadProjectNamed: 'RowanSample1'


Transcript cr; show: 'broken' 

| user symList projects|
	user := System myUserProfile.
	symList := user symbolList.
	projects := Rowan image loadedProjects.
	projects
		do: [ :loadedProject | 
			| spec dict platformSpec symDictNames  |
			spec := loadedProject handle.
			platformSpec := spec platformSpec at: 'gemstone'.
			symDictNames := Set with: 'UnmanagedPackages'.
			symDictNames add: platformSpec defaultSymbolDictName.
			platformSpec packageNameToPlatformPropertiesMap values
				do: [ :packageProperties | 
					packageProperties
						at: 'symbolDictName'
						ifPresent: [ :name | symDictNames add: name ] ].
			#('Globals' 'UserGlobals' 'Published')
				do: [ :reservedName | symDictNames remove: reservedName ifAbsent: [  ] ].
			symDictNames
				do: [ :symDictName | 
					(#('Globals' 'UserGlobals' 'Published') includes: symDictName asString)
						ifTrue: [ self halt ].

					dict := symList objectNamed: symDictName.
					dict
						ifNotNil: [ 
							| index |
							(dict at: GsPackagePolicy globalName otherwise: nil)
								ifNotNil: [ :policy | policy disable ].
							index := symList indexOf: dict.
							index > 0
								ifTrue: [ user removeDictionaryAt: index ] ] ].
			RwGsImage _removeLoadedProject: loadedProject ].
	GsPackagePolicy current refreshSessionMethodDictionary.
