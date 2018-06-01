| loadedProject adoptTool packageName spec gemstoneSpec |
loadedProject := Rowan image loadedProjectNamed: 'Rowan'.
packageName := 'Rowan-JadeServer'.
spec := loadedProject specification.
gemstoneSpec := spec platformSpec at: 'gemstone'.
gemstoneSpec symbolDictName: 'UserGlobals' forPackageNamed: packageName.
Rowan packageTools create createLoadedPackageNamed: packageName inProjectNamed: 'Rowan'.
adoptTool := Rowan packageTools adopt.
#('JadeServer' 'JadeServer64bit' 'JadeServer64bit24' 
	'JadeServer64bit32' 'JadeServer64bit3x' )
	do: [:className |
		adoptTool adoptClassNamed: className intoPackageNamed: packageName
].