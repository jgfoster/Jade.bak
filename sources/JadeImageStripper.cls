"Filed out from Dolphin Smalltalk 7"!

ImageStripper subclass: #JadeImageStripper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeImageStripper guid: (GUID fromString: '{773ab738-9d79-41cb-b663-df50bce9c3e5}')!
JadeImageStripper comment: ''!
!JadeImageStripper categoriesForClass!Unclassified! !
!JadeImageStripper methodsFor!

closeLoginShells

	JadeLoginShell allInstances do: [:each | each view close].

!

copyRuntimeFiles

	| basePath |
	basePath := SessionManager current imageBase.
	#('bin' 'icons') do: [:eachDir |
		(File exists: basePath , 'runtime\' , eachDir) ifTrue: [
			(File isDirectory: basePath , 'runtime\' , eachDir) ifFalse: [eachDir error: 'not a directory'].
			File deleteDirectory: basePath , 'runtime\' , eachDir.
		].
		File createDirectory: basePath , 'runtime\' , eachDir.
		File
			forAll: '*.*'
			in: basePath , eachDir
			do: [:each |
				(2 < each fileName size and: [(each fileName first == $.) not]) ifTrue: [
					File
						copy: basePath , eachDir , '\' , each fileName
						to: basePath , 'runtime\' , eachDir , '\' , each fileName.
				].
			].
	].
!

createVersionResource
"
	JadeImageStripper new createVersionResource.
"
	| versionString version |
	versionString := JadeSessionManager getVersion.
	version := (versionString subStrings: $-) first.
	self versionResource
		fileVersion: version;
		productVersion: version;
		stringTables: (LookupTable new
			at: '040904b0'
			put: (LookupTable new
				at: 'CompanyName' 		put: 'Foster Agile LLC';
				at: 'FileDescription' 		put: 'IDE for GemStone/S';
				at: 'FileVersion' 			put: versionString;
				at: 'InternalName' 		put: 'Jade';
				at: 'LegalCopyright' 		put: 'Copyright © James Foster 2001-2019';
				at: 'OriginalFilename' 		put: 'Jade.exe';
				at: 'ProductName' 		put: 'Jade';
				at: 'ProductVersion' 		put: versionString;
				yourself);
			yourself);
		yourself.
!

exeIconFile

	^'icons\GS32.ico'.
!

finishedWith: selector

	selector = #'finishedWith:' ifTrue: [^self].
	super finishedWith: selector.
!

loadJadeServerSourceCache

	JadeServer allSubclassesDo: [:each | each gsString].
!

logRemainingClasses

	10 timesRepeat: [
		[
			super logRemainingClasses.
			^self.
		] on: Error do: [:ex |
			MemoryManager current collectGarbage; compact.
		].
	].!

prepareToStrip

	Transcript view close.
	JadeSessionManager setVersion.
	self
		loadJadeServerSourceCache;
		copyRuntimeFiles;
		closeLoginShells;
		createVersionResource;
		yourself.
	^super prepareToStrip.
!

requiredClasses
	^(super requiredClasses)
		addAll: JadeServer withAllSubclasses;
		addAll: GciError withAllSubclasses;
		addAll: GsError withAllSubclasses;
		addAll: GciLibrary withAllSubclasses;
		add: NTLibrary;
		add: StParseNodeVisitor;
		addAll: StProgramNode withAllSubclasses;
		addAll: StToken withAllSubclasses;
		add: ScintillaIndicator;
		yourself!

requiredPackageNames

	^super requiredPackageNames
		add: 'Development System'; 		"to get StKeywordToken>>#preComments"
		add: 'GemStone C Interface';
		add: 'GemStone Session';
		add: 'GemStone Objects';
		add: 'Jade Inspector';
		add: 'Jade Login';
		add: 'Jade System Browser';
		add: 'Jade Transcript';
		add: 'Jade User Browser';
		add: 'JGF Dolphin';
		add: 'JGF Fading Dialog';
		add: 'Jade UI';
		add: 'Object Log Browser';
		add: 'Jade Process Browser';
		add: 'Jade Tutorial';
		yourself! !
!JadeImageStripper categoriesFor: #closeLoginShells!public! !
!JadeImageStripper categoriesFor: #copyRuntimeFiles!public! !
!JadeImageStripper categoriesFor: #createVersionResource!public! !
!JadeImageStripper categoriesFor: #exeIconFile!operations!public! !
!JadeImageStripper categoriesFor: #finishedWith:!public! !
!JadeImageStripper categoriesFor: #loadJadeServerSourceCache!public! !
!JadeImageStripper categoriesFor: #logRemainingClasses!public! !
!JadeImageStripper categoriesFor: #prepareToStrip!public! !
!JadeImageStripper categoriesFor: #requiredClasses!public! !
!JadeImageStripper categoriesFor: #requiredPackageNames!public! !

!JadeImageStripper class methodsFor!

versionHistory
"
JadeImageStripper versionHistory.
"
	| imageDir stream |
	stream := WriteStream on: String new.
	imageDir := 'B:\Dolphin\Jade\'.
	#('Jade' 'JGF') do: [:dir |
		File for: '*.pac' in: imageDir , dir do: [:winFileInfo |
			| packageName |
			packageName := winFileInfo fileName.
			stream nextPutAll: dir , '\' , packageName; cr.
			packageName := packageName copyFrom: 1 to: packageName size - 4.
			((Smalltalk at: #'StsManager') current getPackageEditionsFor: packageName) do: [:eachEdition |
				| string |
				string := (eachEdition versionComment collect: [:char | char codePoint < 32 ifTrue: [Character space] ifFalse: [char]]) trimBlanks.
				string notEmpty ifTrue: [
					stream tab; nextPutAll: eachEdition versionDescriptor , ': ' , string; cr.
				].
			].
		].
	].
	^stream contents.
! !
!JadeImageStripper class categoriesFor: #versionHistory!public! !

