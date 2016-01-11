"Filed out from Dolphin Smalltalk 7"!

ImageStripper subclass: #JadeImageStripper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeImageStripper guid: (GUID fromString: '{773AB738-9D79-41CB-B663-DF50BCE9C3E5}')!
JadeImageStripper comment: ''!
!JadeImageStripper categoriesForClass!Unclassified! !
!JadeImageStripper methodsFor!

closeLoginShells

	JadeLoginShell allInstances do: [:each | each view close].

!

copyRuntimeFiles

	| basePath |
	basePath := SessionManager current imageBase , 'Jade\Jade\'.
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

exeIconFile

	^'Jade\icons\GS32x32.ico'.
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

	JadeSessionManager setVersion.
	self 
		"savePackages;"
		loadJadeServerSourceCache; 
		copyRuntimeFiles; 
		closeLoginShells;
		yourself.
	^super prepareToStrip.
!

requiredClasses
	^(super requiredClasses)
		addAll: JadeServer withAllSubclasses;
		addAll: GciError withAllSubclasses;
		addAll: GsError withAllSubclasses;
		addAll: GciLibrary withAllSubclasses;
		addAll: GsHostProcess withAllSubclasses;
		addAll: GsShellCommand withAllSubclasses;
		addAll: GsWin32Service withAllSubclasses;
		add: StParseNodeVisitor;
		addAll: StProgramNode withAllSubclasses;
		addAll: StToken withAllSubclasses;
		add: ScintillaIndicator;
		add: WaitOnGemStoneDialog;
		yourself!

requiredPackageNames

	^super requiredPackageNames
		add: 'Dolphin MVP (Deprecated)';	"Referenced by EditableListView"
		add: 'Dialect Abstraction Layer';		"to get #'isNilOrEmpty'"
		add: 'Development System'; 		"to get StKeywordToken>>#preComments"
		add: 'GemStone C Interface';
		add: 'GemStone Session';
		add: 'GemStone Objects';
		add: 'GemStone Services';
		add: 'Jade Inspector';
		add: 'Jade Login';
		add: 'Jade System Browser';
		add: 'Jade Transcript';
		add: 'Jade User Browser';
		add: 'JGF Dolphin';
		add: 'JGF Dolphin MVP Base';
		add: 'JGF Fading Dialog';
		add: 'Jade UI';
		add: 'Object Log Browser';
		add: 'Jade from Dolphin';
		add: 'Jade Process Browser';
		yourself!

savePackages
"
	JadeImageStripper new savePackages.
"
	(ProgressDialog showModalWhile: [:progress | 
		| list |
		list := PackageManager current packages asArray.
		1 to: list size do: [:i | 
			| each |
			each := list at: i.
			progress
				value: i // list size * 100;
				text: each name;
				yourself.
			(each packagePathname beginsWith: 'Jade\') ifTrue: [
				(each isChanged and: [each name ~= 'Jade Deployment']) ifTrue: [self halt].	"Are you deploying without saving the packages?"
				each save.
			].
		].
	]) ifNil: [self halt].! !
!JadeImageStripper categoriesFor: #closeLoginShells!public! !
!JadeImageStripper categoriesFor: #copyRuntimeFiles!public! !
!JadeImageStripper categoriesFor: #exeIconFile!operations!public! !
!JadeImageStripper categoriesFor: #finishedWith:!public! !
!JadeImageStripper categoriesFor: #loadJadeServerSourceCache!public! !
!JadeImageStripper categoriesFor: #logRemainingClasses!public! !
!JadeImageStripper categoriesFor: #prepareToStrip!public! !
!JadeImageStripper categoriesFor: #requiredClasses!public! !
!JadeImageStripper categoriesFor: #requiredPackageNames!public! !
!JadeImageStripper categoriesFor: #savePackages!public! !

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

