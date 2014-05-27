| package |
package := Package name: 'Jade Deployment'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.041'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAzIEYPEQAEAAAASmFkZUltYWdlU3RyaXBwZXIAAAAAUgAAAA8AAABKYWRlIERlcGxveW1l
bnRSAAAAEAAAAHJ1bnRpbWVcSmFkZS5leGWaAAAAUgAAAA8AAABKYWRlIERlcGxveW1lbnRSAAAA
EgAAAEphZGVTZXNzaW9uTWFuYWdlcu+3JQAAAAAABgMPAFZlcnNpb25SZXNvdXJjZQYBEABWU19G
SVhFREZJTEVJTkZPcgAAADQAAAC9BO/+AAABAAAAAQABAAAAAAABAAEAAAA/AAAAAAAAAAQAAAAC
AAAAAAAAAAAAAAAAAAAA6gAAAPAAAABiAAAAAgAAAFIAAAAIAAAAMDQwOTA0YjDqAAAA8AAAAGIA
AAAOAAAAUgAAAA4AAABQcm9kdWN0VmVyc2lvblIAAAAKAAAAMSwgMCwgMCwgMVIAAAAQAAAAT3Jp
Z2luYWxGaWxlbmFtZVIAAAAIAAAASmFkZS5leGVSAAAACwAAAFByb2R1Y3ROYW1lUgAAAB8AAABB
IERvbHBoaW4gWDYuMSBUb0dvIEFwcGxpY2F0aW9uUgAAAAgAAABDb21tZW50c1IAAAAcAAAAUG93
ZXJlZCBieSBEb2xwaGluIFNtYWxsdGFsa1IAAAAPAAAARmlsZURlc2NyaXB0aW9uUgAAAB0AAABE
b2xwaGluIFg2LjEgVG9HbyBBcHBsaWNhdGlvblIAAAALAAAARmlsZVZlcnNpb25SAAAACgAAADEs
IDAsIDAsIDFSAAAADgAAAExlZ2FsQ29weXJpZ2h0UgAAACsAAABQb3J0aW9ucyBDb3B5cmlnaHQg
qSBPYmplY3QgQXJ0cyAxOTk3LTIwMDguygAAANAAAABiAAAAAQAAAAYCCgBEV09SREFycmF5cgAA
AAQAAAAJBLAEAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA').
package basicScriptAt: #postinstall put: 'SessionManager current 
	when: #''sessionStarted'' 
	send: #''sessionStarted'' 
	to: JadeSessionManager.
''Loaded: Jade Deployment'' yourself.'.

package classNames
	add: #JadeImageStripper;
	add: #JadeSessionManager;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Dialogs\Progress\Dolphin Progress Dialog';
	add: '..\Object Arts\Dolphin\MVP\Views\Scintilla\Dolphin Scintilla View';
	add: 'GemStone C Interface';
	add: 'GemStone Services';
	add: 'GemStone Session';
	add: 'Jade Login';
	add: 'Jade UI';
	add: '..\Object Arts\Dolphin\Lagoon\Lagoon Image Stripper';
	add: '..\Object Arts\Dolphin\System\Compiler\Smalltalk Parser';
	yourself).

package!

"Class Definitions"!

ImageStripper subclass: #JadeImageStripper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RuntimeSessionManager subclass: #JadeSessionManager
	instanceVariableNames: ''
	classVariableNames: 'Version'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

JadeImageStripper guid: (GUID fromString: '{773AB738-9D79-41CB-B663-DF50BCE9C3E5}')!
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

exeIconFile

	^'icons\GS32x32.ico'.
!

finishedWith: selector

	selector = #'finishedWith:' ifTrue: [^self].
	super finishedWith: selector.
!

loadJadeServerSourceCache

	JadeServer withAllSubclassesDo: [:each | each gsString].
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
		savePackages;
		loadJadeServerSourceCache; 
		copyRuntimeFiles; 
		closeLoginShells;
		yourself.
	^super prepareToStrip.
!

requiredClasses

	^super requiredClasses
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
		yourself.

!

requiredPackageNames

	^super requiredPackageNames
		add: 'Dolphin MVP (Deprecated)';		"Referenced by EditableListView"
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
		yourself



!

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

JadeSessionManager guid: (GUID fromString: '{8D7A4642-B8E1-43EA-94D5-E13390CA1F79}')!
JadeSessionManager comment: ''!
!JadeSessionManager categoriesForClass!Unclassified! !
!JadeSessionManager methodsFor!

defaultResLibPath
	"Answer the path of the development resource library."

	^'bin\Jade'!

main
"
	JadeSessionManager basicNew main.
"
	Keyboard default isShiftDown 
		ifTrue: [self mainShellClass show: 'Full view']
		ifFalse: [self mainShellClass show].
!

version

	^Version notNil 
		ifTrue: [Version]
		ifFalse: ['(Development)'].
! !
!JadeSessionManager categoriesFor: #defaultResLibPath!public! !
!JadeSessionManager categoriesFor: #main!public! !
!JadeSessionManager categoriesFor: #version!public! !

!JadeSessionManager class methodsFor!

getVersion

	| list |
	list := [Package manager sourceControl getProjectEditionsFor: 'Jade'] on: Error do: [:ex | ^Date today printString].
	^(list at: 1) projectVersion
		ifNotNil: [:x | x]
		ifNil: [(list at: 2) projectVersion , '+'].
!

mainShellClass

	^JadeLoginShell.
!

sessionStarted

	GciLibrary sessionStarted.
!

setVersion

	Version := self getVersion.!

version

	^Version notNil 
		ifTrue: [Version]
		ifFalse: ['(Development)'].
! !
!JadeSessionManager class categoriesFor: #getVersion!public! !
!JadeSessionManager class categoriesFor: #mainShellClass!public! !
!JadeSessionManager class categoriesFor: #sessionStarted!public! !
!JadeSessionManager class categoriesFor: #setVersion!public! !
!JadeSessionManager class categoriesFor: #version!public! !

"Binary Globals"!

