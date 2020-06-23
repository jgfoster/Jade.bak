"Boot a Jade development image"

SessionManager current splashShowAtStartup: false.
SmalltalkSystemShell allInstances first position: 20@50.
(TranscriptShell allInstances , #(nil)) first ifNotNil: [:shell | shell view position: 20@600; extent: 800@300; setFocus].
!
| list |
list := #(
	'JGF Dolphin'
	'GemStone C Interface'
	'GemStone Objects'
	'GemStone Session'
	'VisualWorks Component'
	'Jade UI Base'
	'Jade Class Browser'
	'Jade Method Browser'
	'Jade Inspector'
	'Jade User Browser'
	'Monticello'
	'Jade UI'
	'Jade Autocompletation'
	'Jade Test Browser'
	'Jade System Browser'
	'Jade Transcript'
	'Jade Process Browser'
	'Object Log Browser'
	'Jade Login'
	'Jade Deployment'
	'Contacts Demo'
	'Jade in Dolphin'
	'Jade Tests'
	'Jade Tutorial'
).
(Delay forMilliseconds: 200) wait.
ProgressDialog showModalWhile: [:progress |
	1 to: list size do: [:i |
		| name percent |
		name := list at: i.
		percent := (i * 100 / list size) asInteger.
		progress
			value: percent;
			text: 'Loading package: ' , name;
			yourself.
		PackageManager current install: 'sources\' , name , '.pax'.
	].
].
!
IdeaSpaceShell allInstances isEmpty ifTrue: [
	IdeaSpaceShell show
		absorbCard: SmalltalkWorkspaceDocument show;
		absorbCard: PackageBrowserShell show;
		absorbCard: SystemBrowserShell show;
		position: 50@80; extent: 1200@700;
		yourself.
].
!
"Set window sizes and selection for Packages tab"
| container filterView layoutManager packagesView sources | 
container := (PackageBrowserShell allInstances first view viewNamed: 'packages') referee. 
filterView := container viewNamed: 'filter'. 
packagesView := container viewNamed: 'packages'. 
layoutManager := container layoutManager. 
layoutManager arrangementOf: filterView put: 1. 
layoutManager arrangementOf: packagesView put: 7. 
container layout. 
sources := filterView model asSet detect: [:each | each name = 'sources'].
filterView presenter selection: sources.
!
"Set window sizes and selection for System Browser tab"
| container filterView layoutManager packagesView sources | 
container := (SystemBrowserShell allInstances first view viewNamed: 'packages') referee. 
filterView := container viewNamed: 'filter'. 
packagesView := container viewNamed: 'packages'. 
layoutManager := container layoutManager. 
layoutManager arrangementOf: filterView put: 2. 
layoutManager arrangementOf: packagesView put: 5. 
container layout. 
sources := filterView model asSet detect: [:each | each name = 'sources'].
filterView presenter selection: sources.
!
ClassBrowserAbstract allSubclasses do: [:each | 
	(each plugins includes: DebugInfoPlugin) ifFalse: [each plugins add: DebugInfoPlugin]].
Debugger defaultExtent: 1000@600.
!
JadeLoginShell show
	position: 50@20;
	yourself.
Cursor wait showWhile: [
	Object recompileAll. "https://github.com/dolphinsmalltalk/Dolphin/issues/852#issuecomment-623143858"
	Transcript flush.	"don't clear Transcript; we want to see any errors"
	SourceManager default compressSources.
	(Delay forMilliseconds: 200) wait.
	SessionManager current saveImage.
].
!
