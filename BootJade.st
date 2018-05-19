"Boot a Jade development image"

| list |
list := #(
	'JGF Dolphin'
	'JGF Fading Dialog'
	'GemStone C Interface'
	'GemStone Objects'
	'GemStone Session'
	'GemStone Services'
	'VisualWorks Component'
	'Jade UI Base'
	'Jade Class Browser'
	'Jade Method Browser'
	'Jade Inspector'
	'Jade Autocompletation'
	'Jade User Browser'
	'Monticello'
	'Jade UI'
	'Jade Test Browser'
	'Jade System Browser'
	'Jade Process Browser'
	'Object Log Browser'
	'Jade Login'
	'Jade Deployment'
	'Contacts Demo'
	'Jade in Dolphin'
	'Jade Tests'
	'Rowan UI Base'
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
SessionManager current splashShowAtStartup: false.
SmalltalkSystemShell allInstances first position: 20@50.
(TranscriptShell allInstances , #(nil)) first ifNotNil: [:shell | shell view position: 20@600].
IdeaSpaceShell allInstances isEmpty ifTrue: [
	IdeaSpaceShell show
		absorbCard: SmalltalkWorkspaceDocument show;
		absorbCard: PackageBrowserShell show;
		absorbCard: SystemBrowserShell show;
		position: 50@80; extent: 1200@700;
		yourself.
].
ClassBrowserAbstract allSubclasses do: [:each | 
	(each plugins includes: DebugInfoPlugin) ifFalse: [each plugins add: DebugInfoPlugin]].
Debugger defaultExtent: 1000@600.
!
RowanLoginShell show
	position: 80@20;
	yourself.
JadeLoginShell removeFromDevelopmentTools.
RowanLoginShell addToDevelopmentTools.
Transcript clear.
Cursor wait showWhile: [
	SourceManager default compressSources.
	(Delay forMilliseconds: 200) wait.
	SessionManager current saveImage.
].
!
