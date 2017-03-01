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
	'Jade Autocompletation'
	'Jade Inspector'
	'Jade Class Browser'
	'Jade Method Browser'
	'Jade User Browser'
	'Monticello'
	'Jade UI'
	'Jade System Browser'
	'Jade Transcript'
	'Jade Process Browser'
	'Object Log Browser'
	'Jade Login'
	'Jade Deployment'
	'Contacts Demo'
	'Jade in Dolphin'
	'Jade Tests'
	'Jade Autocompletation Enhancements'
	'Jade Test Browser'
	'Jade System Browser Tracking'
	'Jade Method Tree Browser'
).
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
SmalltalkSystemShell allInstances first 
	position: 20@50;
	yourself.
IdeaSpaceShell allInstances isEmpty ifTrue: [
	IdeaSpaceShell show
		absorbCard: SmalltalkWorkspaceDocument show;
		absorbCard: SystemBrowserShell show;
		position: 50@80; extent: 1200@700;
		yourself.
].
Debugger defaultExtent: 1000@600.
!
JadeLoginShell show
	position: 80@20;
	yourself.
Transcript clear.
(Delay forMilliseconds: 200) wait.
Cursor wait showWhile: [
	SourceManager default compressSources.
	SessionManager current saveImage.
].
!
