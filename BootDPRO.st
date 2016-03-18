"Boot a DPRO development image"
| list |
SessionManager current splashShowAtStartup: false.
SmalltalkSystemShell allInstances first 
	position: 20@50;
	yourself.
IdeaSpaceShell show
	absorbCard: SmalltalkWorkspaceDocument show;
	absorbCard: SystemBrowserShell show;
	position: 50@80; extent: 1200@750;
	yourself.
TestRunner show
	position: 900@40;
	yourself.
TranscriptShell show.
Debugger defaultExtent: 1000@600.
SourceManager default fileIn: 'RegressionTestsLoad.st'.
list := #(
	'Core\Contributions\JGFoster\StoreString for Views.pax'
	'Core\Contributions\JGFoster\StoreString for Views Test.pax'
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
		PackageManager current install: name.
	].
].
(Delay forMilliseconds: 200) wait.
Cursor wait showWhile: [
	SourceManager default compressSources.
	SessionManager current saveImage.
].
!
