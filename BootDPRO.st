"Boot a DPRO development image"
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
(Delay forMilliseconds: 200) wait.
Cursor wait showWhile: [
	SourceManager default compressSources.
	SessionManager current saveImage.
].
!
