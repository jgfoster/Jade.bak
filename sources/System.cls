"Filed out from Dolphin Smalltalk 7"!

Object subclass: #System
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
System guid: (GUID fromString: '{be3a2d98-7395-4e3b-9cc9-4d18f69e26de}')!
System comment: ''!
!System categoriesForClass!Unclassified! !
!System class methodsFor!

clientEnvironmentVariable: aString

	^SessionManager current getenv: aString.
!

gemEnvironmentVariable: aString

	^SessionManager current getenv: aString.
!

performOnServer: aString

	| stream answer string counter batPath tmpPath outPath |
	batPath := File tempPath , 'gsCmd.bat'.
	tmpPath := File tempPath , 'gsCmd.tmp'.
	outPath := File tempPath , 'gsCmd.out'.
	self removeIfPresent: tmpPath.
	self removeIfPresent: outPath.
	stream := FileStream write: batPath.
	[
		stream nextPutAll: aString.
		stream 
			nextPutAll: ' > gsCmd.tmp 2>&1'; cr;
			nextPutAll: 'copy gsCmd.tmp gsCmd.out'; cr;
			nextPutAll: 'del gsCmd.tmp'; cr;
			yourself.
	] ensure: [
		stream close.
	].
	answer := ShellLibrary default
		shellExecute: View foregroundHandle
		lpOperation: 'open'
		lpFile: 'gsCmd.bat'
		lpParameters: nil
		lpDirectory: File tempPath
		nShowCmd: 0.
	answer := answer asInteger.
	answer <= 32 ifTrue: [self error: 'ShellExecute() failed with error # ' , answer printString].

	counter := 0.
	[
		stream := FileStream read: outPath.
	] on: Error do: [:ex | 
		(counter := counter + 1) > 100 ifTrue: [
			self error: 'Timeout (~50 seconds) wating for results.'.
		].
		Processor sleep: counter * 10.
		ex retry.
	].			
	[
		string := stream upToEnd.
	] ensure: [
		stream close.
	].
	counter := 0.
	[
		(File exists: batPath) ifTrue: [File delete: batPath].
		(File exists: outPath) ifTrue: [File delete: outPath].
	] on: Error do: [:ex | 
		(counter := counter + 1) > 10 ifTrue: [
			self error: 'Timeout (~10 seconds) waiting to delete file.'.
		].
		Processor sleep: counter * 10.
		ex retry.
	].			
	^string.
!

removeIfPresent: aString

	[
		File exists: aString.
	] whileTrue: [
		[
			GsFile removeServerFile: aString.
		] on: Error do: [:ex | 
			ex return.
		].
	].

! !
!System class categoriesFor: #clientEnvironmentVariable:!public! !
!System class categoriesFor: #gemEnvironmentVariable:!public! !
!System class categoriesFor: #performOnServer:!public! !
!System class categoriesFor: #removeIfPresent:!public! !

