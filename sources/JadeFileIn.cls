"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadeFileIn
	instanceVariableNames: 'gciSession stream line words text category className packageName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeFileIn guid: (GUID fromString: '{5eb74a05-1227-4d91-9750-01548085fa61}')!
JadeFileIn comment: ''!
!JadeFileIn categoriesForClass!Unclassified! !
!JadeFileIn methodsFor!

defineClass

	[
		gciSession 
			serverPerform: #'fileInClass:' 
			with: (gciSession encode: text trimBlanks).
	] on: GsCompileError do: [:ex | 
		(JadeWorkspace showOnSession: gciSession)
			caption: 'Jade Workspace - Compile Error';
			showError: ex list on: (gciSession decode: text) trimBlanks.
	].
!

defineClassMethod

	| name pieces |
	pieces := line subStrings: $:.
	name := 1 < pieces size
		ifTrue: [(pieces at: 2) subStrings first]
		ifFalse: [className].
	self
		defineMethod: self readUpToPercent
		inClassNamed: name , ' class'
		inPackageNamed: packageName
		inCategory: category.
!

defineInstanceMethod

	| name pieces |
	pieces := line subStrings: $:.
	name := 1 < pieces size
		ifTrue: [(pieces at: 2) trimBlanks]
		ifFalse: [className].
	self
		defineMethod: self readUpToPercent
		inClassNamed: name
		inPackageNamed: packageName
		inCategory: category.
!

defineMethod: methodString inClassNamed: className inPackageNamed: aPackageName inCategory: categoryName 
 
	| string result |
	string := className , 
		' compileMethod: ' , methodString printString , '
		dictionaries: System myUserProfile symbolList
		category: ' , categoryName printString.
	[
		(result := gciSession executeString: string) isNil ifTrue: [^self].
	] on: GsCompileError do: [:ex | 
		(JadeWorkspace showOnSession: gciSession)
			caption: 'Jade Workspace - Compile Error';
			showError: ex list on: string.
		^self.
	].
	result := GsCompileError
		errorListFor: result
		inSession: gciSession.
	(JadeWorkspace showOnSession: gciSession)
		caption: 'Jade Workspace - Compile Error on ' , className;
		showError: result on: methodString.
!

doIt

	self doIt: text.
!

doIt: aString

	[
		gciSession executeString: aString.
	] on: GsCompileError do: [:ex | 
		(JadeWorkspace showOnSession: gciSession)
			caption: 'Jade Workspace - Compile Error';
			showError: ex list on: aString.
	].
!

fileInPath: aString session: aGciSession

	packageName := (aString subStrings: $\) last.
	stream := FileStream read: aString type: #'utf8'.
	[
		self
			fileInStream: stream 
			session: aGciSession.
	] ensure: [
		stream close.
	].
!

fileInStream: aStream session: aGciSession

	gciSession := aGciSession.
	stream := aStream.
	self read.
!

fileInString: aString session: aGciSession

	self
		fileInStream: (ReadStream on: aString) 
		session: aGciSession!

fileInXML: aString forBrowser: aBrowser

	| source |
	source := VWSourceFile fromString: aString.
	source removeSPort.
	source := source asTopazFileIn.
	(JadeWorkspace showOnSession: aBrowser gciSession)
		caption: 'Jade Workspace - GemStone File-In';
		fileIn: source;
		yourself.
!

isDefineClass

	^(words size > 5 and: [
		(words at: 2) = 'subclass:' and: [
		(words at: 4) = 'instVarNames:']]) or: [
	(words size > 5 and: [
		(words at: 2) = 'byteSubclass:' and: [
		(words at: 4) = 'classVars:']])].
!

isPostloadScript

	^words size > 3 and: [
		(words copyFrom: 1 to: 3) = #('"' 'postload' 'script')].
!

isPreloadScript

	^words size > 3 and: [
		(words copyFrom: 1 to: 3) = #('"' 'preload' 'script')].
!

isRemoveAllMethods

	^words size = 4 and: [
		((words at: 2) beginsWith: 'removeAllMethods.') and: [
		(words at: 3) = 'class' and: [
		(words at: 4) beginsWith: 'removeAllMethods.']]].
!

processLine

	| ucLine |
	(line isEmpty or: [line first = $!!]) ifTrue: [^self].
	ucLine := line asUppercase.
	(ucLine beginsWith: 'CATEGORY:'					) ifTrue: [^self setCategory					].
	(ucLine beginsWith: 'CLASSMETHOD:'				) ifTrue: [^self defineClassMethod			].
	(ucLine beginsWith: 'COMMIT'					) ifTrue: [^self readCommit					].
	(ucLine beginsWith: 'DOIT'						) ifTrue: [^self readDoIt					].
	(ucLine beginsWith: 'ENV'						) ifTrue: [^self readSetCompileEnv			].
	(ucLine beginsWith: 'ERRORCOUNT'				) ifTrue: [^self readErrorCount				].
	(ucLine beginsWith: 'EXPECTVALUE'				) ifTrue: [^self readExpectValue				].
	(ucLine beginsWith: 'INPUT'					) ifTrue: [^self readInput					].
	(ucLine beginsWith: 'METHOD:'					) ifTrue: [^self defineInstanceMethod			].
	(ucLine beginsWith: 'RUN'						) ifTrue: [^self readDoIt					].
	(ucLine beginsWith: 'REMOVEALLMETHODS'		) ifTrue: [^self readRemoveAllMethods		].
	(ucLine beginsWith: 'REMOVEALLCLASSMETHODS'	) ifTrue: [^self readRemoveAllClassMethods	].
	(ucLine beginsWith: 'SEND'						) ifTrue: [^self readSend 					].
	(ucLine beginsWith: 'SET CLASS'					) ifTrue: [^self readSetClass				].
	(ucLine beginsWith: 'SET COMPILE_ENV:'			) ifTrue: [^self readSetCompileEnv			].

	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , ucLine printString , ' during file-in!!'.
	SessionManager current pause.
!

read

	stream reset.
	ProgressDialog showModalWhile: [:progress |
		self readWithProgress: progress.
	].
!

readCommit

	self doIt: 'System commitTransaction'.
!

readDoIt

	text := self readUpToPercent.
	words := (text subStrings: Character space) reject: [:each | each isEmpty].
	self isDefineClass 			ifTrue: [^self defineClass			].
	self isRemoveAllMethods 	ifTrue: [^self removeMethods	].
	self doIt.
!

readErrorCount
!

readExpectValue
!

readInput

	| path |
	path := line copyFrom: 7 to: line size.
	path := path
		copyReplaceAll: '/'
		with: '\'.
	self fileInPath: path.
!

readRemoveAllClassMethods

	| string |
	string := line copyFrom: 22 to: line size.
	self doIt: string , ' class removeAllMethods'.
!

readRemoveAllMethods

	| string |
	string := line copyFrom: 18 to: line size.
	self doIt: string , ' removeAllMethods'.
!

readSend

	text := line copyFrom: 6 to: line size.
	self doIt.
!

readSetClass

	className := line subStrings last.!

readSetCompileEnv

	line asUppercase = 'ENV 0' ifTrue: [^self].
	line asUppercase = 'SET COMPILE_ENV: 0' ifTrue: [^self].
	self error: 'Non-zero compile environment not supported!!'.
!

readUpToPercent

	| nextLine writeStream string |
	writeStream := WriteStream on: String new.
	[
		stream atEnd not and: [
			nextLine := stream nextLine.
			(nextLine beginsWith: '%') not.
		].
	] whileTrue: [
		writeStream nextPutAll: nextLine; lf.
	].
	string := writeStream contents.
	^string.
!

readWithProgress: progress

	[
		self readWithProgressA: progress.
	] on: Error do: [:ex | 
		| myStream |
		SessionManager current logError: ex.
		myStream := WriteStream on: String new.
		ex printTraceOn: myStream.
		(JadeWorkspace showOnSession: gciSession) showText: myStream contents.
		MessageBox notify: ex description.
		SessionManager current pause.
	].
!

readWithProgressA: progress

	[
		stream atEnd not.
	] whileTrue: [
		line := stream nextLine.
		progress 
			value: stream position * 100 // stream size;
			text: line;
			yourself.
		[
			self processLine.
		] on: TerminateProcess do: [:ex | 
			stream setToEnd.
			ex return.		"No need to terminate this Dolphin process (which is the default behavior)"
		].
	].
!

removeMethods

	self doIt.
!

reportCompileFailure: anIXMLDOMElement 

	| errors readStream writeStream begin |
	errors := (anIXMLDOMElement getElementsByTagName: 'error') collect: [:each | 
		(each getAttribute: 'offset') asNumber -> (each text , ' (#' , (each getAttribute: 'number') , ')').
	].
	readStream := ReadStream on: text.
	writeStream := WriteStream on: String new.
	begin := 1.
	errors asSortedCollection do: [:each |
		writeStream 
			nextPutAll: (readStream next: each key - begin);
			nextPutAll: '{';
			nextPutAll: each value;
			nextPutAll: '}';
			yourself.
		begin := each key.
	].
	writeStream nextPutAll: readStream upToEnd.	
	MessageBox notify: writeStream contents.
!

setCategory

	category := (line subStrings: $') at: 2.
!

stream: aStream

	stream := aStream.
! !
!JadeFileIn categoriesFor: #defineClass!public! !
!JadeFileIn categoriesFor: #defineClassMethod!public! !
!JadeFileIn categoriesFor: #defineInstanceMethod!public! !
!JadeFileIn categoriesFor: #defineMethod:inClassNamed:inPackageNamed:inCategory:!public! !
!JadeFileIn categoriesFor: #doIt!public! !
!JadeFileIn categoriesFor: #doIt:!public! !
!JadeFileIn categoriesFor: #fileInPath:session:!public! !
!JadeFileIn categoriesFor: #fileInStream:session:!public! !
!JadeFileIn categoriesFor: #fileInString:session:!public! !
!JadeFileIn categoriesFor: #fileInXML:forBrowser:!public! !
!JadeFileIn categoriesFor: #isDefineClass!public! !
!JadeFileIn categoriesFor: #isPostloadScript!public! !
!JadeFileIn categoriesFor: #isPreloadScript!public! !
!JadeFileIn categoriesFor: #isRemoveAllMethods!public! !
!JadeFileIn categoriesFor: #processLine!public! !
!JadeFileIn categoriesFor: #read!public! !
!JadeFileIn categoriesFor: #readCommit!public! !
!JadeFileIn categoriesFor: #readDoIt!public! !
!JadeFileIn categoriesFor: #readErrorCount!public! !
!JadeFileIn categoriesFor: #readExpectValue!public! !
!JadeFileIn categoriesFor: #readInput!public! !
!JadeFileIn categoriesFor: #readRemoveAllClassMethods!public! !
!JadeFileIn categoriesFor: #readRemoveAllMethods!public! !
!JadeFileIn categoriesFor: #readSend!public! !
!JadeFileIn categoriesFor: #readSetClass!public! !
!JadeFileIn categoriesFor: #readSetCompileEnv!public! !
!JadeFileIn categoriesFor: #readUpToPercent!public! !
!JadeFileIn categoriesFor: #readWithProgress:!public! !
!JadeFileIn categoriesFor: #readWithProgressA:!public! !
!JadeFileIn categoriesFor: #removeMethods!public! !
!JadeFileIn categoriesFor: #reportCompileFailure:!public! !
!JadeFileIn categoriesFor: #setCategory!packages!public! !
!JadeFileIn categoriesFor: #stream:!public! !

