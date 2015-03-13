| package |
package := Package name: 'Jade UI'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.194'.

package basicScriptAt: #postinstall put: '''Loaded: Jade UI'' yourself.'.

package classNames
	add: #JadeCodeBrowser;
	add: #JadeDebugger;
	add: #JadeErrorShell;
	add: #JadeFileIn;
	add: #JadeMethodList;
	add: #WaitOnGemStoneDialog;
	yourself.

package methodNames
	add: #GsError -> #debugError;
	add: #GsObject -> #gsInspect;
	add: #GsProcess -> #step:;
	add: #GsProcess -> #terminate;
	add: #GsProcess -> #trimStackToLevel:;
	add: #JadeServer -> #_oopAndStringFor:;
	add: #JadeServer -> #_sourceForProcess:frame:;
	add: #JadeServer -> #_trimStackOf:toLevel:;
	add: #JadeServer -> #aboutToDebugProcess:;
	add: #JadeServer -> #asAsciiString:;
	add: #JadeServer -> #compile:frame:process:;
	add: #JadeServer -> #isResumableCategory:number:context:;
	add: #JadeServer -> #step:inFrame:;
	add: #JadeServer32bit -> #_oopAndStringFor:;
	add: #JadeServer32bit -> #_trimStackOf:toLevel:;
	add: #JadeServer32bit -> #recompile:withSource:;
	add: #JadeServer32bit -> #sourceForProcess:frame:;
	add: #JadeServer64bit -> #_oopAndStringFor:;
	add: #JadeServer64bit -> #_trimStackOf:toLevel:;
	add: #JadeServer64bit -> #recompile:withSource:;
	add: #JadeServer64bit -> #sourceForProcess:frame:;
	add: #JadeTextDocument -> #fileIn;
	add: #JadeTextDocument -> #jadeInspect;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Dialogs\Progress\Dolphin Progress Dialog';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter';
	add: '..\Object Arts\Dolphin\MVP\Views\Scintilla\Dolphin Scintilla View';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'GemStone Objects';
	add: 'GemStone Session';
	add: 'Jade Inspector';
	add: 'Jade UI Base';
	add: '..\JGF\JGF Fading Dialog';
	add: 'VisualWorks Component';
	yourself).

package!

"Class Definitions"!

Object subclass: #JadeFileIn
	instanceVariableNames: 'stream line words text category browser packageName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FadingDialog subclass: #WaitOnGemStoneDialog
	instanceVariableNames: 'busySeconds codePresenter busySecondsPresenter gciSession timerProcess'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeValueDialog subclass: #JadeErrorShell
	instanceVariableNames: 'messagePresenter stackPresenter textEditPresenter isResumable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeTextDocument subclass: #JadeCodeBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeCodeBrowser subclass: #JadeDebugger
	instanceVariableNames: 'answer errorMessagePresenter frame frameListPresenter gsProcess processList processListPresenter terminateOnClose variableDataPresenter variableListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'debuggers'!
JadeCodeBrowser subclass: #JadeMethodList
	instanceVariableNames: 'methodListPresenter searchString captionString'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!GsError methodsFor!

debugError

	^JadeDebugger reportError: self.

! !
!GsError categoriesFor: #debugError!public! !

!GsObject methodsFor!

gsInspect

	JadeInspector showOn: gciSession -> self oopType.
! !
!GsObject categoriesFor: #gsInspect!public! !

!GsProcess methodsFor!

step: anInteger

	stack := nil.
	^gciSession
		step: self
		inFrame: anInteger.
!

terminate

	gciSession terminate: oopType.
	self error: 'Should not get here!!'.
!

trimStackToLevel: anInteger

	stack := nil.
	gciSession
		send: #'_trimStackToLevel:'
		to: self
		withAll: (Array with: anInteger).
! !
!GsProcess categoriesFor: #step:!public! !
!GsProcess categoriesFor: #terminate!public! !
!GsProcess categoriesFor: #trimStackToLevel:!public! !

!JadeServer methodsFor!

_oopAndStringFor: anObject

	^(self oopOf: anObject) -> anObject printString.
!

_sourceForProcess: gsProcess frame: level

	| frame stepPoint keys values gsMethod stream receiver |
	stream := WriteStream on: String new.
	(frame := gsProcess _frameContentsAt: level) isNil ifTrue: [^'No frame found for level ' , level printString].
	gsMethod := frame at: 1.
	stepPoint := (gsMethod respondsTo: #'_stepPointForIp:level:quick:')  ifTrue: [
		gsMethod
			_stepPointForIp: (frame at: 2) 
			level: level 
			quick: false.
	] ifFalse: [
		gsMethod
			_stepPointForIp: (frame at: 2) 
			level: level 
			isNative: gsProcess _nativeStack.
	].
	stream
		nextPutAll: '<?xml version=''1.0'' ?><frame oop=';
		nextPutAll: (self oopOf: frame) printString printString;
		nextPutAll: ' ipOffset=';
		nextPutAll: (frame at: 2) printString printString;
		nextPutAll: ' frameOffset=';
		nextPutAll: ((frame at: 3) isNil ifTrue: [''] ifFalse: [(frame at: 3) printString]) printString;
		nextPutAll: ' stepPoint=';
		nextPutAll: stepPoint printString printString;
		nextPutAll: '>'; lf;
		yourself.
	receiver := frame at: 10.
	values := OrderedCollection new.
	(self isClientForwarder: receiver) ifTrue: [
		keys := OrderedCollection with: 'clientObject'.
		values add: receiver clientObject.
		receiver := '[aClientForwarder(' , (self oopOf: receiver) printString , ')]'.
	] ifFalse: [
		((receiver isKindOf: BlockClosure) or: [receiver isKindOf: Class]) ifTrue: [
			keys := OrderedCollection new.
		] ifFalse: [
			keys := receiver class allInstVarNames asOrderedCollection collect: [:each | '-' , each].
			1 to: keys size do: [:i |
				values add: (receiver instVarAt: i).
			].
		].
	].
	keys addFirst: #'receiver'.
	values addFirst: receiver.
	keys addAll: (frame at: 9).
	keys := keys reject: [:each | each first == $.].
	values addAll: (frame size >= 11
		ifTrue: [frame copyFrom: 11 to: frame size]
		ifFalse: [#()]).
	1 to: (keys size min: values size) do: [:i | | oop assoc key value |
		key := keys at: i.
		value := values at: i.
		assoc := self _oopAndStringFor: value.
		oop := assoc key.
		value := assoc value.
		value size > 500 ifTrue: [value := (value copyFrom: 1 to: 500) , '...'].
		value := value collect: [:char | (char asciiValue < 32 or: [127 < char asciiValue]) ifTrue: [$?] ifFalse: [char]].
		stream
			nextPutAll: '<var oop=';
			nextPutAll: oop asString printString;
			nextPutAll: ' name=';
			nextPutAll: key asString printString;
			nextPutAll: ' ><';
			nextPutAll: '!![';
			nextPutAll: 'CDATA';
			nextPutAll: '[';
			nextPutAll: value;
			nextPutAll: ']';
			nextPutAll: ']';
			nextPutAll: '></var>'; lf;
			yourself.
	].
	gsMethod _sourceOffsets do: [:each | 
		stream
			nextPutAll: '<offset x=';
			nextPutAll: each printString printString;
			nextPutAll: '/>'; lf;
			yourself.
	].
	(gsMethod class canUnderstand: #'_breakpointIpOffsets') ifTrue: [
		(gsMethod _stepPointsFromBreakIpOffsets: gsMethod _breakpointIpOffsets) do: [:each | 
			stream
				nextPutAll: '<break x=';
				nextPutAll: each printString printString;
				nextPutAll: '/>'; lf;
				yourself.
		].
	].
	stream 
		nextPutAll: '<source';
		nextPutAll: ' ><';
		nextPutAll: '!![';
		nextPutAll: 'CDATA';
		nextPutAll: '[';
		nextPutAll: gsMethod _sourceString;
		nextPutAll: ']';
		nextPutAll: ']';
		nextPutAll: '></source>';
		nextPutAll: '</frame>'; lf;
		yourself.
	^self asAsciiString: stream contents.
!

_trimStackOf: aGsProcess toLevel: anInteger

	aGsProcess _trimStackToLevel: anInteger.
	^aGsProcess.
!

aboutToDebugProcess: aGsProcess

	| testCaseClass |
	aGsProcess isNil ifTrue: [^aGsProcess].
	((Object respondsTo: #'_doesNotUnderstand:') and: [3 < aGsProcess stackDepth and: [(Object compiledMethodAt: #'_doesNotUnderstand:') == (aGsProcess localMethodAt: 3)]]) ifTrue: [
		^self _trimStackOf: aGsProcess toLevel: 3.
	].
	(5 < aGsProcess stackDepth and: [(Object compiledMethodAt: #'error:') == (aGsProcess localMethodAt: 5)]) ifTrue: [
		^self _trimStackOf: aGsProcess toLevel:  4.
	].
	testCaseClass := self objectNamed: #'GSTestCase'.
	(testCaseClass notNil and: [Behavior respondsTo: #'compiledMethodAt:otherwise:']) ifTrue: [
		(6 < aGsProcess stackDepth and: [(testCaseClass compiledMethodAt: #'assert:' otherwise: nil) == (aGsProcess localMethodAt: 6)]) ifTrue: [
			^self _trimStackOf: aGsProcess toLevel:  6.
		].
	].
	^aGsProcess.
!

asAsciiString: aString

	^String withAll: (aString asArray collect: [:char | 
		((32 <= char asciiValue and: [char asciiValue <= 127]) or: [char isSeparator])
			ifTrue: [char]
			ifFalse: [$?].
	]).
!

compile: aString frame: anInteger process: aGsProcess
	"Compile method from within debugger"

	| oldMethod aBehavior selector category result |
	oldMethod := aGsProcess localMethodAt: anInteger.
	result := self 
		recompile: oldMethod
		withSource: aString.
	(result isKindOf: Boolean) ifTrue: [^result].
	aBehavior := oldMethod inClass.
	selector := oldMethod selector.
	selector isNil ifTrue: [^result].
	category := aBehavior categoryOfSelector: selector.
	result := self
		compileMethod: aString 
		behavior: aBehavior 
		user: nil 
		inCategory: category.
	^result.!

isResumableCategory: category number: number context: context

	| exceptionA receiver |
	category == GemStoneError ifTrue: [
		^number // 1000 = 2 or: [number // 1000 = 6].
	].
	(exceptionA := Globals at: #ExceptionA ifAbsent: [nil]) isNil ifTrue: [
		^true.
	].
	receiver := (context _frameContentsAt: 1) at: 8.
	(receiver isKindOf: exceptionA) ifTrue: [
		^receiver isResumable.
	].
	^true.
!

step: aGsProcess inFrame: anInteger

	^aGsProcess _stepOverInFrame: anInteger.
! !
!JadeServer categoriesFor: #_oopAndStringFor:!Debugger!public! !
!JadeServer categoriesFor: #_sourceForProcess:frame:!Debugger!public! !
!JadeServer categoriesFor: #_trimStackOf:toLevel:!Debugger!public! !
!JadeServer categoriesFor: #aboutToDebugProcess:!Debugger!public! !
!JadeServer categoriesFor: #asAsciiString:!Debugger!public! !
!JadeServer categoriesFor: #compile:frame:process:!Debugger!public! !
!JadeServer categoriesFor: #isResumableCategory:number:context:!Debugger!public! !
!JadeServer categoriesFor: #step:inFrame:!Debugger!public! !

!JadeServer32bit methodsFor!

_oopAndStringFor: anObject

	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | ^0 -> '<ERROR IN #printString>'].
	^super _oopAndStringFor: anObject.
!

_trimStackOf: aGsProcess toLevel: anInteger

	Exception
		category: GemStoneError
		number: 2023 
		do: [:ex :cat :num :args | 
			ex remove.
			^self 
				_trimStackOf: aGsProcess 
				toLevel: anInteger - 1.
	].
	^super
		_trimStackOf: aGsProcess 
		toLevel: anInteger.
!

recompile: aMethod withSource: aString

	Exception
		category: nil 
		number: nil 
		do: [:ex :cat :num :args | ^'compile error found in JadeServer>>#compile:frame:process:'].
	^aMethod _recompileWithSource: aString.
!

sourceForProcess: gsProcess frame: level

	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | 
			^self asAsciiString: ('?????' , ex printString , ' - ' , num printString , ' - ' ,  args printString , 
				Character cr asString , (GsProcess stackReportToLevel: 50))].
	^self
		_sourceForProcess: gsProcess 
		frame: level.
! !
!JadeServer32bit categoriesFor: #_oopAndStringFor:!Debugger!public! !
!JadeServer32bit categoriesFor: #_trimStackOf:toLevel:!Debugger!public! !
!JadeServer32bit categoriesFor: #recompile:withSource:!Debugger!public! !
!JadeServer32bit categoriesFor: #sourceForProcess:frame:!Debugger!public! !

!JadeServer64bit methodsFor!

_oopAndStringFor: anObject

	^[
		super _oopAndStringFor: anObject.
	] on: Error do: [:ex | 
		ex return: 0 -> ('<ERROR IN #printString for ' , anObject class name , '>').
	].
!

_trimStackOf: aGsProcess toLevel: anInteger

	^[
		super
			_trimStackOf: aGsProcess 
			toLevel: anInteger.
	] on: Error do: [:ex | 
		self 
			_trimStackOf: aGsProcess 
			toLevel: anInteger - 1.
		ex return.
	].
!

recompile: aMethod withSource: aString

	^[
		| result |
		result := aMethod _recompileWithSource: aString.
		result isNil ifTrue: [^true].		"Bug 41195 returns nil if success so assume it is the same method"
		result.
	] on: Error do: [:ex | 
		ex description.
	].
!

sourceForProcess: gsProcess frame: level

	^[
		self
			_sourceForProcess: gsProcess 
			frame: level.
	] on: Error do: [:ex | 
			self return: (self asAsciiString: ('?????' , ex description , Character cr asString , (GsProcess stackReportToLevel: 50))).
	].
! !
!JadeServer64bit categoriesFor: #_oopAndStringFor:!Debugger!public! !
!JadeServer64bit categoriesFor: #_trimStackOf:toLevel:!Debugger!public! !
!JadeServer64bit categoriesFor: #recompile:withSource:!Debugger!public! !
!JadeServer64bit categoriesFor: #sourceForProcess:frame:!Debugger!public! !

!JadeTextDocument methodsFor!

fileIn

	[
		JadeFileIn new
			fileInString: self activeTextEdit selection
			forBrowser: self.
	] on: Error do: [:ex | 
		| stream |
		SessionManager current logError: ex.
		stream := WriteStream on: String new.
		ex printTraceOn: stream.
		(JadeWorkspace showOn: model) showText: stream contents.
		MessageBox notify: ex description.
		Keyboard default isShiftDown ifTrue: [ex halt].
	].
!

jadeInspect

	| result |
	result := self jadeExecuteAndDisplay: false.
	JadeInspector showOn: gciSession -> result.
! !
!JadeTextDocument categoriesFor: #fileIn!private! !
!JadeTextDocument categoriesFor: #jadeInspect!Jade!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

JadeFileIn guid: (GUID fromString: '{5EB74A05-1227-4D91-9750-01548085FA61}')!
JadeFileIn comment: ''!
!JadeFileIn categoriesForClass!Unclassified! !
!JadeFileIn methodsFor!

defineClass

	browser
		defineClass: text trimBlanks
		inPackageNamed: packageName.
!

defineClassMethod

	| className |
	className := ((line subStrings: $:) at: 2) subStrings first.
	browser
		defineClassMethod: self readUpToPercent
		inClassNamed: className
		inPackageNamed: packageName
		inCategory: category.
!

defineInstanceMethod

	browser
		defineMethod: self readUpToPercent
		inClassNamed: ((line subStrings: $:) at: 2) trimBlanks
		inPackageNamed: packageName
		inCategory: category.
!

doIt

	browser doIt: text.
!

fileInPath: aString forBrowser: aBrowser

	packageName := (aString subStrings: $\) last.
	stream := FileStream read: aString.
	[
		self
			fileInStream: stream 
			forBrowser: aBrowser.
	] ensure: [
		stream close.
	].
!

fileInStream: aStream forBrowser: aBrowser

	browser := aBrowser.
	browser fileInStart: packageName.
	[
		stream := aStream.
		self read.
	] ensure: [
		browser fileInEnd: packageName.
	].
!

fileInString: aString forBrowser: aBrowser

	(aString beginsWith: '<?xml version="1.0"?>') ifTrue: [
		self fileInXML: aString forBrowser: aBrowser.
		^self.
	].
	self
		fileInStream: (ReadStream on: aString) 
		forBrowser: aBrowser.

!

fileInXML: aString forBrowser: aBrowser

	| source |
	source := VWSourceFile fromString: aString.
	source removeSPort.
	source := source asTopazFileIn.
	(JadeWorkspace showOn: aBrowser gciSession)
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
	(ucLine beginsWith: 'CATEGORY:'		) ifTrue: [^self setCategory		].
	(ucLine beginsWith: 'CLASSMETHOD:'	) ifTrue: [^self defineClassMethod	].
	(ucLine beginsWith: 'DOIT'			) ifTrue: [^self readDoIt			].
	(ucLine beginsWith: 'INPUT'			) ifTrue: [^self readInput			].
	(ucLine beginsWith: 'METHOD:'		) ifTrue: [^self defineInstanceMethod	].
	(ucLine beginsWith: 'RUN'			) ifTrue: [^self readDoIt			].
	(ucLine beginsWith: 'EXPECTVALUE'	) ifTrue: [^self readExpectValue		].
	(ucLine beginsWith: 'REMOVEALLMETHODS'	) ifTrue: [^self readRemoveAllMethods	].
	(ucLine beginsWith: 'REMOVEALLCLASSMETHODS'	) ifTrue: [^self readRemoveAllClassMethods	].
	(ucLine beginsWith: 'ERRORCOUNT'	) ifTrue: [^self readErrorCount	].
	(ucLine beginsWith: 'COMMIT'	) ifTrue: [^self readCommit	].
	(ucLine beginsWith: 'SEND') ifTrue: [^self readSend ].
	(ucLine beginsWith: 'SET COMPILE_ENV:') ifTrue: [^self readSetCompileEnv].

	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , ucLine printString , ' during file-in!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
!

read

	stream reset.
	ProgressDialog showModalWhile: [:progress |
		self readWithProgress: progress.
	].
!

readCommit

	browser doIt: 'System commitTransaction'.
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
		copyReplaceAll: '$l2tests'
		with: '\\samba\denile2\users\jfoster\checkouts\gss64bit11\tests'.
	path := path
		copyReplaceAll: '/'
		with: '\'.
	browser fileInPath: path.
!

readRemoveAllClassMethods

	| string |
	string := line copyFrom: 22 to: line size.
	browser doIt: string , ' class removeAllMethods'.
!

readRemoveAllMethods

	| string |
	string := line copyFrom: 18 to: line size.
	browser doIt: string , ' removeAllMethods'.
!

readSend

	text := line copyFrom: 6 to: line size.
	self doIt.
!

readSetCompileEnv

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
		(JadeWorkspace showOn: browser gciSession) showText: myStream contents.
		MessageBox notify: ex description.
		Keyboard default isShiftDown ifTrue: [ex halt].
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
!JadeFileIn categoriesFor: #doIt!public! !
!JadeFileIn categoriesFor: #fileInPath:forBrowser:!public! !
!JadeFileIn categoriesFor: #fileInStream:forBrowser:!public! !
!JadeFileIn categoriesFor: #fileInString:forBrowser:!public! !
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
!JadeFileIn categoriesFor: #readSetCompileEnv!public! !
!JadeFileIn categoriesFor: #readUpToPercent!public! !
!JadeFileIn categoriesFor: #readWithProgress:!public! !
!JadeFileIn categoriesFor: #readWithProgressA:!public! !
!JadeFileIn categoriesFor: #removeMethods!public! !
!JadeFileIn categoriesFor: #reportCompileFailure:!public! !
!JadeFileIn categoriesFor: #setCategory!packages!public! !
!JadeFileIn categoriesFor: #stream:!public! !

WaitOnGemStoneDialog guid: (GUID fromString: '{3C1B5A72-A9D7-4D7D-92EB-6BE38EFC109C}')!
WaitOnGemStoneDialog comment: ''!
!WaitOnGemStoneDialog categoriesForClass!Unclassified! !
!WaitOnGemStoneDialog methodsFor!

busySeconds: anInteger

	busySeconds := anInteger.
!

createComponents

	super createComponents.
	codePresenter 				:= self add: TextPresenter new name: 'code'.
	busySecondsPresenter 	:= self add: TextPresenter new name: 'busySeconds'.
!

gciSession: aGciSession

	gciSession := aGciSession.
!

message: aString

	codePresenter value: aString.
!

onViewClosed

	timerProcess notNil ifTrue: [
		| temp |
		temp := timerProcess.
		timerProcess := nil.
		temp terminate.
	].
	super onViewClosed.
!

onViewOpened

	busySeconds := 0.		"ensure that value is not nil before we start the update process!!"
	super onViewOpened.
	timerProcess := [
		self updateSeconds.
	] forkAt: Processor userBackgroundPriority.!

sendHardBreak

	gciSession hardBreak.
!

sendSoftBreak

	gciSession softBreak.
!

updateSeconds

	[
		self view = DeafObject current.
	] whileFalse: [
		busySecondsPresenter value: busySeconds.
		busySeconds := busySeconds + 1.
		Processor sleep: 1000.
	].
! !
!WaitOnGemStoneDialog categoriesFor: #busySeconds:!public! !
!WaitOnGemStoneDialog categoriesFor: #createComponents!public! !
!WaitOnGemStoneDialog categoriesFor: #gciSession:!public! !
!WaitOnGemStoneDialog categoriesFor: #message:!public! !
!WaitOnGemStoneDialog categoriesFor: #onViewClosed!public! !
!WaitOnGemStoneDialog categoriesFor: #onViewOpened!public! !
!WaitOnGemStoneDialog categoriesFor: #sendHardBreak!public! !
!WaitOnGemStoneDialog categoriesFor: #sendSoftBreak!public! !
!WaitOnGemStoneDialog categoriesFor: #updateSeconds!public! !

!WaitOnGemStoneDialog class methodsFor!

gciSession: aGciSession message: aString havingWaited: anInteger

	^self create
		gciSession: aGciSession;
		message: aString;
		busySeconds: anInteger;
		yourself.
!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.LayeredDialogView)  98 30 0 0 98 2 26214401 1179649 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 10 410 8 ##(Smalltalk.ScintillaView)  98 46 0 416 98 2 8 1176571972 1025 592 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 482 8 4278190080 0 7 0 0 0 592 0 8 4294902267 852486 ##(Smalltalk.NullConverter)  0 0 11 0 234 256 98 42 8 #specialSelector 1182726 ##(Smalltalk.ScintillaTextStyle)  33 196934 1 ##(Smalltalk.RGB)  16646145 0 1 0 0 0 0 848 0 0 0 8 #lineNumber 866 67 0 0 1 0 0 0 0 928 0 0 0 8 #global 866 21 0 0 1 0 0 0 0 960 0 0 0 8 #normal 866 1 0 0 1 0 0 0 0 992 0 0 0 8 #boolean 866 13 912 0 1 0 0 0 0 1024 0 0 0 8 #special 866 25 0 0 1 0 0 0 0 1056 0 0 0 8 #number 866 5 898 16711169 0 1 0 0 0 0 1088 0 0 0 8 #nil 866 19 912 0 1 0 0 0 0 1136 0 0 0 8 #character 866 31 898 16646399 0 1 0 0 0 0 1168 0 0 0 8 #indentGuide 866 75 786694 ##(Smalltalk.IndexedColor)  33554447 0 1 0 0 0 0 1216 0 0 0 8 #braceHighlight 866 69 1250 33554465 0 1 0 0 0 0 1280 0 0 0 8 #string 866 3 898 16646399 0 129 0 0 0 0 1328 0 0 0 8 #symbol 866 9 1250 33554443 0 1 0 0 0 0 1376 0 0 0 8 #super 866 17 912 0 1 0 0 0 0 1424 0 0 0 8 #comment 866 7 898 65025 0 1 0 0 0 0 1456 0 0 0 8 #binary 866 11 1250 33554433 0 1 0 0 0 0 1504 0 0 0 8 #assignment 866 29 0 0 1 0 0 0 0 1552 0 0 0 8 #keywordSend 866 27 1250 33554437 0 1 0 0 0 0 1584 0 0 0 8 #return 866 23 898 321 0 1 0 0 0 0 1632 0 0 0 8 #braceMismatch 866 71 1250 33554459 0 1 0 0 0 0 1680 0 0 0 8 #self 866 15 912 0 1 0 0 0 0 1728 0 0 0 98 40 1008 1344 1104 1472 1392 1520 1040 1744 1440 1152 976 1648 1072 1600 1568 1184 880 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 944 1296 1696 0 1232 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  992 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 1536 1250 33554471 592 8 #circle 202 208 98 0 0 63 9215 0 0 0 0 1264 0 0 0 0 0 0 8 '' 3 234 256 98 4 8 #smalltalk 816 8 #container 234 256 98 2 992 866 1 0 0 1 0 0 0 0 992 0 0 0 0 0 0 0 1 0 234 256 98 6 8 'indicator2' 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  5 592 511 1 32 0 0 8 'indicator1' 2130 3 592 33423361 5 32 0 0 8 'indicator0' 2130 1 592 65025 3 32 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 9 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 2354 789 495 592 2290 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 592 2290 8 #isTextModified: 98 1 32 592 2290 8 #modificationEventMask: 98 1 9215 592 2290 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 592 1 3 32 1 2642 3 592 33 1 16 67108863 2642 5 592 1 1 16 -67108863 592 2290 8 #indentationGuides: 98 1 0 592 2290 8 #tabIndents: 98 1 16 592 2290 8 #tabWidth: 98 1 9 592 2290 8 #setLexerLanguage: 98 1 2000 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 138 1 0 0 247 0 0 0] 98 0 2354 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 3010 8 #fixedParentRight 1 3010 8 #fixedParentTop 1 3010 8 #fixedParentBottom -49 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 3152 0 0 0 7 0 0 0 3152 0 8 4294903167 1180998 4 ##(Smalltalk.CommandDescription)  8 #sendHardBreak 8 'Hard Break' 1 1 0 0 32 0 0 0 2226 202 208 98 3 2290 2320 98 2 2354 469 495 2354 161 51 3152 2290 8 #isEnabled: 98 1 32 3152 2290 8 #text: 98 1 8 'Hard Break' 3152 2898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 234 0 0 0 247 0 0 0 58 1 0 0 16 1 0 0] 98 0 2960 0 29 2978 3056 -319 3010 8 #fixedViewLeft 161 3120 -49 3010 8 #fixedViewTop 51 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 3664 0 0 0 7 0 0 0 3664 0 8 4294902319 786 0 0 0 2226 202 208 98 2 2290 2320 98 2 2354 11 501 2354 211 31 3664 2290 3488 98 1 8 'Busy for seconds:' 3664 2898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 250 0 0 0 110 0 0 0 9 1 0 0] 98 0 2960 0 27 2978 3024 11 3600 211 3088 501 3632 31 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916354 1025 4000 0 482 8 4278190080 0 7 0 0 0 4000 0 8 4294903767 786 0 0 3 2226 202 208 98 3 2290 2320 98 2 2354 225 493 2354 81 41 4000 2290 2416 98 1 2450 3 1 3 4000 2290 2496 98 1 32 4000 2898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 112 0 0 0 246 0 0 0 152 0 0 0 10 1 0 0] 98 0 2960 0 27 2978 3024 225 3600 81 3088 493 3632 41 410 3168 98 20 0 416 98 2 8 1140924416 1 4400 0 0 0 7 0 0 0 4400 0 8 4294903167 3250 8 #sendSoftBreak 8 'Soft Break' 1 1 0 0 32 0 0 0 2226 202 208 98 3 2290 2320 98 2 2354 629 495 2354 161 51 4400 2290 3440 98 1 32 4400 2290 3488 98 1 8 'Soft Break' 4400 2898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 58 1 0 0 247 0 0 0 138 1 0 0 16 1 0 0] 98 0 2960 0 29 2978 3056 -159 3600 161 3120 -49 3632 51 234 256 98 8 4000 8 'busySeconds' 4400 8 'softBreakButton' 3152 8 'hardBreakButton' 592 8 'code' 0 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 1998164183 2226 202 208 98 3 2290 2320 98 2 2354 6239 21 2354 801 601 416 2290 3488 98 1 8 'Executing GemStone/Smalltalk Code...' 416 2290 8 #updateMenuBar 1936 416 2898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 47 12 0 0 10 0 0 0 191 13 0 0 54 1 0 0] 98 5 592 3152 4400 3664 4000 2960 0 27 )! !
!WaitOnGemStoneDialog class categoriesFor: #gciSession:message:havingWaited:!public! !
!WaitOnGemStoneDialog class categoriesFor: #icon!public! !
!WaitOnGemStoneDialog class categoriesFor: #resource_Default_view!public!resources-views! !

JadeErrorShell guid: (GUID fromString: '{3C339B99-4380-48FD-94FA-1B7BFEEF120C}')!
JadeErrorShell comment: ''!
!JadeErrorShell categoriesForClass!Unclassified! !
!JadeErrorShell methodsFor!

createComponents

	super createComponents.
	messagePresenter 	:= self add: TextPresenter		new name: 'message'.
	stackPresenter 		:= self add: TextPresenter		new name: 'stack'.
	textEditPresenter	:= self add: TextPresenter		new name: 'textEdit'.
!

createSchematicWiring

	super createSchematicWiring.
	textEditPresenter 		when: #valueChanged send: #textEntry to: self.
!

doCopy

	stackPresenter view 
		selectAll;
		copySelection.
!

doDebug

	self return: #debug.
!

doResume

	self return: #resume.
!

doTerminate

	self return: #terminate.
!

onViewOpened

	| gsError gsProcess message detail stack stream |
	gsError := self model value.
	super onViewOpened.
	Sound warningBeep.
	gsProcess := gsError gsProcess.
	message := gsError errorReport message.
	stack := gsProcess stack.
	isResumable := gsError isResumableInGem.
	detail := gsError errorReport args at: 3.
	detail := (detail isKindOf: String) 
		ifTrue: [self halt. "session executeString: (session oopFishing: detail)"] 
		ifFalse: [nil].

	detail isNilOrEmpty ifTrue: [detail := message].
	message := message notEmpty
		ifTrue: [(message subStrings: Character lf) first]
		ifFalse: [''].
	message := message copyFrom: 1 to: (message size min: 100).
	self caption: message.
	messagePresenter value: detail replaceLfWithCrLf.
	stream := WriteStream on: String new.
	stack do: [:each | stream nextPutAll: each; cr].
	stackPresenter value: stream contents.
	self model: #'terminate'.		"If window is closed without any buttons pressed!!"!

queryCommand: query

	(query commandSymbol = #'doResume') ifTrue: [
		query isEnabled: isResumable == true.	"I've seen a 'receiver should be boolean' error so this is added as insurance"
		^true.
	].
	^super queryCommand: query.
!

return: aSymbol

	self model: aSymbol.
	self ok.
!

textEntry

	MessageBox notify: 'Sorry, we are not yet prepared to handle this feature!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
! !
!JadeErrorShell categoriesFor: #createComponents!public! !
!JadeErrorShell categoriesFor: #createSchematicWiring!public! !
!JadeErrorShell categoriesFor: #doCopy!public! !
!JadeErrorShell categoriesFor: #doDebug!public! !
!JadeErrorShell categoriesFor: #doResume!public! !
!JadeErrorShell categoriesFor: #doTerminate!public! !
!JadeErrorShell categoriesFor: #onViewOpened!public! !
!JadeErrorShell categoriesFor: #queryCommand:!public! !
!JadeErrorShell categoriesFor: #return:!public! !
!JadeErrorShell categoriesFor: #textEntry!public! !

!JadeErrorShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  801 601 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 10 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 624 0 482 512 0 7 0 0 0 624 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 2 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 624 98 2 8 1144066500 1025 784 0 482 512 0 7 0 0 0 784 0 8 4294903099 852486 ##(Smalltalk.NullConverter)  0 0 11 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 121 530 785 333 784 994 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 784 994 8 #isTextModified: 98 1 32 784 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 60 0 0 0 136 1 0 0 226 0 0 0] 98 0 530 193 193 0 27 7 16 234 256 98 4 410 800 98 16 0 624 98 2 8 1143017796 1025 1328 0 482 512 0 7 0 0 0 1328 0 8 4294903099 898 0 0 11 930 202 208 98 3 994 1024 98 2 530 1 1 530 785 111 1328 994 1104 98 1 1138 3 1 3 1328 994 1184 98 1 32 1328 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 136 1 0 0 55 0 0 0] 98 0 1280 0 27 8 'message' 784 8 'stack' 0 930 202 208 98 1 994 1024 98 2 530 1 1 530 785 453 624 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 136 1 0 0 226 0 0 0] 98 3 1328 410 8 ##(Smalltalk.Splitter)  98 12 0 624 98 2 8 1140850688 1 1872 0 482 512 0 519 0 0 0 1872 930 202 208 98 1 994 1024 98 2 530 1 111 530 785 11 1872 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 55 0 0 0 136 1 0 0 60 0 0 0] 98 0 1280 0 27 784 1280 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 2170 2192 8 #fixedParentRight 1 2170 2192 8 #fixedParentTop 1 2170 2192 8 #fixedParentBottom -79 410 8 ##(Smalltalk.PushButton)  98 17 0 416 98 2 8 1140924416 1 2320 0 482 512 0 7 0 0 0 2320 0 8 4294902797 1180998 4 ##(Smalltalk.CommandDescription)  8 #doTerminate 8 '&Terminate' 1193 1 0 0 32 930 202 208 98 3 994 1024 98 2 530 411 463 530 171 61 2320 994 8 #isEnabled: 98 1 32 2320 994 8 #text: 98 1 8 '&Terminate' 2320 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 205 0 0 0 231 0 0 0 34 1 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 411 2170 2192 8 #fixedViewLeft 171 2288 -69 2170 2192 8 #fixedViewTop 61 410 2336 98 17 0 416 98 2 8 1140924416 1 2848 0 482 512 0 7 0 0 0 2848 0 8 4294902797 2434 8 #doDebug 8 '&Debug' 1161 1 0 0 16 930 202 208 98 3 994 1024 98 2 530 13 463 530 171 61 2848 994 2624 98 1 32 2848 994 2672 98 1 8 '&Debug' 2848 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 6 0 0 0 231 0 0 0 91 0 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 13 2784 171 2288 -69 2816 61 410 2336 98 17 0 416 98 2 8 1140924416 1 3248 0 482 512 0 7 0 0 0 3248 0 8 4294902797 2434 8 #doResume 8 '&Resume' 1189 1 0 0 32 930 202 208 98 3 994 1024 98 2 530 211 463 530 171 61 3248 994 2624 98 1 32 3248 994 2672 98 1 8 '&Resume' 3248 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 231 0 0 0 190 0 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 211 2784 171 2288 -69 2816 61 410 2336 98 17 0 416 98 2 8 1140924416 1 3648 0 482 512 0 7 0 0 0 3648 0 8 4294902797 2434 8 #doCopy 8 '&Copy' 1159 1 0 0 32 930 202 208 98 3 994 1024 98 2 530 611 463 530 161 61 3648 994 2624 98 1 32 3648 994 2672 98 1 8 '&Copy' 3648 1218 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 231 0 0 0 129 1 0 0 5 1 0 0] 98 0 1280 0 27 2130 2176 611 2784 161 2288 -69 2816 61 234 256 98 0 590342 ##(Smalltalk.Rectangle)  530 1 1 530 1 1 0 0 0 0 13627 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 32 8 2010572111 930 202 208 98 2 994 1024 98 2 530 2559 21 530 801 601 416 994 8 #updateMenuBar 4064 416 1218 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 5 2848 3248 2320 3648 624 1280 0 27 )! !
!JadeErrorShell class categoriesFor: #resource_Default_view!public!resources-views! !

JadeCodeBrowser guid: (GUID fromString: '{9FC19CAC-6913-4452-814E-19335EA63B9D}')!
JadeCodeBrowser comment: ''!
!JadeCodeBrowser categoriesForClass!Unclassified! !
!JadeCodeBrowser methodsFor!

browseMethodImplementors

	self browseMethodImplementorsOf: self currentMethodSelector.
!

browseMethodImplementorsOf

	| selector gsCode result list |
	(selector := Prompter prompt: 'Enter selector:') isNil ifTrue: [^self].
	selector := selector reject: [:each | each = Character space].
	(selector includes: $*) ifFalse: [
		self browseMethodImplementorsOf: selector.
		^self.
	].
	list := (selector subStrings: $*) asOrderedCollection collect: [:each | each asUppercase].
	list size - 1 to: 1 do: [:i | list add: $* afterIndex: i].
	selector last = $* ifTrue: [list addLast: $*].
	gsCode := '| stream list classOrganizer |
stream := WriteStream on: String new.
list := ((AllUsers userWithId: #SymbolUser ifAbsent: [AllUsers userWithId: #DataCurator]) resolveSymbol: #AllSymbols) value select: [:each |each asUppercase matchPattern: ' , list asArray printString , '].
classOrganizer := ClassOrganizer new.
list := list select: [:each | (classOrganizer implementorsOf: each) notEmpty].
list := list asSortedCollection.
list do: [:each | stream nextPutAll: each; nextPut: Character lf; yourself].
stream contents'.
	Cursor wait showWhile: [
		(result := gciSession executeString: gsCode) isNil ifTrue: [^self].
		list := result subStrings: Character lf.
	].
	(selector := ChoicePrompter choices: list) isNil ifTrue: [^self].
	self browseMethodImplementorsOf: selector.
!

browseMethodImplementorsOf: aString

	self 
		browseMethodsWhere: 'implementorsOf: #' , aString printString
		searchFor: nil
		caption: 'implementing #' , aString printString.
!

browseMethodSenders

	self browseMethodSendersOf: self currentMethodSelector.
!

browseMethodSendersOf

	| selector gsCode result list |
	(selector := Prompter prompt: 'Enter selector:') isNil ifTrue: [^self].
	selector := selector reject: [:each | each = Character space].
	(selector includes: $*) ifFalse: [
		self browseMethodSendersOf: selector.
		^self.
	].
	list := (selector subStrings: $*) asOrderedCollection collect: [:each | each asUppercase].
	list size - 1 to: 1 do: [:i | list add: $* afterIndex: i].
	selector last = $* ifTrue: [list addLast: $*].
	gsCode := '| stream list classOrganizer |
stream := WriteStream on: String new.
list := AllSymbols select: [:each |each asUppercase matchPattern: ' , list asArray printString , '].
classOrganizer := ClassOrganizer new.
list := list select: [:each | (classOrganizer sendersOf: each) notEmpty].
list := list asSortedCollection.
list do: [:each | stream nextPutAll: each; nextPut: Character lf; yourself].
stream contents'.
	Cursor wait showWhile: [
		(result := gciSession executeString: gsCode) isNil ifTrue: [^self].
		list := result subStrings: Character lf.
	].
	(selector := ChoicePrompter choices: list) isNil ifTrue: [^self].
	self browseMethodSendersOf: selector.
!

browseMethodSendersOf: aString

	self 
		browseMethodsWhere: 'sendersOf: #' , aString printString
		searchFor: aString
		caption: 'sending #' , aString printString.
!

browseMethodsWhere: codeString searchFor: searchString caption: captionString

	| gsCode result list title |
	gsCode := '| stream list |
stream := WriteStream on: String new.
list := ClassOrganizer new ' , codeString , '.
((list isKindOf: Array) and: [list size = 2 and: [list first isKindOf: Collection]]) ifTrue: [
	list := list first.
].
1 to: list size do: [:i | | gsMethod gsClass dictName |
	gsMethod := list at: i.
	gsClass := gsMethod inClass.
	dictName := (System myUserProfile symbolResolutionOf: gsClass thisClass name) subStrings at: 2.
	stream 
		nextPutAll: dictName; tab;
		nextPutAll: gsClass name; tab;
		nextPutAll: gsMethod selector; tab;
		nextPut: Character lf;
		yourself.
].
stream contents'.
	Cursor wait showWhile: [
		(result := gciSession executeString: gsCode) isNil ifTrue: [^self].
		list := result subStrings: Character lf.
		list := list collect: [:each | each subStrings: Character tab].
		list := list collect: [:each | 
			GsMethod new
				symbolDictionaryName: (each at: 1);
				gsBehavior: (each at: 2);
				name: (each at: 3);
				yourself.
		].
	].
	title := captionString notNil
		ifTrue: [captionString]
		ifFalse: ['Jade Method Browser on ' , (searchString notNil
			ifTrue: [searchString]
			ifFalse: [codeString])].
	(JadeMethodList showOn: gciSession)
		methodList: list;
		captionString: title;
		searchFor: searchString;
		yourself.

!

browseMethodsWithSubstring

	| string |
	(string := Prompter prompt: 'Enter substring:') isNil ifTrue: [^self].
	self 
		browseMethodsWhere: 'substringSearch: ' , string printString
		searchFor: string
		caption: 'containing ' , string printString.
!

browseReferencesTo: anObject

	self 
		browseMethodsWhere: 'referencesToObject: ' , anObject codeForObject
		searchFor: anObject name
		caption: 'referencing ' , anObject name.
!

codePresenterIsMethod

	^true!

currentMethodSelector

	self subclassResponsibility.
!

methodSelector

	| list stream |
	list := documentPresenter value subStrings.
	list first last = $: ifFalse: [^list first].
	stream := WriteStream on: String new.
	list do: [:each | 
		each last = $:
			ifTrue: [stream nextPutAll: each]
			ifFalse: [^stream contents].
	].
	MessageBox notify: 'Sorry, we are not yet prepared to handle this feature!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
! !
!JadeCodeBrowser categoriesFor: #browseMethodImplementors!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodImplementorsOf!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodImplementorsOf:!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodSenders!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodSendersOf!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodSendersOf:!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodsWhere:searchFor:caption:!browse!public! !
!JadeCodeBrowser categoriesFor: #browseMethodsWithSubstring!browse!public! !
!JadeCodeBrowser categoriesFor: #browseReferencesTo:!browse!public! !
!JadeCodeBrowser categoriesFor: #codePresenterIsMethod!public! !
!JadeCodeBrowser categoriesFor: #currentMethodSelector!browse!public! !
!JadeCodeBrowser categoriesFor: #methodSelector!public! !

JadeDebugger guid: (GUID fromString: '{54A4DDBF-2101-4884-8568-46D6D2790987}')!
JadeDebugger comment: ''!
!JadeDebugger categoriesForClass!Unclassified! !
!JadeDebugger methodsFor!

answer
		"It is likely that the debugger was opened by some UI command that changed the cursor to a wait cursor.
		Because we are starting a new main UI process, and because our process can be terminated, we can be
		left in a state in which the cursor is not set back to its original value.
		We mitigate that problem by changing it to the default."

	Cursor current: nil.
	SessionManager inputState loopWhile: [answer isNil].
	^answer.
!

clearUI

	frameListPresenter list: #().
	variableListPresenter list: #().
	variableDataPresenter value: ''.
	documentPresenter value: ''.
!

contextObject

	^gciSession oopTypeWithOop: frame vars first key key asNumber.
!

createComponents

	super createComponents.
	errorMessagePresenter	:= self add: TextPresenter		new name: 'errorMessage'.
	frameListPresenter 		:= self add: ListPresenter		new name: 'frameList'.
	processListPresenter 	:= self add: ListPresenter		new name: 'processList'.
	variableDataPresenter 	:= self add: TextPresenter		new name: 'variableData'.
	variableListPresenter		:= self add: ListPresenter		new name: 'variableList'.
!

createSchematicWiring

	super createSchematicWiring.
	frameListPresenter 		when: #selectionChanged 	send: #selectedFrame 	to: self.
	processListPresenter 	when: #selectionChanged 	send: #selectedProcess 	to: self.
	variableListPresenter		when: #selectionChanged	send: #selectedVariable	to: self.
	variableListPresenter		when: #actionPerformed	send: #inspectVariable	to: self.
!

getProcessList

	| string lines |
	string := gciSession serverPerform: #'processes'.
	lines := (string subStrings: Character lf) asOrderedCollection.
	processList := lines removeFirst; collect: [:each | 
		| fields oopType oopValue type |
		fields := each subStrings: Character tab.
		oopValue := (fields at: 2) asNumber.
		oopType := gciSession oopTypeWithOop: oopValue.
		type := fields at: 9.
		(GsProcess session: gciSession oop: oopType)
			type: type;
			yourself
	].
	processList addFirst: gsProcess.
	gsProcess type: 'active'.!

initializeProcess: aProcess message: aString terminateOnClose: aBoolean

	gsProcess := aProcess.
	errorMessagePresenter value: aString.
	terminateOnClose := aBoolean.
	self getProcessList.
	self class debuggers
		at: gsProcess oopType asInteger
		put: self.
	self update.
!

inspectVariable

	| object |
	object := gciSession oopTypeWithOop: variableListPresenter selection key key asNumber.
	JadeInspector showOn: gciSession -> object.
!

onViewClosed

	gsProcess := processList first.
	self class debuggers removeKey: gsProcess oopType asInteger.
	(terminateOnClose and: [answer isNil]) ifTrue: [
		gsProcess terminate.
		self error: 'We should never get here!!'.
	].
!

queryCommand: query

	(#(#'resumeProcess' #'runToCursor' #'stepInto' #'stopOut' #'stepOver') includes: query commandSymbol) ifTrue: [
		query isEnabled: (processList notNil and: [processList notEmpty and: [processList first == gsProcess]]).
		^true.
	].
	(#(#'terminateProcess') includes: query commandSymbol) ifTrue: [
		query isEnabled: (processList notNil and: [processList notEmpty and: [processList first ~~ gsProcess]]).
		^true.
	].
	^false.
!

resumeProcess

	answer := #resume.
	self view close.
!

runToCursor

	MessageBox notify: 'Sorry, this feature is not yet implemented!!'.
!

saveMethod

	| result |
	result := gciSession
		serverPerform: #'compile:frame:process:' 
		with: self getDocumentData
		with: frameListPresenter selectionByIndex 
		with: gsProcess.
	(result isKindOf: Boolean) ifTrue: [
		result ifTrue: [
			gsProcess trimStackToLevel: frameListPresenter selectionByIndex.
			^self update.
		] ifFalse: [
			MessageBox notify: 'New method has different selector so will not be on stack!!'.
			^self selectedFrame.
		].
	].
	MessageBox notify: result.
!

selectedFrame 

	| index breaks offset source source1 source2 word wordLength range lineNumber |
	(index := frameListPresenter selectionByIndex) = 0 ifTrue: [^self].
	(frame := gsProcess frameForLevel: index) isNil ifTrue: [^self].
	source := frame source.
	breaks := frame breaks collect: [:each | frame offsets at: each].
	breaks := breaks collect: [:each | ((source copyFrom: 1 to: each) select: [:char | char = Character lf]) size + 1].
	breaks := breaks asSet asSortedCollection asArray.
	variableListPresenter list: frame vars.
	offset := (frame offsets at: (frame stepPoint min: frame offsets size)) abs.
	source size < offset ifTrue: [offset := source size].
	source1 := (source copyFrom: 1 to: offset) "replaceLfWithCrLf".
	source2 := (source copyFrom: offset + 1 to: source size) "replaceLfWithCrLf".
	source := source1 , source2.
	offset := source1 size.
	documentPresenter 
		value: source;
		isModified: false;
		yourself.
	word := (ReadStream on: source2) nextWord.
	wordLength := (source2 indexOfSubCollection: word) + word size - 1.
	documentPresenter view selectionRange: (offset to: offset + wordLength).
	(range := documentPresenter view selectionRange) isEmpty ifTrue: [^self].
	lineNumber := documentPresenter view lineFromPosition: range first.
	lineNumber := lineNumber - 4 max: 1.
	documentPresenter view lineScroll: lineNumber.
	breaks do: [:each | 
		documentPresenter view 
			addMarkerType: #'breakpoint' 
			at: each.
	].
!

selectedProcess

	| stack |
	self clearUI.
	(gsProcess := processListPresenter selectionOrNil) ifNil: [^self].
	frameListPresenter list: (stack := gsProcess stack).
	stack notEmpty ifTrue: [
		frameListPresenter selectionByIndex: self stackInitialSelection.
	].
!

selectedVariable

	| data |
	variableDataPresenter value: nil.
	variableListPresenter hasSelection ifFalse: [^self].
	data := variableListPresenter selection value.
	variableDataPresenter value: data.
!

showNextStatement

	self 
		update;
		selectedFrame;
		yourself.

!

stackInitialSelection

	| list |
	list := gsProcess stack.
	self stackInitialSelectionData do: [:each | 
		(each first <= list size and: [0 < ((list at: each first - 1) indexOfSubCollection: each last)]) ifTrue: [^each first].
	].
	^1.
!

stackInitialSelectionData
	"Line number to select if string included on previous line (need not be exact match)"

	^#(
		#(2 '(Object)>>#pause @2 line 8')
		#(2 '(Object)>>#error: @1 line 6')
		#(6 '(Object)>>#halt @1 line 6')
		#(10 '(Object)>>#halt @1 line 6')
		#(10 '(Object)>>#error: @3 line 7')
		#(12 '(Object)>>#halt @1 line 6')
		#(21 '(Object)>>#error: @1 line 6')
		#(4 '(Object) >> halt (envId 0) @2 line 5')
		#(4 '(Object) >> error: (envId 0) @6 ine 7')

		#(2 '(Object)>>#_doesNotUnderstand: @1 line 6')
		#(6 'Object >> _doesNotUnderstand:args:envId:reason: (envId 0) @7 line 12')
		#(5 'Object >> _doesNotUnderstand:')

		#(4 '(Object) >> halt @2 line 5')
		#(4 '(Object) >> error: @6 line 7')
		#(4 'Object >> halt (envId 0) @2 line 5')
		#(3 'Object >> error: (envId 0) @6 line 7')

		#(2 '(TestCase)>>#assert: @1 line 1')
		#(3 '(TestCase)>>#should:raise: @10 line 9')
		#(3 '(TestCase)>>#shouldnt:raise: @4 line 2')
		#(3 '(TestCase)>>#deny: @2 line 3')
		#(2 'GSTestCase >> assert: (envId 0) @1 line 1')
		#(5 'TestAsserter >> assert: (envId 0) @5 line 4')
	).
!

step: anInteger

	| result gciError |
	result := gsProcess step: anInteger.
	true ifTrue: [^self update].
	gsProcess oopType = result ifTrue: [^self update].
	result isOopIllegal ifFalse: [
		answer := result.
		^self view close.
	].
	((gciError := GsError forSession: gciSession) isStackBreakpointInSession: gciSession) ifFalse: [
		GsError signalGCI: gciSession.
		Keyboard default isShiftDown ifTrue: [self halt].
	].
	gsProcess := gciError gsProcess.
	errorMessagePresenter value: gciError messageText.
	self update.
!

stepInto

	self step: 0.
!

stepOut

	self step: frameListPresenter selectionByIndex + 1.
!

stepOver

	self step: frameListPresenter selectionByIndex.
!

terminateProcess

	(MessageBox confirm: 'Terminate process?') ifFalse: [^self].
	self clearUI.
	(processList size == 1 or: [gsProcess == processList first]) ifTrue: [self view close. ^self].
	processList := processList copyWithout: gsProcess.
	[
		gsProcess terminate.
	] on: TerminateProcess do: [:ex | 
		ex return: nil.
	].
	gsProcess := processList first.
	self update.!

update

	| stack |
	((stack := gsProcess stack) isEmpty or: [stack = #('' '')]) ifTrue: [
		MessageBox warning: 'We appear to have finished this process!!'. 
		self view close. 
		^self.
	].
	(2 <= processList size and: [processListPresenter selectionOrNil ~~ processList first]) ifTrue: [
		processListPresenter 
			list: processList;	"This triggers a selection changed message that clears the current selection"
			selection: processList first.
	] ifFalse: [
		frameListPresenter 
			list: gsProcess stack;
			selectionByIndex: self stackInitialSelection;
			yourself.
	].!

updateCaption

	self caption: (gciSession titleBarFor: 'Debugger').
! !
!JadeDebugger categoriesFor: #answer!public! !
!JadeDebugger categoriesFor: #clearUI!public! !
!JadeDebugger categoriesFor: #contextObject!public! !
!JadeDebugger categoriesFor: #createComponents!public! !
!JadeDebugger categoriesFor: #createSchematicWiring!public! !
!JadeDebugger categoriesFor: #getProcessList!public! !
!JadeDebugger categoriesFor: #initializeProcess:message:terminateOnClose:!public! !
!JadeDebugger categoriesFor: #inspectVariable!public! !
!JadeDebugger categoriesFor: #onViewClosed!public! !
!JadeDebugger categoriesFor: #queryCommand:!public! !
!JadeDebugger categoriesFor: #resumeProcess!public! !
!JadeDebugger categoriesFor: #runToCursor!public! !
!JadeDebugger categoriesFor: #saveMethod!public! !
!JadeDebugger categoriesFor: #selectedFrame!public! !
!JadeDebugger categoriesFor: #selectedProcess!public! !
!JadeDebugger categoriesFor: #selectedVariable!public! !
!JadeDebugger categoriesFor: #showNextStatement!public! !
!JadeDebugger categoriesFor: #stackInitialSelection!public! !
!JadeDebugger categoriesFor: #stackInitialSelectionData!public! !
!JadeDebugger categoriesFor: #step:!public! !
!JadeDebugger categoriesFor: #stepInto!public! !
!JadeDebugger categoriesFor: #stepOut!public! !
!JadeDebugger categoriesFor: #stepOver!public! !
!JadeDebugger categoriesFor: #terminateProcess!public! !
!JadeDebugger categoriesFor: #update!public! !
!JadeDebugger categoriesFor: #updateCaption!public! !

!JadeDebugger class methodsFor!

debuggerFor: anInteger		"aGsProcess asOop"

	^self debuggers
		at: anInteger
		ifAbsent: [nil].
!

debuggers

	debuggers isNil ifTrue: [debuggers := Dictionary new].
	^debuggers.
!

openDebuggerOnException: gciError 

	^self
		openOn: gciError gsProcess
		message: gciError messageText
		terminateOnClose: true.
!

openOn: gsProcess message: aString terminateOnClose: aBoolean

	^(self showOn: gsProcess gciSession)
		initializeProcess: gsProcess message: aString terminateOnClose: aBoolean;
		answer.
!

reportError: gsError 

	| debugger answer | 
	(debugger := self debuggerFor: gsError processOop) notNil ifTrue: [
		debugger update.
		Processor activeProcess terminate.
		self error: 'We should never get here!!'.
	].
	answer := JadeErrorShell showModalOn: gsError.
	answer = #'terminate' ifTrue: [
		gsError terminateProcess.
		self error: 'We should never get here!!'.
	].
	answer = #resume ifTrue: [
		^#resume.
	].
	answer = #debug ifTrue: [
		^self openDebuggerOnException: gsError.
	].
	self error: 'Unexpected response of ' , answer printString , '!!'.
!

resetDebuggers

	debuggers := nil.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1601 1201 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 6 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140850816 1 624 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 0 0 7 0 0 0 624 0 8 4294902457 852486 ##(Smalltalk.NullConverter)  0 0 3 983302 ##(Smalltalk.MessageSequence)  202 208 98 5 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 291 7 530 1275 39 624 882 8 #text: 98 1 8 'Static Text' 624 882 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 624 882 8 #isTextModified: 98 1 32 624 882 8 #setMarginWidths: 98 1 98 2 7 7 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 145 0 0 0 3 0 0 0 14 3 0 0 22 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 291 1346 8 #fixedParentRight -3 1346 8 #fixedParentTop 7 1346 8 #fixedViewTop 39 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 1488 0 0 0 7 0 0 0 1488 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 98 2 410 8 ##(Smalltalk.ReferenceView)  98 14 0 1488 98 2 8 1140850688 131073 1664 0 482 8 4278190080 0 7 0 0 0 1664 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.JadeCodePresenter)  8 #resource_Default_view 0 818 202 208 98 1 882 912 98 2 530 1 527 530 1569 509 1664 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 7 1 0 0 16 3 0 0 5 2 0 0] 1616 1296 0 27 8 'codePane' 0 818 202 208 98 1 882 912 98 2 530 1 51 530 1569 1035 1488 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 16 3 0 0 30 2 0 0] 98 3 410 1504 98 15 0 1488 98 2 8 1140850688 131073 2160 0 482 512 0 7 0 0 0 2160 1570 234 240 98 4 410 1504 98 15 0 2160 98 2 8 1140850688 131073 2288 0 0 0 7 0 0 0 2288 562 234 240 98 6 410 8 ##(Smalltalk.PushButton)  98 20 0 2288 98 2 8 1140924416 1 2400 0 0 0 7 0 0 0 2400 0 8 4294902445 1180998 4 ##(Smalltalk.CommandDescription)  8 #terminateProcess 8 'Terminate' 1 1 0 0 32 0 0 0 818 202 208 98 2 882 912 98 2 530 715 1 530 141 51 2400 882 992 98 1 8 'Terminate' 2400 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 101 1 0 0 0 0 0 0 171 1 0 0 25 0 0 0] 98 0 1296 0 29 1314 1346 8 #fixedPreviousRight 1 1392 1 1424 1 1456 51 410 8 ##(Smalltalk.ListBox)  98 17 0 2288 98 2 8 1144062209 1025 2816 590662 2 ##(Smalltalk.ListModel)  202 208 1616 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 512 0 7 0 0 0 2816 0 8 4294902573 8 ##(Smalltalk.BasicListAbstract)  1616 32 818 202 208 98 2 882 912 98 2 530 1 51 530 855 459 2816 882 8 #horizontalExtent: 98 1 1 2816 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 171 1 0 0 254 0 0 0] 98 0 1296 0 27 1314 1360 1 1392 1 1346 8 #fixedPreviousBottom 1 1346 8 #fixedParentBottom 1 410 8 ##(Smalltalk.ComboBox)  98 17 0 2288 98 2 8 1412498947 1025 3312 2898 202 208 1616 0 2960 482 8 4278190080 0 7 0 0 0 3312 0 8 4294902459 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each printString]' 8 #[30 105 226 0 106] 8 #printString 3488 7 257 0 1616 401 818 202 208 98 1 882 912 98 2 530 1 1 530 715 47 3312 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 101 1 0 0 23 0 0 0] 98 0 1296 0 27 1314 1360 1 1392 -139 1424 1 1456 47 234 256 98 4 2816 8 'frameList' 3312 8 'processList' 0 818 202 208 98 1 882 912 98 2 530 1 1 530 855 509 2288 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 171 1 0 0 254 0 0 0] 98 3 3312 2400 2816 1296 0 27 3 410 1504 98 15 0 2160 98 2 8 1140850688 131073 4016 0 482 512 0 7 0 0 0 4016 1570 234 240 98 2 410 8 ##(Smalltalk.ListView)  98 30 0 4016 98 2 8 1140920397 1025 4144 2898 202 208 1616 0 2960 482 512 0 7 0 0 0 4144 0 8 4294902395 3008 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 3 920646 5 ##(Smalltalk.ListViewColumn)  8 'Variable' 201 8 #left 3008 8 ##(Smalltalk.SortedCollection)  3474 0 0 3506 2 1 3472 8 'doIt' 8 '[:each | each key value]' 8 #[30 105 226 0 142 106] 8 #key 4432 7 257 0 0 4144 0 1 0 0 4354 8 'Value' 317 4400 3008 4416 3474 0 459302 ##(Smalltalk.Context)  1 1 0 0 3506 0 9 3536 8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  1616 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 4144 0 3 0 0 4354 8 'OOP' 181 8 #right 459270 ##(Smalltalk.Message)  8 #displayString 98 0 4786 8 #<= 4832 3474 0 0 3506 3 1 3536 8 'doIt' 8 '[:each | each key key asNumber]' 8 #[31 105 226 0 158 159 106] 4512 8 #asNumber 4880 7 257 0 0 4144 0 1 0 0 8 #report 1616 0 131169 0 0 818 202 208 98 2 882 912 98 2 530 1 1 530 697 327 4144 882 992 98 1 8 'Variable' 4144 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 92 1 0 0 163 0 0 0] 98 0 1296 0 27 5 16 234 256 98 4 4144 8 'variableList' 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 4016 98 2 8 1143017796 1025 5248 0 482 512 0 7 0 0 0 5248 0 8 4294902457 786 0 0 9 818 202 208 98 3 882 912 98 2 530 1 345 530 697 165 5248 882 1056 98 1 1090 3 1 3 5248 882 1136 98 1 32 5248 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 172 0 0 0 92 1 0 0 254 0 0 0] 98 0 1296 0 27 8 'variableData' 0 818 202 208 98 1 882 912 98 2 530 873 1 530 697 509 4016 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 1 0 0 0 0 0 0 16 3 0 0 254 0 0 0] 98 3 4144 410 8 ##(Smalltalk.Splitter)  98 12 0 4016 98 2 8 1140850688 1 5792 0 482 512 0 519 0 0 0 5792 818 202 208 98 1 882 912 98 2 530 1 327 530 697 19 5792 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 163 0 0 0 92 1 0 0 172 0 0 0] 98 0 1296 0 27 5248 1296 0 27 524806 ##(Smalltalk.Fraction)  253 311 32 234 256 1616 0 818 202 208 98 1 882 912 98 2 530 1 1 530 1569 509 2160 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 254 0 0 0] 98 3 2288 410 5808 98 12 0 2160 98 2 8 1140850688 1 6256 0 482 512 0 519 0 0 0 6256 818 202 208 98 1 882 912 98 2 530 855 1 530 19 509 6256 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 171 1 0 0 0 0 0 0 180 1 0 0 254 0 0 0] 98 0 1296 0 27 4016 1296 0 27 410 5808 98 12 0 1488 98 2 8 1140850688 1 6496 0 482 512 0 519 0 0 0 6496 818 202 208 98 1 882 912 98 2 530 1 509 530 1569 19 6496 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 254 0 0 0 16 3 0 0 7 1 0 0] 98 0 1296 0 27 1664 1296 0 27 1314 1360 1 1392 1 1424 51 3280 1 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1409289036 131137 6752 0 482 8 4278190080 0 519 0 0 0 6752 482 6848 8 4294902319 234 256 1616 234 256 98 12 35619 1115910 ##(Smalltalk.ToolbarIconButton)  35619 0 6752 1 2498 8 #runToCursor 8 'Run to Cursor' 1 1 263494 3 ##(Smalltalk.Icon)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'RunToCursor.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 395334 3 ##(Smalltalk.Bitmap)  0 16 0 0 0 0 3 530 33 33 1 35621 6946 35621 0 6752 1 2498 8 #resumeProcess 8 'Go' 1 1 7026 0 16 7072 8 'Run.ico' 7120 7154 0 16 0 0 0 0 3 530 33 33 1 35611 6946 35611 0 6752 1 2498 8 #showNextStatement 8 'Show Next Statement' 1 1 7026 0 16 7072 8 'ShowNextStatement.ico' 7120 7154 0 16 0 0 0 0 3 530 33 33 1 35613 6946 35613 0 6752 1 2498 8 #stepInto 8 'Step Into' 1 1 7026 0 16 7072 8 'StepInto.ico' 7120 7154 0 16 0 0 0 0 3 530 33 33 1 35615 6946 35615 0 6752 1 2498 8 #stepOver 8 'Step Over' 1 1 7026 0 16 7072 8 'StepOver.ico' 7120 7154 0 16 0 0 0 0 3 530 33 33 1 35617 6946 35617 0 6752 1 2498 8 #stepOut 8 'Step Out' 1 1 7026 0 16 7072 8 'StepOut.ico' 7120 7154 0 16 0 0 0 0 3 530 33 33 1 98 7 7328 7456 7584 7712 6960 7200 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 6752 3 0 1 234 240 98 12 7680 5 7424 1 7552 3 7808 7 7168 9 7296 11 0 1 0 530 33 33 530 45 45 0 0 818 202 208 98 2 882 912 98 2 530 1 1 530 1569 51 6752 882 8 #updateSize 1616 6752 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 25 0 0 0] 98 0 1296 0 27 1314 1360 1 1392 1 1424 1 1456 51 234 256 98 2 624 8 'errorMessage' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 3 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 2498 8 #saveMethod 8 '&Save' 9383 1 0 0 0 8 '&File' 0 134217729 0 0 35605 0 0 8258 0 16 98 0 8 '&Edit' 0 134217729 0 0 35607 0 0 8258 0 16 98 0 8 '&Debug' 0 134217729 0 0 35609 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 7026 0 16 7072 8 'icons\GS32x32.ico' 0 7026 0 16 7072 8 'icons\GS16x16.ico' 0 0 0 1 0 0 818 202 208 98 3 882 912 98 2 530 2879 21 530 1601 1201 416 882 992 98 1 8 'Jade Debugger' 416 882 8 #updateMenuBar 1616 416 1234 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 191 8 0 0 98 2 0 0] 98 3 624 6752 1488 1296 0 27 )! !
!JadeDebugger class categoriesFor: #debuggerFor:!public! !
!JadeDebugger class categoriesFor: #debuggers!public! !
!JadeDebugger class categoriesFor: #openDebuggerOnException:!public! !
!JadeDebugger class categoriesFor: #openOn:message:terminateOnClose:!public! !
!JadeDebugger class categoriesFor: #reportError:!public! !
!JadeDebugger class categoriesFor: #resetDebuggers!public! !
!JadeDebugger class categoriesFor: #resource_Default_view!public!resources-views! !

JadeMethodList guid: (GUID fromString: '{82986EA3-B8FB-45C8-A0BA-7A389D8F9DFC}')!
JadeMethodList comment: ''!
!JadeMethodList categoriesForClass!Unclassified! !
!JadeMethodList methodsFor!

captionString: aString

	captionString := 'Method(s) ' , aString.
	self updateCaption.
!

countOf: subString in: string

	| index next count |
	count := 0.
	index := 1.
	[
		next := string 
			indexOfSubCollection: subString 
			startingAt: index.
		next > 0.
	] whileTrue: [
		index := next + 1.
		((next = 1 or: [(string at: next - 1) isAlphaNumeric not]) and: [
		next + subString size > string size or: [(string at: next + subString size) isAlphaNumeric not]]) ifTrue: [
				count := count + 1.
		].
	].
	^count.
!

createComponents

	super createComponents.
	methodListPresenter 	:= self add: ListPresenter		new name: 'methodList'.
!

createSchematicWiring

	super createSchematicWiring.
	methodListPresenter 	
		when: #selectionChanged 	send: #selectedInMethodList 	to: self;
		when: #leftButtonPressed:	send: #doSearchFork					to: self;
		yourself.
!

doSearch

	| myView foundString lineNumber string |
	searchString isNil ifTrue: [^self].
	myView := documentPresenter view.
	string := searchString.
	(string includes: $:) ifTrue: [string := (string subStrings: $:) first , ':'].
	foundString := myView 
		findNextWrappedEx: string 
		down: true 
		wholeWord: true 
		matchCase: true.
	foundString ifFalse: [
		foundString := myView 
			findNextWrappedEx: string 
			down: true 
			wholeWord: false  
			matchCase: true.
	].
	foundString ifFalse: [^self].
	lineNumber := myView lineFromPosition: myView selectionRange first.
	lineNumber := lineNumber - 4 max: 1.
	myView lineScroll: lineNumber.
	self updateCaption.
!

doSearchFork

	searchString isNil ifTrue: [^self].
	[ 
		self doSearch.
	] forkAt: Processor userBackgroundPriority.
!

filename: aString

	(JadeWorkspace showOn: gciSession)
		filename: aString;
		fileLoad.
!

methodList: aList

	methodListPresenter list: aList.
!

removeFromList

	| oldList selections newList |
	oldList := methodListPresenter list asOrderedCollection.
	selections := methodListPresenter selections.
	newList := oldList copyWithoutAll: selections.
	methodListPresenter 
		resetSelection;
		list: newList.
!

searchFor: aString

	searchString := aString.
!

selectedInMethodList 

	| method gsCode result |
	documentPresenter clear.
	self caption: captionString.
	(method := methodListPresenter selectionOrNil) isNil ifTrue: [^self].
	gsCode := method gsBehavior , ' sourceCodeAt: #' , method name printString.
	(result := gciSession executeString: gsCode) isNil ifTrue: [^self].
	documentPresenter
		value: result "replaceLfWithCrLf";
		isModified: false;
		yourself.
	self doSearch.
!

updateCaption

	| selectionRange index next beforeCount afterCount string |
	captionString notNil ifTrue: [self caption: captionString].
	(selectionRange := documentPresenter view selectionRange) isEmpty ifTrue: [^self].
	string := documentPresenter value  
		copyFrom: 1
		to: selectionRange start.
	beforeCount := self
		countOf: searchString 
		in: string.
	string := documentPresenter value  
		copyFrom: selectionRange stop
		to: documentPresenter value size.
	afterCount := self
		countOf: searchString 
		in: string.
	self caption: captionString , ' (' , (beforeCount + 1) printString , ' of ' , (beforeCount + afterCount + 1) printString , ')'.
! !
!JadeMethodList categoriesFor: #captionString:!public! !
!JadeMethodList categoriesFor: #countOf:in:!public! !
!JadeMethodList categoriesFor: #createComponents!public! !
!JadeMethodList categoriesFor: #createSchematicWiring!public! !
!JadeMethodList categoriesFor: #doSearch!public! !
!JadeMethodList categoriesFor: #doSearchFork!public! !
!JadeMethodList categoriesFor: #filename:!public! !
!JadeMethodList categoriesFor: #methodList:!public! !
!JadeMethodList categoriesFor: #removeFromList!public! !
!JadeMethodList categoriesFor: #searchFor:!public! !
!JadeMethodList categoriesFor: #selectedInMethodList!public! !
!JadeMethodList categoriesFor: #updateCaption!public! !

!JadeMethodList class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 98 4 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 656 0 482 8 4278190080 0 7 0 0 0 656 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.JadeCodePresenter)  8 #resource_Default_view 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 371 530 1185 363 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 185 0 0 0 80 2 0 0 110 1 0 0] 608 530 193 193 0 27 8 'codePane' 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920393 1025 1072 590662 2 ##(Smalltalk.ListModel)  202 208 608 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 512 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #removeFromList 8 'Remove from list' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 1072 0 8 4294903261 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1210 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 3 920646 5 ##(Smalltalk.ListViewColumn)  8 'Dictionary' 241 8 #left 459270 ##(Smalltalk.Message)  8 #displayString 98 0 1634 8 #<= 1680 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 1728 8 'doIt' 8 '[:each | each symbolDictionaryName]' 8 #[30 105 226 0 106] 8 #symbolDictionaryName 1744 7 257 0 0 1072 0 1 0 0 1570 8 'Class' 301 1616 1456 8 ##(Smalltalk.SortedCollection)  1730 0 459302 ##(Smalltalk.Context)  1 1 0 0 1762 1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each gsBehavior]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  608 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #gsBehavior 17 257 0 0 1072 0 1 0 0 1570 8 'Method' 601 1616 1456 1888 1730 0 1922 1 1 0 0 1762 1 9 1968 8 'doIt' 98 2 8 '[:each | each name]' 98 1 202 2064 608 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #name 17 257 0 0 1072 0 1 0 0 8 #report 608 0 131169 0 0 834 202 208 98 3 898 928 98 2 530 1 1 530 1185 361 1072 898 8 #contextMenu: 98 1 1296 1072 898 8 #text: 98 1 8 'Dictionary' 1072 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 80 2 0 0 180 0 0 0] 98 0 1040 0 27 8 'methodList' 0 0 0 0 0 1 263494 3 ##(Smalltalk.Icon)  0 16 1210 8 ##(Smalltalk.ImageRelativeFileLocator)  1520 8 'icons\GS32x32.ico' 0 2610 0 16 2640 8 'icons\GS16x16.ico' 0 0 0 1 0 0 834 202 208 98 3 898 928 98 2 530 2799 21 530 1201 801 416 898 2496 98 1 8 'Jade Method List' 416 898 8 #updateMenuBar 608 416 994 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 5 0 0 10 0 0 0 207 7 0 0 154 1 0 0] 98 3 1072 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2960 0 482 512 0 519 0 0 0 2960 834 202 208 98 1 898 928 98 2 530 1 361 530 1185 11 2960 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 180 0 0 0 80 2 0 0 185 0 0 0] 98 0 1040 0 27 656 1040 0 27 )! !
!JadeMethodList class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

