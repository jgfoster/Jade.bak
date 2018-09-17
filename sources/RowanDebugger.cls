"Filed out from Dolphin Smalltalk 7"!

JadeDebugger subclass: #RowanDebugger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'debuggerClass semaphore'!
RowanDebugger guid: (GUID fromString: '{cae790de-8a45-49eb-813a-6cb32999cd4d}')!
RowanDebugger comment: ''!
!RowanDebugger categoriesForClass!Unclassified! !
!RowanDebugger methodsFor!

_processList

	^processList!

_terminateProcess: aGsProcess

	| string |
	string := '
| gsProcess |
gsProcess := Object _objectForOop: ' , aGsProcess oopType value printString , '.
gsProcess ifNotNil: [gsProcess terminate. (Delay forMilliseconds: 10) wait].
Processor allProcesses includes: gsProcess'.
	5 timesRepeat: [
		(gciSession executeString: string) ifFalse: [^self].
	].
	self error: 'terminate process failed'.
!

basicSelectedFrame
	| index |
	(index := frameListPresenter selectionByIndex) = 0 ifTrue: [^nil].
	index := index + frameListOffset.
	^gsProcess frameForLevel: index!

browseFrameMethod

	| method |
	method := frame gsMethod.
	(RowanSystemBrowser showOnSession: self gciSession)
		selectClass: method className asString
		selector: method selector asString.
!

browseImplementors
	| service string |
	string := frame gsMethod selector.
	service := RowanQueryService command: #implementorsOf:
				withArgs: (Array with: frame gsMethod selector).
	self issueCommand: (Array with: service).
	(JadeiteMethodListBrowser showOnSession: self gciSession) primaryPresenter
		methods: service queryResults
		selecting: string!

browseObjectClass

	| variable |
	variable := variableListPresenter selection.
	(RowanSystemBrowser showOnSession: self gciSession)
		selectClass: variable className asString
		selector: ''.
!

browseSenders
	| service string |
	string := frame gsMethod selector.
	service := RowanQueryService command: #sendersOf: withArgs: (Array with: frame gsMethod selector).
	self issueCommand: (Array with: service).
	(JadeiteMethodListBrowser showOnSession: self gciSession) primaryPresenter
		methods: service queryResults
		selecting: string!

compilationWarningFrom: methodServices

	methodServices do:[:methodService | 
		methodService compilationWarnings ifNotNil:[:warning | ^warning]].
	^nil!

createSchematicWiring
	super createSchematicWiring.
	frameListPresenter when: #actionPerformed 	send: #browseClass 			to: self.
	documentPresenter when: #textChanged 		send: #methodValueChanged 	to: self!

currentMethodSourceFrom: sourceString

	| source fromStream writeStream |
	fromStream := ReadStream on: sourceString.
	writeStream := WriteStream on: String new.
	[
		fromStream atEnd not.
	] whileTrue: [
		| char |
		(char := fromStream next) == Character cr ifTrue: [
			fromStream peek ~~ Character lf ifTrue: [
				writeStream nextPut: Character lf.
			].
		] ifFalse: [
			writeStream nextPut: char.
		].
	].
	source := writeStream contents.
	[
		source last codePoint <= 32.
	] whileTrue: [
		source := source copyFrom: 1 to: source size - 1.
	].
	^source.
!

documentPresenter

	^documentPresenter!

errorMessagePresenter
	^errorMessagePresenter!

frameListPresenter
	^frameListPresenter!

getProcessList
	"#getProcessList copied from RowanProjectsBrowserPresenter>>updatePresenters"

	| debuggerService |
	debuggerService := RowanDebuggerService onProcess: gsProcess.
	self updateServices: (Array with: debuggerService).
	debuggerService processes ifNil:[^self].
	debuggerService processes notEmpty ifTrue:[
		processListPresenter selection: debuggerService processes first]!

handleSourceChanges
	documentPresenter view isModified
		ifTrue: 
			[(MessageBox confirm: 'Copy changes to clipboard?' caption: 'Method has unsaved changes!!')
				ifTrue: [documentPresenter value copyToClipboard].
			documentPresenter value: frame gsMethod source.
			documentPresenter view isModified: false]!

methodListSelection
	"really a selection from the frame list"

	| methodService |
	methodService := self basicSelectedFrame gsMethod.
	methodService command: nil.
	self updateServices: (Array with: methodService).
	^methodService!

methodSourcePresenter

	^codePane!

methodValueChanged
	frameListPresenter selectionIfNone: [^true].
	documentPresenter view
		backcolor: JadeTextStyles default colorForUnsavedEdits;
		clearContainerIndicators!

onCloseRequested: boolValueHolder

	self handleSourceChanges. 
	super onCloseRequested: boolValueHolder!

onIdleEntered
	"Private - Support for automated testing so we know when UI is ready"

	| cachedClass cachedSession |
	cachedClass := self class.
	cachedSession := gciSession.
	[
		[
			(Delay forMilliseconds: 10) wait.
			cachedSession isCallInProgress.
		] whileTrue: [].
		cachedClass signalSemaphore.
	] forkAt: Processor userBackgroundPriority.
	super onIdleEntered.!

onViewClosed
	"Private - copied from RowanSystemBrowserPresenter>>onViewClosed"

	processList := processListPresenter list collect: [:each | GsProcess session: gciSession oopType: each oopType].
	super onViewClosed.
	subPresenters do: [:presenter |
		BrowserUpdate current removeEventsTriggeredFor: presenter.
	].
	"Allow test to know when view is closed"
	self class signalSemaphore.
!

onViewOpened
	"Private - copied from RowanProjectsBrowserPresenter>>onViewOpened"

	super onViewOpened.
	self registerPresentersForUpdates.
	BrowserUpdate current inUpdate: false. "The debugger isn't opened in a forked process so make sure our 'semaphore' won't block updates"!

processListPresenter
	^processListPresenter!

queryCommand: query

	processList := processListPresenter list collect: [:each | GsProcess session: gciSession oopType: each oopType].
	(#(#browseFrameMethod #browseImplementors #browseSenders) includes: query commandSymbol) ifTrue: [
		query isEnabled: frame method selector notNil.
		^true
	].
	(#(#'resumeProcess' #'runToCursor' #'stepInto' #'stepOut' #'stepOver') includes: query commandSymbol) ifTrue: [
		query isEnabled: processListPresenter selectionByIndex == 1.
		^true
	].
	(#(#'terminateProcess') includes: query commandSymbol) ifTrue: [
		query isEnabled: (processList notEmpty and: [processList first ~~ gsProcess]).
		^true
	].
	(#(#'implement') includes: query commandSymbol) ifTrue: [
		query isEnabled: false. "frameListPresenter model first subStrings first = 'MessageNotUnderstood'."
		^true
	].
	(#(#'browseObjectClass') includes: query commandSymbol) ifTrue: [
		query isEnabled: variableListPresenter hasSelection.
		^true
	].
	(query commandSymbol ==#saveMethod) ifTrue: [
		query isEnabled: documentPresenter view isModified.
		^true
	].
	^super queryCommand: query
!

registerPresentersForUpdates
	"copied from RowanProjectsBrowserPresenter>>registerPresentersForUpdates"

	BrowserUpdate current
		register: processListPresenter 
		selector: #processListUpdate:
!

reportCompileError: anArrayOfArray in: methodSourcePresenter

	| source position stream string indicators |
	indicators := OrderedCollection new.
	methodSourcePresenter view clearContainerIndicators.
	source := (self currentMethodSourceFrom: methodSourcePresenter value) , Character lf asString.
	position := methodSourcePresenter view caretPosition.
	methodSourcePresenter value: source.
	methodSourcePresenter view caretPosition: position.
	stream := WriteStream on: String new.
	anArrayOfArray do: [:eachArray | 
		| start |
		start := eachArray at: 2.
		string := eachArray at: 3.
		stream nextPutAll: string , '; '.
		indicators add: (ScintillaIndicator
			styleName: 10 
			range: (start to: (start + 10 min: source size)) 
			tag: string).
	].
	methodSourcePresenter view backcolor: JadeTextStyles default colorForCompileError.
	methodSourcePresenter view indicators: indicators.
	string := stream contents.
	self statusBarText: (string copyFrom: 1 to: string size - 2).!

restart
	self handleSourceChanges.
	(gsProcess gsProcessForSession: gciSession)
		trimStackToLevel: frameListPresenter selectionByIndex + frameListOffset.
	self getProcessList!

saveMethod
	| classService string |
	frame isExecutedBlockContext
		ifTrue: [^MessageBox notify: 'Cannot save code in an executed block context.'].
	string := self getDocumentData reject: [:each | each == Character cr].
	classService := RowanClassService new
				name: frame method className;
				meta: frame method meta;
				yourself.
	classService
		command: #saveMethodSource:category:;
		commandArgs: (Array with: string with: frame method category).
	[self issueCommand: (Array with: classService)] on: GsCompileError
		do: [:ex | ^self reportCompileError: ex list in: nil].
	(self compilationWarningFrom: classService selectedMethods)
		ifNotNil: [:warning | MessageBox notify: warning].
	documentPresenter isModified: false.
	(gsProcess gsProcessForSession: gciSession)
		trimStackToLevel: frameListPresenter selectionByIndex + frameListOffset.
	self getProcessList!

selectedFrame
	"Comment from RowanFrameService>>initializeProcess:level:organizer: ---- 
		'In 3.2.15 the server does some whacky things with IP, stepPoint, and nested methods.
		See http://kermit.gemtalksystems.com/bug?bug=45553 --JGF'
	When the step point error is thrown, we do a step into instead. There may be better
	solutions."

	| index breaks source |
	(index := frameListPresenter selectionByIndex) = 0 ifTrue: [^self].
	index := index + frameListOffset.
	(frame := gsProcess frameForLevel: index) isNil ifTrue: [^self].
	frame stepPoint ifNil: [RowanDebuggerStepPointError signal].		"see comment in  RowanFrameService>>initializeProcess:level:organizer:"
	source := frame source.
	breaks := frame breaks collect: [:each | frame offsets at: each].
	breaks := breaks
				collect: [:each | ((source copyFrom: 1 to: each) select: [:char | char = Character lf]) size + 1].
	breaks := breaks asSet asSortedCollection asArray.
	variableListPresenter list: frame vars.
	self handleSourceChanges.
	codePane
		gsMethod: frame gsMethod.
	frame stepPoint ifNotNil: [codePane showStepPoint: frame stepPoint]. 
	(self view viewNamed: 'toolbar') update!

stepInto
	self handleSourceChanges. 
	gciSession
		stepInto: (GsProcess session: gciSession oopType: gsProcess oopType)
		inFrame: frameListPresenter selectionByIndex + frameListOffset.
	[
		self getProcessList.
	] on: RowanDebuggerStepPointError do: [:ex | 
		self stepInto.
	].!

stepOut
	self handleSourceChanges.
	gciSession stepOut: (GsProcess session: gciSession oopType: gsProcess oopType)
		inFrame: frameListPresenter selectionByIndex + frameListOffset.
	[self getProcessList] on: RowanDebuggerStepPointError do: [:ex | self stepInto]!

stepOver
	self handleSourceChanges.
	gciSession stepOver: (GsProcess session: gciSession oopType: gsProcess oopType)
		inFrame: frameListPresenter selectionByIndex + frameListOffset.
	[self getProcessList] on: RowanDebuggerStepPointError do: [:ex | self stepInto]!

terminateProcess
	(MessageBox confirm: 'Terminate process?' caption: 'GemStone/S Process Termination')
		ifFalse: [^self].
	self handleSourceChanges.
	self _terminateProcess!

updateMethodSource

	self selectedFrame!

variableDataPresenter
	^variableDataPresenter!

variableListPresenter
	^variableListPresenter! !
!RowanDebugger categoriesFor: #_processList!public! !
!RowanDebugger categoriesFor: #_terminateProcess:!public! !
!RowanDebugger categoriesFor: #basicSelectedFrame!public! !
!RowanDebugger categoriesFor: #browseFrameMethod!menu handlers!private! !
!RowanDebugger categoriesFor: #browseImplementors!menu handlers!private! !
!RowanDebugger categoriesFor: #browseObjectClass!menu handlers!private! !
!RowanDebugger categoriesFor: #browseSenders!menu handlers!private! !
!RowanDebugger categoriesFor: #compilationWarningFrom:!compiling!public! !
!RowanDebugger categoriesFor: #createSchematicWiring!public! !
!RowanDebugger categoriesFor: #currentMethodSourceFrom:!compiling!public! !
!RowanDebugger categoriesFor: #documentPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #errorMessagePresenter!Accessing!public! !
!RowanDebugger categoriesFor: #frameListPresenter!Accessing!frames!public! !
!RowanDebugger categoriesFor: #getProcessList!public! !
!RowanDebugger categoriesFor: #handleSourceChanges!frames!private! !
!RowanDebugger categoriesFor: #methodListSelection!Accessing!public! !
!RowanDebugger categoriesFor: #methodSourcePresenter!public! !
!RowanDebugger categoriesFor: #methodValueChanged!event handlers!public! !
!RowanDebugger categoriesFor: #onCloseRequested:!event handlers!private! !
!RowanDebugger categoriesFor: #onIdleEntered!event handlers!private! !
!RowanDebugger categoriesFor: #onViewClosed!event handlers!private! !
!RowanDebugger categoriesFor: #onViewOpened!event handlers!private! !
!RowanDebugger categoriesFor: #processListPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #queryCommand:!menu handlers!public! !
!RowanDebugger categoriesFor: #registerPresentersForUpdates!public!register presenters! !
!RowanDebugger categoriesFor: #reportCompileError:in:!compiling!public! !
!RowanDebugger categoriesFor: #restart!menu handlers!private! !
!RowanDebugger categoriesFor: #saveMethod!compiling!menu handlers!private! !
!RowanDebugger categoriesFor: #selectedFrame!frames!public! !
!RowanDebugger categoriesFor: #stepInto!menu handlers!private! !
!RowanDebugger categoriesFor: #stepOut!menu handlers!private! !
!RowanDebugger categoriesFor: #stepOver!menu handlers!private! !
!RowanDebugger categoriesFor: #terminateProcess!menu handlers!private! !
!RowanDebugger categoriesFor: #updateMethodSource!public! !
!RowanDebugger categoriesFor: #variableDataPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #variableListPresenter!Accessing!public! !

!RowanDebugger class methodsFor!

aboutToShowErrorShellOn: gsError
	self semaphore ifNil: [^self].
	[
		^self openDebuggerOnException: gsError
	] on: GsError do: [:ex | 
		ex debugger view close.
		MessageBox warning: ex description.
		Processor activeProcess terminate. 
	]!

debuggerClass
	^debuggerClass ifNil: [RowanDebugger]!

debuggerClass: aDebugger
"
	RowanDebugger debuggerClass: nil.
	RowanDebugger debuggerClass: JadeDebugger.
	RowanDebugger debuggerClass: RowanDebugger.
"
	debuggerClass := aDebugger.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 551 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140850816 1 608 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil nil nil 7 nil nil nil 608 nil 8 4294902541 852486 ##(Smalltalk.NullConverter) nil nil 3 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 321 7 514 1245 39 608 850 #text: 34 1 8 'Static Text' 608 850 #setMarginWidths: 34 1 8 #(3 3) 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 3 0 0 0 14 3 0 0 22 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 321 1138 #fixedParentRight -3 1138 #fixedParentTop 7 1138 #fixedViewTop 39 410 ##(Smalltalk.ToolbarA) 34 25 nil 416 34 2 8 1140853580 131137 1216 nil 466 8 4278190080 nil 519 nil nil nil 1216 466 8 4278190080 8 4294905713 nil 170 192 34 14 25301 1115910 ##(Smalltalk.ToolbarIconButton) 25301 nil 1216 1 1180998 4 ##(Smalltalk.CommandDescription) #showNextStatement 8 'Top Of Stack' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'icons\TopOfStack2.ico' nil 395334 3 ##(Smalltalk.Bitmap) nil true nil nil nil nil 3 514 33 33 1 25303 1394 25303 nil 1216 1 1426 #stepInto 8 'Step Into' 1 1 1474 nil true 1520 8 'StepInto.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil 1554 nil true nil nil nil nil 3 514 33 33 1 25305 1394 25305 nil 1216 1 1426 #stepOver 8 'Step Over' 1 1 1474 nil true 1520 8 'StepOver.ico' 1696 1554 nil true nil nil nil nil 3 514 33 33 1 25307 1394 25307 nil 1216 1 1426 #stepOut 8 'Step Out' 1 1 1474 nil true 1520 8 'StepOut.ico' 1696 1554 nil true nil nil nil nil 3 514 33 33 1 25309 1394 25309 nil 1216 1 1426 #runToCursor 8 'Run to Cursor' 1 1 1474 nil true 1520 8 'RunToCursor.ico' 1696 1554 nil true nil nil nil nil 3 514 33 33 1 25311 1394 25311 nil 1216 1 1426 #resumeProcess 8 'Go' 1 1 1474 nil true 1520 8 'Run.ico' 1696 1554 nil true nil nil nil nil 3 514 33 33 1 25313 1394 25313 nil 1216 1 1426 #restart 8 'Restart' 13545 1 1474 nil true 1520 8 'icons\ArrowCircle.ico' nil 1554 nil true nil nil nil nil 3 514 33 33 1 34 7 1408 1600 1760 1872 1984 2096 2208 170 176 34 14 2288 13 1728 3 1840 5 1952 7 1568 1 2064 9 2176 11 nil 1 nil 514 33 33 514 45 45 nil nil 786 138 144 34 2 850 #createAt:extent: 34 2 514 1 1 514 321 51 1216 850 #updateSizePosted 8 #() 1216 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 25 0 0 0] 8 #() 1088 nil 27 1106 1152 1 1138 #fixedViewLeft 321 1184 1 1200 51 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 2624 nil nil nil 7 nil nil nil 2624 1180166 ##(Smalltalk.ProportionalLayout) 170 176 2528 true 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 2624 34 2 8 1140850688 131073 2768 nil 466 8 4278190080 nil 7 nil nil nil 2768 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeiteMethodSourcePresenter) #resource_Default_view nil 786 138 144 34 1 850 #createAt:extent: 34 2 514 1 521 514 1569 513 2768 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 4 1 0 0 16 3 0 0 4 2 0 0] 2528 1088 nil 27 8 'codePane' nil 786 138 144 34 1 850 #createAt:extent: 34 2 514 1 51 514 1569 1033 2624 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 16 3 0 0 29 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 2624 34 2 8 1140850688 131073 3216 nil 466 496 nil 7 nil nil nil 3216 2690 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 3216 34 2 8 1140850688 131073 3344 nil nil nil 7 nil nil nil 3344 546 170 176 34 6 410 ##(Smalltalk.ListBox) 34 17 nil 3344 34 2 8 1144062209 1025 3456 590662 2 ##(Smalltalk.ListModel) 138 144 2528 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 496 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 3 984134 2 ##(Smalltalk.CommandMenuItem) 1 1426 #browseFrameMethod 8 '&Browse Frame Method' 1 1 nil nil nil 3666 1 1426 #browseImplementors 8 'Browse &Implementors' 1 1 nil nil nil 3666 1 1426 #browseSenders 8 'Browse &Senders' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 3456 nil 8 4294904823 ##(Smalltalk.BasicListAbstract) 2528 false 786 138 144 34 3 850 #createAt:extent: 34 2 514 1 51 514 859 461 3456 850 #contextMenu: 34 1 3632 3456 850 #horizontalExtent: 8 #(0) 3456 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 173 1 0 0 255 0 0 0] 8 #() 1088 nil 27 1106 1152 1 1168 1 1138 #fixedPreviousBottom 1 1138 #fixedParentBottom 1 410 ##(Smalltalk.PushButton) 34 20 nil 3344 34 2 8 1140924416 1 4128 nil nil nil 7 nil nil nil 4128 nil 8 4294902979 1426 #terminateProcess 8 'Terminate' 1 1 nil nil false nil nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 514 719 1 514 141 51 4128 850 #isEnabled: 8 #(false) 4128 850 #text: 34 1 8 'Terminate' 4128 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 103 1 0 0 0 0 0 0 173 1 0 0 25 0 0 0] 8 #() 1088 nil 29 1106 1138 #fixedPreviousRight 1 1168 1 1184 1 1200 51 410 ##(Smalltalk.ComboBox) 34 17 nil 3344 34 2 8 1412498947 1025 4512 3522 138 144 2528 nil 3584 466 8 4278190080 nil 7 nil nil nil 4512 nil 8 4294906199 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each printString]' 8 #[30 105 226 0 106] #printString 4672 7 257 nil 2528 401 786 138 144 34 1 850 #createAt:extent: 34 2 514 1 1 514 719 53 4512 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 103 1 0 0 26 0 0 0] 8 #() 1088 nil 27 1106 1152 1 1168 -139 1184 1 1200 47 170 192 34 4 3456 8 'frameList' 4512 8 'processList' nil 786 138 144 34 1 850 #createAt:extent: 34 2 514 1 1 514 859 511 3344 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 173 1 0 0 255 0 0 0] 34 3 4512 4128 3456 1088 nil 27 3 410 ##(Smalltalk.ContainerView) 34 15 nil 3216 34 2 8 1140850688 131073 5168 nil 466 496 nil 7 nil nil nil 5168 2690 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 5168 34 2 8 1140920397 1025 5296 3522 138 144 2528 nil 3584 466 496 nil 7 3618 nil true 34 2 3666 1 1426 #browseObjectClass 8 '&Browse Object Class' 1 1 nil nil nil 3666 1 1426 #inspectVariable 8 'Inspect' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 5296 nil 8 4294904829 ##(Smalltalk.BasicListAbstract) nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 3 920646 5 ##(Smalltalk.ListViewColumn) 8 'Variable' 201 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 4658 0 nil 4690 2 1 4656 8 'doIt' 8 '[:each | each key value]' 8 #[30 105 226 0 142 106] #key 5680 7 257 nil nil 5296 nil 1 nil nil 5634 8 'Value' 313 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 4658 0 459302 ##(Smalltalk.Context) 1 1 nil nil 4690 0 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each value]' 34 1 138 ##(Smalltalk.PoolDictionary) 2528 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 nil nil 5296 nil 3 nil nil 5634 8 'OOP' 181 #right 459270 ##(Smalltalk.Message) #displayString 8 #() 5986 #<= 6016 4658 0 nil 4690 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each key key asNumber]' 8 #[31 105 226 0 158 159 106] #key #asNumber 6048 7 257 nil nil 5296 nil 1 nil nil #report 2528 nil 131169 nil 34 4 nil nil 514 1 1 nil 786 138 144 34 3 850 #createAt:extent: 34 2 514 1 1 514 701 333 5296 850 #contextMenu: 34 1 5408 5296 850 #text: 34 1 8 'Variable' 5296 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 166 0 0 0] 8 #() 1088 nil 27 5 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 5168 34 2 8 1140920580 1025 6400 674 nil false 720 nil 466 8 4278190080 nil 7 3618 nil true 34 20 3666 1 1426 #undo 8 '&Undo' 9397 1 nil nil nil 3666 1 1426 #redo 8 'R&edo' 9395 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 3666 1 1426 #editCut 8 'Cu&t' 9393 1 nil nil nil 3666 1 1426 #editCopy 8 '&Copy' 9351 1 nil nil nil 3666 1 1426 #editPaste 8 '&Paste' 9389 1 nil nil nil 3666 1 1426 #editDelete 8 'De&lete' 1629 1 nil nil nil 3666 1 1426 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 6642 4097 3666 1 1426 #editFind 8 '&Find...' 9357 1 nil nil nil 3666 1 1426 #editFindNext 8 'Find &Next' 9359 1 nil nil nil 3666 1 1426 #editReplace 8 '&Replace...' 9361 1 nil nil nil 6642 4097 3666 1 1426 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 3666 1 1426 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 3666 1 1426 #jadeExecute 8 'Execute' 9355 1 nil nil nil 3666 1 1426 #jadeDebug 8 'De&bug' 9349 1 nil nil nil 6642 4097 3666 1 1426 #addQuotesToSelection 8 'Add &Quotes' 1 1 nil nil nil 3666 1 1426 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 6400 nil 8 4294902541 754 nil nil 9 786 138 144 34 3 850 #createAt:extent: 34 2 514 1 343 514 701 169 6400 850 #contextMenu: 34 1 6512 6400 850 #setMarginWidths: 34 1 8 #(3 3) 6400 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 171 0 0 0 94 1 0 0 255 0 0 0] 8 #() 1088 nil 27 3 true 170 192 34 4 5296 8 'variableList' 6400 8 'variableData' nil 786 138 144 34 1 850 #createAt:extent: 34 2 514 869 1 514 701 511 5168 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 178 1 0 0 0 0 0 0 16 3 0 0 255 0 0 0] 34 3 5296 410 ##(Smalltalk.Splitter) 34 12 nil 5168 34 2 8 1140850688 1 7904 nil 466 496 nil 519 nil nil nil 7904 786 138 144 34 1 850 #createAt:extent: 34 2 514 1 333 514 701 11 7904 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 166 0 0 0 94 1 0 0 171 0 0 0] 8 #() 1088 nil 27 6400 1088 nil 27 524806 ##(Smalltalk.Fraction) 253 311 false 170 192 2528 nil 786 138 144 34 1 850 #createAt:extent: 34 2 514 1 1 514 1569 511 3216 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 255 0 0 0] 34 3 3344 410 ##(Smalltalk.Splitter) 34 12 nil 3216 34 2 8 1140850688 1 8352 nil 466 496 nil 519 nil nil nil 8352 786 138 144 34 1 850 #createAt:extent: 34 2 514 859 1 514 11 511 8352 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 173 1 0 0 0 0 0 0 178 1 0 0 255 0 0 0] 8 #() 1088 nil 27 5168 1088 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 2624 34 2 8 1140850688 1 8592 nil 466 496 nil 519 nil nil nil 8592 786 138 144 34 1 850 #createAt:extent: 34 2 514 1 511 514 1569 11 8592 1026 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 255 0 0 0 16 3 0 0 4 1 0 0] 8 #() 1088 nil 27 2768 1088 nil 27 1106 1152 1 1168 1 1184 51 4112 1 170 192 34 4 1216 8 'toolbar' 608 8 'errorMessage' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 2 3618 nil true 34 7 3666 1 1426 #saveMethod 8 '&Save' 9383 1 nil nil nil 3666 1 1426 #copyStack nil 1 1 nil nil nil 6642 4097 3666 1 1426 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 3666 1 1426 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 3666 1 1426 #jadeExecute 8 '&Execute' 9355 1 nil nil nil 3666 1 1426 #jadeDebug 8 'Debu&g' 9359 1 nil nil nil 8 '&Edit' nil 134217729 nil nil 25283 nil nil 3618 nil true 34 7 3666 1 1426 #showNextStatement 8 'Top of Stack' 1 1 1474 nil true 1520 8 'icons\TopOfStack2.ico' nil nil nil 3666 1 1426 #stepInto nil 1 1 1474 nil true 1520 8 'StepInto.ico' 1696 nil nil 3666 1 1426 #stepOver nil 1 1 1474 nil true 1520 8 'StepOver.ico' 1696 nil nil 3666 1 1426 #stepOut nil 1 1 1474 nil true 1520 8 'StepOut.ico' 1696 nil nil 3666 1 1426 #runToCursor nil 1 1 1474 nil true 1520 8 'RunToCursor.ico' 1696 nil nil 3666 1 1426 #resumeProcess 8 'Resume' 1 1 1474 nil true 1520 8 'Run.ico' 1696 nil nil 3666 1 1426 #restart nil 13545 1 1474 nil true 1520 8 'icons\ArrowCircle.ico' nil nil nil 8 '&Debug' nil 134217729 nil nil 25299 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 1 1474 nil true 1520 8 'icons\GS32.ico' nil 1474 nil true 1520 8 'icons\GS16.ico' nil nil nil 1 nil nil 786 138 144 34 3 850 #createAt:extent: 34 2 514 7679 21 514 1601 1201 416 850 #text: 34 1 8 'Jade Debugger' 416 850 #updateMenuBar 2528 416 1026 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 31 18 0 0 98 2 0 0] 34 3 608 1216 2624 1088 nil 27 )!

semaphore
	"Support for automated testing so we know when UI is ready"

	^semaphore!

semaphore: aSemaphore
	"Support for automated testing so we know when UI is ready"

	semaphore := aSemaphore.
!

signalSemaphore

	self semaphore ifNotNil: [:value | 
		[
			value signal.
			SessionManager inputState prod.
		] forkAt: Processor userBackgroundPriority.
	].! !
!RowanDebugger class categoriesFor: #aboutToShowErrorShellOn:!public! !
!RowanDebugger class categoriesFor: #debuggerClass!public! !
!RowanDebugger class categoriesFor: #debuggerClass:!public! !
!RowanDebugger class categoriesFor: #resource_Default_view!public!resources-views! !
!RowanDebugger class categoriesFor: #semaphore!public! !
!RowanDebugger class categoriesFor: #semaphore:!public! !
!RowanDebugger class categoriesFor: #signalSemaphore!public! !

