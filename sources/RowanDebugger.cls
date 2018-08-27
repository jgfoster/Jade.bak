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
	frameListPresenter when: #actionPerformed send: #browseClass to: self.
!

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

	self updateServices: (Array with: (RowanDebuggerService onProcess: gsProcess)).!

methodListSelection
	"really a selection from the frame list"

	| methodService |
	methodService := self basicSelectedFrame gsMethod.
	methodService command: nil.
	self updateServices: (Array with: methodService).
	^methodService!

methodSourcePresenter

	^codePane!

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
	self 
		registerPresentersForUpdates;
		"updatePresenters;"	"see #getProcessList"
		yourself.
!

processListPresenter
	^processListPresenter!

queryCommand: query

	processList := processListPresenter list collect: [:each | GsProcess session: gciSession oopType: each oopType].
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
	(gsProcess gsProcessForSession: gciSession)
		trimStackToLevel: frameListPresenter selectionByIndex + frameListOffset.
	self getProcessList.!

saveMethod
	| classService string |
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
		ifNotNil: [:warning |MessageBox notify: warning].
	documentPresenter isModified: false.
	(gsProcess gsProcessForSession: gciSession)
		trimStackToLevel: frameListPresenter selectionByIndex + frameListOffset.
	self getProcessList.
!

selectedFrame 

	| index breaks source |
	(index := frameListPresenter selectionByIndex) = 0 ifTrue: [^self].
	index := index + frameListOffset.
	(frame := gsProcess frameForLevel: index) isNil ifTrue: [^self].
	frame stepPoint ifNil: [RowanDebuggerStepPointError signal].		"see comment in  RowanFrameService>>initializeProcess:level:organizer:"
	source := frame source.
	breaks := frame breaks collect: [:each | frame offsets at: each].
	breaks := breaks collect: [:each | ((source copyFrom: 1 to: each) select: [:char | char = Character lf]) size + 1].
	breaks := breaks asSet asSortedCollection asArray.
	variableListPresenter list: frame vars.
	codePane 
		gsMethod: frame gsMethod;
		showStepPoint: frame stepPoint;
		yourself.
	"breaks do: [:each | 
		documentPresenter view 
			addMarkerType: #'breakpoint' 
			at: each.
	]."
	(self view viewNamed: 'toolbar') update.!

stepInto

	gciSession
		stepInto: (GsProcess session: gciSession oopType: gsProcess oopType)
		inFrame: frameListPresenter selectionByIndex + frameListOffset.
	[
		self getProcessList.
	] on: RowanDebuggerStepPointError do: [:ex | 
		self stepInto.
	].!

stepOut

	gciSession
		stepOut: (GsProcess session: gciSession oopType: gsProcess oopType)
		inFrame: frameListPresenter selectionByIndex + frameListOffset.
	[
		self getProcessList.
	] on: RowanDebuggerStepPointError do: [:ex | 
		self stepInto.
	].!

stepOver

	gciSession
		stepOver: (GsProcess session: gciSession oopType: gsProcess oopType)
		inFrame: frameListPresenter selectionByIndex + frameListOffset.
	[
		self getProcessList.
	] on: RowanDebuggerStepPointError do: [:ex | 
		self stepInto.
	].!

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
!RowanDebugger categoriesFor: #frameListPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #getProcessList!public! !
!RowanDebugger categoriesFor: #methodListSelection!Accessing!public! !
!RowanDebugger categoriesFor: #methodSourcePresenter!public! !
!RowanDebugger categoriesFor: #onIdleEntered!event handlers!private! !
!RowanDebugger categoriesFor: #onViewClosed!event handlers!private! !
!RowanDebugger categoriesFor: #onViewOpened!event handlers!private! !
!RowanDebugger categoriesFor: #processListPresenter!Accessing!public! !
!RowanDebugger categoriesFor: #queryCommand:!public! !
!RowanDebugger categoriesFor: #registerPresentersForUpdates!public! !
!RowanDebugger categoriesFor: #reportCompileError:in:!compiling!public! !
!RowanDebugger categoriesFor: #restart!menu handlers!private! !
!RowanDebugger categoriesFor: #saveMethod!compiling!menu handlers!private! !
!RowanDebugger categoriesFor: #selectedFrame!public! !
!RowanDebugger categoriesFor: #stepInto!menu handlers!private! !
!RowanDebugger categoriesFor: #stepOut!menu handlers!private! !
!RowanDebugger categoriesFor: #stepOver!menu handlers!private! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 551 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ToolbarA) 34 25 nil 416 34 2 8 1140853580 131137 608 nil 466 8 4278190080 nil 519 nil nil nil 608 466 8 4278190080 8 4294905873 nil 170 192 34 14 10913 1115910 ##(Smalltalk.ToolbarIconButton) 10913 nil 608 1 1180998 4 ##(Smalltalk.CommandDescription) #runToCursor 8 'Run to Cursor' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'RunToCursor.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil 395334 3 ##(Smalltalk.Bitmap) nil true nil nil nil nil 3 514 33 33 1 10915 786 10915 nil 608 1 818 #resumeProcess 8 'Go' 1 1 866 nil true 912 8 'Run.ico' 960 994 nil true nil nil nil nil 3 514 33 33 1 10917 786 10917 nil 608 1 818 #restart 8 'Restart' 13545 1 866 nil true 912 8 'icons\ArrowCircle.ico' nil 994 nil true nil nil nil nil 3 514 33 33 1 10905 786 10905 nil 608 1 818 #showNextStatement 8 'Top Of Stack' 1 1 866 nil true 912 8 'ShowNextStatement.ico' 960 994 nil true nil nil nil nil 3 514 33 33 1 10907 786 10907 nil 608 1 818 #stepInto 8 'Step Into' 1 1 866 nil true 912 8 'StepInto.ico' 960 994 nil true nil nil nil nil 3 514 33 33 1 10909 786 10909 nil 608 1 818 #stepOver 8 'Step Over' 1 1 866 nil true 912 8 'StepOver.ico' 960 994 nil true nil nil nil nil 3 514 33 33 1 10911 786 10911 nil 608 1 818 #stepOut 8 'Step Out' 1 1 866 nil true 912 8 'StepOut.ico' 960 994 nil true nil nil nil nil 3 514 33 33 1 34 7 1264 1376 1488 1600 800 1040 1152 170 176 34 14 1232 13 1456 3 1568 5 1680 7 1344 1 1008 9 1120 11 nil 1 nil 514 33 33 514 45 45 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 321 51 608 1858 #updateSizePosted 8 #() 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 160 0 0 0 25 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2082 #fixedViewLeft 321 2082 #fixedParentTop 1 2082 #fixedViewTop 51 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140850816 1 2160 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil nil nil 7 nil nil nil 2160 nil 8 4294905631 852486 ##(Smalltalk.NullConverter) nil nil 3 1794 138 144 34 3 1858 #createAt:extent: 34 2 514 321 7 514 1245 39 2160 1858 #text: 34 1 8 'Static Text' 2160 1858 #setMarginWidths: 34 1 8 #(3 3) 2160 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 0 0 0 3 0 0 0 14 3 0 0 22 0 0 0] 8 #() 2032 nil 27 2050 2096 321 2082 #fixedParentRight -3 2128 7 2144 39 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 2624 nil nil nil 7 nil nil nil 2624 1180166 ##(Smalltalk.ProportionalLayout) 170 176 1952 true 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 2624 34 2 8 1140850688 131073 2768 nil 466 8 4278190080 nil 7 nil nil nil 2768 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeiteMethodSourcePresenter) #resource_Default_view nil 1794 138 144 34 1 1858 #createAt:extent: 34 2 514 1 521 514 1569 513 2768 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 4 1 0 0 16 3 0 0 4 2 0 0] 1952 2032 nil 27 8 'codePane' nil 1794 138 144 34 1 1858 #createAt:extent: 34 2 514 1 51 514 1569 1033 2624 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 16 3 0 0 29 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 2624 34 2 8 1140850688 131073 3216 nil 466 496 nil 7 nil nil nil 3216 2690 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 3216 34 2 8 1140850688 131073 3344 nil nil nil 7 nil nil nil 3344 546 170 176 34 6 410 ##(Smalltalk.PushButton) 34 20 nil 3344 34 2 8 1140924416 1 3456 nil nil nil 7 nil nil nil 3456 nil 8 4294905517 818 #terminateProcess 8 'Terminate' 1 1 nil nil false nil nil nil 1794 138 144 34 3 1858 #createAt:extent: 34 2 514 719 1 514 141 51 3456 1858 #isEnabled: 8 #(false) 3456 1858 #text: 34 1 8 'Terminate' 3456 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 103 1 0 0 0 0 0 0 173 1 0 0 25 0 0 0] 8 #() 2032 nil 29 2050 2082 #fixedPreviousRight 1 2608 1 2128 1 2144 51 410 ##(Smalltalk.ListBox) 34 17 nil 3344 34 2 8 1144062209 1025 3840 590662 2 ##(Smalltalk.ListModel) 138 144 1952 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 496 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 3 984134 2 ##(Smalltalk.CommandMenuItem) 1 818 #browseFrameMethod 8 '&Browse Frame Method' 1 1 nil nil nil 4050 1 818 #browseImplementors 8 'Browse &Implementors' 1 1 nil nil nil 4050 1 818 #browseSenders 8 'Browse &Senders' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 3840 nil 8 4294906485 ##(Smalltalk.BasicListAbstract) 1952 false 1794 138 144 34 3 1858 #createAt:extent: 34 2 514 1 51 514 859 461 3840 1858 #contextMenu: 34 1 4016 3840 1858 #horizontalExtent: 8 #(0) 3840 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 173 1 0 0 255 0 0 0] 8 #() 2032 nil 27 2050 2096 1 2608 1 2082 #fixedPreviousBottom 1 2082 #fixedParentBottom 1 410 ##(Smalltalk.ComboBox) 34 17 nil 3344 34 2 8 1412498947 1025 4512 3906 138 144 1952 nil 3968 466 8 4278190080 nil 7 nil nil nil 4512 nil 8 4294902773 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each printString]' 8 #[30 105 226 0 106] #printString 4672 7 257 nil 1952 401 1794 138 144 34 1 1858 #createAt:extent: 34 2 514 1 1 514 719 53 4512 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 103 1 0 0 26 0 0 0] 8 #() 2032 nil 27 2050 2096 1 2608 -139 2128 1 2144 47 170 192 34 4 3840 8 'frameList' 4512 8 'processList' nil 1794 138 144 34 1 1858 #createAt:extent: 34 2 514 1 1 514 859 511 3344 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 173 1 0 0 255 0 0 0] 34 3 4512 3456 3840 2032 nil 27 3 410 ##(Smalltalk.ContainerView) 34 15 nil 3216 34 2 8 1140850688 131073 5168 nil 466 496 nil 7 nil nil nil 5168 2690 170 176 34 2 410 ##(Smalltalk.ListView) 34 30 nil 5168 34 2 8 1140920397 1025 5296 3906 138 144 1952 nil 3968 466 496 nil 7 4002 nil true 34 2 4050 1 818 #browseObjectClass 8 '&Browse Object Class' 1 1 nil nil nil 4050 1 818 #inspectVariable 8 'Inspect' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 5296 nil 8 4294905723 ##(Smalltalk.BasicListAbstract) nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 3 920646 5 ##(Smalltalk.ListViewColumn) 8 'Variable' 201 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 4658 0 nil 4690 2 1 4656 8 'doIt' 8 '[:each | each key value]' 8 #[30 105 226 0 142 106] #key 5680 7 257 nil nil 5296 nil 1 nil nil 5634 8 'Value' 313 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 4658 0 459302 ##(Smalltalk.Context) 1 1 nil nil 4690 0 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each value]' 34 1 138 ##(Smalltalk.PoolDictionary) 1952 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 nil nil 5296 nil 3 nil nil 5634 8 'OOP' 181 #right 459270 ##(Smalltalk.Message) #displayString 8 #() 5986 #<= 6016 4658 0 nil 4690 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each key key asNumber]' 8 #[31 105 226 0 158 159 106] #key #asNumber 6048 7 257 nil nil 5296 nil 1 nil nil #report 1952 nil 131169 nil 34 4 nil nil 514 1 1 nil 1794 138 144 34 3 1858 #createAt:extent: 34 2 514 1 1 514 701 333 5296 1858 #contextMenu: 34 1 5408 5296 1858 #text: 34 1 8 'Variable' 5296 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 166 0 0 0] 8 #() 2032 nil 27 5 true 170 192 34 4 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 5168 34 2 8 1143017796 1025 6432 nil 466 496 nil 7 nil nil nil 6432 nil 8 4294905631 2306 nil nil 9 1794 138 144 34 1 1858 #createAt:extent: 34 2 514 1 343 514 701 169 6432 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 171 0 0 0 94 1 0 0 255 0 0 0] 8 #() 2032 nil 27 8 'variableData' 5296 8 'variableList' nil 1794 138 144 34 1 1858 #createAt:extent: 34 2 514 869 1 514 701 511 5168 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 178 1 0 0 0 0 0 0 16 3 0 0 255 0 0 0] 34 3 5296 410 ##(Smalltalk.Splitter) 34 12 nil 5168 34 2 8 1140850688 1 6896 nil 466 496 nil 519 nil nil nil 6896 1794 138 144 34 1 1858 #createAt:extent: 34 2 514 1 333 514 701 11 6896 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 166 0 0 0 94 1 0 0 171 0 0 0] 8 #() 2032 nil 27 6432 2032 nil 27 524806 ##(Smalltalk.Fraction) 253 311 false 170 192 1952 nil 1794 138 144 34 1 1858 #createAt:extent: 34 2 514 1 1 514 1569 511 3216 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 255 0 0 0] 34 3 3344 410 ##(Smalltalk.Splitter) 34 12 nil 3216 34 2 8 1140850688 1 7344 nil 466 496 nil 519 nil nil nil 7344 1794 138 144 34 1 1858 #createAt:extent: 34 2 514 859 1 514 11 511 7344 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 173 1 0 0 0 0 0 0 178 1 0 0 255 0 0 0] 8 #() 2032 nil 27 5168 2032 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 2624 34 2 8 1140850688 1 7584 nil 466 496 nil 519 nil nil nil 7584 1794 138 144 34 1 1858 #createAt:extent: 34 2 514 1 511 514 1569 11 7584 1970 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 255 0 0 0 16 3 0 0 4 1 0 0] 8 #() 2032 nil 27 2768 2032 nil 27 2050 2096 1 2608 1 2128 51 4496 1 170 192 34 4 608 8 'toolbar' 2160 8 'errorMessage' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 4002 nil true 34 1 4050 1 818 #saveMethod 8 '&Save' 9383 1 nil nil nil 8 '&File' nil 134217729 nil nil 10875 nil nil 4002 nil true 34 6 4050 1 818 #copyStack nil 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 4050 1 818 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 4050 1 818 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 4050 1 818 #jadeExecute 8 '&Execute' 9355 1 nil nil nil 4050 1 818 #jadeDebug 8 'Debu&g' 9359 1 nil nil nil 8 '&Edit' nil 134217729 nil nil 10887 nil nil 4002 nil true 34 7 4050 1 818 #showNextStatement 8 'Top of Stack' 1 1 866 nil true 912 8 'ShowNextStatement.ico' 960 nil nil 4050 1 818 #stepInto nil 1 1 866 nil true 912 8 'StepInto.ico' 960 nil nil 4050 1 818 #stepOver nil 1 1 866 nil true 912 8 'StepOver.ico' 960 nil nil 4050 1 818 #stepOut nil 1 1 866 nil true 912 8 'StepOut.ico' 960 nil nil 4050 1 818 #runToCursor nil 1 1 866 nil true 912 8 'RunToCursor.ico' 960 nil nil 4050 1 818 #resumeProcess 8 'Resume' 1 1 866 nil true 912 8 'Run.ico' 960 nil nil 4050 1 818 #restart nil 13545 1 866 nil true 912 8 'Up.ico' 960 nil nil 8 '&Debug' nil 134217729 nil nil 10903 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 1 866 nil true 912 8 'icons\GS32.ico' nil 866 nil true 912 8 'icons\GS16.ico' nil nil nil 1 nil nil 1794 138 144 34 3 1858 #createAt:extent: 34 2 514 7679 21 514 1601 1201 416 1858 #text: 34 1 8 'Jade Debugger' 416 1858 #updateMenuBar 1952 416 1970 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 31 18 0 0 98 2 0 0] 34 3 2160 608 2624 2032 nil 27 )!

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

