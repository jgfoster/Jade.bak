"Filed out from Dolphin Smalltalk 7"!

JadeTextDocument subclass: #JadeDebugger
	instanceVariableNames: 'answer errorMessagePresenter frame frameListOffset frameListPresenter gsProcess processList processListPresenter terminateOnClose variableDataPresenter variableListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'debuggers'!
JadeDebugger guid: (GUID fromString: '{54a4ddbf-2101-4884-8568-46d6d2790987}')!
JadeDebugger comment: ''!
!JadeDebugger categoriesForClass!Unclassified! !
!JadeDebugger methodsFor!

_terminateProcess

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
	frameListOffset := 0.
	variableListPresenter list: #().
	variableDataPresenter value: ''.
	documentPresenter value: ''.
!

codeSourcePresenterClass

	^MethodSourcePresenter
!

contextObject

	^gciSession oopTypeWithOop: frame vars first key key asNumber.
!

createComponents

	super createComponents.
	errorMessagePresenter	:= self add: TextPresenter		new name: 'errorMessage'.
	frameListPresenter 		:= self add: ListPresenter		new name: 'frameList'.
	frameListOffset		:= 0.
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
		type := fields at: 9.	"forked or main (see JadeServer64Bit3x>>#'addProcess:to:withStatus:scheduler:')"
		(GsProcess session: gciSession oopType: oopType)
			type: type;
			yourself
	].
	processList addFirst: gsProcess.
	gsProcess type: 'active'.!

implement

	self halt.
!

initializeProcess: aProcess message: aString terminateOnClose: aBoolean

	gsProcess := aProcess.
	errorMessagePresenter value: aString.
	terminateOnClose := aBoolean.
	[
		self getProcessList.
	] on: GsError do: [:ex | 
		ex debugger: self.
		ex pass.
	].
	self class debuggers
		at: gsProcess oopType asInteger
		put: self.
	self update.
!

inspectVariable

	| object |
	object := gciSession oopTypeWithOop: variableListPresenter selection key key asNumber.
	JadeInspector showOn: object session: gciSession.
!

onViewClosed

	(processList notNil and: [processList notEmpty]) ifTrue: [
		gsProcess := processList first.
		self class debuggers removeKey: gsProcess oopType asInteger ifAbsent: [].
		(terminateOnClose and: [answer isNil]) ifTrue: [
			gsProcess terminate.
			self error: 'We should never get here!!'.
		].
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
	(#(#'implement') includes: query commandSymbol) ifTrue: [
		query isEnabled: false. "frameListPresenter model first subStrings first = 'MessageNotUnderstood'."
		^true.
	].
	^super queryCommand: query.
!

resumeProcess

	answer := #'resume'.
	self view close.
!

runToCursor

	MessageBox notify: 'Sorry, this feature is not yet implemented!!'.
!

saveMethod

	| result |
	result := gciSession
		serverPerform: #'compile:frame:process:' 
		with: (self getDocumentData reject: [:each | each == Character cr])
		with: frameListPresenter selectionByIndex + frameListOffset
		with: gsProcess.
	(result isKindOf: Boolean) ifTrue: [
		result ifTrue: [
			documentPresenter isModified: false.
			gsProcess trimStackToLevel: frameListPresenter selectionByIndex + frameListOffset.
			^self update.
		] ifFalse: [
			MessageBox notify: 'New method has different selector so will not be on stack!!'.
			^self selectedFrame.
		].
	].
	MessageBox notify: result.
!

selectedFrame 

	| index breaks source |
	(index := frameListPresenter selectionByIndex) = 0 ifTrue: [^self].
	index := index + frameListOffset.
	(frame := gsProcess frameForLevel: index) isNil ifTrue: [^self].
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
!

selectedProcess

	self clearUI.
	(gsProcess := processListPresenter selectionOrNil) ifNil: [^self].
	self stack: gsProcess stack.
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

stack: aList

	| index |
	index := self stackInitialSelection.
	frameListOffset := index - 1.
	frameListPresenter list: (aList copyFrom: index to: aList size).
	aList notEmpty ifTrue: [
		frameListPresenter selectionByIndex: 1.
	].
!

stackInitialSelection

	| list |
	list := gsProcess stack.
	self stackInitialSelectionData do: [:each | 
		1 to: list size do: [:i | 
			0 < ((list at: i) indexOfSubCollection: each) ifTrue: [^i + 1].
		].
	].
	^1.
!

stackInitialSelectionData
	"Line number to select if string included on previous line (need not be exact match)"

	^#(
		'(AbstractException) >> _signalAsync @7 line 21'
		'(Object) >> _doesNotUnderstand:args:envId:reason: @7 line 12'
		'(Object) >> _doesNotUnderstand:args:envId:reason: @8 line 13'
		'(Object) >> error: @6 line 7'
		'(Object) >> error: (envId 0) @6 line 7'
		'(Object) >> error: @6 line 7'
		'(Object) >> halt @2 line 5'
		'(Object) >> halt (envId 0) @2 line 5'
		'(Object) >> pause @2 line 7'
		'(Object)>>#_doesNotUnderstand: @1 line 6'
		'(Object)>>#error: @1 line 6'
		'(Object)>>#error: @3 line 7'
		'(Object)>>#halt @1 line 6'
		'(Object)>>#pause @2 line 8'
		'(TestCase)>>#assert: @1 line 1'
		'(TestCase)>>#deny: @2 line 3'
		'(TestCase)>>#should:raise: @10 line 9'
		'(TestCase)>>#shouldnt:raise: @4 line 2'
		'GSTestCase >> assert: @1 line 1'
		'GSTestCase >> assert: (envId 0) @1 line 1'
		'Object >> _doesNotUnderstand:'
		'Object >> _doesNotUnderstand:args:envId:reason: @7 line 12'
		'Object >> _doesNotUnderstand:args:envId:reason: (envId 0) @7 line 12'
		'Object >> _doesNotUnderstand:args:envId:reason: @8 line 13'
		'Object >> error: @6 line 7'
		'Object >> error: (envId 0) @6 line 7'
		'Object >> halt @2 line 5'
		'Object >> halt (envId 0) @2 line 5'
		'(TestAsserter) >> assert: @5 line 4'
		'TestAsserter >> assert: (envId 0) @5 line 4'
	)!

statusBarText: aString
	"called by MethodSourcePresenter"!

stepInto
	
	gsProcess stepInto: frameListPresenter selectionByIndex + frameListOffset.
	^self update!

stepOut

	gsProcess stepOut: frameListPresenter selectionByIndex + frameListOffset + 1.
	^self update!

stepOver

	gsProcess stepOver: frameListPresenter selectionByIndex + frameListOffset.
	^self update!

terminateProcess

	(MessageBox confirm: 'Terminate process?') ifFalse: [^self].
	self _terminateProcess.!

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
			selectionByIndex: 1.
	] ifFalse: [
		self stack: stack.
	].!

updateSaveMethodOop: anInteger
	"do nothing"
! !
!JadeDebugger categoriesFor: #_terminateProcess!public! !
!JadeDebugger categoriesFor: #answer!public! !
!JadeDebugger categoriesFor: #clearUI!public! !
!JadeDebugger categoriesFor: #codeSourcePresenterClass!private! !
!JadeDebugger categoriesFor: #contextObject!public! !
!JadeDebugger categoriesFor: #createComponents!public! !
!JadeDebugger categoriesFor: #createSchematicWiring!public! !
!JadeDebugger categoriesFor: #getProcessList!public! !
!JadeDebugger categoriesFor: #implement!public! !
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
!JadeDebugger categoriesFor: #stack:!public! !
!JadeDebugger categoriesFor: #stackInitialSelection!public! !
!JadeDebugger categoriesFor: #stackInitialSelectionData!public! !
!JadeDebugger categoriesFor: #statusBarText:!public! !
!JadeDebugger categoriesFor: #stepInto!public! !
!JadeDebugger categoriesFor: #stepOut!public! !
!JadeDebugger categoriesFor: #stepOver!public! !
!JadeDebugger categoriesFor: #terminateProcess!public! !
!JadeDebugger categoriesFor: #update!public! !
!JadeDebugger categoriesFor: #updateSaveMethodOop:!public! !

!JadeDebugger class methodsFor!

aboutToShowErrorShellOn: gsError!

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

	^(self showOnSession: gsProcess gciSession)
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
	(#(2709 6005) includes: gsError errorReport number) ifTrue: [
		^self openDebuggerOnException: gsError.
	].
	self aboutToShowErrorShellOn: gsError.	"allow RowanDebuggerTestCase to override and skip error dialog and go straight to debugger"
	answer := (JadeErrorShell showModalOn: gsError) ifNil: [#'terminate'].	"Window was closed without pressing any button!!"
	answer = #'terminate' ifTrue: [
		gsError terminateProcess.
		self error: 'We should never get here!!'.
	].
	answer = #'resume' ifTrue: [
		^#'resume'.
	].
	answer = #'debug' ifTrue: [
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 551 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ToolbarA) 34 25 nil 416 34 2 8 1140853580 131137 608 nil 466 8 4278190080 nil 519 nil nil nil 608 466 8 4278190080 8 4294905431 nil 170 192 34 12 15585 1115910 ##(Smalltalk.ToolbarIconButton) 15585 nil 608 1 1180998 4 ##(Smalltalk.CommandDescription) #showNextStatement 8 'Show Next Statement' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'ShowNextStatement.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil 395334 3 ##(Smalltalk.Bitmap) nil true nil nil nil nil 3 514 33 33 1 15587 786 15587 nil 608 1 818 #stepInto 8 'Step Into' 1 1 866 nil true 912 8 'StepInto.ico' 960 994 nil true nil nil nil nil 3 514 33 33 1 15589 786 15589 nil 608 1 818 #stepOver 8 'Step Over' 1 1 866 nil true 912 8 'StepOver.ico' 960 994 nil true nil nil nil nil 3 514 33 33 1 15591 786 15591 nil 608 1 818 #stepOut 8 'Step Out' 1 1 866 nil true 912 8 'StepOut.ico' 960 994 nil true nil nil nil nil 3 514 33 33 1 15593 786 15593 nil 608 1 818 #runToCursor 8 'Run to Cursor' 1 1 866 nil true 912 8 'RunToCursor.ico' 960 994 nil true nil nil nil nil 3 514 33 33 1 15595 786 15595 nil 608 1 818 #resumeProcess 8 'Go' 1 1 866 nil true 912 8 'Run.ico' 960 994 nil true nil nil nil nil 3 514 33 33 1 34 7 800 1040 1152 1264 1376 1488 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 608 3 nil 1 170 176 34 12 1120 3 1232 5 1344 7 1008 1 1456 9 1568 11 nil 1 nil 514 33 33 514 45 45 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 291 51 608 1778 #updateSizePosted 8 #() 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 145 0 0 0 25 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2002 #fixedViewLeft 291 2002 #fixedParentTop 1 2002 #fixedViewTop 51 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 2080 nil nil nil 7 nil nil nil 2080 1180166 ##(Smalltalk.ProportionalLayout) 170 176 1872 true 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 2080 34 2 8 1140850688 131073 2224 nil 466 8 4278190080 nil 7 nil nil nil 2224 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.MethodSourcePresenter) #resource_Default_view nil 1714 138 144 34 1 1778 #createAt:extent: 34 2 514 1 521 514 1569 513 2224 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 4 1 0 0 16 3 0 0 4 2 0 0] 1872 1952 nil 27 8 'codePane' nil 1714 138 144 34 1 1778 #createAt:extent: 34 2 514 1 51 514 1569 1033 2080 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 16 3 0 0 29 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 2080 34 2 8 1140850688 131073 2672 nil 466 496 nil 7 nil nil nil 2672 2146 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 2672 34 2 8 1140850688 131073 2800 nil 466 496 nil 7 nil nil nil 2800 2146 170 176 34 2 410 ##(Smalltalk.ListView) 34 30 nil 2800 34 2 8 1140920397 1025 2928 590662 2 ##(Smalltalk.ListModel) 138 144 1872 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 496 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 818 #browseObjectClass 8 '&Browse Object Class' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 2928 nil 8 4294905341 ##(Smalltalk.BasicListAbstract) nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 3 920646 5 ##(Smalltalk.ListViewColumn) 8 'Variable' 201 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 3344 8 'doIt' 8 '[:each | each key value]' 8 #[30 105 226 0 142 106] #key 3360 7 257 nil nil 2928 nil 1 nil nil 3298 8 'Value' 313 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 3346 0 459302 ##(Smalltalk.Context) 1 1 nil nil 3378 0 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each value]' 34 1 138 ##(Smalltalk.PoolDictionary) 1872 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 nil nil 2928 nil 3 nil nil 3298 8 'OOP' 181 #right 459270 ##(Smalltalk.Message) #displayString 8 #() 3682 #<= 3712 3346 0 nil 3378 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each key key asNumber]' 8 #[31 105 226 0 158 159 106] #key #asNumber 3744 7 257 nil nil 2928 nil 1 nil nil #report 1872 nil 131169 nil 34 4 nil nil 514 1 1 nil 1714 138 144 34 3 1778 #createAt:extent: 34 2 514 1 1 514 701 333 2928 1778 #contextMenu: 34 1 3104 2928 1778 #text: 34 1 8 'Variable' 2928 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 166 0 0 0] 8 #() 1952 nil 27 5 true 170 192 34 4 2928 8 'variableList' 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 2800 34 2 8 1143017796 1025 4144 nil 466 496 nil 7 nil nil nil 4144 nil 8 4294903327 852486 ##(Smalltalk.NullConverter) nil nil 9 1714 138 144 34 1 1778 #createAt:extent: 34 2 514 1 343 514 701 169 4144 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 171 0 0 0 94 1 0 0 255 0 0 0] 8 #() 1952 nil 27 8 'variableData' nil 1714 138 144 34 1 1778 #createAt:extent: 34 2 514 869 1 514 701 511 2800 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 178 1 0 0 0 0 0 0 16 3 0 0 255 0 0 0] 34 3 2928 410 ##(Smalltalk.Splitter) 34 12 nil 2800 34 2 8 1140850688 1 4608 nil 466 496 nil 519 nil nil nil 4608 1714 138 144 34 1 1778 #createAt:extent: 34 2 514 1 333 514 701 11 4608 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 166 0 0 0 94 1 0 0 171 0 0 0] 8 #() 1952 nil 27 4144 1952 nil 27 524806 ##(Smalltalk.Fraction) 253 311 410 ##(Smalltalk.ContainerView) 34 15 nil 2672 34 2 8 1140850688 131073 4880 nil nil nil 7 nil nil nil 4880 546 170 176 34 6 410 ##(Smalltalk.ListBox) 34 17 nil 4880 34 2 8 1144062209 1025 4992 2994 138 144 1872 nil 3056 466 496 nil 7 3090 nil true 34 3 3138 1 818 #browseFrameMethod 8 '&Browse Frame Method' 1 1 nil nil nil 3138 1 818 #browseImplementors 8 'Browse &Implementors' 1 1 nil nil nil 3138 1 818 #browseSenders 8 'Browse &Senders' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 4992 nil 8 4294903473 ##(Smalltalk.BasicListAbstract) 1872 false 1714 138 144 34 3 1778 #createAt:extent: 34 2 514 1 51 514 859 461 4992 1778 #contextMenu: 34 1 5104 4992 1778 #horizontalExtent: 8 #(0) 4992 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 173 1 0 0 255 0 0 0] 8 #() 1952 nil 27 1970 2016 1 2002 #fixedParentRight 1 2002 #fixedPreviousBottom 1 2002 #fixedParentBottom 1 410 ##(Smalltalk.PushButton) 34 20 nil 4880 34 2 8 1140924416 1 5600 nil nil nil 7 nil nil nil 5600 nil 8 4294905231 818 #terminateProcess 8 'Terminate' 1 1 nil nil false nil nil nil 1714 138 144 34 3 1778 #createAt:extent: 34 2 514 719 1 514 141 51 5600 1778 #isEnabled: 8 #(false) 5600 1778 #text: 34 1 8 'Terminate' 5600 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 103 1 0 0 0 0 0 0 173 1 0 0 25 0 0 0] 8 #() 1952 nil 29 1970 2002 #fixedPreviousRight 1 5552 1 2048 1 2064 51 410 ##(Smalltalk.ComboBox) 34 17 nil 4880 34 2 8 1412498947 1025 5984 2994 138 144 1872 nil 3056 466 8 4278190080 nil 7 nil nil nil 5984 nil 8 4294905397 3346 0 nil 3378 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each printString]' 8 #[30 105 226 0 106] #printString 6128 7 257 nil 1872 401 1714 138 144 34 1 1778 #createAt:extent: 34 2 514 1 1 514 719 53 5984 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 103 1 0 0 26 0 0 0] 8 #() 1952 nil 27 1970 2016 1 5552 -139 2048 1 2064 47 170 192 34 4 4992 8 'frameList' 5984 8 'processList' nil 1714 138 144 34 1 1778 #createAt:extent: 34 2 514 1 1 514 859 511 4880 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 173 1 0 0 255 0 0 0] 34 3 5984 5600 4992 1952 nil 27 3 false 170 192 1872 nil 1714 138 144 34 1 1778 #createAt:extent: 34 2 514 1 1 514 1569 511 2672 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 255 0 0 0] 34 3 4880 410 ##(Smalltalk.Splitter) 34 12 nil 2672 34 2 8 1140850688 1 6784 nil 466 496 nil 519 nil nil nil 6784 1714 138 144 34 1 1778 #createAt:extent: 34 2 514 859 1 514 11 511 6784 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 173 1 0 0 0 0 0 0 178 1 0 0 255 0 0 0] 8 #() 1952 nil 27 2800 1952 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 2080 34 2 8 1140850688 1 7024 nil 466 496 nil 519 nil nil nil 7024 1714 138 144 34 1 1778 #createAt:extent: 34 2 514 1 511 514 1569 11 7024 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 255 0 0 0 16 3 0 0 4 1 0 0] 8 #() 1952 nil 27 2224 1952 nil 27 1970 2016 1 5552 1 2048 51 5584 1 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140850816 1 7280 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil nil nil 7 nil nil nil 7280 nil 8 4294903327 4242 nil nil 3 1714 138 144 34 3 1778 #createAt:extent: 34 2 514 291 7 514 1275 39 7280 1778 #text: 34 1 8 'Static Text' 7280 1778 #setMarginWidths: 34 1 8 #(3 3) 7280 1890 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 145 0 0 0 3 0 0 0 14 3 0 0 22 0 0 0] 8 #() 1952 nil 27 1970 2016 291 5552 -3 2048 7 2064 39 170 192 34 4 608 8 'toolbar' 7280 8 'errorMessage' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 3090 nil true 34 1 3138 1 818 #saveMethod 8 '&Save' 9383 1 nil nil nil 8 '&File' nil 134217729 nil nil 15577 nil nil 3090 nil true 8 #() 8 '&Edit' nil 134217729 nil nil 15579 nil nil 3090 nil true 34 1 3138 1 818 #implement 8 '&Implement Method In ...' 1 1 nil nil nil 8 '&Debug' nil 134217729 nil nil 15583 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 1 866 nil true 912 8 'icons\GS32.ico' nil 866 nil true 912 8 'icons\GS16.ico' nil nil nil 1 nil nil 1714 138 144 34 3 1778 #createAt:extent: 34 2 514 5119 21 514 1601 1201 416 1778 #text: 34 1 8 'Jade Debugger' 416 1778 #updateMenuBar 1872 416 1890 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 31 13 0 0 98 2 0 0] 34 3 7280 608 2080 1952 nil 27 )! !
!JadeDebugger class categoriesFor: #aboutToShowErrorShellOn:!public! !
!JadeDebugger class categoriesFor: #debuggerFor:!public! !
!JadeDebugger class categoriesFor: #debuggers!public! !
!JadeDebugger class categoriesFor: #openDebuggerOnException:!public! !
!JadeDebugger class categoriesFor: #openOn:message:terminateOnClose:!public! !
!JadeDebugger class categoriesFor: #reportError:!public! !
!JadeDebugger class categoriesFor: #resetDebuggers!public! !
!JadeDebugger class categoriesFor: #resource_Default_view!public!resources-views! !

