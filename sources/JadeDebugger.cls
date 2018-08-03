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
	self getProcessList.
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

	processList ifNotNil: [
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
			selectionByIndex: 1.
	] ifFalse: [
		self stack: stack.
	].!

updateCaption

	self caption: (gciSession titleBarFor: 'Debugger').
!

updateSaveMethodOop: anInteger
	"do nothing"
! !
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
!JadeDebugger categoriesFor: #updateCaption!public! !
!JadeDebugger categoriesFor: #updateSaveMethodOop:!public! !

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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1601 1201 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 6 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140850816 1 624 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 0 0 7 0 0 0 624 0 8 4294901907 852486 ##(Smalltalk.NullConverter)  0 0 3 983302 ##(Smalltalk.MessageSequence)  202 208 98 5 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 291 7 530 1275 39 624 882 8 #text: 98 1 8 'Static Text' 624 882 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 624 882 8 #isTextModified: 98 1 32 624 882 8 #setMarginWidths: 98 1 98 2 7 7 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 145 0 0 0 3 0 0 0 14 3 0 0 22 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 291 1346 8 #fixedParentRight -3 1346 8 #fixedParentTop 7 1346 8 #fixedViewTop 39 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 1488 0 0 0 7 0 0 0 1488 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 98 2 410 8 ##(Smalltalk.ReferenceView)  98 14 0 1488 98 2 8 1140850688 131073 1664 0 482 8 4278190080 0 7 0 0 0 1664 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.MethodSourcePresenter)  8 #resource_Default_view 0 818 202 208 98 1 882 912 98 2 530 1 525 530 1569 509 1664 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 6 1 0 0 16 3 0 0 4 2 0 0] 1616 1296 0 27 8 'codePane' 0 818 202 208 98 1 882 912 98 2 530 1 51 530 1569 1033 1488 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 16 3 0 0 29 2 0 0] 98 3 410 1504 98 15 0 1488 98 2 8 1140850688 131073 2160 0 482 512 0 7 0 0 0 2160 1570 234 240 98 4 410 1504 98 15 0 2160 98 2 8 1140850688 131073 2288 0 0 0 7 0 0 0 2288 562 234 240 98 6 410 8 ##(Smalltalk.PushButton)  98 20 0 2288 98 2 8 1140924416 1 2400 0 0 0 7 0 0 0 2400 0 8 4294902463 1180998 4 ##(Smalltalk.CommandDescription)  8 #terminateProcess 8 'Terminate' 1 1 0 0 32 0 0 0 818 202 208 98 3 882 912 98 2 530 715 1 530 141 51 2400 882 8 #isEnabled: 98 1 32 2400 882 992 98 1 8 'Terminate' 2400 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 101 1 0 0 0 0 0 0 171 1 0 0 25 0 0 0] 98 0 1296 0 29 1314 1346 8 #fixedPreviousRight 1 1392 1 1424 1 1456 51 410 8 ##(Smalltalk.ComboBox)  98 17 0 2288 98 2 8 1412498947 1025 2864 590662 2 ##(Smalltalk.ListModel)  202 208 1616 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 2864 0 8 4294902379 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each printString]' 8 #[30 105 226 0 106] 8 #printString 3088 7 257 0 1616 401 818 202 208 98 1 882 912 98 2 530 1 1 530 715 47 2864 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 101 1 0 0 23 0 0 0] 98 0 1296 0 27 1314 1360 1 1392 -139 1424 1 1456 47 410 8 ##(Smalltalk.ListBox)  98 17 0 2288 98 2 8 1144062209 1025 3392 2946 202 208 1616 0 3008 482 512 0 7 0 0 0 3392 0 8 4294903235 8 ##(Smalltalk.BasicListAbstract)  1616 32 818 202 208 98 2 882 912 98 2 530 1 51 530 855 457 3392 882 8 #horizontalExtent: 98 1 1 3392 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 171 1 0 0 253 0 0 0] 98 0 1296 0 27 1314 1360 1 1392 1 1346 8 #fixedPreviousBottom 1 1346 8 #fixedParentBottom 1 234 256 98 4 2864 8 'processList' 3392 8 'frameList' 0 818 202 208 98 1 882 912 98 2 530 1 1 530 855 507 2288 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 171 1 0 0 253 0 0 0] 98 3 2864 2400 3392 1296 0 27 3 410 1504 98 15 0 2160 98 2 8 1140850688 131073 4064 0 482 512 0 7 0 0 0 4064 1570 234 240 98 2 410 8 ##(Smalltalk.ListView)  98 30 0 4064 98 2 8 1140920397 1025 4192 2946 202 208 1616 0 3008 482 512 0 7 0 0 0 4192 0 8 4294903309 3536 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 3 920646 5 ##(Smalltalk.ListViewColumn)  8 'Variable' 201 8 #left 3536 8 ##(Smalltalk.SortedCollection)  3074 0 0 3106 2 1 3072 8 'doIt' 8 '[:each | each key value]' 8 #[30 105 226 0 142 106] 8 #key 4480 7 257 0 0 4192 0 1 0 0 4402 8 'Value' 317 4448 3536 4464 3074 0 459302 ##(Smalltalk.Context)  1 1 0 0 3106 0 9 3136 8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  1616 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 4192 0 3 0 0 4402 8 'OOP' 181 8 #right 459270 ##(Smalltalk.Message)  8 #displayString 98 0 4834 8 #<= 4880 3074 0 0 3106 3 1 3136 8 'doIt' 8 '[:each | each key key asNumber]' 8 #[31 105 226 0 158 159 106] 4560 8 #asNumber 4928 7 257 0 0 4192 0 1 0 0 8 #report 1616 0 131169 0 0 818 202 208 98 2 882 912 98 2 530 1 1 530 697 325 4192 882 992 98 1 8 'Variable' 4192 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 92 1 0 0 162 0 0 0] 98 0 1296 0 27 5 16 234 256 98 4 4192 8 'variableList' 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 4064 98 2 8 1143017796 1025 5296 0 482 512 0 7 0 0 0 5296 0 8 4294901907 786 0 0 9 818 202 208 98 3 882 912 98 2 530 1 343 530 697 165 5296 882 1056 98 1 1090 3 1 3 5296 882 1136 98 1 32 5296 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 171 0 0 0 92 1 0 0 253 0 0 0] 98 0 1296 0 27 8 'variableData' 0 818 202 208 98 1 882 912 98 2 530 873 1 530 697 507 4064 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 1 0 0 0 0 0 0 16 3 0 0 253 0 0 0] 98 3 4192 410 8 ##(Smalltalk.Splitter)  98 12 0 4064 98 2 8 1140850688 1 5840 0 482 512 0 519 0 0 0 5840 818 202 208 98 1 882 912 98 2 530 1 325 530 697 19 5840 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 162 0 0 0 92 1 0 0 171 0 0 0] 98 0 1296 0 27 5296 1296 0 27 524806 ##(Smalltalk.Fraction)  253 311 32 234 256 1616 0 818 202 208 98 1 882 912 98 2 530 1 1 530 1569 507 2160 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 253 0 0 0] 98 3 2288 410 5856 98 12 0 2160 98 2 8 1140850688 1 6304 0 482 512 0 519 0 0 0 6304 818 202 208 98 1 882 912 98 2 530 855 1 530 19 507 6304 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 171 1 0 0 0 0 0 0 180 1 0 0 253 0 0 0] 98 0 1296 0 27 4064 1296 0 27 410 5856 98 12 0 1488 98 2 8 1140850688 1 6544 0 482 512 0 519 0 0 0 6544 818 202 208 98 1 882 912 98 2 530 1 507 530 1569 19 6544 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 253 0 0 0 16 3 0 0 6 1 0 0] 98 0 1296 0 27 1664 1296 0 27 1314 1360 1 1392 1 1424 51 3808 1 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1409289036 131137 6800 0 482 8 4278190080 0 519 0 0 0 6800 482 6896 8 4294903255 234 256 1616 234 256 98 12 38589 1115910 ##(Smalltalk.ToolbarIconButton)  38589 0 6800 1 2498 8 #showNextStatement 8 'Show Next Statement' 1 1 263494 3 ##(Smalltalk.Icon)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'ShowNextStatement.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 395334 3 ##(Smalltalk.Bitmap)  0 16 0 0 0 0 3 530 33 33 1 38591 6994 38591 0 6800 1 2498 8 #stepInto 8 'Step Into' 1 1 7074 0 16 7120 8 'StepInto.ico' 7168 7202 0 16 0 0 0 0 3 530 33 33 1 38593 6994 38593 0 6800 1 2498 8 #stepOver 8 'Step Over' 1 1 7074 0 16 7120 8 'StepOver.ico' 7168 7202 0 16 0 0 0 0 3 530 33 33 1 38595 6994 38595 0 6800 1 2498 8 #stepOut 8 'Step Out' 1 1 7074 0 16 7120 8 'StepOut.ico' 7168 7202 0 16 0 0 0 0 3 530 33 33 1 38597 6994 38597 0 6800 1 2498 8 #runToCursor 8 'Run to Cursor' 1 1 7074 0 16 7120 8 'RunToCursor.ico' 7168 7202 0 16 0 0 0 0 3 530 33 33 1 38599 6994 38599 0 6800 1 2498 8 #resumeProcess 8 'Go' 1 1 7074 0 16 7120 8 'Run.ico' 7168 7202 0 16 0 0 0 0 3 530 33 33 1 98 7 7008 7248 7376 7504 7632 7760 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 6800 3 0 1 234 240 98 12 7472 5 7216 1 7344 3 7600 7 7728 9 7856 11 0 1 0 530 33 33 530 45 45 0 0 818 202 208 98 2 882 912 98 2 530 1 1 530 1569 51 6800 882 8 #updateSize 1616 6800 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 25 0 0 0] 98 0 1296 0 27 1314 1360 1 1392 1 1424 1 1456 51 234 256 98 2 624 8 'errorMessage' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 3 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 2498 8 #saveMethod 8 '&Save' 9383 1 0 0 0 8 '&File' 0 134217729 0 0 38581 0 0 8306 0 16 98 0 8 '&Edit' 0 134217729 0 0 38583 0 0 8306 0 16 98 1 8354 1 2498 8 #implement 8 '&Implement Method In ...' 1 1 0 0 0 8 '&Debug' 0 134217729 0 0 38587 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 7074 0 16 7120 8 'icons\GS32.ico' 0 7074 0 16 7120 8 'icons\GS16.ico' 0 0 0 1 0 0 818 202 208 98 3 882 912 98 2 530 2879 21 530 1601 1201 416 882 992 98 1 8 'Jade Debugger' 416 882 8 #updateMenuBar 1616 416 1234 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 191 8 0 0 98 2 0 0] 98 3 624 6800 1488 1296 0 27 )! !
!JadeDebugger class categoriesFor: #debuggerFor:!public! !
!JadeDebugger class categoriesFor: #debuggers!public! !
!JadeDebugger class categoriesFor: #openDebuggerOnException:!public! !
!JadeDebugger class categoriesFor: #openOn:message:terminateOnClose:!public! !
!JadeDebugger class categoriesFor: #reportError:!public! !
!JadeDebugger class categoriesFor: #resetDebuggers!public! !
!JadeDebugger class categoriesFor: #resource_Default_view!public!resources-views! !

