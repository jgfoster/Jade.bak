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
	[
		SessionManager inputState loopWhile: [answer isNil].
	] on: GPFault do: [:ex | ].	"It seems that Scintilla can get an error when the debugger closes"
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

inspectGsProcess

	JadeInspector showOn: gsProcess oopType session: gciSession.
!

inspectVariable

	| object |
	object := gciSession oopTypeWithOop: variableListPresenter selection key key asNumber.
	JadeInspector showOn: object session: gciSession.
!

onViewClosed

	(processList isNil or: [processList isEmpty]) ifTrue: [^self].	"Occasionally a walkback here."
	gsProcess := processList first.
	self class debuggers removeKey: gsProcess oopType asInteger ifAbsent: [].
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
		with: (gciSession encode: self getDocumentData)
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
	MessageBox notify: (gciSession decode: result).
!

selectedFrame 

	| index breaks source |
	(index := frameListPresenter selectionByIndex) = 0 ifTrue: [^self].
	index := index + frameListOffset.
	(frame := gsProcess frameForLevel: index) isNil ifTrue: [^self].
	frame == GsStackFrame ifTrue: [self error: 'Should have gotten an instance of GsStackFrame, but got the class instead!!'].
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
		'(GSTestCase) >> assert: @2 line 3'
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
!JadeDebugger categoriesFor: #inspectGsProcess!public! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 327686 ##(Smalltalk.Color) #default 328198 ##(Smalltalk.Point) 1601 1201 549 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140850816 1 592 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil nil nil 7 nil nil nil 592 nil 8 1924714368 852486 ##(Smalltalk.NullConverter) nil nil 3 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 498 291 7 498 1275 39 592 834 #text: 34 1 8 'Static Text' 592 834 #setMarginWidths: 34 1 8 #(3 3) 592 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 145 0 0 0 3 0 0 0 14 3 0 0 22 0 0 0] 8 #() 498 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 291 1122 #fixedParentRight -3 1122 #fixedParentTop 7 1122 #fixedViewTop 39 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 1200 nil nil nil 7 nil nil nil 1200 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 1200 34 2 8 1140850688 131073 1360 nil 480 nil 7 nil nil nil 1360 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.MethodSourcePresenter) #resource_Default_view nil 770 138 144 34 1 834 #createAt:extent: 34 2 498 1 521 498 1569 513 1360 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 4 1 0 0 16 3 0 0 4 2 0 0] 1312 1072 nil 27 8 'codePane' nil 770 138 144 34 1 834 #createAt:extent: 34 2 498 1 51 498 1569 1033 1200 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 16 3 0 0 29 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 1200 34 2 8 1140850688 131073 1776 nil 480 nil 7 nil nil nil 1776 1266 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 1776 34 2 8 1140850688 131073 1888 nil 480 nil 7 nil nil nil 1888 1266 170 176 34 2 410 ##(Smalltalk.ListView) 34 45 nil 1888 34 2 8 1140920397 1025 2000 590662 2 ##(Smalltalk.ListModel) 138 144 1312 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 480 nil 7 nil nil nil 2000 nil 8 1924746528 ##(Smalltalk.BasicListAbstract) nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 3 920646 5 ##(Smalltalk.ListViewColumn) 8 'Variable' 201 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 2272 8 'doIt' 8 '[:each | each key value]' 8 #[30 105 226 0 142 106] #key 2288 7 257 nil nil 2000 nil 1 nil nil 2226 8 'Value' 313 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2274 0 459302 ##(Smalltalk.Context) 1 1 nil nil 2306 0 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each value]' 34 1 138 ##(Smalltalk.PoolDictionary) 1312 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 nil nil 2000 nil 3 nil nil 2226 8 'OOP' 181 #right 459270 ##(Smalltalk.Message) #displayString 8 #() 2610 #<= 2640 2274 0 nil 2306 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each key key asNumber]' 8 #[31 105 226 0 158 159 106] #key #asNumber 2672 7 257 nil nil 2000 nil 1 nil nil #report 8 #() nil 131169 nil 1 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 770 138 144 34 2 834 #createAt:extent: 34 2 498 1 1 498 701 333 2000 834 #text: 34 1 8 'Variable' 2000 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 166 0 0 0] 8 #() 1072 nil 35 5 true 170 192 34 4 2000 8 'variableList' 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 1888 34 2 8 1143017796 1025 3024 nil 480 nil 7 nil nil nil 3024 nil 8 1924714368 738 nil nil 9 770 138 144 34 1 834 #createAt:extent: 34 2 498 1 343 498 701 169 3024 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 171 0 0 0 94 1 0 0 255 0 0 0] 8 #() 1072 nil 27 8 'variableData' nil 770 138 144 34 1 834 #createAt:extent: 34 2 498 869 1 498 701 511 1888 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 178 1 0 0 0 0 0 0 16 3 0 0 255 0 0 0] 34 3 2000 410 ##(Smalltalk.Splitter) 34 12 nil 1888 34 2 8 1140850688 1 3456 nil 480 nil 519 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 3456 nil 1 #left nil nil nil 498 1 1 498 9 9 nil 3552 nil 770 138 144 34 1 834 #createAt:extent: 34 2 498 1 333 498 701 11 3456 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 166 0 0 0 94 1 0 0 171 0 0 0] 8 #() 1072 nil 27 3024 1072 nil 27 524806 ##(Smalltalk.Fraction) 253 311 410 ##(Smalltalk.ContainerView) 34 15 nil 1776 34 2 8 1140850688 131073 3776 nil nil nil 7 nil nil nil 3776 530 170 176 34 6 410 ##(Smalltalk.PushButton) 34 20 nil 3776 34 2 8 1140924416 1 3888 nil nil nil 7 nil nil nil 3888 nil 8 1924727040 1180998 4 ##(Smalltalk.CommandDescription) #terminateProcess 8 'Terminate' 1 1 nil nil false nil nil nil 770 138 144 34 3 834 #createAt:extent: 34 2 498 719 1 498 141 51 3888 834 #isEnabled: 8 #(false) 3888 834 #text: 34 1 8 'Terminate' 3888 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 103 1 0 0 0 0 0 0 173 1 0 0 25 0 0 0] 8 #() 1072 nil 29 1090 1122 #fixedPreviousRight 1 1152 1 1168 1 1184 51 410 ##(Smalltalk.ComboBox) 34 17 nil 3776 34 2 8 1412498947 1025 4288 2066 138 144 1312 nil 2128 480 nil 7 nil nil nil 4288 nil 8 1924579984 2274 0 nil 2306 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each printString]' 8 #[30 105 226 0 106] #printString 4400 7 257 nil 2752 401 770 138 144 34 1 834 #createAt:extent: 34 2 498 1 1 498 719 51 4288 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 103 1 0 0 25 0 0 0] 8 #() 1072 nil 27 1090 1136 1 1152 -139 1168 1 1184 47 410 ##(Smalltalk.ListBox) 34 17 nil 3776 34 2 8 1144062209 1025 4656 2066 138 144 1312 nil 2128 480 nil 7 nil nil nil 4656 nil 8 1924573152 ##(Smalltalk.BasicListAbstract) 2752 false 770 138 144 34 2 834 #createAt:extent: 34 2 498 1 51 498 859 461 4656 834 #horizontalExtent: 8 #(0) 4656 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 173 1 0 0 255 0 0 0] 8 #() 1072 nil 27 1090 1136 1 1152 1 1122 #fixedPreviousBottom 1 1122 #fixedParentBottom 1 170 192 34 4 4656 8 'frameList' 4288 8 'processList' nil 770 138 144 34 1 834 #createAt:extent: 34 2 498 1 1 498 859 511 3776 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 173 1 0 0 255 0 0 0] 34 3 4288 3888 4656 1072 nil 27 3 false 170 192 1312 nil 770 138 144 34 1 834 #createAt:extent: 34 2 498 1 1 498 1569 511 1776 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 255 0 0 0] 34 3 3776 410 ##(Smalltalk.Splitter) 34 12 nil 1776 34 2 8 1140850688 1 5408 nil 480 nil 519 nil nil nil 3522 5408 nil 1 #left nil nil nil 3552 3568 nil 3552 nil 770 138 144 34 1 834 #createAt:extent: 34 2 498 859 1 498 11 511 5408 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 173 1 0 0 0 0 0 0 178 1 0 0 255 0 0 0] 8 #() 1072 nil 27 1888 1072 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 1200 34 2 8 1140850688 1 5648 nil 480 nil 519 nil nil nil 3522 5648 nil 1 #left nil nil nil 3552 3568 nil 3552 nil 770 138 144 34 1 834 #createAt:extent: 34 2 498 1 511 498 1569 11 5648 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 255 0 0 0 16 3 0 0 4 1 0 0] 8 #() 1072 nil 27 1360 1072 nil 27 1090 1136 1 1152 1 1168 51 4992 1 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409289036 131137 5904 nil 480 nil 519 nil nil nil 5904 480 8 1924740080 170 192 1312 170 192 34 12 11651 1115206 1 ##(Smalltalk.ToolbarIconButton) 5904 1 3970 #runToCursor 8 'Run to Cursor' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'RunToCursor.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil 11651 11647 6034 5904 1 3970 #stepOver 8 'Step Over' 1 1 6098 nil true 6144 8 'StepOver.ico' 6192 11647 11643 6034 5904 1 3970 #showNextStatement 8 'Show Next Statement' 1 1 6098 nil true 6144 8 'ShowNextStatement.ico' 6192 11643 11653 6034 5904 1 3970 #resumeProcess 8 'Go' 1 1 6098 nil true 6144 8 'Run.ico' 6192 11653 11649 6034 5904 1 3970 #stepOut 8 'Step Out' 1 1 6098 nil true 6144 8 'StepOut.ico' 6192 11649 11645 6034 5904 1 3970 #stepInto 8 'Step Into' 1 1 6098 nil true 6144 8 'StepInto.ico' 6192 11645 34 7 6304 6544 6224 6464 6048 6384 1049158 1 ##(Smalltalk.ToolbarSeparator) 5904 1 nil nil 1 nil 498 33 33 498 45 45 nil nil 770 138 144 34 1 834 #createAt:extent: 34 2 498 1 1 498 1569 51 5904 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 25 0 0 0] 8 #() 1072 nil 27 1090 1136 1 1152 1 1168 1 1184 51 170 192 34 2 592 8 'errorMessage' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 3 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 3970 #saveMethod 8 '&Save' 9383 1 nil nil nil 8 '&File' nil 134217729 nil nil 11735 nil nil 6978 nil true 8 #() 8 '&Edit' nil 134217729 nil nil 11737 nil nil 6978 nil true 34 2 7026 1 3970 #inspectGsProcess 8 'Inspect &GsProcess' 1 1 nil nil nil 7026 1 3970 #implement 8 '&Implement Method In ...' 1 1 nil nil nil 8 '&Debug' nil 134217729 nil nil 11743 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 1 6098 nil true 6144 8 'icons\GS32.ico' nil 6098 nil true 6144 8 'icons\GS16.ico' nil nil nil 1 nil nil 770 138 144 34 2 834 #createAt:extent: 34 2 498 5119 21 498 1601 1201 416 834 #text: 34 1 8 'Jade Debugger' 416 1010 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 31 13 0 0 98 2 0 0] 34 3 592 5904 1200 1072 nil 27 )! !
!JadeDebugger class categoriesFor: #debuggerFor:!public! !
!JadeDebugger class categoriesFor: #debuggers!public! !
!JadeDebugger class categoriesFor: #openDebuggerOnException:!public! !
!JadeDebugger class categoriesFor: #openOn:message:terminateOnClose:!public! !
!JadeDebugger class categoriesFor: #reportError:!public! !
!JadeDebugger class categoriesFor: #resetDebuggers!public! !
!JadeDebugger class categoriesFor: #resource_Default_view!public!resources-views! !

