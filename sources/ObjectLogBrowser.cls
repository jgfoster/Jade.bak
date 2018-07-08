"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #ObjectLogBrowser
	instanceVariableNames: 'listPresenter fatalPresenter errorPresenter warningPresenter infoPresenter debugPresenter tracePresenter transcriptPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ObjectLogBrowser guid: (GUID fromString: '{d6406bca-c1b0-4a7b-81cf-66ac7ab28e63}')!
ObjectLogBrowser comment: ''!
!ObjectLogBrowser categoriesForClass!Unclassified! !
!ObjectLogBrowser methodsFor!

commit

	self 
		commitTransaction;
		update;
		yourself.
 !

commitTransaction

	gciSession commit ifTrue: [
		Sound informationBeep.
		self update.
	] ifFalse: [
		MessageBox warning: 'Commit failed!!'.
	].	self update.
!

createComponents

	super createComponents.
	listPresenter 				:= self add: ListPresenter 		new name: 'objectLog'.
	fatalPresenter 			:= self add: ValuePresenter 	new name: 'fatal'.
	errorPresenter 			:= self add: ValuePresenter 	new name: 'error'.
	warningPresenter 		:= self add: ValuePresenter	new name: 'warning'.
	infoPresenter 			:= self add: ValuePresenter 	new name: 'info'.
	debugPresenter 		:= self add: ValuePresenter 	new name: 'debug'.
	tracePresenter 			:= self add: ValuePresenter 	new name: 'trace'.
	transcriptPresenter 	:= self add: ValuePresenter 	new name: 'transcript'.
!

createSchematicWiring

	super createSchematicWiring.
	listPresenter	when: #'actionPerformed' send: #'open' to: self.!

debug: anArray

	| oopType |
	oopType := gciSession oopTypeWithOop: (anArray at: 9) asNumber.
	JadeDebugger 
		openOn: (GsProcess session: gciSession oopType: oopType) 
		message: (anArray at: 8)
		terminateOnClose: false.
!

delete

	| stream |
	stream := (WriteStream on: String new)
		nextPutAll: 'objectLog'; tab;
		nextPutAll: 'delete'; tab;
		yourself.
	listPresenter selections do: [:each | 
		stream nextPutAll: (each at: 1); tab.
	].
	stream lf; nextPutAll: self requestString.
	self updateA: stream contents.
!

onViewOpened

	super onViewOpened.
	fatalPresenter 			value: true.
	errorPresenter  		value: true.
	warningPresenter  	value: true.
	infoPresenter  			value: false.
	debugPresenter  		value: true.
	tracePresenter  		value: false.
	transcriptPresenter  	value: true.
	self update.
!

open

	listPresenter selections do: [:each | 
		(each at: 9) = '0' ifTrue: [
			self viewEntry: each.
		] ifFalse: [
			self debug: each.
		].
	].
!

queryCommand: aCommandQuery

	(aCommandQuery commandSymbol = #'debug') ifTrue: [
		aCommandQuery isEnabled: (listPresenter selections size = 1 and: [(listPresenter selection at: 2) = 'WAObjectLogEntry']).
		^true.
	].
	(aCommandQuery commandSymbol = #'delete') ifTrue: [
		aCommandQuery isEnabled: listPresenter hasSelection.
		^true.
	].
	^super queryCommand: aCommandQuery.
!

requestString 

	| stream |
	stream := (WriteStream on: String new)
		nextPutAll: 'objectLog'; tab;
		nextPutAll: 'list'; tab;
		yourself.
	fatalPresenter 			value ifTrue: [stream nextPut: $1].
	errorPresenter  		value ifTrue: [stream nextPut: $2].
	warningPresenter		value ifTrue: [stream nextPut: $3].
	infoPresenter  			value ifTrue: [stream nextPut: $4].
	debugPresenter  		value ifTrue: [stream nextPut: $5].
	tracePresenter  		value ifTrue: [stream nextPut: $6].
	transcriptPresenter  	value ifTrue: [stream nextPut: $7].
	stream tab.
	^stream contents.
!

shellName 

	^'Object Log'
!

statusBarText: aString

	(self view viewNamed: 'statusBarField') model: (ValueHolder with: aString).
!

update 

	self updateA: self requestString.
!

updateA: aString

	| time1 time2 time3 string stream |
	time1 := Time millisecondsToRun: [
		string := gciSession encode: '0 ' , aString.
		string := gciSession 
			serverPerform: #'systemBrowser:' 
			with: string.
		string := gciSession decode: string.
	].
	time2 := Time millisecondsToRun: [
		| x |
		stream := ReadStream on: string.
		time3 := stream nextLine asNumber.
		(x := stream nextLine) isEmpty ifTrue: [
			MessageBox notify: 'ObjectLogEntry class not found!!'.
			[
				self view close.
			] forkAt: Processor userBackgroundPriority.
			^self.
		].
		x = 'objectLog' ifFalse: [self error: 'unexpected response: ' , x printString].
		self updateB: stream.
	].

	self statusBarText:
		'Found ' , listPresenter list size printString , ' entries; ' , 
		'server took ' , time3 printString , 'ms; ' , 
		'network took ' , (time1 - time3) printString , 'ms; ' , 
		'client took ' , time2 printString , 'ms; ' , 
		'total of ' , (time1 + time2) printString , 'ms'.

!

updateB: stream
		"oop class pid stamp label priority tag object"
	| list |
	list := OrderedCollection new.
	[
		stream atEnd not.
	] whileTrue: [
		| array |
		array := (stream nextLine subStrings: Character tab) collect: [:each | each = 'nil' ifTrue: [''] ifFalse: [each]].
		array size < 8 ifTrue: [self error: 'Not enough data from the server!!'. array := array , #('' '' '' '' '' '' '' '' '')].
		list add: array.
	].
	listPresenter list: list.
!

viewEntry: anArray

	ObjectLogEntryViewer showOn: gciSession -> anArray.
! !
!ObjectLogBrowser categoriesFor: #commit!public! !
!ObjectLogBrowser categoriesFor: #commitTransaction!public! !
!ObjectLogBrowser categoriesFor: #createComponents!public! !
!ObjectLogBrowser categoriesFor: #createSchematicWiring!public! !
!ObjectLogBrowser categoriesFor: #debug:!public! !
!ObjectLogBrowser categoriesFor: #delete!public! !
!ObjectLogBrowser categoriesFor: #onViewOpened!public! !
!ObjectLogBrowser categoriesFor: #open!public! !
!ObjectLogBrowser categoriesFor: #queryCommand:!public! !
!ObjectLogBrowser categoriesFor: #requestString!public! !
!ObjectLogBrowser categoriesFor: #shellName!public! !
!ObjectLogBrowser categoriesFor: #statusBarText:!public! !
!ObjectLogBrowser categoriesFor: #update!public! !
!ObjectLogBrowser categoriesFor: #updateA:!public! !
!ObjectLogBrowser categoriesFor: #updateB:!public! !
!ObjectLogBrowser categoriesFor: #viewEntry:!public! !

!ObjectLogBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13565952 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 22 410 ##(Smalltalk.CheckBox) 34 16 0 416 34 2 8 1409363203 1 608 721990 2 ##(Smalltalk.ValueHolder) 0 false 1114118 ##(Smalltalk.NeverSearchPolicy) true 0 0 7 0 0 0 608 0 8 4294903353 852486 ##(Smalltalk.NullConverter) 0 0 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 151 1 514 151 43 608 850 #text: 34 1 8 'Error' 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 0 0 0 0 150 0 0 0 21 0 0 0] 8 #() 514 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 151 1090 #fixedViewLeft 151 1090 #fixedParentTop 1 1090 #fixedViewTop 43 410 ##(Smalltalk.CheckBox) 34 16 0 416 34 2 8 1409363203 1 1168 674 0 0 720 false 0 0 7 0 0 0 1168 0 8 4294903353 754 0 0 0 786 138 144 34 2 850 #createAt:extent: 34 2 514 751 1 514 151 43 1168 850 #text: 34 1 8 'Trace' 1168 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 1 0 0 0 0 0 0 194 1 0 0 21 0 0 0] 8 #() 1040 0 27 1058 1104 751 1120 151 1136 1 1152 43 410 ##(Smalltalk.PushButton) 34 20 0 416 34 2 8 1140924416 1 1504 0 0 0 7 0 0 0 1504 0 8 4294903353 1180998 4 ##(Smalltalk.CommandDescription) #commit 8 'Commit' 1 1 0 0 false 0 0 0 786 138 144 34 3 850 #createAt:extent: 34 2 514 1269 1 514 141 43 1504 850 #isEnabled: 8 #(false) 1504 850 #text: 34 1 8 'Commit' 1504 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 122 2 0 0 0 0 0 0 192 2 0 0 21 0 0 0] 8 #() 1040 0 29 1058 1090 #fixedParentRight -299 1120 141 1136 1 1152 43 410 ##(Smalltalk.CheckBox) 34 16 0 416 34 2 8 1409363203 1 1904 674 0 false 720 true 0 0 7 0 0 0 1904 0 8 4294903353 754 0 0 0 786 138 144 34 2 850 #createAt:extent: 34 2 514 601 1 514 151 43 1904 850 #text: 34 1 8 'Debug' 1904 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 1 0 0 0 0 0 0 119 1 0 0 21 0 0 0] 8 #() 1040 0 27 1058 1104 601 1120 151 1136 1 1152 43 410 ##(Smalltalk.CheckBox) 34 16 0 416 34 2 8 1409363203 1 2240 674 0 false 720 true 0 0 7 0 0 0 2240 0 8 4294903353 754 0 0 0 786 138 144 34 2 850 #createAt:extent: 34 2 514 301 1 514 151 43 2240 850 #text: 34 1 8 'Warning' 2240 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 0 0 0 0 225 0 0 0 21 0 0 0] 8 #() 1040 0 27 1058 1104 301 1120 151 1136 1 1152 43 410 ##(Smalltalk.StatusBar) 34 18 0 416 34 2 8 1409288460 1 2576 0 466 8 4278190080 0 7 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 0 2576 0 8 4294903107 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 2576 0 459270 ##(Smalltalk.Message) #displayString 8 #() 2850 #iconImageIndex 8 #() 1049926 1 ##(Smalltalk.IconImageManager) 8 'statusBarField' 34 1 2832 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 2576 0 0 786 138 144 34 1 850 #createAt:extent: 34 2 514 1 1039 514 1569 45 2576 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 7 2 0 0 16 3 0 0 29 2 0 0] 8 #() 1040 0 27 1058 1104 1 1888 1 1090 #fixedParentBottom -43 1152 45 410 ##(Smalltalk.CheckBox) 34 16 0 416 34 2 8 1409363203 1 3216 674 0 0 720 false 0 0 7 0 0 0 3216 0 8 4294903353 754 0 0 0 786 138 144 34 2 850 #createAt:extent: 34 2 514 905 5 514 181 39 3216 850 #text: 34 1 8 'Transcript' 3216 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 196 1 0 0 2 0 0 0 30 2 0 0 21 0 0 0] 8 #() 1040 0 27 1058 1104 905 1120 181 1136 5 1152 39 410 ##(Smalltalk.ListView) 34 30 0 416 34 2 8 1409355849 1025 3552 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu) 0 true 34 2 984134 2 ##(Smalltalk.CommandMenuItem) 1 1586 #delete 8 '&Delete' 1629 1 0 0 0 3794 1 1586 #open 8 'Debu&g' 9353 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 3552 0 8 4294903097 2850 #displayString 8 #() 0 2944 0 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 1 83886081 3968 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 3984 7 257 0 0 0 0 0 138 144 34 6 920646 5 ##(Smalltalk.ListViewColumn) 8 'Stamp' 351 #left 2850 #displayString 8 #() 2850 #<= 4176 3970 0 0 4002 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 4208 7 257 0 0 3552 0 1 0 0 4114 8 'PID' 101 #right 2850 #displayString 4176 2850 #<= 4176 3970 0 0 4002 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 4352 7 257 0 0 3552 0 1 0 0 4114 8 'Label' 241 #left 2850 #displayString 4176 2850 #<= 4176 3970 0 0 4002 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 4496 7 257 0 0 3552 0 1 0 0 4114 8 'Type' 161 #left 2850 #displayString 4176 2850 #<= 4176 3970 0 0 4002 3 1 3968 8 'doIt' 8 '[:each | #(''fatal'' ''error'' ''warn'' ''info'' ''debug'' ''trace'' ''transcript'') at: (each at: 6) asNumber]' 8 #[31 105 29 17 214 6 148 159 148 106] 34 7 8 'fatal' 8 'error' 8 'warn' 8 'info' 8 'debug' 8 'trace' 8 'transcript' #asNumber 4640 7 257 0 0 3552 0 1 0 0 4114 8 'Tag' 101 #left 2850 #displayString 4176 2850 #<= 4176 3970 0 0 4002 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 7]' 8 #[29 105 17 214 7 148 106] 4912 7 257 0 0 3552 0 1 0 0 4114 8 'Object' 611 #left 2850 #displayString 4176 2850 #<= 4176 3970 0 0 4002 1 83886081 3968 8 'doIt' 8 '[:each | each at: 8]' 8 #[29 105 17 214 8 148 106] 5056 7 257 0 0 3552 0 3 0 0 #report 3664 0 133217 0 34 4 0 0 514 1 1 0 786 138 144 34 3 850 #createAt:extent: 34 2 514 1 45 514 1569 995 3552 850 #contextMenu: 34 1 3760 3552 850 #text: 34 1 8 'Stamp' 3552 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 22 0 0 0 16 3 0 0 7 2 0 0] 8 #() 1040 0 27 1058 1104 1 1888 1 1136 45 3200 -43 410 ##(Smalltalk.PushButton) 34 20 0 416 34 2 8 1140924416 1 5424 0 0 0 7 0 0 0 5424 0 8 4294903353 1586 #update 8 'Update' 1 1 0 0 true 0 0 0 786 138 144 34 2 850 #createAt:extent: 34 2 514 1429 1 514 141 43 5424 850 #text: 34 1 8 'Update' 5424 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 202 2 0 0 0 0 0 0 16 3 0 0 21 0 0 0] 8 #() 1040 0 29 1058 1888 -139 1120 141 1136 1 1152 43 410 ##(Smalltalk.CheckBox) 34 16 0 416 34 2 8 1409363203 1 5760 674 0 false 720 false 0 0 7 0 0 0 5760 0 8 4294903353 754 0 0 0 786 138 144 34 2 850 #createAt:extent: 34 2 514 451 1 514 151 43 5760 850 #text: 34 1 8 'Info' 5760 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 225 0 0 0 0 0 0 0 44 1 0 0 21 0 0 0] 8 #() 1040 0 27 1058 1104 451 1120 151 1136 1 1152 43 410 ##(Smalltalk.CheckBox) 34 16 0 416 34 2 8 1409363203 1 6096 674 0 false 720 true 0 0 7 0 0 0 6096 0 8 4294903353 754 0 0 0 786 138 144 34 2 850 #createAt:extent: 34 2 514 11 1 514 141 43 6096 850 #text: 34 1 8 'Fatal' 6096 978 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 0 0 0 0 75 0 0 0 21 0 0 0] 8 #() 1040 0 27 1058 1104 11 1120 141 1136 1 1152 43 170 192 34 18 1168 8 'trace' 608 8 'error' 1904 8 'debug' 3216 8 'transcript' 3552 8 'objectLog' 2240 8 'warning' 2576 8 'statusBar' 5760 8 'info' 6096 8 'fatal' 0 461638 4 ##(Smalltalk.MenuBar) 0 true 34 1 3746 0 true 34 2 3794 1 1586 #delete 8 '&Delete' 1629 1 0 0 0 3794 1 1586 #debug 8 'Debu&g' 9353 1 0 0 0 8 'Entry' 0 134217729 0 0 13783 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 10179 0 0 0 0 1 0 0 786 138 144 34 3 850 #createAt:extent: 34 2 514 5119 21 514 1601 1201 416 850 #text: 34 1 8 'Object Log' 416 850 #updateMenuBar 3664 416 978 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 31 13 0 0 98 2 0 0] 34 11 6096 608 2240 5760 1904 1168 3216 1504 5424 3552 2576 1040 0 27 )! !
!ObjectLogBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

