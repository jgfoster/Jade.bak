"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #ObjectLogBrowser
	instanceVariableNames: 'listPresenter fatalPresenter errorPresenter warningPresenter infoPresenter debugPresenter tracePresenter transcriptPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ObjectLogBrowser guid: (GUID fromString: '{D6406BCA-C1B0-4A7B-81CF-66AC7AB28E63}')!
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
		string := gciSession 
			serverPerform: #'systemBrowser:' 
			with: aString.
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1601 1201 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 22 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 624 0 0 0 7 0 0 0 624 0 8 4294903251 1180998 4 ##(Smalltalk.CommandDescription)  8 #commit 8 'Commit' 1 1 0 0 32 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1269 1 530 141 43 624 850 8 #isEnabled: 98 1 32 624 850 8 #text: 98 1 8 'Commit' 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 122 2 0 0 0 0 0 0 192 2 0 0 21 0 0 0] 98 0 530 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentRight -299 1170 8 #fixedViewLeft 141 1170 8 #fixedParentTop 1 1170 8 #fixedViewTop 43 410 8 ##(Smalltalk.CheckBox)  98 16 0 416 98 2 8 1409363203 1 1312 721990 2 ##(Smalltalk.ValueHolder)  0 32 1114118 ##(Smalltalk.NeverSearchPolicy)  16 0 0 7 0 0 0 1312 0 8 4294903251 852486 ##(Smalltalk.NullConverter)  0 0 0 786 202 208 98 2 850 880 98 2 530 151 1 530 151 43 1312 850 1008 98 1 8 'Error' 1312 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 0 0 0 0 150 0 0 0 21 0 0 0] 98 0 1120 0 27 1138 1170 8 #fixedParentLeft 151 1216 151 1248 1 1280 43 410 640 98 20 0 416 98 2 8 1140924416 1 1760 0 0 0 7 0 0 0 1760 0 8 4294903251 722 8 #update 8 'Update' 1 1 0 0 16 0 0 0 786 202 208 98 2 850 880 98 2 530 1429 1 530 141 43 1760 850 1008 98 1 8 'Update' 1760 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 202 2 0 0 0 0 0 0 16 3 0 0 21 0 0 0] 98 0 1120 0 29 1138 1184 -139 1216 141 1248 1 1280 43 410 8 ##(Smalltalk.StatusBar)  98 18 0 416 98 2 8 1409288460 1 2112 0 482 8 4278190080 0 7 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 530 193 193 0 2112 0 8 4294902963 234 256 98 2 853766 ##(Smalltalk.StatusBarItem)  1 -1 2112 0 459270 ##(Smalltalk.Message)  8 #displayString 98 0 2402 8 #iconImageIndex 98 0 1049670 1 ##(Smalltalk.IconImageManager)  8 'statusBarField' 98 1 2384 1115142 ##(Smalltalk.StatusBarNullItem)  513 1 2112 0 0 786 202 208 98 1 850 880 98 2 530 1 1041 530 1569 45 2112 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 8 2 0 0 16 3 0 0 30 2 0 0] 98 0 1120 0 27 1138 1728 1 1184 1 1170 8 #fixedParentBottom -43 1280 45 410 1328 98 16 0 416 98 2 8 1409363203 1 2816 1394 0 32 1440 32 0 0 7 0 0 0 2816 0 8 4294903251 1474 0 0 0 786 202 208 98 2 850 880 98 2 530 451 1 530 151 43 2816 850 1008 98 1 8 'Info' 2816 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 225 0 0 0 0 0 0 0 44 1 0 0 21 0 0 0] 98 0 1120 0 27 1138 1728 451 1216 151 1248 1 1280 43 410 1328 98 16 0 416 98 2 8 1409363203 1 3152 1394 0 32 1440 16 0 0 7 0 0 0 3152 0 8 4294903251 1474 0 0 0 786 202 208 98 2 850 880 98 2 530 601 1 530 151 43 3152 850 1008 98 1 8 'Debug' 3152 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 1 0 0 0 0 0 0 119 1 0 0 21 0 0 0] 98 0 1120 0 27 1138 1728 601 1216 151 1248 1 1280 43 410 1328 98 16 0 416 98 2 8 1409363203 1 3488 1394 0 32 1440 16 0 0 7 0 0 0 3488 0 8 4294903251 1474 0 0 0 786 202 208 98 2 850 880 98 2 530 301 1 530 151 43 3488 850 1008 98 1 8 'Warning' 3488 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 0 0 0 0 225 0 0 0 21 0 0 0] 98 0 1120 0 27 1138 1728 301 1216 151 1248 1 1280 43 410 1328 98 16 0 416 98 2 8 1409363203 1 3824 1394 0 32 1440 16 0 0 7 0 0 0 3824 0 8 4294903251 1474 0 0 0 786 202 208 98 2 850 880 98 2 530 11 1 530 141 43 3824 850 1008 98 1 8 'Fatal' 3824 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 0 0 0 0 75 0 0 0 21 0 0 0] 98 0 1120 0 27 1138 1728 11 1216 141 1248 1 1280 43 410 1328 98 16 0 416 98 2 8 1409363203 1 4160 1394 0 0 1440 32 0 0 7 0 0 0 4160 0 8 4294903251 1474 0 0 0 786 202 208 98 2 850 880 98 2 530 901 1 530 151 43 4160 850 1008 98 1 8 'Transcript' 4160 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 194 1 0 0 0 0 0 0 13 2 0 0 21 0 0 0] 98 0 1120 0 27 1138 1728 901 1216 151 1248 1 1280 43 410 1328 98 16 0 416 98 2 8 1409363203 1 4496 1394 0 0 1440 32 0 0 7 0 0 0 4496 0 8 4294903251 1474 0 0 0 786 202 208 98 2 850 880 98 2 530 751 1 530 151 43 4496 850 1008 98 1 8 'Trace' 4496 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 1 0 0 0 0 0 0 194 1 0 0 21 0 0 0] 98 0 1120 0 27 1138 1728 751 1216 151 1248 1 1280 43 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1409355849 1025 4832 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 2 984134 2 ##(Smalltalk.CommandMenuItem)  1 722 8 #delete 8 '&Delete' 1629 1 0 0 0 5090 1 722 8 #debug 8 'Debu&g' 9353 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 4832 0 8 4294902983 2402 2432 98 0 0 2528 0 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 5296 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 5312 7 257 0 0 0 0 0 202 208 98 6 920646 5 ##(Smalltalk.ListViewColumn)  8 'Stamp' 351 8 #left 2402 2432 98 0 2402 8 #<= 5520 5298 0 0 5330 1 83886081 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 5568 7 257 0 0 4832 0 1 0 0 5442 8 'PID' 101 8 #right 2402 2432 5520 2402 5552 5520 5298 0 0 5330 1 83886081 5600 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 5744 7 257 0 0 4832 0 1 0 0 5442 8 'Label' 241 5488 2402 2432 5520 2402 5552 5520 5298 0 0 5330 1 83886081 5600 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 5888 7 257 0 0 4832 0 1 0 0 5442 8 'Type' 161 5488 2402 2432 5520 2402 5552 5520 5298 0 0 5330 3 1 5296 8 'doIt' 8 '[:each | #(''fatal'' ''error'' ''warn'' ''info'' ''debug'' ''trace'' ''transcript'') at: (each at: 6) asNumber]' 8 #[31 105 29 17 214 6 148 159 148 106] 98 7 8 'fatal' 8 'error' 8 'warn' 8 'info' 8 'debug' 8 'trace' 8 'transcript' 8 #asNumber 6032 7 257 0 0 4832 0 1 0 0 5442 8 'Tag' 101 5488 2402 2432 5520 2402 5552 5520 5298 0 0 5330 1 83886081 5600 8 'doIt' 8 '[:each | each at: 7]' 8 #[29 105 17 214 7 148 106] 6320 7 257 0 0 4832 0 1 0 0 5442 8 'Object' 689 5488 2402 2432 5520 2402 5552 5520 5298 0 0 5330 1 83886081 5296 8 'doIt' 8 '[:each | each at: 8]' 8 #[29 105 17 214 8 148 106] 6464 7 257 0 0 4832 0 3 0 0 8 #report 4960 0 133217 0 0 786 202 208 98 3 850 880 98 2 530 1 45 530 1569 997 4832 850 8 #contextMenu: 98 1 5056 4832 850 1008 98 1 8 'Stamp' 4832 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 22 0 0 0 16 3 0 0 8 2 0 0] 98 0 1120 0 27 1138 1728 1 1184 1 1248 45 2784 -43 234 256 98 18 4832 8 'objectLog' 3152 8 'debug' 3824 8 'fatal' 4496 8 'trace' 2816 8 'info' 4160 8 'transcript' 3488 8 'warning' 2112 8 'statusBar' 1312 8 'error' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 1 5042 0 16 98 2 5090 1 722 5136 8 '&Delete' 1629 1 0 0 0 5090 1 722 5200 8 'Debu&g' 9353 1 0 0 0 8 'Entry' 0 134217729 0 0 20083 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 10829 0 0 0 0 1 0 0 786 202 208 98 3 850 880 98 2 530 2879 21 530 1601 1201 416 850 1008 98 1 8 'Object Log' 416 850 8 #updateMenuBar 4960 416 1058 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 191 8 0 0 98 2 0 0] 98 11 3824 1312 3488 2816 3152 4496 4160 624 1760 4832 2112 1120 0 27 )! !
!ObjectLogBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

