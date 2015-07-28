| package |
package := Package name: 'Object Log Browser'.
package paxVersion: 1;
	basicComment: 'Fix Compiler Warning'.

package basicPackageVersion: '0.021'.


package classNames
	add: #ObjectLogBrowser;
	add: #ObjectLogEntryViewer;
	yourself.

package methodNames
	add: #JadeServer -> #sbObjectLog:;
	add: #JadeTextDocument -> #jadeBrowseObjectLog;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'GemStone Objects';
	add: 'GemStone Session';
	add: 'Jade UI';
	add: 'Jade UI Base';
	yourself).

package!

"Class Definitions"!

Shell subclass: #ObjectLogBrowser
	instanceVariableNames: 'listPresenter fatalPresenter errorPresenter warningPresenter infoPresenter debugPresenter tracePresenter transcriptPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #ObjectLogEntryViewer
	instanceVariableNames: 'entryArray oopPresenter classNamePresenter pidPresenter stampPresenter labelPresenter priorityPresenter tagPresenter objectStringPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JadeServer methodsFor!

sbObjectLog: anOrderedCollection

	| command priorities class log debuggerLogEntryClass | 
	(class := self objectNamed: #'ObjectLogEntry') isNil ifTrue: [^self].
	debuggerLogEntryClass := self objectNamed: #'DebuggerLogEntry'.
	(command := anOrderedCollection removeFirst) = 'delete' ifTrue: [
		anOrderedCollection do: [:each | 			| oop entry |
			oop := each asNumber.			entry := class objectLog detect: [:each2 | (self oopOf: each2) = oop] ifNone: [nil].			entry notNil ifTrue: [class objectLog remove: entry].
		].
		^self systemBrowserCommand.
	].
	writeStream nextPutAll: 'objectLog'; lf.
	priorities := anOrderedCollection removeFirst asArray collect: [:each | each asString asNumber].
	log := class objectLog select: [:each | priorities includes: each priority].
	log reverseDo: [:each | 
		| labelString objectString |
		objectString := String withAll: (each objectString asArray collect: [:char | 
			char asciiValue < 32 ifTrue: [Character space] ifFalse: [
			127 < char asciiValue ifTrue: [$?] ifFalse: [char]]]).
		500 < objectString size ifTrue: [objectString := (objectString copyFrom: 1 to: 500) , '...'].
		each label = each object printString ifTrue: [
			labelString := ''.
		] ifFalse: [
			labelString := String withAll: (each labelString asArray collect: [:char | 
				char asciiValue < 32 ifTrue: [Character space] ifFalse: [
				127 < char asciiValue ifTrue: [$?] ifFalse: [char]]]).
			500 < labelString size ifTrue: [labelString := (labelString copyFrom: 1 to: 500) , '...'].
		].
"1"	(self oopOf: each) printOn: writeStream.
"2"	writeStream tab; nextPutAll: each class name; tab.
"3"	each pid printOn: writeStream. 
		writeStream tab.
"4"	each stamp rounded printOn: writeStream.
"5"	writeStream tab; nextPutAll: labelString; tab.
"6"	each priority printOn: writeStream.
		writeStream tab.
"7"	each tag printOn: writeStream.
"8"	writeStream tab; nextPutAll: objectString; tab.
		(debuggerLogEntryClass notNil and: [each isKindOf: debuggerLogEntryClass]) ifTrue: [
"9"		(self oopOf: each continuation) printOn: writeStream.
		] ifFalse: [
			writeStream nextPutAll: '0'.
		].
		writeStream lf.
	].
! !
!JadeServer categoriesFor: #sbObjectLog:!public!System Browser! !

!JadeTextDocument methodsFor!

jadeBrowseObjectLog

	gciSession hasServer ifTrue: [
		^ObjectLogBrowser showOn: gciSession.
	].
	MessageBox
		warning: 'Server initialization failed at login.'
		caption: 'Unable to Open Browser'.
! !
!JadeTextDocument categoriesFor: #jadeBrowseObjectLog!Jade!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

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

	self model commit ifTrue: [
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

	| oop |
	oop := model oopTypeWithOop: (anArray at: 9) asNumber.
	JadeDebugger 
		openOn: (GsProcess session: model oop: oop) 
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

logoutRequested: aValueHolder

	aValueHolder value: true.
!

model: aGciSession

	super model: aGciSession.
	model
		when: #'logoutRequested:'	send: #'logoutRequested:'	to: self;
		when: #'logoutPending'		send: #'exit'						to: self;
		yourself.
!

onViewClosed

	model notNil ifTrue: [
		| temp |
		temp := model.
		model := nil.
		temp removeEventsTriggeredFor: self.
	].
	super onViewClosed.
!

onViewOpened

	super onViewOpened.
	self caption: (self model titleBarFor: 'Object Log').
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

statusBarText: aString

	(self view viewNamed: 'statusBarField') model: (ValueHolder with: aString).
!

update 

	self updateA: self requestString.
!

updateA: aString

	| time1 time2 time3 string stream |
	time1 := Time millisecondsToRun: [
		string := model 
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

	ObjectLogEntryViewer showOn: model -> anArray.
! !
!ObjectLogBrowser categoriesFor: #commit!public! !
!ObjectLogBrowser categoriesFor: #commitTransaction!public! !
!ObjectLogBrowser categoriesFor: #createComponents!public! !
!ObjectLogBrowser categoriesFor: #createSchematicWiring!public! !
!ObjectLogBrowser categoriesFor: #debug:!public! !
!ObjectLogBrowser categoriesFor: #delete!public! !
!ObjectLogBrowser categoriesFor: #logoutRequested:!public! !
!ObjectLogBrowser categoriesFor: #model:!public! !
!ObjectLogBrowser categoriesFor: #onViewClosed!public! !
!ObjectLogBrowser categoriesFor: #onViewOpened!public! !
!ObjectLogBrowser categoriesFor: #open!public! !
!ObjectLogBrowser categoriesFor: #queryCommand:!public! !
!ObjectLogBrowser categoriesFor: #requestString!public! !
!ObjectLogBrowser categoriesFor: #statusBarText:!public! !
!ObjectLogBrowser categoriesFor: #update!public! !
!ObjectLogBrowser categoriesFor: #updateA:!public! !
!ObjectLogBrowser categoriesFor: #updateB:!public! !
!ObjectLogBrowser categoriesFor: #viewEntry:!public! !

!ObjectLogBrowser class methodsFor!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1601 1201 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 22 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 624 0 0 0 7 0 0 0 624 0 8 4294903251 1180998 4 ##(Smalltalk.CommandDescription)  8 #commit 8 'Commit' 1 1 0 0 32 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1269 1 530 141 43 624 850 8 #isEnabled: 98 1 32 624 850 8 #text: 98 1 8 'Commit' 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 122 2 0 0 0 0 0 0 192 2 0 0 21 0 0 0] 98 0 530 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentRight -299 1170 8 #fixedViewLeft 141 1170 8 #fixedParentTop 1 1170 8 #fixedViewTop 43 410 8 ##(Smalltalk.CheckBox)  98 16 0 416 98 2 8 1409363203 1 1312 721990 2 ##(Smalltalk.ValueHolder)  0 32 1114118 ##(Smalltalk.NeverSearchPolicy)  16 0 0 7 0 0 0 1312 0 8 4294903251 852486 ##(Smalltalk.NullConverter)  0 0 0 786 202 208 98 2 850 880 98 2 530 151 1 530 151 43 1312 850 1008 98 1 8 'Error' 1312 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 0 0 0 0 150 0 0 0 21 0 0 0] 98 0 1120 0 27 1138 1170 8 #fixedParentLeft 151 1216 151 1248 1 1280 43 410 640 98 20 0 416 98 2 8 1140924416 1 1760 0 0 0 7 0 0 0 1760 0 8 4294903251 722 8 #update 8 'Update' 1 1 0 0 16 0 0 0 786 202 208 98 2 850 880 98 2 530 1429 1 530 141 43 1760 850 1008 98 1 8 'Update' 1760 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 202 2 0 0 0 0 0 0 16 3 0 0 21 0 0 0] 98 0 1120 0 29 1138 1184 -139 1216 141 1248 1 1280 43 410 8 ##(Smalltalk.StatusBar)  98 18 0 416 98 2 8 1409288460 1 2112 0 482 8 4278190080 0 7 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 530 193 193 0 2112 0 8 4294902963 234 256 98 2 853766 ##(Smalltalk.StatusBarItem)  1 -1 2112 0 459270 ##(Smalltalk.Message)  8 #displayString 98 0 2402 8 #iconImageIndex 98 0 1049670 1 ##(Smalltalk.IconImageManager)  8 'statusBarField' 98 1 2384 1115142 ##(Smalltalk.StatusBarNullItem)  513 1 2112 0 0 786 202 208 98 1 850 880 98 2 530 1 1041 530 1569 45 2112 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 8 2 0 0 16 3 0 0 30 2 0 0] 98 0 1120 0 27 1138 1728 1 1184 1 1170 8 #fixedParentBottom -43 1280 45 410 1328 98 16 0 416 98 2 8 1409363203 1 2816 1394 0 32 1440 32 0 0 7 0 0 0 2816 0 8 4294903251 1474 0 0 0 786 202 208 98 2 850 880 98 2 530 451 1 530 151 43 2816 850 1008 98 1 8 'Info' 2816 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 225 0 0 0 0 0 0 0 44 1 0 0 21 0 0 0] 98 0 1120 0 27 1138 1728 451 1216 151 1248 1 1280 43 410 1328 98 16 0 416 98 2 8 1409363203 1 3152 1394 0 32 1440 16 0 0 7 0 0 0 3152 0 8 4294903251 1474 0 0 0 786 202 208 98 2 850 880 98 2 530 601 1 530 151 43 3152 850 1008 98 1 8 'Debug' 3152 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 1 0 0 0 0 0 0 119 1 0 0 21 0 0 0] 98 0 1120 0 27 1138 1728 601 1216 151 1248 1 1280 43 410 1328 98 16 0 416 98 2 8 1409363203 1 3488 1394 0 32 1440 16 0 0 7 0 0 0 3488 0 8 4294903251 1474 0 0 0 786 202 208 98 2 850 880 98 2 530 301 1 530 151 43 3488 850 1008 98 1 8 'Warning' 3488 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 0 0 0 0 225 0 0 0 21 0 0 0] 98 0 1120 0 27 1138 1728 301 1216 151 1248 1 1280 43 410 1328 98 16 0 416 98 2 8 1409363203 1 3824 1394 0 32 1440 16 0 0 7 0 0 0 3824 0 8 4294903251 1474 0 0 0 786 202 208 98 2 850 880 98 2 530 11 1 530 141 43 3824 850 1008 98 1 8 'Fatal' 3824 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 0 0 0 0 75 0 0 0 21 0 0 0] 98 0 1120 0 27 1138 1728 11 1216 141 1248 1 1280 43 410 1328 98 16 0 416 98 2 8 1409363203 1 4160 1394 0 0 1440 32 0 0 7 0 0 0 4160 0 8 4294903251 1474 0 0 0 786 202 208 98 2 850 880 98 2 530 901 1 530 151 43 4160 850 1008 98 1 8 'Transcript' 4160 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 194 1 0 0 0 0 0 0 13 2 0 0 21 0 0 0] 98 0 1120 0 27 1138 1728 901 1216 151 1248 1 1280 43 410 1328 98 16 0 416 98 2 8 1409363203 1 4496 1394 0 0 1440 32 0 0 7 0 0 0 4496 0 8 4294903251 1474 0 0 0 786 202 208 98 2 850 880 98 2 530 751 1 530 151 43 4496 850 1008 98 1 8 'Trace' 4496 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 1 0 0 0 0 0 0 194 1 0 0 21 0 0 0] 98 0 1120 0 27 1138 1728 751 1216 151 1248 1 1280 43 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1409355849 1025 4832 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 2 984134 2 ##(Smalltalk.CommandMenuItem)  1 722 8 #delete 8 '&Delete' 1629 1 0 0 0 5090 1 722 8 #debug 8 'Debu&g' 9353 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 4832 0 8 4294902983 2402 2432 98 0 0 2528 0 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 5296 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 5312 7 257 0 0 0 0 0 202 208 98 6 920646 5 ##(Smalltalk.ListViewColumn)  8 'Stamp' 351 8 #left 2402 2432 98 0 2402 8 #<= 5520 5298 0 0 5330 1 83886081 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 5568 7 257 0 0 4832 0 1 0 0 5442 8 'PID' 101 8 #right 2402 2432 5520 2402 5552 5520 5298 0 0 5330 1 83886081 5600 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 5744 7 257 0 0 4832 0 1 0 0 5442 8 'Label' 241 5488 2402 2432 5520 2402 5552 5520 5298 0 0 5330 1 83886081 5600 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 5888 7 257 0 0 4832 0 1 0 0 5442 8 'Type' 161 5488 2402 2432 5520 2402 5552 5520 5298 0 0 5330 3 1 5296 8 'doIt' 8 '[:each | #(''fatal'' ''error'' ''warn'' ''info'' ''debug'' ''trace'' ''transcript'') at: (each at: 6) asNumber]' 8 #[31 105 29 17 214 6 148 159 148 106] 98 7 8 'fatal' 8 'error' 8 'warn' 8 'info' 8 'debug' 8 'trace' 8 'transcript' 8 #asNumber 6032 7 257 0 0 4832 0 1 0 0 5442 8 'Tag' 101 5488 2402 2432 5520 2402 5552 5520 5298 0 0 5330 1 83886081 5600 8 'doIt' 8 '[:each | each at: 7]' 8 #[29 105 17 214 7 148 106] 6320 7 257 0 0 4832 0 1 0 0 5442 8 'Object' 689 5488 2402 2432 5520 2402 5552 5520 5298 0 0 5330 1 83886081 5296 8 'doIt' 8 '[:each | each at: 8]' 8 #[29 105 17 214 8 148 106] 6464 7 257 0 0 4832 0 3 0 0 8 #report 4960 0 133217 0 0 786 202 208 98 3 850 880 98 2 530 1 45 530 1569 997 4832 850 8 #contextMenu: 98 1 5056 4832 850 1008 98 1 8 'Stamp' 4832 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 22 0 0 0 16 3 0 0 8 2 0 0] 98 0 1120 0 27 1138 1728 1 1184 1 1248 45 2784 -43 234 256 98 18 4832 8 'objectLog' 3152 8 'debug' 3824 8 'fatal' 4496 8 'trace' 2816 8 'info' 4160 8 'transcript' 3488 8 'warning' 2112 8 'statusBar' 1312 8 'error' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 1 5042 0 16 98 2 5090 1 722 5136 8 '&Delete' 1629 1 0 0 0 5090 1 722 5200 8 'Debu&g' 9353 1 0 0 0 8 'Entry' 0 134217729 0 0 20083 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 10829 0 0 0 0 1 0 0 786 202 208 98 3 850 880 98 2 530 2879 21 530 1601 1201 416 850 1008 98 1 8 'Object Log' 416 850 8 #updateMenuBar 4960 416 1058 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 191 8 0 0 98 2 0 0] 98 11 3824 1312 3488 2816 3152 4496 4160 624 1760 4832 2112 1120 0 27 )! !
!ObjectLogBrowser class categoriesFor: #icon!public! !
!ObjectLogBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

ObjectLogEntryViewer guid: (GUID fromString: '{EA1914B4-9890-4A42-A223-F1E04095018F}')!
ObjectLogEntryViewer comment: ''!
!ObjectLogEntryViewer categoriesForClass!Unclassified! !
!ObjectLogEntryViewer methodsFor!

createComponents

	super createComponents.
	oopPresenter 				:= self add: TextPresenter new name: 'oop'.
	classNamePresenter 	:= self add: TextPresenter new name: 'className'.
	pidPresenter 				:= self add: TextPresenter new name: 'pid'.
	stampPresenter 			:= self add: TextPresenter new name: 'stamp'.
	labelPresenter 				:= self add: TextPresenter new name: 'label'.
	priorityPresenter 			:= self add: TextPresenter new name: 'priority'.
	tagPresenter 				:= self add: TextPresenter new name: 'tag'.
	objectStringPresenter 	:= self add: TextPresenter new name: 'objectString'.
!

logoutRequested: aValueHolder

	aValueHolder value: true.
!

model: anAssociation

	super model: anAssociation key.
	entryArray := anAssociation value.
	model
		when: #'logoutRequested:'	send: #'logoutRequested:'	to: self;
		when: #'logoutPending'		send: #'exit'						to: self;
		yourself.
!

onViewOpened

	super onViewOpened.
	self caption: (self model titleBarFor: 'Object Log Entry').
	oopPresenter value: (entryArray at: 1).
	classNamePresenter value: (entryArray at: 2).
	pidPresenter value: (entryArray at: 3).
	stampPresenter value: (entryArray at: 4).
	labelPresenter value: (entryArray at: 5).
	priorityPresenter value: (entryArray at: 6).
	tagPresenter value: (entryArray at: 7).
	objectStringPresenter value: (entryArray at: 8).
	! !
!ObjectLogEntryViewer categoriesFor: #createComponents!public! !
!ObjectLogEntryViewer categoriesFor: #logoutRequested:!public! !
!ObjectLogEntryViewer categoriesFor: #model:!public! !
!ObjectLogEntryViewer categoriesFor: #onViewOpened!public! !

!ObjectLogEntryViewer class methodsFor!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1031 681 551 0 0 0 416 0 234 256 98 16 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 592 0 482 8 4278190080 0 7 0 0 0 592 0 8 4294903553 852486 ##(Smalltalk.NullConverter)  0 0 3 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 801 131 530 181 41 592 818 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 592 818 8 #isTextModified: 98 1 32 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 144 1 0 0 65 0 0 0 234 1 0 0 85 0 0 0] 98 0 530 193 193 0 27 8 'tag' 410 608 98 16 0 416 98 2 8 1140916352 1025 1136 0 482 688 0 7 0 0 0 1136 0 8 4294903553 722 0 0 3 754 202 208 98 3 818 848 98 2 530 171 91 530 351 41 1136 818 928 98 1 962 3 1 3 1136 818 1008 98 1 32 1136 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 85 0 0 0 45 0 0 0 4 1 0 0 65 0 0 0] 98 0 1104 0 27 8 'label' 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 416 98 2 8 1143017796 1025 1504 0 482 8 4278190080 0 7 0 0 0 1504 0 8 4294903553 722 0 0 11 754 202 208 98 3 818 848 98 2 530 171 171 530 811 421 1504 818 928 98 1 962 3 1 3 1504 818 1008 98 1 32 1504 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 85 0 0 0 85 0 0 0 234 1 0 0 39 1 0 0] 98 0 1104 0 27 8 'objectString' 410 608 98 16 0 416 98 2 8 1140916354 1025 1904 0 482 688 0 7 0 0 0 1904 0 8 4294903553 722 0 0 3 754 202 208 98 3 818 848 98 2 530 801 91 530 181 41 1904 818 928 98 1 962 3 1 3 1904 818 1008 98 1 32 1904 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 144 1 0 0 45 0 0 0 234 1 0 0 65 0 0 0] 98 0 1104 0 27 8 'priority' 410 608 98 16 0 416 98 2 8 1140916354 1025 2272 0 482 688 0 7 0 0 0 2272 0 8 4294903553 722 0 0 3 754 202 208 98 3 818 848 98 2 530 801 11 530 181 41 2272 818 928 98 1 962 3 1 3 2272 818 1008 98 1 32 2272 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 144 1 0 0 5 0 0 0 234 1 0 0 25 0 0 0] 98 0 1104 0 27 8 'oop' 410 608 98 16 0 416 98 2 8 1140916354 1025 2640 0 482 688 0 7 0 0 0 2640 0 8 4294903553 722 0 0 3 754 202 208 98 3 818 848 98 2 530 801 51 530 181 41 2640 818 928 98 1 962 3 1 3 2640 818 1008 98 1 32 2640 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 144 1 0 0 25 0 0 0 234 1 0 0 45 0 0 0] 98 0 1104 0 27 8 'pid' 410 608 98 16 0 416 98 2 8 1140916352 1025 3008 0 482 688 0 7 0 0 0 3008 0 8 4294903553 722 0 0 3 754 202 208 98 3 818 848 98 2 530 171 11 530 351 41 3008 818 928 98 1 962 3 1 3 3008 818 1008 98 1 32 3008 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 85 0 0 0 5 0 0 0 4 1 0 0 25 0 0 0] 98 0 1104 0 27 8 'className' 410 608 98 16 0 416 98 2 8 1140916352 1025 3376 0 482 688 0 7 0 0 0 3376 0 8 4294903553 722 0 0 3 754 202 208 98 3 818 848 98 2 530 171 51 530 351 41 3376 818 928 98 1 962 3 1 3 3376 818 1008 98 1 32 3376 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 85 0 0 0 25 0 0 0 4 1 0 0 45 0 0 0] 98 0 1104 0 27 8 'stamp' 0 0 0 0 0 1 0 0 0 0 1 0 0 754 202 208 98 2 818 848 98 2 530 2879 21 530 1031 681 416 818 8 #updateMenuBar 98 0 416 1042 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 162 7 0 0 94 1 0 0] 98 16 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 3952 0 0 0 7 0 0 0 3952 0 8 4294902987 722 0 0 0 754 202 208 98 2 818 848 98 2 530 541 11 530 241 39 3952 818 8 #text: 98 1 8 'ObjectLogEntry OOP:' 3952 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 1 0 0 5 0 0 0 134 1 0 0 24 0 0 0] 98 0 1104 0 27 410 3968 98 16 0 416 98 2 8 1140850944 1 4288 0 0 0 7 0 0 0 4288 0 8 4294902987 722 0 0 0 754 202 208 98 2 818 848 98 2 530 11 11 530 161 39 4288 818 4192 98 1 8 'Class Name:' 4288 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 85 0 0 0 24 0 0 0] 98 0 1104 0 27 410 3968 98 16 0 416 98 2 8 1140850944 1 4592 0 0 0 7 0 0 0 4592 0 8 4294902987 722 0 0 0 754 202 208 98 2 818 848 98 2 530 541 51 530 241 39 4592 818 4192 98 1 8 'Gem Session PID:' 4592 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 1 0 0 25 0 0 0 134 1 0 0 44 0 0 0] 98 0 1104 0 27 410 3968 98 16 0 416 98 2 8 1140850944 1 4896 0 0 0 7 0 0 0 4896 0 8 4294902987 722 0 0 0 754 202 208 98 2 818 848 98 2 530 11 51 530 161 39 4896 818 4192 98 1 8 'Timestamp:' 4896 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 25 0 0 0 85 0 0 0 44 0 0 0] 98 0 1104 0 27 410 3968 98 16 0 416 98 2 8 1140850944 1 5200 0 0 0 7 0 0 0 5200 0 8 4294902987 722 0 0 0 754 202 208 98 2 818 848 98 2 530 11 91 530 161 39 5200 818 4192 98 1 8 'Label:' 5200 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 45 0 0 0 85 0 0 0 64 0 0 0] 98 0 1104 0 27 410 3968 98 16 0 416 98 2 8 1140850944 1 5504 0 0 0 7 0 0 0 5504 0 8 4294902987 722 0 0 0 754 202 208 98 2 818 848 98 2 530 541 91 530 241 39 5504 818 4192 98 1 8 'Priority:' 5504 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 1 0 0 45 0 0 0 134 1 0 0 64 0 0 0] 98 0 1104 0 27 410 3968 98 16 0 416 98 2 8 1140850944 1 5808 0 0 0 7 0 0 0 5808 0 8 4294902987 722 0 0 0 754 202 208 98 2 818 848 98 2 530 541 131 530 241 39 5808 818 4192 98 1 8 'Tag:' 5808 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 1 0 0 65 0 0 0 134 1 0 0 84 0 0 0] 98 0 1104 0 27 410 3968 98 16 0 416 98 2 8 1140850944 1 6112 0 0 0 7 0 0 0 6112 0 8 4294902987 722 0 0 0 754 202 208 98 2 818 848 98 2 530 11 171 530 161 41 6112 818 4192 98 1 8 'Object String:' 6112 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 85 0 0 0 85 0 0 0 105 0 0 0] 98 0 1104 0 27 3008 2272 3376 2640 1136 1904 592 1504 1104 0 27 )! !
!ObjectLogEntryViewer class categoriesFor: #icon!public! !
!ObjectLogEntryViewer class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

