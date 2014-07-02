| package |
package := Package name: 'Jade Process Browser'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.006'.


package classNames
	add: #JadeProcessBrowser;
	yourself.

package methodNames
	add: #JadeServer -> #addProcess:to:withStatus:scheduler:;
	add: #JadeServer -> #processes;
	add: #JadeTranscript -> #browseProcesses;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'GemStone Session';
	add: 'Jade Transcript';
	yourself).

package!

"Class Definitions"!

Shell subclass: #JadeProcessBrowser
	instanceVariableNames: 'prioritiesPresenter processesPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JadeServer methodsFor!

addProcess: aProcess to: aStream withStatus: aString scheduler: aScheduler

	| x |
	aStream lf
"1"	nextPutAll: aString; tab;
"2"	nextPutAll: aProcess asOop printString; tab;
"3"	nextPutAll: aProcess priority printString; tab;
"4"	nextPutAll: (aProcess createdByApplication 	ifTrue: ['Y'] ifFalse: ['']); tab; 
"5"	nextPutAll: ((x := aProcess stackId) == -1 	ifTrue: [''] ifFalse: [x printString]); tab;
"6"	nextPutAll: ((x := aProcess waitingOn) 			ifNil: [''] ifNotNil: [x asOop printString]); tab;
"7"	nextPutAll: ((x := aProcess _signalTime) 		ifNil: [''] ifNotNil: [(x - aScheduler _now) printString]); tab;
"8"	nextPutAll: (aProcess isPartialContinuation		ifTrue: ['partial'] ifFalse: [aProcess isContinuation ifTrue: ['full'] ifFalse: ['']]); tab;
		yourself.
!

processes

	| scheduler stream |
	scheduler := (self objectNamed: 'ProcessorScheduler') scheduler.
	stream := (WriteStream on: String new)
		nextPutAll: 'highestPriority'; 						space; nextPutAll: scheduler highestPriority 					printString; tab;
		nextPutAll: 'highIOPriority'; 						space; nextPutAll: scheduler highIOPriority 					printString; tab;
		nextPutAll: 'lowestPriority'; 						space; nextPutAll: scheduler lowestPriority 					printString; tab;
		nextPutAll: 'lowIOPriority'; 						space; nextPutAll: scheduler lowIOPriority 					printString; tab;
		nextPutAll: 'systemBackgroundPriority'; 	space; nextPutAll: scheduler systemBackgroundPriority 	printString; tab;
		nextPutAll: 'timingPriority'; 						space; nextPutAll: scheduler timingPriority 						printString; tab;
		nextPutAll: 'userBackgroundPriority'; 		space; nextPutAll: scheduler userBackgroundPriority 		printString; tab;
		nextPutAll: 'userInterruptPriority'; 				space; nextPutAll: scheduler userInterruptPriority 			printString; tab;
		nextPutAll: 'userSchedulingPriority'; 			space; nextPutAll: scheduler userSchedulingPriority 		printString; tab;
		yourself.
	self addProcess: scheduler activeProcess to: stream withStatus: 'active' scheduler: scheduler.
	scheduler readyProcesses 			do: [:each | self addProcess: each to: stream withStatus: 'ready'			scheduler: scheduler].
	scheduler suspendedProcesses 	do: [:each | self addProcess: each to: stream withStatus: 'suspended'	scheduler: scheduler].
	scheduler waitingProcesses 		do: [:each | self addProcess: each to: stream withStatus: 'waiting'			scheduler: scheduler].
	^stream contents.

! !
!JadeServer categoriesFor: #addProcess:to:withStatus:scheduler:!Processes!public! !
!JadeServer categoriesFor: #processes!Processes!public! !

!JadeTranscript methodsFor!

browseProcesses

	JadeProcessBrowser showOn: gciSession.
! !
!JadeTranscript categoriesFor: #browseProcesses!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

JadeProcessBrowser guid: (GUID fromString: '{2340DC62-6F94-4AAB-93D4-4B8AC4A1487E}')!
JadeProcessBrowser comment: ''!
!JadeProcessBrowser categoriesForClass!Unclassified! !
!JadeProcessBrowser methodsFor!

createComponents  

	super createComponents.
	prioritiesPresenter :=  self add: ListPresenter new name: 'priorities'.
	processesPresenter :=  self add: ListPresenter new name: 'processes'.
!

onViewOpened

	super onViewOpened.
	self update.
!

queryCommand: aCommandQuery

	| command row state |
	command := aCommandQuery commandSymbol.
	command == #'refresh' ifTrue: [
		aCommandQuery isEnabled: true.
		^true.
	].
	processesPresenter selections size ~= 1 ifTrue: [
		aCommandQuery isEnabled: false.
		^true.
	].
	command := aCommandQuery commandSymbol.
	row := processesPresenter selection.
	state := row at: 1.
	(state = 'active' or: [(state = 'ready' and: [command == #'resume']) or: [state = 'suspended' and: [command == #'suspend']]]) ifTrue: [
		aCommandQuery isEnabled: false.
		^true.
	].
	aCommandQuery isEnabled: true.
	^true.
!

refresh

	self update.
!

resume

	self sendToSelectedProcess: #'resume'.!

sendToSelectedProcess: aSymbol

	| oop oopType |
	oop := (processesPresenter selection at: 2) asNumber.
	oopType := model oopTypeWithOop: oop.
	model send: aSymbol to: oopType.
	self update.!

suspend

	self sendToSelectedProcess: #'suspend'.
	!

terminate

	self sendToSelectedProcess: #'terminate'.
!

terminate9

	self sendToSelectedProcess: #'terminate9'.
!

update

	| string lines priorities |
	string := model serverPerform: #'processes'.
	lines := (string subStrings: Character lf) asOrderedCollection.
	priorities := lines removeFirst subStrings: Character tab.
	priorities := priorities collect: [:each | | pieces | pieces := each subStrings: Character space. pieces first -> pieces last asNumber].
	priorities := priorities asSortedCollection: [:a :b | a value > b value].
	prioritiesPresenter model list: priorities.
	lines := lines collect: [:each | each subStrings: Character tab].
	processesPresenter model list: lines.
!

updateCaption

	self caption: (model titleBarFor: 'Processes').
! !
!JadeProcessBrowser categoriesFor: #createComponents!public! !
!JadeProcessBrowser categoriesFor: #onViewOpened!public! !
!JadeProcessBrowser categoriesFor: #queryCommand:!public! !
!JadeProcessBrowser categoriesFor: #refresh!public! !
!JadeProcessBrowser categoriesFor: #resume!public! !
!JadeProcessBrowser categoriesFor: #sendToSelectedProcess:!public! !
!JadeProcessBrowser categoriesFor: #suspend!public! !
!JadeProcessBrowser categoriesFor: #terminate!public! !
!JadeProcessBrowser categoriesFor: #terminate9!public! !
!JadeProcessBrowser categoriesFor: #update!public! !
!JadeProcessBrowser categoriesFor: #updateCaption!public! !

!JadeProcessBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1601 481 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1409355853 1025 624 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 624 0 8 4294902751 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Process Priorities' 321 8 #left 850 880 896 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] 8 #key 1088 7 257 0 0 624 0 1 0 0 978 8 'Value' 101 8 #right 850 880 98 0 850 8 #<= 1280 1074 0 0 1106 1 83886081 1136 8 'doIt' 8 '[:each | each value]' 8 #[29 105 17 142 106] 1328 7 257 0 0 624 0 1 0 0 8 #report 752 0 131169 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 461 405 624 1490 8 #text: 98 1 8 'Process Priorities' 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 230 0 0 0 202 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 1762 8 #fixedViewLeft 461 1762 8 #fixedParentTop 1 1762 8 #fixedParentBottom 1 410 640 98 30 0 416 98 2 8 1409355853 1025 1904 706 202 208 752 0 784 482 816 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 6 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #resume 8 'Resume' 1 1 0 0 0 2066 1 2098 8 #suspend 8 'Suspend' 1025 1 0 0 0 2066 1 2098 8 #terminate 8 'Terminate' 1 1 0 0 0 2066 1 2098 8 #terminate9 8 'Terminate 9' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 2066 1 2098 8 #refresh 8 'Refresh' 1025 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 1904 0 8 4294902751 850 880 98 0 0 928 0 0 0 0 0 0 202 208 98 8 978 8 'State' 151 1024 850 880 2496 1056 1074 0 0 1106 1 83886081 1136 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 2592 7 257 0 0 1904 0 1 0 0 978 8 'OOP' 171 1248 850 880 1280 850 1312 1280 1074 0 0 1106 1 83886081 1136 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 2736 7 257 0 0 1904 0 1 0 0 978 8 'Priority' 101 1248 850 880 1280 850 1312 1280 1074 0 0 1106 1 83886081 1136 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 2880 7 257 0 0 1904 0 1 0 0 978 8 'App' 81 8 #center 850 880 1280 850 1312 1280 1074 0 0 1106 1 83886081 1136 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 3040 7 257 0 0 1904 0 1 0 0 978 8 'Stack' 101 1248 850 880 1280 850 1312 1280 1074 0 0 1106 1 83886081 1136 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 3184 7 257 0 0 1904 0 1 0 0 978 8 'Waiting On' 171 1248 850 880 1280 850 1312 1280 1074 0 0 1106 1 83886081 1136 8 'doIt' 8 '[:each | each at: 6]' 8 #[29 105 17 214 6 148 106] 3328 7 257 0 0 1904 0 1 0 0 978 8 'Ms Left' 111 1248 850 880 1280 850 1312 1280 1074 0 0 1106 1 83886081 1136 8 'doIt' 8 '[:each | each at: 7]' 8 #[29 105 17 214 7 148 106] 3472 7 257 0 0 1904 0 1 0 0 978 8 'Continuation' 181 2992 850 880 98 0 850 1312 3600 1074 0 0 1106 1 83886081 1136 8 'doIt' 8 '[:each | each at: 8]' 8 #[29 105 17 214 8 148 106] 3632 7 257 0 0 1904 0 1 0 0 1408 752 0 131169 0 0 1426 202 208 98 3 1490 1520 98 2 530 461 1 530 1109 405 1904 1490 8 #contextMenu: 98 1 2032 1904 1490 1600 98 1 8 'State' 1904 1650 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 230 0 0 0 0 0 0 0 16 3 0 0 202 0 0 0] 98 0 1712 0 27 1730 1762 8 #fixedPreviousRight 1 1762 8 #fixedParentRight 1 1840 1 1872 1 234 256 98 4 624 8 'priorities' 1904 8 'processes' 0 0 0 0 0 1 0 0 0 0 1 0 0 1426 202 208 98 2 1490 1520 98 2 530 2879 21 530 1601 481 416 1490 8 #updateMenuBar 752 416 1650 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 191 8 0 0 250 0 0 0] 98 2 624 1904 1712 0 27 )! !
!JadeProcessBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

