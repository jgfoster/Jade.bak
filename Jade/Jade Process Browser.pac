| package |
package := Package name: 'Jade Process Browser'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.010'.


package classNames
	add: #JadeProcessBrowser;
	yourself.

package methodNames
	add: #JadeServer -> #addProcess:to:withStatus:scheduler:;
	add: #JadeServer -> #processes;
	add: #JadeServer64bit3x -> #addProcess:to:withStatus:scheduler:;
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
	add: 'GemStone Objects';
	add: 'GemStone Session';
	add: 'Jade Inspector';
	add: 'Jade Transcript';
	add: 'Jade UI';
	add: 'Jade UI Base';
	yourself).

package!

"Class Definitions"!

JadeShell subclass: #JadeProcessBrowser
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
"6"	nextPutAll: ((x := aProcess waitingOn) 			isNil ifTrue: [''] ifFalse: [x asOop printString]); tab;
"7"	nextPutAll: ((x := aProcess _signalTime) 	isNil ifTrue: [''] ifFalse: [(x - aScheduler _now) printString]); tab;
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
	scheduler readyProcesses 			do: [:each | self addProcess: each to: stream withStatus: 'ready'			scheduler: scheduler].
	scheduler suspendedProcesses 	do: [:each | self addProcess: each to: stream withStatus: 'suspended'	scheduler: scheduler].
	scheduler waitingProcesses 		do: [:each | self addProcess: each to: stream withStatus: 'waiting'			scheduler: scheduler].
	^stream contents.

! !
!JadeServer categoriesFor: #addProcess:to:withStatus:scheduler:!Processes!public! !
!JadeServer categoriesFor: #processes!Processes!public! !

!JadeServer64bit3x methodsFor!

addProcess: aProcess to: aStream withStatus: aString scheduler: aScheduler

	| instVarNumber modeInfo modeInfo_forked modeInfo_terminated |
	super addProcess: aProcess to: aStream withStatus: aString scheduler: aScheduler.
	(instVarNumber := GsProcess instVarNames indexOf: #'modeInfo') == 0 ifTrue: [^self].
	modeInfo := aProcess instVarAt: instVarNumber.
	(modeInfo_forked := GsProcess _classVars at: #'ModeInfo_forked' ifAbsent: [nil]) ifNil: [^self].
	(modeInfo_terminated := GsProcess _classVars at: #'ModeInfo_terminated' ifAbsent: [nil]) ifNil: [^self].
	aStream
"9"		nextPutAll: (0 < (modeInfo bitAnd: modeInfo_forked) ifTrue: ['forked'] ifFalse: ['main']); tab;
"10"	nextPutAll: (0 < (modeInfo bitAnd: modeInfo_forked) ifTrue: ['terminated'] ifFalse: ['']); tab;
			yourself.
! !
!JadeServer64bit3x categoriesFor: #addProcess:to:withStatus:scheduler:!Processes!public! !

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

createSchematicWiring

	processesPresenter when: #'actionPerformed' send: #'inspectProcess' to: self.
!

debugProcess

	| oop oopType |
	oop := (processesPresenter selection at: 2) asNumber.
	oopType := model oopTypeWithOop: oop.
	JadeDebugger 
		openOn: (GsProcess session: model oop: oopType) 
		message: 'background process'
		terminateOnClose: false.
!

inspectProcess

	| oop oopType |
	oop := (processesPresenter selection at: 2) asNumber.
	oopType := model oopTypeWithOop: oop.
	JadeInspector showOn: model -> oopType.
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
	lines := lines collect: [:each | (each subStrings: Character tab) , #('?' '?')].
	processesPresenter model list: lines.
!

updateCaption

	self caption: (model titleBarFor: 'Processes').
! !
!JadeProcessBrowser categoriesFor: #createComponents!public! !
!JadeProcessBrowser categoriesFor: #createSchematicWiring!public! !
!JadeProcessBrowser categoriesFor: #debugProcess!public! !
!JadeProcessBrowser categoriesFor: #inspectProcess!public! !
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1881 481 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1409355853 1025 624 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 9 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #debugProcess 8 'Debug' 1 1 0 0 0 882 1 914 8 #inspectProcess 8 'Inspect' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 882 1 914 8 #resume 8 'Resume' 1 1 0 0 0 882 1 914 8 #suspend 8 'Suspend' 1025 1 0 0 0 882 1 914 8 #terminate 8 'Terminate' 1 1 0 0 0 882 1 914 8 #terminate9 8 'Terminate 9' 1025 1 0 0 0 1042 4097 882 1 914 8 #refresh 8 'Refresh' 1025 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 624 0 8 4294902395 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 10 920646 5 ##(Smalltalk.ListViewColumn)  8 'State' 151 8 #left 1442 1472 1488 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 1680 7 257 0 0 624 0 1 0 0 1570 8 'OOP' 171 8 #right 1442 1472 98 0 1442 8 #<= 1856 1666 0 0 1698 1 83886081 1728 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 1904 7 257 0 0 624 0 1 0 0 1570 8 'Priority' 101 1824 1442 1472 1856 1442 1888 1856 1666 0 0 1698 1 83886081 1728 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 2048 7 257 0 0 624 0 1 0 0 1570 8 'App' 81 8 #center 1442 1472 1856 1442 1888 1856 1666 0 0 1698 1 83886081 1728 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 2208 7 257 0 0 624 0 1 0 0 1570 8 'Stack' 101 1824 1442 1472 1856 1442 1888 1856 1666 0 0 1698 1 83886081 1728 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 2352 7 257 0 0 624 0 1 0 0 1570 8 'Waiting On' 171 1824 1442 1472 1856 1442 1888 1856 1666 0 0 1698 1 83886081 1728 8 'doIt' 8 '[:each | each at: 6]' 8 #[29 105 17 214 6 148 106] 2496 7 257 0 0 624 0 1 0 0 1570 8 'Ms Left' 111 1824 1442 1472 1856 1442 1888 1856 1666 0 0 1698 1 83886081 1728 8 'doIt' 8 '[:each | each at: 7]' 8 #[29 105 17 214 7 148 106] 2640 7 257 0 0 624 0 1 0 0 1570 8 'Continuation' 181 2160 1442 1472 98 0 1442 1888 2768 1666 0 0 1698 1 83886081 1728 8 'doIt' 8 '[:each | each at: 8]' 8 #[29 105 17 214 8 148 106] 2800 7 257 0 0 624 0 1 0 0 1570 8 'Type' 121 2160 1442 1472 752 1442 1888 752 1666 0 0 1698 1 83886081 1728 8 'doIt' 8 '[:each | each at: 9]' 8 #[29 105 17 214 9 148 106] 2944 7 257 0 0 624 0 1 0 0 1570 8 'Terminated' 161 2160 1442 1472 752 1442 1888 752 1666 0 0 1698 1 83886081 1728 8 'doIt' 8 '[:each | each at: 10]' 8 #[29 105 17 214 10 148 106] 3088 7 257 0 0 624 0 1 0 0 8 #report 752 0 131169 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 461 1 530 1389 405 624 3250 8 #contextMenu: 98 1 848 624 3250 8 #text: 98 1 8 'State' 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 230 0 0 0 0 0 0 0 156 3 0 0 202 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedPreviousRight 1 3570 8 #fixedParentRight 1 3570 8 #fixedParentTop 1 3570 8 #fixedParentBottom 1 410 640 98 30 0 416 98 2 8 1409355853 1025 3712 706 202 208 752 0 784 482 816 0 7 0 0 0 3712 0 8 4294902395 1442 1472 98 0 0 1520 0 0 0 0 0 0 202 208 98 2 1570 8 'Process Priorities' 321 1616 1442 1472 3856 1648 1666 0 0 1698 2 1 1728 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] 8 #key 3952 7 257 0 0 3712 0 1 0 0 1570 8 'Value' 101 1824 1442 1472 1856 1442 1888 1856 1666 0 0 1698 1 83886081 1728 8 'doIt' 8 '[:each | each value]' 8 #[29 105 17 142 106] 4112 7 257 0 0 3712 0 1 0 0 3168 752 0 131169 0 0 3186 202 208 98 2 3250 3280 98 2 530 1 1 530 461 405 3712 3250 3408 98 1 8 'Process Priorities' 3712 3458 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 230 0 0 0 202 0 0 0] 98 0 3520 0 27 3538 3570 8 #fixedParentLeft 1 3570 8 #fixedViewLeft 461 3648 1 3680 1 234 256 98 4 624 8 'processes' 3712 8 'priorities' 0 0 0 0 0 1 0 0 0 0 1 0 0 3186 202 208 98 2 3250 3280 98 2 530 2879 21 530 1881 481 416 3250 8 #updateMenuBar 752 416 3458 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 75 9 0 0 250 0 0 0] 98 2 3712 624 3520 0 27 )! !
!JadeProcessBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

