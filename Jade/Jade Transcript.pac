| package |
package := Package name: 'Jade Transcript'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.055'.


package classNames
	add: #JadeTranscript;
	yourself.

package methodNames
	add: #GsSession -> #sendSigAbort;
	add: #GsSession -> #sendSigUsr1;
	add: #GsSession -> #stopSession;
	add: #JadeServer -> #addSessionWithId:toStream:;
	add: #JadeServer -> #allSessions;
	add: #JadeServer -> #descriptionOfErrorNumber:;
	add: #JadeServer -> #sendSigAbortToSession:;
	add: #JadeServer -> #sendSigUsr1ToSession:;
	add: #JadeServer -> #sleepAndCommit;
	add: #JadeServer -> #stopSession:;
	add: #JadeServer32bit -> #addSessionWithId:toStream:;
	add: #JadeServer64bit -> #addSessionWithId:toStream:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Boolean\Dolphin Boolean Presenter';
	add: '..\Object Arts\Dolphin\MVP\Views\Cards\Dolphin Card Containers';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter';
	add: '..\Object Arts\Dolphin\MVP\Views\Scintilla\Dolphin Scintilla View';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'GemStone Objects';
	add: 'GemStone Session';
	add: 'Jade Autocompletation';
	add: 'Jade Inspector';
	add: 'Jade UI Base';
	yourself).

package!

"Class Definitions"!

JadeWorkspace subclass: #JadeTranscript
	instanceVariableNames: 'gciVersionPresenter userIDPresenter stoneHostPresenter stoneNamePresenter gemTypePresenter gemHostPresenter netPortPresenter netTaskPresenter sessionListPresenter sessionListErrorPresenter stoneSessionIDPresenter stoneSerialPresenter gciSessionIdPresenter autoUpdatePresenter autoUpdateProcess'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!GsSession methodsFor!

sendSigAbort

	gciSession 
		serverPerform: #'sendSigAbortToSession:'
		with: id.
!

sendSigUsr1

	gciSession 
		serverPerform: #'sendSigUsr1ToSession:'
		with: id.
!

stopSession

	gciSession 
		serverPerform: #'stopSession:'
		with: id.
! !
!GsSession categoriesFor: #sendSigAbort!accessing!public! !
!GsSession categoriesFor: #sendSigUsr1!accessing!public! !
!GsSession categoriesFor: #stopSession!accessing!public! !

!JadeServer methodsFor!

addSessionWithId: anInteger toStream: aStream

	| array gsSession timeGmt x |
	array := System descriptionOfSession: anInteger.
	array size: 20.
	gsSession := GsSession sessionWithSerialNumber: (array at: 9).
	timeGmt := System timeGmt.
	aStream
		nextPutAll: '<session oop=';
		nextPutAll: (self oopOf: gsSession) printString printString;
		nextPutAll: ' name=';
		nextPutAll: (array at: 1) userId printString;
		nextPutAll: ' process=';
		nextPutAll: (array at: 2) printString printString;
		nextPutAll: ' host=';
		nextPutAll: (array at: 3) printString;
		nextPutAll: ' primitive=';
		nextPutAll: (array at: 4) printString printString;
		nextPutAll: ' viewAge=';
		nextPutAll: (timeGmt - (array at: 5)) printString printString;
		nextPutAll: ' state=';
		nextPutAll: (array at: 6) printString printString;
		nextPutAll: ' transaction=';
		nextPutAll: (array at: 7) printString printString;
		nextPutAll: ' hasOldestCR=';
		nextPutAll: (array at: 8) printString printString;
		nextPutAll: ' serial=';
		nextPutAll: (array at: 9) printString printString;
		nextPutAll: ' id=';
		nextPutAll: (array at: 10) printString printString;
		nextPutAll: ' ip=';
		nextPutAll: (array at: 11) printString;
		nextPutAll: ' priority=';
		nextPutAll: ((x := array at: 12) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' hostId=';
		nextPutAll: ((x := array at: 13)  isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' quietTime=';
		nextPutAll: ((x := array at: 14) isNil ifTrue: [''] ifFalse: [(timeGmt - x)  printString]) printString;
		nextPutAll: ' lifeTime=';
		nextPutAll: ((x := array at: 15) isNil ifTrue: [''] ifFalse: [(timeGmt - x)  printString]) printString;
		nextPutAll: ' backlog=';
		nextPutAll: ((x := array at: 16) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' description=';
		nextPutAll: ((x := array at: 17) isNil ifTrue: [''] ifFalse: [x]) printString;
		nextPutAll: ' objects=';
		nextPutAll: ((x := array at: 18) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' pages=';
		nextPutAll: ((x := array at: 19) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' voteState=';
		nextPutAll: ((x := array at: 20) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' />';
		yourself.
!

allSessions

	| list stream |
	stream := WriteStream on: String new.
	stream nextPutAll: '<?xml version=''1.0'' ?><sessions>'.
	list := System currentSessionNames subStrings: Character lf.
	list := list reject: [:each | each isEmpty].
	list := list collect: [:each | (each subStrings at: 3) asNumber].
	list do: [:each | 
		self
			addSessionWithId: each
			toStream: stream.
	].
	^stream 
		nextPutAll: '</sessions>';
		contents.
!

descriptionOfErrorNumber: anInteger

	| array stream |
	array := GemStoneError at: #'English'.
	anInteger <= 0 ifTrue: [^'Invalid number!!'].
	array size < anInteger ifTrue: [^'Invalid number!!'].
	stream := WriteStream on: String new.
	array := array at: anInteger.
	array isNil ifTrue: [^'No entry in GemStoneError for #' , anInteger printString , '!!'].
	(array isKindOf: String) ifTrue: [array := Array with: array].
	array do: [:each | 
		(each isKindOf: Integer) ifTrue: [
			stream space; nextPut: $%.
			each printOn: stream.
		] ifFalse: [
			stream nextPutAll: each.
		].
	].
	^stream contents.
!

sendSigAbortToSession: anInteger

	System sendSigAbortToSession: anInteger negated.
!

sendSigUsr1ToSession: anInteger

	| description command result |
	description := System descriptionOfSession: anInteger.
	command := 'kill -usr1 ' , (description at: 2) printString.
	result := System performOnServer: command.
	result trimSeparators notEmpty ifTrue: [self error: result trimSeparators].
!

sleepAndCommit

	[
		System commitTransaction.
	] whileTrue: [
		(Delay forSeconds: 30) wait.
	].
!

stopSession: anInteger

	System stopSession: anInteger.
! !
!JadeServer categoriesFor: #addSessionWithId:toStream:!private!Sessions! !
!JadeServer categoriesFor: #allSessions!public!Sessions! !
!JadeServer categoriesFor: #descriptionOfErrorNumber:!public!Sessions! !
!JadeServer categoriesFor: #sendSigAbortToSession:!public!Sessions! !
!JadeServer categoriesFor: #sendSigUsr1ToSession:!public!Sessions! !
!JadeServer categoriesFor: #sleepAndCommit!public!Transcript! !
!JadeServer categoriesFor: #stopSession:!public!Sessions! !

!JadeServer32bit methodsFor!

addSessionWithId: anInteger toStream: aStream

	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | '?????'].
	super
		addSessionWithId: anInteger 
		toStream: aStream.
! !
!JadeServer32bit categoriesFor: #addSessionWithId:toStream:!public!Sessions! !

!JadeServer64bit methodsFor!

addSessionWithId: anInteger toStream: aStream

	[
		super
			addSessionWithId: anInteger
			toStream: aStream.
	] on: Error do: [:ex | 
		ex resume: '?????'.
	].
! !
!JadeServer64bit categoriesFor: #addSessionWithId:toStream:!public!Sessions! !

"End of package definition"!

"Source Globals"!

"Classes"!

JadeTranscript guid: (GUID fromString: '{7D871BC4-1338-4BA1-AF6A-C1EDD936D683}')!
JadeTranscript comment: ''!
!JadeTranscript categoriesForClass!Unclassified! !
!JadeTranscript methodsFor!

autocompletion

	JadeAutocompletationConfigurationShell show.
!

autoUpdateChanged

	autoUpdatePresenter model value ifTrue: [
		self fillSessionListRegularly.
	] ifFalse: [
		self terminateAutoUpdate.
	].
!

browseMonticelloRepositories

	self jadeBrowseMonticello.
!

cr

	^self nextPutAll: Character lf asString.
!

createComponents

	super createComponents.
	gciVersionPresenter			:= self add: TextPresenter			new name: 'gciVersion'.
	userIDPresenter				:= self add: TextPresenter			new name: 'userID'.
	sessionListPresenter 			:= self add: ListPresenter			new name: 'sessionList'.
	sessionListErrorPresenter 	:= self add: TextPresenter			new name: 'sessionListError'.
	stoneHostPresenter			:= self add: TextPresenter			new name: 'stoneHost'.
	stoneNamePresenter			:= self add: TextPresenter			new name: 'stoneName'.
	gemTypePresenter			:= self add: TextPresenter			new name: 'gemType'.
	gemHostPresenter			:= self add: TextPresenter			new name: 'gemHost'.
	netPortPresenter				:= self add: TextPresenter			new name: 'netPort'.
	netTaskPresenter				:= self add: TextPresenter			new name: 'netTask'.
	stoneSessionIDPresenter	:= self add: TextPresenter			new name: 'stoneSessionID'.
	stoneSerialPresenter			:= self add: TextPresenter			new name: 'stoneSerial'.
	gciSessionIdPresenter		:= self add: TextPresenter			new name: 'gciSessionId'.
	autoUpdatePresenter			:= self add: BooleanPresenter	new name: 'autoUpdate'.

!

createSchematicWiring

	super createSchematicWiring.
	(self view viewNamed: 'cardContainer') when: #'currentCardChanged' send: #'currentCardChanged' to: self.
	autoUpdatePresenter	when: #'valueChanged' send: #'autoUpdateChanged' to: self.!

currentCardChanged

	| currentCard |
	currentCard := (self view viewNamed: 'cardContainer') currentCard.
	(currentCard isKindOf: TextEdit)  ifTrue: [currentCard setFocus].
	(currentCard isKindOf: ReferenceView) ifTrue: [
		[
			(currentCard viewNamed: 'document') setFocus.
		] fork.
	].
!

describeErrorNumber

	| string result |
	(string := Prompter prompt: 'Enter GemStoneError number:') isNil ifTrue: [^self].
	(string allSatisfy: [:each | each isDigit]) ifFalse: [
		MessageBox warning: 'Invalid number!!'.
		^self.
	].
	result := gciSession 
		serverPerform: #'descriptionOfErrorNumber:'
		with: string asNumber.
	MessageBox 
		notify: result
		caption: 'GemStoneError ' , string.
!

disableNativeCode

	gciSession executeString: 'System configurationAt: #GemNativeCodeEnabled put: false'.
!

editCut

	View focus == self transcriptView ifTrue: [
		View focus isReadOnly: false.
		super editCut.
		View focus isReadOnly: true.
	].
	super editCut.
!

editDelete

	View focus == self transcriptView ifTrue: [
		View focus isReadOnly: false.
		super editDelete.
		View focus isReadOnly: true.
		^self.
	].
	super editDelete.
!

fillSessionInfo

	gciVersionPresenter 	value: gciSession libraryVersion.
	userIDPresenter		value: gciSession userID.
	stoneHostPresenter	value: gciSession stoneHost.
	stoneNamePresenter	value: gciSession stoneName.
	gciSession isLinkedGem ifTrue: [
		gemTypePresenter	value: 'linked'.
	] ifFalse: [
		gemTypePresenter	value: 'RPC'.
		gemHostPresenter	value: gciSession gemHost.
		netPortPresenter		value: gciSession netPort.
		netTaskPresenter		value: gciSession netTask.
	].
	stoneSessionIDPresenter	value: gciSession stoneSessionID.
	stoneSerialPresenter			value: gciSession stoneSerial.
	gciSessionIdPresenter		value: gciSession gciSessionId.
!

fillSessionList

	| string list |
	[
		string := gciSession serverPerform: #'allSessions'.
	] on: GsError , Error do: [:ex | 
		sessionListPresenter view hide.
		sessionListErrorPresenter value: 
'When attempting to fill the session list the following error was received:
' , ((ex isKindOf: GsError) ifTrue: [ex errorReport message printString] ifFalse: [ex description]).
		^self.
	].
	sessionListPresenter view show.
	list := GsSession 
		fromStringXML: string 
		session: gciSession.
	sessionListPresenter list: list.
!

fillSessionListRegularly

	autoUpdateProcess := [
		| count |
		count := 0.
		[
			count < 5 and: [self view class ~~ DeafObject].
		] whileTrue: [
			[
				self fillSessionList.
				count := 0.
			] on: Error do: [:ex | 
				count := count + 1.	"After a number of errors, let's stop trying!!"
				ex return. "If busy, update later"
			].
			[(Delay forSeconds: 15) wait] on: Error do: [:ex | count := count + 1].
		].
		autoUpdateProcess := nil.
	] forkAt: Processor userBackgroundPriority.
!

hardBreak

	gciSession hardBreak.
!

inspectOop

	| string result |
	(string := Prompter prompt: 'Enter OOP Number:') isNil ifTrue: [^self].
	(string allSatisfy: [:each | each isDigit]) ifFalse: [
		MessageBox warning: 'Invalid number!!'.
		^self.
	].
	result := gciSession executeString: 'Object _objectForOop: ' , string.
	JadeInspector showOn: gciSession -> result.
!

invalidateUserInterface

	[
		super invalidateUserInterface.
	] on: Error do: [:ex | 
		ex return.
	].
!

nextPut: aCharacter

	^self nextPutAll: aCharacter asString.
!

nextPutAll: anObject

	| string transcript |
	string := (anObject isKindOf: String)
		ifTrue: [anObject]
		ifFalse: [anObject printString].
	(transcript := self transcriptView)
		isReadOnly: false;
		goto: transcript textLength + 1;
		insertText: string at: transcript caretPosition;
		goto: transcript caretPosition + string size;
		isReadOnly: true;
		yourself.
	"marker to signal not to replace the top-of-stack"
	^gciSession oopIllegal.
!

onCloseRequested: boolValueHolder
 
	| answer |
	super onCloseRequested: boolValueHolder.
	boolValueHolder value ifFalse: [^self].
	gciSession isNil ifTrue: [^self].
	gciSession isValidSession ifFalse: [^self].
	gciSession logoutRequested ifFalse: [
		boolValueHolder value: false.
		^self.
	].
	answer := MessageBox new
		caption: 'Close Requested';
		confirmOrCancel: 'Commit current transaction?'.
	boolValueHolder value: (#(#yes #no) includes: answer).
	answer = #yes ifTrue: [gciSession commit].
	boolValueHolder value ifFalse: [^self].

	"We are going to close, so do some things first..."
	"self view hide."
	self terminateAutoUpdate.
	gciSession notNil ifTrue: [
		| temp |
		temp := gciSession.		"A logout will trigger closing all open windows, including this one."
		gciSession := nil.
		temp forceLogout.
	].
!

onViewOpened

	super onViewOpened.
	self view position: 95 @ 35.
	self fillSessionInfo.
	(gciSession indexOfClientForwarder: self) = 2 ifFalse: [self error: 'Transcript should be the second client forwarder!!'].
	"autoUpdatePresenter value: false."!

queryCommand: query

	(#(#stopSession) includes: query commandSymbol) ifTrue: [
		query isEnabled: sessionListPresenter hasSelection.
		^true.
	].
	(#(#'turnAutoMigrateOff' #'turnAutoMigrateOn') includes: query commandSymbol) ifTrue: [
		query isEnabled: true.
		^true.
	].
	^super queryCommand: query.

!

resetCursor

	[Cursor current: nil] forkAt: Processor userBackgroundPriority.
!

sendSigAbort

	| gsSession |
	(gsSession := sessionListPresenter selectionOrNil) isNil ifTrue: [^self].
	gsSession sendSigAbort.
	(Delay forSeconds: 1) wait.
	self fillSessionList.
!

sendSigUsr1

	sessionListPresenter selectionOrNil ifNotNil: [:gsSession | gsSession sendSigUsr1].
!

show: aString

	^self nextPutAll: aString.
!

sleepAndCommit

	[
		gciSession serverPerform: #'sleepAndCommit'.
	] on: GsSoftBreak , GsHardBreak do: [:ex | 
		ex terminateProcess.
	].
!

softBreak

	gciSession softBreak.
!

stopSession

	| gsSession |
	(gsSession := sessionListPresenter selectionOrNil) isNil ifTrue: [^self].
	(MessageBox confirm: 'Stop Session?') ifFalse: [^self].
	gsSession stopSession.
	(Delay forSeconds: 1) wait.
	self fillSessionList.
!

tab

	^self nextPutAll: Character tab asString.
!

terminateAutoUpdate

	autoUpdateProcess notNil ifTrue: [
		autoUpdateProcess terminate.
		autoUpdateProcess := nil.
	].
!

transcriptView

	^self view viewNamed: 'transcript'.
!

turnAutoMigrateOff

	gciSession executeString: 'MCPlatformSupport autoMigrate: false'.
!

turnAutoMigrateOn

	gciSession executeString: 'MCPlatformSupport autoMigrate: true'.
!

updateCaption

	self caption: (gciSession titleBarFor: 'Transcript').
! !
!JadeTranscript categoriesFor: #autocompletion!event handling!public! !
!JadeTranscript categoriesFor: #autoUpdateChanged!public! !
!JadeTranscript categoriesFor: #browseMonticelloRepositories!event handling!public! !
!JadeTranscript categoriesFor: #cr!public!Transcript! !
!JadeTranscript categoriesFor: #createComponents!public! !
!JadeTranscript categoriesFor: #createSchematicWiring!public! !
!JadeTranscript categoriesFor: #currentCardChanged!public! !
!JadeTranscript categoriesFor: #describeErrorNumber!event handling!public! !
!JadeTranscript categoriesFor: #disableNativeCode!event handling!public! !
!JadeTranscript categoriesFor: #editCut!public!Transcript! !
!JadeTranscript categoriesFor: #editDelete!public!Transcript! !
!JadeTranscript categoriesFor: #fillSessionInfo!public!updating! !
!JadeTranscript categoriesFor: #fillSessionList!public!updating! !
!JadeTranscript categoriesFor: #fillSessionListRegularly!public!updating! !
!JadeTranscript categoriesFor: #hardBreak!public! !
!JadeTranscript categoriesFor: #inspectOop!event handling!public! !
!JadeTranscript categoriesFor: #invalidateUserInterface!public!updating! !
!JadeTranscript categoriesFor: #nextPut:!public!Transcript! !
!JadeTranscript categoriesFor: #nextPutAll:!public!Transcript! !
!JadeTranscript categoriesFor: #onCloseRequested:!event handling!public! !
!JadeTranscript categoriesFor: #onViewOpened!public!updating! !
!JadeTranscript categoriesFor: #queryCommand:!public!updating! !
!JadeTranscript categoriesFor: #resetCursor!event handling!public! !
!JadeTranscript categoriesFor: #sendSigAbort!public!updating! !
!JadeTranscript categoriesFor: #sendSigUsr1!public!updating! !
!JadeTranscript categoriesFor: #show:!public!Transcript! !
!JadeTranscript categoriesFor: #sleepAndCommit!event handling!public! !
!JadeTranscript categoriesFor: #softBreak!public! !
!JadeTranscript categoriesFor: #stopSession!public!updating! !
!JadeTranscript categoriesFor: #tab!public!Transcript! !
!JadeTranscript categoriesFor: #terminateAutoUpdate!public! !
!JadeTranscript categoriesFor: #transcriptView!public! !
!JadeTranscript categoriesFor: #turnAutoMigrateOff!event handling!public! !
!JadeTranscript categoriesFor: #turnAutoMigrateOn!event handling!public! !
!JadeTranscript categoriesFor: #updateCaption!public! !

!JadeTranscript class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 2 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1140850688 131073 624 0 482 512 0 7 0 0 0 624 655878 ##(Smalltalk.CardLayout)  202 208 98 4 721414 ##(Smalltalk.Association)  8 'My Session' 410 8 ##(Smalltalk.ContainerView)  98 15 0 624 98 2 8 1140850688 131073 832 0 0 0 5 0 0 0 832 0 234 256 98 22 410 8 ##(Smalltalk.TextEdit)  98 16 0 832 98 2 8 1140916354 1025 944 0 482 8 4278190080 0 5 0 0 0 944 0 8 4294903259 852486 ##(Smalltalk.NullConverter)  0 0 3 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 931 111 530 111 41 944 1170 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 944 1170 8 #isTextModified: 98 1 32 944 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 209 1 0 0 55 0 0 0 8 2 0 0 75 0 0 0] 98 0 530 193 193 0 27 8 'gciSessionId' 410 960 98 16 0 832 98 2 8 1140916354 1025 1488 0 482 1040 0 5 0 0 0 1488 0 8 4294903259 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 191 161 530 111 41 1488 1170 1280 98 1 1314 3 1 3 1488 1170 1360 98 1 32 1488 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 80 0 0 0 150 0 0 0 100 0 0 0] 98 0 1456 0 27 8 'stoneSessionID' 410 960 98 16 0 832 98 2 8 1140916352 1025 1856 0 482 1040 0 5 0 0 0 1856 0 8 4294903259 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 551 61 530 211 41 1856 1170 1280 98 1 1314 3 1 3 1856 1170 1360 98 1 32 1856 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 19 1 0 0 30 0 0 0 124 1 0 0 50 0 0 0] 98 0 1456 0 27 8 'gemType' 410 960 98 16 0 832 98 2 8 1140916352 1025 2224 0 482 1040 0 5 0 0 0 2224 0 8 4294903259 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 191 61 530 211 41 2224 1170 1280 98 1 1314 3 1 3 2224 1170 1360 98 1 32 2224 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 30 0 0 0 200 0 0 0 50 0 0 0] 98 0 1456 0 27 8 'stoneHost' 410 960 98 16 0 832 98 2 8 1140916352 1025 2592 0 482 1040 0 5 0 0 0 2592 0 8 4294903259 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 931 61 530 211 41 2592 1170 1280 98 1 1314 3 1 3 2592 1170 1360 98 1 32 2592 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 209 1 0 0 30 0 0 0 58 2 0 0 50 0 0 0] 98 0 1456 0 27 8 'gciVersion' 410 960 98 16 0 832 98 2 8 1140916352 1025 2960 0 482 1040 0 5 0 0 0 2960 0 8 4294903259 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 551 111 530 211 41 2960 1170 1280 98 1 1314 3 1 3 2960 1170 1360 98 1 32 2960 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 19 1 0 0 55 0 0 0 124 1 0 0 75 0 0 0] 98 0 1456 0 27 8 'netPort' 410 960 98 16 0 832 98 2 8 1140916352 1025 3328 0 482 1040 0 5 0 0 0 3328 0 8 4294903259 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 191 11 530 211 41 3328 1170 1280 98 1 1314 3 1 3 3328 1170 1360 98 1 32 3328 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 5 0 0 0 200 0 0 0 25 0 0 0] 98 0 1456 0 27 8 'userID' 410 960 98 16 0 832 98 2 8 1140916352 1025 3696 0 482 1040 0 5 0 0 0 3696 0 8 4294903259 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 551 161 530 211 41 3696 1170 1280 98 1 1314 3 1 3 3696 1170 1360 98 1 32 3696 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 19 1 0 0 80 0 0 0 124 1 0 0 100 0 0 0] 98 0 1456 0 27 8 'netTask' 410 960 98 16 0 832 98 2 8 1140916354 1025 4064 0 482 1040 0 5 0 0 0 4064 0 8 4294903259 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 191 211 530 111 41 4064 1170 1280 98 1 1314 3 1 3 4064 1170 1360 98 1 32 4064 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 105 0 0 0 150 0 0 0 125 0 0 0] 98 0 1456 0 27 8 'stoneSerial' 410 960 98 16 0 832 98 2 8 1140916352 1025 4432 0 482 1040 0 5 0 0 0 4432 0 8 4294903259 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 551 11 530 591 41 4432 1170 1280 98 1 1314 3 1 3 4432 1170 1360 98 1 32 4432 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 19 1 0 0 5 0 0 0 58 2 0 0 25 0 0 0] 98 0 1456 0 27 8 'gemHost' 410 960 98 16 0 832 98 2 8 1140916352 1025 4800 0 482 1040 0 5 0 0 0 4800 0 8 4294903259 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 191 111 530 211 41 4800 1170 1280 98 1 1314 3 1 3 4800 1170 1360 98 1 32 4800 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 55 0 0 0 200 0 0 0 75 0 0 0] 98 0 1456 0 27 8 'stoneName' 0 1106 202 208 98 1 1170 1200 98 2 530 9 49 530 1153 629 832 1394 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 68 2 0 0 82 1 0 0] 98 26 3328 2224 4800 1488 4064 4432 1856 2960 3696 2592 944 410 8 ##(Smalltalk.StaticText)  98 16 0 832 98 2 8 1140850944 1 5328 0 0 0 5 0 0 0 5328 0 8 4294902369 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 11 161 530 181 41 5328 1170 8 #text: 98 1 8 'Stone Session:' 5328 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 80 0 0 0 95 0 0 0 100 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 5664 0 0 0 5 0 0 0 5664 0 8 4294902369 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 11 11 530 161 41 5664 1170 5568 98 1 8 'User ID:' 5664 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 85 0 0 0 25 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 5968 0 0 0 5 0 0 0 5968 0 8 4294902369 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 11 61 530 161 41 5968 1170 5568 98 1 8 'Stone Host:' 5968 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 30 0 0 0 85 0 0 0 50 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 6272 0 0 0 5 0 0 0 6272 0 8 4294902369 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 11 111 530 161 41 6272 1170 5568 98 1 8 'Stone Name:' 6272 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 55 0 0 0 85 0 0 0 75 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 6576 0 0 0 5 0 0 0 6576 0 8 4294902369 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 771 111 530 161 41 6576 1170 5568 98 1 8 'GCI Session:' 6576 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 129 1 0 0 55 0 0 0 209 1 0 0 75 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 6880 0 0 0 5 0 0 0 6880 0 8 4294902369 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 771 61 530 161 41 6880 1170 5568 98 1 8 'GCI Version:' 6880 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 129 1 0 0 30 0 0 0 209 1 0 0 50 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 7184 0 0 0 5 0 0 0 7184 0 8 4294902369 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 411 61 530 141 41 7184 1170 5568 98 1 8 'Gem Type:' 7184 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 205 0 0 0 30 0 0 0 19 1 0 0 50 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 7488 0 0 0 5 0 0 0 7488 0 8 4294902369 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 411 11 530 141 41 7488 1170 5568 98 1 8 'Gem Host:' 7488 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 205 0 0 0 5 0 0 0 19 1 0 0 25 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 7792 0 0 0 5 0 0 0 7792 0 8 4294902369 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 411 111 530 141 41 7792 1170 5568 98 1 8 'Net Service:' 7792 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 205 0 0 0 55 0 0 0 19 1 0 0 75 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 8096 0 0 0 5 0 0 0 8096 0 8 4294902369 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 411 161 530 141 41 8096 1170 5568 98 1 8 'Net Task:' 8096 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 205 0 0 0 80 0 0 0 19 1 0 0 100 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 8400 0 0 0 5 0 0 0 8400 0 8 4294902369 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 11 211 530 161 41 8400 1170 5568 98 1 8 'Stone Serial:
' 8400 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 105 0 0 0 85 0 0 0 125 0 0 0] 98 0 1456 0 27 410 8 ##(Smalltalk.PushButton)  98 20 0 832 98 2 8 1140924416 1 8704 0 0 0 5 0 0 0 8704 0 8 4294903249 1180998 4 ##(Smalltalk.CommandDescription)  8 #abortTransaction 8 'Abort' 1 1 0 0 32 0 0 0 1106 202 208 98 3 1170 1200 98 2 530 11 551 530 141 51 8704 1170 8 #isEnabled: 98 1 32 8704 1170 5568 98 1 8 'Abort' 8704 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 19 1 0 0 75 0 0 0 44 1 0 0] 98 0 1456 0 29 410 8720 98 20 0 832 98 2 8 1140924416 1 9120 0 0 0 5 0 0 0 9120 0 8 4294903249 8802 8 #commitTransaction 8 'Commit' 1 1 0 0 32 0 0 0 1106 202 208 98 3 1170 1200 98 2 530 161 551 530 141 51 9120 1170 8992 98 1 32 9120 1170 5568 98 1 8 'Commit' 9120 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 19 1 0 0 150 0 0 0 44 1 0 0] 98 0 1456 0 29 410 8720 98 20 0 832 98 2 8 1140924416 1 9488 0 0 0 5 0 0 0 9488 0 8 4294903249 8802 8 #softBreak 8 'Soft Break' 1 1 0 0 32 0 0 0 1106 202 208 98 3 1170 1200 98 2 530 311 551 530 141 51 9488 1170 8992 98 1 32 9488 1170 5568 98 1 8 'Soft Break' 9488 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 0 0 0 19 1 0 0 225 0 0 0 44 1 0 0] 98 0 1456 0 29 410 8720 98 20 0 832 98 2 8 1140924416 1 9856 0 0 0 5 0 0 0 9856 0 8 4294903249 8802 8 #hardBreak 8 'Hard Break' 1 1 0 0 32 0 0 0 1106 202 208 98 3 1170 1200 98 2 530 461 551 530 141 51 9856 1170 8992 98 1 32 9856 1170 5568 98 1 8 'Hard Break' 9856 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 230 0 0 0 19 1 0 0 44 1 0 0 44 1 0 0] 98 0 1456 0 29 1456 0 27 786 8 'All Sessions' 410 848 98 15 0 624 98 2 8 1140850688 131073 10256 0 0 0 5 0 0 0 10256 562 234 240 98 8 410 8720 98 20 0 10256 98 2 8 1140924416 1 10368 0 0 0 5 0 0 0 10368 0 8 4294903249 8802 8 #fillSessionList 8 'Update' 1 1 0 0 32 0 0 0 1106 202 208 98 3 1170 1200 98 2 530 1013 579 530 141 51 10368 1170 8992 98 1 32 10368 1170 5568 98 1 8 'Update' 10368 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 250 1 0 0 33 1 0 0 64 2 0 0 58 1 0 0] 98 0 1456 0 29 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentRight -139 10770 8 #fixedViewLeft 141 10770 8 #fixedParentBottom -49 10770 8 #fixedViewTop 51 410 8 ##(Smalltalk.ListView)  98 30 0 10256 98 2 8 1140920397 1025 10912 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 512 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 4 984134 2 ##(Smalltalk.CommandMenuItem)  1 8802 10464 8 '&Update' 1 1 0 0 0 11154 1 8802 8 #sendSigAbort 8 'Send Sig&Abort' 1 1 0 0 0 11154 1 8802 8 #sendSigUsr1 8 'Request &Stack' 1 1 0 0 0 11154 1 8802 8 #stopSession 8 'S&top Session' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 10912 0 8 4294902943 8 ##(Smalltalk.BasicListAbstract)  0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 20 920646 5 ##(Smalltalk.ListViewColumn)  8 '#' 61 8 #right 11440 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 459302 ##(Smalltalk.Context)  1 1 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each id]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  11040 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #id 17 257 0 0 10912 0 1 0 0 11522 8 'Serial' 111 11568 11440 11584 11602 0 11634 1 1 0 0 11666 1 9 11696 8 'doIt' 98 2 8 '[:each | each serial]' 98 1 202 11792 11040 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #serial 17 257 0 0 10912 0 1 0 0 11522 8 'User' 201 8 #left 11440 11584 11602 0 11634 1 1 0 0 11666 1 9 11696 8 'doIt' 98 2 8 '[:each | each name]' 98 1 202 11792 11040 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #name 17 257 0 0 10912 0 1 0 0 11522 8 'View Age' 131 11568 11440 11584 11602 0 0 11666 2 1 11600 8 'doIt' 8 '[:each | each viewAge]' 8 #[30 105 226 0 106] 8 #viewAge 12272 7 257 0 0 10912 0 1 0 0 11522 8 'Oldest' 121 8 #center 11440 11584 11602 0 11634 1 1 0 0 11666 1 9 11696 8 'doIt' 98 2 8 '[:each | each hasOldestCR]' 98 1 202 11792 11040 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #hasOldestCR 17 257 0 0 10912 0 1 0 0 11522 8 'Gem Host' 281 12064 11440 11584 11602 0 11634 1 1 0 0 11666 1 9 11696 8 'doIt' 98 2 8 '[:each | each host]' 98 1 202 11792 11040 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #host 17 257 0 0 10912 0 1 0 0 11522 8 'Process' 131 11568 11440 11584 11602 0 11634 1 1 0 0 11666 1 9 11696 8 'doIt' 98 2 8 '[:each | each process]' 98 1 202 11792 11040 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #process 17 257 0 0 10912 0 1 0 0 11522 8 'Primitive' 131 11568 11440 11584 11602 0 11634 1 1 0 0 11666 1 9 11696 8 'doIt' 98 2 8 '[:each | each primitive]' 98 1 202 11792 11040 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #primitive 17 257 0 0 10912 0 1 0 0 11522 8 'State' 111 11568 11440 11584 11602 0 11634 1 1 0 0 11666 1 9 11696 8 'doIt' 98 2 8 '[:each | each state]' 98 1 202 11792 11040 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #state 17 257 0 0 10912 0 1 0 0 11522 8 'Trans' 111 11568 11440 11584 11602 0 11634 1 1 0 0 11666 1 9 11696 8 'doIt' 98 2 8 '[:each | each transaction]' 98 1 202 11792 11040 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #transaction 17 257 0 0 10912 0 1 0 0 11522 8 'GCI IP' 241 12064 11440 11584 11602 0 11634 1 1 0 0 11666 1 9 11696 8 'doIt' 98 2 8 '[:each | each ip]' 98 1 202 11792 11040 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #ip 17 257 0 0 10912 0 1 0 0 11522 8 'Priority' 111 11568 459270 ##(Smalltalk.Message)  8 #displayString 98 0 13762 8 #<= 13808 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | each priority]' 8 #[30 105 226 0 106] 8 #priority 13856 7 257 0 0 10912 0 1 0 0 11522 8 'Host ID' 121 11568 13762 13792 13808 13762 13840 13808 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | each hostId]' 8 #[30 105 226 0 106] 8 #hostId 14016 7 257 0 0 10912 0 1 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | ''Unique host ID of the host where the session is running (an Integer)'']' 8 #[30 105 29 106] 8 'Unique host ID of the host where the session is running (an Integer)' 14112 7 257 0 0 11522 8 'Quiet' 121 11568 13762 13792 13808 13762 13840 13808 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | each quietTime]' 8 #[30 105 226 0 106] 8 #quietTime 14272 7 257 0 0 10912 0 1 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | ''Seconds since the session''''s most recent request to the stone'']' 8 #[30 105 29 106] 8 'Seconds since the session''s most recent request to the stone' 14368 7 257 0 0 11522 8 'Age' 121 11568 13762 13792 13808 13762 13840 13808 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | each lifeTime]' 8 #[30 105 226 0 106] 8 #lifeTime 14528 7 257 0 0 10912 0 1 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | ''Seconds since the session logged in'']' 8 #[30 105 29 106] 8 'Seconds since the session logged in' 14624 7 257 0 0 11522 8 'Backlog' 121 11568 13762 13792 13808 13762 13840 13808 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | each backlog]' 8 #[30 105 226 0 106] 8 #backlog 14784 7 257 0 0 10912 0 1 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | ''Number of commits which have occurred since the session obtained its view'']' 8 #[30 105 29 106] 8 'Number of commits which have occurred since the session obtained its view' 14880 7 257 0 0 11522 8 'Type' 201 12064 13762 13792 13808 13762 13840 13808 11602 0 0 11666 2 1 11600 8 'doIt' 8 '[:each | each description]' 8 #[30 105 226 0 106] 8 #description 15040 7 257 0 0 10912 0 1 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | ''Nil or a String describing a system or GC gem'']' 8 #[30 105 29 106] 8 'Nil or a String describing a system or GC gem' 15136 7 257 0 0 11522 8 'Objects' 121 11568 13762 13792 13808 13762 13840 13808 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | each objects]' 8 #[30 105 226 0 106] 8 #objects 15296 7 257 0 0 10912 0 1 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | ''Number of temporary (unused) object IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (unused) object IDs allocated to the session' 15392 7 257 0 0 11522 8 'Pages' 121 11568 13762 13792 13808 13762 13840 13808 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | each pages]' 8 #[30 105 226 0 106] 8 #pages 15552 7 257 0 0 10912 0 1 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | ''Number of temporary (non-persistent) page IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (non-persistent) page IDs allocated to the session' 15648 7 257 0 0 11522 8 'Voting' 121 11568 13762 13792 13808 13762 13840 13808 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | each voteState]' 8 #[30 105 226 0 106] 8 #voteState 15808 7 257 0 0 10912 0 1 11602 0 0 11666 2 1 11696 8 'doIt' 8 '[:each | ''0: session has not voted; 1: voting now; 2: voted'']' 8 #[30 105 29 106] 8 '0: session has not voted; 1: voting now; 2: voted' 15904 7 257 0 0 8 #report 11040 0 131169 0 0 1106 202 208 98 3 1170 1200 98 2 530 1 1 530 1153 579 10912 1170 8 #contextMenu: 98 1 11120 10912 1170 5568 98 1 8 '#' 10912 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 33 1 0 0] 98 0 1456 0 27 10738 10770 8 #fixedParentLeft 1 10784 1 10770 8 #fixedParentTop 1 10848 -49 410 8 ##(Smalltalk.CheckBox)  98 16 0 10256 98 2 8 1409363203 1 16352 721990 2 ##(Smalltalk.ValueHolder)  0 0 1114118 ##(Smalltalk.NeverSearchPolicy)  32 0 0 5 0 0 0 16352 0 8 4294903249 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 11 579 530 227 43 16352 1170 5568 98 1 8 'Auto-update' 16352 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 33 1 0 0 118 0 0 0 54 1 0 0] 98 0 1456 0 27 10738 16288 11 10816 227 10848 -49 10880 43 410 5344 98 16 0 10256 98 2 8 1140850944 1 16752 0 0 0 5 0 0 0 16752 0 8 4294902369 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 1 1 530 1153 629 16752 1170 5568 98 1 8 'Error obtaining session list. Likely problem is that login user does not have permission to view session list.' 16752 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 58 1 0 0] 98 0 1456 0 27 10738 16288 1 10784 1 16320 1 10848 1 234 256 98 6 16352 8 'autoUpdate' 16752 8 'sessionListError' 10912 8 'sessionList' 0 1106 202 208 98 1 1170 1200 98 2 530 9 49 530 1153 629 10256 1394 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 68 2 0 0 82 1 0 0] 98 4 10368 10912 16352 16752 1456 0 27 786 8 'Workspace' 410 8 ##(Smalltalk.ReferenceView)  98 14 0 624 98 2 8 1140850688 131073 17344 0 482 8 4278190080 0 7 0 0 0 17344 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.JadeCodePresenter)  8 #resource_Default_view 0 1106 202 208 98 1 1170 1200 98 2 530 9 49 530 1153 629 17344 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 68 2 0 0 82 1 0 0] 11040 1456 0 27 786 8 'Transcript' 410 848 98 15 0 624 98 2 8 1140850688 131073 17696 0 0 0 5 0 0 0 17696 562 234 240 98 4 410 8 ##(Smalltalk.Toolbar)  98 25 0 17696 98 2 8 1140851500 131137 17808 0 482 8 4278190080 0 517 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 17808 482 17904 8 4294903237 234 256 11040 234 256 98 38 47967 853766 ##(Smalltalk.ToolbarButton)  47967 0 17808 1 8802 8 #jadeExecute 8 'Evaluate Selection or Line' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 530 1857 33 57 47969 18098 47969 0 17808 1 8802 8 #jadeInspect 8 'Inspect Selection or Line' 1 1 0 18192 59 47971 1246982 ##(Smalltalk.ToolbarSystemButton)  47971 0 17808 1 8802 8 #fileNew 8 'New Workspace' 1 1 0 1 13 47973 18386 47973 0 17808 1 8802 8 #fileOpen 8 'Open Workspace' 1 1 0 1 15 47975 18386 47975 0 17808 1 8802 8 #fileSave 8 'Save' 1 1 0 1 17 47977 18386 47977 0 17808 1 8802 8 #editCut 8 'Cut' 1 1 0 1 1 47979 18386 47979 0 17808 1 8802 8 #editCopy 8 'Copy' 1 1 0 1 3 47981 18386 47981 0 17808 1 8802 8 #editPaste 8 'Paste' 1 1 0 1 5 47983 18386 47983 0 17808 1 8802 8 #editDelete 8 'Delete' 1 1 0 1 11 47985 18386 47985 0 17808 1 8802 8 #undo 8 'Undo' 1 1 0 1 7 47987 18386 47987 0 17808 1 8802 8 #redo 8 'Redo' 1 1 0 1 9 47989 18386 47989 0 17808 1 8802 8 #editFind 8 'Find' 1 1 0 1 25 47991 18386 47991 0 17808 1 8802 8 #editReplace 8 'Replace' 1 1 0 1 27 47955 18098 47955 0 17808 1 8802 8832 8 'Abort Transaction' 1 1 0 18192 1 47957 18098 47957 0 17808 1 8802 9216 8 'Commit Transaction' 1 1 0 18192 27 47959 18098 47959 0 17808 1 8802 8 #jadeBrowseUsers 8 'Browse Users' 1 1 0 18192 75 47961 18098 47961 0 17808 1 8802 8 #jadeBrowseClasses 8 'Open System Browser' 1 1 0 18192 17 47963 18098 47963 0 17808 1 8802 8 #jadeBrowseMonticello 8 'Open Monticello Browser' 1 1 0 18192 3 47965 18098 47965 0 17808 1 8802 8 #jadeDisplay 8 'Print Result of Selection or Line' 1 1 0 18192 55 98 24 19104 19152 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 17808 3 0 1 19200 19264 19328 19474 0 0 17808 3 0 1 19392 18112 18320 19474 0 0 17808 3 0 1 18400 18464 18528 19474 0 0 17808 3 0 1 18592 18656 18720 18784 18848 18912 19474 0 0 17808 3 0 1 18976 19040 234 240 98 4 1 117 18192 1 0 1 0 530 33 33 530 45 45 0 0 1106 202 208 98 2 1170 1200 98 2 530 1 1 530 1153 51 17808 1170 8 #updateSize 11040 17808 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 25 0 0 0] 98 0 1456 0 27 10738 16288 1 10784 1 16320 1 10880 51 410 8 ##(Smalltalk.ScintillaView)  98 46 0 17696 98 2 8 1445007428 1025 19840 16434 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 482 8 4278190080 0 5 0 0 0 19840 0 8 4294903129 1074 0 0 11 0 234 256 98 2 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 20064 0 0 0 98 40 20096 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  20064 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 786694 ##(Smalltalk.IndexedColor)  33554433 20242 33554471 19840 8 #circle 202 208 11040 0 63 9215 0 0 0 0 20242 33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 20032 0 0 0 0 1 0 234 256 98 6 1 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  1 19840 65025 3 32 1 0 3 20434 3 19840 33423361 5 32 3 0 5 20434 5 19840 511 1 32 5 0 1106 202 208 98 8 1170 1200 98 2 530 1 51 530 1153 579 19840 1170 1280 98 1 1314 3 1 3 19840 1170 1360 98 1 32 19840 1170 8 #modificationEventMask: 98 1 9215 19840 1170 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 19840 1 3 32 1 20802 3 19840 33 1 16 67108863 20802 5 19840 1 1 16 -67108863 19840 1170 8 #indentationGuides: 98 1 0 19840 1170 8 #tabIndents: 98 1 16 19840 1170 8 #tabWidth: 98 1 9 19840 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 64 2 0 0 58 1 0 0] 98 0 1456 0 27 10738 16288 1 10784 1 16320 51 10848 1 234 256 98 2 19840 8 'transcript' 0 1106 202 208 98 1 1170 1200 98 2 530 9 49 530 1153 629 17696 1394 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 68 2 0 0 82 1 0 0] 98 2 17808 19840 1456 0 27 17344 234 256 98 6 10256 8 'sessionListTab' 17344 8 'codePane' 832 8 'mySessionTab' 0 410 8 ##(Smalltalk.TabView)  98 23 0 624 98 2 8 1140916736 1 21360 10994 202 208 98 4 17680 816 10240 17328 0 11072 721158 ##(Smalltalk.SystemColor)  31 0 1 0 0 0 21360 0 8 4294902479 11440 8 ##(Smalltalk.IconicListAbstract)  11472 0 0 0 0 0 8 #noIcons 1106 202 208 98 3 1170 1200 98 2 530 1 1 530 1169 685 21360 1170 8 #basicSelectionsByIndex: 98 1 98 1 9 21360 1170 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 21360 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 86 1 0 0] 98 0 1456 0 27 1106 202 208 98 1 1170 1200 98 2 530 1 1 530 1169 685 624 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 86 1 0 0] 98 5 17696 832 10256 17344 21360 1456 0 27 10738 16288 1 10784 1 16320 1 10848 1 234 256 98 2 624 8 'cardContainer' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 11106 0 16 98 9 11154 1 8802 18432 8 '&New Workspace' 9373 1 0 0 0 11154 1 8802 18496 8 '&Open Workspace...' 9375 1 0 0 0 11154 1 8802 18560 8 '&Save' 9383 1 0 0 0 11154 1 8802 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 11154 1 8802 8 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 11154 1 8802 8 #resetCursor 8 'Reset &Cursor' 1025 1 0 0 0 22418 4097 11154 1 8802 8 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 48097 0 0 11106 0 16 98 15 11154 1 8802 18880 8 '&Undo' 9397 1 0 0 0 11154 1 8802 18944 8 'R&edo' 9395 1 0 0 0 22418 4097 11154 1 8802 18624 8 'Cu&t' 9393 1 0 0 0 11154 1 8802 18688 8 '&Copy' 9351 1 0 0 0 11154 1 8802 18752 8 '&Paste' 9389 1 0 0 0 11154 1 8802 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 11154 1 8802 18816 8 '&Delete' 1629 1 0 0 0 22418 4097 11154 1 8802 19008 8 '&Find...' 9357 1 0 0 0 11154 1 8802 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 11154 1 8802 19072 8 '&Replace...' 9361 1 0 0 0 22418 4097 11154 1 8802 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 11154 1 8802 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 48123 0 0 11106 0 16 98 21 11154 1 8802 8 #sleepAndCommit 8 '&Sleep and Commit' 1 1 0 0 0 11154 1 8802 8832 8 '&Abort Transaction' 1 1 0 0 0 11154 1 8802 9216 8 '&Commit Transaction' 1 1 0 0 0 11154 1 8802 8 #disableNativeCode 8 'Disable Naitive Code' 1 1 0 0 0 22418 4097 11154 1 8802 18352 8 '&Inspect' 9379 1 0 0 0 11154 1 8802 19424 8 '&Display' 9353 1 0 0 0 11154 1 8802 18144 8 '&Execute' 9355 1 0 0 0 11154 1 8802 8 #fileIn 8 'Fi&le In' 1 1 0 0 0 22418 4097 11154 1 8802 19296 8 '&Browse Classes' 9349 1 0 0 0 11154 1 8802 8 #browseMonticelloRepositories 8 'Browser &Monticello Repositories' 9371 1 0 0 0 11154 1 8802 8 #jadeBrowseObjectLog 8 'Browse &Object Log' 9369 1 0 0 0 11154 1 8802 8 #browseProcesses 8 'Browse &Processes' 9377 1 0 0 0 11154 1 8802 8 #autocompletion 8 'Configure Autocompletion' 1 1 0 0 0 22418 4097 11154 1 8802 8 #inspectOop 8 'Inspect Oop ...' 1 1 0 0 0 11154 1 8802 8 #describeErrorNumber 8 'Description for Error Number ...' 1025 1 0 0 0 22418 4097 11154 1025 8802 8 #turnAutoMigrateOff 8 'Turn Auto Migrate Off' 1 1 0 0 0 11154 1025 8802 8 #turnAutoMigrateOn 8 'Turn Auto Migrate On' 1 1 0 0 0 8 '&Jade' 0 1 0 0 48159 0 0 11106 0 16 98 1 11154 1 8802 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 48163 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 263494 3 ##(Smalltalk.Icon)  0 16 18224 8 'icons\GS32x32.ico' 0 24578 0 16 18224 8 'icons\GS16x16.ico' 0 0 0 1 0 0 1106 202 208 98 3 1170 1200 98 2 530 2879 21 530 1201 801 416 1170 5568 98 1 8 'Jade Session' 416 1170 8 #updateMenuBar 11040 416 1394 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 98 1 624 1456 0 27 )! !
!JadeTranscript class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

