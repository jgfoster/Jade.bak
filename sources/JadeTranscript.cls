"Filed out from Dolphin Smalltalk 7"!

JadeWorkspace subclass: #JadeTranscript
	instanceVariableNames: 'gciVersionPresenter userIDPresenter stoneHostPresenter stoneNamePresenter gemTypePresenter gemHostPresenter netPortPresenter netTaskPresenter sessionListPresenter sessionListErrorPresenter stoneSessionIDPresenter stoneSerialPresenter gciSessionIdPresenter autoUpdatePresenter autoUpdateProcess'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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
	JadeInspector showOn: result session: gciSession.
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 2 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1140850688 131073 624 0 482 512 0 7 0 0 0 624 655878 ##(Smalltalk.CardLayout)  202 208 98 4 721414 ##(Smalltalk.Association)  8 'My Session' 410 8 ##(Smalltalk.ContainerView)  98 15 0 624 98 2 8 1140850688 131073 832 0 0 0 5 0 0 0 832 0 234 256 98 22 410 8 ##(Smalltalk.TextEdit)  98 16 0 832 98 2 8 1140916352 1025 944 0 482 8 4278190080 0 5 0 0 0 944 0 8 4294901907 852486 ##(Smalltalk.NullConverter)  0 0 3 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 551 11 530 591 41 944 1170 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 944 1170 8 #isTextModified: 98 1 32 944 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 19 1 0 0 5 0 0 0 58 2 0 0 25 0 0 0] 98 0 530 193 193 0 27 8 'gemHost' 410 960 98 16 0 832 98 2 8 1140916352 1025 1488 0 482 1040 0 5 0 0 0 1488 0 8 4294901907 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 931 61 530 211 41 1488 1170 1280 98 1 1314 3 1 3 1488 1170 1360 98 1 32 1488 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 209 1 0 0 30 0 0 0 58 2 0 0 50 0 0 0] 98 0 1456 0 27 8 'gciVersion' 410 960 98 16 0 832 98 2 8 1140916354 1025 1856 0 482 1040 0 5 0 0 0 1856 0 8 4294901907 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 931 111 530 111 41 1856 1170 1280 98 1 1314 3 1 3 1856 1170 1360 98 1 32 1856 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 209 1 0 0 55 0 0 0 8 2 0 0 75 0 0 0] 98 0 1456 0 27 8 'gciSessionId' 410 960 98 16 0 832 98 2 8 1140916354 1025 2224 0 482 1040 0 5 0 0 0 2224 0 8 4294901907 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 191 161 530 111 41 2224 1170 1280 98 1 1314 3 1 3 2224 1170 1360 98 1 32 2224 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 80 0 0 0 150 0 0 0 100 0 0 0] 98 0 1456 0 27 8 'stoneSessionID' 410 960 98 16 0 832 98 2 8 1140916352 1025 2592 0 482 1040 0 5 0 0 0 2592 0 8 4294901907 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 191 11 530 211 41 2592 1170 1280 98 1 1314 3 1 3 2592 1170 1360 98 1 32 2592 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 5 0 0 0 200 0 0 0 25 0 0 0] 98 0 1456 0 27 8 'userID' 410 960 98 16 0 832 98 2 8 1140916354 1025 2960 0 482 1040 0 5 0 0 0 2960 0 8 4294901907 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 191 211 530 111 41 2960 1170 1280 98 1 1314 3 1 3 2960 1170 1360 98 1 32 2960 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 105 0 0 0 150 0 0 0 125 0 0 0] 98 0 1456 0 27 8 'stoneSerial' 410 960 98 16 0 832 98 2 8 1140916352 1025 3328 0 482 1040 0 5 0 0 0 3328 0 8 4294901907 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 191 61 530 211 41 3328 1170 1280 98 1 1314 3 1 3 3328 1170 1360 98 1 32 3328 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 30 0 0 0 200 0 0 0 50 0 0 0] 98 0 1456 0 27 8 'stoneHost' 410 960 98 16 0 832 98 2 8 1140916352 1025 3696 0 482 1040 0 5 0 0 0 3696 0 8 4294901907 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 551 61 530 211 41 3696 1170 1280 98 1 1314 3 1 3 3696 1170 1360 98 1 32 3696 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 19 1 0 0 30 0 0 0 124 1 0 0 50 0 0 0] 98 0 1456 0 27 8 'gemType' 410 960 98 16 0 832 98 2 8 1140916352 1025 4064 0 482 1040 0 5 0 0 0 4064 0 8 4294901907 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 551 111 530 211 41 4064 1170 1280 98 1 1314 3 1 3 4064 1170 1360 98 1 32 4064 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 19 1 0 0 55 0 0 0 124 1 0 0 75 0 0 0] 98 0 1456 0 27 8 'netPort' 410 960 98 16 0 832 98 2 8 1140916352 1025 4432 0 482 1040 0 5 0 0 0 4432 0 8 4294901907 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 551 161 530 211 41 4432 1170 1280 98 1 1314 3 1 3 4432 1170 1360 98 1 32 4432 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 19 1 0 0 80 0 0 0 124 1 0 0 100 0 0 0] 98 0 1456 0 27 8 'netTask' 410 960 98 16 0 832 98 2 8 1140916352 1025 4800 0 482 1040 0 5 0 0 0 4800 0 8 4294901907 1074 0 0 3 1106 202 208 98 3 1170 1200 98 2 530 191 111 530 211 41 4800 1170 1280 98 1 1314 3 1 3 4800 1170 1360 98 1 32 4800 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 95 0 0 0 55 0 0 0 200 0 0 0 75 0 0 0] 98 0 1456 0 27 8 'stoneName' 0 1106 202 208 98 1 1170 1200 98 2 530 9 49 530 1153 627 832 1394 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 68 2 0 0 81 1 0 0] 98 26 2592 3328 4800 2224 2960 944 3696 4064 4432 1488 1856 410 8 ##(Smalltalk.StaticText)  98 16 0 832 98 2 8 1140850944 1 5328 0 0 0 5 0 0 0 5328 0 8 4294902501 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 11 161 530 181 41 5328 1170 8 #text: 98 1 8 'Stone Session:' 5328 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 80 0 0 0 95 0 0 0 100 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 5664 0 0 0 5 0 0 0 5664 0 8 4294902501 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 11 11 530 161 41 5664 1170 5568 98 1 8 'User ID:' 5664 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 85 0 0 0 25 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 5968 0 0 0 5 0 0 0 5968 0 8 4294902501 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 11 61 530 161 41 5968 1170 5568 98 1 8 'Stone Host:' 5968 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 30 0 0 0 85 0 0 0 50 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 6272 0 0 0 5 0 0 0 6272 0 8 4294902501 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 11 111 530 161 41 6272 1170 5568 98 1 8 'Stone Name:' 6272 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 55 0 0 0 85 0 0 0 75 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 6576 0 0 0 5 0 0 0 6576 0 8 4294902501 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 771 111 530 161 41 6576 1170 5568 98 1 8 'GCI Session:' 6576 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 129 1 0 0 55 0 0 0 209 1 0 0 75 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 6880 0 0 0 5 0 0 0 6880 0 8 4294902501 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 771 61 530 161 41 6880 1170 5568 98 1 8 'GCI Version:' 6880 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 129 1 0 0 30 0 0 0 209 1 0 0 50 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 7184 0 0 0 5 0 0 0 7184 0 8 4294902501 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 411 61 530 141 41 7184 1170 5568 98 1 8 'Gem Type:' 7184 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 205 0 0 0 30 0 0 0 19 1 0 0 50 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 7488 0 0 0 5 0 0 0 7488 0 8 4294902501 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 411 11 530 141 41 7488 1170 5568 98 1 8 'Gem Host:' 7488 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 205 0 0 0 5 0 0 0 19 1 0 0 25 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 7792 0 0 0 5 0 0 0 7792 0 8 4294902501 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 411 111 530 141 41 7792 1170 5568 98 1 8 'Net Service:' 7792 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 205 0 0 0 55 0 0 0 19 1 0 0 75 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 8096 0 0 0 5 0 0 0 8096 0 8 4294902501 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 411 161 530 141 41 8096 1170 5568 98 1 8 'Net Task:' 8096 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 205 0 0 0 80 0 0 0 19 1 0 0 100 0 0 0] 98 0 1456 0 27 410 5344 98 16 0 832 98 2 8 1140850944 1 8400 0 0 0 5 0 0 0 8400 0 8 4294902501 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 11 211 530 161 41 8400 1170 5568 98 1 8 'Stone Serial:
' 8400 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 105 0 0 0 85 0 0 0 125 0 0 0] 98 0 1456 0 27 410 8 ##(Smalltalk.PushButton)  98 20 0 832 98 2 8 1140924416 1 8704 0 0 0 5 0 0 0 8704 0 8 4294902463 1180998 4 ##(Smalltalk.CommandDescription)  8 #abortTransaction 8 'Abort' 1 1 0 0 32 0 0 0 1106 202 208 98 3 1170 1200 98 2 530 11 551 530 141 51 8704 1170 8 #isEnabled: 98 1 32 8704 1170 5568 98 1 8 'Abort' 8704 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 19 1 0 0 75 0 0 0 44 1 0 0] 98 0 1456 0 29 410 8720 98 20 0 832 98 2 8 1140924416 1 9120 0 0 0 5 0 0 0 9120 0 8 4294902463 8802 8 #commitTransaction 8 'Commit' 1 1 0 0 32 0 0 0 1106 202 208 98 3 1170 1200 98 2 530 161 551 530 141 51 9120 1170 8992 98 1 32 9120 1170 5568 98 1 8 'Commit' 9120 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 19 1 0 0 150 0 0 0 44 1 0 0] 98 0 1456 0 29 410 8720 98 20 0 832 98 2 8 1140924416 1 9488 0 0 0 5 0 0 0 9488 0 8 4294902463 8802 8 #softBreak 8 'Soft Break' 1 1 0 0 32 0 0 0 1106 202 208 98 3 1170 1200 98 2 530 311 551 530 141 51 9488 1170 8992 98 1 32 9488 1170 5568 98 1 8 'Soft Break' 9488 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 0 0 0 19 1 0 0 225 0 0 0 44 1 0 0] 98 0 1456 0 29 410 8720 98 20 0 832 98 2 8 1140924416 1 9856 0 0 0 5 0 0 0 9856 0 8 4294902463 8802 8 #hardBreak 8 'Hard Break' 1 1 0 0 32 0 0 0 1106 202 208 98 3 1170 1200 98 2 530 461 551 530 141 51 9856 1170 8992 98 1 32 9856 1170 5568 98 1 8 'Hard Break' 9856 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 230 0 0 0 19 1 0 0 44 1 0 0 44 1 0 0] 98 0 1456 0 29 1456 0 27 786 8 'All Sessions' 410 848 98 15 0 624 98 2 8 1140850688 131073 10256 0 0 0 5 0 0 0 10256 562 234 240 98 8 410 8 ##(Smalltalk.CheckBox)  98 16 0 10256 98 2 8 1409363203 1 10368 721990 2 ##(Smalltalk.ValueHolder)  0 0 1114118 ##(Smalltalk.NeverSearchPolicy)  32 0 0 5 0 0 0 10368 0 8 4294902463 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 11 577 530 227 43 10368 1170 5568 98 1 8 'Auto-update' 10368 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 32 1 0 0 118 0 0 0 53 1 0 0] 98 0 1456 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 11 10786 8 #fixedViewLeft 227 10786 8 #fixedParentBottom -49 10786 8 #fixedViewTop 43 410 8720 98 20 0 10256 98 2 8 1140924416 1 10928 0 0 0 5 0 0 0 10928 0 8 4294902463 8802 8 #fillSessionList 8 'Update' 1 1 0 0 32 0 0 0 1106 202 208 98 3 1170 1200 98 2 530 1013 577 530 141 51 10928 1170 8992 98 1 32 10928 1170 5568 98 1 8 'Update' 10928 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 250 1 0 0 32 1 0 0 64 2 0 0 57 1 0 0] 98 0 1456 0 29 10754 10786 8 #fixedParentRight -139 10832 141 10864 -49 10896 51 410 8 ##(Smalltalk.ListView)  98 30 0 10256 98 2 8 1140920397 1025 11344 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 512 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 4 984134 2 ##(Smalltalk.CommandMenuItem)  1 8802 11024 8 '&Update' 1 1 0 0 0 11586 1 8802 8 #sendSigAbort 8 'Send Sig&Abort' 1 1 0 0 0 11586 1 8802 8 #sendSigUsr1 8 'Request &Stack' 1 1 0 0 0 11586 1 8802 8 #stopSession 8 'S&top Session' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 11344 0 8 4294903309 8 ##(Smalltalk.BasicListAbstract)  0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 20 920646 5 ##(Smalltalk.ListViewColumn)  8 '#' 61 8 #right 11872 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 459302 ##(Smalltalk.Context)  1 1 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each id]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  11472 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #id 17 257 0 0 11344 0 1 0 0 11954 8 'Serial' 111 12000 11872 12016 12034 0 12066 1 1 0 0 12098 1 9 12128 8 'doIt' 98 2 8 '[:each | each serial]' 98 1 202 12224 11472 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #serial 17 257 0 0 11344 0 1 0 0 11954 8 'User' 201 8 #left 11872 12016 12034 0 12066 1 1 0 0 12098 1 9 12128 8 'doIt' 98 2 8 '[:each | each name]' 98 1 202 12224 11472 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #name 17 257 0 0 11344 0 1 0 0 11954 8 'View Age' 131 12000 11872 12016 12034 0 0 12098 2 1 12032 8 'doIt' 8 '[:each | each viewAge]' 8 #[30 105 226 0 106] 8 #viewAge 12704 7 257 0 0 11344 0 1 0 0 11954 8 'Oldest' 121 8 #center 11872 12016 12034 0 12066 1 1 0 0 12098 1 9 12128 8 'doIt' 98 2 8 '[:each | each hasOldestCR]' 98 1 202 12224 11472 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #hasOldestCR 17 257 0 0 11344 0 1 0 0 11954 8 'Gem Host' 281 12496 11872 12016 12034 0 12066 1 1 0 0 12098 1 9 12128 8 'doIt' 98 2 8 '[:each | each host]' 98 1 202 12224 11472 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #host 17 257 0 0 11344 0 1 0 0 11954 8 'Process' 131 12000 11872 12016 12034 0 12066 1 1 0 0 12098 1 9 12128 8 'doIt' 98 2 8 '[:each | each process]' 98 1 202 12224 11472 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #process 17 257 0 0 11344 0 1 0 0 11954 8 'Primitive' 131 12000 11872 12016 12034 0 12066 1 1 0 0 12098 1 9 12128 8 'doIt' 98 2 8 '[:each | each primitive]' 98 1 202 12224 11472 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #primitive 17 257 0 0 11344 0 1 0 0 11954 8 'State' 111 12000 11872 12016 12034 0 12066 1 1 0 0 12098 1 9 12128 8 'doIt' 98 2 8 '[:each | each state]' 98 1 202 12224 11472 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #state 17 257 0 0 11344 0 1 0 0 11954 8 'Trans' 111 12000 11872 12016 12034 0 12066 1 1 0 0 12098 1 9 12128 8 'doIt' 98 2 8 '[:each | each transaction]' 98 1 202 12224 11472 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #transaction 17 257 0 0 11344 0 1 0 0 11954 8 'GCI IP' 241 12496 11872 12016 12034 0 12066 1 1 0 0 12098 1 9 12128 8 'doIt' 98 2 8 '[:each | each ip]' 98 1 202 12224 11472 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #ip 17 257 0 0 11344 0 1 0 0 11954 8 'Priority' 111 12000 459270 ##(Smalltalk.Message)  8 #displayString 98 0 14194 8 #<= 14240 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | each priority]' 8 #[30 105 226 0 106] 8 #priority 14288 7 257 0 0 11344 0 1 0 0 11954 8 'Host ID' 121 12000 14194 14224 14240 14194 14272 14240 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | each hostId]' 8 #[30 105 226 0 106] 8 #hostId 14448 7 257 0 0 11344 0 1 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | ''Unique host ID of the host where the session is running (an Integer)'']' 8 #[30 105 29 106] 8 'Unique host ID of the host where the session is running (an Integer)' 14544 7 257 0 0 11954 8 'Quiet' 121 12000 14194 14224 14240 14194 14272 14240 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | each quietTime]' 8 #[30 105 226 0 106] 8 #quietTime 14704 7 257 0 0 11344 0 1 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | ''Seconds since the session''''s most recent request to the stone'']' 8 #[30 105 29 106] 8 'Seconds since the session''s most recent request to the stone' 14800 7 257 0 0 11954 8 'Age' 121 12000 14194 14224 14240 14194 14272 14240 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | each lifeTime]' 8 #[30 105 226 0 106] 8 #lifeTime 14960 7 257 0 0 11344 0 1 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | ''Seconds since the session logged in'']' 8 #[30 105 29 106] 8 'Seconds since the session logged in' 15056 7 257 0 0 11954 8 'Backlog' 121 12000 14194 14224 14240 14194 14272 14240 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | each backlog]' 8 #[30 105 226 0 106] 8 #backlog 15216 7 257 0 0 11344 0 1 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | ''Number of commits which have occurred since the session obtained its view'']' 8 #[30 105 29 106] 8 'Number of commits which have occurred since the session obtained its view' 15312 7 257 0 0 11954 8 'Type' 201 12496 14194 14224 14240 14194 14272 14240 12034 0 0 12098 2 1 12032 8 'doIt' 8 '[:each | each description]' 8 #[30 105 226 0 106] 8 #description 15472 7 257 0 0 11344 0 1 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | ''Nil or a String describing a system or GC gem'']' 8 #[30 105 29 106] 8 'Nil or a String describing a system or GC gem' 15568 7 257 0 0 11954 8 'Objects' 121 12000 14194 14224 14240 14194 14272 14240 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | each objects]' 8 #[30 105 226 0 106] 8 #objects 15728 7 257 0 0 11344 0 1 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | ''Number of temporary (unused) object IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (unused) object IDs allocated to the session' 15824 7 257 0 0 11954 8 'Pages' 121 12000 14194 14224 14240 14194 14272 14240 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | each pages]' 8 #[30 105 226 0 106] 8 #pages 15984 7 257 0 0 11344 0 1 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | ''Number of temporary (non-persistent) page IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (non-persistent) page IDs allocated to the session' 16080 7 257 0 0 11954 8 'Voting' 121 12000 14194 14224 14240 14194 14272 14240 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | each voteState]' 8 #[30 105 226 0 106] 8 #voteState 16240 7 257 0 0 11344 0 1 12034 0 0 12098 2 1 12128 8 'doIt' 8 '[:each | ''0: session has not voted; 1: voting now; 2: voted'']' 8 #[30 105 29 106] 8 '0: session has not voted; 1: voting now; 2: voted' 16336 7 257 0 0 8 #report 11472 0 131169 0 0 1106 202 208 98 3 1170 1200 98 2 530 1 1 530 1153 577 11344 1170 8 #contextMenu: 98 1 11552 11344 1170 5568 98 1 8 '#' 11344 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 32 1 0 0] 98 0 1456 0 27 10754 10800 1 11312 1 10786 8 #fixedParentTop 1 10864 -49 410 5344 98 16 0 10256 98 2 8 1140850944 1 16752 0 0 0 5 0 0 0 16752 0 8 4294902501 1074 0 0 0 1106 202 208 98 2 1170 1200 98 2 530 1 1 530 1153 627 16752 1170 5568 98 1 8 'Error obtaining session list. Likely problem is that login user does not have permission to view session list.' 16752 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 57 1 0 0] 98 0 1456 0 27 10754 10800 1 11312 1 16720 1 10864 1 234 256 98 6 11344 8 'sessionList' 10368 8 'autoUpdate' 16752 8 'sessionListError' 0 1106 202 208 98 1 1170 1200 98 2 530 9 49 530 1153 627 10256 1394 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 68 2 0 0 81 1 0 0] 98 4 10928 11344 10368 16752 1456 0 27 786 8 'Workspace' 410 8 ##(Smalltalk.ReferenceView)  98 14 0 624 98 2 8 1140850688 131073 17344 0 482 8 4278190080 0 7 0 0 0 17344 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.CodeSourcePresenter)  8 #resource_Default_view 0 1106 202 208 98 1 1170 1200 98 2 530 9 49 530 1153 627 17344 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 68 2 0 0 81 1 0 0] 11472 1456 0 27 786 8 'Transcript' 410 848 98 15 0 624 98 2 8 1140850688 131073 17696 0 0 0 5 0 0 0 17696 562 234 240 98 4 410 8 ##(Smalltalk.Toolbar)  98 25 0 17696 98 2 8 1140851500 131137 17808 0 482 8 4278190080 0 517 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 17808 482 17904 8 4294903255 234 256 11472 234 256 98 38 40833 853766 ##(Smalltalk.ToolbarButton)  40833 0 17808 1 8802 8832 8 'Abort Transaction' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 530 1857 33 1 40835 18098 40835 0 17808 1 8802 9216 8 'Commit Transaction' 1 1 0 18176 27 40837 18098 40837 0 17808 1 8802 8 #jadeBrowseUsers 8 'Browse Users' 1 1 0 18176 75 40839 18098 40839 0 17808 1 8802 8 #jadeBrowseClasses 8 'Open System Browser' 1 1 0 18176 17 40841 18098 40841 0 17808 1 8802 8 #jadeBrowseMonticello 8 'Open Monticello Browser' 1 1 0 18176 3 40843 18098 40843 0 17808 1 8802 8 #jadeDisplay 8 'Print Result of Selection or Line' 1 1 0 18176 55 40845 18098 40845 0 17808 1 8802 8 #jadeExecute 8 'Evaluate Selection or Line' 1 1 0 18176 57 40847 18098 40847 0 17808 1 8802 8 #jadeInspect 8 'Inspect Selection or Line' 1 1 0 18176 59 40849 1246982 ##(Smalltalk.ToolbarSystemButton)  40849 0 17808 1 8802 8 #fileNew 8 'New Workspace' 1 1 0 1 13 40851 18738 40851 0 17808 1 8802 8 #fileOpen 8 'Open Workspace' 1 1 0 1 15 40853 18738 40853 0 17808 1 8802 8 #fileSave 8 'Save' 1 1 0 1 17 40855 18738 40855 0 17808 1 8802 8 #editCut 8 'Cut' 1 1 0 1 1 40857 18738 40857 0 17808 1 8802 8 #editCopy 8 'Copy' 1 1 0 1 3 40859 18738 40859 0 17808 1 8802 8 #editPaste 8 'Paste' 1 1 0 1 5 40861 18738 40861 0 17808 1 8802 8 #editDelete 8 'Delete' 1 1 0 1 11 40863 18738 40863 0 17808 1 8802 8 #undo 8 'Undo' 1 1 0 1 7 40865 18738 40865 0 17808 1 8802 8 #redo 8 'Redo' 1 1 0 1 9 40867 18738 40867 0 17808 1 8802 8 #editFind 8 'Find' 1 1 0 1 25 40869 18738 40869 0 17808 1 8802 8 #editReplace 8 'Replace' 1 1 0 1 27 98 24 18112 18304 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 17808 3 0 1 18352 18416 18480 19474 0 0 17808 3 0 1 18544 18608 18672 19474 0 0 17808 3 0 1 18752 18816 18880 19474 0 0 17808 3 0 1 18944 19008 19072 19136 19200 19264 19474 0 0 17808 3 0 1 19328 19392 234 240 98 4 1 117 18176 1 0 1 0 530 33 33 530 45 45 0 0 1106 202 208 98 2 1170 1200 98 2 530 1 1 530 1153 51 17808 1170 8 #updateSize 11472 17808 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 25 0 0 0] 98 0 1456 0 27 10754 10800 1 11312 1 16720 1 10896 51 410 8 ##(Smalltalk.ScintillaView)  98 46 0 17696 98 2 8 1445007428 1025 19840 10450 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 482 8 4278190080 0 5 0 0 0 19840 0 8 4294903141 1074 0 0 11 0 234 256 98 2 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 20064 0 0 0 98 40 20096 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  20064 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 786694 ##(Smalltalk.IndexedColor)  33554433 20242 33554471 19840 8 #circle 202 208 11472 0 63 9215 0 0 0 0 20242 33554447 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 20032 0 0 0 0 1 0 234 256 98 6 1 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  1 19840 65025 3 32 1 0 3 20434 3 19840 33423361 5 32 3 0 5 20434 5 19840 511 1 32 5 0 1106 202 208 98 8 1170 1200 98 2 530 1 51 530 1153 577 19840 1170 1280 98 1 1314 3 1 3 19840 1170 1360 98 1 32 19840 1170 8 #modificationEventMask: 98 1 9215 19840 1170 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 19840 1 3 32 1 20802 3 19840 33 1 16 67108863 20802 5 19840 1 1 16 -67108863 19840 1170 8 #indentationGuides: 98 1 0 19840 1170 8 #tabIndents: 98 1 16 19840 1170 8 #tabWidth: 98 1 9 19840 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 64 2 0 0 57 1 0 0] 98 0 1456 0 27 10754 10800 1 11312 1 16720 51 10864 1 234 256 98 2 19840 8 'transcript' 0 1106 202 208 98 1 1170 1200 98 2 530 9 49 530 1153 627 17696 1394 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 68 2 0 0 81 1 0 0] 98 2 17808 19840 1456 0 27 17344 234 256 98 8 10256 8 'All Sessions' 17696 8 'Transcript' 17344 8 'codePane' 832 8 'My Session' 0 410 8 ##(Smalltalk.TabView)  98 23 0 624 98 2 8 1140916736 1 21376 11426 202 208 98 4 17680 816 10240 17328 0 11504 721158 ##(Smalltalk.SystemColor)  31 0 1 0 0 0 21376 0 8 4294902623 11872 8 ##(Smalltalk.IconicListAbstract)  11904 0 0 0 0 0 8 #noIcons 1106 202 208 98 3 1170 1200 98 2 530 1 1 530 1169 683 21376 1170 8 #basicSelectionsByIndex: 98 1 98 1 9 21376 1170 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 21376 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 85 1 0 0] 98 0 1456 0 27 1106 202 208 98 1 1170 1200 98 2 530 1 1 530 1169 683 624 1394 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 85 1 0 0] 98 5 17696 832 10256 17344 21376 1456 0 27 10754 10800 1 11312 1 16720 1 10864 1 234 256 98 2 624 8 'cardContainer' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 11538 0 16 98 9 11586 1 8802 18784 8 '&New Workspace' 9373 1 0 0 0 11586 1 8802 18848 8 '&Open Workspace...' 9375 1 0 0 0 11586 1 8802 18912 8 '&Save' 9383 1 0 0 0 11586 1 8802 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 11586 1 8802 8 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 11586 1 8802 8 #resetCursor 8 'Reset &Cursor' 1025 1 0 0 0 22434 4097 11586 1 8802 8 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 40765 0 0 11538 0 16 98 15 11586 1 8802 19232 8 '&Undo' 9397 1 0 0 0 11586 1 8802 19296 8 'R&edo' 9395 1 0 0 0 22434 4097 11586 1 8802 18976 8 'Cu&t' 9393 1 0 0 0 11586 1 8802 19040 8 '&Copy' 9351 1 0 0 0 11586 1 8802 19104 8 '&Paste' 9389 1 0 0 0 11586 1 8802 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 11586 1 8802 19168 8 '&Delete' 1629 1 0 0 0 22434 4097 11586 1 8802 19360 8 '&Find...' 9357 1 0 0 0 11586 1 8802 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 11586 1 8802 19424 8 '&Replace...' 9361 1 0 0 0 22434 4097 11586 1 8802 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 11586 1 8802 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 40791 0 0 11538 0 16 98 21 11586 1 8802 8 #sleepAndCommit 8 '&Sleep and Commit' 1 1 0 0 0 11586 1 8802 8832 8 '&Abort Transaction' 1 1 0 0 0 11586 1 8802 9216 8 '&Commit Transaction' 1 1 0 0 0 11586 1 8802 8 #disableNativeCode 8 'Disable Naitive Code' 1 1 0 0 0 22434 4097 11586 1 8802 18704 8 '&Inspect' 9379 1 0 0 0 11586 1 8802 18576 8 '&Display' 9353 1 0 0 0 11586 1 8802 18640 8 '&Execute' 9355 1 0 0 0 11586 1 8802 8 #fileIn 8 'Fi&le In' 1 1 0 0 0 22434 4097 11586 1 8802 18448 8 '&Browse Classes' 9349 1 0 0 0 11586 1 8802 8 #browseMonticelloRepositories 8 'Browser &Monticello Repositories' 9371 1 0 0 0 11586 1 8802 8 #jadeBrowseObjectLog 8 'Browse &Object Log' 9369 1 0 0 0 11586 1 8802 8 #browseProcesses 8 'Browse &Processes' 9377 1 0 0 0 11586 1 8802 8 #autocompletion 8 'Configure Autocompletion' 1 1 0 0 0 22434 4097 11586 1 8802 8 #inspectOop 8 'Inspect Oop ...' 1 1 0 0 0 11586 1 8802 8 #describeErrorNumber 8 'Description for Error Number ...' 1025 1 0 0 0 22434 4097 11586 1025 8802 8 #turnAutoMigrateOff 8 'Turn Auto Migrate Off' 1 1 0 0 0 11586 1025 8802 8 #turnAutoMigrateOn 8 'Turn Auto Migrate On' 1 1 0 0 0 8 '&Jade' 0 1 0 0 40827 0 0 11538 0 16 98 1 11586 1 8802 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 40831 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 263494 3 ##(Smalltalk.Icon)  0 16 18208 8 'icons\GS32x32.ico' 0 24594 0 16 18208 8 'icons\GS16x16.ico' 0 0 0 1 0 0 1106 202 208 98 3 1170 1200 98 2 530 2879 21 530 1201 801 416 1170 5568 98 1 8 'Jade Session' 416 1170 8 #updateMenuBar 11472 416 1394 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 98 1 624 1456 0 27 )! !
!JadeTranscript class categoriesFor: #resource_Default_view!public!resources-views! !

