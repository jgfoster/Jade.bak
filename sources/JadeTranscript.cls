"Filed out from Dolphin Smalltalk 7"!

JadeWorkspace subclass: #JadeTranscript
	instanceVariableNames: 'autoUpdatePresenter autoUpdateProcess mySessionPresenter sessionListPresenter sessionListErrorPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeTranscript guid: (GUID fromString: '{7D871BC4-1338-4BA1-AF6A-C1EDD936D683}')!
JadeTranscript comment: ''!
!JadeTranscript categoriesForClass!Unclassified! !
!JadeTranscript methodsFor!

_mySessionPresenter

	^mySessionPresenter.
!

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
	autoUpdatePresenter		:= self add: BooleanPresenter	new name: 'autoUpdate'.
	mySessionPresenter		:= self add: MySessionPresenter	new name: 'My Session'.
	sessionListPresenter 		:= self add: ListPresenter		new name: 'sessionList'.
	sessionListErrorPresenter 	:= self add: TextPresenter		new name: 'sessionListError'.

!

createSchematicWiring

	super createSchematicWiring.
	(self view viewNamed: 'cardContainer') when: #'currentCardChanged' send: #'currentCardChanged' to: self.
	autoUpdatePresenter	when: #'valueChanged' send: #'autoUpdateChanged' to: self.!

currentCardChanged

	| currentCard |
	currentCard := (self view viewNamed: 'cardContainer') currentCard.
	(currentCard isKindOf: TextEdit)  ifTrue: [currentCard setFocus].
	(currentCard name = 'codePane') ifTrue: [
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

	self error: 'moved to MySessionPresenter'.!

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

gciSession: aGciSession

	super gciSession: aGciSession.
	mySessionPresenter gciSession: aGciSession.
!

hardBreak

self error: 'moved to MySessionPresenter'.
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

model: anObject

	super model: anObject.
	(anObject isKindOf: GciSession) ifTrue: [mySessionPresenter gciSession: anObject].!

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
	(gciSession indexOfClientForwarder: self) = 2 ifFalse: [self error: 'Transcript should be the second client forwarder!!'].
	"autoUpdatePresenter value: false."
!

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

	self error: 'moved to MySessionPresenter'.!

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

	^self view viewNamed: 'Transcript'.
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
!JadeTranscript categoriesFor: #_mySessionPresenter!private! !
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
!JadeTranscript categoriesFor: #gciSession:!private! !
!JadeTranscript categoriesFor: #hardBreak!public! !
!JadeTranscript categoriesFor: #inspectOop!event handling!public! !
!JadeTranscript categoriesFor: #invalidateUserInterface!public!updating! !
!JadeTranscript categoriesFor: #model:!public! !
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 2 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1140850688 131073 624 0 482 512 0 7 0 0 0 624 655878 ##(Smalltalk.CardLayout)  202 208 98 4 721414 ##(Smalltalk.Association)  8 'All Sessions' 410 8 ##(Smalltalk.ContainerView)  98 15 0 624 98 2 8 1140850688 131073 832 0 0 0 5 0 0 0 832 562 234 240 98 8 410 8 ##(Smalltalk.ListView)  98 30 0 832 98 2 8 1140920397 1025 960 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 512 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 4 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #fillSessionList 8 '&Update' 1 1 0 0 0 1202 1 1234 8 #sendSigAbort 8 'Send Sig&Abort' 1 1 0 0 0 1202 1 1234 8 #sendSigUsr1 8 'Request &Stack' 1 1 0 0 0 1202 1 1234 8 #stopSession 8 'S&top Session' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 960 0 8 4294902401 8 ##(Smalltalk.BasicListAbstract)  0 1049926 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 20 920646 5 ##(Smalltalk.ListViewColumn)  8 '#' 61 8 #right 1520 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 459302 ##(Smalltalk.Context)  1 1 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each id]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  1088 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #id 17 257 0 0 960 0 1 0 0 1602 8 'Serial' 111 1648 1520 1664 1682 0 1714 1 1 0 0 1746 1 9 1776 8 'doIt' 98 2 8 '[:each | each serial]' 98 1 202 1872 1088 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #serial 17 257 0 0 960 0 1 0 0 1602 8 'User' 201 8 #left 1520 1664 1682 0 1714 1 1 0 0 1746 1 9 1776 8 'doIt' 98 2 8 '[:each | each name]' 98 1 202 1872 1088 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #name 17 257 0 0 960 0 1 0 0 1602 8 'View Age' 131 1648 1520 1664 1682 0 0 1746 2 1 1680 8 'doIt' 8 '[:each | each viewAge]' 8 #[30 105 226 0 106] 8 #viewAge 2352 7 257 0 0 960 0 1 0 0 1602 8 'Oldest' 121 8 #center 1520 1664 1682 0 1714 1 1 0 0 1746 1 9 1776 8 'doIt' 98 2 8 '[:each | each hasOldestCR]' 98 1 202 1872 1088 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #hasOldestCR 17 257 0 0 960 0 1 0 0 1602 8 'Gem Host' 281 2144 1520 1664 1682 0 1714 1 1 0 0 1746 1 9 1776 8 'doIt' 98 2 8 '[:each | each host]' 98 1 202 1872 1088 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #host 17 257 0 0 960 0 1 0 0 1602 8 'Process' 131 1648 1520 1664 1682 0 1714 1 1 0 0 1746 1 9 1776 8 'doIt' 98 2 8 '[:each | each process]' 98 1 202 1872 1088 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #process 17 257 0 0 960 0 1 0 0 1602 8 'Primitive' 131 1648 1520 1664 1682 0 1714 1 1 0 0 1746 1 9 1776 8 'doIt' 98 2 8 '[:each | each primitive]' 98 1 202 1872 1088 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #primitive 17 257 0 0 960 0 1 0 0 1602 8 'State' 111 1648 1520 1664 1682 0 1714 1 1 0 0 1746 1 9 1776 8 'doIt' 98 2 8 '[:each | each state]' 98 1 202 1872 1088 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #state 17 257 0 0 960 0 1 0 0 1602 8 'Trans' 111 1648 1520 1664 1682 0 1714 1 1 0 0 1746 1 9 1776 8 'doIt' 98 2 8 '[:each | each transaction]' 98 1 202 1872 1088 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #transaction 17 257 0 0 960 0 1 0 0 1602 8 'GCI IP' 241 2144 1520 1664 1682 0 1714 1 1 0 0 1746 1 9 1776 8 'doIt' 98 2 8 '[:each | each ip]' 98 1 202 1872 1088 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #ip 17 257 0 0 960 0 1 0 0 1602 8 'Priority' 111 1648 459270 ##(Smalltalk.Message)  8 #displayString 98 0 3842 8 #<= 3888 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | each priority]' 8 #[30 105 226 0 106] 8 #priority 3936 7 257 0 0 960 0 1 0 0 1602 8 'Host ID' 121 1648 3842 3872 3888 3842 3920 3888 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | each hostId]' 8 #[30 105 226 0 106] 8 #hostId 4096 7 257 0 0 960 0 1 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | ''Unique host ID of the host where the session is running (an Integer)'']' 8 #[30 105 29 106] 8 'Unique host ID of the host where the session is running (an Integer)' 4192 7 257 0 0 1602 8 'Quiet' 121 1648 3842 3872 3888 3842 3920 3888 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | each quietTime]' 8 #[30 105 226 0 106] 8 #quietTime 4352 7 257 0 0 960 0 1 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | ''Seconds since the session''''s most recent request to the stone'']' 8 #[30 105 29 106] 8 'Seconds since the session''s most recent request to the stone' 4448 7 257 0 0 1602 8 'Age' 121 1648 3842 3872 3888 3842 3920 3888 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | each lifeTime]' 8 #[30 105 226 0 106] 8 #lifeTime 4608 7 257 0 0 960 0 1 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | ''Seconds since the session logged in'']' 8 #[30 105 29 106] 8 'Seconds since the session logged in' 4704 7 257 0 0 1602 8 'Backlog' 121 1648 3842 3872 3888 3842 3920 3888 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | each backlog]' 8 #[30 105 226 0 106] 8 #backlog 4864 7 257 0 0 960 0 1 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | ''Number of commits which have occurred since the session obtained its view'']' 8 #[30 105 29 106] 8 'Number of commits which have occurred since the session obtained its view' 4960 7 257 0 0 1602 8 'Type' 201 2144 3842 3872 3888 3842 3920 3888 1682 0 0 1746 2 1 1680 8 'doIt' 8 '[:each | each description]' 8 #[30 105 226 0 106] 8 #description 5120 7 257 0 0 960 0 1 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | ''Nil or a String describing a system or GC gem'']' 8 #[30 105 29 106] 8 'Nil or a String describing a system or GC gem' 5216 7 257 0 0 1602 8 'Objects' 121 1648 3842 3872 3888 3842 3920 3888 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | each objects]' 8 #[30 105 226 0 106] 8 #objects 5376 7 257 0 0 960 0 1 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | ''Number of temporary (unused) object IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (unused) object IDs allocated to the session' 5472 7 257 0 0 1602 8 'Pages' 121 1648 3842 3872 3888 3842 3920 3888 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | each pages]' 8 #[30 105 226 0 106] 8 #pages 5632 7 257 0 0 960 0 1 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | ''Number of temporary (non-persistent) page IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (non-persistent) page IDs allocated to the session' 5728 7 257 0 0 1602 8 'Voting' 121 1648 3842 3872 3888 3842 3920 3888 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | each voteState]' 8 #[30 105 226 0 106] 8 #voteState 5888 7 257 0 0 960 0 1 1682 0 0 1746 2 1 1776 8 'doIt' 8 '[:each | ''0: session has not voted; 1: voting now; 2: voted'']' 8 #[30 105 29 106] 8 '0: session has not voted; 1: voting now; 2: voted' 5984 7 257 0 0 8 #report 1088 0 131169 0 98 1 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 1153 571 960 6178 8 #contextMenu: 98 1 1168 960 6178 8 #text: 98 1 8 '#' 960 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 29 1 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 6498 8 #fixedParentRight 1 6498 8 #fixedParentTop 1 6498 8 #fixedParentBottom -49 410 8 ##(Smalltalk.PushButton)  98 20 0 832 98 2 8 1140924416 1 6640 0 0 0 5 0 0 0 6640 0 8 4294903405 1234 1264 8 'Update' 1 1 0 0 32 0 0 0 6114 202 208 98 3 6178 6208 98 2 530 1013 571 530 141 51 6640 6178 8 #isEnabled: 98 1 32 6640 6178 6336 98 1 8 'Update' 6640 6386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 250 1 0 0 29 1 0 0 64 2 0 0 54 1 0 0] 98 0 6448 0 29 6466 6544 -139 6498 8 #fixedViewLeft 141 6608 -49 6498 8 #fixedViewTop 51 410 8 ##(Smalltalk.CheckBox)  98 16 0 832 98 2 8 1409363203 1 7104 721990 2 ##(Smalltalk.ValueHolder)  0 0 1114118 ##(Smalltalk.NeverSearchPolicy)  32 0 0 5 0 0 0 7104 0 8 4294903405 852486 ##(Smalltalk.NullConverter)  0 0 0 6114 202 208 98 2 6178 6208 98 2 530 11 571 530 227 43 7104 6178 6336 98 1 8 'Auto-update' 7104 6386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 29 1 0 0 118 0 0 0 50 1 0 0] 98 0 6448 0 27 6466 6512 11 7040 227 6608 -49 7072 43 410 8 ##(Smalltalk.StaticText)  98 16 0 832 98 2 8 1140850944 1 7520 0 0 0 5 0 0 0 7520 0 8 4294902455 7266 0 0 0 6114 202 208 98 2 6178 6208 98 2 530 1 1 530 1153 621 7520 6178 6336 98 1 8 'Error obtaining session list. Likely problem is that login user does not have permission to view session list.' 7520 6386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 54 1 0 0] 98 0 6448 0 27 6466 6512 1 6544 1 6576 1 6608 1 234 256 98 6 7104 8 'autoUpdate' 960 8 'sessionList' 7520 8 'sessionListError' 0 6114 202 208 98 1 6178 6208 98 2 530 9 55 530 1153 621 832 6386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 81 1 0 0] 98 4 6640 960 7104 7520 6448 0 27 786 8 'Workspace' 410 8 ##(Smalltalk.ReferenceView)  98 14 0 624 98 2 8 1140850688 131073 8128 0 482 8 4278190080 0 7 0 0 0 8128 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.CodeSourcePresenter)  8 #resource_Default_view 0 6114 202 208 98 1 6178 6208 98 2 530 9 55 530 1153 621 8128 6386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 81 1 0 0] 1088 6448 0 27 786 8 'Transcript' 410 848 98 15 0 624 98 2 8 1140850688 131073 8480 0 0 0 5 0 0 0 8480 562 234 240 98 4 410 8 ##(Smalltalk.Toolbar)  98 25 0 8480 98 2 8 1140851500 131137 8592 0 482 8 4278190080 0 517 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 8592 482 8688 8 4294902561 234 256 1088 234 256 98 38 21465 853766 ##(Smalltalk.ToolbarButton)  21465 0 8592 1 1234 8 #abortTransaction 8 'Abort Transaction' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr7.dll' 0 0 7 530 1857 33 1 21467 8882 21467 0 8592 1 1234 8 #commitTransaction 8 'Commit Transaction' 1 1 0 8976 27 21469 8882 21469 0 8592 1 1234 8 #jadeBrowseUsers 8 'Browse Users' 1 1 0 8976 75 21471 8882 21471 0 8592 1 1234 8 #jadeBrowseClasses 8 'Open System Browser' 1 1 0 8976 17 21473 8882 21473 0 8592 1 1234 8 #jadeBrowseMonticello 8 'Open Monticello Browser' 1 1 0 8976 3 21475 8882 21475 0 8592 1 1234 8 #jadeDisplay 8 'Print Result of Selection or Line' 1 1 0 8976 55 21477 8882 21477 0 8592 1 1234 8 #jadeExecute 8 'Evaluate Selection or Line' 1 1 0 8976 57 21479 8882 21479 0 8592 1 1234 8 #jadeInspect 8 'Inspect Selection or Line' 1 1 0 8976 59 21481 1246982 ##(Smalltalk.ToolbarSystemButton)  21481 0 8592 1 1234 8 #fileNew 8 'New Workspace' 1 1 0 1 13 21483 9554 21483 0 8592 1 1234 8 #fileOpen 8 'Open Workspace' 1 1 0 1 15 21485 9554 21485 0 8592 1 1234 8 #fileSave 8 'Save' 1 1 0 1 17 21487 9554 21487 0 8592 1 1234 8 #editCut 8 'Cut' 1 1 0 1 1 21489 9554 21489 0 8592 1 1234 8 #editCopy 8 'Copy' 1 1 0 1 3 21491 9554 21491 0 8592 1 1234 8 #editPaste 8 'Paste' 1 1 0 1 5 21493 9554 21493 0 8592 1 1234 8 #editDelete 8 'Delete' 1 1 0 1 11 21495 9554 21495 0 8592 1 1234 8 #undo 8 'Undo' 1 1 0 1 7 21497 9554 21497 0 8592 1 1234 8 #redo 8 'Redo' 1 1 0 1 9 21499 9554 21499 0 8592 1 1234 8 #editFind 8 'Find' 1 1 0 1 25 21501 9554 21501 0 8592 1 1234 8 #editReplace 8 'Replace' 1 1 0 1 27 98 24 8896 9104 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 8592 3 0 1 9168 9232 9296 10290 0 0 8592 3 0 1 9360 9424 9488 10290 0 0 8592 3 0 1 9568 9632 9696 10290 0 0 8592 3 0 1 9760 9824 9888 9952 10016 10080 10290 0 0 8592 3 0 1 10144 10208 234 240 98 4 1 117 8976 1 0 1 0 530 33 33 530 45 45 0 0 6114 202 208 98 2 6178 6208 98 2 530 1 1 530 1153 51 8592 6178 8 #updateSizePosted 1088 8592 6386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 25 0 0 0] 98 0 6448 0 27 6466 6512 1 6544 1 6576 1 7072 51 410 8 ##(Smalltalk.ScintillaView)  98 50 0 8480 98 2 8 1445007428 1025 10656 7186 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 482 8 4278190080 0 5 0 0 0 10656 0 8 4294902461 7266 0 0 11 0 234 256 98 4 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 10880 0 0 0 8 #callTip 10898 77 786694 ##(Smalltalk.IndexedColor)  33554457 10962 33554471 1 0 0 0 0 10928 0 0 0 98 40 10912 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  10880 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 10962 33554433 10992 10656 8 #circle 202 208 1088 0 63 9215 0 0 0 0 10962 33554447 0 0 65 0 0 0 8 '' 3 234 256 98 2 8 #container 10848 0 0 0 0 1 0 234 256 98 6 1 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  1 10656 65025 3 32 1 0 3 11298 3 10656 33423361 5 32 3 0 5 11298 5 10656 511 1 32 5 0 0 0 234 256 98 6 8 #Notification 10898 1029 0 196934 1 ##(Smalltalk.RGB)  31185883 1 0 0 0 0 11392 0 0 0 8 #Error 10898 1031 11426 335 11426 31317503 1 0 0 0 0 11456 0 0 0 8 #Warning 10898 1027 11426 60239 11426 31326207 1 0 0 0 0 11520 0 0 0 0 6114 202 208 98 11 6178 6208 98 2 530 1 51 530 1153 571 10656 6178 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 10656 6178 8 #isTextModified: 98 1 32 10656 6178 8 #modificationEventMask: 98 1 9215 10656 6178 8 #secondaryCaretForecolor: 98 1 11426 16711423 10656 6178 8 #caretPeriod: 98 1 1061 10656 6178 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin)  1 10656 1 3 32 1 5 12050 3 10656 33 1 16 67108863 5 12050 5 10656 1 1 16 -67108863 5 10656 6178 8 #indentationGuides: 98 1 0 10656 6178 8 #tabIndents: 98 1 16 10656 6178 8 #tabWidth: 98 1 9 10656 6178 8 #areAdditionalCaretsVisible: 98 1 16 10656 6386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 64 2 0 0 54 1 0 0] 98 0 6448 0 29 6466 6512 1 6544 1 6576 51 6608 1 234 256 98 2 10656 8 'transcript' 0 6114 202 208 98 1 6178 6208 98 2 530 9 55 530 1153 621 8480 6386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 81 1 0 0] 98 2 8592 10656 6448 0 27 786 8 'My Session' 410 8144 98 14 0 624 98 2 8 1140850688 131073 12608 0 0 0 5 0 0 0 12608 8242 8 ##(Smalltalk.MySessionPresenter)  8288 0 6114 202 208 98 1 6178 6208 98 2 530 9 55 530 1153 621 12608 6386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 68 2 0 0 81 1 0 0] 1088 6448 0 27 8128 234 256 98 8 8480 8 'Transcript' 8128 8 'codePane' 832 8 'All Sessions' 12608 8 'My Session' 0 410 8 ##(Smalltalk.TabView)  98 23 0 624 98 2 8 1140916736 1 12944 1042 202 208 98 4 8464 12592 816 8112 0 1120 721158 ##(Smalltalk.SystemColor)  31 0 1 0 0 0 12944 0 8 4294902167 1520 8 ##(Smalltalk.IconicListAbstract)  1552 0 0 0 0 0 8 #noIcons 6114 202 208 98 3 6178 6208 98 2 530 1 1 530 1169 683 12944 6178 8 #basicSelectionsByIndex: 98 1 98 1 9 12944 6178 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 12944 6386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 85 1 0 0] 98 0 6448 0 27 6114 202 208 98 1 6178 6208 98 2 530 1 1 530 1169 683 624 6386 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 85 1 0 0] 98 5 8480 12608 832 8128 12944 6448 0 27 6466 6512 1 6544 1 6576 1 6608 1 234 256 98 2 624 8 'cardContainer' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 4 1154 0 16 98 9 1202 1 1234 9600 8 '&New Workspace' 9373 1 0 0 0 1202 1 1234 9664 8 '&Open Workspace...' 9375 1 0 0 0 1202 1 1234 9728 8 '&Save' 9383 1 0 0 0 1202 1 1234 8 #fileSaveAs 8 'Save &As...' 1 1 0 0 0 1202 1 1234 8 #fileRevert 8 '&Revert' 1025 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 1202 1 1234 8 #resetCursor 8 'Reset &Cursor' 1025 1 0 0 0 14002 4097 1202 1 1234 8 #exit 8 'E&xit Jade' 17639 1 0 0 0 8 '&File' 0 1 0 0 21397 0 0 1154 0 16 98 15 1202 1 1234 10048 8 '&Undo' 9397 1 0 0 0 1202 1 1234 10112 8 'R&edo' 9395 1 0 0 0 14002 4097 1202 1 1234 9792 8 'Cu&t' 9393 1 0 0 0 1202 1 1234 9856 8 '&Copy' 9351 1 0 0 0 1202 1 1234 9920 8 '&Paste' 9389 1 0 0 0 1202 1 1234 8 #editSelectAll 8 'Select &All' 9347 1 0 0 0 1202 1 1234 9984 8 '&Delete' 1629 1 0 0 0 14002 4097 1202 1 1234 10176 8 '&Find...' 9357 1 0 0 0 1202 1 1234 8 #editFindNext 8 'Find &Next' 9359 1 0 0 0 1202 1 1234 10240 8 '&Replace...' 9361 1 0 0 0 14002 4097 1202 1 1234 8 #addQuotesToSelection 8 'Add &Quotes' 1 1 0 0 0 1202 1 1234 8 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 0 0 0 8 '&Edit' 0 1 0 0 21423 0 0 1154 0 16 98 21 1202 1 1234 8 #sleepAndCommit 8 '&Sleep and Commit' 1 1 0 0 0 1202 1 1234 8928 8 '&Abort Transaction' 1 1 0 0 0 1202 1 1234 9136 8 '&Commit Transaction' 1 1 0 0 0 1202 1 1234 8 #disableNativeCode 8 'Disable Naitive Code' 1 1 0 0 0 14002 4097 1202 1 1234 9520 8 '&Inspect' 9379 1 0 0 0 1202 1 1234 9392 8 '&Display' 9353 1 0 0 0 1202 1 1234 9456 8 '&Execute' 9355 1 0 0 0 1202 1 1234 8 #fileIn 8 'Fi&le In' 1 1 0 0 0 14002 4097 1202 1 1234 9264 8 '&Browse Classes' 9349 1 0 0 0 1202 1 1234 8 #browseMonticelloRepositories 8 'Browser &Monticello Repositories' 9371 1 0 0 0 1202 1 1234 8 #jadeBrowseObjectLog 8 'Browse &Object Log' 9369 1 0 0 0 1202 1 1234 8 #browseProcesses 8 'Browse &Processes' 9377 1 0 0 0 1202 1 1234 8 #autocompletion 8 'Configure Autocompletion' 1 1 0 0 0 14002 4097 1202 1 1234 8 #inspectOop 8 'Inspect Oop ...' 1 1 0 0 0 1202 1 1234 8 #describeErrorNumber 8 'Description for Error Number ...' 1025 1 0 0 0 14002 4097 1202 1025 1234 8 #turnAutoMigrateOff 8 'Turn Auto Migrate Off' 1 1 0 0 0 1202 1025 1234 8 #turnAutoMigrateOn 8 'Turn Auto Migrate On' 1 1 0 0 0 8 '&Jade' 0 1 0 0 21459 0 0 1154 0 16 98 1 1202 1 1234 8 #aboutJade 8 '&About Jade' 1 1 0 0 0 8 '&Help' 0 1 0 0 21463 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 263494 3 ##(Smalltalk.Icon)  0 16 9008 8 'icons\GS32x32.ico' 0 16162 0 16 9008 8 'icons\GS16x16.ico' 0 0 0 1 0 0 6114 202 208 98 3 6178 6208 98 2 530 5119 21 530 1201 801 416 6178 6336 98 1 8 'Jade Session' 416 6178 8 #updateMenuBar 1088 416 6386 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 154 1 0 0] 98 1 624 6448 0 27 )! !
!JadeTranscript class categoriesFor: #resource_Default_view!public!resources-views! !

