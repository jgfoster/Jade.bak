"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #AllSessionsPresenter
	instanceVariableNames: 'autoUpdatePresenter autoUpdateProcess sessionListPresenter sessionListErrorPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AllSessionsPresenter guid: (GUID fromString: '{238d40de-7e96-4f13-81e3-92ccdca57c55}')!
AllSessionsPresenter comment: ''!
!AllSessionsPresenter categoriesForClass!Unclassified! !
!AllSessionsPresenter methodsFor!

aboutToClose

	self terminateAutoUpdate.
!

autoUpdateChanged

	autoUpdatePresenter model value ifTrue: [
		self fillSessionListRegularly.
	] ifFalse: [
		self terminateAutoUpdate.
	].
!

createComponents

	super createComponents.
	autoUpdatePresenter		:= self add: BooleanPresenter	new name: 'autoUpdate'.
	sessionListPresenter 		:= self add: ListPresenter		new name: 'sessionList'.
	sessionListErrorPresenter 	:= self add: TextPresenter		new name: 'sessionListError'.

!

createSchematicWiring

	super createSchematicWiring.
	autoUpdatePresenter	when: #'valueChanged' send: #'autoUpdateChanged' to: self.
!

fillSessionList

	| string list |
	[
		string := gciSession serverPerform: #'allSessions'.
		string := gciSession decode: string.
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

queryCommand: query

	(#(#stopSession) includes: query commandSymbol) ifTrue: [
		query isEnabled: sessionListPresenter hasSelection.
		^true.
	].
	^super queryCommand: query.

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

stopSession

	| gsSession |
	(gsSession := sessionListPresenter selectionOrNil) isNil ifTrue: [^self].
	(MessageBox confirm: 'Stop Session?') ifFalse: [^self].
	gsSession stopSession.
	(Delay forSeconds: 1) wait.
	self fillSessionList.
!

terminateAutoUpdate

	autoUpdateProcess notNil ifTrue: [
		autoUpdateProcess terminate.
		autoUpdateProcess := nil.
	].
! !
!AllSessionsPresenter categoriesFor: #aboutToClose!public! !
!AllSessionsPresenter categoriesFor: #autoUpdateChanged!public! !
!AllSessionsPresenter categoriesFor: #createComponents!public! !
!AllSessionsPresenter categoriesFor: #createSchematicWiring!public! !
!AllSessionsPresenter categoriesFor: #fillSessionList!public!updating! !
!AllSessionsPresenter categoriesFor: #fillSessionListRegularly!public!updating! !
!AllSessionsPresenter categoriesFor: #queryCommand:!public! !
!AllSessionsPresenter categoriesFor: #sendSigAbort!public!updating! !
!AllSessionsPresenter categoriesFor: #sendSigUsr1!public!updating! !
!AllSessionsPresenter categoriesFor: #stopSession!public!updating! !
!AllSessionsPresenter categoriesFor: #terminateAutoUpdate!public! !

!AllSessionsPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 8 410 8 ##(Smalltalk.CheckBox)  98 16 0 416 98 2 8 1409363203 1 560 721990 2 ##(Smalltalk.ValueHolder)  0 0 1114118 ##(Smalltalk.NeverSearchPolicy)  32 0 0 5 0 0 0 560 0 8 4294903587 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  3 571 882 201 45 560 818 8 #text: 98 1 8 'Auto-update' 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 29 1 0 0 101 0 0 0 51 1 0 0] 98 0 882 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 3 1106 8 #fixedViewLeft 201 1106 8 #fixedParentBottom -49 1106 8 #fixedViewTop 45 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 1248 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  524550 ##(Smalltalk.ColorRef)  8 4278190080 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 4 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #fillSessionList 8 '&Update' 1 1 0 0 0 1522 1 1554 8 #sendSigAbort 8 'Send Sig&Abort' 1 1 0 0 0 1522 1 1554 8 #sendSigUsr1 8 'Request &Stack' 1 1 0 0 0 1522 1 1554 8 #stopSession 8 'S&top Session' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 1248 0 8 4294902185 8 ##(Smalltalk.BasicListAbstract)  0 1049926 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 20 920646 5 ##(Smalltalk.ListViewColumn)  8 '#' 61 8 #right 1840 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 459302 ##(Smalltalk.Context)  1 1 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each id]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  1376 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #id 17 257 0 0 1248 0 1 0 0 1922 8 'Serial' 111 1968 1840 1984 2002 0 2034 1 1 0 0 2066 1 9 2096 8 'doIt' 98 2 8 '[:each | each serial]' 98 1 202 2192 1376 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #serial 17 257 0 0 1248 0 1 0 0 1922 8 'User' 201 8 #left 1840 1984 2002 0 2034 1 1 0 0 2066 1 9 2096 8 'doIt' 98 2 8 '[:each | each name]' 98 1 202 2192 1376 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #name 17 257 0 0 1248 0 1 0 0 1922 8 'View Age' 131 1968 1840 1984 2002 0 0 2066 2 1 2000 8 'doIt' 8 '[:each | each viewAge]' 8 #[30 105 226 0 106] 8 #viewAge 2672 7 257 0 0 1248 0 1 0 0 1922 8 'Oldest' 121 8 #center 1840 1984 2002 0 2034 1 1 0 0 2066 1 9 2096 8 'doIt' 98 2 8 '[:each | each hasOldestCR]' 98 1 202 2192 1376 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #hasOldestCR 17 257 0 0 1248 0 1 0 0 1922 8 'Gem Host' 281 2464 1840 1984 2002 0 2034 1 1 0 0 2066 1 9 2096 8 'doIt' 98 2 8 '[:each | each host]' 98 1 202 2192 1376 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #host 17 257 0 0 1248 0 1 0 0 1922 8 'Process' 131 1968 1840 1984 2002 0 2034 1 1 0 0 2066 1 9 2096 8 'doIt' 98 2 8 '[:each | each process]' 98 1 202 2192 1376 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #process 17 257 0 0 1248 0 1 0 0 1922 8 'Primitive' 131 1968 1840 1984 2002 0 2034 1 1 0 0 2066 1 9 2096 8 'doIt' 98 2 8 '[:each | each primitive]' 98 1 202 2192 1376 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #primitive 17 257 0 0 1248 0 1 0 0 1922 8 'State' 111 1968 1840 1984 2002 0 2034 1 1 0 0 2066 1 9 2096 8 'doIt' 98 2 8 '[:each | each state]' 98 1 202 2192 1376 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #state 17 257 0 0 1248 0 1 0 0 1922 8 'Trans' 111 1968 1840 1984 2002 0 2034 1 1 0 0 2066 1 9 2096 8 'doIt' 98 2 8 '[:each | each transaction]' 98 1 202 2192 1376 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #transaction 17 257 0 0 1248 0 1 0 0 1922 8 'GCI IP' 241 2464 1840 1984 2002 0 2034 1 1 0 0 2066 1 9 2096 8 'doIt' 98 2 8 '[:each | each ip]' 98 1 202 2192 1376 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] 8 #ip 17 257 0 0 1248 0 1 0 0 1922 8 'Priority' 111 1968 459270 ##(Smalltalk.Message)  8 #displayString 98 0 4162 8 #<= 4208 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | each priority]' 8 #[30 105 226 0 106] 8 #priority 4256 7 257 0 0 1248 0 1 0 0 1922 8 'Host ID' 121 1968 4162 4192 4208 4162 4240 4208 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | each hostId]' 8 #[30 105 226 0 106] 8 #hostId 4416 7 257 0 0 1248 0 1 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | ''Unique host ID of the host where the session is running (an Integer)'']' 8 #[30 105 29 106] 8 'Unique host ID of the host where the session is running (an Integer)' 4512 7 257 0 0 1922 8 'Quiet' 121 1968 4162 4192 4208 4162 4240 4208 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | each quietTime]' 8 #[30 105 226 0 106] 8 #quietTime 4672 7 257 0 0 1248 0 1 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | ''Seconds since the session''''s most recent request to the stone'']' 8 #[30 105 29 106] 8 'Seconds since the session''s most recent request to the stone' 4768 7 257 0 0 1922 8 'Age' 121 1968 4162 4192 4208 4162 4240 4208 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | each lifeTime]' 8 #[30 105 226 0 106] 8 #lifeTime 4928 7 257 0 0 1248 0 1 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | ''Seconds since the session logged in'']' 8 #[30 105 29 106] 8 'Seconds since the session logged in' 5024 7 257 0 0 1922 8 'Backlog' 121 1968 4162 4192 4208 4162 4240 4208 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | each backlog]' 8 #[30 105 226 0 106] 8 #backlog 5184 7 257 0 0 1248 0 1 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | ''Number of commits which have occurred since the session obtained its view'']' 8 #[30 105 29 106] 8 'Number of commits which have occurred since the session obtained its view' 5280 7 257 0 0 1922 8 'Type' 201 2464 4162 4192 4208 4162 4240 4208 2002 0 0 2066 2 1 2000 8 'doIt' 8 '[:each | each description]' 8 #[30 105 226 0 106] 8 #description 5440 7 257 0 0 1248 0 1 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | ''Nil or a String describing a system or GC gem'']' 8 #[30 105 29 106] 8 'Nil or a String describing a system or GC gem' 5536 7 257 0 0 1922 8 'Objects' 121 1968 4162 4192 4208 4162 4240 4208 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | each objects]' 8 #[30 105 226 0 106] 8 #objects 5696 7 257 0 0 1248 0 1 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | ''Number of temporary (unused) object IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (unused) object IDs allocated to the session' 5792 7 257 0 0 1922 8 'Pages' 121 1968 4162 4192 4208 4162 4240 4208 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | each pages]' 8 #[30 105 226 0 106] 8 #pages 5952 7 257 0 0 1248 0 1 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | ''Number of temporary (non-persistent) page IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (non-persistent) page IDs allocated to the session' 6048 7 257 0 0 1922 8 'Voting' 121 1968 4162 4192 4208 4162 4240 4208 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | each voteState]' 8 #[30 105 226 0 106] 8 #voteState 6208 7 257 0 0 1248 0 1 2002 0 0 2066 2 1 2096 8 'doIt' 8 '[:each | ''0: session has not voted; 1: voting now; 2: voted'']' 8 #[30 105 29 106] 8 '0: session has not voted; 1: voting now; 2: voted' 6304 7 257 0 0 8 #report 1376 0 131169 0 98 4 0 0 882 1 1 0 754 202 208 98 3 818 848 98 2 882 3 3 882 1153 567 1248 818 8 #contextMenu: 98 1 1488 1248 818 944 98 1 8 '#' 1248 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 1 0 0 0 65 2 0 0 28 1 0 0] 98 0 1056 0 27 1074 1120 3 1106 8 #fixedParentRight 3 1106 8 #fixedParentTop 3 1184 -51 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 6784 0 0 0 7 0 0 0 6784 0 8 4294903587 1554 1584 8 'Update' 1 1 0 0 32 0 0 0 754 202 208 98 3 818 848 98 2 882 1011 569 882 141 51 6784 818 8 #isEnabled: 98 1 32 6784 818 944 98 1 8 'Update' 6784 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 1 0 0 28 1 0 0 63 2 0 0 53 1 0 0] 98 0 1056 0 29 1074 1106 8 #fixedViewRight -139 6720 -1 1184 -51 1216 51 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 7216 0 0 0 5 0 0 0 7216 0 8 4294902617 722 0 0 0 754 202 208 98 2 818 848 98 2 882 1 1 882 1153 621 7216 818 944 98 1 8 'Error obtaining session list. Likely problem is that login user does not have permission to view session list.' 7216 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 54 1 0 0] 98 0 1056 0 27 1074 1120 1 6720 1 6752 1 1184 1 234 256 98 6 1248 8 'sessionList' 7216 8 'sessionListError' 560 8 'autoUpdate' 0 754 202 208 98 1 818 848 98 2 882 2879 21 882 1153 621 416 994 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 223 7 0 0 64 1 0 0] 98 4 1248 6784 560 7216 1056 0 27 )! !
!AllSessionsPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

