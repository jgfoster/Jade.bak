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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 8 410 ##(Smalltalk.ListView) 34 30 nil 416 34 2 8 1140920397 1025 544 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 4 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #fillSessionList 8 '&Update' 1 1 nil nil nil 802 1 834 #sendSigAbort 8 'Send Sig&Abort' 1 1 nil nil nil 802 1 834 #sendSigUsr1 8 'Request &Stack' 1 1 nil nil nil 802 1 834 #stopSession 8 'S&top Session' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 544 nil 8 1631709296 ##(Smalltalk.BasicListAbstract) nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 21 920646 5 ##(Smalltalk.ListViewColumn) 8 '#' 61 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 459302 ##(Smalltalk.Context) 1 1 nil nil 1180966 ##(Smalltalk.CompiledExpression) 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each id]' 34 1 138 ##(Smalltalk.PoolDictionary) 656 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #id 17 257 nil nil 544 nil 1 nil nil 1122 8 'Serial' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1170 0 1202 1 1 nil nil 1234 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each serial]' 34 1 138 ##(Smalltalk.PoolDictionary) 656 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #serial 17 257 nil nil 544 nil 1 nil nil 1122 8 'Description' 201 #left 459270 ##(Smalltalk.Message) #cacheDesc 8 #() 1570 #<= 1600 nil nil 544 nil 1 nil nil 1122 8 'User' 201 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1170 0 1202 1 1 nil nil 1234 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each name]' 34 1 138 ##(Smalltalk.PoolDictionary) 656 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #name 17 257 nil nil 544 nil 1 nil nil 1122 8 'View Age' 141 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1170 0 nil 1234 2 1 1168 8 'doIt' 8 '[:each | each viewAge]' 8 #[30 105 226 0 106] #viewAge 1840 7 257 nil nil 544 nil 1 nil nil 1122 8 'Oldest' 121 #center ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1170 0 1202 1 1 nil nil 1234 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each hasOldestCR]' 34 1 138 ##(Smalltalk.PoolDictionary) 656 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #hasOldestCR 17 257 nil nil 544 nil 1 nil nil 1122 8 'Gem Host' 281 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1170 0 1202 1 1 nil nil 1234 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each host]' 34 1 138 ##(Smalltalk.PoolDictionary) 656 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #host 17 257 nil nil 544 nil 1 nil nil 1122 8 'Process' 131 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1170 0 1202 1 1 nil nil 1234 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each process]' 34 1 138 ##(Smalltalk.PoolDictionary) 656 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #process 17 257 nil nil 544 nil 1 nil nil 1122 8 'Primitive' 131 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1170 0 1202 1 1 nil nil 1234 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each primitive]' 34 1 138 ##(Smalltalk.PoolDictionary) 656 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #primitive 17 257 nil nil 544 nil 1 nil nil 1122 8 'State' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1170 0 1202 1 1 nil nil 1234 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each state]' 34 1 138 ##(Smalltalk.PoolDictionary) 656 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #state 17 257 nil nil 544 nil 1 nil nil 1122 8 'Trans' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1170 0 1202 1 1 nil nil 1234 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each transaction]' 34 1 138 ##(Smalltalk.PoolDictionary) 656 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #transaction 17 257 nil nil 544 nil 1 nil nil 1122 8 'GCI IP' 241 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1170 0 1202 1 1 nil nil 1234 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each ip]' 34 1 138 ##(Smalltalk.PoolDictionary) 656 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #ip 17 257 nil nil 544 nil 1 nil nil 1122 8 'Priority' 111 #right 1570 #displayString 8 #() 1570 #<= 3200 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each priority]' 8 #[30 105 226 0 106] #priority 3232 7 257 nil nil 544 nil 1 nil nil 1122 8 'Host ID' 121 #right 1570 #displayString 3200 1570 #<= 3200 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each hostId]' 8 #[30 105 226 0 106] #hostId 3376 7 257 nil nil 544 nil 1 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Unique host ID of the host where the session is running (an Integer)'']' 8 #[30 105 29 106] 8 'Unique host ID of the host where the session is running (an Integer)' 3456 7 257 nil nil 1122 8 'Quiet' 121 #right 1570 #displayString 3200 1570 #<= 3200 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each quietTime]' 8 #[30 105 226 0 106] #quietTime 3616 7 257 nil nil 544 nil 1 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Seconds since the session''''s most recent request to the stone'']' 8 #[30 105 29 106] 8 'Seconds since the session''s most recent request to the stone' 3696 7 257 nil nil 1122 8 'Age' 121 #right 1570 #displayString 3200 1570 #<= 3200 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each lifeTime]' 8 #[30 105 226 0 106] #lifeTime 3856 7 257 nil nil 544 nil 1 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Seconds since the session logged in'']' 8 #[30 105 29 106] 8 'Seconds since the session logged in' 3936 7 257 nil nil 1122 8 'Backlog' 121 #right 1570 #displayString 3200 1570 #<= 3200 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each backlog]' 8 #[30 105 226 0 106] #backlog 4096 7 257 nil nil 544 nil 1 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of commits which have occurred since the session obtained its view'']' 8 #[30 105 29 106] 8 'Number of commits which have occurred since the session obtained its view' 4176 7 257 nil nil 1122 8 'Type' 201 #left 1570 #displayString 3200 1570 #<= 3200 1170 0 nil 1234 2 1 1168 8 'doIt' 8 '[:each | each description]' 8 #[30 105 226 0 106] #description 4336 7 257 nil nil 544 nil 1 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Nil or a String describing a system or GC gem'']' 8 #[30 105 29 106] 8 'Nil or a String describing a system or GC gem' 4416 7 257 nil nil 1122 8 'Objects' 121 #right 1570 #displayString 3200 1570 #<= 3200 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each objects]' 8 #[30 105 226 0 106] #objects 4576 7 257 nil nil 544 nil 1 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of temporary (unused) object IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (unused) object IDs allocated to the session' 4656 7 257 nil nil 1122 8 'Pages' 121 #right 1570 #displayString 3200 1570 #<= 3200 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each pages]' 8 #[30 105 226 0 106] #pages 4816 7 257 nil nil 544 nil 1 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of temporary (non-persistent) page IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (non-persistent) page IDs allocated to the session' 4896 7 257 nil nil 1122 8 'Voting' 121 #right 1570 #displayString 3200 1570 #<= 3200 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each voteState]' 8 #[30 105 226 0 106] #voteState 5056 7 257 nil nil 544 nil 1 1170 0 nil 1234 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''0: session has not voted; 1: voting now; 2: voted'']' 8 #[30 105 29 106] 8 '0: session has not voted; 1: voting now; 2: voted' 5136 7 257 nil nil #report 656 nil 131169 nil 34 4 nil nil 328198 ##(Smalltalk.Point) 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 5250 3 3 5250 1153 567 544 5346 #contextMenu: 34 1 768 544 5346 #text: 34 1 8 '#' 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 1 0 0 0 65 2 0 0 28 1 0 0] 8 #() 5250 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 3 5618 #fixedParentRight 3 5618 #fixedParentTop 3 5618 #fixedParentBottom -51 410 ##(Smalltalk.CheckBox) 34 16 nil 416 34 2 8 1409363203 1 5696 721990 2 ##(Smalltalk.ValueHolder) nil nil 1114118 ##(Smalltalk.NeverSearchPolicy) false nil nil 5 nil nil nil 5696 nil 8 1631818816 852486 ##(Smalltalk.NullConverter) nil nil nil 5282 138 144 34 2 5346 #createAt:extent: 34 2 5250 3 571 5250 201 45 5696 5346 #text: 34 1 8 'Auto-update' 5696 5506 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 29 1 0 0 101 0 0 0 51 1 0 0] 8 #() 5568 nil 27 5586 5632 3 5618 #fixedViewLeft 201 5680 -49 5618 #fixedViewTop 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 6128 nil nil nil 5 nil nil nil 6128 nil 8 1631816352 5842 nil nil nil 5282 138 144 34 2 5346 #createAt:extent: 34 2 5250 1 1 5250 1153 621 6128 5346 #text: 34 1 8 'Error obtaining session list. Likely problem is that login user does not have permission to view session list.' 6128 5506 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 54 1 0 0] 8 #() 5568 nil 27 5586 5632 1 5648 1 5664 1 5680 1 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 6448 nil nil nil 7 nil nil nil 6448 nil 8 1631818816 834 #fillSessionList 8 'Update' 1 1 nil nil false nil nil nil 5282 138 144 34 3 5346 #createAt:extent: 34 2 5250 1011 569 5250 141 51 6448 5346 #isEnabled: 8 #(false) 6448 5346 #text: 34 1 8 'Update' 6448 5506 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 1 0 0 28 1 0 0 63 2 0 0 53 1 0 0] 8 #() 5568 nil 29 5586 5618 #fixedViewRight -139 5648 -1 5680 -51 6112 51 170 192 34 6 544 8 'sessionList' 6128 8 'sessionListError' 5696 8 'autoUpdate' nil 5282 138 144 34 1 5346 #createAt:extent: 34 2 5250 3839 21 5250 1153 621 416 5506 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 191 9 0 0 64 1 0 0] 34 4 544 6448 5696 6128 5568 nil 27 )! !
!AllSessionsPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

