"Filed out from Dolphin Smalltalk 7"!

ContactsShell subclass: #ContactsBrowser
	instanceVariableNames: 'listPresenter commitRecordPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ContactsBrowser guid: (GUID fromString: '{c082a068-8d8d-4292-8200-6cc42c48d673}')!
ContactsBrowser comment: ''!
!ContactsBrowser categoriesForClass!Unclassified! !
!ContactsBrowser methodsFor!

abort

	self model abort.
	Sound informationBeep.
	self fillList.
!

begin

	self model begin.
	Sound informationBeep.
	self fillList.
!

commit

	| string |
	self model commit ifTrue: [
		Sound informationBeep.
		self fillList.
		^self.
	].
	string := self model executeString: self commitConflictsSource.
	MessageBox notify: string.

!

commitConflictsSource

^'| stream dict string |
dict := System transactionConflicts.
stream := (WriteStream on: String new)
	nextPutAll: ''Commit result: '';
	nextPutAll: (dict at: #''commitResult''); cr;
	yourself.
dict removeKey: #''commitResult''.
dict keys do: [:each | 
	(each includesString: ''Rc'') ifTrue: [
		dict removeKey: each.
	].
].
dict keysAndValuesDo: [:type :conflicts | 
	stream nextPutAll: type; cr.
	conflicts do: [:each | 
		string := (ReadStream on: each printString) nextLine.
		string := string copyFrom: 1 to: (string size min: 40).
		stream
			tab; nextPutAll: each asOop printString;
			tab; nextPutAll: each class name;
			tab; nextPutAll: string;
			cr.
	].
].
stream contents.'.
!

createComponents

	super createComponents.
	listPresenter := self add: ListPresenter new name: 'list'.
	commitRecordPresenter := self add: TextPresenter new name: 'commitRecord'.
!

createSchematicWiring

	super createSchematicWiring.
	listPresenter when: #actionPerformed send: #editRequest to: self.
!

editRequest

	ContactEditor 
		showOn: listPresenter selection 
		inSession: self model.
!

fillList

	| list number |
	list := Contact listFromGciSession: model.
	listPresenter list: list.
	number := [
		self model executeString: 'System commitRecordPageForSessionId: System session'.
	] on: Error do: [:ex | 
		ex return: 'N/A'.
	].
	commitRecordPresenter value: number.
!

newRequest
	ContactEditor showOn: self model!

onViewActivated: anObject

	self fillList.
!

onViewClosed

	super onViewClosed.
	self model notNil ifTrue: [self model logout].
!

onViewOpened

	self caption: (self model titleBarFor: 'Contacts Browser').
	super onViewOpened.
	[
		self fillList.
		self model commit ifFalse: [self error: 'Commit failed!!'].
	] on: Error do: [:ex |
		self model logout.
		MessageBox warning: ex description.
		Processor terminateActive.
	].
!

queryCommand: aCommandQuery

	aCommandQuery commandSymbol = #'editRequest' ifTrue: [
		aCommandQuery isEnabled: listPresenter hasSelection.
		^true.
	].
	^super queryCommand: aCommandQuery.
! !
!ContactsBrowser categoriesFor: #abort!public! !
!ContactsBrowser categoriesFor: #begin!public! !
!ContactsBrowser categoriesFor: #commit!public! !
!ContactsBrowser categoriesFor: #commitConflictsSource!public! !
!ContactsBrowser categoriesFor: #createComponents!public! !
!ContactsBrowser categoriesFor: #createSchematicWiring!public! !
!ContactsBrowser categoriesFor: #editRequest!public! !
!ContactsBrowser categoriesFor: #fillList!public! !
!ContactsBrowser categoriesFor: #newRequest!public! !
!ContactsBrowser categoriesFor: #onViewActivated:!public! !
!ContactsBrowser categoriesFor: #onViewClosed!public! !
!ContactsBrowser categoriesFor: #onViewOpened!public! !
!ContactsBrowser categoriesFor: #queryCommand:!public! !

!ContactsBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 18 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 624 0 0 0 7 0 0 0 624 0 8 4294902441 1180998 4 ##(Smalltalk.CommandDescription)  8 #commit 8 'Commit' 1 1 0 0 32 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 421 683 530 141 51 624 850 8 #isEnabled: 98 1 32 624 850 8 #text: 98 1 8 'Commit' 624 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 210 0 0 0 85 1 0 0 24 1 0 0 110 1 0 0] 98 0 530 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 421 1170 8 #fixedViewLeft 141 1170 8 #fixedParentBottom -49 1170 8 #fixedViewTop 51 410 640 98 20 0 416 98 2 8 1140924416 1 1312 0 0 0 7 0 0 0 1312 0 8 4294902441 722 8 #fillList 8 'Refresh' 1 1 0 0 32 0 0 0 786 202 208 98 3 850 880 98 2 530 701 683 530 141 51 1312 850 960 98 1 32 1312 850 1008 98 1 8 'Refresh' 1312 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 94 1 0 0 85 1 0 0 164 1 0 0 110 1 0 0] 98 0 1120 0 29 1138 1184 701 1216 141 1248 -49 1280 51 410 640 98 20 0 416 98 2 8 1140924416 1 1696 0 0 0 7 0 0 0 1696 0 8 4294902441 722 8 #abort 8 'Abort' 1 1 0 0 32 0 0 0 786 202 208 98 3 850 880 98 2 530 561 683 530 141 51 1696 850 960 98 1 32 1696 850 1008 98 1 8 'Abort' 1696 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 24 1 0 0 85 1 0 0 94 1 0 0 110 1 0 0] 98 0 1120 0 29 1138 1184 561 1216 141 1248 -49 1280 51 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1409355853 1025 2080 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 2080 0 8 4294902273 459270 ##(Smalltalk.Message)  8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 3 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 393 8 #left 2306 2336 2352 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 2544 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] 8 #name 2560 7 257 0 0 2080 0 3 0 0 2450 8 'Title' 393 2496 2306 2336 98 0 2306 8 #<= 2720 2546 0 0 2578 2 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each title]' 8 #[30 105 226 0 106] 8 #title 2768 7 257 0 0 2080 0 3 0 0 2450 8 'Phone' 393 2496 2306 2336 2720 2306 2752 2720 2546 0 0 2578 2 1 2544 8 'doIt' 8 '[:each | each phone]' 8 #[30 105 226 0 106] 8 #phone 2944 7 257 0 0 2080 0 3 0 0 8 #report 2208 0 131169 0 0 786 202 208 98 2 850 880 98 2 530 1 1 530 1185 683 2080 850 1008 98 1 8 'Name' 2080 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 80 2 0 0 85 1 0 0] 98 0 1120 0 27 1138 1184 1 1170 8 #fixedParentRight 1 1170 8 #fixedParentTop 1 1248 -49 410 640 98 20 0 416 98 2 8 1140924416 1 3344 0 0 0 7 0 0 0 3344 0 8 4294902441 722 8 #begin 8 'Begin' 1 1 0 0 32 0 0 0 786 202 208 98 3 850 880 98 2 530 281 683 530 141 51 3344 850 960 98 1 32 3344 850 1008 98 1 8 'Begin' 3344 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 0 0 0 85 1 0 0 210 0 0 0 110 1 0 0] 98 0 1120 0 29 1138 1184 281 1216 141 1248 -49 1280 51 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140924546 1025 3728 0 482 8 4278190080 0 7 0 0 0 3728 0 8 4294902123 852486 ##(Smalltalk.NullConverter)  0 0 3 786 202 208 98 3 850 880 98 2 530 1085 687 530 101 41 3728 850 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 3728 850 8 #isTextModified: 98 1 32 3728 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 30 2 0 0 87 1 0 0 80 2 0 0 107 1 0 0] 98 0 1120 0 27 1138 3280 -99 1216 101 1248 -45 1280 41 410 640 98 20 0 416 98 2 8 1140924416 1 4192 0 0 0 7 0 0 0 4192 0 8 4294902441 722 8 #editRequest 8 'Edit' 1 1 0 0 32 0 0 0 786 202 208 98 3 850 880 98 2 530 141 683 530 141 51 4192 850 960 98 1 32 4192 850 1008 98 1 8 'Edit' 4192 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 85 1 0 0 140 0 0 0 110 1 0 0] 98 0 1120 0 29 1138 1184 141 1216 141 1248 -49 1280 51 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 4576 0 0 0 7 0 0 0 4576 0 8 4294902305 3858 0 0 0 786 202 208 98 2 850 880 98 2 530 895 691 530 191 31 4576 850 1008 98 1 8 'Commit Record:' 4576 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 191 1 0 0 89 1 0 0 30 2 0 0 104 1 0 0] 98 0 1120 0 27 1138 3280 -289 1216 191 1248 -41 1280 31 410 640 98 20 0 416 98 2 8 1140924416 1 4912 0 0 0 7 0 0 0 4912 0 8 4294902441 722 8 #newRequest 8 'New' 1 1 0 0 32 0 0 0 786 202 208 98 3 850 880 98 2 530 1 683 530 141 51 4912 850 960 98 1 32 4912 850 1008 98 1 8 'New' 4912 1058 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 85 1 0 0 70 0 0 0 110 1 0 0] 98 0 1120 0 29 1138 1184 1 1216 141 1248 -49 1280 51 234 256 98 4 2080 8 'list' 3728 8 'commitRecord' 0 0 0 0 0 1 0 0 0 0 1 0 0 786 202 208 98 3 850 880 98 2 530 3359 21 530 1201 801 416 850 1008 98 1 8 'Contacts Browser' 416 850 8 #updateMenuBar 2208 416 1058 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 143 6 0 0 10 0 0 0 231 8 0 0 154 1 0 0] 98 9 2080 4912 4192 3344 624 1696 1312 4576 3728 1120 0 27 )! !
!ContactsBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

