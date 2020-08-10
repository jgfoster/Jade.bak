"Filed out from Dolphin Smalltalk 7"!

ContactsShell subclass: #ContactsBrowser
	instanceVariableNames: 'listPresenter beginPresenter listClassPresenter commitRecordPresenter'
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
	self update.
!

begin

	self model begin.
	Sound informationBeep.
	self update.
!

commit

	| string |
	self model commit ifTrue: [
		Sound informationBeep.
		self update.
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
	beginPresenter := self add: RadioButtonSetPresenter new name: 'transactionMode'.
	listClassPresenter := self add: RadioButtonSetPresenter new name: 'listClass'.
	commitRecordPresenter := self add: TextPresenter new name: 'commitRecord'.
!

createSchematicWiring

	super createSchematicWiring.
	beginPresenter when: #valueChanged send: #transactionModeChanged to: self.
	listClassPresenter when: #valueChanged send: #listClassChanged to: self.
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

listClassChanged

	model executeString: '
(ContactList isKindOf: ' , listClassPresenter value , ') ifFalse: [ContactList := ' , listClassPresenter value , ' withAll: ContactList]'.
!

newRequest
	ContactEditor showOn: self model!

onViewActivated: anObject

	self update.
!

onViewClosed

	super onViewClosed.
	self model notNil ifTrue: [self model logout].
!

onViewOpened

	self caption: (self model titleBarFor: 'Contacts Browser').
	super onViewOpened.
	[
		self update.
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
!

transactionModeChanged

	model executeString: 'System transactionMode: #' , beginPresenter value.
!

update

	| list number |
	list := Contact listFromGciSession: model.
	listPresenter list: list.
	number := [
		self model executeString: 'System commitRecordPageForSessionId: System session'.
	] on: Error do: [:ex | 
		ex return: 'N/A'.
	].
	commitRecordPresenter value: number.
	beginPresenter value: (model executeString: 'System transactionMode').
	listClassPresenter value: (model executeString: 'ContactList class name').
! !
!ContactsBrowser categoriesFor: #abort!public! !
!ContactsBrowser categoriesFor: #begin!public! !
!ContactsBrowser categoriesFor: #commit!public! !
!ContactsBrowser categoriesFor: #commitConflictsSource!public! !
!ContactsBrowser categoriesFor: #createComponents!public! !
!ContactsBrowser categoriesFor: #createSchematicWiring!public! !
!ContactsBrowser categoriesFor: #editRequest!public! !
!ContactsBrowser categoriesFor: #fillList!public! !
!ContactsBrowser categoriesFor: #listClassChanged!public! !
!ContactsBrowser categoriesFor: #newRequest!public! !
!ContactsBrowser categoriesFor: #onViewActivated:!public! !
!ContactsBrowser categoriesFor: #onViewClosed!public! !
!ContactsBrowser categoriesFor: #onViewOpened!public! !
!ContactsBrowser categoriesFor: #queryCommand:!public! !
!ContactsBrowser categoriesFor: #transactionModeChanged!public! !
!ContactsBrowser categoriesFor: #update!public! !

!ContactsBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 327686 ##(Smalltalk.Color) #default 328198 ##(Smalltalk.Point) 1201 801 549 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 26 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 592 nil nil nil 7 nil nil nil 592 nil 8 1920925952 1180998 4 ##(Smalltalk.CommandDescription) #commit 8 'Commit' 1 1 nil nil false nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 498 421 673 498 141 51 592 786 #isEnabled: 8 #(false) 592 786 #text: 34 1 8 'Commit' 592 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 210 0 0 0 80 1 0 0 24 1 0 0 105 1 0 0] 8 #() 498 193 193 nil 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 421 1058 #fixedViewLeft 141 1058 #fixedParentBottom -49 1058 #fixedViewTop 51 410 ##(Smalltalk.ListView) 34 45 nil 416 34 2 8 1409355853 1025 1136 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 480 nil 7 nil nil nil 1136 nil 8 1920945440 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 5 920646 5 ##(Smalltalk.ListViewColumn) 8 'Name' 301 #left 1314 #displayString 1344 ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 1488 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 1504 7 257 nil nil 1136 nil 3 nil nil 1426 8 'Title' 301 #left 1314 #displayString 8 #() 1314 #<= 1648 1490 0 nil 1522 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each title]' 8 #[30 105 226 0 106] #title 1680 7 257 nil nil 1136 nil 3 nil nil 1426 8 'Phone' 241 #left 1314 #displayString 1648 1314 #<= 1648 1490 0 nil 1522 2 1 1488 8 'doIt' 8 '[:each | each phone]' 8 #[30 105 226 0 106] #phone 1824 7 257 nil nil 1136 nil 3 nil nil 1426 8 'Version' 121 #right 1314 #displayString 1248 1314 #<= 1248 1490 0 nil 1522 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each version]' 8 #[30 105 226 0 106] #version 1968 7 257 nil nil 1136 nil 1 nil nil 1426 8 'OOP' 161 #right 1314 #displayString 1248 1314 #<= 1248 1490 0 nil 1522 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each oop]' 8 #[30 105 226 0 106] #oop 2112 7 257 nil nil 1136 nil 1 nil nil #report 1248 nil 131169 nil 1 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 498 1 1 498 1169 623 1136 786 #text: 34 1 8 'Name' 1136 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 55 1 0 0] 8 #() 1008 nil 35 1026 1072 1 1058 #fixedParentRight 1 1058 #fixedParentTop 1 1104 -99 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 2448 nil nil nil 7 nil nil nil 2448 nil 8 1920925952 674 #editRequest 8 'Edit' 1 1 nil nil false nil nil nil 722 138 144 34 3 786 #createAt:extent: 34 2 498 141 673 498 141 51 2448 786 #isEnabled: 8 #(false) 2448 786 #text: 34 1 8 'Edit' 2448 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 80 1 0 0 140 0 0 0 105 1 0 0] 8 #() 1008 nil 29 1026 1072 141 1088 141 1104 -49 1120 51 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850946 65 2816 nil nil nil 7 nil nil nil 2816 nil 8 1920890832 852486 ##(Smalltalk.NullConverter) nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 498 585 623 498 161 39 2816 786 #text: 34 1 8 'List:  ' 2816 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 36 1 0 0 55 1 0 0 116 1 0 0 74 1 0 0] 8 #() 1008 nil 27 1026 1058 #fixedPreviousRight 1 1088 161 1104 -99 1120 39 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850946 65 3168 nil nil nil 7 nil nil nil 3168 nil 8 1920890832 2898 nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 498 1 623 498 161 39 3168 786 #text: 34 1 8 'Transaction:  ' 3168 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 55 1 0 0 80 0 0 0 74 1 0 0] 8 #() 1008 nil 27 1026 1072 1 1088 161 1104 -99 1120 39 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 3488 nil nil nil 7 nil nil nil 3488 nil 8 1920925952 674 #abort 8 'Abort' 1 1 nil nil false nil nil nil 722 138 144 34 3 786 #createAt:extent: 34 2 498 561 673 498 141 51 3488 786 #isEnabled: 8 #(false) 3488 786 #text: 34 1 8 'Abort' 3488 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 24 1 0 0 80 1 0 0 94 1 0 0 105 1 0 0] 8 #() 1008 nil 29 1026 1072 561 1088 141 1104 -49 1120 51 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1409286144 131073 3856 nil 480 nil 7 nil nil nil 3856 656390 ##(Smalltalk.GridLayout) 1 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 3856 34 2 8 1140924425 1 3984 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 1314 #= 8 #() 1314 #hash 8 #() false 480 nil 7 nil nil nil 3984 nil 8 1920925952 2898 nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 498 213 1 498 213 51 3984 786 #text: 34 1 8 'RcIdentityBag' 3984 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 106 0 0 0 0 0 0 0 212 0 0 0 25 0 0 0] 8 #() 1008 nil 27 8 'RcIdentityBag' 410 ##(Smalltalk.RadioButton) 34 16 nil 3856 34 2 8 1141055497 1 4432 4050 nil false 4082 1314 #= 8 #() 1314 #hash 8 #() true 480 nil 7 nil nil nil 4432 nil 8 1920925952 2898 nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 498 1 1 498 213 51 4432 786 #text: 34 1 8 'IdentityBag' 4432 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 106 0 0 0 25 0 0 0] 8 #() 1008 nil 27 8 'IdentityBag' nil 722 138 144 34 1 786 #createAt:extent: 34 2 498 745 623 498 425 51 3856 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 116 1 0 0 55 1 0 0 72 2 0 0 80 1 0 0] 34 2 4432 3984 1008 nil 27 1026 3152 1 2416 1 1104 -99 1120 51 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 5024 nil nil nil 7 nil nil nil 5024 nil 8 1920890832 2898 nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 498 879 681 498 191 31 5024 786 #text: 34 1 8 'Commit Record:' 5024 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 183 1 0 0 84 1 0 0 22 2 0 0 99 1 0 0] 8 #() 1008 nil 27 1026 2416 -289 1088 191 1104 -41 1120 31 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1409286144 131073 5344 nil 480 nil 7 nil nil nil 5344 3922 1 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 5344 34 2 8 1141055497 1 5456 4050 nil false 4082 1314 #= 8 #() 1314 #hash 8 #() true 480 nil 7 nil nil nil 5456 nil 8 1920925952 2898 nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 498 1 1 498 213 51 5456 786 #text: 34 1 8 'Manual Begin' 5456 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 106 0 0 0 25 0 0 0] 8 #() 1008 nil 27 8 'manualBegin' 410 ##(Smalltalk.RadioButton) 34 16 nil 5344 34 2 8 1140924425 1 5872 4050 nil nil 4082 1314 #= 8 #() 1314 #hash 8 #() false 480 nil 7 nil nil nil 5872 nil 8 1920925952 2898 nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 498 213 1 498 213 51 5872 786 #text: 34 1 8 'Auto Begin' 5872 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 106 0 0 0 0 0 0 0 212 0 0 0 25 0 0 0] 8 #() 1008 nil 27 8 'autoBegin' nil 722 138 144 34 2 786 #createAt:extent: 34 2 498 161 623 498 425 51 5344 786 #text: 34 1 8 'Transaction Mode' 5344 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 55 1 0 0 36 1 0 0 80 1 0 0] 34 2 5456 5872 1008 nil 27 1026 3152 1 1058 #relativeParentWidth 327734 ##(Smalltalk.Float) 8 0 0 0 0 0 0 224 63 1104 -99 1120 51 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 6560 nil nil nil 7 nil nil nil 6560 nil 8 1920925952 674 #begin 8 'Begin' 1 1 nil nil false nil nil nil 722 138 144 34 3 786 #createAt:extent: 34 2 498 281 673 498 141 51 6560 786 #isEnabled: 8 #(false) 6560 786 #text: 34 1 8 'Begin' 6560 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 0 0 0 80 1 0 0 210 0 0 0 105 1 0 0] 8 #() 1008 nil 29 1026 1072 281 1088 141 1104 -49 1120 51 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 6928 nil nil nil 7 nil nil nil 6928 nil 8 1920925952 674 #update 8 'Refresh' 1 1 nil nil false nil nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 498 701 673 498 141 51 6928 786 #text: 34 1 8 'Refresh' 6928 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 94 1 0 0 80 1 0 0 164 1 0 0 105 1 0 0] 8 #() 1008 nil 29 1026 1072 701 1088 141 1104 -49 1120 51 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140924546 1025 7264 nil 480 nil 7 nil nil nil 7264 nil 8 1920913264 2898 nil nil 3 722 138 144 34 2 786 #createAt:extent: 34 2 498 1069 677 498 101 41 7264 786 #setMarginWidths: 34 1 8 #(3 3) 7264 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 22 2 0 0 82 1 0 0 72 2 0 0 102 1 0 0] 8 #() 1008 nil 27 1026 2416 -99 1088 101 1104 -45 1120 41 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 7584 nil nil nil 7 nil nil nil 7584 nil 8 1920925952 674 #newRequest 8 'New' 1 1 nil nil false nil nil nil 722 138 144 34 3 786 #createAt:extent: 34 2 498 1 673 498 141 51 7584 786 #isEnabled: 8 #(false) 7584 786 #text: 34 1 8 'New' 7584 946 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 80 1 0 0 70 0 0 0 105 1 0 0] 8 #() 1008 nil 29 1026 1072 1 1088 141 1104 -49 1120 51 170 192 34 8 3856 8 'listClass' 1136 8 'list' 7264 8 'commitRecord' 5344 8 'transactionMode' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 722 138 144 34 2 786 #createAt:extent: 34 2 498 5119 21 498 1201 801 416 786 #text: 34 1 8 'Contacts Browser' 416 946 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 154 1 0 0] 34 13 1136 7584 2448 6560 592 3488 6928 5024 7264 3168 5344 2816 3856 1008 nil 27 )! !
!ContactsBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

