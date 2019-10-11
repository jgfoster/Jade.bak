"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadeSystemBrowser
	instanceVariableNames: 'cardsPresenter roundTripCount'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSystemBrowser guid: (GUID fromString: '{72210941-97b6-4113-8ec8-e23015415de6}')!
JadeSystemBrowser comment: ''!
!JadeSystemBrowser categoriesForClass!Unclassified! !
!JadeSystemBrowser methodsFor!

abortTransaction

	self isOkayToChange ifTrue: [
		gciSession abort.
		self update.
	].
!

addSystemBrowser

	self addSystemBrowserWithLayoutInfo: (self currentCard ifNotNil: [:currentCard | currentCard layoutInfo]).
!

addSystemBrowserForClass: anArray

	(JadeAutoSystemBrowserPresenter
		createIn: cardsPresenter 
		gciSession: gciSession)
		updateAfterFindClass: anArray
		isMeta: nil 
		selector: ''.
!

addSystemBrowserWithLayoutInfo: each

	GciSession log: 'JadeSystemBrowserPresenter>>addSystemBrowserWithLayoutInfo:'.
	(JadeAutoSystemBrowserPresenter
		createIn: cardsPresenter 
		gciSession: gciSession)
		layoutInfo: each.
!

addWorkspace

	JadeWorkspace showOnSession: gciSession.
!

closeCard

	cardsPresenter view removeSubView: self currentCard view.!

closeRequested: anAssociation

	anAssociation value ifTrue: [
		self isOkayToChange ifTrue: [
			[self saveLayoutAndContents] on: Error do: [:ex | ex return].
		] ifFalse: [
			anAssociation value: false.
		].
	].
!

commitTransaction

	gciSession commit ifTrue: [
		Sound informationBeep.
		self update.
	] ifFalse: [
		MessageBox warning: 'Commit failed!!'.
	].
!

createComponents

	super createComponents.
	cardsPresenter := self add: Presenter new name: 'cardContainer'.
!

createSchematicWiring

	| toolbar |
	super createSchematicWiring.
	toolbar := self view viewNamed: 'leftToolbar'.
	cardsPresenter view 	when: #'currentCardChanged' 	send: #'update' 			to: self.
	self  				when: #'closeRequested:'		send: #'closeRequested:'	to: self.
	self  view 			when: #'viewActivated'		send: #'update'			to: self.
	toolbar			when: #dropDown: 			send: #onDropDown: 		to: self.!

currentCard

	^cardsPresenter view currentCard ifNotNil: [:cardView | cardView presenter].
!

handleInvalidSession

	| hadDialog |
	hadDialog := false.
	cardsPresenter view cards do: [:each | 
		(each presenter isKindOf: JadeSystemBrowserPresenter) ifTrue: [
			hadDialog := each presenter handleInvalidSession or: [hadDialog].
		].
	].
	hadDialog ifFalse: [
		MessageBox
			warning: 'All windows for this session will close due to unexpected logout.'
			caption: 'Invalid session!!'.
	].
	gciSession forceLogout.
!

initialize

	super initialize.
	roundTripCount := 0.!

isOkayToChange

	cardsPresenter view cards do: [:each | 
		each presenter isOkayToChange ifFalse: [^false].
	].
	^true.
!

jadeBrowseTests

	JadeSUnitBrowser showOnSession: gciSession.

!

layoutFilePath

	^SessionManager current imageBase , 'Jade System Browser Layout.stb'!

logoutRequested: aValueHolder
	"Opportunity to save changes."

	self closeRequested: aValueHolder.
!

maximizeCode

	(self currentCard class == JadeAutoSystemBrowserPresenter) ifFalse: [^self].

	self currentCard bottomArrangement 
	ifNil: [
		self currentCard bottomArrangement: (self currentCard view viewNamed: 'textAreaTabs') arrangement.
		(self currentCard view viewNamed: 'textAreaTabs') arrangement: 100]
	ifNotNil: [:value | 
		(self currentCard view viewNamed: 'textAreaTabs') arrangement: self currentCard bottomArrangement.
		self currentCard bottomArrangement: nil].

	self currentCard view invalidate.!

moveCardLeft

	| cardView nextSibling |
	cardView := self currentCard view.
	nextSibling := cardView previousSiblingView.
	nextSibling notNil ifTrue: [nextSibling := nextSibling previousSiblingView].
	nextSibling notNil ifTrue: [cardView zOrderAfter: nextSibling] ifFalse: [cardView zOrderTop]!

moveCardRight

	| cardView nextSibling |
	cardView := self currentCard view.
	nextSibling := cardView nextSiblingView.
	nextSibling notNil ifTrue: [cardView zOrderAfter: nextSibling].
!

onDropDown: aToolbarButton 
	"Private - The receiver's toolbar has sent a notification that a button's drop-down arrow
	has been pressed. Generate and pop-up the appropriate menu."
	| popup |

	aToolbarButton command == #historyBack ifTrue: [popup :=  self currentCard historyBackMenu].
	aToolbarButton command == #historyForward ifTrue: [popup := self currentCard historyForwardMenu].
	popup queryAllFromView: self view.
	popup showIn: self position: aToolbarButton screenRectangle bottomLeft.

	^0!

onViewOpened

	super onViewOpened.
	cardsPresenter model: gciSession.
	self restoreLayoutAndContents.
!

queryCommand: aCommandQuery

	(#(#'closeCard' #'moveCardLeft' #'moveCardRight') includes: aCommandQuery command)  ifTrue: [
		aCommandQuery isEnabled: 1 < cardsPresenter view cards size. 
		^true.
	].
	^super queryCommand: aCommandQuery.

!

renameSelection

	MessageBox notify: 'Sorry, we are not yet prepared to handle this feature!!'.
	SessionManager current pause.
!

restoreLayoutAndContents

	self restoreLayoutAndContentsFromFile ifFalse: [
		self view position: 115 @ 70.
		self addSystemBrowserWithLayoutInfo: nil.
	].
!

restoreLayoutAndContentsFromFile

	| path file bytes data |
	path := self layoutFilePath.
	(File exists: path) ifFalse: [^false].
	file := File 
		open: path
		mode: #read.
	bytes := ByteArray new: file size.
	file read: bytes; close.
	data := Object fromBinaryStoreBytes: bytes.
	(data isKindOf: OrderedCollection) ifFalse: [File delete: path. ^false].
	(data at: 1) == 1 ifFalse: [File delete: path. ^false].
	(data at: 2) top < 0 ifTrue: [File delete: path. ^false].	"Occasionally the position seems to be bad"
	self view rectangle: (data at: 2).
	(data at: 3) do: [:each | 
		self addSystemBrowserWithLayoutInfo: each.
	].
	^true.
!

saveLayoutAndContents

	| data |
	data := OrderedCollection new
		add: 1;	"file version number"
		add: self view rectangle;
		add: (cardsPresenter view cards collect: [:each | each presenter layoutInfo]);
		yourself.
	(File 
		open: self layoutFilePath
		mode: #truncate 
		check: false)
		write: data binaryStoreBytes;
		close.
!

selectClass: classString selector: methodString

	GciSession log: 'JadeSystemBrowser>>selectClass - ' , classString , ' selector: ' , methodString.
	self currentCard
		selectClass: classString 
		selector: methodString.
!

shellName

	^'System Browse'.
!

statusBarServerRequestText: aString

	roundTripCount := roundTripCount + 1.
	self statusBarText: 'Server request #' , roundTripCount printString , '; ' , aString.
!

statusBarText: aString

	(self view viewNamed: 'statusBarField') model: (ValueHolder with: aString).
!

update

	self currentCard ifNotNil: [:currentCard | currentCard onSetFocus].
! !
!JadeSystemBrowser categoriesFor: #abortTransaction!public! !
!JadeSystemBrowser categoriesFor: #addSystemBrowser!public! !
!JadeSystemBrowser categoriesFor: #addSystemBrowserForClass:!public! !
!JadeSystemBrowser categoriesFor: #addSystemBrowserWithLayoutInfo:!public! !
!JadeSystemBrowser categoriesFor: #addWorkspace!public! !
!JadeSystemBrowser categoriesFor: #closeCard!public! !
!JadeSystemBrowser categoriesFor: #closeRequested:!public! !
!JadeSystemBrowser categoriesFor: #commitTransaction!public! !
!JadeSystemBrowser categoriesFor: #createComponents!public! !
!JadeSystemBrowser categoriesFor: #createSchematicWiring!history!public! !
!JadeSystemBrowser categoriesFor: #currentCard!public! !
!JadeSystemBrowser categoriesFor: #handleInvalidSession!public! !
!JadeSystemBrowser categoriesFor: #initialize!public! !
!JadeSystemBrowser categoriesFor: #isOkayToChange!public! !
!JadeSystemBrowser categoriesFor: #jadeBrowseTests!public! !
!JadeSystemBrowser categoriesFor: #layoutFilePath!public! !
!JadeSystemBrowser categoriesFor: #logoutRequested:!public! !
!JadeSystemBrowser categoriesFor: #maximizeCode!public! !
!JadeSystemBrowser categoriesFor: #moveCardLeft!public! !
!JadeSystemBrowser categoriesFor: #moveCardRight!public! !
!JadeSystemBrowser categoriesFor: #onDropDown:!history!public! !
!JadeSystemBrowser categoriesFor: #onViewOpened!public! !
!JadeSystemBrowser categoriesFor: #queryCommand:!public! !
!JadeSystemBrowser categoriesFor: #renameSelection!public! !
!JadeSystemBrowser categoriesFor: #restoreLayoutAndContents!public! !
!JadeSystemBrowser categoriesFor: #restoreLayoutAndContentsFromFile!public! !
!JadeSystemBrowser categoriesFor: #saveLayoutAndContents!public! !
!JadeSystemBrowser categoriesFor: #selectClass:selector:!public! !
!JadeSystemBrowser categoriesFor: #shellName!overrides!private! !
!JadeSystemBrowser categoriesFor: #statusBarServerRequestText:!public! !
!JadeSystemBrowser categoriesFor: #statusBarText:!public! !
!JadeSystemBrowser categoriesFor: #update!public! !

!JadeSystemBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1801 1201 549 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 8 410 ##(Smalltalk.StatusBar) 34 18 nil 416 34 2 8 1409288460 1 608 nil 480 nil 7 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 nil 608 nil 8 1949670520 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 608 nil 459270 ##(Smalltalk.Message) #displayString 8 #() 850 #iconImageIndex 8 #() 1049926 1 ##(Smalltalk.IconImageManager) 8 'statusBarField' 34 1 832 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 608 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1039 514 1769 45 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 7 2 0 0 116 3 0 0 29 2 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 1282 #fixedParentRight 1 1282 #fixedParentBottom -43 1328 1 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409288972 131137 1344 nil 480 nil 519 nil 674 nil true 706 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 1344 480 8 1949831345 170 192 8 #() 170 192 34 6 46911 1115206 1 ##(Smalltalk.ToolbarIconButton) 1344 1 1180998 4 ##(Smalltalk.CommandDescription) #moveCardRight 8 'Move Card Right' 1 1 788806 1 ##(Smalltalk.TextTileIcon) $\x25B6 8 'Cambria' nil 262406 ##(Smalltalk.ARGB) 8 4281833960 nil nil 1 nil nil 46911 46913 1554 1344 1 1586 #closeCard 8 'Close This Card' 1 1 1634 $\x2716 nil nil nil nil nil 1 nil nil 46913 46909 1554 1344 1 1586 #moveCardLeft 8 'Move Card Left' 1 1 1634 $\x25C0 8 'Cambria' nil 1682 8 4281833704 nil nil 1 nil nil 46909 34 4 1049158 1 ##(Smalltalk.ToolbarSeparator) 1344 1 1792 1568 1728 nil nil 1 nil 514 33 33 514 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 1026 138 144 34 2 1090 #createAt:extent: 34 2 514 1609 1 514 161 51 1344 1090 #updateSizePosted 1504 1344 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 36 3 0 0 0 0 0 0 116 3 0 0 25 0 0 0] 8 #() 1232 nil 27 1250 1312 -159 1282 #fixedViewLeft 161 1282 #fixedParentTop 1 1282 #fixedViewTop 51 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 2256 nil 480 nil 1031 nil nil nil 2256 655878 ##(Smalltalk.CardLayout) 138 144 1504 nil 170 192 1504 nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 2256 34 2 8 1140916736 1 2384 590662 2 ##(Smalltalk.ListModel) 138 144 1504 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) nil nil 1 nil nil nil 2384 nil 8 1949556446 787814 3 ##(Smalltalk.BlockClosure) 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 2560 7 257 nil nil 944 nil nil nil nil nil #noIcons nil nil nil nil nil 1026 138 144 34 2 1090 #createAt:extent: 34 2 514 1 1 514 1785 997 2384 1090 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2384 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 242 1 0 0] 8 #() 1232 nil 27 1026 138 144 34 1 1090 #createAt:extent: 34 2 514 -7 51 514 1785 997 2256 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 252 255 255 255 25 0 0 0 120 3 0 0 11 2 0 0] 34 1 2384 1232 nil 27 1250 1296 -7 1312 9 2224 51 1328 -35 410 ##(Smalltalk.Toolbar) 34 25 nil 416 34 2 8 1409288972 131137 2992 nil 480 nil 519 nil 674 nil true 706 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 2992 480 8 1949831345 170 192 1504 170 192 34 18 48347 1554 2992 1 1586 #addWorkspace 8 'Add Workspace' 1 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'NewWorkspace.ico' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil 48347 48349 1554 2992 1 1586 #addSystemBrowser 8 'Add System Browser' 1 1 3234 nil true 3280 8 'SystemBrowserShell.ico' 3328 48349 48351 1554 2992 1 1586 #browseUsers 8 'Browse Users' 1 1 3234 nil true 3280 8 'MethodProtocol.ico' 3328 48351 48043 1554 2992 1 1586 #abortTransaction 8 'Abort Transaction' 1 1 3234 nil true 3280 8 'Panic.ico' 3328 48043 48045 1554 2992 1 1586 #commitTransaction 8 'Commit Transaction' 1 1 3234 nil true 3280 8 'Snapshot.ico' 3328 48045 46901 1554 2992 1 1586 #maximizeCode 8 'Maximize Code' 1 1 3234 nil true 3280 8 'Shell.ico' 3328 46901 48697 1554 2992 1 1586 #jadeBrowseTests 8 'Open SUnit Browser' 1 1 3234 nil true 3280 8 'TestRunner.ico' 3328 48697 46905 1554 2992 17 1586 #historyBack 8 'History Back' 1 1 3234 nil true 3280 8 'HistoryBack.ico' 3328 46905 46907 1554 2992 17 1586 #historyForward 8 'History Forward' 1 1 3234 nil true 3280 8 'HistoryForward.ico' 3328 46907 34 13 3520 3600 1922 2992 1 3184 3360 3440 1922 2992 1 3680 1922 2992 1 3760 1922 2992 1 3840 3920 nil nil 1 nil 514 33 33 514 45 45 nil 1986 1 1 1 1026 138 144 34 2 1090 #createAt:extent: 34 2 514 -1 1 514 1611 51 2992 1090 #updateSizePosted 1504 2992 1170 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 36 3 0 0 25 0 0 0] 8 #() 1232 nil 27 1250 1296 -1 1312 -159 2224 1 2240 51 170 192 34 8 608 8 'statusBar' 1344 8 'rightToolbar' 2256 8 'cardContainer' 2992 8 'leftToolbar' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 1 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1586 #close 8 'Exit' 1025 1 nil nil nil 8 'Jade' nil 134217729 nil nil 46889 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 1 nil nil nil nil 1 nil nil 1026 138 144 34 3 1090 #createAt:extent: 34 2 514 3059 21 514 1801 1201 416 1090 #text: 34 1 8 'Jade System Browser' 416 1090 #updateMenuBar 1504 416 1170 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 5 0 0 10 0 0 0 125 9 0 0 98 2 0 0] 34 4 2992 1344 2256 608 1232 nil 27 )! !
!JadeSystemBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

