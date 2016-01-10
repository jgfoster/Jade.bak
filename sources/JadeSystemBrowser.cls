"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadeSystemBrowser
	instanceVariableNames: 'cardsPresenter roundTripCount'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSystemBrowser guid: (GUID fromString: '{72210941-97B6-4113-8EC8-E23015415DE6}')!
JadeSystemBrowser comment: ''!
!JadeSystemBrowser categoriesForClass!Unclassified! !
!JadeSystemBrowser methodsFor!

abortTransaction

	self isOkayToChange ifTrue: [
		model abort.
		self update.
	].
!

addSystemBrowser

	self addSystemBrowserWithLayoutInfo: (self currentCard ifNotNil: [:currentCard | currentCard layoutInfo]).
!

addSystemBrowserForClass: anArray

	(JadeAutoSystemBrowserPresenter
		createIn: cardsPresenter 
		on: model)
		updateAfterFindClass: anArray
		isMeta: nil 
		selector: ''.
!

addSystemBrowserWithLayoutInfo: each

	(JadeAutoSystemBrowserPresenter
		createIn: cardsPresenter 
		on: model)
		layoutInfo: each.
!

addWorkspace

	JadeWorkspace showOn: self model.
!

closeCard

	cardsPresenter view removeSubView: self currentCard view.!

closeRequested: anAssociation

	anAssociation value ifTrue: [
		self isOkayToChange ifTrue: [
			self saveLayoutAndContents.
		] ifFalse: [
			anAssociation value: false.
		].
	].
!

commitTransaction

	self model commit ifTrue: [
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

	super createSchematicWiring.
	cardsPresenter view 	when: #'currentCardChanged' 	send: #'update' 					to: self.
	self  							when: #'closeRequested:'			send: #'closeRequested:'	to: self.
	self  view 					when: #'viewActivated'				send: #'update'					to: self.
!

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
	model forceLogout.
!

initialize

	super initialize.
	roundTripCount := 0.
!

isOkayToChange

	cardsPresenter view cards do: [:each | 
		each presenter isOkayToChange ifFalse: [^false].
	].
	^true.
!

logoutRequested: aValueHolder
	"Opportunity to save changes."

	self closeRequested: aValueHolder.
!

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

onViewOpened

	super onViewOpened.
	cardsPresenter model: model.
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
	Keyboard default isShiftDown ifTrue: [self halt].
!

restoreLayoutAndContents

	self restoreLayoutAndContentsFromFile ifFalse: [
		self view position: 115 @ 70.
		self addSystemBrowserWithLayoutInfo: nil.
	].
!

restoreLayoutAndContentsFromFile

	| path file bytes data |
	path := SessionManager current imageBase , 'Jade\Jade System Browser Layout.stb'.
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

	| path data |
	path := SessionManager current imageBase , 'Jade\Jade System Browser Layout.stb' .
	data := OrderedCollection new
		add: 1;	"file version number"
		add: self view rectangle;
		add: (cardsPresenter view cards collect: [:each | each presenter layoutInfo]);
		yourself.
	(File 
		open: path
		mode: #truncate 
		check: false)
		write: data binaryStoreBytes;
		close.
!

selectClass: classString selector: methodString

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
!JadeSystemBrowser categoriesFor: #createSchematicWiring!public! !
!JadeSystemBrowser categoriesFor: #currentCard!public! !
!JadeSystemBrowser categoriesFor: #handleInvalidSession!public! !
!JadeSystemBrowser categoriesFor: #initialize!public! !
!JadeSystemBrowser categoriesFor: #isOkayToChange!public! !
!JadeSystemBrowser categoriesFor: #logoutRequested:!public! !
!JadeSystemBrowser categoriesFor: #moveCardLeft!public! !
!JadeSystemBrowser categoriesFor: #moveCardRight!public! !
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1801 1201 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 8 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1409286144 131073 624 0 482 8 4278190080 0 1031 0 0 0 624 655878 ##(Smalltalk.CardLayout)  202 208 98 0 0 234 256 784 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 624 98 2 8 1140916736 1 816 590662 2 ##(Smalltalk.ListModel)  202 208 784 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  0 0 1 0 0 0 816 0 8 4294903287 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 8 #displayString 1008 7 257 0 994 0 0 1026 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 1120 7 257 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 8 #smallIcons 0 0 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 1785 999 816 1330 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 816 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 243 1 0 0] 98 0 530 193 193 0 27 1266 202 208 98 1 1330 1360 98 2 530 -7 51 530 1785 999 624 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 252 255 255 255 25 0 0 0 120 3 0 0 12 2 0 0] 98 1 816 1536 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft -7 1746 8 #fixedParentRight 9 1746 8 #fixedParentTop 51 1746 8 #fixedParentBottom -35 410 8 ##(Smalltalk.Toolbar)  98 25 0 416 98 2 8 1409288972 131137 1888 0 482 8 4278190080 0 519 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 1888 482 1984 8 4294903851 234 256 784 234 256 98 6 24993 853766 ##(Smalltalk.ToolbarButton)  24993 0 1888 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #moveCardRight 8 'Move Card Right' 1 1 0 657990 3 ##(Smalltalk.DIBSection)  0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator)  8 'IdeaSpaceCardBar.bmp' 0 0 7 530 161 33 17 0 3 24995 2178 24995 0 1888 1 2210 8 #closeCard 8 'Close Card' 1 1 0 2288 5 24991 2178 24991 0 1888 1 2210 8 #moveCardLeft 8 'Move Card Left' 1 1 0 2288 1 98 4 1050118 ##(Smalltalk.ToolbarSeparator)  0 0 1888 3 0 1 2432 2192 2368 234 240 98 2 2288 1 0 1 0 530 33 33 530 45 45 0 656198 1 ##(Smalltalk.FlowLayout)  1 1 1 1266 202 208 98 2 1330 1360 98 2 530 1609 1 530 161 51 1888 1330 8 #updateSize 784 1888 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 36 3 0 0 0 0 0 0 116 3 0 0 25 0 0 0] 98 0 1536 0 27 1714 1792 -159 1746 8 #fixedViewLeft 161 1824 1 1746 8 #fixedViewTop 51 410 8 ##(Smalltalk.StatusBar)  98 18 0 416 98 2 8 1409288460 1 2912 0 482 8 4278190080 0 7 0 2002 0 16 2034 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 530 193 193 0 2912 0 8 4294903489 234 256 98 2 853766 ##(Smalltalk.StatusBarItem)  1 -1 2912 0 459270 ##(Smalltalk.Message)  1104 98 0 3170 1200 98 0 1232 8 'statusBarField' 98 1 3152 1115142 ##(Smalltalk.StatusBarNullItem)  513 1 2912 0 0 1266 202 208 98 1 1330 1360 98 2 530 1 1041 530 1769 45 2912 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 8 2 0 0 116 3 0 0 30 2 0 0] 98 0 1536 0 27 1714 1760 1 1792 1 1856 -43 1856 1 410 1904 98 25 0 416 98 2 8 1409288972 131137 3488 0 482 8 4278190080 0 519 0 2002 0 16 2034 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 3488 482 3568 8 4294903851 234 256 784 234 256 98 10 24981 2178 24981 0 3488 1 2210 8 #abortTransaction 8 'Abort Transaction' 1 1 0 395334 3 ##(Smalltalk.Bitmap)  0 16 2320 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy)  8 'dolphindr006.dll' 0 0 7 530 1857 33 1 24983 2178 24983 0 3488 1 2210 8 #commitTransaction 8 'Commit Transaction' 1 1 0 3808 27 24985 1246982 ##(Smalltalk.ToolbarSystemButton)  24985 0 3488 1 2210 8 #addWorkspace 8 'Add Workspace' 1 1 0 1 13 24987 2178 24987 0 3488 1 2210 8 #addSystemBrowser 8 'Add System Browser' 1 1 0 3808 17 24989 2178 24989 0 3488 1 2210 8 #browseUsers 8 'Browse Users' 1 1 0 3808 75 98 7 3728 3904 2514 0 0 3488 3 0 1 3984 4048 4112 2514 0 0 3488 3 0 1 234 240 98 4 1 117 3808 1 0 1 0 530 33 33 530 45 45 0 2610 1 1 1 1266 202 208 98 2 1330 1360 98 2 530 -1 1 530 1611 51 3488 1330 2768 784 3488 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 36 3 0 0 25 0 0 0] 98 0 1536 0 27 1714 1760 -1 1792 -159 1824 1 2880 51 234 256 98 8 624 8 'cardContainer' 1888 8 'rightToolbar' 2912 8 'statusBar' 3488 8 'leftToolbar' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 1 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 2210 8 #close 8 'Exit' 1025 1 0 0 0 8 'Jade' 0 134217729 0 0 24979 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1266 202 208 98 3 1330 1360 98 2 530 6239 21 530 1801 1201 416 1330 8 #text: 98 1 8 'Jade System Browser' 416 1330 8 #updateMenuBar 784 416 1474 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 47 12 0 0 10 0 0 0 179 15 0 0 98 2 0 0] 98 4 3488 1888 624 2912 1536 0 27 )! !
!JadeSystemBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

