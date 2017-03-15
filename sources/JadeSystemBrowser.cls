"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadeSystemBrowser
	instanceVariableNames: 'cardsPresenter roundTripCount historyEntriesDictionary historyCurrentIndexDictionary'
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
		on: gciSession)
		updateAfterFindClass: anArray
		isMeta: nil 
		selector: ''.
!

addSystemBrowserWithLayoutInfo: each

	(JadeAutoSystemBrowserPresenter
		createIn: cardsPresenter 
		gciSession: gciSession)
		layoutInfo: each.
!

addWorkspace

	JadeWorkspace showOnSession: gciSession.
!

buildHistoryBackMenu
	| systemBrowser currentIndex entryCollection |

	systemBrowser := self currentCard.
	entryCollection := historyEntriesDictionary at: systemBrowser ifAbsent: [^Menu new].
	((historyCurrentIndexDictionary at: systemBrowser ifAbsent: [0]) <= 1) ifTrue: [^Menu new].

	currentIndex := historyCurrentIndexDictionary at: systemBrowser put: (historyCurrentIndexDictionary at: systemBrowser).

	^(self buildHistoryMenuWith: (entryCollection copyFrom: 1 to: currentIndex) command: #historyBack:)
		text: 'Backward';
		yourself	
!

buildHistoryForwardMenu
	| systemBrowser currentIndex entryCollection |

	systemBrowser := self currentCard.
	entryCollection := historyEntriesDictionary at: systemBrowser ifAbsent: [^Menu new].
	((historyCurrentIndexDictionary at: systemBrowser ifAbsent: [0]) >= entryCollection size) ifTrue: [^Menu new].

	currentIndex := historyCurrentIndexDictionary at: systemBrowser put: (historyCurrentIndexDictionary at: systemBrowser).

	^(self buildHistoryMenuWith: (entryCollection copyFrom: currentIndex to: entryCollection size) command: #historyForward:)
		text: 'Forward';
		yourself!

buildHistoryMenuWith: entryCollection command: command
	| popup subMenu classesNames |

	classesNames := Set new.
	popup := Menu new.

	entryCollection do: [:historyEntry | 
		(classesNames includes: historyEntry getClassName) 
		ifFalse: [subMenu := popup addSubmenu: historyEntry getClassName.
				classesNames add: historyEntry getClassName].
		subMenu addCommand: (Message selector: command argument: historyEntry) description: historyEntry getMethodName.
		subMenu setDefault: 1
	].

	popup setDefault: 1.
	^popup!

closeCard
	| cardToClose |

	cardToClose := self currentCard.
	historyCurrentIndexDictionary removeKey: cardToClose ifAbsent: [].
	historyEntriesDictionary removeKey: cardToClose ifAbsent: [].

	cardsPresenter view removeSubView: cardToClose view.!

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
	gciSession forceLogout.
!

historyBack
	| systemBrowser currentIndex entryCollection |

	systemBrowser := self currentCard.
	entryCollection := historyEntriesDictionary at: systemBrowser ifAbsent: [^self].
	entryCollection isEmpty ifTrue: [^self].

	currentIndex := historyCurrentIndexDictionary at: systemBrowser put: 
				(((historyCurrentIndexDictionary at: systemBrowser ifAbsent: [0]) <= 1) 
				ifTrue: [entryCollection size]
				ifFalse: [(historyCurrentIndexDictionary at: systemBrowser) - 1]).

	systemBrowser restoreFrom: (entryCollection at: currentIndex).!

historyBack: aJadeHistoryEntry

	self currentCard restoreFrom: aJadeHistoryEntry!

historyCurrentIndexDictionary
	^historyCurrentIndexDictionary!

historyCurrentIndexDictionary: anObject
	historyCurrentIndexDictionary := anObject!

historyEntriesDictionary
	^historyEntriesDictionary!

historyEntriesDictionary: anObject
	historyEntriesDictionary := anObject!

historyForward
	| systemBrowser currentIndex entryCollection |

	systemBrowser := self currentCard.
	entryCollection := historyEntriesDictionary at: systemBrowser ifAbsent: [^self].
	entryCollection isEmpty ifTrue: [^self].

	currentIndex := historyCurrentIndexDictionary at: systemBrowser put: 
				(((historyCurrentIndexDictionary at: systemBrowser ifAbsent: [0]) >= entryCollection size) 
				ifTrue: [1]
				ifFalse: [(historyCurrentIndexDictionary at: systemBrowser) + 1]).

	systemBrowser restoreFrom: (entryCollection at: currentIndex)!

historyForward: aJadeHistoryEntry

	self currentCard restoreFrom: aJadeHistoryEntry!

initialize

	super initialize.
	roundTripCount := 0.
	historyCurrentIndexDictionary := Dictionary new.
	historyEntriesDictionary := Dictionary new.
!

isOkayToChange

	cardsPresenter view cards do: [:each | 
		each presenter isOkayToChange ifFalse: [^false].
	].
	^true.
!

jadeBrowseTests

	JadeSUnitBrowser showOnSession: gciSession.
!

logoutRequested: aValueHolder
	"Opportunity to save changes."

	self closeRequested: aValueHolder.
!

maximizeCode
	| codePresenter |

	(self currentCard class == JadeAutoSystemBrowserPresenter) ifFalse: [^self].

	codePresenter := self currentCard methodSourcePresenter .

	^(JadeMaximizedCodeShell showOn: codePresenter  model) parentMethodPresenter: codePresenter !

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

	aToolbarButton command == #historyBack ifTrue: [popup :=  self buildHistoryBackMenu].
	aToolbarButton command == #historyForward ifTrue: [popup := self buildHistoryForwardMenu].
	popup queryAllFromView: self view.
	popup showIn: self position: aToolbarButton screenRectangle bottomLeft.

	^0!

onViewOpened
	| toolbar |

	super onViewOpened.
	cardsPresenter model: gciSession.
	self restoreLayoutAndContents.

	toolbar := self view viewNamed: 'leftToolbar'.
	toolbar presenterConnectionPoint 
		when: #dropDown:
		send: #onDropDown:
		to: self.!

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
	path := SessionManager current imageBase , 'Jade System Browser Layout.stb'.
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
	path := SessionManager current imageBase , 'Jade System Browser Layout.stb' .
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
!

updateHistoryFor: aJadeAutoSystemBrowserPresenter
	| entry |

	"only one method must be selected"
	(aJadeAutoSystemBrowserPresenter methodListPresenter hasSelection and: [aJadeAutoSystemBrowserPresenter methodListPresenter selections size = 1]) ifFalse: [^self].

	(historyEntriesDictionary includesKey: aJadeAutoSystemBrowserPresenter) "check if system browser already exist "
	ifFalse: [historyEntriesDictionary at: aJadeAutoSystemBrowserPresenter put: OrderedCollection new.
			historyCurrentIndexDictionary at: aJadeAutoSystemBrowserPresenter put: 1].

	entry := JadeHistoryEntry newFrom: aJadeAutoSystemBrowserPresenter.

	((historyEntriesDictionary at:  aJadeAutoSystemBrowserPresenter) includes: entry)
	ifFalse: [(historyEntriesDictionary at:  aJadeAutoSystemBrowserPresenter) add:entry].

	"historyCurrentIndexDictionary at: aJadeAutoSystemBrowserPresenter put: ((historyCurrentIndexDictionary at: aJadeAutoSystemBrowserPresenter) + 1)"! !
!JadeSystemBrowser categoriesFor: #abortTransaction!public! !
!JadeSystemBrowser categoriesFor: #addSystemBrowser!public! !
!JadeSystemBrowser categoriesFor: #addSystemBrowserForClass:!public! !
!JadeSystemBrowser categoriesFor: #addSystemBrowserWithLayoutInfo:!public! !
!JadeSystemBrowser categoriesFor: #addWorkspace!public! !
!JadeSystemBrowser categoriesFor: #buildHistoryBackMenu!history!public! !
!JadeSystemBrowser categoriesFor: #buildHistoryForwardMenu!history!public! !
!JadeSystemBrowser categoriesFor: #buildHistoryMenuWith:command:!history!public! !
!JadeSystemBrowser categoriesFor: #closeCard!public! !
!JadeSystemBrowser categoriesFor: #closeRequested:!public! !
!JadeSystemBrowser categoriesFor: #commitTransaction!public! !
!JadeSystemBrowser categoriesFor: #createComponents!public! !
!JadeSystemBrowser categoriesFor: #createSchematicWiring!public! !
!JadeSystemBrowser categoriesFor: #currentCard!public! !
!JadeSystemBrowser categoriesFor: #handleInvalidSession!public! !
!JadeSystemBrowser categoriesFor: #historyBack!history!public! !
!JadeSystemBrowser categoriesFor: #historyBack:!history!public! !
!JadeSystemBrowser categoriesFor: #historyCurrentIndexDictionary!accessing!history!private! !
!JadeSystemBrowser categoriesFor: #historyCurrentIndexDictionary:!accessing!history!private! !
!JadeSystemBrowser categoriesFor: #historyEntriesDictionary!accessing!history!private! !
!JadeSystemBrowser categoriesFor: #historyEntriesDictionary:!accessing!history!private! !
!JadeSystemBrowser categoriesFor: #historyForward!history!public! !
!JadeSystemBrowser categoriesFor: #historyForward:!history!public! !
!JadeSystemBrowser categoriesFor: #initialize!public! !
!JadeSystemBrowser categoriesFor: #isOkayToChange!public! !
!JadeSystemBrowser categoriesFor: #jadeBrowseTests!public! !
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
!JadeSystemBrowser categoriesFor: #updateHistoryFor:!history!public! !

!JadeSystemBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13565952 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1801 1201 551 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 8 410 ##(Smalltalk.Toolbar) 34 25 0 416 34 2 8 1409288972 131137 608 0 466 8 4278190080 0 519 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 0 608 466 688 8 4294905921 170 192 8 #() 170 192 34 6 12211 853766 ##(Smalltalk.ToolbarButton) 12211 0 608 1 1180998 4 ##(Smalltalk.CommandDescription) #closeCard 8 'Close Card' 1 1 0 657990 3 ##(Smalltalk.DIBSection) 0 true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'IdeaSpaceCardBar.bmp' 0 0 7 514 161 33 17 0 5 12207 898 12207 0 608 1 930 #moveCardLeft 8 'Move Card Left' 1 1 0 992 1 12209 898 12209 0 608 1 930 #moveCardRight 8 'Move Card Right' 1 1 0 992 3 34 4 1050118 ##(Smalltalk.ToolbarSeparator) 0 0 608 3 0 1 1072 1120 912 170 176 34 2 992 1 0 1 0 514 33 33 514 45 45 0 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1609 1 514 161 51 608 1378 #updateSizePosted 848 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 36 3 0 0 0 0 0 0 116 3 0 0 25 0 0 0] 8 #() 514 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentRight -159 1586 #fixedViewLeft 161 1586 #fixedParentTop 1 1586 #fixedViewTop 51 410 ##(Smalltalk.CardContainer) 34 16 0 416 34 2 8 1409286144 131073 1664 0 466 8 4278190080 0 1031 0 0 0 1664 655878 ##(Smalltalk.CardLayout) 138 144 848 0 170 192 848 0 410 ##(Smalltalk.TabViewXP) 34 28 0 1664 34 2 8 1140916736 1 1824 590662 2 ##(Smalltalk.ListModel) 138 144 848 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 0 0 1 0 0 0 1824 0 8 4294904009 787814 3 ##(Smalltalk.BlockClosure) 0 0 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 2000 7 257 0 1986 0 0 2018 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 2064 7 257 0 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 #smallIcons 0 0 0 0 0 1314 138 144 34 2 1378 #createAt:extent: 34 2 514 1 1 514 1785 997 1824 1378 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 1824 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 242 1 0 0] 8 #() 1536 0 27 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 -7 51 514 1785 997 1664 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 252 255 255 255 25 0 0 0 120 3 0 0 11 2 0 0] 34 1 1824 1536 0 27 1554 1586 #fixedParentLeft -7 1600 9 1632 51 1586 #fixedParentBottom -35 410 ##(Smalltalk.StatusBar) 34 18 0 416 34 2 8 1409288460 1 2544 0 466 8 4278190080 0 7 0 706 0 true 738 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 514 193 193 0 2544 0 8 4294904347 170 192 34 2 853766 ##(Smalltalk.StatusBarItem) 1 -1 2544 0 459270 ##(Smalltalk.Message) #displayString 8 #() 2786 #iconImageIndex 8 #() 2128 8 'statusBarField' 34 1 2768 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 2544 0 0 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 1 1039 514 1769 45 2544 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 7 2 0 0 116 3 0 0 29 2 0 0] 8 #() 1536 0 27 1554 2512 1 1600 1 2528 -43 2528 1 410 ##(Smalltalk.Toolbar) 34 25 0 416 34 2 8 1409288972 131137 3104 0 466 8 4278190080 0 519 0 706 0 true 738 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 0 3104 466 3184 8 4294905921 170 192 848 170 192 34 18 12191 898 12191 0 3104 1 930 #commitTransaction 8 'Commit Transaction' 1 1 0 395334 3 ##(Smalltalk.Bitmap) 0 true 1024 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 0 7 514 1857 33 27 12193 1246982 ##(Smalltalk.ToolbarSystemButton) 12193 0 3104 1 930 #addWorkspace 8 'Add Workspace' 1 1 0 1 13 12195 898 12195 0 3104 1 930 #addSystemBrowser 8 'Add System Browser' 1 1 0 3408 17 12197 898 12197 0 3104 1 930 #browseUsers 8 'Browse Users' 1 1 0 3408 75 12199 1115910 ##(Smalltalk.ToolbarIconButton) 12199 0 3104 1 930 #maximizeCode 8 'Maximize Code' 1 1 263494 3 ##(Smalltalk.Icon) 0 true 1024 8 'Shell.ico' 3456 3394 0 true 0 0 0 0 3 514 33 33 1 12201 3666 12201 0 3104 1 930 #jadeBrowseTests 8 'Open SUnit Browser' 1 1 3730 0 true 1024 8 'icons\UnitTestPlugin.ico' 0 3394 0 true 0 0 0 0 3 514 33 33 1 12203 3666 12203 0 3104 17 930 #historyBack 8 'History Back' 1 1 3730 0 true 1024 8 'HistoryBack.ico' 3456 3394 0 true 0 0 0 0 3 514 33 33 1 12205 3666 12205 0 3104 17 930 #historyForward 8 'History Forward' 1 1 3730 0 true 1024 8 'HistoryForward.ico' 3456 3394 0 true 0 0 0 0 3 514 33 33 1 12189 898 12189 0 3104 1 930 #abortTransaction 8 'Abort Transaction' 1 1 0 3408 1 34 13 4144 3344 1186 0 0 3104 3 0 1 3520 3568 3616 1186 0 0 3104 3 0 1 3680 1186 0 0 3104 3 0 1 3808 1186 0 0 3104 3 0 1 3920 4032 170 176 34 12 1 117 3408 1 3888 149 3776 147 4000 151 4112 153 0 1 0 514 33 33 514 45 45 0 1282 1 1 1 1314 138 144 34 2 1378 #createAt:extent: 34 2 514 -1 1 514 1611 51 3104 1378 #updateSizePosted 848 3104 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 36 3 0 0 25 0 0 0] 8 #() 1536 0 27 1554 2512 -1 1600 -159 1632 1 1648 51 170 192 34 8 608 8 'rightToolbar' 1664 8 'cardContainer' 2544 8 'statusBar' 3104 8 'leftToolbar' 0 461638 4 ##(Smalltalk.MenuBar) 0 true 34 1 265030 4 ##(Smalltalk.Menu) 0 true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 930 #close 8 'Exit' 1025 1 0 0 0 8 'Jade' 0 134217729 0 0 12999 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1314 138 144 34 3 1378 #createAt:extent: 34 2 514 5119 21 514 1801 1201 416 1378 #text: 34 1 8 'Jade System Browser' 416 1378 #updateMenuBar 848 416 1474 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 131 13 0 0 98 2 0 0] 34 4 3104 608 1664 2544 1536 0 27 )! !
!JadeSystemBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

