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
! !
!JadeSystemBrowser categoriesFor: #abortTransaction!public! !
!JadeSystemBrowser categoriesFor: #addSystemBrowser!public! !
!JadeSystemBrowser categoriesFor: #addSystemBrowserForClass:!public! !
!JadeSystemBrowser categoriesFor: #addSystemBrowserWithLayoutInfo:!public! !
!JadeSystemBrowser categoriesFor: #addWorkspace!public! !
!JadeSystemBrowser categoriesFor: #closeRequested:!public! !
!JadeSystemBrowser categoriesFor: #commitTransaction!public! !
!JadeSystemBrowser categoriesFor: #createComponents!public! !
!JadeSystemBrowser categoriesFor: #createSchematicWiring!public! !
!JadeSystemBrowser categoriesFor: #currentCard!public! !
!JadeSystemBrowser categoriesFor: #handleInvalidSession!public! !
!JadeSystemBrowser categoriesFor: #isOkayToChange!public! !
!JadeSystemBrowser categoriesFor: #logoutRequested:!public! !
!JadeSystemBrowser categoriesFor: #moveCardLeft!public! !
!JadeSystemBrowser categoriesFor: #moveCardRight!public! !
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

