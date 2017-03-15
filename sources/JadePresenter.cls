"Filed out from Dolphin Smalltalk 7"!

Presenter subclass: #JadePresenter
	instanceVariableNames: 'gciSession'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter guid: (GUID fromString: '{b55f33eb-59fb-46d6-8fec-7df1571c7d45}')!
JadePresenter comment: ''!
!JadePresenter categoriesForClass!MVP-Presenters! !
!JadePresenter methodsFor!

gciSession

	^gciSession
!

gciSession: aGciSession

	gciSession := aGciSession.
!

resetCursor

	(Delay forMilliseconds: 20) wait.
	[Cursor current: nil] forkAt: Processor userBackgroundPriority.
!

subMenuName

	^nil.
!

subMenuPresenter

	^nil.
!

updateMenuBar: aMenuBar

	self
		updateMenuBar: aMenuBar 
		withName: self subMenuName 
		itemsFrom: self subMenuPresenter.
!

updateMenuBar: aMenuBar withName: aString itemsFrom: aPresenter

	| contextMenu commandsToRemove items newMenu |
	aString isNil ifTrue: [^self].
	(contextMenu := aPresenter view contextMenu) isNil ifTrue: [^self].
	commandsToRemove := #(#'rename').
	items := contextMenu items reject: [:each | commandsToRemove includes: each command].
	items isEmpty ifTrue: [^self].
	newMenu := aMenuBar addSubmenu: aString.
	1 to: items size do: [:i | 
		| item description |
		item := (items at: i) copy.
		(item isKindOf: CommandMenuItem) ifTrue: [
			description := item commandDescription.
			description := ClosedCommandDescription new
				command: 		description command;
				description: 		description description;
				acceleratorKey: 	description acceleratorKey;
				isModalCommand: 	description isModalCommand;
				image: 			description image;
				receiver:			self;
				queryBlock: 		[:aCommandQuery | self queryCommand: aCommandQuery];
				yourself.
			item commandDescription: description.
		].
		newMenu 
			insertItem: item
			at: i.
	].
! !
!JadePresenter categoriesFor: #gciSession!public! !
!JadePresenter categoriesFor: #gciSession:!public! !
!JadePresenter categoriesFor: #resetCursor!public! !
!JadePresenter categoriesFor: #subMenuName!menus!public! !
!JadePresenter categoriesFor: #subMenuPresenter!menus!public! !
!JadePresenter categoriesFor: #updateMenuBar:!menus!public! !
!JadePresenter categoriesFor: #updateMenuBar:withName:itemsFrom:!menus!public! !

