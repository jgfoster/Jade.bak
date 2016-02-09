"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #UserListPresenter
	instanceVariableNames: 'userListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
UserListPresenter guid: (GUID fromString: '{4556FB52-1146-43B0-94FD-10FE7D352252}')!
UserListPresenter comment: 'Presents a list of UserProfile instances in the AllUsersPresenter in the AllUsersShell.'!
!UserListPresenter categoriesForClass!Unclassified! !
!UserListPresenter methodsFor!

createComponents

	super createComponents.
	userListPresenter	:= self add: ListPresenter new name: 'userList'.
!

createSchematicWiring

	super createSchematicWiring.
	userListPresenter when: #'selectionChanged' send: #'trigger:' to: self with: #'selectionChanged'.
!

fillUserList

	userListPresenter list: (GsUserProfile allIn: gciSession).
!

newUser

	| newUserID existingUser string list |
	newUserID := Prompter 
		prompt: 'Enter new user ID:' 
		caption: 'Jade - New User'.
	newUserID isNil ifTrue: [^self].
	existingUser := userListPresenter list
		detect: [:each | each name = newUserID]
		ifNone: [nil].
	existingUser notNil ifTrue: [
		userListPresenter selection: existingUser.
		^self
	].
	string := gciSession
		serverPerform: #'newUser:'
		with: newUserID.
	list := GsUserProfile
		listFromString: string 
		session: gciSession.
	list := userListPresenter list , list.
	userListPresenter
		list: list;
		selectionByIndex: list size.
!

onViewOpened

	super onViewOpened.
	self fillUserList.
!

selectFirstUser

		userListPresenter list isEmpty ifTrue: [^self].
		userListPresenter selectionByIndex: 1.
!

selection

	^userListPresenter selectionOrNil.
! !
!UserListPresenter categoriesFor: #createComponents!public! !
!UserListPresenter categoriesFor: #createSchematicWiring!public! !
!UserListPresenter categoriesFor: #fillUserList!public! !
!UserListPresenter categoriesFor: #newUser!public! !
!UserListPresenter categoriesFor: #onViewOpened!public! !
!UserListPresenter categoriesFor: #selectFirstUser!public! !
!UserListPresenter categoriesFor: #selection!public! !

!UserListPresenter class methodsFor!

publishedEventsOfInstances
    
    	^super publishedEventsOfInstances
			add: #'selectionChanged';
			yourself.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 2 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 592 590662 2 ##(Smalltalk.ListModel)  202 208 544 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #newUser 8 '&New User' 1025 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 592 0 8 4294903731 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 5 920646 5 ##(Smalltalk.ListViewColumn)  8 'User' 241 8 #left 978 1008 1024 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] 8 #name 1216 7 257 0 0 592 0 1 0 0 1106 8 'Last Login' 321 1152 978 1008 98 0 978 8 #<= 1392 1202 0 0 1234 2 1 1264 8 'doIt' 8 '[:each | each lastLoginTime]' 8 #[30 105 226 0 106] 8 #lastLoginTime 1440 7 257 0 0 592 0 1 0 0 1106 8 'Logins Left' 161 1152 978 1008 1392 978 1424 1392 1202 0 0 1234 2 1 1264 8 'doIt' 8 '[:each | each remainingLogins]' 8 #[30 105 226 0 106] 8 #remainingLogins 1600 7 257 0 0 592 0 1 0 0 1106 8 'Is Disabled' 161 1152 978 1008 1392 978 1424 1392 1202 0 0 1234 2 1 1264 8 'doIt' 8 '[:each | each isDisabled]' 8 #[30 105 226 0 106] 8 #isDisabled 1760 7 257 0 0 592 0 1 0 0 1106 8 'Max Sessions' 201 1152 978 1008 98 0 978 1424 1904 1202 0 0 1234 2 1 1264 8 'doIt' 8 '[:each | each maxSessions]' 8 #[30 105 226 0 106] 8 #maxSessions 1936 7 257 0 0 592 0 1 0 0 8 #report 544 0 131169 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 2178 1161 301 592 2114 8 #contextMenu: 98 1 816 592 2114 8 #text: 98 1 8 'User' 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 68 2 0 0 150 0 0 0] 98 0 2178 193 193 0 27 8 'userList' 0 2050 202 208 98 1 2114 2144 98 2 2178 5119 21 2178 1161 301 416 2338 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 67 12 0 0 160 0 0 0] 98 1 592 2400 0 27 )! !
!UserListPresenter class categoriesFor: #publishedEventsOfInstances!public! !
!UserListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

