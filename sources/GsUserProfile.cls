"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #GsUserProfile
	instanceVariableNames: 'lastLoginTime remainingLogins isDisabled maxSessions language disabledReason lastPasswordChange isSpecial'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsUserProfile guid: (GUID fromString: '{399e972a-24dd-4e6c-a458-fc8e949470a5}')!
GsUserProfile comment: ''!
!GsUserProfile categoriesForClass!Unclassified! !
!GsUserProfile methodsFor!

disabledReason

	^disabledReason.
!

editGroup: anArray value: aBoolean

	| selector |
	anArray
		at: 3
		put: aBoolean.
	selector := aBoolean
		ifTrue: [#'addGroup:toUser:']
		ifFalse: [#'removeGroup:fromUser:'].
	gciSession
		serverPerform: selector
		with: (anArray at: 2) 
		with: self.
!

editPrivilege: anArray value: aBoolean

	| selector |
	anArray
		at: 3
		put: aBoolean.
	selector := aBoolean
		ifTrue: [#'addPrivilege:toUser:']
		ifFalse: [#'removePrivilege:fromUser:'].
	gciSession
		serverPerform: selector
		with: (anArray at: 2) 
		with: self.
!

groupList

	| string list |
	string := gciSession
		serverPerform: #'groupListFor:'
		with: self.
	list := string subStrings: Character lf.
	list := list collect: [:each | each subStrings: Character tab].
	list := list collect: [:each | Array with: self with: (each at: 1) with: ((each at: 2) = 'true')].
	^list.
!

initialize: aList

	| list |
	list := aList collect: [:each | each = 'nil' ifTrue: [nil] ifFalse: [each]].
	list size < 10 ifTrue: [^self].
	lastLoginTime 				:= list at: 3.
	remainingLogins 			:= list at: 4.
	isDisabled 					:= (list at: 5) = 'true'.
	maxSessions 				:= list at: 6.
	language						:= list at: 7.
	disabledReason				:= list at: 8.
	lastPasswordChange 	:= list at: 9.
	isSpecial 						:= (list at: 10) = 'true'.
!

isDisabled

	^isDisabled.
!

isSpecial

	^isSpecial.
!

language

	^language.
!

lastLoginTime

	^lastLoginTime.
!

lastPasswordChange

	^lastPasswordChange.
!

maxSessions

	^maxSessions.
!

privilegeList

	| string list |
	string := gciSession
		serverPerform: #'privilegeListFor:'
		with: self.
	list := string subStrings: Character lf.
	list := list collect: [:each | each subStrings: Character tab].
	list := list collect: [:each | Array with: self with: (each at: 1) with: ((each at: 2) = 'true')].
	^list.
!

remainingLogins

	^remainingLogins.
!

symbolList

	| string list |
	string := gciSession
		serverPerform: #'dictionaryListFor:'
		with: self.
	list := string subStrings: Character lf.
	list := list collect: [:each | GsSymbolDictionary new initialize: each session: gciSession].
	^list.
! !
!GsUserProfile categoriesFor: #disabledReason!public! !
!GsUserProfile categoriesFor: #editGroup:value:!public! !
!GsUserProfile categoriesFor: #editPrivilege:value:!public! !
!GsUserProfile categoriesFor: #groupList!public! !
!GsUserProfile categoriesFor: #initialize:!public! !
!GsUserProfile categoriesFor: #isDisabled!public! !
!GsUserProfile categoriesFor: #isSpecial!public! !
!GsUserProfile categoriesFor: #language!public! !
!GsUserProfile categoriesFor: #lastLoginTime!public! !
!GsUserProfile categoriesFor: #lastPasswordChange!public! !
!GsUserProfile categoriesFor: #maxSessions!public! !
!GsUserProfile categoriesFor: #privilegeList!public! !
!GsUserProfile categoriesFor: #remainingLogins!public! !
!GsUserProfile categoriesFor: #symbolList!public! !

!GsUserProfile class methodsFor!

allIn: aGciSession

	| string list |
	(string := aGciSession serverPerform: #'userList') isNil ifTrue: [^#()].
	list := string subStrings: Character lf.
	list := list collect: [:each | self new initialize: each session: aGciSession].
	^list.
! !
!GsUserProfile class categoriesFor: #allIn:!public! !

