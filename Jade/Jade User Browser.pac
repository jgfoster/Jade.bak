| package |
package := Package name: 'Jade User Browser'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.031'.


package classNames
	add: #AllUsersPresenter;
	add: #AllUsersShell;
	add: #GsSymbolDictionary;
	add: #GsUserProfile;
	add: #UserListPresenter;
	add: #UserProfileDetailsPresenter;
	add: #UserProfilePasswordPresenter;
	add: #UserProfilePresenter;
	add: #UserProfileSetPresenter;
	yourself.

package methodNames
	add: #JadeServer -> #addGroup:toUser:;
	add: #JadeServer -> #addPrivilege:toUser:;
	add: #JadeServer -> #addUser:toStream:;
	add: #JadeServer -> #allGroups;
	add: #JadeServer -> #allUsersPasswordLimits;
	add: #JadeServer -> #categoryListFor:;
	add: #JadeServer -> #classListFor:category:;
	add: #JadeServer -> #dictionaryListFor:;
	add: #JadeServer -> #globalsFor:;
	add: #JadeServer -> #groupListFor:;
	add: #JadeServer -> #newUser:;
	add: #JadeServer -> #privilegeListFor:;
	add: #JadeServer -> #removeGroup:fromUser:;
	add: #JadeServer -> #removePrivilege:fromUser:;
	add: #JadeServer -> #userList;
	add: #JadeServer32bit -> #addUser:toStream:;
	add: #JadeServer64bit -> #addUser:toStream:;
	add: #JadeTextDocument -> #jadeBrowseUsers;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Boolean\Dolphin Boolean Presenter';
	add: '..\Object Arts\Dolphin\MVP\Views\Cards\Dolphin Card Containers';
	add: '..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Number\Dolphin Number Presenter';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter';
	add: '..\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: 'GemStone Objects';
	add: 'GemStone Session';
	add: 'Jade Class Browser';
	add: 'Jade UI Base';
	add: '..\Solutions Software\Widgets\SSW EditableListView';
	yourself).

package!

"Class Definitions"!

GsObject subclass: #GsSymbolDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #GsUserProfile
	instanceVariableNames: 'lastLoginTime remainingLogins isDisabled maxSessions language disabledReason lastPasswordChange isSpecial'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #AllUsersPresenter
	instanceVariableNames: 'userListPresenter userProfilePresenter passwordLimitsPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter subclass: #UserListPresenter
	instanceVariableNames: 'userListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter subclass: #UserProfileDetailsPresenter
	instanceVariableNames: 'userIDPresenter maxLoginsPresenter languagePresenter lastLoginPresenter disabledReasonPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter subclass: #UserProfilePasswordPresenter
	instanceVariableNames: 'whenPasswordChangedPresenter loginsRemainingPresenter isSpecialUserPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter subclass: #UserProfilePresenter
	instanceVariableNames: 'user detailsPresenter passwordPresenter symbolListPresenter groupListPresenter privilegeListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePresenter subclass: #UserProfileSetPresenter
	instanceVariableNames: 'disallowUsedPasswordsPresenter minPasswordSizePresenter maxPasswordSizePresenter maxRepeatingCharsPresenter maxConsecutiveCharsPresenter maxCharsOfSameTypePresenter staleAccountAgeLimitPresenter passwordAgeLimitPresenter passwordAgeWarningPresenter disallowedPasswordListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeShell subclass: #AllUsersShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JadeServer methodsFor!

addGroup: aString toUser: aUserProfile

	aUserProfile addGroup: aString.
!

addPrivilege: aString toUser: aUserProfile

	aUserProfile addPrivilege: aString.
!

addUser: aUserProfile toStream: aStream

	(self oopOf: aUserProfile) printOn: aStream.
	aStream tab; nextPutAll: aUserProfile userId.
	aStream tab; nextPutAll: (aUserProfile lastLoginTime asStringUsingFormat: #(1 2 3 $  2 1 $: true true true false)).
	aStream tab. aUserProfile loginsAllowedBeforeExpiration printOn: aStream.
	aStream tab. aUserProfile isDisabled printOn: aStream.
	aStream tab. aUserProfile activeUserIdLimit printOn: aStream.
	aStream tab.	"; nextPutAll: aUserProfile nativeLanguage asString."
	aStream tab. aUserProfile reasonForDisabledAccount printOn: aStream.
	aStream tab; nextPutAll: (aUserProfile lastPasswordChange asStringUsingFormat: #(1 2 3 $  2 1 $: true true true false)).
	aStream tab. aUserProfile passwordNeverExpires printOn: aStream.
	aStream lf.
!

allGroups

	| allGroups myGroups stream |
	allGroups := AllGroups keys asSortedCollection.
	myGroups := (AllUsers userWithId: 'GcUser') groups.
	stream := WriteStream on: String new.
	allGroups do: [:each | 
		stream nextPutAll: each; tab.
		(myGroups includes: each) printOn: stream.
		stream lf.
	].
	^stream contents.
!

allUsersPasswordLimits

	| stream |
	stream := WriteStream on: String new.
	AllUsers disallowUsedPasswords printOn: stream. stream tab.
	AllUsers minPasswordSize printOn: stream. stream tab.
	AllUsers maxPasswordSize printOn: stream. stream tab.
	AllUsers maxRepeatingChars printOn: stream. stream tab.
	AllUsers maxConsecutiveChars printOn: stream. stream tab.
	AllUsers maxCharsOfSameType printOn: stream. stream tab.
	AllUsers staleAccountAgeLimit printOn: stream. stream tab.
	AllUsers passwordAgeLimit printOn: stream. stream lf.
	AllUsers disallowedPasswords do: [:each | 
		stream nextPutAll: each; tab.
	].
	stream lf. AllUsers passwordAgeWarning printOn: stream. stream lf.
	^stream contents.
!

categoryListFor: aSymbolDictionary

	| categories stream |
	categories := Set new.
	aSymbolDictionary do: [:each | 
		each isBehavior ifTrue: [
			categories add: each category.
		].
	].
	categories copy do: [:each | 
		1 to: each size do: [:i | 
			(each at: i) = $- ifTrue: [
				| string |
				string := each copyFrom: 1 to: i - 1.
				(categories includes: string) ifFalse: [
					categories add: string.
					self _addToPureExportSet: string.
				].
			].
		].
	].
	stream := WriteStream on: String new.
	categories asSortedCollection do: [:each | 
		(self oopOf: each) printOn: stream.
		stream tab; nextPutAll: each; lf.
	].
	^stream contents.
!

classListFor: aDictionary category: aString

	| visibleClasses allClasses stream queue |
	visibleClasses := aDictionary asArray select: [:each | 
		each isBehavior and: [aString isNil or: [
			| category |
			(category := each category) notNil and: [
			category = aString or: [
			category matchPattern: (Array with: aString with: $*)]]]]].
	allClasses := visibleClasses asIdentitySet.
	queue := visibleClasses asOrderedCollection.
	[
		queue notEmpty.
	] whileTrue: [
		| parent |
		parent := queue removeFirst superclass.
		(parent notNil and: [(allClasses includes: parent) not]) ifTrue: [
			queue add: parent.
			allClasses add: parent.
		].
	].
	stream := WriteStream on: String new.
	allClasses do: [:each |
		self
			_addClass: each 
			toStream: stream 
			isVisible: (visibleClasses includes: each)
			fromDictionary: aDictionary.
	].
	^stream contents.
!

dictionaryListFor: aUserProfile

	| symbolList list stream |
	symbolList := aUserProfile symbolList.
	list := symbolList namesReport subStrings: Character lf.
	list := list reject: [:each | each isEmpty].
	list := list collect: [:each | each subStrings].
	stream := WriteStream on: String new.
	list do: [:each | 
		(self oopOf: (symbolList at: (each at: 1) asNumber)) printOn: stream.
		stream tab; nextPutAll: (each at: 2); lf.
	].
	^stream contents.
!

globalsFor: aSymbolDictionary

	| stream |
	stream := WriteStream on: String new.
	aSymbolDictionary keysAndValuesDo: [:eachKey :eachValue | 
		eachValue isBehavior ifFalse: [
			| data |
			data := (self _oopAndStringFor: eachValue) value.
			data size > 200 ifTrue: [data := data copyFrom: 1 to: 200].
			data := String withAll: (data asArray collect: [:each | (each >= Character space and: [each <= $~]) ifTrue: [each] ifFalse: [$?]]).
			stream
	"1"		nextPutAll: (self oopOf: eachValue) printString; tab;
	"2"		nextPutAll: eachKey; tab;
	"3"		nextPutAll: eachValue class name; tab;
	"4"		nextPutAll: data; tab;
				lf;
				yourself.
		].
	].
	^stream contents.
!

groupListFor: aUserProfile

	| allGroups myGroups stream |
	allGroups := AllGroups keys asSortedCollection.
	myGroups := aUserProfile groups.
	stream := WriteStream on: String new.
	allGroups do: [:each | 
		stream nextPutAll: each; tab.
		(myGroups includes: each) printOn: stream.
		stream lf.
	].
	^stream contents.
!

newUser: aString

	| userProfile stream |
	userProfile := UserProfile 
		newWithUserId: aString
		password: 'swordfish'
		privileges: #()
		inGroups: #().
	stream := WriteStream on: String new.
	self
		addUser: userProfile 
		toStream: stream.
	^stream contents.
!

privilegeListFor: aUserProfile

	| allPrivileges myPrivileges stream |
	allPrivileges := (aUserProfile class instVarAt: 6) at: #'PrivilegeNames'.
	myPrivileges := aUserProfile privileges.
	stream := WriteStream on: String new.
	allPrivileges do: [:each | 
		stream nextPutAll: each; tab.
		(myPrivileges includes: each) printOn: stream.
		stream lf.
	].
	^stream contents.
!

removeGroup: aString fromUser: aUserProfile

	aUserProfile removeGroup: aString.
!

removePrivilege: aString fromUser: aUserProfile

	aUserProfile deletePrivilege: aString.
!

userList

	| list me stream |
	list := (AllUsers asSortedCollection: [:a :b | a userId <= b userId]) asOrderedCollection.
	me := System myUserProfile.
	list
		remove: me;
		addFirst: me;
		yourself.
	stream := WriteStream on: String new.
	list do: [:each | 
		self
			addUser: each 
			toStream: stream.
	].
	^stream contents.
! !
!JadeServer categoriesFor: #addGroup:toUser:!public!UserProfile! !
!JadeServer categoriesFor: #addPrivilege:toUser:!public!UserProfile! !
!JadeServer categoriesFor: #addUser:toStream:!public!UserProfile! !
!JadeServer categoriesFor: #allGroups!public!UserProfile! !
!JadeServer categoriesFor: #allUsersPasswordLimits!public!UserProfile! !
!JadeServer categoriesFor: #categoryListFor:!public!SymbolDictionary! !
!JadeServer categoriesFor: #classListFor:category:!Classes!public! !
!JadeServer categoriesFor: #dictionaryListFor:!public! !
!JadeServer categoriesFor: #globalsFor:!public! !
!JadeServer categoriesFor: #groupListFor:!public!UserProfile! !
!JadeServer categoriesFor: #newUser:!public!UserProfile! !
!JadeServer categoriesFor: #privilegeListFor:!public!UserProfile! !
!JadeServer categoriesFor: #removeGroup:fromUser:!public!UserProfile! !
!JadeServer categoriesFor: #removePrivilege:fromUser:!public!UserProfile! !
!JadeServer categoriesFor: #userList!public!UserProfile! !

!JadeServer32bit methodsFor!

addUser: aUserProfile toStream: aStream

	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | aStream lf. ^self].
	super
		addUser: aUserProfile 
		toStream: aStream.
! !
!JadeServer32bit categoriesFor: #addUser:toStream:!public!UserProfile! !

!JadeServer64bit methodsFor!

addUser: aUserProfile toStream: aStream

	[
		super
			addUser: aUserProfile 
			toStream: aStream.
	] on: Error do: [:ex | 
		aStream lf.
		ex return.
	].
! !
!JadeServer64bit categoriesFor: #addUser:toStream:!public!UserProfile! !

!JadeTextDocument methodsFor!

jadeBrowseUsers

	gciSession hasServer ifTrue: [
		^AllUsersShell showOn: gciSession.
	].
	MessageBox
		warning: 'Server initialization failed at login.'
		caption: 'Unable to Open Browser'.
! !
!JadeTextDocument categoriesFor: #jadeBrowseUsers!Jade!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

GsSymbolDictionary guid: (GUID fromString: '{EAE064DD-2265-4B2F-ACA0-766DFBFA394C}')!
GsSymbolDictionary comment: ''!
!GsSymbolDictionary categoriesForClass!Unclassified! !
!GsSymbolDictionary methodsFor!

categoryList

	| string list |
	string := gciSession 
		serverPerform: #'categoryListFor:' 
		with: self.
	list := GsString
		listFromString: string 
		session: gciSession.
	^list.
!

classesInCategory: aGsString 

	| string |
	string := gciSession 
		serverPerform: #classListFor:category:
		with: self
		with: aGsString.
	^GsClass 
		listFromString: string 
		session: gciSession.
!

globals

	| string |
	string := gciSession serverPerform: #globalsFor: with: self.
	^GsGlobal 
		listFromString: string 
		session: gciSession.
! !
!GsSymbolDictionary categoriesFor: #categoryList!public! !
!GsSymbolDictionary categoriesFor: #classesInCategory:!public! !
!GsSymbolDictionary categoriesFor: #globals!public! !

GsUserProfile guid: (GUID fromString: '{399E972A-24DD-4E6C-A458-FC8E949470A5}')!
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

AllUsersPresenter guid: (GUID fromString: '{8E70AEE2-43A1-498D-8E5A-1F5C9D72EBC1}')!
AllUsersPresenter comment: ''!
!AllUsersPresenter categoriesForClass!Unclassified! !
!AllUsersPresenter methodsFor!

createComponents

	super createComponents.
	userListPresenter 				:= self add: UserListPresenter 			new name: 'userList'.
	userProfilePresenter 			:= self add: UserProfilePresenter 		new name: 'userProfile'.
	passwordLimitsPresenter 	:= self add: UserProfileSetPresenter 	new name: 'passwordLimits'.
!

createSchematicWiring

	super createSchematicWiring.
	userListPresenter when: #'selectionChanged' send: #'update' to: userProfilePresenter.
	userProfilePresenter when: #'needSelectedUser' send: #'selection' to: userListPresenter.
!

model: aGciSession

	super model: aGciSession.
	userListPresenter 				model: aGciSession.
	userProfilePresenter 			model: aGciSession.
	passwordLimitsPresenter	model: aGciSession.
!

onViewOpened

	super onViewOpened.
	userListPresenter selectFirstUser.
!

updateMenuBar: aMenuBar! !
!AllUsersPresenter categoriesFor: #createComponents!private! !
!AllUsersPresenter categoriesFor: #createSchematicWiring!private! !
!AllUsersPresenter categoriesFor: #model:!private! !
!AllUsersPresenter categoriesFor: #onViewOpened!private! !
!AllUsersPresenter categoriesFor: #updateMenuBar:!private! !

!AllUsersPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 544 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  2559 21 706 1201 801 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 87 7 0 0 154 1 0 0] 98 1 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1140850688 131073 816 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 816 655878 ##(Smalltalk.CardLayout)  202 208 98 2 721414 ##(Smalltalk.Association)  8 'All Users' 410 432 98 15 0 816 98 2 8 1140850688 131073 1056 0 0 0 5 0 0 0 1056 498 234 240 98 4 410 8 ##(Smalltalk.ReferenceView)  98 14 0 1056 98 2 8 1140850688 131073 1168 0 898 8 4278190080 0 5 0 0 0 1168 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.UserListPresenter)  8 #resource_Default_view 0 578 202 208 98 1 642 672 98 2 706 1 1 706 1185 293 1168 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 80 2 0 0 146 0 0 0] 544 706 193 193 0 27 5 410 1184 98 14 0 1056 98 2 8 1140850688 131073 1504 0 898 1264 0 5 0 0 0 1504 1282 8 ##(Smalltalk.UserProfilePresenter)  1328 0 578 202 208 98 1 642 672 98 2 706 1 303 706 1185 443 1504 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 151 0 0 0 80 2 0 0 116 1 0 0] 544 1488 0 27 7 16 234 256 98 4 1168 8 'userList' 1504 8 'userProfile' 0 578 202 208 98 1 642 672 98 2 706 9 49 706 1185 745 1056 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 84 2 0 0 140 1 0 0] 98 3 1168 410 8 ##(Smalltalk.Splitter)  98 12 0 1056 98 2 8 1140850688 1 1984 0 898 8 4278190080 0 517 0 0 0 1984 578 202 208 98 1 642 672 98 2 706 1 293 706 1185 11 1984 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 146 0 0 0 80 2 0 0 151 0 0 0] 98 0 1488 0 27 1504 1488 0 27 1010 8 'Password Limits' 410 1184 98 14 0 816 98 2 8 1140850688 131073 2288 0 898 8 4278190080 0 5 0 0 0 2288 1282 8 ##(Smalltalk.UserProfileSetPresenter)  1328 0 578 202 208 98 1 642 672 98 2 706 9 49 706 1185 745 2288 754 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 84 2 0 0 140 1 0 0] 544 1488 0 27 1056 234 256 98 2 2288 8 'passwordLimits' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 816 98 2 8 1140916736 1 2608 590662 2 ##(Smalltalk.ListModel)  202 208 98 2 1040 2272 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 0 0 1 0 0 0 2608 0 8 4294904209 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 8 #displayString 2848 7 257 0 2834 0 0 2866 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 2960 7 257 0 2762 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 8 #noIcons 0 0 0 0 0 578 202 208 98 3 642 672 98 2 706 1 1 706 1201 801 2608 642 8 #selectionByIndex:ifAbsent: 98 2 3 642 8 #yourself 544 0 2608 642 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 2608 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 144 1 0 0] 98 0 1488 0 27 578 202 208 98 1 642 672 98 2 706 1 1 706 1201 801 816 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 144 1 0 0] 98 3 1056 2288 2608 1488 0 27 1488 0 27 )! !
!AllUsersPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

UserListPresenter guid: (GUID fromString: '{4556FB52-1146-43B0-94FD-10FE7D352252}')!
UserListPresenter comment: ''!
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

	userListPresenter list: (GsUserProfile allIn: self model).
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
	string := self model
		serverPerform: #'newUser:'
		with: newUserID.
	list := GsUserProfile
		listFromString: string 
		session: self model.
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

UserProfileDetailsPresenter guid: (GUID fromString: '{F6811F82-8AFC-4569-A453-15B155F97432}')!
UserProfileDetailsPresenter comment: ''!
!UserProfileDetailsPresenter categoriesForClass!Unclassified! !
!UserProfileDetailsPresenter methodsFor!

createComponents

	super createComponents.
	userIDPresenter 				:= self add: TextPresenter new name: 'userID'.
	maxLoginsPresenter 			:= self add: TextPresenter new name: 'maxLogins'.
	languagePresenter 			:= self add: TextPresenter new name: 'language'.
	lastLoginPresenter 			:= self add: TextPresenter new name: 'lastLogin'.
	disabledReasonPresenter 	:= self add: TextPresenter new name: 'disabledReason'.
!

onViewOpened

	super onViewOpened.
"	userIDPresenter 				value: model userID.
	maxLoginsPresenter 			value: model maxSessions.
	languagePresenter 			value: model language.
	lastLoginPresenter 			value: model lastLoginTime.
	disabledReasonPresenter 	value: model disabledReason.
"!

updateForUser: aGsUser

	userIDPresenter 				value: nil.
	maxLoginsPresenter 			value: nil.
	languagePresenter 			value: nil.
	lastLoginPresenter 			value: nil.
	disabledReasonPresenter 	value: nil.
	aGsUser isNil ifTrue: [^self].
	userIDPresenter 				value: aGsUser name.
	maxLoginsPresenter 			value: aGsUser maxSessions.
	languagePresenter 			value: aGsUser language.
	lastLoginPresenter 			value: aGsUser lastLoginTime.
	disabledReasonPresenter 	value: aGsUser disabledReason.
! !
!UserProfileDetailsPresenter categoriesFor: #createComponents!public! !
!UserProfileDetailsPresenter categoriesFor: #onViewOpened!public! !
!UserProfileDetailsPresenter categoriesFor: #updateForUser:!public! !

!UserProfileDetailsPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 16 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140924546 1025 560 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 560 0 8 4294903717 852486 ##(Smalltalk.NullConverter)  0 0 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  281 41 866 61 41 560 802 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 560 802 8 #isTextModified: 98 1 32 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 0 0 0 20 0 0 0 170 0 0 0 40 0 0 0] 98 0 866 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 281 1154 8 #fixedViewLeft 61 1154 8 #fixedParentTop 41 1154 8 #fixedViewTop 41 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 1296 0 0 0 5 0 0 0 1296 0 8 4294903713 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 121 866 281 41 1296 802 8 #text: 98 1 8 'Disabled Reason:' 1296 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 60 0 0 0 140 0 0 0 80 0 0 0] 98 0 1104 0 27 1122 1168 1 1200 281 1232 121 1264 41 410 576 98 16 0 416 98 2 8 1140916352 1025 1648 0 642 8 4278190080 0 5 0 0 0 1648 0 8 4294903717 706 0 0 1 738 202 208 98 3 802 832 98 2 866 281 1 866 321 41 1648 802 928 98 1 962 3 1 3 1648 802 1008 98 1 32 1648 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 0 0 0 0 0 0 0 44 1 0 0 20 0 0 0] 98 0 1104 0 27 1122 1168 281 1200 321 1232 1 1264 41 410 576 98 16 0 416 98 2 8 1140916352 1025 2032 0 642 672 0 5 0 0 0 2032 0 8 4294903717 706 0 0 1 738 202 208 98 3 802 832 98 2 866 281 121 866 821 41 2032 802 928 98 1 962 3 1 3 2032 802 1008 98 1 32 2032 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 0 0 0 60 0 0 0 38 2 0 0 80 0 0 0] 98 0 1104 0 27 1122 1168 281 1154 8 #fixedParentRight 1 1232 121 1264 41 410 576 98 16 0 416 98 2 8 1140916352 1025 2432 0 642 672 0 5 0 0 0 2432 0 8 4294903717 706 0 0 3 738 202 208 98 3 802 832 98 2 866 281 81 866 321 41 2432 802 928 98 1 962 3 1 3 2432 802 1008 98 1 32 2432 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 0 0 0 40 0 0 0 44 1 0 0 60 0 0 0] 98 0 1104 0 27 1122 1168 281 1200 321 1232 81 1264 41 410 1312 98 16 0 416 98 2 8 1140850944 1 2800 0 0 0 5 0 0 0 2800 0 8 4294903713 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 81 866 281 41 2800 802 1536 98 1 8 'Most Recent Login:' 2800 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 40 0 0 0 140 0 0 0 60 0 0 0] 98 0 1104 0 27 1122 1168 1 1200 281 1232 81 1264 41 410 1312 98 16 0 416 98 2 8 1140850944 1 3120 0 0 0 5 0 0 0 3120 0 8 4294903713 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 1 866 281 41 3120 802 1536 98 1 8 'User ID:' 3120 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 140 0 0 0 20 0 0 0] 98 0 1104 0 27 1122 1168 1 1200 281 1232 1 1264 41 410 1312 98 16 0 416 98 2 8 1140850944 1 3440 0 0 0 5 0 0 0 3440 0 8 4294903713 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 41 866 281 41 3440 802 1536 98 1 8 ' Max Concurrent Logins:' 3440 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 140 0 0 0 40 0 0 0] 98 0 1104 0 27 1122 1168 1 1200 281 1232 41 1264 41 234 256 98 8 560 8 'maxLogins' 2432 8 'lastLogin' 2032 8 'disabledReason' 1648 8 'userID' 0 738 202 208 98 1 802 832 98 2 866 5119 21 866 1101 501 416 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 37 12 0 0 4 1 0 0] 98 8 3120 1648 3440 560 2800 2432 1296 2032 1104 0 27 )! !
!UserProfileDetailsPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

UserProfilePasswordPresenter guid: (GUID fromString: '{5BBE25B8-5AFD-4858-AC78-843769085AA8}')!
UserProfilePasswordPresenter comment: ''!
!UserProfilePasswordPresenter categoriesForClass!Unclassified! !
!UserProfilePasswordPresenter methodsFor!

createComponents

	super createComponents.
	whenPasswordChangedPresenter 	:= self add: TextPresenter 		new name: 'whenPasswordChanged'.
	loginsRemainingPresenter 				:= self add: NumberPresenter 	new name: 'loginsRemaining'.
	isSpecialUserPresenter 					:= self add: BooleanPresenter 	new name: 'isSpecialUser'.
!

updateForUser: aGsUserProfile 

	whenPasswordChangedPresenter 	value: nil.
	loginsRemainingPresenter 				value: nil.
	isSpecialUserPresenter 					value: nil.
	aGsUserProfile isNil ifTrue: [^self].
	whenPasswordChangedPresenter 	value: aGsUserProfile lastPasswordChange.
	loginsRemainingPresenter 				value: aGsUserProfile remainingLogins.
	isSpecialUserPresenter 					value: aGsUserProfile isSpecial.
! !
!UserProfilePasswordPresenter categoriesFor: #createComponents!public! !
!UserProfilePasswordPresenter categoriesFor: #updateForUser:!public! !

!UserProfilePasswordPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 28 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140924546 1025 560 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 560 0 8 4294904165 852486 ##(Smalltalk.NullConverter)  0 0 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  511 41 866 61 41 560 802 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 560 802 8 #isTextModified: 98 1 32 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 20 0 0 0 29 1 0 0 40 0 0 0] 98 0 866 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 511 1162 1184 8 #fixedViewLeft 61 1162 1184 8 #fixedParentTop 41 1162 1184 8 #fixedViewTop 41 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 1312 0 0 0 5 0 0 0 1312 0 8 4294904479 706 0 0 0 738 202 208 98 2 802 832 98 2 866 41 171 866 111 41 1312 802 8 #text: 98 1 8 'New:' 1312 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 85 0 0 0 75 0 0 0 105 0 0 0] 98 0 1104 0 27 1122 1168 41 1216 111 1248 171 1280 41 410 576 98 16 0 416 98 2 8 1140916384 1025 1664 0 642 8 4278190080 0 5 0 0 0 1664 0 8 4294904165 706 0 0 1 738 202 208 98 3 802 832 98 2 866 151 171 866 181 41 1664 802 928 98 1 962 3 1 3 1664 802 1008 98 1 32 1664 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 85 0 0 0 165 0 0 0 105 0 0 0] 98 0 1104 0 27 1122 1168 151 1216 181 1248 171 1280 41 410 576 98 16 0 416 98 2 8 1140916384 1025 2048 0 642 1744 0 5 0 0 0 2048 0 8 4294904165 706 0 0 1 738 202 208 98 3 802 832 98 2 866 151 131 866 181 41 2048 802 928 98 1 962 3 1 3 2048 802 1008 98 1 32 2048 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 65 0 0 0 165 0 0 0 85 0 0 0] 98 0 1104 0 27 1122 1168 151 1216 181 1248 131 1280 41 410 1328 98 16 0 416 98 2 8 1140850944 1 2416 0 0 0 5 0 0 0 2416 0 8 4294904479 706 0 0 0 738 202 208 98 2 802 832 98 2 866 41 211 866 111 41 2416 802 1552 98 1 8 'Confirm:' 2416 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 105 0 0 0 75 0 0 0 125 0 0 0] 98 0 1104 0 27 1122 1168 41 1216 111 1248 211 1280 41 410 8 ##(Smalltalk.PushButton)  98 17 0 416 98 2 8 1140924416 1 2736 0 0 0 5 0 0 0 2736 0 8 4294904283 1180998 4 ##(Smalltalk.CommandDescription)  0 8 'Set New Password' 1 1 0 0 32 738 202 208 98 2 802 832 98 2 866 41 251 866 291 51 2736 802 1552 98 1 8 'Set New Password' 2736 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 125 0 0 0 165 0 0 0 150 0 0 0] 98 0 1104 0 27 1122 1168 41 1216 291 1248 251 1280 51 410 1328 98 16 0 416 98 2 8 1140850944 1 3104 0 0 0 5 0 0 0 3104 0 8 4294904479 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 1 866 511 41 3104 802 1552 98 1 8 'Most Recent Password Change:	' 3104 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 255 0 0 0 20 0 0 0] 98 0 1104 0 27 1122 1168 1 1216 511 1248 1 1280 41 410 1328 98 16 0 416 98 2 8 1140850944 1 3424 0 0 0 5 0 0 0 3424 0 8 4294904479 706 0 0 0 738 202 208 98 2 802 832 98 2 866 41 131 866 111 41 3424 802 1552 98 1 8 'Old:' 3424 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 65 0 0 0 75 0 0 0 85 0 0 0] 98 0 1104 0 27 1122 1168 41 1216 111 1248 131 1280 41 410 8 ##(Smalltalk.CheckBox)  98 16 0 416 98 2 8 1409363203 1 3744 721990 2 ##(Smalltalk.ValueHolder)  0 0 1162 8 ##(Smalltalk.SearchPolicy)  8 #never 32 0 0 5 0 0 0 3744 0 8 4294904283 706 0 0 0 738 202 208 98 2 802 832 98 2 866 581 41 866 521 41 3744 802 1552 98 1 8 'Is Special User (Password Never Expires)' 3744 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 34 1 0 0 20 0 0 0 38 2 0 0 40 0 0 0] 98 0 1104 0 27 1122 1168 581 1162 1184 8 #fixedParentRight 1 1248 41 1280 41 410 1328 98 16 0 416 98 2 8 1140850944 1 4192 0 0 0 5 0 0 0 4192 0 8 4294904479 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 41 866 511 41 4192 802 1552 98 1 8 'Logins Remaining Before Password Expires:' 4192 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 40 0 0 0] 98 0 1104 0 27 1122 1168 1 1216 511 1248 41 1280 41 410 576 98 16 0 416 98 2 8 1140916384 1025 4512 0 642 1744 0 5 0 0 0 4512 0 8 4294904165 706 0 0 1 738 202 208 98 3 802 832 98 2 866 151 211 866 181 41 4512 802 928 98 1 962 3 1 3 4512 802 1008 98 1 32 4512 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 75 0 0 0 105 0 0 0 165 0 0 0 125 0 0 0] 98 0 1104 0 27 1122 1168 151 1216 181 1248 211 1280 41 410 8 ##(Smalltalk.GroupBox)  98 14 0 416 98 2 8 1140850695 65 4880 0 642 8 4294967295 0 5 0 0 0 4880 0 8 4294904283 738 202 208 98 2 802 832 98 2 866 11 91 866 341 231 4880 802 1552 98 1 8 'Change Password' 4880 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 45 0 0 0 175 0 0 0 160 0 0 0] 98 0 1104 0 27 1122 1168 11 1216 341 1248 91 1280 231 410 2752 98 17 0 416 98 2 8 1140924416 1 5232 0 0 0 5 0 0 0 5232 0 8 4294904283 2834 8 #clearOldPasswords 8 'Clear Old Passwords (to allow reuse)' 1 1 0 0 32 738 202 208 98 2 802 832 98 2 866 511 81 866 591 51 5232 802 1552 98 1 8 'Clear Old Passwords (to allow reuse)' 5232 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 40 0 0 0 38 2 0 0 65 0 0 0] 98 0 1104 0 27 1122 1168 511 4160 1 1248 81 1280 51 410 576 98 16 0 416 98 2 8 1140916352 1025 5584 0 642 672 0 5 0 0 0 5584 0 8 4294904165 706 0 0 1 738 202 208 98 3 802 832 98 2 866 511 1 866 591 41 5584 802 928 98 1 962 3 1 3 5584 802 1008 98 1 32 5584 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 38 2 0 0 20 0 0 0] 98 0 1104 0 27 1122 1168 511 4160 1 1248 1 1280 41 234 256 98 12 2048 8 'oldPassword' 560 8 'loginsRemaining' 1664 8 'newPassword' 4512 8 'confirmPassword' 3744 8 'isSpecialUser' 5584 8 'whenPasswordChanged' 0 738 202 208 98 1 802 832 98 2 866 2559 21 866 1101 501 416 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 37 7 0 0 4 1 0 0] 98 14 3104 5584 4192 560 3744 5232 3424 2048 1312 1664 2416 4512 2736 4880 1104 0 27 )! !
!UserProfilePasswordPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

UserProfilePresenter guid: (GUID fromString: '{23AF6D76-E0D4-4D7E-8C81-46A93349090B}')!
UserProfilePresenter comment: ''!
!UserProfilePresenter categoriesForClass!Unclassified! !
!UserProfilePresenter methodsFor!

createComponents

	super createComponents.
	detailsPresenter 			:= self add: UserProfileDetailsPresenter 		new 	name: 'detailsTab'.
	passwordPresenter 		:= self add: UserProfilePasswordPresenter 	new 	name: 'passwordTab'.
	symbolListPresenter 		:= self add: ListPresenter 								new 	name: 'symbolList'.
	groupListPresenter 		:= self add: ListPresenter 								new 	name: 'groupList'.
	privilegeListPresenter 	:= self add: ListPresenter 								new 	name: 'privilegeList'.
	!

model: aGciSession

	super model: aGciSession.
	detailsPresenter model: aGciSession.
	passwordPresenter model: aGciSession.
!

update

	user := self trigger: #'needSelectedUser'.
	detailsPresenter 			updateForUser: user.
	passwordPresenter 		updateForUser: user.
	symbolListPresenter		list: #().
	groupListPresenter		list: #().
	privilegeListPresenter	list: #().
	user isNil ifTrue: [^self].
	symbolListPresenter		list: user symbolList.
	groupListPresenter 		list: user groupList.
	privilegeListPresenter	list: user privilegeList.
! !
!UserProfilePresenter categoriesFor: #createComponents!public! !
!UserProfilePresenter categoriesFor: #model:!public! !
!UserProfilePresenter categoriesFor: #update!public! !

!UserProfilePresenter class methodsFor!

publishedEventsOfInstances
    
    	^super publishedEventsOfInstances
			add: #'needSelectedUser';
			yourself.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 544 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  5119 21 706 1161 501 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 67 12 0 0 4 1 0 0] 98 1 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1140850688 131073 816 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 816 655878 ##(Smalltalk.CardLayout)  202 208 98 5 721414 ##(Smalltalk.Association)  8 'Symbol List' 410 8 ##(Smalltalk.ListView)  98 30 0 816 98 2 8 1140920397 1025 1056 590662 2 ##(Smalltalk.ListModel)  202 208 544 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  898 8 4278190080 0 5 0 0 0 1056 0 8 4294903731 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 'Dictionaries' 1137 8 #left 1266 1296 1312 8 ##(Smalltalk.SortedCollection)  0 0 1056 0 3 0 0 8 #report 544 0 131169 0 0 578 202 208 98 2 642 672 98 2 706 9 49 706 1145 445 1056 642 8 #text: 98 1 8 'Dictionaries' 1056 754 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 64 2 0 0 246 0 0 0] 98 0 706 193 193 0 27 1010 8 'Groups' 410 8 ##(Smalltalk.EditableListView)  98 38 0 816 98 2 8 1140969549 1025 1776 1138 202 208 544 0 1200 898 8 4278190080 0 5 0 0 0 1776 0 8 4294903731 8 ##(Smalltalk.BasicListAbstract)  0 1344 0 0 0 706 65 65 0 0 202 208 98 2 1447494 14 ##(Smalltalk.EditableListViewColumn)  8 '' 41 1440 1266 1296 98 0 1266 8 #<= 2064 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 2112 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 2128 7 257 0 0 1776 0 1 0 0 16 2114 0 0 2146 2 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:array :value | (array at: 1) editGroup: array value: value]' 8 #[30 105 17 63 148 233 1 190 106] 8 #editGroup:value: 2224 7 513 0 1052742 13 ##(Smalltalk.EmbeddedCheckBox)  0 0 98 2 134348801 1 2352 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  32 898 8 4278190080 0 5 0 0 0 2352 0 0 0 0 0 0 0 0 0 0 0 2002 8 'Groups' 1097 1440 1936 1472 2114 0 0 2146 1 83886081 2112 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 2512 7 257 0 0 1776 0 3 0 0 32 0 1052998 13 ##(Smalltalk.EmbeddedTextEdit)  0 0 98 2 134349057 1 2608 2386 0 32 2432 0 898 2464 0 5 0 0 0 2608 0 0 852486 ##(Smalltalk.NullConverter)  0 2032 1 0 0 0 0 0 0 0 0 1488 544 0 131137 0 0 0 202 208 544 0 0 0 3 0 0 578 202 208 98 1 642 672 98 2 706 9 49 706 1145 445 1776 754 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 64 2 0 0 246 0 0 0] 98 0 1728 0 29 1010 8 'Details' 410 8 ##(Smalltalk.ReferenceView)  98 14 0 816 98 2 8 1140850688 131073 2912 0 898 8 4278190080 0 5 0 0 0 2912 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.UserProfileDetailsPresenter)  8 #resource_Default_view 0 578 202 208 98 1 642 672 98 2 706 9 49 706 1145 445 2912 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 64 2 0 0 246 0 0 0] 544 1728 0 27 1010 8 'Password' 410 2928 98 14 0 816 98 2 8 1140850688 131073 3264 0 898 3008 0 5 0 0 0 3264 3026 8 ##(Smalltalk.UserProfilePasswordPresenter)  3072 0 578 202 208 98 1 642 672 98 2 706 9 49 706 1145 445 3264 754 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 64 2 0 0 246 0 0 0] 544 1728 0 27 1010 8 'Privileges' 410 1792 98 38 0 816 98 2 8 1140969549 1025 3552 1138 202 208 544 0 1200 898 1904 0 5 0 0 0 3552 0 8 4294903731 1936 0 1344 0 0 0 706 65 65 0 0 202 208 98 2 2002 2032 41 1440 1266 1296 98 0 1266 2096 3760 2114 0 0 2146 1 83886081 2112 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 3792 7 257 0 0 3552 0 1 0 0 16 2114 0 0 2146 2 1 2112 8 'doIt' 8 '[:array :value | (array at: 1) editPrivilege: array value: value]' 8 #[30 105 17 63 148 233 1 190 106] 8 #editPrivilege:value: 3872 7 513 0 2338 0 0 98 2 134348801 1 3968 2386 0 32 2432 32 898 2464 0 5 0 0 0 3968 0 0 0 0 0 0 0 0 0 0 0 2002 2496 1097 1440 1936 1472 2114 0 0 2146 1 83886081 2112 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 4048 7 257 0 0 3552 0 3 0 0 32 0 2594 0 0 98 2 134349057 1 4128 2386 0 32 2432 0 898 2464 0 5 0 0 0 4128 0 0 2674 0 2032 1 0 0 0 0 0 0 0 0 1488 544 0 131137 0 0 0 202 208 544 0 0 0 3 0 0 578 202 208 98 1 642 672 98 2 706 9 49 706 1145 445 3552 754 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 64 2 0 0 246 0 0 0] 98 0 1728 0 29 2912 234 256 98 10 3552 8 'privilegeList' 1056 8 'symbolList' 2912 8 'detailsTab' 1776 8 'groupList' 3264 8 'passwordTab' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 816 98 2 8 1140916736 1 4496 1138 202 208 98 5 2896 3248 1040 1760 3536 0 1200 0 0 1 0 0 0 4496 0 8 4294903041 2114 0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 1296 4640 7 257 0 2114 0 0 4658 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 4736 7 257 0 1344 0 0 0 0 0 8 #noIcons 0 0 0 0 0 578 202 208 98 3 642 672 98 2 706 1 1 706 1161 501 4496 642 8 #basicSelectionsByIndex: 98 1 98 1 3 4496 642 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 4496 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 68 2 0 0 250 0 0 0] 98 0 1728 0 27 578 202 208 98 1 642 672 98 2 706 1 1 706 1161 501 816 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 68 2 0 0 250 0 0 0] 98 6 2912 3264 1056 1776 3552 4496 1728 0 27 1728 0 27 )! !
!UserProfilePresenter class categoriesFor: #publishedEventsOfInstances!public! !
!UserProfilePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

UserProfileSetPresenter guid: (GUID fromString: '{2E2A392C-D8FE-4A26-84C1-770DA035658E}')!
UserProfileSetPresenter comment: ''!
!UserProfileSetPresenter categoriesForClass!Unclassified! !
!UserProfileSetPresenter methodsFor!

createComponents

	super createComponents.
	disallowUsedPasswordsPresenter 	:= self add: BooleanPresenter 	new name: 'disallowUsedPasswords'.
	minPasswordSizePresenter 			:= self add: NumberPresenter 	new name: 'minPasswordSize'.
	maxPasswordSizePresenter 			:= self add: NumberPresenter 	new name: 'maxPasswordSize'.
	maxRepeatingCharsPresenter 		:= self add: NumberPresenter 	new name: 'maxRepeatingChars'.
	maxConsecutiveCharsPresenter 	:= self add: NumberPresenter 	new name: 'maxConsecutiveChars'.
	maxCharsOfSameTypePresenter 	:= self add: NumberPresenter 	new name: 'maxCharsOfSameType'.
	staleAccountAgeLimitPresenter 	:= self add: NumberPresenter 	new name: 'staleAccountAgeLimit'.
	passwordAgeLimitPresenter 			:= self add: NumberPresenter 	new name: 'passwordAgeLimit'.
	passwordAgeWarningPresenter 	:= self add: TextPresenter 		new name: 'passwordAgeWarning'.
	disallowedPasswordListPresenter 	:= self add: ListPresenter 			new name: 'disallowedPasswordList'.
	!

fillFields

	| string lines numbers |
	string := self model serverPerform: #'allUsersPasswordLimits'.
	lines := string subStrings: Character lf.
	numbers := lines first subStrings: Character tab.
	disallowUsedPasswordsPresenter 	value: numbers first = 'true'.
	numbers := numbers collect: [:each | 
		(each allSatisfy: [:char | char isDigit])
			ifTrue: [each asNumber]
			ifFalse: [nil].
	].
	minPasswordSizePresenter 			value: (numbers at: 2).
	maxPasswordSizePresenter 			value: (numbers at: 3).
	maxRepeatingCharsPresenter 		value: (numbers at: 4).
	maxConsecutiveCharsPresenter 	value: (numbers at: 5).
	maxCharsOfSameTypePresenter 	value: (numbers at: 6).
	staleAccountAgeLimitPresenter 	value: (numbers at: 7).
	passwordAgeLimitPresenter 			value: (numbers at: 8).
	disallowedPasswordListPresenter 	list: ((lines at: 2) subStrings: Character tab).
	(string := lines at: 3) = 'nil' ifTrue: [string := ''].
	passwordAgeWarningPresenter 	value: string.
!

onViewOpened

	super onViewOpened.
	self fillFields.
! !
!UserProfileSetPresenter categoriesFor: #createComponents!public! !
!UserProfileSetPresenter categoriesFor: #fillFields!public! !
!UserProfileSetPresenter categoriesFor: #onViewOpened!public! !

!UserProfileSetPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 38 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140924546 1025 560 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 560 0 8 4294904211 852486 ##(Smalltalk.NullConverter)  0 0 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  301 281 866 81 41 560 802 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 560 802 8 #isTextModified: 98 1 32 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 140 0 0 0 190 0 0 0 160 0 0 0] 98 0 866 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 301 1162 1184 8 #fixedViewLeft 81 1162 1184 8 #fixedParentTop 281 1162 1184 8 #fixedViewTop 41 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920399 1025 1312 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1162 8 ##(Smalltalk.SearchPolicy)  8 #identity 642 8 4278190080 0 5 0 0 0 1312 0 8 4294903997 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 1162 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 'Disallowed Passwords' 413 8 #left 1554 1584 1600 8 ##(Smalltalk.SortedCollection)  0 0 1312 0 3 0 0 8 #list 1440 0 131169 0 0 738 202 208 98 2 802 832 98 2 866 401 41 866 401 281 1312 802 8 #text: 98 1 8 'Disallowed Passwords' 1312 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 200 0 0 0 20 0 0 0 144 1 0 0 160 0 0 0] 98 0 1104 0 27 1122 1168 401 1162 1184 8 #fixedParentRight 1 1248 41 1280 281 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2080 0 0 0 5 0 0 0 2080 0 8 4294904055 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 281 866 301 41 2080 802 1936 98 1 8 'Password Age Limit:' 2080 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 140 0 0 0 150 0 0 0 160 0 0 0] 98 0 1104 0 27 1122 1168 1 1216 301 1248 281 1280 41 410 2096 98 16 0 416 98 2 8 1140850944 1 2416 0 0 0 5 0 0 0 2416 0 8 4294904055 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 241 866 301 41 2416 802 1936 98 1 8 'Stale Account Age Limit:' 2416 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 120 0 0 0 150 0 0 0 140 0 0 0] 98 0 1104 0 27 1122 1168 1 1216 301 1248 241 1280 41 410 2096 98 16 0 416 98 2 8 1140850944 1 2736 0 0 0 5 0 0 0 2736 0 8 4294904055 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 321 866 301 41 2736 802 1936 98 1 8 'Password Age Warning:' 2736 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 160 0 0 0 150 0 0 0 180 0 0 0] 98 0 1104 0 27 1122 1168 1 1216 301 1248 321 1280 41 410 576 98 16 0 416 98 2 8 1140924546 1025 3056 0 642 672 0 5 0 0 0 3056 0 8 4294904211 706 0 0 1 738 202 208 98 3 802 832 98 2 866 301 241 866 81 41 3056 802 928 98 1 962 3 1 3 3056 802 1008 98 1 32 3056 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 120 0 0 0 190 0 0 0 140 0 0 0] 98 0 1104 0 27 1122 1168 301 1216 81 1248 241 1280 41 410 576 98 16 0 416 98 2 8 1140924546 1025 3424 0 642 672 0 5 0 0 0 3424 0 8 4294904211 706 0 0 1 738 202 208 98 3 802 832 98 2 866 301 41 866 81 41 3424 802 928 98 1 962 3 1 3 3424 802 1008 98 1 32 3424 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 20 0 0 0 190 0 0 0 40 0 0 0] 98 0 1104 0 27 1122 1168 301 1216 81 1248 41 1280 41 410 2096 98 16 0 416 98 2 8 1140850944 1 3792 0 0 0 5 0 0 0 3792 0 8 4294904055 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 201 866 301 41 3792 802 1936 98 1 8 'Max Chars of Same Type:' 3792 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 100 0 0 0 150 0 0 0 120 0 0 0] 98 0 1104 0 27 1122 1168 1 1216 301 1248 201 1280 41 410 576 98 16 0 416 98 2 8 1140916352 1025 4112 0 642 672 0 5 0 0 0 4112 0 8 4294904211 706 0 0 1 738 202 208 98 3 802 832 98 2 866 301 321 866 501 41 4112 802 928 98 1 962 3 1 3 4112 802 1008 98 1 32 4112 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 160 0 0 0 144 1 0 0 180 0 0 0] 98 0 1104 0 27 1122 1168 301 2048 1 1248 321 1280 41 410 2096 98 16 0 416 98 2 8 1140850944 1 4480 0 0 0 5 0 0 0 4480 0 8 4294904055 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 161 866 301 41 4480 802 1936 98 1 8 'Max Consecutive Chars:' 4480 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 80 0 0 0 150 0 0 0 100 0 0 0] 98 0 1104 0 27 1122 1168 1 1216 301 1248 161 1280 41 410 576 98 16 0 416 98 2 8 1140924546 1025 4800 0 642 672 0 5 0 0 0 4800 0 8 4294904211 706 0 0 1 738 202 208 98 3 802 832 98 2 866 301 81 866 81 41 4800 802 928 98 1 962 3 1 3 4800 802 1008 98 1 32 4800 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 40 0 0 0 190 0 0 0 60 0 0 0] 98 0 1104 0 27 1122 1168 301 1216 81 1248 81 1280 41 410 2096 98 16 0 416 98 2 8 1140850944 1 5168 0 0 0 5 0 0 0 5168 0 8 4294904055 706 0 0 0 738 202 208 98 2 802 832 98 2 866 401 1 866 401 41 5168 802 1936 98 1 8 'Disallowed Passwords:' 5168 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 200 0 0 0 0 0 0 0 144 1 0 0 20 0 0 0] 98 0 1104 0 27 1122 1168 401 2048 1 1248 1 1280 41 410 8 ##(Smalltalk.CheckBox)  98 16 0 416 98 2 8 1409363203 1 5488 721990 2 ##(Smalltalk.ValueHolder)  0 0 1162 1472 8 #never 32 0 0 5 0 0 0 5488 0 8 4294904197 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 1 866 381 41 5488 802 1936 98 1 8 'Disallow Used Passwords' 5488 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 190 0 0 0 20 0 0 0] 98 0 1104 0 27 1122 1168 1 1216 381 1248 1 1280 41 410 576 98 16 0 416 98 2 8 1140924546 1025 5888 0 642 672 0 5 0 0 0 5888 0 8 4294904211 706 0 0 1 738 202 208 98 3 802 832 98 2 866 301 121 866 81 41 5888 802 928 98 1 962 3 1 3 5888 802 1008 98 1 32 5888 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 60 0 0 0 190 0 0 0 80 0 0 0] 98 0 1104 0 27 1122 1168 301 1216 81 1248 121 1280 41 410 2096 98 16 0 416 98 2 8 1140850944 1 6256 0 0 0 5 0 0 0 6256 0 8 4294904055 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 121 866 301 41 6256 802 1936 98 1 8 'Max Repeating Chars:' 6256 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 60 0 0 0 150 0 0 0 80 0 0 0] 98 0 1104 0 27 1122 1168 1 1216 301 1248 121 1280 41 410 576 98 16 0 416 98 2 8 1140924546 1025 6576 0 642 672 0 5 0 0 0 6576 0 8 4294904211 706 0 0 1 738 202 208 98 3 802 832 98 2 866 301 161 866 81 41 6576 802 928 98 1 962 3 1 3 6576 802 1008 98 1 32 6576 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 80 0 0 0 190 0 0 0 100 0 0 0] 98 0 1104 0 27 1122 1168 301 1216 81 1248 161 1280 41 410 2096 98 16 0 416 98 2 8 1140850944 1 6944 0 0 0 5 0 0 0 6944 0 8 4294904055 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 41 866 301 41 6944 802 1936 98 1 8 'Min Password Size:' 6944 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 150 0 0 0 40 0 0 0] 98 0 1104 0 27 1122 1168 1 1216 301 1248 41 1280 41 410 2096 98 16 0 416 98 2 8 1140850944 1 7264 0 0 0 5 0 0 0 7264 0 8 4294904055 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 81 866 301 41 7264 802 1936 98 1 8 'Max Password Size:' 7264 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 40 0 0 0 150 0 0 0 60 0 0 0] 98 0 1104 0 27 1122 1168 1 1216 301 1248 81 1280 41 410 576 98 16 0 416 98 2 8 1140924546 1025 7584 0 642 672 0 5 0 0 0 7584 0 8 4294904211 706 0 0 1 738 202 208 98 3 802 832 98 2 866 301 201 866 81 41 7584 802 928 98 1 962 3 1 3 7584 802 1008 98 1 32 7584 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 150 0 0 0 100 0 0 0 190 0 0 0 120 0 0 0] 98 0 1104 0 27 1122 1168 301 1216 81 1248 201 1280 41 234 256 98 20 560 8 'passwordAgeLimit' 1312 8 'disallowedPasswordList' 3056 8 'staleAccountAgeLimit' 3424 8 'minPasswordSize' 4112 8 'passwordAgeWarning' 4800 8 'maxPasswordSize' 5488 8 'disallowUsedPasswords' 5888 8 'maxRepeatingChars' 7584 8 'maxCharsOfSameType' 6576 8 'maxConsecutiveChars' 0 738 202 208 98 1 802 832 98 2 866 2799 21 866 801 401 416 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 5 0 0 10 0 0 0 7 7 0 0 210 0 0 0] 98 19 5488 6944 3424 7264 4800 6256 5888 4480 6576 3792 7584 2416 3056 2080 560 2736 4112 5168 1312 1104 0 27 )! !
!UserProfileSetPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

AllUsersShell guid: (GUID fromString: '{2E98AD2C-4678-44A7-86F8-CD5D4C42CF12}')!
AllUsersShell comment: ''!
!AllUsersShell categoriesForClass!Unclassified! !
!AllUsersShell methodsFor!

presenterClass
 
	^AllUsersPresenter.
!

shellName
 
	^'User Browser'.
! !
!AllUsersShell categoriesFor: #presenterClass!private! !
!AllUsersShell categoriesFor: #shellName!overrides!private! !

!AllUsersShell class methodsFor!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 608 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 5119 21 530 1201 801 416 706 8 #updateMenuBar 608 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 154 1 0 0] 98 1 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 896 0 482 8 4278190080 0 7 0 0 0 896 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.AllUsersPresenter)  8 #resource_Default_view 0 642 202 208 98 1 706 736 98 2 530 1 1 530 1169 725 896 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 106 1 0 0] 608 530 193 193 0 27 1216 0 27 )! !
!AllUsersShell class categoriesFor: #icon!public! !
!AllUsersShell class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

