"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #AllUsersPresenter
	instanceVariableNames: 'userListPresenter userProfilePresenter passwordLimitsPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AllUsersPresenter guid: (GUID fromString: '{8E70AEE2-43A1-498D-8E5A-1F5C9D72EBC1}')!
AllUsersPresenter comment: ''!
!AllUsersPresenter categoriesForClass!Unclassified! !
!AllUsersPresenter methodsFor!

createComponents

	super createComponents.
	userListPresenter 		:= self add: UserListPresenter 		new name: 'userList'.
	userProfilePresenter 		:= self add: UserProfilePresenter 	new name: 'userProfile'.
	passwordLimitsPresenter 	:= self add: UserProfileSetPresenter 	new name: 'passwordLimits'.
!

createSchematicWiring

	super createSchematicWiring.
	userListPresenter 	when: #'selectionChanged' 	send: #'update' 		to: userProfilePresenter.
	userProfilePresenter 	when: #'needSelectedUser' 	send: #'selection' 	to: userListPresenter.
!

gciSession: aGciSession

	super gciSession: aGciSession.
	userListPresenter 		gciSession: aGciSession.
	userProfilePresenter 		gciSession: aGciSession.
	passwordLimitsPresenter	gciSession: aGciSession.
!

onViewOpened

	super onViewOpened.
	userListPresenter selectFirstUser.
! !
!AllUsersPresenter categoriesFor: #createComponents!private! !
!AllUsersPresenter categoriesFor: #createSchematicWiring!private! !
!AllUsersPresenter categoriesFor: #gciSession:!private! !
!AllUsersPresenter categoriesFor: #onViewOpened!private! !

!AllUsersPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 544 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  2559 21 706 1201 801 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 87 7 0 0 154 1 0 0] 98 1 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1140850688 131073 816 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 816 655878 ##(Smalltalk.CardLayout)  202 208 98 2 721414 ##(Smalltalk.Association)  8 'All Users' 410 432 98 15 0 816 98 2 8 1140850688 131073 1056 0 0 0 5 0 0 0 1056 498 234 240 98 4 410 8 ##(Smalltalk.ReferenceView)  98 14 0 1056 98 2 8 1140850688 131073 1168 0 898 8 4278190080 0 5 0 0 0 1168 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.UserListPresenter)  8 #resource_Default_view 0 578 202 208 98 1 642 672 98 2 706 1 1 706 1185 293 1168 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 80 2 0 0 146 0 0 0] 544 706 193 193 0 27 5 410 1184 98 14 0 1056 98 2 8 1140850688 131073 1504 0 898 1264 0 5 0 0 0 1504 1282 8 ##(Smalltalk.UserProfilePresenter)  1328 0 578 202 208 98 1 642 672 98 2 706 1 303 706 1185 443 1504 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 151 0 0 0 80 2 0 0 116 1 0 0] 544 1488 0 27 7 16 234 256 98 4 1168 8 'userList' 1504 8 'userProfile' 0 578 202 208 98 1 642 672 98 2 706 9 49 706 1185 745 1056 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 84 2 0 0 140 1 0 0] 98 3 1168 410 8 ##(Smalltalk.Splitter)  98 12 0 1056 98 2 8 1140850688 1 1984 0 898 8 4278190080 0 517 0 0 0 1984 578 202 208 98 1 642 672 98 2 706 1 293 706 1185 11 1984 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 146 0 0 0 80 2 0 0 151 0 0 0] 98 0 1488 0 27 1504 1488 0 27 1010 8 'Password Limits' 410 1184 98 14 0 816 98 2 8 1140850688 131073 2288 0 898 8 4278190080 0 5 0 0 0 2288 1282 8 ##(Smalltalk.UserProfileSetPresenter)  1328 0 578 202 208 98 1 642 672 98 2 706 9 49 706 1185 745 2288 754 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 84 2 0 0 140 1 0 0] 544 1488 0 27 1056 234 256 98 2 2288 8 'passwordLimits' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 816 98 2 8 1140916736 1 2608 590662 2 ##(Smalltalk.ListModel)  202 208 98 2 1040 2272 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 0 0 1 0 0 0 2608 0 8 4294904209 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 8 #displayString 2848 7 257 0 2834 0 0 2866 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 2960 7 257 0 2762 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 8 #noIcons 0 0 0 0 0 578 202 208 98 3 642 672 98 2 706 1 1 706 1201 801 2608 642 8 #selectionByIndex:ifAbsent: 98 2 3 642 8 #yourself 544 0 2608 642 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 2608 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 144 1 0 0] 98 0 1488 0 27 578 202 208 98 1 642 672 98 2 706 1 1 706 1201 801 816 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 144 1 0 0] 98 3 1056 2288 2608 1488 0 27 1488 0 27 )! !
!AllUsersPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

