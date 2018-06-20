"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #AllUsersPresenter
	instanceVariableNames: 'userListPresenter userProfilePresenter passwordLimitsPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AllUsersPresenter guid: (GUID fromString: '{8e70aee2-43a1-498d-8e5a-1f5c9d72ebc1}')!
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ContainerView) 98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout) 234 240 98 0 32 234 256 544 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 1 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point) 5119 21 706 1201 801 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 154 1 0 0] 98 1 410 8 ##(Smalltalk.CardContainer) 98 16 0 416 98 2 8 1140850688 131073 816 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 5 0 0 0 816 655878 ##(Smalltalk.CardLayout) 202 208 98 2 721414 ##(Smalltalk.Association) 8 'All Users' 410 432 98 15 0 816 98 2 8 1140850688 131073 1056 0 0 0 5 0 0 0 1056 498 234 240 98 4 410 8 ##(Smalltalk.ReferenceView) 98 14 0 1056 98 2 8 1140850688 131073 1168 0 898 8 4278190080 0 5 0 0 0 1168 1180166 ##(Smalltalk.ResourceIdentifier) 8 ##(Smalltalk.UserProfilePresenter) 8 #resource_Default_view 0 578 202 208 98 1 642 672 98 2 706 1 331 706 1185 409 1168 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 165 0 0 0 80 2 0 0 113 1 0 0] 544 706 193 193 0 27 524806 ##(Smalltalk.Fraction) 511 183 410 1184 98 14 0 1056 98 2 8 1140850688 131073 1536 0 898 1264 0 5 0 0 0 1536 1282 8 ##(Smalltalk.UserListPresenter) 1328 0 578 202 208 98 1 642 672 98 2 706 1 1 706 1185 321 1536 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 80 2 0 0 160 0 0 0] 544 1488 0 27 1506 401 183 16 234 256 98 4 1168 8 'userProfile' 1536 8 'userList' 0 578 202 208 98 1 642 672 98 2 706 9 55 706 1185 739 1056 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 84 2 0 0 140 1 0 0] 98 3 1536 410 8 ##(Smalltalk.Splitter) 98 12 0 1056 98 2 8 1140850688 1 2032 0 898 8 4278190080 0 517 0 0 0 2032 578 202 208 98 1 642 672 98 2 706 1 321 706 1185 11 2032 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 160 0 0 0 80 2 0 0 165 0 0 0] 98 0 1488 0 27 1168 1488 0 27 1010 8 'Password Limits' 410 1184 98 14 0 816 98 2 8 1140850688 131073 2336 0 898 8 4278190080 0 5 0 0 0 2336 1282 8 ##(Smalltalk.UserProfileSetPresenter) 1328 0 578 202 208 98 1 642 672 98 2 706 9 55 706 1185 739 2336 754 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 84 2 0 0 140 1 0 0] 544 1488 0 27 1056 234 256 98 2 2336 8 'passwordLimits' 0 410 8 ##(Smalltalk.TabViewXP) 98 28 0 816 98 2 8 1140916736 1 2656 590662 2 ##(Smalltalk.ListModel) 202 208 98 2 1040 2320 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 0 0 1 0 0 0 2656 0 8 4294904353 787814 3 ##(Smalltalk.BlockClosure) 0 0 918822 ##(Smalltalk.CompiledMethod) 2 3 8 ##(Smalltalk.ListControlView) 8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 8 #displayString 2864 7 257 0 2850 0 0 2882 2 3 8 ##(Smalltalk.IconicListAbstract) 8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 2976 7 257 0 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 8 #noIcons 0 0 0 0 0 578 202 208 98 3 642 672 98 2 706 1 1 706 1201 801 2656 642 8 #basicSelectionsByIndex: 98 1 98 1 3 2656 642 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 2656 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 144 1 0 0] 98 0 1488 0 27 578 202 208 98 1 642 672 98 2 706 1 1 706 1201 801 816 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 2 0 0 144 1 0 0] 98 3 1056 2336 2656 1488 0 27 1488 0 27 )! !
!AllUsersPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

