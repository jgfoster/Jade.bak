"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #UserProfilePresenter
	instanceVariableNames: 'user detailsPresenter passwordPresenter symbolListPresenter groupListPresenter privilegeListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

