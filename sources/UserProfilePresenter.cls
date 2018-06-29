"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #UserProfilePresenter
	instanceVariableNames: 'user detailsPresenter passwordPresenter symbolListPresenter groupListPresenter privilegeListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
UserProfilePresenter guid: (GUID fromString: '{23af6d76-e0d4-4d7e-8c81-46a93349090b}')!
UserProfilePresenter comment: ''!
!UserProfilePresenter categoriesForClass!Unclassified! !
!UserProfilePresenter methodsFor!

createComponents

	super createComponents.
	detailsPresenter 		:= self add: UserProfileDetailsPresenter 	new 	name: 'detailsTab'.
	passwordPresenter 		:= self add: UserProfilePasswordPresenter 	new 	name: 'passwordTab'.
	symbolListPresenter 		:= self add: ListPresenter 				new 	name: 'symbolList'.
	groupListPresenter 		:= self add: ListPresenter 				new 	name: 'groupList'.
	privilegeListPresenter 	:= self add: ListPresenter 				new 	name: 'privilegeList'.
	!

gciSession: aGciSession

	super 			gciSession: aGciSession.
	detailsPresenter 	gciSession: aGciSession.
	passwordPresenter 	gciSession: aGciSession.
!

update

	user := self trigger: #'needSelectedUser'.
	detailsPresenter 		updateForUser: user.
	passwordPresenter 		updateForUser: user.
	symbolListPresenter		list: #().
	groupListPresenter		list: #().
	privilegeListPresenter	list: #().
	user isNil ifTrue: [^self].
	symbolListPresenter		list: user symbolList.
	groupListPresenter 		list: user groupList.
	privilegeListPresenter	list: user privilegeList.
! !
!UserProfilePresenter categoriesFor: #createComponents!private! !
!UserProfilePresenter categoriesFor: #gciSession:!private! !
!UserProfilePresenter categoriesFor: #update!private! !

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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 528 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 2879 21 674 1161 501 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 227 7 0 0 4 1 0 0] 34 1 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1140850688 131073 784 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 5 nil nil nil 784 655878 ##(Smalltalk.CardLayout) 138 144 34 5 721414 ##(Smalltalk.Association) 8 'Symbol List' 410 ##(Smalltalk.ListView) 34 30 nil 784 34 2 8 1140969549 1025 1008 590662 2 ##(Smalltalk.ListModel) 138 144 528 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 850 8 4278190080 nil 5 nil nil nil 1008 nil 8 1795483760 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Dictionaries' 1137 #left 1202 #displayString 1232 ##(Smalltalk.SortedCollection) nil nil 1008 nil 3 nil nil #report 528 nil 131137 nil 34 4 nil nil 674 1 1 nil 562 138 144 34 2 626 #createAt:extent: 34 2 674 9 55 674 1145 439 1008 626 #text: 34 1 8 'Dictionaries' 1008 722 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 64 2 0 0 246 0 0 0] 8 #() 674 193 193 nil 27 962 8 'Groups' 410 ##(Smalltalk.EditableListView) 34 40 nil 784 34 2 8 1140969549 1025 1664 1074 138 144 528 nil 1136 850 8 4278190080 nil 5 nil nil nil 1664 nil 8 1795483760 ##(Smalltalk.BasicListAbstract) nil 1264 nil nil nil 674 65 65 nil nil 138 144 34 2 1447750 15 ##(Smalltalk.EditableListViewColumn) 8 '' 41 #left 1202 #displayString 8 #() 1202 #<= 1920 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 1 83886081 1952 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 1968 7 257 nil nil 1664 nil 1 nil nil true 1954 0 nil 1986 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:array :value | (array at: 1) editGroup: array value: value]' 8 #[30 105 17 63 148 233 1 190 106] #editGroup:value: 2064 7 513 nil 1053510 21 ##(Smalltalk.EmbeddedCheckBox) nil nil 8 #(67174400 0) 2160 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) false 850 8 4278190080 nil 5 nil nil nil 2160 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1858 8 'Groups' 1097 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1954 0 nil 1986 1 83886081 1952 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 2320 7 257 nil nil 1664 nil 3 nil nil false nil 1052998 20 ##(Smalltalk.EmbeddedTextEdit) nil nil 8 #(67174528 0) 2416 2194 nil false 2240 nil 850 2272 nil 5 nil nil nil 2416 nil nil 852486 ##(Smalltalk.NullConverter) nil 1888 1 nil nil nil nil nil nil nil nil nil #report 528 nil 131137 nil 8 #(nil) nil nil nil nil nil 3 nil nil nil nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 9 55 674 1145 439 1664 722 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 64 2 0 0 246 0 0 0] 8 #() 1616 nil 33 962 8 'Details' 410 ##(Smalltalk.ReferenceView) 34 14 nil 784 34 2 8 1140850688 131073 2720 nil 850 8 4278190080 nil 5 nil nil nil 2720 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.UserProfileDetailsPresenter) #resource_Default_view nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 9 55 674 1145 439 2720 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 64 2 0 0 246 0 0 0] 528 1616 nil 27 962 8 'Password' 410 ##(Smalltalk.ReferenceView) 34 14 nil 784 34 2 8 1140850688 131073 3024 nil 850 2800 nil 5 nil nil nil 3024 2818 ##(Smalltalk.UserProfilePasswordPresenter) #resource_Default_view nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 9 55 674 1145 439 3024 722 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 64 2 0 0 246 0 0 0] 528 1616 nil 27 962 8 'Privileges' 410 ##(Smalltalk.EditableListView) 34 40 nil 784 34 2 8 1140969549 1025 3296 1074 138 144 528 nil 1136 850 1776 nil 5 nil nil nil 3296 nil 8 1795483760 ##(Smalltalk.BasicListAbstract) nil 1264 nil nil nil 674 65 65 nil nil 138 144 34 2 1858 1888 41 #left 1202 #displayString 8 #() 1202 #<= 3504 1954 0 nil 1986 1 83886081 1952 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 3536 7 257 nil nil 3296 nil 1 nil nil true 1954 0 nil 1986 2 1 1952 8 'doIt' 8 '[:array :value | (array at: 1) editPrivilege: array value: value]' 8 #[30 105 17 63 148 233 1 190 106] #editPrivilege:value: 3616 7 513 nil 2146 nil nil 8 #(67174400 0) 3696 2194 nil false 2240 false 850 2272 nil 5 nil nil nil 3696 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1858 2304 1097 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1954 0 nil 1986 1 83886081 1952 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 3776 7 257 nil nil 3296 nil 3 nil nil false nil 2402 nil nil 8 #(67174528 0) 3856 2194 nil false 2240 nil 850 2272 nil 5 nil nil nil 3856 nil nil 2482 nil 1888 1 nil nil nil nil nil nil nil nil nil #report 528 nil 131137 nil 8 #(nil) nil nil nil nil nil 3 nil nil nil nil 562 138 144 34 1 626 #createAt:extent: 34 2 674 9 55 674 1145 439 3296 722 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 64 2 0 0 246 0 0 0] 8 #() 1616 nil 33 2720 170 192 34 10 3296 8 'privilegeList' 1664 8 'groupList' 2720 8 'detailsTab' 1008 8 'symbolList' 3024 8 'passwordTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 784 34 2 8 1140916736 1 4224 1074 138 144 34 5 2704 3008 992 1648 3280 nil 1136 nil nil 1 nil nil nil 4224 nil 8 1795821024 1954 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 4352 7 257 nil 1954 0 nil 4370 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 4416 7 257 nil 1264 nil nil nil nil nil #noIcons nil nil nil nil nil 562 138 144 34 3 626 #createAt:extent: 34 2 674 1 1 674 1161 501 4224 626 #basicSelectionsByIndex: 34 1 8 #(1) 4224 626 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 4224 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 68 2 0 0 250 0 0 0] 8 #() 1616 nil 27 562 138 144 34 1 626 #createAt:extent: 34 2 674 1 1 674 1161 501 784 722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 68 2 0 0 250 0 0 0] 34 6 2720 3024 1008 1664 3296 4224 1616 nil 27 1616 nil 27 )! !
!UserProfilePresenter class categoriesFor: #publishedEventsOfInstances!public! !
!UserProfilePresenter class categoriesFor: #resource_Default_view!public!resources-views! !

