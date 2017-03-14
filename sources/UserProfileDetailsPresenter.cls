"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #UserProfileDetailsPresenter
	instanceVariableNames: 'userIDPresenter maxLoginsPresenter languagePresenter lastLoginPresenter disabledReasonPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
UserProfileDetailsPresenter guid: (GUID fromString: '{f6811f82-8afc-4569-a453-15b155f97432}')!
UserProfileDetailsPresenter comment: 'Presents details about a UserProfile instance in the AllUsersPresenter in the AllUsersShell.'!
!UserProfileDetailsPresenter categoriesForClass!Unclassified! !
!UserProfileDetailsPresenter methodsFor!

createComponents

	super createComponents.
	userIDPresenter 		:= self add: TextPresenter new name: 'userID'.
	maxLoginsPresenter 		:= self add: TextPresenter new name: 'maxLogins'.
	languagePresenter 		:= self add: TextPresenter new name: 'language'.
	lastLoginPresenter 		:= self add: TextPresenter new name: 'lastLogin'.
	disabledReasonPresenter 	:= self add: TextPresenter new name: 'disabledReason'.
!

updateForUser: aGsUser

	userIDPresenter 		value: nil.
	maxLoginsPresenter 		value: nil.
	languagePresenter 		value: nil.
	lastLoginPresenter 		value: nil.
	disabledReasonPresenter 	value: nil.
	aGsUser isNil ifTrue: [^self].
	userIDPresenter 		value: aGsUser name.
	maxLoginsPresenter 		value: aGsUser maxSessions.
	languagePresenter 		value: aGsUser language.
	lastLoginPresenter 		value: aGsUser lastLoginTime.
	disabledReasonPresenter 	value: aGsUser disabledReason.
! !
!UserProfileDetailsPresenter categoriesFor: #createComponents!private! !
!UserProfileDetailsPresenter categoriesFor: #updateForUser:!private! !

!UserProfileDetailsPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ContainerView) 98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 234 240 98 16 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 1 560 0 0 0 5 0 0 0 560 0 8 4294903929 852486 ##(Smalltalk.NullConverter) 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point) 3 139 818 301 45 560 754 8 #text: 98 1 8 'Disabled Reason:' 560 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 69 0 0 0 151 0 0 0 91 0 0 0] 98 0 818 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) 8 #fixedParentLeft 3 1042 8 #fixedViewLeft 301 1042 8 #fixedPreviousBottom 3 1042 8 #fixedViewTop 45 410 8 ##(Smalltalk.TextEdit) 98 16 0 416 98 2 8 1140916352 1025 1184 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 5 0 0 0 1184 0 8 4294903463 658 0 0 1 690 202 208 98 3 754 784 98 2 818 303 1 818 321 45 1184 754 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval) 3 1 3 1184 754 8 #isTextModified: 98 1 32 1184 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 151 0 0 0 0 0 0 0 55 1 0 0 22 0 0 0] 98 0 992 0 27 1010 1042 8 #fixedPreviousRight 1 1088 321 1042 8 #fixedPreviousTop 1 1152 45 410 1200 98 16 0 416 98 2 8 1140916352 1025 1712 0 1266 8 4278190080 0 5 0 0 0 1712 0 8 4294903463 658 0 0 3 690 202 208 98 3 754 784 98 2 818 303 93 818 321 45 1712 754 1472 98 1 1506 3 1 3 1712 754 1552 98 1 32 1712 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 151 0 0 0 46 0 0 0 55 1 0 0 68 0 0 0] 98 0 992 0 27 1010 1648 1 1088 321 1680 1 1152 45 410 1200 98 16 0 416 98 2 8 1140916352 1025 2096 0 1266 1792 0 5 0 0 0 2096 0 8 4294903463 658 0 0 1 690 202 208 98 3 754 784 98 2 818 303 139 818 799 45 2096 754 1472 98 1 1506 3 1 3 2096 754 1552 98 1 32 2096 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 151 0 0 0 69 0 0 0 38 2 0 0 91 0 0 0] 98 0 992 0 27 1010 1648 1 1042 8 #fixedParentRight 1 1680 1 1152 45 410 576 98 16 0 416 98 2 8 1140850944 1 2496 0 0 0 5 0 0 0 2496 0 8 4294903929 658 0 0 0 690 202 208 98 2 754 784 98 2 818 3 93 818 301 45 2496 754 880 98 1 8 'Most Recent Login:' 2496 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 46 0 0 0 151 0 0 0 68 0 0 0] 98 0 992 0 27 1010 1056 3 1088 301 1120 3 1152 45 410 576 98 16 0 416 98 2 8 1140850944 1 2816 0 0 0 5 0 0 0 2816 0 8 4294903929 658 0 0 0 690 202 208 98 2 754 784 98 2 818 3 47 818 301 45 2816 754 880 98 1 8 'Max Concurrent Logins:' 2816 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 23 0 0 0 151 0 0 0 45 0 0 0] 98 0 992 0 27 1010 1056 3 1088 301 1120 3 1152 45 410 1200 98 16 0 416 98 2 8 1140924546 1025 3136 0 1266 1792 0 5 0 0 0 3136 0 8 4294903463 658 0 0 1 690 202 208 98 3 754 784 98 2 818 303 47 818 61 45 3136 754 1472 98 1 1506 3 1 3 3136 754 1552 98 1 32 3136 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 151 0 0 0 23 0 0 0 181 0 0 0 45 0 0 0] 98 0 992 0 27 1010 1648 1 1088 61 1680 1 1152 45 410 576 98 16 0 416 98 2 8 1140850944 1 3504 0 0 0 5 0 0 0 3504 0 8 4294903929 658 0 0 0 690 202 208 98 2 754 784 98 2 818 3 1 818 301 45 3504 754 880 98 1 8 'User ID:' 3504 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 0 0 0 0 151 0 0 0 22 0 0 0] 98 0 992 0 27 1010 1056 3 1088 301 1042 8 #fixedParentTop 1 1152 45 234 256 98 8 1184 8 'userID' 1712 8 'lastLogin' 2096 8 'disabledReason' 3136 8 'maxLogins' 0 690 202 208 98 1 754 784 98 2 818 5119 21 818 1101 501 416 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 37 12 0 0 4 1 0 0] 98 8 3504 1184 2816 3136 2496 1712 560 2096 992 0 27 )! !
!UserProfileDetailsPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

