"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #UserProfilePasswordPresenter
	instanceVariableNames: 'whenPasswordChangedPresenter loginsRemainingPresenter isSpecialUserPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
UserProfilePasswordPresenter guid: (GUID fromString: '{5BBE25B8-5AFD-4858-AC78-843769085AA8}')!
UserProfilePasswordPresenter comment: ''!
!UserProfilePasswordPresenter categoriesForClass!Unclassified! !
!UserProfilePasswordPresenter methodsFor!

createComponents

	super createComponents.
	whenPasswordChangedPresenter := self add: TextPresenter 	new name: 'whenPasswordChanged'.
	loginsRemainingPresenter 		:= self add: NumberPresenter 	new name: 'loginsRemaining'.
	isSpecialUserPresenter 		:= self add: BooleanPresenter 	new name: 'isSpecialUser'.
!

updateForUser: aGsUserProfile 

	whenPasswordChangedPresenter 	value: nil.
	loginsRemainingPresenter 			value: nil.
	isSpecialUserPresenter 			value: nil.
	aGsUserProfile isNil ifTrue: [^self].
	whenPasswordChangedPresenter 	value: aGsUserProfile lastPasswordChange.
	loginsRemainingPresenter 			value: aGsUserProfile remainingLogins.
	isSpecialUserPresenter 			value: aGsUserProfile isSpecial.
! !
!UserProfilePasswordPresenter categoriesFor: #createComponents!private! !
!UserProfilePasswordPresenter categoriesFor: #updateForUser:!private! !

!UserProfilePasswordPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ContainerView) 98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 234 240 98 28 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 1 560 0 0 0 5 0 0 0 560 0 8 4294903929 852486 ##(Smalltalk.NullConverter) 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point) 11 181 818 111 45 560 754 8 #text: 98 1 8 'Old:' 560 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 90 0 0 0 60 0 0 0 112 0 0 0] 98 0 818 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) 8 #fixedParentLeft 11 1042 8 #fixedViewLeft 111 1042 8 #fixedPreviousTop 41 1042 8 #fixedViewTop 45 410 576 98 16 0 416 98 2 8 1140850944 1 1184 0 0 0 7 0 0 0 1184 0 8 4294903929 658 0 0 0 690 202 208 98 2 754 784 98 2 818 3 1 818 511 45 1184 754 880 98 1 8 'Most Recent Password Change:	' 1184 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 0 0 0 0 0 1 0 0 22 0 0 0] 98 0 992 0 27 1010 1056 3 1088 511 1042 8 #fixedParentTop 1 1152 45 410 8 ##(Smalltalk.CheckBox) 98 16 0 416 98 2 8 1409363203 1 1536 721990 2 ##(Smalltalk.ValueHolder) 0 0 1114118 ##(Smalltalk.NeverSearchPolicy) 32 0 0 5 0 0 0 1536 0 8 4294902303 658 0 0 0 690 202 208 98 2 754 784 98 2 818 3 95 818 511 45 1536 754 880 98 1 8 'Is Special User (Password Never Expires)' 1536 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 47 0 0 0 0 1 0 0 69 0 0 0] 98 0 992 0 27 1010 1056 3 1088 511 1042 8 #fixedPreviousBottom 3 1152 45 410 576 98 16 0 416 98 2 8 1140850944 1 1968 0 0 0 7 0 0 0 1968 0 8 4294903929 658 0 0 0 690 202 208 98 2 754 784 98 2 818 3 47 818 511 45 1968 754 880 98 1 8 'Logins Remaining Before Password Expires:' 1968 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 23 0 0 0 0 1 0 0 45 0 0 0] 98 0 992 0 27 1010 1056 3 1088 511 1936 3 1152 45 410 576 98 16 0 416 98 2 8 1140850944 1 2288 0 0 0 5 0 0 0 2288 0 8 4294903929 658 0 0 0 690 202 208 98 2 754 784 98 2 818 371 181 818 111 45 2288 754 880 98 1 8 'New:' 2288 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 90 0 0 0 240 0 0 0 112 0 0 0] 98 0 992 0 27 1010 1042 8 #fixedPreviousRight 11 1088 111 1120 1 1152 45 410 8 ##(Smalltalk.TextEdit) 98 16 0 416 98 2 8 1140916352 1025 2640 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 7 0 0 0 2640 0 8 4294903463 658 0 0 1 690 202 208 98 3 754 784 98 2 818 513 1 818 587 45 2640 754 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval) 3 1 3 2640 754 8 #isTextModified: 98 1 32 2640 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 1 0 0 0 0 0 0 37 2 0 0 22 0 0 0] 98 0 992 0 27 1010 2608 1 1042 8 #fixedParentRight -1 1120 1 1152 45 410 2656 98 16 0 416 98 2 8 1140924546 1025 3136 0 2722 2752 0 7 0 0 0 3136 0 8 4294903463 658 0 0 1 690 202 208 98 3 754 784 98 2 818 513 49 818 61 45 3136 754 2928 98 1 2962 3 1 3 3136 754 3008 98 1 32 3136 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 1 0 0 24 0 0 0 30 1 0 0 46 0 0 0] 98 0 992 0 27 1010 2608 1 1088 61 1120 3 1152 45 410 576 98 16 0 416 98 2 8 1140850944 1 3504 0 0 0 5 0 0 0 3504 0 8 4294903929 658 0 0 0 690 202 208 98 2 754 784 98 2 818 731 181 818 111 45 3504 754 880 98 1 8 'Confirm:' 3504 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 109 1 0 0 90 0 0 0 164 1 0 0 112 0 0 0] 98 0 992 0 27 1010 2608 11 1088 111 1120 1 1152 45 410 2656 98 16 0 416 98 2 8 1140916384 1025 3824 0 2722 8 4278190080 0 5 0 0 0 3824 0 8 4294903463 658 0 0 1 690 202 208 98 3 754 784 98 2 818 481 181 818 241 45 3824 754 2928 98 1 2962 3 1 3 3824 754 3008 98 1 32 3824 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 0 0 0 90 0 0 0 104 1 0 0 112 0 0 0] 98 0 992 0 27 1010 2608 1 1088 241 1120 1 1152 45 410 8 ##(Smalltalk.GroupBox) 98 14 0 416 98 2 8 1140850695 65 4208 0 196934 1 ##(Smalltalk.RGB) 8 4294967295 0 5 0 0 0 4208 0 8 4294902303 690 202 208 98 2 754 784 98 2 818 3 141 818 1097 137 4208 754 880 98 1 8 'Change Password' 4208 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 70 0 0 0 37 2 0 0 138 0 0 0] 98 0 992 0 27 1010 1056 3 3104 -1 1936 3 1152 137 410 8 ##(Smalltalk.PushButton) 98 20 0 416 98 2 8 1140924416 1 4576 0 0 0 5 0 0 0 4576 0 8 4294902303 1180998 4 ##(Smalltalk.CommandDescription) 0 8 'Set New Password' 1 1 0 0 32 0 0 0 690 202 208 98 2 754 784 98 2 818 841 227 818 241 45 4576 754 880 98 1 8 'Set New Password' 4576 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 164 1 0 0 113 0 0 0 28 2 0 0 135 0 0 0] 98 0 992 0 29 1010 1042 8 #fixedViewRight -239 2608 1 1936 3 1152 45 410 2656 98 16 0 416 98 2 8 1140916384 1025 4976 0 2722 3904 0 5 0 0 0 4976 0 8 4294903463 658 0 0 1 690 202 208 98 3 754 784 98 2 818 121 181 818 241 45 4976 754 2928 98 1 2962 3 1 3 4976 754 3008 98 1 32 4976 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 90 0 0 0 180 0 0 0 112 0 0 0] 98 0 992 0 27 1010 2608 1 1088 241 1120 1 1152 45 410 2656 98 16 0 416 98 2 8 1140916384 1025 5344 0 2722 3904 0 5 0 0 0 5344 0 8 4294903463 658 0 0 1 690 202 208 98 3 754 784 98 2 818 841 181 818 241 45 5344 754 2928 98 1 2962 3 1 3 5344 754 3008 98 1 32 5344 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 164 1 0 0 90 0 0 0 28 2 0 0 112 0 0 0] 98 0 992 0 27 1010 2608 1 1088 241 1120 1 1152 45 410 4592 98 20 0 416 98 2 8 1140924416 1 5712 0 0 0 7 0 0 0 5712 0 8 4294902303 4674 8 #clearOldPasswords 8 'Clear Old Passwords (to allow reuse)' 1 1 0 0 32 0 0 0 690 202 208 98 2 754 784 98 2 818 593 49 818 507 45 5712 754 880 98 1 8 'Clear Old Passwords (to allow reuse)' 5712 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 40 1 0 0 24 0 0 0 37 2 0 0 46 0 0 0] 98 0 992 0 29 1010 2608 21 3104 -1 1120 1 1152 45 234 256 98 12 3136 8 'loginsRemaining' 1536 8 'isSpecialUser' 2640 8 'whenPasswordChanged' 4976 8 'oldPassword' 3824 8 'newPassword' 5344 8 'confirmPassword' 0 690 202 208 98 1 754 784 98 2 818 5119 21 818 1101 501 416 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 37 12 0 0 4 1 0 0] 98 14 1184 2640 1968 3136 5712 1536 4208 560 4976 2288 3824 3504 5344 4576 992 0 27 )! !
!UserProfilePasswordPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

