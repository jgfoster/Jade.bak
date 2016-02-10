"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #UserProfileDetailsPresenter
	instanceVariableNames: 'userIDPresenter maxLoginsPresenter languagePresenter lastLoginPresenter disabledReasonPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
UserProfileDetailsPresenter guid: (GUID fromString: '{F6811F82-8AFC-4569-A453-15B155F97432}')!
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 16 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140924546 1025 560 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 560 0 8 4294903717 852486 ##(Smalltalk.NullConverter)  0 0 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  281 41 866 61 41 560 802 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 560 802 8 #isTextModified: 98 1 32 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 0 0 0 20 0 0 0 170 0 0 0 40 0 0 0] 98 0 866 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 281 1154 8 #fixedViewLeft 61 1154 8 #fixedParentTop 41 1154 8 #fixedViewTop 41 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 1296 0 0 0 5 0 0 0 1296 0 8 4294903713 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 121 866 281 41 1296 802 8 #text: 98 1 8 'Disabled Reason:' 1296 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 60 0 0 0 140 0 0 0 80 0 0 0] 98 0 1104 0 27 1122 1168 1 1200 281 1232 121 1264 41 410 576 98 16 0 416 98 2 8 1140916352 1025 1648 0 642 8 4278190080 0 5 0 0 0 1648 0 8 4294903717 706 0 0 1 738 202 208 98 3 802 832 98 2 866 281 1 866 321 41 1648 802 928 98 1 962 3 1 3 1648 802 1008 98 1 32 1648 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 0 0 0 0 0 0 0 44 1 0 0 20 0 0 0] 98 0 1104 0 27 1122 1168 281 1200 321 1232 1 1264 41 410 576 98 16 0 416 98 2 8 1140916352 1025 2032 0 642 672 0 5 0 0 0 2032 0 8 4294903717 706 0 0 1 738 202 208 98 3 802 832 98 2 866 281 121 866 821 41 2032 802 928 98 1 962 3 1 3 2032 802 1008 98 1 32 2032 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 0 0 0 60 0 0 0 38 2 0 0 80 0 0 0] 98 0 1104 0 27 1122 1168 281 1154 8 #fixedParentRight 1 1232 121 1264 41 410 576 98 16 0 416 98 2 8 1140916352 1025 2432 0 642 672 0 5 0 0 0 2432 0 8 4294903717 706 0 0 3 738 202 208 98 3 802 832 98 2 866 281 81 866 321 41 2432 802 928 98 1 962 3 1 3 2432 802 1008 98 1 32 2432 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 140 0 0 0 40 0 0 0 44 1 0 0 60 0 0 0] 98 0 1104 0 27 1122 1168 281 1200 321 1232 81 1264 41 410 1312 98 16 0 416 98 2 8 1140850944 1 2800 0 0 0 5 0 0 0 2800 0 8 4294903713 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 81 866 281 41 2800 802 1536 98 1 8 'Most Recent Login:' 2800 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 40 0 0 0 140 0 0 0 60 0 0 0] 98 0 1104 0 27 1122 1168 1 1200 281 1232 81 1264 41 410 1312 98 16 0 416 98 2 8 1140850944 1 3120 0 0 0 5 0 0 0 3120 0 8 4294903713 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 1 866 281 41 3120 802 1536 98 1 8 'User ID:' 3120 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 140 0 0 0 20 0 0 0] 98 0 1104 0 27 1122 1168 1 1200 281 1232 1 1264 41 410 1312 98 16 0 416 98 2 8 1140850944 1 3440 0 0 0 5 0 0 0 3440 0 8 4294903713 706 0 0 0 738 202 208 98 2 802 832 98 2 866 1 41 866 281 41 3440 802 1536 98 1 8 ' Max Concurrent Logins:' 3440 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 140 0 0 0 40 0 0 0] 98 0 1104 0 27 1122 1168 1 1200 281 1232 41 1264 41 234 256 98 8 560 8 'maxLogins' 2432 8 'lastLogin' 2032 8 'disabledReason' 1648 8 'userID' 0 738 202 208 98 1 802 832 98 2 866 5119 21 866 1101 501 416 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 37 12 0 0 4 1 0 0] 98 8 3120 1648 3440 560 2800 2432 1296 2032 1104 0 27 )! !
!UserProfileDetailsPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

