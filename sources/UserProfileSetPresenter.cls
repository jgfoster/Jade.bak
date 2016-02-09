"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #UserProfileSetPresenter
	instanceVariableNames: 'disallowUsedPasswordsPresenter minPasswordSizePresenter maxPasswordSizePresenter maxRepeatingCharsPresenter maxConsecutiveCharsPresenter maxCharsOfSameTypePresenter staleAccountAgeLimitPresenter passwordAgeLimitPresenter passwordAgeWarningPresenter disallowedPasswordListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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
	string := gciSession serverPerform: #'allUsersPasswordLimits'.
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

