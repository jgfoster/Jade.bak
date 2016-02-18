"Filed out from Dolphin Smalltalk 7"!

FadingDialog subclass: #WaitOnGemStoneDialog
	instanceVariableNames: 'busySeconds codePresenter busySecondsPresenter gciSession timerProcess'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WaitOnGemStoneDialog guid: (GUID fromString: '{3C1B5A72-A9D7-4D7D-92EB-6BE38EFC109C}')!
WaitOnGemStoneDialog comment: ''!
!WaitOnGemStoneDialog categoriesForClass!Unclassified! !
!WaitOnGemStoneDialog methodsFor!

busySeconds: anInteger

	busySeconds := anInteger.
!

createComponents

	super createComponents.
	codePresenter 				:= self add: TextPresenter new name: 'code'.
	busySecondsPresenter 	:= self add: TextPresenter new name: 'busySeconds'.
!

gciSession: aGciSession

	gciSession := aGciSession.
!

message: aString

	codePresenter value: aString.
!

onViewClosed

	timerProcess notNil ifTrue: [
		| temp |
		temp := timerProcess.
		timerProcess := nil.
		temp terminate.
	].
	super onViewClosed.
!

onViewOpened

	busySeconds := 0.		"ensure that value is not nil before we start the update process!!"
	super onViewOpened.
	timerProcess := [
		self updateSeconds.
	] forkAt: Processor userBackgroundPriority.!

sendHardBreak

	gciSession hardBreak.
!

sendSoftBreak

	gciSession softBreak.
!

updateSeconds

	[
		self view = DeafObject current.
	] whileFalse: [
		busySecondsPresenter value: busySeconds.
		busySeconds := busySeconds + 1.
		Processor sleep: 1000.
	].
! !
!WaitOnGemStoneDialog categoriesFor: #busySeconds:!public! !
!WaitOnGemStoneDialog categoriesFor: #createComponents!public! !
!WaitOnGemStoneDialog categoriesFor: #gciSession:!public! !
!WaitOnGemStoneDialog categoriesFor: #message:!public! !
!WaitOnGemStoneDialog categoriesFor: #onViewClosed!public! !
!WaitOnGemStoneDialog categoriesFor: #onViewOpened!public! !
!WaitOnGemStoneDialog categoriesFor: #sendHardBreak!public! !
!WaitOnGemStoneDialog categoriesFor: #sendSoftBreak!public! !
!WaitOnGemStoneDialog categoriesFor: #updateSeconds!public! !

!WaitOnGemStoneDialog class methodsFor!

gciSession: aGciSession message: aString havingWaited: anInteger

	^self create
		gciSession: aGciSession;
		message: aString;
		busySeconds: anInteger;
		yourself.
!

icon

	^Icon fromFile: 'icons\GS32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.LayeredDialogView) 98 30 0 0 98 2 26214401 1179649 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 167 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 234 240 98 10 410 8 ##(Smalltalk.PushButton) 98 20 0 416 98 2 8 1140924416 1 592 0 0 0 7 0 0 0 592 0 8 4294902745 1180998 4 ##(Smalltalk.CommandDescription) 8 #sendSoftBreak 8 'Soft Break' 1 1 0 0 32 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 3 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point) 609 473 882 161 51 592 818 8 #isEnabled: 98 1 32 592 818 8 #text: 98 1 8 'Soft Break' 592 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 48 1 0 0 236 0 0 0 128 1 0 0 5 1 0 0] 98 0 882 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) 8 #fixedParentRight -159 1154 8 #fixedViewLeft 161 1154 8 #fixedParentBottom -49 1154 8 #fixedViewTop 51 410 608 98 20 0 416 98 2 8 1140924416 1 1296 0 0 0 7 0 0 0 1296 0 8 4294902745 690 8 #sendHardBreak 8 'Hard Break' 1 1 0 0 32 0 0 0 754 202 208 98 3 818 848 98 2 882 449 473 882 161 51 1296 818 944 98 1 32 1296 818 992 98 1 8 'Hard Break' 1296 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 224 0 0 0 236 0 0 0 48 1 0 0 5 1 0 0] 98 0 1104 0 29 1122 1168 -319 1200 161 1232 -49 1264 51 410 8 ##(Smalltalk.TextEdit) 98 16 0 416 98 2 8 1140916354 1025 1680 0 482 8 4278190080 0 7 0 0 0 1680 0 8 4294904533 852486 ##(Smalltalk.NullConverter) 0 0 3 754 202 208 98 3 818 848 98 2 882 225 477 882 81 45 1680 818 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval) 3 1 3 1680 818 8 #isTextModified: 98 1 32 1680 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 112 0 0 0 238 0 0 0 152 0 0 0 4 1 0 0] 98 0 1104 0 27 1122 1154 8 #fixedParentLeft 225 1200 81 1232 -45 1264 45 410 8 ##(Smalltalk.ScintillaView) 98 50 0 416 98 2 8 1176571972 1025 2176 721990 2 ##(Smalltalk.ValueHolder) 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 482 8 4278190080 0 7 0 0 0 2176 0 8 4294904345 1810 0 0 11 0 234 256 98 44 8 #callTip 1182790 1 ##(Smalltalk.ScintillaTextStyle) 77 786694 ##(Smalltalk.IndexedColor) 33554457 2466 33554471 1 0 0 0 0 2416 0 0 0 8 #specialSelector 2434 33 196934 1 ##(Smalltalk.RGB) 16646145 0 1 0 0 0 0 2512 0 0 0 8 #symbol 2434 9 2466 33554443 0 1 0 0 0 0 2576 0 0 0 8 #binary 2434 11 2466 33554433 0 1 0 0 0 0 2624 0 0 0 8 #braceMismatch 2434 71 2466 33554459 0 1 0 0 0 0 2672 0 0 0 8 #nil 2434 19 2560 0 1 0 0 0 0 2720 0 0 0 8 #assignment 2434 29 0 0 1 0 0 0 0 2752 0 0 0 8 #braceHighlight 2434 69 2466 33554465 0 1 0 0 0 0 2784 0 0 0 8 #self 2434 15 2560 0 1 0 0 0 0 2832 0 0 0 8 #character 2434 31 2546 16646399 0 1 0 0 0 0 2864 0 0 0 8 #super 2434 17 2560 0 1 0 0 0 0 2912 0 0 0 8 #normal 2434 1 0 0 1 0 0 0 0 2944 0 0 0 8 #lineNumber 2434 67 0 0 1 0 0 0 0 2976 0 0 0 8 #string 2434 3 2546 16646399 0 129 0 0 0 0 3008 0 0 0 8 #global 2434 21 0 0 1 0 0 0 0 3056 0 0 0 8 #keywordSend 2434 27 2466 33554437 0 1 0 0 0 0 3088 0 0 0 8 #special 2434 25 0 0 1 0 0 0 0 3136 0 0 0 8 #boolean 2434 13 2560 0 1 0 0 0 0 3168 0 0 0 8 #comment 2434 7 2546 65025 0 1 0 0 0 0 3200 0 0 0 8 #number 2434 5 2546 16711169 0 1 0 0 0 0 3248 0 0 0 8 #return 2434 23 2546 321 0 1 0 0 0 0 3296 0 0 0 8 #indentGuide 2434 75 2466 33554447 0 1 0 0 0 0 3344 0 0 0 98 40 2960 3024 3264 3216 2592 2640 3184 2848 2928 2736 3072 3312 3152 3104 2768 2880 2528 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2992 2800 2688 0 3360 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler) 2944 234 256 98 2 8 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 0 0 0 2176 8 #circle 0 0 0 0 0 0 202 208 98 0 0 63 9215 0 0 0 0 3376 0 0 65 0 0 0 8 '' 3 234 256 98 4 8 #smalltalk 2384 8 #container 234 256 98 2 2944 2434 1 0 0 1 0 0 0 0 2944 0 0 0 0 0 0 0 1 0 234 256 98 6 8 'indicator2' 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 5 2176 511 1 32 0 0 0 0 0 0 0 8 'indicator1' 3746 3 2176 33423361 5 32 0 0 0 0 0 0 0 8 'indicator0' 3746 1 2176 65025 3 32 0 0 0 0 0 0 0 0 0 234 256 98 6 8 #Notification 2434 1029 0 2546 31185883 1 0 0 0 0 3872 0 0 0 8 #Error 2434 1031 2546 335 2546 31317503 1 0 0 0 0 3920 0 0 0 8 #Warning 2434 1027 2546 60239 2546 31326207 1 0 0 0 0 3984 0 0 0 0 754 202 208 98 11 818 848 98 2 882 1 1 882 769 473 2176 818 1968 98 1 2002 3 1 3 2176 818 2048 98 1 32 2176 818 8 #modificationEventMask: 98 1 9215 2176 818 8 #secondaryCaretForecolor: 98 1 2546 16711423 2176 818 8 #caretPeriod: 98 1 1061 2176 818 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin) 1 2176 1 3 32 1 15 4466 3 2176 33 1 16 67108863 15 4466 5 2176 1 1 16 -67108863 15 2176 818 8 #tabIndents: 98 1 16 2176 818 8 #tabWidth: 98 1 9 2176 818 8 #setLexerLanguage: 98 1 3616 2176 818 8 #areAdditionalCaretsVisible: 98 1 16 2176 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 236 0 0 0] 98 0 1104 0 29 1122 2144 1 1168 1 1154 8 #fixedParentTop 1 1232 -49 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 1 4816 0 0 0 7 0 0 0 4816 0 8 4294902549 1810 0 0 0 754 202 208 98 2 818 848 98 2 882 11 477 882 211 45 4816 818 992 98 1 8 'Busy for seconds:' 4816 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 238 0 0 0 110 0 0 0 4 1 0 0] 98 0 1104 0 27 1122 2144 11 1200 211 1232 -45 1264 45 234 256 98 8 2176 8 'code' 1680 8 'busySeconds' 1296 8 'hardBreakButton' 592 8 'softBreakButton' 0 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore) 0 0 1 0 8 2004626976 754 202 208 98 3 818 848 98 2 882 5119 21 882 801 601 416 818 992 98 1 8 'Executing GemStone/Smalltalk Code...' 416 818 8 #updateMenuBar 3552 416 1042 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 143 11 0 0 54 1 0 0] 98 5 2176 1296 592 4816 1680 1104 0 27 )! !
!WaitOnGemStoneDialog class categoriesFor: #gciSession:message:havingWaited:!public! !
!WaitOnGemStoneDialog class categoriesFor: #icon!public! !
!WaitOnGemStoneDialog class categoriesFor: #resource_Default_view!public!resources-views! !

