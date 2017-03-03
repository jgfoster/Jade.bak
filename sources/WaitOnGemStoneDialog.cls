"Filed out from Dolphin Smalltalk 7"!

FadingDialog subclass: #WaitOnGemStoneDialog
	instanceVariableNames: 'busySeconds codePresenter busySecondsPresenter gciSession timerProcess'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WaitOnGemStoneDialog guid: (GUID fromString: '{3c1b5a72-a9d7-4d7d-92eb-6be38efc109c}')!
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.LayeredDialogView) 98 30 0 0 98 2 26214401 1179649 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 167 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 234 240 98 10 410 8 ##(Smalltalk.PushButton) 98 20 0 416 98 2 8 1140924416 1 592 0 0 0 7 0 0 0 592 0 8 4294902755 1180998 4 ##(Smalltalk.CommandDescription) 8 #sendHardBreak 8 'Hard Break' 1 1 0 0 32 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 3 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point) 449 473 882 161 51 592 818 8 #isEnabled: 98 1 32 592 818 8 #text: 98 1 8 'Hard Break' 592 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 224 0 0 0 236 0 0 0 48 1 0 0 5 1 0 0] 98 0 882 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) 8 #fixedParentRight -319 1154 8 #fixedViewLeft 161 1154 8 #fixedParentBottom -49 1154 8 #fixedViewTop 51 410 8 ##(Smalltalk.ScintillaView) 98 50 0 416 98 2 8 1176571972 1025 1296 721990 2 ##(Smalltalk.ValueHolder) 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 482 8 4278190080 0 7 0 0 0 1296 0 8 4294902941 852486 ##(Smalltalk.NullConverter) 0 0 11 0 234 256 98 4 8 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 0 0 1 0 0 0 0 1552 0 0 0 8 #callTip 1570 77 786694 ##(Smalltalk.IndexedColor) 33554457 1634 33554471 1 0 0 0 0 1600 0 0 0 98 256 1584 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1616 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 0 0 32 202 208 98 0 234 256 98 2 8 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 0 0 0 1296 8 #circle 0 0 0 0 0 0 202 8 ##(Smalltalk.IdentitySet) 1744 0 234 240 1744 9215 0 0 0 0 1634 33554447 0 0 65 0 0 234 256 98 6 8 #literalBytes 8 '[]' 8 #literalArray 8 '()' 8 #specialCharacter 8 '()[]<>' 8 '' 3 234 256 98 4 8 #container 234 256 98 2 1552 1584 8 #smalltalk 234 256 98 44 8 #indentGuide 1570 75 1904 0 1 0 0 0 0 2192 0 0 0 8 #super 1570 17 196934 1 ##(Smalltalk.RGB) 16646145 0 1 0 0 0 0 2224 0 0 0 8 #specialSelector 1570 33 2272 0 1 0 0 0 0 2288 0 0 0 8 #assignment 1570 29 0 0 1 0 0 0 0 2320 0 0 0 8 #lineNumber 1570 67 0 0 1 0 0 0 0 2352 0 0 0 8 #nil 1570 19 2272 0 1 0 0 0 0 2384 0 0 0 8 #global 1570 21 0 0 1 0 0 0 0 2416 0 0 0 8 #string 1570 3 2258 16646399 0 129 0 0 0 0 2448 0 0 0 8 #character 1570 31 2258 16646399 0 1 0 0 0 0 2496 0 0 0 8 #keywordSend 1570 27 1634 33554437 0 1 0 0 0 0 2544 0 0 0 8 #braceHighlight 1570 69 1634 33554465 0 1 0 0 0 0 2592 0 0 0 8 #special 1570 25 0 0 1 0 0 0 0 2640 0 0 0 8 #symbol 1570 9 1634 33554443 0 1 0 0 0 0 2672 0 0 0 1600 1570 77 1648 1664 1 0 0 0 0 1600 0 0 0 8 #number 1570 5 2258 16711169 0 1 0 0 0 0 2736 0 0 0 8 #binary 1570 11 1634 33554433 0 1 0 0 0 0 2784 0 0 0 8 #return 1570 23 2258 321 0 1 0 0 0 0 2832 0 0 0 1552 1570 1 0 0 1 0 0 0 0 1552 0 0 0 8 #braceMismatch 1570 71 1634 33554459 0 1 0 0 0 0 2896 0 0 0 8 #comment 1570 7 2258 65025 0 1 0 0 0 0 2944 0 0 0 8 #boolean 1570 13 2272 0 1 0 0 0 0 2992 0 0 0 8 #self 1570 15 2272 0 1 0 0 0 0 3024 0 0 0 0 0 0 0 1 0 234 256 98 6 8 'indicator0' 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 1296 65025 3 32 0 0 0 0 0 0 0 8 'indicator1' 3106 3 1296 33423361 5 32 0 0 0 0 0 0 0 8 'indicator2' 3106 5 1296 511 1 32 0 0 0 0 0 0 0 0 0 234 256 98 6 8 #Notification 1570 1029 0 2258 31185883 1 0 0 0 0 3232 0 0 0 8 #Error 1570 1031 2258 335 2258 31317503 1 0 0 0 0 3280 0 0 0 8 #Warning 1570 1027 2258 60239 2258 31326207 1 0 0 0 0 3344 0 0 0 0 754 202 208 98 5 818 848 98 2 882 1 1 882 769 473 1296 818 8 #modificationEventMask: 98 1 9215 1296 818 8 #caretPeriod: 98 1 1061 1296 818 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin) 1 1296 1 3 32 1 15 3682 3 1296 33 1 16 67108863 15 3682 5 1296 1 1 16 -67108863 15 1296 818 8 #tabWidth: 98 1 9 1296 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 236 0 0 0] 98 0 1104 0 29 1122 1154 8 #fixedParentLeft 1 1168 1 1154 8 #fixedParentTop 1 1232 -49 410 8 ##(Smalltalk.TextEdit) 98 16 0 416 98 2 8 1140916354 1025 3920 0 482 8 4278190080 0 7 0 0 0 3920 0 8 4294902043 1490 0 0 3 754 202 208 98 1 818 848 98 2 882 225 477 882 81 45 3920 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 112 0 0 0 238 0 0 0 152 0 0 0 4 1 0 0] 98 0 1104 0 27 1122 3856 225 1200 81 1232 -45 1264 45 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 1 4240 0 0 0 7 0 0 0 4240 0 8 4294903017 1490 0 0 0 754 202 208 98 2 818 848 98 2 882 11 477 882 211 45 4240 818 992 98 1 8 'Busy for seconds:' 4240 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 238 0 0 0 110 0 0 0 4 1 0 0] 98 0 1104 0 27 1122 3856 11 1200 211 1232 -45 1264 45 410 608 98 20 0 416 98 2 8 1140924416 1 4576 0 0 0 7 0 0 0 4576 0 8 4294902755 690 8 #sendSoftBreak 8 'Soft Break' 1 1 0 0 32 0 0 0 754 202 208 98 3 818 848 98 2 882 609 473 882 161 51 4576 818 944 98 1 32 4576 818 992 98 1 8 'Soft Break' 4576 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 48 1 0 0 236 0 0 0 128 1 0 0 5 1 0 0] 98 0 1104 0 29 1122 1168 -159 1200 161 1232 -49 1264 51 234 256 98 8 4576 8 'softBreakButton' 1296 8 'code' 592 8 'hardBreakButton' 3920 8 'busySeconds' 0 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore) 0 0 1 0 8 2000703696 754 202 208 98 3 818 848 98 2 882 2559 21 882 801 601 416 818 992 98 1 8 'Executing GemStone/Smalltalk Code...' 416 818 8 #updateMenuBar 1744 416 1042 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 5 1296 592 4576 4240 3920 1104 0 27 )! !
!WaitOnGemStoneDialog class categoriesFor: #gciSession:message:havingWaited:!public! !
!WaitOnGemStoneDialog class categoriesFor: #icon!public! !
!WaitOnGemStoneDialog class categoriesFor: #resource_Default_view!public!resources-views! !

