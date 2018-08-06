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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.LayeredDialogView) 34 30 0 0 8 #(13107200 589824) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 167 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 10 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 1 576 0 0 0 7 0 0 0 576 0 8 4294902971 852486 ##(Smalltalk.NullConverter) 0 0 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 11 477 802 211 45 576 754 #text: 34 1 8 'Busy for seconds:' 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 238 0 0 0 110 0 0 0 4 1 0 0] 8 #() 802 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 11 1010 #fixedViewLeft 211 1010 #fixedParentBottom -45 1010 #fixedViewTop 45 410 ##(Smalltalk.PushButton) 34 20 0 416 34 2 8 1140924416 1 1088 0 0 0 7 0 0 0 1088 0 8 4294903089 1180998 4 ##(Smalltalk.CommandDescription) #sendHardBreak 8 'Hard Break' 1 1 0 0 false 0 0 0 690 138 144 34 3 754 #createAt:extent: 34 2 802 449 473 802 161 51 1088 754 #isEnabled: 8 #(false) 1088 754 #text: 34 1 8 'Hard Break' 1088 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 224 0 0 0 236 0 0 0 48 1 0 0 5 1 0 0] 8 #() 960 0 29 978 1010 #fixedParentRight -319 1040 161 1056 -49 1072 51 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916354 1025 1488 0 466 8 4278190080 0 7 0 0 0 1488 0 8 4294904453 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 802 225 477 802 81 45 1488 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 112 0 0 0 238 0 0 0 152 0 0 0 4 1 0 0] 8 #() 960 0 27 978 1024 225 1040 81 1056 -45 1072 45 410 ##(Smalltalk.ScintillaView) 34 50 0 416 34 2 8 1176571972 1025 1792 721990 2 ##(Smalltalk.ValueHolder) 0 false 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 466 8 4278190080 0 7 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 802 193 193 0 1792 0 8 4294905729 658 0 0 11 0 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 0 0 1 0 0 0 0 #normal 0 0 0 #callTip 2114 77 786694 ##(Smalltalk.IndexedColor) 33554457 2162 33554471 1 0 0 0 0 #callTip 0 0 0 34 256 2128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 0 0 false 138 144 8 #() 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 0 0 0 1792 #circle 0 0 0 0 0 0 138 ##(Smalltalk.IdentitySet) 2272 0 170 176 2272 9215 0 0 0 0 2162 33554447 0 0 65 0 0 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 4 #smalltalk 170 192 34 44 #global 2114 21 0 0 1 0 0 0 0 #global 0 0 0 #number 2114 5 196934 1 ##(Smalltalk.RGB) 16711169 0 1 0 0 0 0 #number 0 0 0 #return 2114 23 2594 321 0 1 0 0 0 0 #return 0 0 0 #normal 2114 1 0 0 1 0 0 0 0 #normal 0 0 0 #callTip 2114 77 2176 2192 1 0 0 0 0 #callTip 0 0 0 #nil 2114 19 2594 16646145 0 1 0 0 0 0 #nil 0 0 0 #character 2114 31 2594 16646399 0 1 0 0 0 0 #character 0 0 0 #braceHighlight 2114 69 2162 33554465 0 1 0 0 0 0 #braceHighlight 0 0 0 #lineNumber 2114 67 0 0 1 0 0 0 0 #lineNumber 0 0 0 #string 2114 3 2594 16646399 0 129 0 0 0 0 #string 0 0 0 #assignment 2114 29 0 0 1 0 0 0 0 #assignment 0 0 0 #boolean 2114 13 2704 0 1 0 0 0 0 #boolean 0 0 0 #super 2114 17 2704 0 1 0 0 0 0 #super 0 0 0 #keywordSend 2114 27 2162 33554437 0 1 0 0 0 0 #keywordSend 0 0 0 #braceMismatch 2114 71 2162 33554459 0 1 0 0 0 0 #braceMismatch 0 0 0 #comment 2114 7 2594 65025 0 1 0 0 0 0 #comment 0 0 0 #symbol 2114 9 2162 33554443 0 1 0 0 0 0 #symbol 0 0 0 #binary 2114 11 2162 33554433 0 1 0 0 0 0 #binary 0 0 0 #self 2114 15 2704 0 1 0 0 0 0 #self 0 0 0 #special 2114 25 0 0 1 0 0 0 0 #special 0 0 0 #indentGuide 2114 75 2384 0 1 0 0 0 0 #indentGuide 0 0 0 #specialSelector 2114 33 2704 0 1 0 0 0 0 #specialSelector 0 0 0 #container 170 192 34 2 #normal 2128 0 0 0 0 1 0 170 192 34 6 8 'indicator1' 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 3 1792 33423361 5 false 0 0 0 0 0 0 0 8 'indicator0' 3186 1 1792 65025 3 false 0 0 0 0 0 0 0 8 'indicator2' 3186 5 1792 511 1 false 0 0 0 0 0 0 0 0 0 170 192 34 6 #Notification 2114 1029 0 2594 31185883 1 0 0 0 0 #Notification 0 0 0 #Warning 2114 1027 2594 60239 2594 31326207 1 0 0 0 0 #Warning 0 0 0 #Error 2114 1031 2594 335 2594 31317503 1 0 0 0 0 #Error 0 0 0 0 690 138 144 34 5 754 #createAt:extent: 34 2 802 1 1 802 769 473 1792 754 #modificationEventMask: 8 #(4607) 1792 754 #caretPeriod: 8 #(530) 1792 754 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 1792 1 3 0 0 0 0 3666 3 1792 33 0 3 67108863 0 0 3666 5 1792 1 0 3 -67108863 0 0 1792 754 #tabWidth: 8 #(4) 1792 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 236 0 0 0] 8 #() 960 0 29 978 1024 1 1472 1 1010 #fixedParentTop 1 1056 -49 410 ##(Smalltalk.PushButton) 34 20 0 416 34 2 8 1140924416 1 3840 0 0 0 7 0 0 0 3840 0 8 4294903089 1170 #sendSoftBreak 8 'Soft Break' 1 1 0 0 false 0 0 0 690 138 144 34 3 754 #createAt:extent: 34 2 802 609 473 802 161 51 3840 754 #isEnabled: 8 #(false) 3840 754 #text: 34 1 8 'Soft Break' 3840 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 48 1 0 0 236 0 0 0 128 1 0 0 5 1 0 0] 8 #() 960 0 29 978 1472 -159 1040 161 1056 -49 1072 51 170 192 34 8 1488 8 'busySeconds' 3840 8 'softBreakButton' 1792 8 'code' 1088 8 'hardBreakButton' 0 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore) 0 0 1 0 8 2004772080 690 138 144 34 3 754 #createAt:extent: 34 2 802 5119 21 802 801 601 416 754 #text: 34 1 8 'Executing GemStone/Smalltalk Code...' 416 754 #updateMenuBar 2272 416 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 143 11 0 0 54 1 0 0] 34 5 1792 1088 3840 576 1488 960 0 27 )! !
!WaitOnGemStoneDialog class categoriesFor: #gciSession:message:havingWaited:!public! !
!WaitOnGemStoneDialog class categoriesFor: #icon!public! !
!WaitOnGemStoneDialog class categoriesFor: #resource_Default_view!public!resources-views! !

