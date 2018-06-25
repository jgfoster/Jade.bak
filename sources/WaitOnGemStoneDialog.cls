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
	codePresenter 			:= self add: TextPresenter new name: 'code'.
	busySecondsPresenter 	:= self add: TextPresenter new name: 'busySeconds'.
!

gciSession: aGciSession

	gciSession := aGciSession.
!

message: aString

	codePresenter value: aString asUtf8String.
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.LayeredDialogView) 34 30 nil nil 8 #(13107200 589824) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 167 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 10 410 ##(Smalltalk.ScintillaView) 34 50 nil 416 34 2 8 1176571972 1025 576 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 466 8 4278190080 nil 7 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 nil 576 nil 504331129 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 914 77 786694 ##(Smalltalk.IndexedColor) 33554457 962 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 928 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 944 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 8 #() 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 576 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 1072 nil 170 176 1072 9215 nil nil nil nil 962 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #literalArray 8 '()' #specialCharacter 8 '()[]<>' 8 '' 3 170 192 34 4 #container 170 192 34 2 #normal 928 #smalltalk 170 192 34 44 #normal 914 1 nil nil 1 nil nil nil nil #normal nil nil nil #super 914 17 196934 1 ##(Smalltalk.RGB) 16646145 nil 1 nil nil nil nil #super nil nil nil #number 914 5 1426 16711169 nil 1 nil nil nil nil #number nil nil nil #lineNumber 914 67 nil nil 1 nil nil nil nil #lineNumber nil nil nil #braceMismatch 914 71 962 33554459 nil 1 nil nil nil nil #braceMismatch nil nil nil #symbol 914 9 962 33554443 nil 1 nil nil nil nil #symbol nil nil nil #return 914 23 1426 321 nil 1 nil nil nil nil #return nil nil nil #binary 914 11 962 33554433 nil 1 nil nil nil nil #binary nil nil nil #specialSelector 914 33 1440 nil 1 nil nil nil nil #specialSelector nil nil nil #string 914 3 1426 16646399 nil 129 nil nil nil nil #string nil nil nil #keywordSend 914 27 962 33554437 nil 1 nil nil nil nil #keywordSend nil nil nil #character 914 31 1426 16646399 nil 1 nil nil nil nil #character nil nil nil #assignment 914 29 nil nil 1 nil nil nil nil #assignment nil nil nil #self 914 15 1440 nil 1 nil nil nil nil #self nil nil nil #special 914 25 nil nil 1 nil nil nil nil #special nil nil nil #global 914 21 nil nil 1 nil nil nil nil #global nil nil nil #callTip 914 77 976 992 1 nil nil nil nil #callTip nil nil nil #nil 914 19 1440 nil 1 nil nil nil nil #nil nil nil nil #braceHighlight 914 69 962 33554465 nil 1 nil nil nil nil #braceHighlight nil nil nil #indentGuide 914 75 1184 nil 1 nil nil nil nil #indentGuide nil nil nil #comment 914 7 1426 65025 nil 1 nil nil nil nil #comment nil nil nil #boolean 914 13 1440 nil 1 nil nil nil nil #boolean nil nil nil nil nil nil nil 1 nil 170 192 34 6 8 'indicator1' 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 3 576 33423361 5 false nil nil nil nil nil nil nil 8 'indicator0' 1986 1 576 65025 3 false nil nil nil nil nil nil nil 8 'indicator2' 1986 5 576 511 1 false nil nil nil nil nil nil nil nil nil 170 192 34 6 #Warning 914 1027 1426 60239 1426 31326207 1 nil nil nil nil #Warning nil nil nil #Error 914 1031 1426 335 1426 31317503 1 nil nil nil nil #Error nil nil nil #Notification 914 1029 nil 1426 31185883 1 nil nil nil nil #Notification nil nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 9 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 818 1 1 818 769 473 576 2306 #textLimit: 8 #(1073741823) 576 2306 #modificationEventMask: 8 #(4607) 576 2306 #caretPeriod: 8 #(530) 576 2306 #sciSetEOLMode: 8 #(2) 576 2306 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 576 1 3 nil nil nil nil 2562 3 576 33 nil 3 67108863 nil nil 2562 5 576 1 nil 3 -67108863 nil nil 576 2306 #tabWidth: 8 #(4) 576 2306 #maxCompletionListHeight: 8 #(9) 576 2306 #sciSetCodePage: 8 #(65001) 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 236 0 0 0] 8 #() 818 193 193 nil 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2834 #fixedParentRight 1 2834 #fixedParentTop 1 2834 #fixedParentBottom -49 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 2912 nil nil nil 7 nil nil nil 2912 nil 8 1795593280 1180998 4 ##(Smalltalk.CommandDescription) #sendSoftBreak 8 'Soft Break' 1 1 nil nil false nil nil nil 2242 138 144 34 3 2306 #createAt:extent: 34 2 818 609 473 818 161 51 2912 2306 #isEnabled: 8 #(false) 2912 2306 #text: 34 1 8 'Soft Break' 2912 2722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 48 1 0 0 236 0 0 0 128 1 0 0 5 1 0 0] 8 #() 2784 nil 29 2802 2864 -159 2834 #fixedViewLeft 161 2896 -49 2834 #fixedViewTop 51 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916354 1025 3328 nil 466 8 4278190080 nil 7 nil nil nil 3328 nil 8 1795431936 850 nil nil 3 2242 138 144 34 1 2306 #createAt:extent: 34 2 818 225 477 818 81 45 3328 2722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 112 0 0 0 238 0 0 0 152 0 0 0 4 1 0 0] 8 #() 2784 nil 27 2802 2848 225 3296 81 2896 -45 3312 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 3632 nil nil nil 7 nil nil nil 3632 nil 8 1795590816 850 nil nil nil 2242 138 144 34 2 2306 #createAt:extent: 34 2 818 11 477 818 211 45 3632 2306 #text: 34 1 8 'Busy for seconds:' 3632 2722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 238 0 0 0 110 0 0 0 4 1 0 0] 8 #() 2784 nil 27 2802 2848 11 3296 211 2896 -45 3312 45 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 3952 nil nil nil 7 nil nil nil 3952 nil 8 1795593280 2994 #sendHardBreak 8 'Hard Break' 1 1 nil nil false nil nil nil 2242 138 144 34 3 2306 #createAt:extent: 34 2 818 449 473 818 161 51 3952 2306 #isEnabled: 8 #(false) 3952 2306 #text: 34 1 8 'Hard Break' 3952 2722 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 224 0 0 0 236 0 0 0 48 1 0 0 5 1 0 0] 8 #() 2784 nil 29 2802 2864 -319 3296 161 2896 -49 3312 51 170 192 34 8 576 8 'code' 3952 8 'hardBreakButton' 2912 8 'softBreakButton' 3328 8 'busySeconds' nil nil nil nil nil 3 nil nil nil nil 1 nil nil 590598 ##(Smalltalk.Semaphore) nil nil 1 nil 8 1998149120 2242 138 144 34 3 2306 #createAt:extent: 34 2 818 2879 21 818 801 601 416 2306 #text: 34 1 8 'Executing GemStone/Smalltalk Code...' 416 2306 #updateMenuBar 1072 416 2722 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 47 7 0 0 54 1 0 0] 34 5 576 3952 2912 3632 3328 2784 nil 27 )! !
!WaitOnGemStoneDialog class categoriesFor: #gciSession:message:havingWaited:!public! !
!WaitOnGemStoneDialog class categoriesFor: #icon!public! !
!WaitOnGemStoneDialog class categoriesFor: #resource_Default_view!public!resources-views! !

