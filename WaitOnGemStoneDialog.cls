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

	^Icon fromFile: 'icons\GS32x32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.LayeredDialogView)  98 30 0 0 98 2 26214401 1179649 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 10 410 8 ##(Smalltalk.ScintillaView)  98 46 0 416 98 2 8 1176571972 1025 592 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 482 8 4278190080 0 7 0 0 0 592 0 8 4294902267 852486 ##(Smalltalk.NullConverter)  0 0 11 0 234 256 98 42 8 #specialSelector 1182726 ##(Smalltalk.ScintillaTextStyle)  33 196934 1 ##(Smalltalk.RGB)  16646145 0 1 0 0 0 0 848 0 0 0 8 #lineNumber 866 67 0 0 1 0 0 0 0 928 0 0 0 8 #global 866 21 0 0 1 0 0 0 0 960 0 0 0 8 #normal 866 1 0 0 1 0 0 0 0 992 0 0 0 8 #boolean 866 13 912 0 1 0 0 0 0 1024 0 0 0 8 #special 866 25 0 0 1 0 0 0 0 1056 0 0 0 8 #number 866 5 898 16711169 0 1 0 0 0 0 1088 0 0 0 8 #nil 866 19 912 0 1 0 0 0 0 1136 0 0 0 8 #character 866 31 898 16646399 0 1 0 0 0 0 1168 0 0 0 8 #indentGuide 866 75 786694 ##(Smalltalk.IndexedColor)  33554447 0 1 0 0 0 0 1216 0 0 0 8 #braceHighlight 866 69 1250 33554465 0 1 0 0 0 0 1280 0 0 0 8 #string 866 3 898 16646399 0 129 0 0 0 0 1328 0 0 0 8 #symbol 866 9 1250 33554443 0 1 0 0 0 0 1376 0 0 0 8 #super 866 17 912 0 1 0 0 0 0 1424 0 0 0 8 #comment 866 7 898 65025 0 1 0 0 0 0 1456 0 0 0 8 #binary 866 11 1250 33554433 0 1 0 0 0 0 1504 0 0 0 8 #assignment 866 29 0 0 1 0 0 0 0 1552 0 0 0 8 #keywordSend 866 27 1250 33554437 0 1 0 0 0 0 1584 0 0 0 8 #return 866 23 898 321 0 1 0 0 0 0 1632 0 0 0 8 #braceMismatch 866 71 1250 33554459 0 1 0 0 0 0 1680 0 0 0 8 #self 866 15 912 0 1 0 0 0 0 1728 0 0 0 98 40 1008 1344 1104 1472 1392 1520 1040 1744 1440 1152 976 1648 1072 1600 1568 1184 880 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 944 1296 1696 0 1232 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  992 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 1536 1250 33554471 592 8 #circle 202 208 98 0 0 63 9215 0 0 0 0 1264 0 0 0 0 0 0 8 '' 3 234 256 98 4 8 #smalltalk 816 8 #container 234 256 98 2 992 866 1 0 0 1 0 0 0 0 992 0 0 0 0 0 0 0 1 0 234 256 98 6 8 'indicator2' 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  5 592 511 1 32 0 0 8 'indicator1' 2130 3 592 33423361 5 32 0 0 8 'indicator0' 2130 1 592 65025 3 32 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 9 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 2354 789 495 592 2290 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 592 2290 8 #isTextModified: 98 1 32 592 2290 8 #modificationEventMask: 98 1 9215 592 2290 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 592 1 3 32 1 2642 3 592 33 1 16 67108863 2642 5 592 1 1 16 -67108863 592 2290 8 #indentationGuides: 98 1 0 592 2290 8 #tabIndents: 98 1 16 592 2290 8 #tabWidth: 98 1 9 592 2290 8 #setLexerLanguage: 98 1 2000 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 138 1 0 0 247 0 0 0] 98 0 2354 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 3010 8 #fixedParentRight 1 3010 8 #fixedParentTop 1 3010 8 #fixedParentBottom -49 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 3152 0 0 0 7 0 0 0 3152 0 8 4294903167 1180998 4 ##(Smalltalk.CommandDescription)  8 #sendHardBreak 8 'Hard Break' 1 1 0 0 32 0 0 0 2226 202 208 98 3 2290 2320 98 2 2354 469 495 2354 161 51 3152 2290 8 #isEnabled: 98 1 32 3152 2290 8 #text: 98 1 8 'Hard Break' 3152 2898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 234 0 0 0 247 0 0 0 58 1 0 0 16 1 0 0] 98 0 2960 0 29 2978 3056 -319 3010 8 #fixedViewLeft 161 3120 -49 3010 8 #fixedViewTop 51 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 3664 0 0 0 7 0 0 0 3664 0 8 4294902319 786 0 0 0 2226 202 208 98 2 2290 2320 98 2 2354 11 501 2354 211 31 3664 2290 3488 98 1 8 'Busy for seconds:' 3664 2898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 250 0 0 0 110 0 0 0 9 1 0 0] 98 0 2960 0 27 2978 3024 11 3600 211 3088 501 3632 31 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916354 1025 4000 0 482 8 4278190080 0 7 0 0 0 4000 0 8 4294903767 786 0 0 3 2226 202 208 98 3 2290 2320 98 2 2354 225 493 2354 81 41 4000 2290 2416 98 1 2450 3 1 3 4000 2290 2496 98 1 32 4000 2898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 112 0 0 0 246 0 0 0 152 0 0 0 10 1 0 0] 98 0 2960 0 27 2978 3024 225 3600 81 3088 493 3632 41 410 3168 98 20 0 416 98 2 8 1140924416 1 4400 0 0 0 7 0 0 0 4400 0 8 4294903167 3250 8 #sendSoftBreak 8 'Soft Break' 1 1 0 0 32 0 0 0 2226 202 208 98 3 2290 2320 98 2 2354 629 495 2354 161 51 4400 2290 3440 98 1 32 4400 2290 3488 98 1 8 'Soft Break' 4400 2898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 58 1 0 0 247 0 0 0 138 1 0 0 16 1 0 0] 98 0 2960 0 29 2978 3056 -159 3600 161 3120 -49 3632 51 234 256 98 8 4000 8 'busySeconds' 4400 8 'softBreakButton' 3152 8 'hardBreakButton' 592 8 'code' 0 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 1998164183 2226 202 208 98 3 2290 2320 98 2 2354 6239 21 2354 801 601 416 2290 3488 98 1 8 'Executing GemStone/Smalltalk Code...' 416 2290 8 #updateMenuBar 1936 416 2898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 47 12 0 0 10 0 0 0 191 13 0 0 54 1 0 0] 98 5 592 3152 4400 3664 4000 2960 0 27 )! !
!WaitOnGemStoneDialog class categoriesFor: #gciSession:message:havingWaited:!public! !
!WaitOnGemStoneDialog class categoriesFor: #icon!public! !
!WaitOnGemStoneDialog class categoriesFor: #resource_Default_view!public!resources-views! !

