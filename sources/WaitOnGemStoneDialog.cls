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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.LayeredDialogView) 98 30 0 0 98 2 26214401 1179649 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 167 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 234 240 98 10 410 8 ##(Smalltalk.PushButton) 98 20 0 416 98 2 8 1140924416 1 592 0 0 0 7 0 0 0 592 0 8 4294904777 1180998 4 ##(Smalltalk.CommandDescription) 8 #sendHardBreak 8 'Hard Break' 1 1 0 0 32 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 3 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point) 449 473 882 161 51 592 818 8 #isEnabled: 98 1 32 592 818 8 #text: 98 1 8 'Hard Break' 592 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 224 0 0 0 236 0 0 0 48 1 0 0 5 1 0 0] 98 0 882 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) 8 #fixedParentRight -319 1154 8 #fixedViewLeft 161 1154 8 #fixedParentBottom -49 1154 8 #fixedViewTop 51 410 8 ##(Smalltalk.ScintillaView) 98 50 0 416 98 2 8 1176571972 1025 1296 721990 2 ##(Smalltalk.ValueHolder) 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 482 8 4278190080 0 7 0 0 0 1296 0 8 4294904307 852486 ##(Smalltalk.NullConverter) 0 0 11 0 234 256 98 44 8 #special 1182790 1 ##(Smalltalk.ScintillaTextStyle) 25 0 0 1 0 0 0 0 1552 0 0 0 8 #self 1570 15 196934 1 ##(Smalltalk.RGB) 16646145 0 1 0 0 0 0 1600 0 0 0 8 #keywordSend 1570 27 786694 ##(Smalltalk.IndexedColor) 33554437 0 1 0 0 0 0 1664 0 0 0 8 #indentGuide 1570 75 1698 33554447 0 1 0 0 0 0 1728 0 0 0 8 #global 1570 21 0 0 1 0 0 0 0 1776 0 0 0 8 #specialSelector 1570 33 1648 0 1 0 0 0 0 1808 0 0 0 8 #symbol 1570 9 1698 33554443 0 1 0 0 0 0 1840 0 0 0 8 #super 1570 17 1648 0 1 0 0 0 0 1888 0 0 0 8 #binary 1570 11 1698 33554433 0 1 0 0 0 0 1920 0 0 0 8 #braceMismatch 1570 71 1698 33554459 0 1 0 0 0 0 1968 0 0 0 8 #callTip 1570 77 1698 33554457 1698 33554471 1 0 0 0 0 2016 0 0 0 8 #normal 1570 1 0 0 1 0 0 0 0 2080 0 0 0 8 #assignment 1570 29 0 0 1 0 0 0 0 2112 0 0 0 8 #number 1570 5 1634 16711169 0 1 0 0 0 0 2144 0 0 0 8 #nil 1570 19 1648 0 1 0 0 0 0 2192 0 0 0 8 #return 1570 23 1634 321 0 1 0 0 0 0 2224 0 0 0 8 #lineNumber 1570 67 0 0 1 0 0 0 0 2272 0 0 0 8 #string 1570 3 1634 16646399 0 129 0 0 0 0 2304 0 0 0 8 #comment 1570 7 1634 65025 0 1 0 0 0 0 2352 0 0 0 8 #boolean 1570 13 1648 0 1 0 0 0 0 2400 0 0 0 8 #braceHighlight 1570 69 1698 33554465 0 1 0 0 0 0 2432 0 0 0 8 #character 1570 31 1634 16646399 0 1 0 0 0 0 2480 0 0 0 98 40 2096 2320 2160 2368 1856 1936 2416 1616 1904 2208 1792 2240 1584 1680 2128 2496 1824 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2288 2448 1984 0 1744 0 0 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 0 0 32 202 208 98 0 234 256 98 2 8 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 0 0 0 1296 8 #circle 0 0 0 0 0 0 202 208 2592 0 63 9215 0 0 0 0 1760 0 0 65 0 0 234 256 98 6 8 #specialCharacter 8 '()[]<>' 8 #literalBytes 8 '[]' 8 #literalArray 8 '()' 8 '' 3 234 256 98 4 8 #container 234 256 98 2 2080 1570 1 0 0 1 0 0 0 0 2080 0 0 0 8 #smalltalk 1520 0 0 0 0 1 0 234 256 98 6 8 'indicator0' 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 1296 65025 3 32 0 0 0 0 0 0 0 8 'indicator1' 3026 3 1296 33423361 5 32 0 0 0 0 0 0 0 8 'indicator2' 3026 5 1296 511 1 32 0 0 0 0 0 0 0 0 0 234 256 98 6 8 #Notification 1570 1029 0 1634 31185883 1 0 0 0 0 3152 0 0 0 8 #Error 1570 1031 1634 335 1634 31317503 1 0 0 0 0 3200 0 0 0 8 #Warning 1570 1027 1634 60239 1634 31326207 1 0 0 0 0 3264 0 0 0 0 754 202 208 98 7 818 848 98 2 882 1 1 882 769 473 1296 818 8 #modificationEventMask: 98 1 9215 1296 818 8 #caretPeriod: 98 1 1061 1296 818 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin) 1 1296 1 3 32 1 15 3602 3 1296 33 1 16 67108863 15 3602 5 1296 1 1 16 -67108863 15 1296 818 8 #markers: 98 1 2704 1296 818 8 #tabWidth: 98 1 9 1296 818 8 #setLexerLanguage: 98 1 2960 1296 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 128 1 0 0 236 0 0 0] 98 0 1104 0 29 1122 1154 8 #fixedParentLeft 1 1168 1 1154 8 #fixedParentTop 1 1232 -49 410 8 ##(Smalltalk.TextEdit) 98 16 0 416 98 2 8 1140916354 1025 3936 0 482 8 4278190080 0 7 0 0 0 3936 0 8 4294904281 1490 0 0 3 754 202 208 98 1 818 848 98 2 882 225 477 882 81 45 3936 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 112 0 0 0 238 0 0 0 152 0 0 0 4 1 0 0] 98 0 1104 0 27 1122 3872 225 1200 81 1232 -45 1264 45 410 8 ##(Smalltalk.StaticText) 98 16 0 416 98 2 8 1140850944 1 4256 0 0 0 7 0 0 0 4256 0 8 4294904807 1490 0 0 0 754 202 208 98 2 818 848 98 2 882 11 477 882 211 45 4256 818 992 98 1 8 'Busy for seconds:' 4256 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 238 0 0 0 110 0 0 0 4 1 0 0] 98 0 1104 0 27 1122 3872 11 1200 211 1232 -45 1264 45 410 608 98 20 0 416 98 2 8 1140924416 1 4592 0 0 0 7 0 0 0 4592 0 8 4294904777 690 8 #sendSoftBreak 8 'Soft Break' 1 1 0 0 32 0 0 0 754 202 208 98 3 818 848 98 2 882 609 473 882 161 51 4592 818 944 98 1 32 4592 818 992 98 1 8 'Soft Break' 4592 1042 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 48 1 0 0 236 0 0 0 128 1 0 0 5 1 0 0] 98 0 1104 0 29 1122 1168 -159 1200 161 1232 -49 1264 51 234 256 98 8 1296 8 'code' 4592 8 'softBreakButton' 592 8 'hardBreakButton' 3936 8 'busySeconds' 0 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore) 0 0 1 0 8 1999392976 754 202 208 98 3 818 848 98 2 882 2559 21 882 801 601 416 818 992 98 1 8 'Executing GemStone/Smalltalk Code...' 416 818 8 #updateMenuBar 2592 416 1042 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 143 6 0 0 54 1 0 0] 98 5 1296 592 4592 4256 3936 1104 0 27 )! !
!WaitOnGemStoneDialog class categoriesFor: #gciSession:message:havingWaited:!public! !
!WaitOnGemStoneDialog class categoriesFor: #icon!public! !
!WaitOnGemStoneDialog class categoriesFor: #resource_Default_view!public!resources-views! !

