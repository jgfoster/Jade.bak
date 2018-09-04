"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #MySessionPresenter
	instanceVariableNames: 'configurationReportPresenter gciSessionIdPresenter gciVersionPresenter gemHostPresenter gemTypePresenter logPathPresenter netPortPresenter netTaskPresenter stoneHostPresenter stoneNamePresenter stoneSerialPresenter stoneSessionIDPresenter userIDPresenter versionReportPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MySessionPresenter guid: (GUID fromString: '{b36a1c7d-3698-4294-b47d-101c34edf22d}')!
MySessionPresenter comment: ''!
!MySessionPresenter categoriesForClass!Unclassified! !
!MySessionPresenter methodsFor!

abortTransaction

	parentPresenter abortTransaction.
!

commitTransaction

	parentPresenter commitTransaction.
!

configDetailsFor: aPresenter

	| line string |
	line := aPresenter selection.	string := gciSession serverPerform: #'descriptionOfConfigOption:' with: line first.
	string := string , 'System gemConfigurationAt: #' , (line at: 1) , ' "' , (line at: 2) , '"'.
	(JadeWorkspace showOnSession: gciSession)
		model: string;
		isModified: false;
		yourself.!

createComponents

	super createComponents.
	configurationReportPresenter	:= self add: ListPresenter	new name: 'configurationReport'.
	gciSessionIdPresenter		:= self add: TextPresenter	new name: 'gciSessionId'.
	gciVersionPresenter			:= self add: TextPresenter	new name: 'gciVersion'.
	gemHostPresenter			:= self add: TextPresenter	new name: 'gemHost'.
	gemTypePresenter			:= self add: TextPresenter	new name: 'gemType'.
	logPathPresenter			:= self add: TextPresenter new name: 'logPath'.
	netPortPresenter			:= self add: TextPresenter	new name: 'netPort'.
	netTaskPresenter			:= self add: TextPresenter	new name: 'netTask'.
	stoneHostPresenter			:= self add: TextPresenter	new name: 'stoneHost'.
	stoneNamePresenter			:= self add: TextPresenter	new name: 'stoneName'.
	stoneSerialPresenter			:= self add: TextPresenter	new name: 'stoneSerial'.
	stoneSessionIDPresenter		:= self add: TextPresenter	new name: 'stoneSessionID'.
	userIDPresenter				:= self add: TextPresenter	new name: 'userID'.
	versionReportPresenter		:= self add: ListPresenter	new name: 'versionReport'.
!

createSchematicWiring

	super createSchematicWiring.
	configurationReportPresenter 	when: #'actionPerformed' send: #'configDetailsFor:' 	to: self with: configurationReportPresenter.
	versionReportPresenter 		when: #'actionPerformed' send: #'versionDetailsFor:' 	to: self with: versionReportPresenter.
!

fillSessionInfo

	| line list stream |
	gciVersionPresenter 	value: gciSession libraryVersion.
	userIDPresenter		value: gciSession userID.
	stoneHostPresenter	value: gciSession stoneHost.
	stoneNamePresenter	value: gciSession stoneName.
	gciSession isLinkedGem ifTrue: [
		gemTypePresenter	value: 'linked'.
	] ifFalse: [
		gemTypePresenter	value: 'RPC'.
		gemHostPresenter	value: gciSession gemHost.
		netPortPresenter	value: gciSession netPort.
		netTaskPresenter	value: gciSession netTask.
	].
	stoneSessionIDPresenter	value: gciSession stoneSessionID.
	stoneSerialPresenter		value: gciSession stoneSerial.
	gciSessionIdPresenter	value: gciSession gciSessionId.

	stream := ReadStream on: (gciSession serverPerform: #'mySessionInfo').
	logPathPresenter value: stream nextLine.
	list := OrderedCollection new.
	[
		(line := stream nextLine) = '%'.
	] whileFalse: [
		list add: (line subStrings: Character tab).
	].
	versionReportPresenter list: list.
	list := OrderedCollection new.
	[
		(line := stream nextLine) = '%'.
	] whileFalse: [
		list add: (line subStrings: Character tab).
	].
	configurationReportPresenter list: list.!

hardBreak

	gciSession hardBreak.
!

softBreak

	gciSession softBreak.
!

versionDetailsFor: aPresenter

	| line string |
	line := aPresenter selection.	string := 'System gemVersionAt: #' , (line at: 1) , ' "' , (line at: 2) , '"'.
	(JadeWorkspace showOnSession: gciSession)
		model: string;
		isModified: false;
		yourself.! !
!MySessionPresenter categoriesFor: #abortTransaction!public! !
!MySessionPresenter categoriesFor: #commitTransaction!public! !
!MySessionPresenter categoriesFor: #configDetailsFor:!public! !
!MySessionPresenter categoriesFor: #createComponents!public! !
!MySessionPresenter categoriesFor: #createSchematicWiring!public! !
!MySessionPresenter categoriesFor: #fillSessionInfo!public!updating! !
!MySessionPresenter categoriesFor: #hardBreak!public! !
!MySessionPresenter categoriesFor: #softBreak!public! !
!MySessionPresenter categoriesFor: #versionDetailsFor:!public! !

!MySessionPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil 328198 ##(Smalltalk.Point) 1153 621 5 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 60 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 576 nil nil nil 5 nil nil nil 576 nil 8 1716360256 1180998 4 ##(Smalltalk.CommandDescription) #commitTransaction 8 'Commit' 1 1 nil nil false nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 482 133 571 482 121 51 576 770 #isEnabled: 8 #(false) 576 770 #text: 34 1 8 'Commit' 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 66 0 0 0 29 1 0 0 126 0 0 0 54 1 0 0] 8 #() 482 193 193 nil 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedPreviousRight 11 1042 #fixedViewLeft 121 1042 #fixedPreviousTop 1 1042 #fixedViewTop 51 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 1120 nil nil nil 5 nil nil nil 1120 nil 8 1716357792 852486 ##(Smalltalk.NullConverter) nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 783 49 482 161 45 1120 770 #text: 34 1 8 'GCI Version:' 1120 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 135 1 0 0 24 0 0 0 215 1 0 0 46 0 0 0] 8 #() 992 nil 27 1010 1056 11 1072 161 1088 1 1104 45 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916352 1025 1456 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 nil nil nil 1456 nil 8 1716198912 1202 nil nil 3 706 138 144 34 1 770 #createAt:extent: 34 2 482 563 3 482 591 45 1456 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 1 0 0 1 0 0 0 64 2 0 0 23 0 0 0] 8 #() 992 nil 27 1010 1056 1 1072 591 1088 1 1104 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 1776 nil nil nil 5 nil nil nil 1776 nil 8 1716357792 1202 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 3 49 482 181 45 1776 770 #text: 34 1 8 'Stone Host:' 1776 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 24 0 0 0 91 0 0 0 46 0 0 0] 8 #() 992 nil 27 1010 1042 #fixedParentLeft 3 1072 181 1042 #fixedPreviousBottom 3 1104 45 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916354 1025 2128 nil 1536 nil 5 nil nil nil 2128 nil 8 1716198912 1202 nil nil 3 706 138 144 34 1 770 #createAt:extent: 34 2 482 943 141 482 211 45 2128 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 215 1 0 0 70 0 0 0 64 2 0 0 92 0 0 0] 8 #() 992 nil 27 1010 1056 1 1072 211 1088 1 1104 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 2400 nil nil nil 5 nil nil nil 2400 nil 8 1716357792 1202 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 783 95 482 161 45 2400 770 #text: 34 1 8 'GCI Session:' 2400 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 135 1 0 0 47 0 0 0 215 1 0 0 69 0 0 0] 8 #() 992 nil 27 1010 1056 11 1072 161 1088 1 1104 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 2720 nil nil nil 5 nil nil nil 2720 nil 8 1716357792 1202 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 403 141 482 161 45 2720 770 #text: 34 1 8 'Net Task:' 2720 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 201 0 0 0 70 0 0 0 25 1 0 0 92 0 0 0] 8 #() 992 nil 27 1010 1056 11 1072 161 1088 1 1104 45 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916354 1025 3040 nil 1536 nil 5 nil nil nil 3040 nil 8 1716198912 1202 nil nil 3 706 138 144 34 1 770 #createAt:extent: 34 2 482 943 95 482 211 45 3040 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 215 1 0 0 47 0 0 0 64 2 0 0 69 0 0 0] 8 #() 992 nil 27 1010 1056 1 1072 211 1088 1 1104 45 410 ##(Smalltalk.ListView) 34 30 nil 416 34 2 8 1409355853 1025 3312 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 1536 nil 5 nil nil nil 3312 nil 8 1716250736 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Version' 271 #left 3490 #first 8 #() ##(Smalltalk.SortedCollection) nil nil 3312 nil 1 nil nil 3602 8 'Value' 261 #left 3490 #last 3664 3490 #<= 3664 nil nil 3312 nil 1 nil nil #report 3424 nil 131169 nil 34 4 nil nil 482 1 1 nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 3 233 482 575 339 3312 770 #text: 34 1 8 'Version' 3312 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 116 0 0 0 32 1 0 0 29 1 0 0] 8 #() 992 nil 27 1010 2096 3 1042 #relativeParentWidth 524806 ##(Smalltalk.Fraction) 3 5 2112 3 1042 #fixedParentBottom -49 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916352 1025 4064 nil 1536 nil 5 nil nil nil 4064 nil 8 1716198912 1202 nil nil 3 706 138 144 34 1 770 #createAt:extent: 34 2 482 943 49 482 211 45 4064 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 215 1 0 0 24 0 0 0 64 2 0 0 46 0 0 0] 8 #() 992 nil 27 1010 1056 1 1072 211 1088 1 1104 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 4336 nil nil nil 5 nil nil nil 4336 nil 8 1716357792 1202 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 783 141 482 161 45 4336 770 #text: 34 1 8 'Stone Serial:
' 4336 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 135 1 0 0 70 0 0 0 215 1 0 0 92 0 0 0] 8 #() 992 nil 27 1010 1056 11 1072 161 1088 1 1104 45 410 ##(Smalltalk.ListView) 34 30 nil 416 34 2 8 1409355853 1025 4656 3378 138 144 3424 nil 3456 1536 nil 5 nil nil nil 4656 nil 8 1716250736 3490 #displayString 8 #() nil 3552 nil nil nil nil nil nil 138 144 34 2 3602 8 'Configuration' 381 #left 3490 #first 3664 ##(Smalltalk.SortedCollection) nil nil 4656 nil 1 nil nil 3602 8 'Value' 151 #right 3490 #last 3664 3490 #<= 3664 nil nil 4656 nil 1 nil nil #report 3424 nil 131169 nil 34 4 nil nil 482 1 1 nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 577 233 482 577 389 4656 770 #text: 34 1 8 'Configuration' 4656 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 32 1 0 0 116 0 0 0 64 2 0 0 54 1 0 0] 8 #() 992 nil 27 1010 1056 1 1042 #fixedParentRight 1 1088 1 4048 1 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916352 1025 5216 nil 1536 nil 5 nil nil nil 5216 nil 8 1716198912 1202 nil nil 3 706 138 144 34 1 770 #createAt:extent: 34 2 482 563 95 482 211 45 5216 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 1 0 0 47 0 0 0 130 1 0 0 69 0 0 0] 8 #() 992 nil 27 1010 1056 1 1072 211 1088 1 1104 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 65 5488 nil nil nil 5 nil nil nil 5488 nil 8 1716357792 1202 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 3 187 482 181 45 5488 770 #text: 34 1 8 'Log Path:' 5488 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 93 0 0 0 91 0 0 0 115 0 0 0] 8 #() 992 nil 27 1010 2096 3 1072 181 2112 3 1104 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 5808 nil nil nil 5 nil nil nil 5808 nil 8 1716357792 1202 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 3 141 482 181 45 5808 770 #text: 34 1 8 'Stone Session:' 5808 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 70 0 0 0 91 0 0 0 92 0 0 0] 8 #() 992 nil 27 1010 2096 3 1072 181 2112 3 1104 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 6128 nil nil nil 5 nil nil nil 6128 nil 8 1716357792 1202 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 3 95 482 181 45 6128 770 #text: 34 1 8 'Stone Name:' 6128 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 47 0 0 0 91 0 0 0 69 0 0 0] 8 #() 992 nil 27 1010 2096 3 1072 181 2112 3 1104 45 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 6448 nil nil nil 5 nil nil nil 6448 nil 8 1716360256 658 #softBreak 8 'Soft Break' 1 1 nil nil false nil nil nil 706 138 144 34 3 770 #createAt:extent: 34 2 482 263 571 482 151 51 6448 770 #isEnabled: 8 #(false) 6448 770 #text: 34 1 8 'Soft Break' 6448 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 131 0 0 0 29 1 0 0 206 0 0 0 54 1 0 0] 8 #() 992 nil 29 1010 1056 11 1072 151 1088 1 1104 51 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916352 1025 6816 nil 1536 nil 5 nil nil nil 6816 nil 8 1716198912 1202 nil nil 3 706 138 144 34 1 770 #createAt:extent: 34 2 482 183 95 482 211 45 6816 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 0 0 0 47 0 0 0 196 0 0 0 69 0 0 0] 8 #() 992 nil 27 1010 1056 1 1072 211 1088 1 1104 45 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 7088 nil nil nil 5 nil nil nil 7088 nil 8 1716360256 658 #hardBreak 8 'Hard Break' 1 1 nil nil false nil nil nil 706 138 144 34 3 770 #createAt:extent: 34 2 482 423 571 482 151 51 7088 770 #isEnabled: 8 #(false) 7088 770 #text: 34 1 8 'Hard Break' 7088 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 211 0 0 0 29 1 0 0 30 1 0 0 54 1 0 0] 8 #() 992 nil 29 1010 1056 11 1072 151 1088 1 1104 51 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916352 1025 7456 nil 1536 nil 5 nil nil nil 7456 nil 8 1716198912 1202 nil nil 3 706 138 144 34 1 770 #createAt:extent: 34 2 482 183 49 482 211 45 7456 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 0 0 0 24 0 0 0 196 0 0 0 46 0 0 0] 8 #() 992 nil 27 1010 1056 1 1072 211 1088 1 1104 45 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916352 1025 7728 nil 1536 nil 5 nil nil nil 7728 nil 8 1716198912 1202 nil nil 3 706 138 144 34 1 770 #createAt:extent: 34 2 482 183 3 482 211 45 7728 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 0 0 0 1 0 0 0 196 0 0 0 23 0 0 0] 8 #() 992 nil 27 1010 1056 1 1072 211 1088 1 1104 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 8000 nil nil nil 5 nil nil nil 8000 nil 8 1716357792 1202 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 403 49 482 161 45 8000 770 #text: 34 1 8 'Gem Type:' 8000 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 201 0 0 0 24 0 0 0 25 1 0 0 46 0 0 0] 8 #() 992 nil 27 1010 1056 11 1072 161 1088 1 1104 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 8320 nil nil nil 5 nil nil nil 8320 nil 8 1716357792 1202 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 403 3 482 161 45 8320 770 #text: 34 1 8 'Gem Host:' 8320 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 201 0 0 0 1 0 0 0 25 1 0 0 23 0 0 0] 8 #() 992 nil 27 1010 1056 11 1072 161 1088 1 1104 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 8640 nil nil nil 5 nil nil nil 8640 nil 8 1716357792 1202 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 3 3 482 181 45 8640 770 #text: 34 1 8 'User ID:' 8640 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 1 0 0 0 91 0 0 0 23 0 0 0] 8 #() 992 nil 27 1010 2096 3 1072 181 1042 #fixedParentTop 3 1104 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 8976 nil nil nil 5 nil nil nil 8976 nil 8 1716357792 1202 nil nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 482 403 95 482 161 45 8976 770 #text: 34 1 8 'Net Service:' 8976 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 201 0 0 0 47 0 0 0 25 1 0 0 69 0 0 0] 8 #() 992 nil 27 1010 1056 11 1072 161 1088 1 1104 45 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916352 1025 9296 nil 1536 nil 5 nil nil nil 9296 nil 8 1716198912 1202 nil nil 3 706 138 144 34 1 770 #createAt:extent: 34 2 482 563 141 482 211 45 9296 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 1 0 0 70 0 0 0 130 1 0 0 92 0 0 0] 8 #() 992 nil 27 1010 1056 1 1072 211 1088 1 1104 45 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916352 1025 9568 nil 1536 nil 5 nil nil nil 9568 nil 8 1716198912 1202 nil nil 3 706 138 144 34 1 770 #createAt:extent: 34 2 482 183 187 482 971 45 9568 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 0 0 0 93 0 0 0 64 2 0 0 115 0 0 0] 8 #() 992 nil 27 1010 1056 1 5200 1 1088 1 1104 45 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916354 1025 9840 nil 1536 nil 5 nil nil nil 9840 nil 8 1716198912 1202 nil nil 3 706 138 144 34 1 770 #createAt:extent: 34 2 482 183 141 482 211 45 9840 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 0 0 0 70 0 0 0 196 0 0 0 92 0 0 0] 8 #() 992 nil 27 1010 1056 1 1072 211 1088 1 1104 45 410 ##(Smalltalk.PushButton) 34 20 nil 416 34 2 8 1140924416 1 10112 nil nil nil 5 nil nil nil 10112 nil 8 1716360256 658 #abortTransaction 8 'Abort' 1 1 nil nil false nil nil nil 706 138 144 34 3 770 #createAt:extent: 34 2 482 3 571 482 121 51 10112 770 #isEnabled: 8 #(false) 10112 770 #text: 34 1 8 'Abort' 10112 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 29 1 0 0 61 0 0 0 54 1 0 0] 8 #() 992 nil 29 1010 2096 3 1072 121 4048 -49 1104 51 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916352 1025 10480 nil 1536 nil 5 nil nil nil 10480 nil 8 1716198912 1202 nil nil 3 706 138 144 34 1 770 #createAt:extent: 34 2 482 563 49 482 211 45 10480 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 1 0 0 24 0 0 0 130 1 0 0 46 0 0 0] 8 #() 992 nil 27 1010 1056 1 1072 211 1088 1 1104 45 170 192 34 28 4064 8 'gciVersion' 7456 8 'stoneHost' 5216 8 'netPort' 9840 8 'stoneSessionID' 2128 8 'stoneSerial' 1456 8 'gemHost' 9296 8 'netTask' 4656 8 'configurationReport' 3312 8 'versionReport' 3040 8 'gciSessionId' 10480 8 'gemType' 7728 8 'userID' 9568 8 'logPath' 6816 8 'stoneName' nil 706 138 144 34 1 770 #createAt:extent: 34 2 482 5119 21 482 1153 621 416 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 63 12 0 0 64 1 0 0] 34 30 8640 7728 8320 1456 1776 7456 8000 10480 1120 4064 6128 6816 8976 5216 2400 3040 5808 9840 2720 9296 4336 2128 5488 9568 3312 4656 10112 576 6448 7088 992 nil 27 )! !
!MySessionPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

