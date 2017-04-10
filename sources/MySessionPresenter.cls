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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 0 0 34 2 8 1409286144 131073 416 0 0 328198 ##(Smalltalk.Point) 1153 621 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 60 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 1 576 0 0 0 5 0 0 0 576 0 8 4294902755 852486 ##(Smalltalk.NullConverter) 0 0 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 482 3 141 482 181 45 576 754 #text: 34 1 8 'Stone Session:' 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 70 0 0 0 91 0 0 0 92 0 0 0] 8 #() 482 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 3 994 #fixedViewLeft 181 994 #fixedPreviousBottom 3 994 #fixedViewTop 45 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916352 1025 1072 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 5 0 0 0 1072 0 8 4294902793 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 482 183 95 482 211 45 1072 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 0 0 0 47 0 0 0 196 0 0 0 69 0 0 0] 8 #() 944 0 27 962 994 #fixedPreviousRight 1 1024 211 994 #fixedPreviousTop 1 1056 45 410 ##(Smalltalk.ListView) 34 30 0 416 34 2 8 1409355853 1025 1424 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 1138 8 4278190080 0 5 0 0 0 1424 0 8 4294903241 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Version' 271 #left 1634 #first 1536 ##(Smalltalk.SortedCollection) 0 0 1424 0 1 0 0 1746 8 'Value' 261 #left 1634 #last 1536 1634 #<= 1536 0 0 1424 0 1 0 0 #report 1536 0 131169 0 34 4 0 0 482 1 1 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 3 233 482 575 339 1424 754 #text: 34 1 8 'Version' 1424 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 116 0 0 0 32 1 0 0 29 1 0 0] 8 #() 944 0 27 962 1008 3 994 #relativeParentWidth 524806 ##(Smalltalk.Fraction) 3 5 1040 3 994 #fixedParentBottom -49 410 ##(Smalltalk.ListView) 34 30 0 416 34 2 8 1409355853 1025 2192 1490 138 144 1536 0 1568 1138 1600 0 5 0 0 0 2192 0 8 4294903241 1634 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1696 0 0 0 0 0 0 138 144 34 2 1746 8 'Configuration' 381 #left 1634 #first 1536 ##(Smalltalk.SortedCollection) 0 0 2192 0 1 0 0 1746 8 'Value' 151 #right 1634 #last 1536 1634 #<= 1536 0 0 2192 0 1 0 0 #report 1536 0 131169 0 34 4 0 0 482 1 1 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 577 233 482 577 389 2192 754 #text: 34 1 8 'Configuration' 2192 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 32 1 0 0 116 0 0 0 64 2 0 0 54 1 0 0] 8 #() 944 0 27 962 1392 1 994 #fixedParentRight 1 1408 1 2176 1 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 1 2768 0 0 0 5 0 0 0 2768 0 8 4294902755 658 0 0 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 783 141 482 161 45 2768 754 #text: 34 1 8 'Stone Serial:
' 2768 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 135 1 0 0 70 0 0 0 215 1 0 0 92 0 0 0] 8 #() 944 0 27 962 1392 11 1024 161 1408 1 1056 45 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 1 3088 0 0 0 5 0 0 0 3088 0 8 4294902755 658 0 0 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 403 95 482 161 45 3088 754 #text: 34 1 8 'Net Service:' 3088 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 201 0 0 0 47 0 0 0 25 1 0 0 69 0 0 0] 8 #() 944 0 27 962 1392 11 1024 161 1408 1 1056 45 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 1 3408 0 0 0 5 0 0 0 3408 0 8 4294902755 658 0 0 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 403 49 482 161 45 3408 754 #text: 34 1 8 'Gem Type:' 3408 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 201 0 0 0 24 0 0 0 25 1 0 0 46 0 0 0] 8 #() 944 0 27 962 1392 11 1024 161 1408 1 1056 45 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916352 1025 3728 0 1138 1168 0 5 0 0 0 3728 0 8 4294902793 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 482 183 3 482 211 45 3728 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 0 0 0 1 0 0 0 196 0 0 0 23 0 0 0] 8 #() 944 0 27 962 1392 1 1024 211 1408 1 1056 45 410 ##(Smalltalk.PushButton) 34 20 0 416 34 2 8 1140924416 1 4016 0 0 0 5 0 0 0 4016 0 8 4294902781 1180998 4 ##(Smalltalk.CommandDescription) #commitTransaction 8 'Commit' 1 1 0 0 false 0 0 0 690 138 144 34 3 754 #createAt:extent: 34 2 482 133 571 482 121 51 4016 754 #isEnabled: 8 #(false) 4016 754 #text: 34 1 8 'Commit' 4016 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 66 0 0 0 29 1 0 0 126 0 0 0 54 1 0 0] 8 #() 944 0 29 962 1392 11 1024 121 1408 1 1056 51 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 1 4400 0 0 0 5 0 0 0 4400 0 8 4294902755 658 0 0 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 403 141 482 161 45 4400 754 #text: 34 1 8 'Net Task:' 4400 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 201 0 0 0 70 0 0 0 25 1 0 0 92 0 0 0] 8 #() 944 0 27 962 1392 11 1024 161 1408 1 1056 45 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916354 1025 4720 0 1138 1168 0 5 0 0 0 4720 0 8 4294902793 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 482 943 95 482 211 45 4720 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 215 1 0 0 47 0 0 0 64 2 0 0 69 0 0 0] 8 #() 944 0 27 962 1392 1 1024 211 1408 1 1056 45 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 1 5008 0 0 0 5 0 0 0 5008 0 8 4294902755 658 0 0 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 3 49 482 181 45 5008 754 #text: 34 1 8 'Stone Host:' 5008 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 24 0 0 0 91 0 0 0 46 0 0 0] 8 #() 944 0 27 962 1008 3 1024 181 1040 3 1056 45 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916352 1025 5328 0 1138 1168 0 5 0 0 0 5328 0 8 4294902793 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 482 563 141 482 211 45 5328 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 1 0 0 70 0 0 0 130 1 0 0 92 0 0 0] 8 #() 944 0 27 962 1392 1 1024 211 1408 1 1056 45 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916352 1025 5616 0 1138 8 4278190080 0 5 0 0 0 5616 0 8 4294902793 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 482 183 187 482 971 45 5616 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 0 0 0 93 0 0 0 64 2 0 0 115 0 0 0] 8 #() 944 0 27 962 1392 1 2752 1 1408 1 1056 45 410 ##(Smalltalk.PushButton) 34 20 0 416 34 2 8 1140924416 1 5920 0 0 0 5 0 0 0 5920 0 8 4294902781 4098 #abortTransaction 8 'Abort' 1 1 0 0 false 0 0 0 690 138 144 34 3 754 #createAt:extent: 34 2 482 3 571 482 121 51 5920 754 #isEnabled: 8 #(false) 5920 754 #text: 34 1 8 'Abort' 5920 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 29 1 0 0 61 0 0 0 54 1 0 0] 8 #() 944 0 29 962 1008 3 1024 121 2176 -49 1056 51 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916352 1025 6288 0 1138 1168 0 5 0 0 0 6288 0 8 4294902793 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 482 183 49 482 211 45 6288 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 0 0 0 24 0 0 0 196 0 0 0 46 0 0 0] 8 #() 944 0 27 962 1392 1 1024 211 1408 1 1056 45 410 ##(Smalltalk.PushButton) 34 20 0 416 34 2 8 1140924416 1 6576 0 0 0 5 0 0 0 6576 0 8 4294902781 4098 #hardBreak 8 'Hard Break' 1 1 0 0 false 0 0 0 690 138 144 34 3 754 #createAt:extent: 34 2 482 423 571 482 151 51 6576 754 #isEnabled: 8 #(false) 6576 754 #text: 34 1 8 'Hard Break' 6576 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 211 0 0 0 29 1 0 0 30 1 0 0 54 1 0 0] 8 #() 944 0 29 962 1392 11 1024 151 1408 1 1056 51 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916352 1025 6944 0 1138 1168 0 5 0 0 0 6944 0 8 4294902793 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 482 943 49 482 211 45 6944 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 215 1 0 0 24 0 0 0 64 2 0 0 46 0 0 0] 8 #() 944 0 27 962 1392 1 1024 211 1408 1 1056 45 410 ##(Smalltalk.PushButton) 34 20 0 416 34 2 8 1140924416 1 7232 0 0 0 5 0 0 0 7232 0 8 4294902781 4098 #softBreak 8 'Soft Break' 1 1 0 0 false 0 0 0 690 138 144 34 3 754 #createAt:extent: 34 2 482 263 571 482 151 51 7232 754 #isEnabled: 8 #(false) 7232 754 #text: 34 1 8 'Soft Break' 7232 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 131 0 0 0 29 1 0 0 206 0 0 0 54 1 0 0] 8 #() 944 0 29 962 1392 11 1024 151 1408 1 1056 51 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916354 1025 7600 0 1138 1168 0 5 0 0 0 7600 0 8 4294902793 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 482 943 141 482 211 45 7600 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 215 1 0 0 70 0 0 0 64 2 0 0 92 0 0 0] 8 #() 944 0 27 962 1392 1 1024 211 1408 1 1056 45 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 1 7888 0 0 0 5 0 0 0 7888 0 8 4294902755 658 0 0 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 783 95 482 161 45 7888 754 #text: 34 1 8 'GCI Session:' 7888 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 135 1 0 0 47 0 0 0 215 1 0 0 69 0 0 0] 8 #() 944 0 27 962 1392 11 1024 161 1408 1 1056 45 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916354 1025 8208 0 1138 1168 0 5 0 0 0 8208 0 8 4294902793 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 482 183 141 482 211 45 8208 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 0 0 0 70 0 0 0 196 0 0 0 92 0 0 0] 8 #() 944 0 27 962 1392 1 1024 211 1408 1 1056 45 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 1 8496 0 0 0 5 0 0 0 8496 0 8 4294902755 658 0 0 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 783 49 482 161 45 8496 754 #text: 34 1 8 'GCI Version:' 8496 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 135 1 0 0 24 0 0 0 215 1 0 0 46 0 0 0] 8 #() 944 0 27 962 1392 11 1024 161 1408 1 1056 45 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 1 8816 0 0 0 5 0 0 0 8816 0 8 4294902755 658 0 0 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 403 3 482 161 45 8816 754 #text: 34 1 8 'Gem Host:' 8816 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 201 0 0 0 1 0 0 0 25 1 0 0 23 0 0 0] 8 #() 944 0 27 962 1392 11 1024 161 1408 1 1056 45 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916352 1025 9136 0 1138 1168 0 5 0 0 0 9136 0 8 4294902793 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 482 563 95 482 211 45 9136 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 1 0 0 47 0 0 0 130 1 0 0 69 0 0 0] 8 #() 944 0 27 962 1392 1 1024 211 1408 1 1056 45 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916352 1025 9424 0 1138 1168 0 5 0 0 0 9424 0 8 4294902793 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 482 563 49 482 211 45 9424 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 1 0 0 24 0 0 0 130 1 0 0 46 0 0 0] 8 #() 944 0 27 962 1392 1 1024 211 1408 1 1056 45 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 1 9712 0 0 0 5 0 0 0 9712 0 8 4294902755 658 0 0 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 3 95 482 181 45 9712 754 #text: 34 1 8 'Stone Name:' 9712 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 47 0 0 0 91 0 0 0 69 0 0 0] 8 #() 944 0 27 962 1008 3 1024 181 1040 3 1056 45 410 ##(Smalltalk.TextEdit) 34 16 0 416 34 2 8 1140916352 1025 10032 0 1138 1168 0 5 0 0 0 10032 0 8 4294902793 658 0 0 3 690 138 144 34 1 754 #createAt:extent: 34 2 482 563 3 482 591 45 10032 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 1 0 0 1 0 0 0 64 2 0 0 23 0 0 0] 8 #() 944 0 27 962 1392 1 1024 591 1408 1 1056 45 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 1 10320 0 0 0 5 0 0 0 10320 0 8 4294902755 658 0 0 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 3 3 482 181 45 10320 754 #text: 34 1 8 'User ID:' 10320 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 1 0 0 0 91 0 0 0 23 0 0 0] 8 #() 944 0 27 962 1008 3 1024 181 994 #fixedParentTop 3 1056 45 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 65 10656 0 0 0 5 0 0 0 10656 0 8 4294902755 658 0 0 0 690 138 144 34 2 754 #createAt:extent: 34 2 482 3 187 482 181 45 10656 754 #text: 34 1 8 'Log Path:' 10656 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 93 0 0 0 91 0 0 0 115 0 0 0] 8 #() 944 0 27 962 1008 3 1024 181 1040 3 1056 45 170 192 34 28 9136 8 'netPort' 9424 8 'gemType' 4720 8 'gciSessionId' 3728 8 'userID' 8208 8 'stoneSessionID' 10032 8 'gemHost' 5328 8 'netTask' 6944 8 'gciVersion' 7600 8 'stoneSerial' 6288 8 'stoneHost' 5616 8 'logPath' 2192 8 'configurationReport' 1424 8 'versionReport' 1072 8 'stoneName' 0 690 138 144 34 1 754 #createAt:extent: 34 2 482 2559 21 482 1153 621 416 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 63 7 0 0 64 1 0 0] 34 30 10320 3728 8816 10032 5008 6288 3408 9424 8496 6944 9712 1072 3088 9136 7888 4720 576 8208 4400 5328 2768 7600 10656 5616 1424 2192 5920 4016 7232 6576 944 0 27 )! !
!MySessionPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

