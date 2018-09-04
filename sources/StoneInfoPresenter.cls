"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #StoneInfoPresenter
	instanceVariableNames: 'configurationReportPresenter streamTypePresenter stringTypePresenter versionReportPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StoneInfoPresenter guid: (GUID fromString: '{41118487-5cba-4c04-a425-873c6da92b67}')!
StoneInfoPresenter comment: ''!
!StoneInfoPresenter categoriesForClass!Unclassified! !
!StoneInfoPresenter methodsFor!

configDetailsFor: aPresenter

	| line string |
	line := aPresenter selection.	string := gciSession serverPerform: #'descriptionOfConfigOption:' with: line first.
	string := string , 'System stoneConfigurationAt: #' , (line at: 1) , ' "' , (line at: 2) , '"'.
	(JadeWorkspace showOnSession: gciSession)
		model: string;
		isModified: false;
		yourself.!

createComponents

	super createComponents.
	configurationReportPresenter	:= self add: ListPresenter			new name: 'configurationReport'.
	streamTypePresenter			:= self add: RadioButtonSetPresenter	new name: 'streamType'.
	stringTypePresenter			:= self add: RadioButtonSetPresenter	new name: 'stringType'.
	versionReportPresenter		:= self add: ListPresenter			new name: 'versionReport'.
!

createSchematicWiring

	super createSchematicWiring.
	configurationReportPresenter 	when: #'actionPerformed' send: #'configDetailsFor:' 	to: self with: configurationReportPresenter.
	versionReportPresenter 		when: #'actionPerformed' send: #'versionDetailsFor:' 	to: self with: versionReportPresenter.
!

fillSessionInfo

	| line list stream |
	stream := ReadStream on: (gciSession serverPerform: #'stoneInfo').
	streamTypePresenter 	value: (stream upTo: Character tab).
	stringTypePresenter		value: (stream upTo: Character tab).
	stream nextLine.
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
	configurationReportPresenter list: list.
!

versionDetailsFor: aPresenter

	| line string |
	line := aPresenter selection.	string := 'System stoneVersionAt: #' , (line at: 1) , ' "' , (line at: 2) , '"'.
	(JadeWorkspace showOnSession: gciSession)
		model: string;
		isModified: false;
		yourself.! !
!StoneInfoPresenter categoriesFor: #configDetailsFor:!public! !
!StoneInfoPresenter categoriesFor: #createComponents!public! !
!StoneInfoPresenter categoriesFor: #createSchematicWiring!public! !
!StoneInfoPresenter categoriesFor: #fillSessionInfo!public! !
!StoneInfoPresenter categoriesFor: #versionDetailsFor:!public! !

!StoneInfoPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 10 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1409286144 131073 544 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 nil nil nil 544 656390 ##(Smalltalk.GridLayout) 1 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 544 34 2 8 1141055497 1 720 721990 2 ##(Smalltalk.ValueHolder) nil nil 1376774 ##(Smalltalk.PluggableSearchPolicy) 459270 ##(Smalltalk.Message) #= 8 #() 850 #hash 8 #() false 624 nil 5 nil nil nil 720 nil 8 1716360256 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 1 1090 141 45 720 1042 #text: 34 1 8 'String' 720 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 70 0 0 0 22 0 0 0] 8 #() 1090 193 193 nil 27 8 'String' 410 ##(Smalltalk.RadioButton) 34 16 nil 544 34 2 8 1140924425 1 1280 786 nil nil 818 850 #= 8 #() 850 #hash 8 #() false 624 nil 5 nil nil nil 1280 nil 8 1716360256 946 nil nil nil 978 138 144 34 2 1042 #createAt:extent: 34 2 1090 141 1 1090 141 45 1280 1042 #text: 34 1 8 'Unicode' 1280 1186 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 0 0 0 0 140 0 0 0 22 0 0 0] 8 #() 1248 nil 27 8 'Unicode16' nil 978 138 144 34 2 1042 #createAt:extent: 34 2 1090 855 1 1090 281 45 544 1042 #isEnabled: 8 #(false) 544 1186 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 171 1 0 0 0 0 0 0 55 2 0 0 22 0 0 0] 34 2 720 1280 1248 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedPreviousRight 1 1922 #fixedViewLeft 281 1922 #fixedPreviousTop 1 1922 #fixedViewTop 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 65 2000 nil nil nil 5 nil nil nil 2000 nil 8 1716357792 946 nil nil nil 978 138 144 34 2 1042 #createAt:extent: 34 2 1090 1 1 1090 311 49 2000 1042 #text: 34 1 8 'PositionableStream Type:' 2000 1186 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 155 0 0 0 24 0 0 0] 8 #() 1248 nil 27 1890 1922 #fixedParentLeft 1 1952 311 1922 #fixedParentTop 1 1984 49 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1409286144 131073 2352 nil 624 nil 5 nil nil nil 2352 658 1 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 nil 2352 34 2 8 1140924425 1 2464 786 nil nil 818 850 #= 8 #() 850 #hash 8 #() false 624 nil 5 nil nil nil 2464 nil 8 1716360256 946 nil nil nil 978 138 144 34 2 1042 #createAt:extent: 34 2 1090 121 1 1090 121 45 2464 1042 #text: 34 1 8 'Legacy' 2464 1186 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 0 0 0 0 120 0 0 0 22 0 0 0] 8 #() 1248 nil 27 8 'Legacy' 410 ##(Smalltalk.RadioButton) 34 16 nil 2352 34 2 8 1141055497 1 2880 786 nil nil 818 850 #= 8 #() 850 #hash 8 #() false 624 nil 5 nil nil nil 2880 nil 8 1716360256 946 nil nil nil 978 138 144 34 2 1042 #createAt:extent: 34 2 1090 1 1 1090 121 45 2880 1042 #text: 34 1 8 'ANSI' 2880 1186 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 60 0 0 0 22 0 0 0] 8 #() 1248 nil 27 8 'ANSI' nil 978 138 144 34 2 1042 #createAt:extent: 34 2 1090 311 1 1090 241 45 2352 1042 #isEnabled: 8 #(false) 2352 1186 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 0 0 0 0 0 0 0 19 1 0 0 22 0 0 0] 34 2 2880 2464 1248 nil 27 1890 1936 1 1952 241 1968 1 1984 45 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 65 3504 nil nil nil 5 nil nil nil 3504 nil 8 1716357792 946 nil nil nil 978 138 144 34 2 1042 #createAt:extent: 34 2 1090 695 1 1090 161 45 3504 1042 #text: 34 1 8 'String Type:' 3504 1186 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 1 0 0 0 0 0 0 171 1 0 0 22 0 0 0] 8 #() 1248 nil 27 1890 1922 #fixedParentRight -439 1952 161 1968 1 1984 45 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 3840 nil nil nil 5 nil nil nil 3840 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 4 410 ##(Smalltalk.ListView) 34 30 nil 3840 34 2 8 1409355853 1025 4000 590662 2 ##(Smalltalk.ListModel) 138 144 3952 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 624 nil 5 nil nil nil 4000 nil 8 1716250736 850 #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Configuration' 381 #left 850 #first 8 #() ##(Smalltalk.SortedCollection) nil nil 4000 nil 1 nil nil 4258 8 'Value' 151 #right 850 #last 4320 850 #<= 4320 nil nil 4000 nil 1 nil nil #report 3952 nil 131169 nil 34 4 nil nil 1090 1 1 nil 978 138 144 34 2 1042 #createAt:extent: 34 2 1090 567 1 1090 569 527 4000 1042 #text: 34 1 8 'Configuration' 4000 1186 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 27 1 0 0 0 0 0 0 55 2 0 0 7 1 0 0] 8 #() 1248 nil 27 8 'configurationReport' 410 ##(Smalltalk.ListView) 34 30 nil 3840 34 2 8 1409355853 1025 4656 4066 138 144 3952 nil 4128 624 nil 5 nil nil nil 4656 nil 8 1716250736 850 #displayString 8 #() nil 4208 nil nil nil nil nil nil 138 144 34 2 4258 8 'Version' 261 #left 850 #first 8 #() ##(Smalltalk.SortedCollection) nil nil 4656 nil 1 nil nil 4258 8 'Value' 261 #left 850 #last 4880 850 #<= 4880 nil nil 4656 nil 1 nil nil #report 3952 nil 131169 nil 34 4 nil nil 1090 1 1 nil 978 138 144 34 2 1042 #createAt:extent: 34 2 1090 1 1 1090 567 527 4656 1042 #text: 34 1 8 'Version' 4656 1186 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 27 1 0 0 7 1 0 0] 8 #() 1248 nil 27 8 'versionReport' nil 978 138 144 34 1 1042 #createAt:extent: 34 2 1090 1 45 1090 1135 527 3840 1186 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 22 0 0 0 55 2 0 0 29 1 0 0] 34 2 4656 4000 1248 nil 27 1890 2320 1 3824 1 1922 #fixedPreviousBottom 1 1922 #fixedParentBottom 1 170 192 34 4 2352 8 'streamType' 544 8 'stringType' nil 978 138 144 34 1 1042 #createAt:extent: 34 2 1090 5119 21 1090 1135 571 416 1186 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 54 12 0 0 39 1 0 0] 34 5 2000 2352 3504 544 3840 1248 nil 27 )! !
!StoneInfoPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

