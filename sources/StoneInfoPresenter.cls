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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 0 0 34 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 10 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 544 0 0 0 5 0 0 0 544 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 4 410 ##(Smalltalk.ListView) 34 30 0 544 34 2 8 1409355853 1025 704 590662 2 ##(Smalltalk.ListModel) 138 144 656 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 5 0 0 0 704 0 8 4294905239 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Configuration' 381 #left 914 #first 8 #() ##(Smalltalk.SortedCollection) 0 0 704 0 1 0 0 1026 8 'Value' 151 #right 914 #last 1088 914 #<= 1088 0 0 704 0 1 0 0 #report 656 0 131169 0 34 4 0 0 328198 ##(Smalltalk.Point) 1 1 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 1186 567 1 1186 569 527 704 1282 #text: 34 1 8 'Configuration' 704 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 27 1 0 0 0 0 0 0 55 2 0 0 7 1 0 0] 8 #() 1186 193 193 0 27 8 'configurationReport' 410 ##(Smalltalk.ListView) 34 30 0 544 34 2 8 1409355853 1025 1504 770 138 144 656 0 832 850 880 0 5 0 0 0 1504 0 8 4294905239 914 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 976 0 0 0 0 0 0 138 144 34 2 1026 8 'Version' 261 #left 914 #first 8 #() ##(Smalltalk.SortedCollection) 0 0 1504 0 1 0 0 1026 8 'Value' 261 #left 914 #last 1744 914 #<= 1744 0 0 1504 0 1 0 0 #report 656 0 131169 0 34 4 0 0 1186 1 1 0 1218 138 144 34 2 1282 #createAt:extent: 34 2 1186 1 1 1186 567 527 1504 1282 #text: 34 1 8 'Version' 1504 1410 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 27 1 0 0 7 1 0 0] 8 #() 1472 0 27 8 'versionReport' 0 1218 138 144 34 1 1282 #createAt:extent: 34 2 1186 1 45 1186 1135 527 544 1410 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 22 0 0 0 55 2 0 0 29 1 0 0] 34 2 1504 704 1472 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2274 #fixedParentRight 1 2274 #fixedPreviousBottom 1 2274 #fixedParentBottom 1 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 65 2352 0 0 0 5 0 0 0 2352 0 8 4294904559 852486 ##(Smalltalk.NullConverter) 0 0 0 1218 138 144 34 2 1282 #createAt:extent: 34 2 1186 1 1 1186 311 49 2352 1282 #text: 34 1 8 'PositionableStream Type:' 2352 1410 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 155 0 0 0 24 0 0 0] 8 #() 1472 0 27 2242 2288 1 2274 #fixedViewLeft 311 2274 #fixedParentTop 1 2274 #fixedViewTop 49 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1409286144 131073 2736 0 850 8 4278190080 0 5 0 0 0 2736 656390 ##(Smalltalk.GridLayout) 1 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 0 2736 34 2 8 1141055497 1 2896 721990 2 ##(Smalltalk.ValueHolder) 0 0 1376774 ##(Smalltalk.PluggableSearchPolicy) 914 #= 8 #() 914 #hash 8 #() false 850 2816 0 5 0 0 0 2896 0 8 4294903949 2434 0 0 0 1218 138 144 34 2 1282 #createAt:extent: 34 2 1186 1 1 1186 141 45 2896 1282 #text: 34 1 8 'String' 2896 1410 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 70 0 0 0 22 0 0 0] 8 #() 1472 0 27 8 'String' 410 ##(Smalltalk.RadioButton) 34 16 0 2736 34 2 8 1140924425 1 3360 2962 0 0 2994 914 #= 8 #() 914 #hash 8 #() false 850 2816 0 5 0 0 0 3360 0 8 4294903949 2434 0 0 0 1218 138 144 34 2 1282 #createAt:extent: 34 2 1186 141 1 1186 141 45 3360 1282 #text: 34 1 8 'Unicode' 3360 1410 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 0 0 0 0 140 0 0 0 22 0 0 0] 8 #() 1472 0 27 8 'Unicode16' 0 1218 138 144 34 2 1282 #createAt:extent: 34 2 1186 855 1 1186 281 45 2736 1282 #isEnabled: 8 #(false) 2736 1410 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 171 1 0 0 0 0 0 0 55 2 0 0 22 0 0 0] 34 2 2896 3360 1472 0 27 2242 2274 #fixedPreviousRight 1 2688 281 2274 #fixedPreviousTop 1 2720 45 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1409286144 131073 4032 0 850 2816 0 5 0 0 0 4032 2834 1 5 1 1 170 192 34 4 410 ##(Smalltalk.RadioButton) 34 16 0 4032 34 2 8 1141055497 1 4160 2962 0 0 2994 914 #= 8 #() 914 #hash 8 #() false 850 2816 0 5 0 0 0 4160 0 8 4294903949 2434 0 0 0 1218 138 144 34 2 1282 #createAt:extent: 34 2 1186 1 1 1186 121 45 4160 1282 #text: 34 1 8 'ANSI' 4160 1410 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 60 0 0 0 22 0 0 0] 8 #() 1472 0 27 8 'ANSI' 410 ##(Smalltalk.RadioButton) 34 16 0 4032 34 2 8 1140924425 1 4592 2962 0 0 2994 914 #= 8 #() 914 #hash 8 #() false 850 2816 0 5 0 0 0 4592 0 8 4294903949 2434 0 0 0 1218 138 144 34 2 1282 #createAt:extent: 34 2 1186 121 1 1186 121 45 4592 1282 #text: 34 1 8 'Legacy' 4592 1410 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 60 0 0 0 0 0 0 0 120 0 0 0 22 0 0 0] 8 #() 1472 0 27 8 'Legacy' 0 1218 138 144 34 2 1282 #createAt:extent: 34 2 1186 311 1 1186 241 45 4032 1282 #isEnabled: 8 #(false) 4032 1410 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 0 0 0 0 0 0 0 19 1 0 0 22 0 0 0] 34 2 4160 4592 1472 0 27 2242 4000 1 2688 241 4016 1 2720 45 410 ##(Smalltalk.StaticText) 34 16 0 416 34 2 8 1140850944 65 5232 0 0 0 5 0 0 0 5232 0 8 4294904559 2434 0 0 0 1218 138 144 34 2 1282 #createAt:extent: 34 2 1186 695 1 1186 161 45 5232 1282 #text: 34 1 8 'String Type:' 5232 1410 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 1 0 0 0 0 0 0 171 1 0 0 22 0 0 0] 8 #() 1472 0 27 2242 2304 -439 2688 161 4016 1 2720 45 170 192 34 4 4032 8 'streamType' 2736 8 'stringType' 0 1218 138 144 34 1 1282 #createAt:extent: 34 2 1186 5119 21 1186 1135 571 416 1410 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 54 12 0 0 39 1 0 0] 34 5 2352 4032 5232 2736 544 1472 0 27 )! !
!StoneInfoPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

