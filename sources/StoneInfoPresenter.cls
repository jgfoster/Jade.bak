"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #StoneInfoPresenter
	instanceVariableNames: 'configurationReportPresenter versionReportPresenter'
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
	configurationReportPresenter	:= self add: ListPresenter	new name: 'configurationReport'.
	versionReportPresenter		:= self add: ListPresenter	new name: 'versionReport'.
!

createSchematicWiring

	super createSchematicWiring.
	configurationReportPresenter 	when: #'actionPerformed' send: #'configDetailsFor:' 	to: self with: configurationReportPresenter.
	versionReportPresenter 		when: #'actionPerformed' send: #'versionDetailsFor:' 	to: self with: versionReportPresenter.
!

fillSessionInfo

	| line list stream |
	stream := ReadStream on: (gciSession serverPerform: #'stoneInfo').
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 0 0 34 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 4 410 ##(Smalltalk.ListView) 34 30 0 416 34 2 8 1409355853 1025 576 590662 2 ##(Smalltalk.ListModel) 138 144 528 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 5 0 0 0 576 0 8 4294905239 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Version' 271 #left 786 #first 8 #() ##(Smalltalk.SortedCollection) 0 0 576 0 1 0 0 898 8 'Value' 261 #left 786 #last 960 786 #<= 960 0 0 576 0 1 0 0 #report 528 0 131169 0 34 4 0 0 328198 ##(Smalltalk.Point) 1 1 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 1058 1 1 1058 595 801 576 1154 #text: 34 1 8 'Version' 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 41 1 0 0 144 1 0 0] 8 #() 1058 193 193 0 27 8 'versionReport' 410 ##(Smalltalk.ListView) 34 30 0 416 34 2 8 1409355853 1025 1376 642 138 144 528 0 704 722 752 0 5 0 0 0 1376 0 8 4294905239 786 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 848 0 0 0 0 0 0 138 144 34 2 898 8 'Configuration' 381 #left 786 #first 8 #() ##(Smalltalk.SortedCollection) 0 0 1376 0 1 0 0 898 8 'Value' 151 #right 786 #last 1616 786 #<= 1616 0 0 1376 0 1 0 0 #report 528 0 131169 0 34 4 0 0 1058 1 1 0 1090 138 144 34 2 1154 #createAt:extent: 34 2 1058 605 1 1058 597 801 1376 1154 #text: 34 1 8 'Configuration' 1376 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 46 1 0 0 0 0 0 0 88 2 0 0 144 1 0 0] 8 #() 1344 0 27 8 'configurationReport' 0 1090 138 144 34 1 1154 #createAt:extent: 34 2 1058 5119 21 1058 1201 801 416 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 154 1 0 0] 34 3 576 410 ##(Smalltalk.Splitter) 34 12 0 416 34 2 8 1140850688 1 2112 0 722 8 4278190080 0 517 0 0 0 2112 1090 138 144 34 1 1154 #createAt:extent: 34 2 1058 595 1 1058 11 801 2112 1282 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 41 1 0 0 0 0 0 0 46 1 0 0 144 1 0 0] 8 #() 1344 0 27 1376 1344 0 27 )! !
!StoneInfoPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

