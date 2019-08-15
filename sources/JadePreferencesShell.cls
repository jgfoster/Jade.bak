"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadePreferencesShell
	instanceVariableNames: 'preferencesPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePreferencesShell guid: (GUID fromString: '{a193b613-cc5f-442d-88b2-85f73b3ce67e}')!
JadePreferencesShell comment: ''!
!JadePreferencesShell categoriesForClass!Unclassified! !
!JadePreferencesShell methodsFor!

createComponents
	"Create the presenters contained by the receiver"


	super createComponents.
	preferencesPresenter := self add: TwinLevelInspector new name: 'preferences'!

model: aListModelOfObjects

	super model: aListModelOfObjects.
	preferencesPresenter model: aListModelOfObjects!

onViewOpened
	super onViewOpened.

	JadePreferenceObject gciSession: gciSession!

shellName

	^'Jade Preferences'.! !
!JadePreferencesShell categoriesFor: #createComponents!public! !
!JadePreferencesShell categoriesFor: #model:!public! !
!JadePreferencesShell categoriesFor: #onViewOpened!public! !
!JadePreferencesShell categoriesFor: #shellName!public! !

!JadePreferencesShell class methodsFor!

defaultModel
	^OrderedCollection new
		add: JadeAllSessionsPreference;
		add: JadeCurrentSessionPreference;
		add: JadeMethodExplorerPreference;
		add: JadeFileOutEncodingPreference;
		asArray;
		yourself!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 2 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1149239296 131073 576 nil 480 nil 7 nil nil nil 576 514 170 176 34 2 410 ##(Smalltalk.ContainerView) 34 15 nil 576 34 2 8 1409286144 131073 688 nil nil nil 7 nil nil nil 688 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.StaticText) 34 16 nil 688 34 2 8 1140850945 65 784 nil 786694 ##(Smalltalk.IndexedColor) 33554447 nil 7 nil nil nil 784 nil 8 1921133906 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 1 1042 647 39 784 994 #text: 34 1 8 'Properties' 784 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 67 1 0 0 19 0 0 0] 8 #() 1042 193 193 nil 27 nil nil nil 410 ##(Smalltalk.CardContainer) 34 16 nil 688 34 2 8 1409286144 131073 1216 nil nil nil 7 nil nil nil 1216 655878 ##(Smalltalk.CardLayout) 138 144 34 1 721414 ##(Smalltalk.Association) ##(Smalltalk.PropertyInspector) 410 ##(Smalltalk.ListTreeView) 34 44 nil 1216 34 2 8 1140953421 1 1376 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) nil nil 15 nil nil nil 1376 nil 8 1921377647 459270 ##(Smalltalk.Message) #valueText 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 3 920646 5 ##(Smalltalk.ListViewColumn) 8 'Property' 293 #left 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 3 1 1552 8 'doIt' 8 '[:x | x aspectDescriptor displayString]' 8 #[31 105 226 0 159 106] #aspectDescriptor #displayString 1728 7 257 nil 1554 #<= 8 #() nil nil 1376 nil 3 nil nil 1666 8 'Value' 291 #left 1714 0 nil 1746 2 1 1712 8 'doIt' 8 '[:x | x valueTextOnly ]' 8 #[30 105 226 0 106] #valueTextOnly 1888 7 257 nil 1554 #<= 1840 nil nil 1376 1714 0 nil 1746 2 1 1712 8 'doIt' 8 '[:x | x valueImage ]' 8 #[30 105 226 0 106] #valueImage 1984 7 257 nil 3 nil nil 1666 8 '' 41 #center 1554 #displayString 8 #() 1554 #<= 2112 nil nil 1376 1714 0 nil 1746 3 1 1712 8 'doIt' 8 '[:x | PropertyInspector editButtonImageIndexFor: x ]' 8 #[31 105 29 17 177 106] ##(Smalltalk.PropertyInspector) #editButtonImageIndexFor: 2144 7 257 nil 1 nil nil #report 1488 nil 359 1042 129 129 34 4 nil nil 1042 1 1 nil false 1050438 3 ##(Smalltalk.VirtualTreeModel) nil 1520 138 144 1488 nil nil 1714 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 2272 #defaultHasChildrenBlock 508791331 8 #[252 1 0 0 1 4 0 65 176 159 106 105] #childrenOf: #notEmpty 17 257 2288 1714 0 nil 2338 1 83886083 2272 #defaultFilterBlock 508773827 8 #[29 105 17 157 106] 2384 7 257 nil nil 138 ##(Smalltalk.PluggableSet) 1488 138 ##(Smalltalk.PluggableSet) 1488 13 5 nil nil nil nil nil nil nil 930 138 144 34 2 994 #createAt:extent: 34 2 1042 9 9 1042 631 621 1376 994 #text: 34 1 8 'Property' 1376 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 63 1 0 0 58 1 0 0] 8 #() 1200 nil 2021 1376 170 192 34 2 1376 8 'aspects' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 1216 34 2 8 1140916738 1 2720 1442 138 144 8 #(##(Smalltalk.PropertyInspector)) nil 1520 nil nil 1 nil nil nil 2720 nil 8 1921048286 1714 0 nil 1746 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:x | nil]' 1782343965 2848 7 257 nil 1714 0 nil 2338 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 2912 7 257 nil 1616 nil nil nil nil nil #smallIcons nil nil nil nil nil 930 138 144 34 3 994 #createAt:extent: 34 2 1042 1 1 1042 647 681 2720 994 #basicSelectionsByIndex: 34 1 8 #(1) 2720 994 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2720 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 67 1 0 0 84 1 0 0] 8 #() 1200 nil 27 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 39 1042 647 681 1216 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 67 1 0 0 103 1 0 0] 34 2 1376 2720 1200 nil 27 170 192 1488 nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 519 1 1042 647 719 688 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 3 1 0 0 0 0 0 0 70 2 0 0 103 1 0 0] 34 2 784 1216 1200 nil 27 524806 ##(Smalltalk.Fraction) 1553 1227 false 170 192 34 2 688 8 'inspector' 590342 ##(Smalltalk.Rectangle) 1042 1 1 1042 1 1 930 138 144 34 2 994 #createAt:extent: 34 2 1042 1 1 1042 1169 723 576 994 #text: 34 1 8 'Inspector' 576 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 105 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 576 34 2 8 1140850688 131073 3888 nil nil nil 7 nil nil nil 3888 754 1 1 410 ##(Smalltalk.StaticText) 34 16 nil 3888 34 2 8 1140850945 65 3968 nil 864 nil 7 nil nil nil 3968 nil 8 1921133906 898 nil nil nil 930 138 144 34 2 994 #createAt:extent: 34 2 1042 1 1 1042 509 39 3968 994 #text: 34 1 8 'Jade Options' 3968 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 0 0 0 19 0 0 0] 8 #() 1200 nil 27 nil nil nil 410 ##(Smalltalk.ListView) 34 30 nil 3888 34 2 8 1409372236 1 4272 1442 138 144 1488 nil 1520 721158 ##(Smalltalk.SystemColor) 31 nil 7 nil nil nil 4272 nil 8 1921377647 1554 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1616 nil nil nil 1042 129 129 nil nil 138 144 34 1 1666 8 'Column 1' 201 #left 1554 #displayString 4432 1554 #<= 8 #() nil nil 4272 nil 1 nil nil #thumbnails 1488 1042 193 193 131169 1042 65 65 34 4 nil nil 1042 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 1042 1 39 1042 509 681 4272 994 #text: 34 1 8 'Column 1' 4272 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 254 0 0 0 103 1 0 0] 8 #() 1200 nil 27 170 192 34 2 4272 8 'contents' nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 1 1042 509 719 3888 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 0 0 0 103 1 0 0] 34 2 3968 4272 1200 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 576 34 2 8 1140850688 1 5056 nil 480 nil 519 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 5056 nil 1 #left nil nil nil 1042 1 1 1042 9 9 nil 5152 nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 509 1 1042 11 719 5056 1138 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 254 0 0 0 0 0 0 0 3 1 0 0 103 1 0 0] 8 #() 1200 nil 27 688 1200 nil 27 3 false 170 192 34 2 576 8 'preferences' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 930 138 144 34 2 994 #createAt:extent: 34 2 1042 3059 21 1042 1201 801 416 994 #updateMenuBar 1488 416 1138 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 5 0 0 10 0 0 0 81 8 0 0 154 1 0 0] 34 1 576 1200 nil 27 )! !
!JadePreferencesShell class categoriesFor: #defaultModel!public! !
!JadePreferencesShell class categoriesFor: #resource_Default_view!public!resources-views! !

