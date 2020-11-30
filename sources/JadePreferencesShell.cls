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
	preferencesPresenter := self add: JadeTwinLevelInspector new name: 'preferences'!

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
	^OrderedCollection withAll: (JadePreferenceObject allSubclasses collect: [:each | each default])
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 327686 ##(Smalltalk.Color) #default 328198 ##(Smalltalk.Point) 1201 901 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 2 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1149239296 131073 592 nil 480 nil 7 nil nil nil 592 530 170 176 34 2 410 ##(Smalltalk.ContainerView) 34 15 nil 592 34 2 8 1409286144 131073 704 nil nil nil 7 nil nil nil 704 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.StaticText) 34 16 nil 704 34 2 8 1140850945 65 800 nil 466 #silver nil 7 nil nil nil 800 nil 8 1967984352 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 498 1 1 498 647 39 800 994 #text: 34 1 8 'Properties' 800 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 67 1 0 0 19 0 0 0] 8 #() 498 193 193 nil 27 nil nil nil 410 ##(Smalltalk.CardContainer) 34 16 nil 704 34 2 8 1409286144 131073 1200 nil nil nil 7 nil nil nil 1200 656710 1 ##(Smalltalk.CardLayout) 138 144 34 1 721414 ##(Smalltalk.Association) ##(Smalltalk.PropertyInspector) 410 ##(Smalltalk.ListTreeView) 34 59 nil 1200 34 2 8 1140953421 1 1360 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) nil nil 15 nil nil nil 1360 nil 8 1968071424 459270 ##(Smalltalk.Message) #valueText 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 3 920646 5 ##(Smalltalk.ListViewColumn) 8 'Property' 293 #left 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 3 1 1536 8 'doIt' 8 '[:x | x aspectDescriptor displayString]' 8 #[31 105 226 0 159 106] #aspectDescriptor #displayString 1712 7 257 nil 1538 #<= 8 #() nil nil 1360 nil 3 nil nil 1650 8 'Value' 291 #left 1698 0 nil 1730 2 1 1696 8 'doIt' 8 '[:x | x valueTextOnly ]' 8 #[30 105 226 0 106] #valueTextOnly 1872 7 257 nil 1538 #<= 1824 nil nil 1360 1698 0 nil 1730 2 1 1696 8 'doIt' 8 '[:x | x valueImage ]' 8 #[30 105 226 0 106] #valueImage 1968 7 257 nil 3 nil nil 1650 8 '' 41 #center 1538 #displayString 8 #() 1538 #<= 2096 nil nil 1360 1698 0 nil 1730 3 1 1696 8 'doIt' 8 '[:x | PropertyInspector editButtonImageIndexFor: x ]' 8 #[31 105 29 17 177 106] ##(Smalltalk.PropertyInspector) #editButtonImageIndexFor: 2128 7 257 nil 1 nil nil #report 8 #() nil 359 498 129 129 1 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil false 1050438 3 ##(Smalltalk.VirtualTreeModel) nil 1504 138 144 1472 nil nil 1698 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 2240 #defaultHasChildrenBlock 508791331 8 #[252 1 0 0 1 4 0 65 176 159 106 105] #childrenOf: #notEmpty 17 257 2256 1698 0 nil 2306 1 83886083 2240 #defaultFilterBlock 508773827 8 #[29 105 17 157 106] 2352 7 257 nil nil 138 ##(Smalltalk.PluggableSet) 1472 138 ##(Smalltalk.IdentitySet) 1472 13 5 nil nil nil nil nil nil nil 930 138 144 34 2 994 #createAt:extent: 34 2 498 9 9 498 631 621 1360 994 #text: 34 1 8 'Property' 1360 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 63 1 0 0 58 1 0 0] 8 #() 1184 nil 2029 1360 nil nil nil 170 192 34 2 1360 8 'aspects' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 1200 34 2 8 1140916738 1 2688 1426 138 144 8 #(##(Smalltalk.PropertyInspector)) nil 1504 nil nil 1 nil nil nil 2688 nil 8 1967838848 1698 0 nil 1730 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:x | nil]' 1782343965 2816 7 257 nil 1698 0 nil 2306 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 2880 7 257 nil 1600 nil nil nil nil nil #smallIcons nil nil nil nil nil 930 138 144 34 3 994 #createAt:extent: 34 2 498 1 1 498 647 681 2688 994 #setSingleSelection: 8 #(1) 2688 994 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2688 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 67 1 0 0 84 1 0 0] 8 #() 1184 nil 27 930 138 144 34 1 994 #createAt:extent: 34 2 498 1 39 498 647 681 1200 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 67 1 0 0 103 1 0 0] 34 2 1360 2688 1184 nil 27 170 192 1472 nil 930 138 144 34 1 994 #createAt:extent: 34 2 498 519 1 498 647 719 704 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 3 1 0 0 0 0 0 0 70 2 0 0 103 1 0 0] 34 2 800 1200 1184 nil 27 524806 ##(Smalltalk.Fraction) 1553 1227 false 170 192 34 2 704 8 'inspector' 590342 ##(Smalltalk.Rectangle) 498 1 1 498 1 1 930 138 144 34 2 994 #createAt:extent: 34 2 498 1 1 498 1169 723 592 994 #text: 34 1 8 'Inspector' 592 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 105 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 592 34 2 8 1140850688 131073 3840 nil nil nil 7 nil nil nil 3840 770 1 1 410 ##(Smalltalk.StaticText) 34 16 nil 3840 34 2 8 1140850945 65 3920 nil 466 #silver nil 7 nil nil nil 3920 nil 8 1967984352 898 nil nil nil 930 138 144 34 2 994 #createAt:extent: 34 2 498 1 1 498 509 39 3920 994 #text: 34 1 8 'Jade Options' 3920 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 0 0 0 19 0 0 0] 8 #() 1184 nil 27 nil nil nil 410 ##(Smalltalk.ListView) 34 45 nil 3840 34 2 8 1409372236 1 4240 1426 138 144 1472 nil 1504 466 #face3d nil 7 nil nil nil 4240 nil 8 1968071424 1538 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1600 nil nil nil 498 129 129 nil nil 138 144 34 1 1650 8 'Column 1' 201 #left 1538 #displayString 4384 1538 #<= 8 #() nil nil 4240 nil 1 nil nil #thumbnails 2208 498 193 193 131169 498 65 65 1 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 930 138 144 34 2 994 #createAt:extent: 34 2 498 1 39 498 509 681 4240 994 #text: 34 1 8 'Column 1' 4240 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 19 0 0 0 254 0 0 0 103 1 0 0] 8 #() 1184 nil 35 170 192 34 2 4240 8 'contents' nil 930 138 144 34 1 994 #createAt:extent: 34 2 498 1 1 498 509 719 3840 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 0 0 0 103 1 0 0] 34 2 3920 4240 1184 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 592 34 2 8 1140850688 1 4976 nil 480 nil 519 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 4976 nil 1 #left nil nil nil 498 1 1 498 9 9 nil 5072 nil 930 138 144 34 1 994 #createAt:extent: 34 2 498 509 1 498 11 719 4976 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 254 0 0 0 0 0 0 0 3 1 0 0 103 1 0 0] 8 #() 1184 nil 27 704 1184 nil 27 3 false 170 192 34 2 592 8 'preferences' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 930 138 144 34 1 994 #createAt:extent: 34 2 498 5119 21 498 1201 801 416 1122 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 154 1 0 0] 34 1 592 1184 nil 27 )! !
!JadePreferencesShell class categoriesFor: #defaultModel!public! !
!JadePreferencesShell class categoriesFor: #resource_Default_view!public!resources-views! !

