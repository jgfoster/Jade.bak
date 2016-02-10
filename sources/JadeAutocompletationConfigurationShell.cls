"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #JadeAutocompletationConfigurationShell
	instanceVariableNames: 'configurationComposite registryComposite mapingComposite paramComposite'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeAutocompletationConfigurationShell guid: (GUID fromString: '{FC1F72BB-A9F1-47FF-857D-675CF4028FA4}')!
JadeAutocompletationConfigurationShell comment: ''!
!JadeAutocompletationConfigurationShell categoriesForClass!Unclassified! !
!JadeAutocompletationConfigurationShell methodsFor!

createComponents
	super createComponents.

	configurationComposite := self add: JadeAutocompletationConfigurationPresenter new name: 'referee'.

	registryComposite := self add: JadeRegistryPresenter new name: 'registryReferee'.

	mapingComposite := self add: JadeMapAutocompletationPresenter new  name: 'mappingReferee'.

	paramComposite := self add: JadeParamAutocompletationPresenter new name: 'paramReferee'.!

model: anObject

	super model:  anObject.

	configurationComposite model: anObject.

	registryComposite model: self registry
	!

registry

	^JadeAutocompletationRegistry default! !
!JadeAutocompletationConfigurationShell categoriesFor: #createComponents!public! !
!JadeAutocompletationConfigurationShell categoriesFor: #model:!public! !
!JadeAutocompletationConfigurationShell categoriesFor: #registry!public! !

!JadeAutocompletationConfigurationShell class methodsFor!

icon

	^Icon fromFile: 'icons\GS32x32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 26214401 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 0 39 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 544 0 0 0 7 0 0 0 544 0 234 256 98 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 813 786 949 111 544 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 150 1 0 0 218 1 0 0 205 1 0 0] 98 1 410 8 ##(Smalltalk.PushButton)  98 20 0 544 98 2 8 1140924416 1 896 0 0 0 7 0 0 0 896 0 8 4294903566 1180998 4 ##(Smalltalk.CommandDescription)  8 #exit 8 'Close' 1 1 0 0 32 0 0 0 658 202 208 98 2 722 752 98 2 786 751 21 786 191 81 896 722 8 #text: 98 1 8 'Close' 896 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 1 0 0 10 0 0 0 214 1 0 0 50 0 0 0] 98 0 786 193 193 0 29 1280 0 27 0 0 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1409286144 131073 1296 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 7 0 0 0 1296 655878 ##(Smalltalk.CardLayout)  202 208 98 4 721414 ##(Smalltalk.Association)  8 'Options' 410 560 98 15 0 1296 98 2 8 1140850688 131073 1536 0 0 0 7 0 0 0 1536 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.GroupBox)  98 14 0 1536 98 2 8 1140850695 65 1664 0 1378 8 4278190080 0 7 0 0 0 1664 0 8 4294903566 658 202 208 98 2 722 752 98 2 786 27 31 786 821 681 1664 722 1184 98 1 8 'Autocompletation Options' 1664 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 13 0 0 0 15 0 0 0 167 1 0 0 99 1 0 0] 98 0 1280 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 27 2034 8 #fixedViewLeft 821 2034 8 #fixedParentTop 31 2034 8 #fixedViewTop 681 410 8 ##(Smalltalk.ReferenceView)  98 14 0 1536 98 2 8 1140850688 131073 2176 0 1378 8 4278190080 0 7 0 0 0 2176 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.JadeAutocompletationConfigurationPresenter)  8 #resource_Default_view 0 658 202 208 98 1 722 752 98 2 786 57 81 786 781 621 2176 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 28 0 0 0 40 0 0 0 162 1 0 0 94 1 0 0] 640 1280 0 27 2002 2048 57 2080 781 2112 81 2144 621 234 256 98 2 2176 8 'referee' 0 658 202 208 98 1 722 752 98 2 786 9 49 786 933 757 1536 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 214 1 0 0 146 1 0 0] 98 2 1664 2176 1280 0 27 1490 8 'Params Mapping' 410 2192 98 14 0 1296 98 2 8 1140850688 131073 2752 0 1378 8 4278190080 0 5 0 0 0 2752 2290 8 ##(Smalltalk.JadeParamAutocompletationPresenter)  2336 0 658 202 208 98 1 722 752 98 2 786 9 49 786 933 757 2752 834 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 214 1 0 0 146 1 0 0] 640 1280 0 27 1490 8 'Variable Name Mapping' 410 2192 98 14 0 1296 98 2 8 1140850688 131073 3056 0 1378 8 4278190080 0 5 0 0 0 3056 2290 8 ##(Smalltalk.JadeMapAutocompletationPresenter)  2336 0 658 202 208 98 1 722 752 98 2 786 9 49 786 933 757 3056 834 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 214 1 0 0 146 1 0 0] 640 1280 0 27 1490 8 'Registry' 410 2192 98 14 0 1296 98 2 8 1140850688 131073 3360 0 1378 8 4278190080 0 5 0 0 0 3360 2290 8 ##(Smalltalk.JadeRegistryPresenter)  2336 0 658 202 208 98 1 722 752 98 2 786 9 49 786 933 757 3360 834 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 214 1 0 0 146 1 0 0] 640 1280 0 27 1536 234 256 98 6 3056 8 'mappingReferee' 3360 8 'registryReferee' 2752 8 'paramReferee' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 1296 98 2 8 1140916736 1 3712 590662 2 ##(Smalltalk.ListModel)  202 208 98 4 1520 3344 3040 2736 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  0 0 1 0 0 0 3712 0 8 4294903494 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 8 #displayString 3920 7 257 0 3906 0 0 3938 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 4032 7 257 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 8 #noIcons 0 0 0 0 0 658 202 208 98 3 722 752 98 2 786 1 1 786 949 813 3712 722 8 #basicSelectionsByIndex: 98 1 98 1 3 3712 722 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 3712 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 1 0 0 150 1 0 0] 98 0 1280 0 27 658 202 208 98 1 722 752 98 2 786 1 1 786 949 813 1296 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 218 1 0 0 150 1 0 0] 98 5 1536 3360 3056 2752 3712 1280 0 27 234 256 640 0 0 0 0 0 1 0 0 0 0 1 0 0 658 202 208 98 3 722 752 98 2 786 2731 21 786 961 981 416 722 1184 98 1 8 'Jade Autocompletation Configuration' 416 722 8 #updateMenuBar 640 416 834 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 23 5 0 0 10 0 0 0 247 6 0 0 244 1 0 0] 98 2 1296 544 1280 0 27 )! !
!JadeAutocompletationConfigurationShell class categoriesFor: #icon!public! !
!JadeAutocompletationConfigurationShell class categoriesFor: #resource_Default_view!public!resources-views! !

