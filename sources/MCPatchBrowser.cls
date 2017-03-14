"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #MCPatchBrowser
	instanceVariableNames: 'patch includeIdenticalPresenter operationListPresenter leftTextPresenter leftMemoPresenter rightTextPresenter rightMemoPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCPatchBrowser guid: (GUID fromString: '{6a36f578-7c9d-4ace-a3b0-d2e1313fbbca}')!
MCPatchBrowser comment: ''!
!MCPatchBrowser categoriesForClass!Unclassified! !
!MCPatchBrowser methodsFor!

createComponents

	super createComponents.
	includeIdenticalPresenter	:= self add: BooleanPresenter	new name: 'includeIdenticalSource'.
	operationListPresenter 		:= self add: ListPresenter 			new name: 'operationList'.
	leftTextPresenter				:= self add: TextPresenter			new name: 'leftText'.
	leftMemoPresenter			:= self add: TextPresenter			new name: 'leftMemo'.
	rightTextPresenter			:= self add: TextPresenter			new name: 'rightText'.
	rightMemoPresenter			:= self add: TextPresenter			new name: 'rightMemo'.
!

createSchematicWiring

	super createSchematicWiring.
	includeIdenticalPresenter	when: #'valueChanged'		send: #'refresh'					to: self.
	operationListPresenter 		when: #'selectionChanged' send: #'operationSelected' 	to: self.
!

inspectLine

	| line |
	(line := operationListPresenter selectionOrNil) isNil ifTrue: [^self].
	line halt.
!

model: anMCPatch

	patch := anMCPatch.
	self gciSession: anMCPatch gciSession.
!

onViewOpened

	super onViewOpened.
	self refresh.
!

operationSelected

	| operation |
	leftTextPresenter 		value: ''.
	leftMemoPresenter	value: ''.
	rightTextPresenter	value: ''.
	rightMemoPresenter	value: ''.
	operationListPresenter hasSelection ifFalse: [^self].
	operation := operationListPresenter selection.
	leftTextPresenter 		value: operation obsoletionText.
	leftMemoPresenter	value: operation obsoletionMemo.
	rightTextPresenter	value: operation modificationText.
	rightMemoPresenter	value: operation modificationMemo.
	operation obsoletionMemoMarkers do: [:each | 
		leftMemoPresenter view addMarkerType: each key at: each value.
	].
	operation modificationMemoMarkers do: [:each |
		rightMemoPresenter view addMarkerType: each key at: each value.
	].
!

operationsList

	| list |
	list := patch operations.
	includeIdenticalPresenter value ifFalse: [
		list := list reject: [:each | each hasEquivalentText].
	].
	^list asSortedCollection asArray.
!

refresh

	operationListPresenter
		resetSelection;
		list: self operationsList;
		yourself!

shellName

	^patch name
! !
!MCPatchBrowser categoriesFor: #createComponents!public! !
!MCPatchBrowser categoriesFor: #createSchematicWiring!public! !
!MCPatchBrowser categoriesFor: #inspectLine!public! !
!MCPatchBrowser categoriesFor: #model:!public! !
!MCPatchBrowser categoriesFor: #onViewOpened!public! !
!MCPatchBrowser categoriesFor: #operationSelected!public! !
!MCPatchBrowser categoriesFor: #operationsList!public! !
!MCPatchBrowser categoriesFor: #refresh!public! !
!MCPatchBrowser categoriesFor: #shellName!public! !

!MCPatchBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout) 234 240 98 0 16 234 256 608 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 530 2559 21 530 1601 1201 416 706 8 #updateMenuBar 608 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 31 8 0 0 98 2 0 0] 98 3 410 8 ##(Smalltalk.ContainerView) 98 15 0 416 98 2 8 1140850688 131073 896 0 0 0 7 0 0 0 896 852230 ##(Smalltalk.FramingLayout) 234 240 98 4 410 8 ##(Smalltalk.ListView) 98 30 0 896 98 2 8 1409355853 1025 1040 590662 2 ##(Smalltalk.ListModel) 202 208 608 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 482 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu) 0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) 8 #inspectLine 8 'Inspect' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 1040 0 8 4294902815 459270 ##(Smalltalk.Message) 8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 202 208 98 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Type' 201 8 #left 1410 1440 98 0 1410 8 #<= 1632 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 2 1 8 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each typeString]' 8 #[30 105 226 0 106] 8 #typeString 1696 7 257 0 0 1040 0 1 0 0 1554 8 'Class' 201 1600 1410 1440 1456 8 ##(Smalltalk.SortedCollection) 1682 0 0 1714 2 1 1744 8 'doIt' 8 '[:each | each className]' 8 #[30 105 226 0 106] 8 #className 1888 7 257 0 0 1040 0 1 0 0 1554 8 'Selector' 201 1600 1410 1440 1632 1410 1664 1632 1682 0 0 1714 2 1 1744 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] 8 #selector 2048 7 257 0 0 1040 0 1 0 0 1554 8 'Detail' 961 1600 1410 1440 1632 1410 1664 1632 1682 0 0 1714 2 1 1744 8 'doIt' 8 '[:each | each detailsString]' 8 #[30 105 226 0 106] 8 #detailsString 2208 7 257 0 0 1040 0 3 0 0 8 #report 608 0 131169 0 98 1 0 642 202 208 98 3 706 736 98 2 530 1 41 530 1569 517 1040 706 8 #contextMenu: 98 1 1248 1040 706 8 #text: 98 1 8 'Type' 1040 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 16 3 0 0 22 1 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) 8 #fixedParentLeft 1 2658 8 #fixedParentRight 1 2658 8 #fixedParentTop 41 2658 8 #fixedParentBottom 1 410 8 ##(Smalltalk.CheckBox) 98 16 0 896 98 2 8 1409363203 1 2800 721990 2 ##(Smalltalk.ValueHolder) 0 0 1114118 ##(Smalltalk.NeverSearchPolicy) 32 0 0 7 0 0 0 2800 0 8 4294902755 852486 ##(Smalltalk.NullConverter) 0 0 0 642 202 208 98 2 706 736 98 2 530 5 1 530 511 41 2800 706 2512 98 1 8 'Include methods with identical source' 2800 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 0 0 0 0 1 1 0 0 20 0 0 0] 98 0 2608 0 27 2626 2672 5 2658 8 #fixedViewLeft 511 2736 1 2658 8 #fixedViewTop 41 234 256 98 4 1040 8 'operationList' 2800 8 'includeIdenticalSource' 0 642 202 208 98 1 706 736 98 2 530 1 1 530 1569 557 896 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 22 1 0 0] 98 2 2800 1040 2608 0 27 410 8 ##(Smalltalk.Splitter) 98 12 0 416 98 2 8 1140850688 1 3504 0 482 8 4278190080 0 519 0 0 0 3504 642 202 208 98 1 706 736 98 2 530 1 557 530 1569 11 3504 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 22 1 0 0 16 3 0 0 27 1 0 0] 98 0 2608 0 27 410 912 98 15 0 416 98 2 8 1140850688 131073 3776 0 0 0 7 0 0 0 3776 562 234 240 608 32 234 256 608 0 642 202 208 98 1 706 736 98 2 530 1 567 530 1569 557 3776 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 1 0 0 16 3 0 0 49 2 0 0] 98 3 410 912 98 15 0 3776 98 2 8 1140850688 131073 4048 0 0 0 7 0 0 0 4048 978 234 240 98 4 410 8 ##(Smalltalk.ScintillaView) 98 50 0 4048 98 2 8 1445007428 1025 4160 2882 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 482 8 4278190080 0 7 0 0 0 4160 0 8 4294902941 2962 0 0 11 0 234 256 98 4 8 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 0 0 1 0 0 0 0 4384 0 0 0 8 #callTip 4402 77 786694 ##(Smalltalk.IndexedColor) 33554457 4466 33554471 1 0 0 0 0 4432 0 0 0 98 256 4416 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 0 0 32 202 208 608 234 256 98 8 8 #changed 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 7 45 0 0 4160 4608 0 0 0 0 0 0 8 #added 4626 3 45 0 0 4160 4656 0 0 0 0 0 0 8 #removed 4626 5 45 0 0 4160 4688 0 0 0 0 0 0 8 #circle 4626 1 0 0 0 4160 4720 0 0 0 0 0 0 202 8 ##(Smalltalk.IdentitySet) 608 0 234 240 608 9215 0 0 0 0 4466 33554447 0 0 65 0 0 234 256 98 6 8 #literalBytes 8 '[]' 8 #literalArray 8 '()' 8 #specialCharacter 8 '()[]<>' 8 '' 3 234 256 98 2 8 #container 234 256 98 2 4384 4416 0 0 0 0 1 0 234 256 98 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 4160 65025 3 32 1 0 0 0 0 0 0 3 5074 3 4160 33423361 5 32 3 0 0 0 0 0 0 5 5074 5 4160 511 1 32 5 0 0 0 0 0 0 0 0 234 256 98 6 8 #Notification 4402 1027 0 196934 1 ##(Smalltalk.RGB) 31185883 1 0 0 0 0 5168 0 0 0 8 #Error 4402 1029 5202 335 5202 31317503 1 0 0 0 0 5232 0 0 0 8 #Warning 4402 1031 5202 60239 5202 31326207 1 0 0 0 0 5296 0 0 0 0 642 202 208 98 5 706 736 98 2 530 1 41 530 779 517 4160 706 8 #modificationEventMask: 98 1 9215 4160 706 8 #caretPeriod: 98 1 1061 4160 706 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin) 1 4160 1 3 32 1 15 5634 3 4160 33 1 32 67108863 15 5634 5 4160 1 1 16 -67108863 15 4160 706 8 #tabWidth: 98 1 9 4160 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 133 1 0 0 22 1 0 0] 98 0 2608 0 29 2626 2672 1 2704 1 2736 41 2768 1 410 8 ##(Smalltalk.TextEdit) 98 16 0 4048 98 2 8 1140916352 1025 5808 0 482 8 4278190080 0 7 0 0 0 5808 0 8 4294902043 2962 0 0 3 642 202 208 98 1 706 736 98 2 530 1 1 530 779 39 5808 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 133 1 0 0 19 0 0 0] 98 0 2608 0 27 2626 2672 1 2704 1 2736 1 3248 39 234 256 98 4 4160 8 'leftMemo' 5808 8 'leftText' 0 642 202 208 98 1 706 736 98 2 530 1 1 530 779 557 4048 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 133 1 0 0 22 1 0 0] 98 2 5808 4160 2608 0 27 410 3520 98 12 0 3776 98 2 8 1140850688 1 6352 0 482 3600 0 519 0 0 0 6352 642 202 208 98 1 706 736 98 2 530 779 1 530 11 557 6352 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 133 1 0 0 0 0 0 0 138 1 0 0 22 1 0 0] 98 0 2608 0 27 410 912 98 15 0 3776 98 2 8 1140850688 131073 6592 0 0 0 7 0 0 0 6592 978 234 240 98 4 410 4176 98 50 0 6592 98 2 8 1445007428 1025 6704 2882 0 32 4272 0 482 4304 0 7 0 0 0 6704 0 8 4294902941 2962 0 0 11 0 234 256 98 4 4384 4402 1 0 0 1 0 0 0 0 4384 0 0 0 4432 4402 77 4480 4496 1 0 0 0 0 4432 0 0 0 98 256 6864 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6880 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4530 1 0 0 32 202 208 608 234 256 98 8 4608 4626 7 45 0 0 6704 4608 0 0 0 0 0 0 4656 4626 3 45 0 0 6704 4656 0 0 0 0 0 0 4688 4626 5 45 0 0 6704 4688 0 0 0 0 0 0 4720 4626 1 0 0 0 6704 4720 0 0 0 0 0 0 202 4768 608 0 234 240 608 9215 0 0 0 0 4800 0 0 65 0 0 4816 8 '' 3 234 256 98 2 4992 234 256 98 2 4384 6864 0 0 0 0 1 0 234 256 98 6 1 5074 1 6704 65025 3 32 1 0 0 0 0 0 0 3 5074 3 6704 33423361 5 32 3 0 0 0 0 0 0 5 5074 5 6704 511 1 32 5 0 0 0 0 0 0 0 0 234 256 98 6 5168 4402 1027 0 5202 31185883 1 0 0 0 0 5168 0 0 0 5232 4402 1029 5202 335 5202 31317503 1 0 0 0 0 5232 0 0 0 5296 4402 1031 5202 60239 5202 31326207 1 0 0 0 0 5296 0 0 0 0 642 202 208 98 5 706 736 98 2 530 1 41 530 781 517 6704 706 5488 98 1 9215 6704 706 5536 98 1 1061 6704 706 5584 98 1 98 3 5634 1 6704 1 3 32 1 15 5634 3 6704 33 1 32 67108863 15 5634 5 6704 1 1 16 -67108863 15 6704 706 5712 98 1 9 6704 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 134 1 0 0 22 1 0 0] 98 0 2608 0 29 2626 2672 1 2704 1 2736 41 2768 1 410 5824 98 16 0 6592 98 2 8 1140916352 1025 7760 0 482 5904 0 7 0 0 0 7760 0 8 4294902043 2962 0 0 3 642 202 208 98 1 706 736 98 2 530 1 1 530 781 39 7760 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 134 1 0 0 19 0 0 0] 98 0 2608 0 27 2626 2672 1 2704 1 2736 1 3248 39 234 256 98 4 6704 8 'rightMemo' 7760 8 'rightText' 0 642 202 208 98 1 706 736 98 2 530 789 1 530 781 557 6592 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 138 1 0 0 0 0 0 0 16 3 0 0 22 1 0 0] 98 2 7760 6704 2608 0 27 2608 0 27 2608 0 27 )! !
!MCPatchBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

