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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13565952 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 592 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 5119 21 514 1601 1201 416 690 #updateMenuBar 592 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 31 13 0 0 98 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 848 0 0 0 7 0 0 0 848 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 0 848 34 2 8 1409355853 1025 976 590662 2 ##(Smalltalk.ListModel) 138 144 592 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu) 0 true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #inspectLine 8 'Inspect' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 976 0 8 4294904383 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 138 144 34 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Type' 201 #left 1314 #displayString 8 #() 1314 #<= 1488 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each typeString]' 8 #[30 105 226 0 106] #typeString 1536 7 257 0 0 976 0 1 0 0 1426 8 'Class' 201 #left 1314 #displayString 1344 ##(Smalltalk.SortedCollection) 1522 0 0 1554 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each className]' 8 #[30 105 226 0 106] #className 1680 7 257 0 0 976 0 1 0 0 1426 8 'Selector' 201 #left 1314 #displayString 1488 1314 #<= 1488 1522 0 0 1554 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 1824 7 257 0 0 976 0 1 0 0 1426 8 'Detail' 961 #left 1314 #displayString 1488 1314 #<= 1488 1522 0 0 1554 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each detailsString]' 8 #[30 105 226 0 106] #detailsString 1968 7 257 0 0 976 0 3 0 0 #report 592 0 131169 0 8 #(nil) 626 138 144 34 3 690 #createAt:extent: 34 2 514 1 41 514 1569 517 976 690 #contextMenu: 34 1 1168 976 690 #text: 34 1 8 'Type' 976 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 16 3 0 0 22 1 0 0] 8 #() 514 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2354 #fixedParentRight 1 2354 #fixedParentTop 41 2354 #fixedParentBottom 1 410 ##(Smalltalk.CheckBox) 34 16 0 848 34 2 8 1409363203 1 2432 721990 2 ##(Smalltalk.ValueHolder) 0 0 1114118 ##(Smalltalk.NeverSearchPolicy) false 0 0 7 0 0 0 2432 0 8 4294903089 852486 ##(Smalltalk.NullConverter) 0 0 0 626 138 144 34 2 690 #createAt:extent: 34 2 514 5 1 514 511 41 2432 690 #text: 34 1 8 'Include methods with identical source' 2432 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 0 0 0 0 1 1 0 0 20 0 0 0] 8 #() 2304 0 27 2322 2368 5 2354 #fixedViewLeft 511 2400 1 2354 #fixedViewTop 41 170 192 34 4 976 8 'operationList' 2432 8 'includeIdenticalSource' 0 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 1569 557 848 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 22 1 0 0] 34 2 2432 976 2304 0 27 410 ##(Smalltalk.Splitter) 34 12 0 416 34 2 8 1140850688 1 3088 0 466 8 4278190080 0 519 0 0 0 3088 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 557 514 1569 11 3088 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 22 1 0 0 16 3 0 0 27 1 0 0] 8 #() 2304 0 27 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 3344 0 0 0 7 0 0 0 3344 546 170 176 592 false 170 192 592 0 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 567 514 1569 557 3344 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 1 0 0 16 3 0 0 49 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 0 3344 34 2 8 1140850688 131073 3616 0 0 0 7 0 0 0 3616 914 170 176 34 4 410 ##(Smalltalk.ScintillaView) 34 50 0 3616 34 2 8 1445007428 1025 3728 2498 0 false 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 466 8 4278190080 0 7 0 263174 ##(Smalltalk.Font) 0 true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 0 3728 0 8 4294905729 2578 0 0 11 0 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 0 0 1 0 0 0 0 #normal 0 0 0 #callTip 4034 77 786694 ##(Smalltalk.IndexedColor) 33554457 4082 33554471 1 0 0 0 0 #callTip 0 0 0 34 256 4048 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4064 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 0 0 false 138 144 592 170 192 34 8 #added 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 3 45 0 0 3728 #added 0 0 0 0 0 0 #changed 4226 7 45 0 0 3728 #changed 0 0 0 0 0 0 #removed 4226 5 45 0 0 3728 #removed 0 0 0 0 0 0 #circle 4226 1 0 0 0 3728 #circle 0 0 0 0 0 0 138 ##(Smalltalk.IdentitySet) 592 0 170 176 592 9215 0 0 0 0 4082 33554447 0 0 65 0 0 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 4048 0 0 0 0 1 0 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 3728 65025 3 false 1 0 0 0 0 0 0 3 4546 3 3728 33423361 5 false 3 0 0 0 0 0 0 5 4546 5 3728 511 1 false 5 0 0 0 0 0 0 0 0 170 192 34 6 #Notification 4034 1027 0 196934 1 ##(Smalltalk.RGB) 31185883 1 0 0 0 0 #Notification 0 0 0 #Warning 4034 1031 4658 60239 4658 31326207 1 0 0 0 0 #Warning 0 0 0 #Error 4034 1029 4658 335 4658 31317503 1 0 0 0 0 #Error 0 0 0 0 626 138 144 34 5 690 #createAt:extent: 34 2 514 1 41 514 779 517 3728 690 #modificationEventMask: 8 #(4607) 3728 690 #caretPeriod: 8 #(530) 3728 690 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 3728 1 3 0 0 0 0 5010 3 3728 33 0 0 67108863 0 0 5010 5 3728 1 0 3 -67108863 0 0 3728 690 #tabWidth: 8 #(4) 3728 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 133 1 0 0 22 1 0 0] 8 #() 2304 0 29 2322 2368 1 2384 1 2400 41 2416 1 410 ##(Smalltalk.TextEdit) 34 16 0 3616 34 2 8 1140916352 1025 5168 0 466 8 4278190080 0 7 0 0 0 5168 0 8 4294904453 2578 0 0 3 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 779 39 5168 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 133 1 0 0 19 0 0 0] 8 #() 2304 0 27 2322 2368 1 2384 1 2400 1 2848 39 170 192 34 4 3728 8 'leftMemo' 5168 8 'leftText' 0 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 779 557 3616 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 133 1 0 0 22 1 0 0] 34 2 5168 3728 2304 0 27 410 ##(Smalltalk.Splitter) 34 12 0 3344 34 2 8 1140850688 1 5696 0 466 3168 0 519 0 0 0 5696 626 138 144 34 1 690 #createAt:extent: 34 2 514 779 1 514 11 557 5696 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 133 1 0 0 0 0 0 0 138 1 0 0 22 1 0 0] 8 #() 2304 0 27 410 ##(Smalltalk.ContainerView) 34 15 0 3344 34 2 8 1140850688 131073 5936 0 0 0 7 0 0 0 5936 914 170 176 34 4 410 ##(Smalltalk.TextEdit) 34 16 0 5936 34 2 8 1140916352 1025 6048 0 466 5248 0 7 0 0 0 6048 0 8 4294904453 2578 0 0 3 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 781 39 6048 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 134 1 0 0 19 0 0 0] 8 #() 2304 0 27 2322 2368 1 2384 1 2400 1 2848 39 410 ##(Smalltalk.ScintillaView) 34 50 0 5936 34 2 8 1445007428 1025 6336 2498 0 false 3824 0 466 3856 0 7 0 3874 0 true 3906 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 0 6336 0 8 4294905729 2578 0 0 11 0 170 192 34 4 #normal 4034 1 0 0 1 0 0 0 0 #normal 0 0 0 #callTip 4034 77 4096 4112 1 0 0 0 0 #callTip 0 0 0 34 256 6560 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6576 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4146 1 0 0 false 138 144 592 170 192 34 8 #added 4226 3 45 0 0 6336 #added 0 0 0 0 0 0 #changed 4226 7 45 0 0 6336 #changed 0 0 0 0 0 0 #removed 4226 5 45 0 0 6336 #removed 0 0 0 0 0 0 #circle 4226 1 0 0 0 6336 #circle 0 0 0 0 0 0 138 ##(Smalltalk.IdentitySet) 592 0 170 176 592 9215 0 0 0 0 4336 0 0 65 0 0 4352 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 6560 0 0 0 0 1 0 170 192 34 6 1 4546 1 6336 65025 3 false 1 0 0 0 0 0 0 3 4546 3 6336 33423361 5 false 3 0 0 0 0 0 0 5 4546 5 6336 511 1 false 5 0 0 0 0 0 0 0 0 170 192 34 6 #Notification 4034 1027 0 4658 31185883 1 0 0 0 0 #Notification 0 0 0 #Warning 4034 1031 4658 60239 4658 31326207 1 0 0 0 0 #Warning 0 0 0 #Error 4034 1029 4658 335 4658 31317503 1 0 0 0 0 #Error 0 0 0 0 626 138 144 34 5 690 #createAt:extent: 34 2 514 1 41 514 781 517 6336 690 #modificationEventMask: 8 #(4607) 6336 690 #caretPeriod: 8 #(530) 6336 690 #margins: 34 1 34 3 5010 1 6336 1 3 0 0 0 0 5010 3 6336 33 0 0 67108863 0 0 5010 5 6336 1 0 3 -67108863 0 0 6336 690 #tabWidth: 8 #(4) 6336 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 134 1 0 0 22 1 0 0] 8 #() 2304 0 29 2322 2368 1 2384 1 2400 41 2416 1 170 192 34 4 6048 8 'rightText' 6336 8 'rightMemo' 0 626 138 144 34 1 690 #createAt:extent: 34 2 514 789 1 514 781 557 5936 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 138 1 0 0 0 0 0 0 16 3 0 0 22 1 0 0] 34 2 6048 6336 2304 0 27 2304 0 27 2304 0 27 )! !
!MCPatchBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

