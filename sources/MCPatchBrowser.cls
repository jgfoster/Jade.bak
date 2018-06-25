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
	operationListPresenter 	:= self add: ListPresenter 		new name: 'operationList'.
	leftTextPresenter		:= self add: TextPresenter		new name: 'leftText'.
	leftMemoPresenter		:= self add: TextPresenter		new name: 'leftMemo'.
	rightTextPresenter		:= self add: TextPresenter		new name: 'rightText'.
	rightMemoPresenter		:= self add: TextPresenter		new name: 'rightMemo'.
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
	leftTextPresenter 	value: ''.
	leftMemoPresenter	value: ''.
	rightTextPresenter	value: ''.
	rightMemoPresenter	value: ''.
	operationListPresenter hasSelection ifFalse: [^self].
	operation := operationListPresenter selection.
	leftTextPresenter 	value: operation obsoletionText.
	leftMemoPresenter	value: operation obsoletionMemo asUtf8String.
	rightTextPresenter	value: operation modificationText.
	rightMemoPresenter	value: operation modificationMemo asUtf8String.
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 551 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 592 nil nil nil nil nil 1 nil nil nil nil 1 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 2879 21 514 1601 1201 416 690 #updateMenuBar 592 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 191 8 0 0 98 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 848 nil nil nil 7 nil nil nil 848 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 848 34 2 8 1409355853 1025 976 590662 2 ##(Smalltalk.ListModel) 138 144 592 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #inspectLine 8 'Inspect' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 976 nil 8 1795483760 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Type' 201 #left 1314 #displayString 8 #() 1314 #<= 1488 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each typeString]' 8 #[30 105 226 0 106] #typeString 1536 7 257 nil nil 976 nil 1 nil nil 1426 8 'Class' 201 #left 1314 #displayString 1344 ##(Smalltalk.SortedCollection) 1522 0 nil 1554 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each className]' 8 #[30 105 226 0 106] #className 1680 7 257 nil nil 976 nil 1 nil nil 1426 8 'Selector' 201 #left 1314 #displayString 1488 1314 #<= 1488 1522 0 nil 1554 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 1824 7 257 nil nil 976 nil 1 nil nil 1426 8 'Detail' 961 #left 1314 #displayString 1488 1314 #<= 1488 1522 0 nil 1554 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each detailsString]' 8 #[30 105 226 0 106] #detailsString 1968 7 257 nil nil 976 nil 3 nil nil #report 592 nil 131169 nil 8 #(nil) 626 138 144 34 3 690 #createAt:extent: 34 2 514 1 41 514 1569 517 976 690 #contextMenu: 34 1 1168 976 690 #text: 34 1 8 'Type' 976 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 16 3 0 0 22 1 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2354 #fixedParentRight 1 2354 #fixedParentTop 41 2354 #fixedParentBottom 1 410 ##(Smalltalk.CheckBox) 34 16 nil 848 34 2 8 1409363203 1 2432 721990 2 ##(Smalltalk.ValueHolder) nil nil 1114118 ##(Smalltalk.NeverSearchPolicy) false nil nil 7 nil nil nil 2432 nil 8 1795593280 852486 ##(Smalltalk.NullConverter) nil nil nil 626 138 144 34 2 690 #createAt:extent: 34 2 514 5 1 514 511 41 2432 690 #text: 34 1 8 'Include methods with identical source' 2432 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 0 0 0 0 1 1 0 0 20 0 0 0] 8 #() 2304 nil 27 2322 2368 5 2354 #fixedViewLeft 511 2400 1 2354 #fixedViewTop 41 170 192 34 4 976 8 'operationList' 2432 8 'includeIdenticalSource' nil 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 1569 557 848 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 22 1 0 0] 34 2 2432 976 2304 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 3088 nil 466 8 4278190080 nil 519 nil nil nil 3088 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 557 514 1569 11 3088 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 22 1 0 0 16 3 0 0 27 1 0 0] 8 #() 2304 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 3344 nil nil nil 7 nil nil nil 3344 546 170 176 592 false 170 192 592 nil 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 567 514 1569 557 3344 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 1 0 0 16 3 0 0 49 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 3344 34 2 8 1140850688 131073 3616 nil nil nil 7 nil nil nil 3616 914 170 176 34 4 410 ##(Smalltalk.ScintillaView) 34 50 nil 3616 34 2 8 1445007428 1025 3728 2498 nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 466 8 4278190080 nil 7 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 3728 nil 504331129 2578 nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 4018 77 786694 ##(Smalltalk.IndexedColor) 33554457 4066 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 4032 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 4048 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 592 170 192 34 8 #circle 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 3728 #circle nil nil nil nil nil nil #changed 4210 7 45 nil nil 3728 #changed nil nil nil nil nil nil #added 4210 3 45 nil nil 3728 #added nil nil nil nil nil nil #removed 4210 5 45 nil nil 3728 #removed nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 592 nil 170 176 592 9215 nil nil nil nil 4066 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #literalArray 8 '()' #specialCharacter 8 '()[]<>' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 4032 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 3728 65025 3 false 1 nil nil nil nil nil nil 3 4530 3 3728 33423361 5 false 3 nil nil nil nil nil nil 5 4530 5 3728 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Warning 4018 1031 196934 1 ##(Smalltalk.RGB) 60239 4642 31326207 1 nil nil nil nil #Warning nil nil nil #Error 4018 1029 4642 335 4642 31317503 1 nil nil nil nil #Error nil nil nil #Notification 4018 1027 nil 4642 31185883 1 nil nil nil nil #Notification nil nil nil nil 626 138 144 34 9 690 #createAt:extent: 34 2 514 1 41 514 779 517 3728 690 #textLimit: 8 #(1073741823) 3728 690 #modificationEventMask: 8 #(4607) 3728 690 #caretPeriod: 8 #(530) 3728 690 #sciSetEOLMode: 8 #(2) 3728 690 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 3728 1 3 nil nil nil nil 5058 3 3728 33 nil nil 67108863 nil nil 5058 5 3728 1 nil 3 -67108863 nil nil 3728 690 #tabWidth: 8 #(4) 3728 690 #maxCompletionListHeight: 8 #(9) 3728 690 #sciSetCodePage: 8 #(65001) 3728 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 133 1 0 0 22 1 0 0] 8 #() 2304 nil 29 2322 2368 1 2384 1 2400 41 2416 1 410 ##(Smalltalk.TextEdit) 34 16 nil 3616 34 2 8 1140916352 1025 5280 nil 466 8 4278190080 nil 7 nil nil nil 5280 nil 8 1795431936 2578 nil nil 3 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 779 39 5280 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 133 1 0 0 19 0 0 0] 8 #() 2304 nil 27 2322 2368 1 2384 1 2400 1 2848 39 170 192 34 4 3728 8 'leftMemo' 5280 8 'leftText' nil 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 779 557 3616 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 133 1 0 0 22 1 0 0] 34 2 5280 3728 2304 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 3344 34 2 8 1140850688 1 5808 nil 466 3168 nil 519 nil nil nil 5808 626 138 144 34 1 690 #createAt:extent: 34 2 514 779 1 514 11 557 5808 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 133 1 0 0 0 0 0 0 138 1 0 0 22 1 0 0] 8 #() 2304 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 3344 34 2 8 1140850688 131073 6048 nil nil nil 7 nil nil nil 6048 914 170 176 34 4 410 ##(Smalltalk.TextEdit) 34 16 nil 6048 34 2 8 1140916352 1025 6160 nil 466 5360 nil 7 nil nil nil 6160 nil 8 1795431936 2578 nil nil 3 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 781 39 6160 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 134 1 0 0 19 0 0 0] 8 #() 2304 nil 27 2322 2368 1 2384 1 2400 1 2848 39 410 ##(Smalltalk.ScintillaView) 34 50 nil 6048 34 2 8 1445007428 1025 6448 2498 nil false 3824 nil 466 3856 nil 7 nil 3874 nil true 3906 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 6448 nil 504331129 2578 nil nil 11 nil 170 192 34 4 #normal 4018 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 4018 77 4080 4096 1 nil nil nil nil #callTip nil nil nil 34 256 6656 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 6672 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 4130 1 nil nil false 138 144 592 170 192 34 8 #circle 4210 1 nil nil nil 6448 #circle nil nil nil nil nil nil #changed 4210 7 45 nil nil 6448 #changed nil nil nil nil nil nil #added 4210 3 45 nil nil 6448 #added nil nil nil nil nil nil #removed 4210 5 45 nil nil 6448 #removed nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 592 nil 170 176 592 9215 nil nil nil nil 4320 nil nil 65 nil nil 4336 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 6656 nil nil nil nil 1 nil 170 192 34 6 1 4530 1 6448 65025 3 false 1 nil nil nil nil nil nil 3 4530 3 6448 33423361 5 false 3 nil nil nil nil nil nil 5 4530 5 6448 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Warning 4018 1031 4642 60239 4642 31326207 1 nil nil nil nil #Warning nil nil nil #Error 4018 1029 4642 335 4642 31317503 1 nil nil nil nil #Error nil nil nil #Notification 4018 1027 nil 4642 31185883 1 nil nil nil nil #Notification nil nil nil nil 626 138 144 34 9 690 #createAt:extent: 34 2 514 1 41 514 781 517 6448 690 #textLimit: 8 #(1073741823) 6448 690 #modificationEventMask: 8 #(4607) 6448 690 #caretPeriod: 8 #(530) 6448 690 #sciSetEOLMode: 8 #(2) 6448 690 #margins: 34 1 34 3 5058 1 6448 1 3 nil nil nil nil 5058 3 6448 33 nil nil 67108863 nil nil 5058 5 6448 1 nil 3 -67108863 nil nil 6448 690 #tabWidth: 8 #(4) 6448 690 #maxCompletionListHeight: 8 #(9) 6448 690 #sciSetCodePage: 8 #(65001) 6448 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 134 1 0 0 22 1 0 0] 8 #() 2304 nil 29 2322 2368 1 2384 1 2400 41 2416 1 170 192 34 4 6160 8 'rightText' 6448 8 'rightMemo' nil 626 138 144 34 1 690 #createAt:extent: 34 2 514 789 1 514 781 557 6048 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 138 1 0 0 0 0 0 0 16 3 0 0 22 1 0 0] 34 2 6160 6448 2304 nil 27 2304 nil 27 2304 nil 27 )! !
!MCPatchBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

