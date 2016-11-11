"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #MCPatchBrowser
	instanceVariableNames: 'patch includeIdenticalPresenter operationListPresenter leftTextPresenter leftMemoPresenter rightTextPresenter rightMemoPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCPatchBrowser guid: (GUID fromString: '{6A36F578-7C9D-4ACE-A3B0-D2E1313FBBCA}')!
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout) 234 240 98 0 16 234 256 608 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 2 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 530 2559 21 530 1601 1201 416 706 8 #updateMenuBar 608 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 31 8 0 0 98 2 0 0] 98 3 410 8 ##(Smalltalk.ContainerView) 98 15 0 416 98 2 8 1140850688 131073 896 0 0 0 7 0 0 0 896 852230 ##(Smalltalk.FramingLayout) 234 240 98 4 410 8 ##(Smalltalk.ListView) 98 30 0 896 98 2 8 1409355853 1025 1040 590662 2 ##(Smalltalk.ListModel) 202 208 608 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 482 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu) 0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) 8 #inspectLine 8 'Inspect' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 1040 0 8 4294904667 459270 ##(Smalltalk.Message) 8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 202 208 98 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Type' 201 8 #left 1410 1440 98 0 1410 8 #<= 1632 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 2 1 8 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each typeString]' 8 #[30 105 226 0 106] 8 #typeString 1696 7 257 0 0 1040 0 1 0 0 1554 8 'Class' 201 1600 1410 1440 1456 8 ##(Smalltalk.SortedCollection) 1682 0 0 1714 2 1 1744 8 'doIt' 8 '[:each | each className]' 8 #[30 105 226 0 106] 8 #className 1888 7 257 0 0 1040 0 1 0 0 1554 8 'Selector' 201 1600 1410 1440 1632 1410 1664 1632 1682 0 0 1714 2 1 1744 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] 8 #selector 2048 7 257 0 0 1040 0 1 0 0 1554 8 'Detail' 961 1600 1410 1440 1632 1410 1664 1632 1682 0 0 1714 2 1 1744 8 'doIt' 8 '[:each | each detailsString]' 8 #[30 105 226 0 106] 8 #detailsString 2208 7 257 0 0 1040 0 3 0 0 8 #report 608 0 131169 0 98 1 0 642 202 208 98 3 706 736 98 2 530 1 41 530 1569 517 1040 706 8 #contextMenu: 98 1 1248 1040 706 8 #text: 98 1 8 'Type' 1040 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 16 3 0 0 22 1 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) 8 #fixedParentLeft 1 2658 8 #fixedParentRight 1 2658 8 #fixedParentTop 41 2658 8 #fixedParentBottom 1 410 8 ##(Smalltalk.CheckBox) 98 16 0 896 98 2 8 1409363203 1 2800 721990 2 ##(Smalltalk.ValueHolder) 0 0 1114118 ##(Smalltalk.NeverSearchPolicy) 32 0 0 7 0 0 0 2800 0 8 4294904777 852486 ##(Smalltalk.NullConverter) 0 0 0 642 202 208 98 2 706 736 98 2 530 5 1 530 511 41 2800 706 2512 98 1 8 'Include methods with identical source' 2800 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 0 0 0 0 1 1 0 0 20 0 0 0] 98 0 2608 0 27 2626 2672 5 2658 8 #fixedViewLeft 511 2736 1 2658 8 #fixedViewTop 41 234 256 98 4 1040 8 'operationList' 2800 8 'includeIdenticalSource' 0 642 202 208 98 1 706 736 98 2 530 1 1 530 1569 557 896 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 22 1 0 0] 98 2 2800 1040 2608 0 27 410 8 ##(Smalltalk.Splitter) 98 12 0 416 98 2 8 1140850688 1 3504 0 482 8 4278190080 0 519 0 0 0 3504 642 202 208 98 1 706 736 98 2 530 1 557 530 1569 11 3504 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 22 1 0 0 16 3 0 0 27 1 0 0] 98 0 2608 0 27 410 912 98 15 0 416 98 2 8 1140850688 131073 3776 0 0 0 7 0 0 0 3776 562 234 240 608 32 234 256 608 0 642 202 208 98 1 706 736 98 2 530 1 567 530 1569 557 3776 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 1 0 0 16 3 0 0 49 2 0 0] 98 3 410 912 98 15 0 3776 98 2 8 1140850688 131073 4048 0 0 0 7 0 0 0 4048 978 234 240 98 4 410 8 ##(Smalltalk.TextEdit) 98 16 0 4048 98 2 8 1140916352 1025 4160 0 482 8 4278190080 0 7 0 0 0 4160 0 8 4294904281 2962 0 0 3 642 202 208 98 1 706 736 98 2 530 1 1 530 779 39 4160 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 133 1 0 0 19 0 0 0] 98 0 2608 0 27 2626 2672 1 2704 1 2736 1 3248 39 410 8 ##(Smalltalk.ScintillaView) 98 50 0 4048 98 2 8 1445007428 1025 4480 2882 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 482 8 4278190080 0 7 0 0 0 4480 0 8 4294904307 2962 0 0 11 0 234 256 98 44 8 #character 1182790 1 ##(Smalltalk.ScintillaTextStyle) 31 196934 1 ##(Smalltalk.RGB) 16646399 0 3 0 0 0 0 4704 0 1401 0 8 #self 4722 15 4754 16646145 0 3 0 0 0 0 4784 0 1401 0 8 #keywordSend 4722 27 786694 ##(Smalltalk.IndexedColor) 33554437 0 3 0 0 0 0 4832 0 1401 0 8 #indentGuide 4722 75 4866 33554447 0 1 0 0 0 0 4896 0 0 0 8 #global 4722 21 0 0 3 0 0 0 0 4944 0 1401 0 8 #specialSelector 4722 33 4816 0 3 0 0 0 0 4976 0 1401 0 8 #symbol 4722 9 4866 33554443 0 1 0 0 0 0 5008 0 0 0 8 #number 4722 5 4754 16711169 0 1 0 0 0 0 5056 0 0 0 8 #super 4722 17 4816 0 3 0 0 0 0 5104 0 1401 0 8 #binary 4722 11 4866 33554433 0 1 0 0 0 0 5136 0 0 0 8 #braceMismatch 4722 71 4866 33554459 0 3 0 0 0 0 5184 0 1401 0 8 #normal 4722 1 0 0 1 0 0 0 0 5232 0 0 0 8 #assignment 4722 29 0 0 3 0 0 0 0 5264 0 1401 0 8 #return 4722 23 4754 321 0 3 0 0 0 0 5296 0 1401 0 8 #nil 4722 19 4816 0 3 0 0 0 0 5344 0 1401 0 8 #callTip 4722 77 4866 33554457 4866 33554471 1 0 0 0 0 5376 0 0 0 8 #lineNumber 4722 67 0 0 1 0 0 0 0 5440 0 0 0 8 #string 4722 3 4754 16646399 0 129 0 0 0 0 5472 0 0 0 8 #comment 4722 7 4754 65025 0 1 0 0 0 0 5520 0 0 0 8 #boolean 4722 13 4816 0 3 0 0 0 0 5568 0 1401 0 8 #braceHighlight 4722 69 4866 33554465 0 3 0 0 0 0 5600 0 1401 0 8 #special 4722 25 0 0 3 0 0 0 0 5648 0 1401 0 98 40 5248 5488 5072 5536 5024 5152 5584 4800 5120 5360 4960 5312 5664 4848 5280 4736 4992 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 5456 5616 5200 0 4912 0 0 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 0 0 32 202 208 608 234 256 98 8 8 #added 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 3 45 0 0 4480 5776 0 0 0 0 0 0 8 #changed 5794 7 45 0 0 4480 5824 0 0 0 0 0 0 8 #removed 5794 5 45 0 0 4480 5856 0 0 0 0 0 0 8 #circle 5794 1 0 0 0 4480 5888 0 0 0 0 0 0 202 208 608 0 63 9215 0 0 0 0 4928 0 0 65 0 0 234 256 98 6 8 #specialCharacter 8 '()[]<>' 8 #literalBytes 8 '[]' 8 #literalArray 8 '()' 8 '' 3 234 256 98 2 8 #container 234 256 98 2 5232 4722 1 0 0 1 0 0 0 0 5232 0 0 0 0 0 0 0 1 0 234 256 98 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 4480 65025 3 32 1 0 0 0 0 0 0 3 6210 3 4480 33423361 5 32 3 0 0 0 0 0 0 5 6210 5 4480 511 1 32 5 0 0 0 0 0 0 0 0 234 256 98 6 8 #Notification 4722 1027 0 4754 31185883 1 0 0 0 0 6304 0 0 0 8 #Error 4722 1029 4754 335 4754 31317503 1 0 0 0 0 6352 0 0 0 8 #Warning 4722 1031 4754 60239 4754 31326207 1 0 0 0 0 6416 0 0 0 0 642 202 208 98 7 706 736 98 2 530 1 41 530 779 517 4480 706 8 #modificationEventMask: 98 1 9215 4480 706 8 #caretPeriod: 98 1 1061 4480 706 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin) 1 4480 1 3 32 1 15 6754 3 4480 33 1 32 67108863 15 6754 5 4480 1 1 16 -67108863 15 4480 706 8 #markers: 98 1 5920 4480 706 8 #tabWidth: 98 1 9 4480 706 8 #setLexerLanguage: 98 1 8 #smalltalk 4480 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 133 1 0 0 22 1 0 0] 98 0 2608 0 29 2626 2672 1 2704 1 2736 41 2768 1 234 256 98 4 4160 8 'leftText' 4480 8 'leftMemo' 0 642 202 208 98 1 706 736 98 2 530 1 1 530 779 557 4048 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 133 1 0 0 22 1 0 0] 98 2 4160 4480 2608 0 27 410 3520 98 12 0 3776 98 2 8 1140850688 1 7264 0 482 3600 0 519 0 0 0 7264 642 202 208 98 1 706 736 98 2 530 779 1 530 11 557 7264 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 133 1 0 0 0 0 0 0 138 1 0 0 22 1 0 0] 98 0 2608 0 27 410 912 98 15 0 3776 98 2 8 1140850688 131073 7504 0 0 0 7 0 0 0 7504 978 234 240 98 4 410 4496 98 50 0 7504 98 2 8 1445007428 1025 7616 2882 0 32 4592 0 482 4624 0 7 0 0 0 7616 0 8 4294904307 2962 0 0 11 0 234 256 98 44 4704 4722 31 4768 0 3 0 0 0 0 4704 0 1401 0 4784 4722 15 4816 0 3 0 0 0 0 4784 0 1401 0 4832 4722 27 4880 0 3 0 0 0 0 4832 0 1401 0 4896 4722 75 4928 0 1 0 0 0 0 4896 0 0 0 4944 4722 21 0 0 3 0 0 0 0 4944 0 1401 0 4976 4722 33 4816 0 3 0 0 0 0 4976 0 1401 0 5008 4722 9 5040 0 1 0 0 0 0 5008 0 0 0 5056 4722 5 5088 0 1 0 0 0 0 5056 0 0 0 5104 4722 17 4816 0 3 0 0 0 0 5104 0 1401 0 5136 4722 11 5168 0 1 0 0 0 0 5136 0 0 0 5184 4722 71 5216 0 3 0 0 0 0 5184 0 1401 0 5232 4722 1 0 0 1 0 0 0 0 5232 0 0 0 5264 4722 29 0 0 3 0 0 0 0 5264 0 1401 0 5296 4722 23 5328 0 3 0 0 0 0 5296 0 1401 0 5344 4722 19 4816 0 3 0 0 0 0 5344 0 1401 0 5376 4722 77 5408 5424 1 0 0 0 0 5376 0 0 0 5440 4722 67 0 0 1 0 0 0 0 5440 0 0 0 5472 4722 3 5504 0 129 0 0 0 0 5472 0 0 0 5520 4722 7 5552 0 1 0 0 0 0 5520 0 0 0 5568 4722 13 4816 0 3 0 0 0 0 5568 0 1401 0 5600 4722 69 5632 0 3 0 0 0 0 5600 0 1401 0 5648 4722 25 0 0 3 0 0 0 0 5648 0 1401 0 98 40 7952 8048 7888 8064 7872 7920 8080 7792 7904 8000 7840 7984 8112 7808 7968 7776 7856 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8032 8096 7936 0 7824 0 0 5698 1 0 0 32 202 208 608 234 256 98 8 5776 5794 3 45 0 0 7616 5776 0 0 0 0 0 0 5824 5794 7 45 0 0 7616 5824 0 0 0 0 0 0 5856 5794 5 45 0 0 7616 5856 0 0 0 0 0 0 5888 5794 1 0 0 0 7616 5888 0 0 0 0 0 0 202 208 608 0 63 9215 0 0 0 0 4928 0 0 65 0 0 5936 8 '' 3 234 256 98 2 6112 234 256 98 2 5232 4722 1 0 0 1 0 0 0 0 5232 0 0 0 0 0 0 0 1 0 234 256 98 6 1 6210 1 7616 65025 3 32 1 0 0 0 0 0 0 3 6210 3 7616 33423361 5 32 3 0 0 0 0 0 0 5 6210 5 7616 511 1 32 5 0 0 0 0 0 0 0 0 234 256 98 6 6304 4722 1027 0 4754 31185883 1 0 0 0 0 6304 0 0 0 6352 4722 1029 4754 335 4754 31317503 1 0 0 0 0 6352 0 0 0 6416 4722 1031 4754 60239 4754 31326207 1 0 0 0 0 6416 0 0 0 0 642 202 208 98 7 706 736 98 2 530 1 41 530 781 517 7616 706 6608 98 1 9215 7616 706 6656 98 1 1061 7616 706 6704 98 1 98 3 6754 1 7616 1 3 32 1 15 6754 3 7616 33 1 32 67108863 15 6754 5 7616 1 1 16 -67108863 15 7616 706 6832 98 1 8272 7616 706 6880 98 1 9 7616 706 6928 98 1 6960 7616 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 134 1 0 0 22 1 0 0] 98 0 2608 0 29 2626 2672 1 2704 1 2736 41 2768 1 410 4176 98 16 0 7504 98 2 8 1140916352 1025 9056 0 482 4256 0 7 0 0 0 9056 0 8 4294904281 2962 0 0 3 642 202 208 98 1 706 736 98 2 530 1 1 530 781 39 9056 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 134 1 0 0 19 0 0 0] 98 0 2608 0 27 2626 2672 1 2704 1 2736 1 3248 39 234 256 98 4 7616 8 'rightMemo' 9056 8 'rightText' 0 642 202 208 98 1 706 736 98 2 530 789 1 530 781 557 7504 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 138 1 0 0 0 0 0 0 16 3 0 0 22 1 0 0] 98 2 9056 7616 2608 0 27 2608 0 27 2608 0 27 )! !
!MCPatchBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

