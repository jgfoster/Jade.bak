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
	super model: anMCPatch gciSession.
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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1601 1201 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 608 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 2879 21 530 1201 801 416 706 8 #updateMenuBar 608 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 98 3 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 896 0 0 0 7 0 0 0 896 852230 ##(Smalltalk.FramingLayout)  234 240 98 4 410 8 ##(Smalltalk.ListView)  98 30 0 896 98 2 8 1409355853 1025 1040 590662 2 ##(Smalltalk.ListModel)  202 208 608 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #inspectLine 8 'Inspect' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 1040 0 8 4294904169 459270 ##(Smalltalk.Message)  8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 4 920646 5 ##(Smalltalk.ListViewColumn)  8 'Type' 201 8 #left 1410 1440 98 0 1410 8 #<= 1632 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  2 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each typeString]' 8 #[30 105 226 0 106] 8 #typeString 1696 7 257 0 0 1040 0 1 0 0 1554 8 'Class' 201 1600 1410 1440 1456 8 ##(Smalltalk.SortedCollection)  1682 0 0 1714 2 1 1744 8 'doIt' 8 '[:each | each className]' 8 #[30 105 226 0 106] 8 #className 1888 7 257 0 0 1040 0 1 0 0 1554 8 'Selector' 201 1600 1410 1440 1632 1410 1664 1632 1682 0 0 1714 2 1 1744 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] 8 #selector 2048 7 257 0 0 1040 0 1 0 0 1554 8 'Detail' 569 1600 1410 1440 1632 1410 1664 1632 1682 0 0 1714 2 1 1744 8 'doIt' 8 '[:each | each detailsString]' 8 #[30 105 226 0 106] 8 #detailsString 2208 7 257 0 0 1040 0 3 0 0 8 #report 608 0 131169 0 0 642 202 208 98 3 706 736 98 2 530 1 41 530 1169 313 1040 706 8 #contextMenu: 98 1 1248 1040 706 8 #text: 98 1 8 'Type' 1040 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 72 2 0 0 176 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 2642 8 #fixedParentRight 1 2642 8 #fixedParentTop 41 2642 8 #fixedParentBottom 1 410 8 ##(Smalltalk.CheckBox)  98 16 0 896 98 2 8 1409363203 1 2784 721990 2 ##(Smalltalk.ValueHolder)  0 0 1114118 ##(Smalltalk.NeverSearchPolicy)  32 0 0 7 0 0 0 2784 0 8 4294904245 852486 ##(Smalltalk.NullConverter)  0 0 0 642 202 208 98 2 706 736 98 2 530 5 1 530 511 41 2784 706 2496 98 1 8 'Include methods with identical source' 2784 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 0 0 0 0 1 1 0 0 20 0 0 0] 98 0 2592 0 27 2610 2656 5 2642 8 #fixedViewLeft 511 2720 1 2642 8 #fixedViewTop 41 234 256 98 4 1040 8 'operationList' 2784 8 'includeIdenticalSource' 0 642 202 208 98 1 706 736 98 2 530 1 1 530 1169 353 896 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 176 0 0 0] 98 2 2784 1040 2592 0 27 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 3488 0 482 8 4278190080 0 519 0 0 0 3488 642 202 208 98 1 706 736 98 2 530 1 353 530 1169 19 3488 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 176 0 0 0 72 2 0 0 185 0 0 0] 98 0 2592 0 27 410 912 98 15 0 416 98 2 8 1140850688 131073 3760 0 0 0 7 0 0 0 3760 562 234 240 608 32 234 256 608 0 642 202 208 98 1 706 736 98 2 530 1 371 530 1169 355 3760 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 185 0 0 0 72 2 0 0 106 1 0 0] 98 3 410 912 98 15 0 3760 98 2 8 1140850688 131073 4032 0 0 0 7 0 0 0 4032 978 234 240 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 4032 98 2 8 1140916352 1025 4144 0 482 8 4278190080 0 7 0 0 0 4144 0 8 4294904227 2946 0 0 3 642 202 208 98 3 706 736 98 2 530 1 1 530 575 39 4144 706 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 4144 706 8 #isTextModified: 98 1 32 4144 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 31 1 0 0 19 0 0 0] 98 0 2592 0 27 2610 2656 1 2688 1 2720 1 3232 39 410 8 ##(Smalltalk.ScintillaView)  98 46 0 4032 98 2 8 1445007428 1025 4592 2866 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 482 8 4278190080 0 7 0 0 0 4592 0 8 4294904201 2946 0 0 11 0 234 256 98 42 8 #lineNumber 1182726 ##(Smalltalk.ScintillaTextStyle)  67 0 0 1 0 0 0 0 4816 0 0 0 8 #specialSelector 4834 33 196934 1 ##(Smalltalk.RGB)  16646145 0 3 0 0 0 0 4864 0 0 0 8 #global 4834 21 0 0 3 0 0 0 0 4928 0 0 0 8 #normal 4834 1 0 0 1 0 0 0 0 4960 0 0 0 8 #boolean 4834 13 4912 0 3 0 0 0 0 4992 0 0 0 8 #special 4834 25 0 0 3 0 0 0 0 5024 0 0 0 8 #number 4834 5 4898 16711169 0 1 0 0 0 0 5056 0 0 0 8 #nil 4834 19 4912 0 3 0 0 0 0 5104 0 0 0 8 #character 4834 31 4898 16646399 0 3 0 0 0 0 5136 0 0 0 8 #braceHighlight 4834 69 786694 ##(Smalltalk.IndexedColor)  33554465 0 3 0 0 0 0 5184 0 0 0 8 #indentGuide 4834 75 5218 33554447 0 1 0 0 0 0 5248 0 0 0 8 #string 4834 3 4898 16646399 0 129 0 0 0 0 5296 0 0 0 8 #symbol 4834 9 5218 33554443 0 1 0 0 0 0 5344 0 0 0 8 #super 4834 17 4912 0 3 0 0 0 0 5392 0 0 0 8 #comment 4834 7 4898 65025 0 1 0 0 0 0 5424 0 0 0 8 #binary 4834 11 5218 33554433 0 1 0 0 0 0 5472 0 0 0 8 #assignment 4834 29 0 0 3 0 0 0 0 5520 0 0 0 8 #keywordSend 4834 27 5218 33554437 0 3 0 0 0 0 5552 0 0 0 8 #return 4834 23 4898 321 0 3 0 0 0 0 5600 0 0 0 8 #braceMismatch 4834 71 5218 33554459 0 3 0 0 0 0 5648 0 0 0 8 #self 4834 15 4912 0 3 0 0 0 0 5696 0 0 0 98 40 4976 5312 5072 5440 5360 5488 5008 5712 5408 5120 4944 5616 5040 5568 5536 5152 4880 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4848 5200 5664 0 5264 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  4960 234 256 98 8 8 #changed 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  7 45 5504 4898 16908287 4592 5808 8 #removed 5826 5 45 5504 4898 16843263 4592 5872 8 #circle 5826 1 1 5504 5218 33554471 4592 5920 8 #added 5826 3 45 5504 4898 16908033 4592 5968 202 208 608 0 63 9215 0 0 0 0 5280 0 0 0 0 0 0 8 '' 3 234 256 98 2 8 #container 234 256 98 2 4960 4834 1 0 0 1 0 0 0 0 4960 0 0 0 0 0 0 0 1 0 234 256 98 6 1 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  1 4592 65025 3 32 1 0 3 6178 3 4592 33423361 5 32 3 0 5 6178 5 4592 511 1 32 5 0 642 202 208 98 9 706 736 98 2 530 1 41 530 575 315 4592 706 4416 98 1 4450 3 1 3 4592 706 4496 98 1 32 4592 706 8 #modificationEventMask: 98 1 9215 4592 706 8 #margins: 98 1 98 3 984582 ##(Smalltalk.ScintillaMargin)  1 4592 1 3 32 1 6546 3 4592 33 1 32 67108863 6546 5 4592 1 1 16 -67108863 4592 706 8 #indentationGuides: 98 1 0 4592 706 8 #tabIndents: 98 1 16 4592 706 8 #tabWidth: 98 1 9 4592 706 8 #setLexerLanguage: 98 1 8 #smalltalk 4592 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 31 1 0 0 177 0 0 0] 98 0 2592 0 27 2610 2656 1 2688 1 2720 41 2752 1 234 256 98 4 4144 8 'leftText' 4592 8 'leftMemo' 0 642 202 208 98 1 706 736 98 2 530 1 1 530 575 355 4032 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 31 1 0 0 177 0 0 0] 98 2 4144 4592 2592 0 27 410 3504 98 12 0 3760 98 2 8 1140850688 1 7104 0 482 3584 0 519 0 0 0 7104 642 202 208 98 1 706 736 98 2 530 575 1 530 19 355 7104 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 31 1 0 0 0 0 0 0 40 1 0 0 177 0 0 0] 98 0 2592 0 27 410 912 98 15 0 3760 98 2 8 1140850688 131073 7344 0 0 0 7 0 0 0 7344 978 234 240 98 4 410 4608 98 46 0 7344 98 2 8 1445007428 1025 7456 2866 0 32 4704 0 482 4736 0 7 0 0 0 7456 0 8 4294904201 2946 0 0 11 0 234 256 98 42 4816 4834 67 0 0 1 0 0 0 0 4816 0 0 0 4864 4834 33 4912 0 3 0 0 0 0 4864 0 0 0 4928 4834 21 0 0 3 0 0 0 0 4928 0 0 0 4960 4834 1 0 0 1 0 0 0 0 4960 0 0 0 4992 4834 13 4912 0 3 0 0 0 0 4992 0 0 0 5024 4834 25 0 0 3 0 0 0 0 5024 0 0 0 5056 4834 5 5088 0 1 0 0 0 0 5056 0 0 0 5104 4834 19 4912 0 3 0 0 0 0 5104 0 0 0 5136 4834 31 5168 0 3 0 0 0 0 5136 0 0 0 5184 4834 69 5232 0 3 0 0 0 0 5184 0 0 0 5248 4834 75 5280 0 1 0 0 0 0 5248 0 0 0 5296 4834 3 5328 0 129 0 0 0 0 5296 0 0 0 5344 4834 9 5376 0 1 0 0 0 0 5344 0 0 0 5392 4834 17 4912 0 3 0 0 0 0 5392 0 0 0 5424 4834 7 5456 0 1 0 0 0 0 5424 0 0 0 5472 4834 11 5504 0 1 0 0 0 0 5472 0 0 0 5520 4834 29 0 0 3 0 0 0 0 5520 0 0 0 5552 4834 27 5584 0 3 0 0 0 0 5552 0 0 0 5600 4834 23 5632 0 3 0 0 0 0 5600 0 0 0 5648 4834 71 5680 0 3 0 0 0 0 5648 0 0 0 5696 4834 15 4912 0 3 0 0 0 0 5696 0 0 0 98 40 7664 7792 7712 7840 7808 7856 7680 7936 7824 7728 7648 7904 7696 7888 7872 7744 7632 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7616 7760 7920 0 7776 0 0 5746 4960 234 256 98 8 5808 5826 7 45 5504 4898 16908287 7456 5808 5872 5826 5 45 5504 4898 16843263 7456 5872 5920 5826 1 1 5504 5952 7456 5920 5968 5826 3 45 5504 4898 16908033 7456 5968 202 208 608 0 63 9215 0 0 0 0 5280 0 0 0 0 0 0 8 '' 3 234 256 98 2 6080 234 256 98 2 4960 4834 1 0 0 1 0 0 0 0 4960 0 0 0 0 0 0 0 1 0 234 256 98 6 1 6178 1 7456 65025 3 32 1 0 3 6178 3 7456 33423361 5 32 3 0 5 6178 5 7456 511 1 32 5 0 642 202 208 98 9 706 736 98 2 530 1 41 530 577 315 7456 706 4416 98 1 4450 3 1 3 7456 706 4496 98 1 32 7456 706 6448 98 1 9215 7456 706 6496 98 1 98 3 6546 1 7456 1 3 32 1 6546 3 7456 33 1 32 67108863 6546 5 7456 1 1 16 -67108863 7456 706 6624 98 1 0 7456 706 6672 98 1 16 7456 706 6720 98 1 9 7456 706 6768 98 1 6800 7456 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 32 1 0 0 177 0 0 0] 98 0 2592 0 27 2610 2656 1 2688 1 2720 41 2752 1 410 4160 98 16 0 7344 98 2 8 1140916352 1025 8832 0 482 4240 0 7 0 0 0 8832 0 8 4294904227 2946 0 0 3 642 202 208 98 3 706 736 98 2 530 1 1 530 577 39 8832 706 4416 98 1 4450 3 1 3 8832 706 4496 98 1 32 8832 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 32 1 0 0 19 0 0 0] 98 0 2592 0 27 2610 2656 1 2688 1 2720 1 3232 39 234 256 98 4 7456 8 'rightMemo' 8832 8 'rightText' 0 642 202 208 98 1 706 736 98 2 530 593 1 530 577 355 7344 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 40 1 0 0 0 0 0 0 72 2 0 0 177 0 0 0] 98 2 8832 7456 2592 0 27 2592 0 27 2592 0 27 )! !
!MCPatchBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

