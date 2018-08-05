"Filed out from Dolphin Smalltalk 7"!

MCPatchBrowser subclass: #RowanPatchBrowser
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanPatchBrowser guid: (GUID fromString: '{5754d2bc-5a68-40d5-a2b0-36ac0383ab9f}')!
RowanPatchBrowser comment: ''!
!RowanPatchBrowser categoriesForClass!Unclassified! !
!RowanPatchBrowser methodsFor!

createComponents
	super createComponents.
	includeIdenticalPresenter value: true!

name
	^name!

name: anObject
	name := anObject!

operationSelected

	| operation |
	leftTextPresenter 		value: ''.
	leftMemoPresenter	value: ''.
	rightTextPresenter	value: ''.
	rightMemoPresenter	value: ''.
	operationListPresenter hasSelection ifFalse: [^self].
	operation := operationListPresenter selection.
	leftTextPresenter 		value: 'Disk'.
	leftMemoPresenter	value: operation obsoletionMemo.
	rightTextPresenter	value: 'Image'.
	rightMemoPresenter	value: operation modificationMemo. 
	operation obsoletionMemoMarkers do: [:each | 
		leftMemoPresenter view addMarkerType: each key at: each value.
	].
	operation modificationMemoMarkers do: [:each |
		rightMemoPresenter view addMarkerType: each key at: each value.
	].
!

shellName

	^name, ' Changes'
! !
!RowanPatchBrowser categoriesFor: #createComponents!public! !
!RowanPatchBrowser categoriesFor: #name!accessing!public! !
!RowanPatchBrowser categoriesFor: #name:!accessing!public! !
!RowanPatchBrowser categoriesFor: #operationSelected!public! !
!RowanPatchBrowser categoriesFor: #shellName!overrides!private! !

!RowanPatchBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 551 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 592 nil nil nil nil nil 1 nil nil nil nil 1 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 7679 21 514 1601 1201 416 690 #updateMenuBar 592 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 31 18 0 0 98 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 848 nil nil nil 7 nil nil nil 848 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CheckBox) 34 16 nil 848 34 2 8 1409363203 1 976 721990 2 ##(Smalltalk.ValueHolder) nil false 1114118 ##(Smalltalk.NeverSearchPolicy) true nil nil 7 nil nil nil 976 nil 8 4294909673 852486 ##(Smalltalk.NullConverter) nil nil nil 626 138 144 34 2 690 #createAt:extent: 34 2 514 5 1 514 511 41 976 690 #text: 34 1 8 'Include methods with identical source' 976 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 0 0 0 0 1 1 0 0 20 0 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 5 1410 #fixedViewLeft 511 1410 #fixedParentTop 1 1410 #fixedViewTop 41 410 ##(Smalltalk.ListView) 34 30 nil 848 34 2 8 1409355853 1025 1488 590662 2 ##(Smalltalk.ListModel) 138 144 592 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 2 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #inspectLine 8 'Inspect' 1 1 nil nil nil 1714 1 1746 #browseChangedThing 8 'Browse' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 1488 nil 8 4294910083 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Type' 301 #left 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 2032 8 'doIt' 8 '[:each | each typeString]' 8 #[30 105 226 0 106] #typeString 2048 7 257 nil 1874 #<= 8 #() nil nil 1488 nil 1 2034 0 nil 2066 5 1 2032 8 'doIt' 8 '[:each | each className, ''>>'', each selector]' 8 #[33 105 226 0 30 178 226 3 178 106] #className 8 '>>' #, #selector 2176 7 257 nil nil 1986 8 'Class' 401 #left 2034 0 nil 2066 2 1 1872 8 'doIt' 8 '[:each | each className]' 8 #[30 105 226 0 106] #className 2304 7 257 nil ##(Smalltalk.SortedCollection) nil nil 1488 nil 1 nil nil 1986 8 'Selector' 401 #left 2034 0 nil 2066 2 1 112 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 2416 7 257 nil 1874 #<= 2160 nil nil 1488 nil 1 nil nil 1986 8 'Detail' 961 #left 2034 0 nil 2066 2 1 1872 8 'doIt' 8 '[:each | each detailsString]' 8 #[30 105 226 0 106] #detailsString 2544 7 257 nil 1874 #<= 2160 nil nil 1488 nil 3 nil nil #report 592 nil 133217 nil 34 4 nil nil 514 1 1 nil 626 138 144 34 3 690 #createAt:extent: 34 2 514 1 41 514 1569 517 1488 690 #contextMenu: 34 1 1680 1488 690 #text: 34 1 8 'Type' 1488 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 16 3 0 0 22 1 0 0] 8 #() 1360 nil 27 1378 1424 1 1410 #fixedParentRight 1 1456 41 1410 #fixedParentBottom 1 170 192 34 4 976 8 'includeIdenticalSource' 1488 8 'operationList' nil 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 1569 557 848 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 22 1 0 0] 34 2 976 1488 1360 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 3184 nil 466 8 4278190080 nil 519 nil nil nil 3184 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 557 514 1569 11 3184 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 22 1 0 0 16 3 0 0 27 1 0 0] 8 #() 1360 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 3440 nil nil nil 7 nil nil nil 3440 546 170 176 592 false 170 192 592 nil 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 567 514 1569 557 3440 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 1 0 0 16 3 0 0 49 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 3440 34 2 8 1140850688 131073 3712 nil nil nil 7 nil nil nil 3712 914 170 176 34 4 410 ##(Smalltalk.TextEdit) 34 16 nil 3712 34 2 8 1140916352 1025 3824 nil 466 8 4278190080 nil 7 nil nil nil 3824 nil 8 4294905961 1122 nil nil 3 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 779 39 3824 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 133 1 0 0 19 0 0 0] 8 #() 1360 nil 27 1378 1424 1 2928 1 1456 1 1472 39 410 ##(Smalltalk.ScintillaView) 34 50 nil 3712 34 2 8 1445007428 1025 4128 1042 nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 466 8 4278190080 nil 7 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 4128 nil 8 4294910145 1122 nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 4434 77 786694 ##(Smalltalk.IndexedColor) 33554457 4482 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 4448 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 4464 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 592 170 192 34 8 #removed 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 5 20091 nil nil 4128 #removed nil nil nil nil nil nil #changed 4626 7 5 nil nil 4128 #changed nil nil nil nil nil nil #added 4626 3 20087 nil nil 4128 #added nil nil nil nil nil nil #circle 4626 1 nil nil nil 4128 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 592 nil 170 176 592 9215 nil nil nil nil 4482 33554447 nil nil 65 nil nil 170 192 34 6 #literalBytes 8 '[]' #specialCharacter 8 '()[]<>' #literalArray 8 '()' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 4448 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 4128 65025 3 false 1 nil nil nil nil nil nil 3 4946 3 4128 33423361 5 false 3 nil nil nil nil nil nil 5 4946 5 4128 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 4434 1027 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 4434 1031 5058 60239 5058 31326207 1 nil nil nil nil #Warning nil nil nil #Error 4434 1029 5058 335 5058 31317503 1 nil nil nil nil #Error nil nil nil nil 626 138 144 34 6 690 #createAt:extent: 34 2 514 1 41 514 779 517 4128 690 #modificationEventMask: 8 #(4607) 4128 690 #caretPeriod: 8 #(530) 4128 690 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 4128 1 3 nil nil nil nil 5410 3 4128 33 nil nil 67108863 nil nil 5410 5 4128 1 nil 3 -67108863 nil nil 4128 690 #tabWidth: 8 #(4) 4128 690 #maxCompletionListHeight: 8 #(9) 4128 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 133 1 0 0 22 1 0 0] 8 #() 1360 nil 29 1378 1424 1 2928 1 1456 41 2944 1 170 192 34 4 3824 8 'leftText' 4128 8 'leftMemo' nil 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 779 557 3712 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 133 1 0 0 22 1 0 0] 34 2 3824 4128 1360 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 3440 34 2 8 1140850688 1 5824 nil 466 3264 nil 519 nil nil nil 5824 626 138 144 34 1 690 #createAt:extent: 34 2 514 779 1 514 11 557 5824 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 133 1 0 0 0 0 0 0 138 1 0 0 22 1 0 0] 8 #() 1360 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 3440 34 2 8 1140850688 131073 6064 nil nil nil 7 nil nil nil 6064 914 170 176 34 4 410 ##(Smalltalk.TextEdit) 34 16 nil 6064 34 2 8 1140916352 1025 6176 nil 466 3904 nil 7 nil nil nil 6176 nil 8 4294905961 1122 nil nil 3 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 781 39 6176 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 134 1 0 0 19 0 0 0] 8 #() 1360 nil 27 1378 1424 1 2928 1 1456 1 1472 39 410 ##(Smalltalk.ScintillaView) 34 50 nil 6064 34 2 8 1445007428 1025 6464 1042 nil false 4224 nil 466 4256 nil 7 nil 4274 nil true 4306 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 514 193 193 nil 6464 nil 8 4294910145 1122 nil nil 11 nil 170 192 34 4 #normal 4434 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 4434 77 4496 4512 1 nil nil nil nil #callTip nil nil nil 34 256 6688 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 6704 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 4546 1 nil nil false 138 144 592 170 192 34 8 #removed 4626 5 20091 nil nil 6464 #removed nil nil nil nil nil nil #changed 4626 7 5 nil nil 6464 #changed nil nil nil nil nil nil #added 4626 3 20087 nil nil 6464 #added nil nil nil nil nil nil #circle 4626 1 nil nil nil 6464 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 592 nil 170 176 592 9215 nil nil nil nil 4736 nil nil 65 nil nil 4752 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 6688 nil nil nil nil 1 nil 170 192 34 6 1 4946 1 6464 65025 3 false 1 nil nil nil nil nil nil 3 4946 3 6464 33423361 5 false 3 nil nil nil nil nil nil 5 4946 5 6464 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 4434 1027 nil 5058 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 4434 1031 5058 60239 5058 31326207 1 nil nil nil nil #Warning nil nil nil #Error 4434 1029 5058 335 5058 31317503 1 nil nil nil nil #Error nil nil nil nil 626 138 144 34 6 690 #createAt:extent: 34 2 514 1 41 514 781 517 6464 690 #modificationEventMask: 8 #(4607) 6464 690 #caretPeriod: 8 #(530) 6464 690 #margins: 34 1 34 3 5410 1 6464 1 3 nil nil nil nil 5410 3 6464 33 nil nil 67108863 nil nil 5410 5 6464 1 nil 3 -67108863 nil nil 6464 690 #tabWidth: 8 #(4) 6464 690 #maxCompletionListHeight: 8 #(9) 6464 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 134 1 0 0 22 1 0 0] 8 #() 1360 nil 29 1378 1424 1 2928 1 1456 41 2944 1 170 192 34 4 6176 8 'rightText' 6464 8 'rightMemo' nil 626 138 144 34 1 690 #createAt:extent: 34 2 514 789 1 514 781 557 6064 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 138 1 0 0 0 0 0 0 16 3 0 0 22 1 0 0] 34 2 6176 6464 1360 nil 27 1360 nil 27 1360 nil 27 )!

showOn: aModel name: aString
	| browser | 
	browser := self on: aModel.
	browser name: aString. 
	^browser
		createView: self defaultView;
		showShell;
		yourself! !
!RowanPatchBrowser class categoriesFor: #resource_Default_view!public!resources-views! !
!RowanPatchBrowser class categoriesFor: #showOn:name:!public! !

