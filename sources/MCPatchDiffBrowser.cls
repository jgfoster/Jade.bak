"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #MCPatchDiffBrowser
	instanceVariableNames: 'patch includeIdenticalPresenter operationListPresenter diffPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCPatchDiffBrowser guid: (GUID fromString: '{132e9065-8fb2-4eb8-9a5b-8f1ac7a50f69}')!
MCPatchDiffBrowser comment: ''!
!MCPatchDiffBrowser categoriesForClass!Unclassified! !
!MCPatchDiffBrowser methodsFor!

cinspectLine

	| line |
	(line := operationListPresenter selectionOrNil) isNil ifTrue: [^self].
	line halt.!

createComponents

	super createComponents.
	includeIdenticalPresenter	:= self add: BooleanPresenter	new name: 'includeIdenticalSource'.
	operationListPresenter 	:= self add: ListPresenter 		new name: 'operationList'.
	diffPresenter := self add: DifferencesPresenter new name: 'difference'.!

createSchematicWiring

	super createSchematicWiring.
	includeIdenticalPresenter	when: #'valueChanged'		send: #'refresh'					to: self.
operationListPresenter when: #'selectionChanged' send: #'operationSelected' to: self.!

model: anMCPatch

	patch := anMCPatch.
	self gciSession: anMCPatch gciSession.!

onViewOpened

	super onViewOpened.

	diffPresenter textStyles: SmalltalkWorkspace textStyles.
	diffPresenter styler: SmalltalkMethodStyler new.

	self refresh.!

operationSelected
	| operation | 

	operationListPresenter hasSelection ifFalse: [^self].
	operation := operationListPresenter selection.

	diffPresenter compare: operation obsoletionSource with: operation modificationSource.!

operationsList

	| list |
	list := patch operations.
	includeIdenticalPresenter value ifFalse: [
		list := list reject: [:each | each hasEquivalentText].
	].
	^list asSortedCollection asArray.!

refresh

	operationListPresenter
		resetSelection;
		list: self operationsList;
		yourself!

shellName

	^patch name! !
!MCPatchDiffBrowser categoriesFor: #cinspectLine!public! !
!MCPatchDiffBrowser categoriesFor: #createComponents!public! !
!MCPatchDiffBrowser categoriesFor: #createSchematicWiring!public! !
!MCPatchDiffBrowser categoriesFor: #model:!public! !
!MCPatchDiffBrowser categoriesFor: #onViewOpened!public! !
!MCPatchDiffBrowser categoriesFor: #operationSelected!public! !
!MCPatchDiffBrowser categoriesFor: #operationsList!public! !
!MCPatchDiffBrowser categoriesFor: #refresh!public! !
!MCPatchDiffBrowser categoriesFor: #shellName!public! !

!MCPatchDiffBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 592 nil nil nil nil nil 1 nil nil nil nil 1 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 3059 21 514 1601 1201 416 690 #updateMenuBar 592 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 5 0 0 10 0 0 0 25 9 0 0 98 2 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 848 nil nil nil 7 nil nil nil 848 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 848 34 2 8 1409355853 1025 976 590662 2 ##(Smalltalk.ListModel) 138 144 592 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 480 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #inspectLine 8 'Inspect' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 976 nil 8 1933174127 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Type' 201 #left 1282 #displayString 8 #() 1282 #<= 1456 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each typeString]' 8 #[30 105 226 0 106] #typeString 1504 7 257 nil nil 976 nil 1 nil nil 1394 8 'Class' 201 #left 1282 #displayString 1312 ##(Smalltalk.SortedCollection) 1490 0 nil 1522 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each className]' 8 #[30 105 226 0 106] #className 1648 7 257 nil nil 976 nil 1 nil nil 1394 8 'Selector' 201 #left 1282 #displayString 1456 1282 #<= 1456 1490 0 nil 1522 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each selector]' 8 #[30 105 226 0 106] #selector 1792 7 257 nil nil 976 nil 1 nil nil 1394 8 'Detail' 961 #left 1282 #displayString 1456 1282 #<= 1456 1490 0 nil 1522 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each detailsString]' 8 #[30 105 226 0 106] #detailsString 1936 7 257 nil nil 976 nil 3 nil nil #report 592 nil 131169 nil 8 #(nil) 626 138 144 34 3 690 #createAt:extent: 34 2 514 1 41 514 1569 517 976 690 #contextMenu: 34 1 1136 976 690 #text: 34 1 8 'Type' 976 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 16 3 0 0 22 1 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 2322 #fixedParentRight 1 2322 #fixedParentTop 41 2322 #fixedParentBottom 1 410 ##(Smalltalk.CheckBox) 34 16 nil 848 34 2 8 1409363203 1 2400 721990 2 ##(Smalltalk.ValueHolder) nil nil 1114118 ##(Smalltalk.NeverSearchPolicy) false nil nil 7 nil nil nil 2400 nil 8 1933017484 852486 ##(Smalltalk.NullConverter) nil nil nil 626 138 144 34 2 690 #createAt:extent: 34 2 514 5 1 514 511 41 2400 690 #text: 34 1 8 'Include methods with identical source' 2400 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 0 0 0 0 1 1 0 0 20 0 0 0] 8 #() 2272 nil 27 2290 2336 5 2322 #fixedViewLeft 511 2368 1 2322 #fixedViewTop 41 170 192 34 4 976 8 'operationList' 2400 8 'includeIdenticalSource' nil 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 1569 557 848 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 22 1 0 0] 34 2 2400 976 2272 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 3056 nil 480 nil 519 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 3056 nil 1 #left nil nil nil 514 1 1 514 9 9 nil 3152 nil 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 557 514 1569 11 3056 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 22 1 0 0 16 3 0 0 27 1 0 0] 8 #() 2272 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 3344 nil nil nil 7 nil nil nil 3344 546 170 176 592 false 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 3344 34 2 8 1140850688 131073 3472 nil 480 nil 7 nil nil nil 3472 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.DifferencesPresenter) #resource_Default_view nil 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 1 514 1569 557 3472 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 22 1 0 0] 592 2272 nil 27 8 'difference' nil 626 138 144 34 1 690 #createAt:extent: 34 2 514 1 567 514 1569 557 3344 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 1 0 0 16 3 0 0 49 2 0 0] 34 1 3472 2272 nil 27 2272 nil 27 )! !
!MCPatchDiffBrowser class categoriesFor: #resource_Default_view!public! !

