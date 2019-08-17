"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #JadeRegistryPresenter
	instanceVariableNames: 'registryPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeRegistryPresenter guid: (GUID fromString: '{adfffc60-58d5-4425-979f-30fdcea64884}')!
JadeRegistryPresenter comment: ''!
!JadeRegistryPresenter categoriesForClass!Unclassified! !
!JadeRegistryPresenter methodsFor!

createComponents

	super createComponents.

	registryPresenter := self add: ListPresenter new name: 'registry'.!

deleteGsClassShape

	registryPresenter hasSelection ifFalse: [^self].
	(MessageBox confirm: 'Are sure to delete the selected Gs Class Shape ?') ifFalse: [^self].

	model unregisterClassNamed: registryPresenter selection.

	self model: self model.!

model: aJadeAutocompletationRegistry

	super model: aJadeAutocompletationRegistry.

	registryPresenter model: (ListModel on: aJadeAutocompletationRegistry getAllClassesNames asSortedCollection)

	! !
!JadeRegistryPresenter categoriesFor: #createComponents!public! !
!JadeRegistryPresenter categoriesFor: #deleteGsClassShape!public! !
!JadeRegistryPresenter categoriesFor: #model:!public! !

!JadeRegistryPresenter class methodsFor!

defaultModel

	^JadeAutocompletationRegistry default!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil 721158 ##(Smalltalk.SystemColor) 31 nil 517 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 560 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 2719 21 706 881 791 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 79 5 0 0 10 0 0 0 7 7 0 0 149 1 0 0] 34 1 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 816 nil nil nil 5 nil nil nil 816 514 170 176 560 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 816 34 2 8 1409355853 1025 944 590662 2 ##(Smalltalk.ListModel) 138 144 560 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #deleteGsClassShape 8 'Delete Gs Class Shape' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 944 nil 8 1921377647 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Class Name' 881 #left 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 1 83886081 1456 8 'doIt' 8 '[:each | each]' 1779525917 1472 7 257 nil ##(Smalltalk.SortedCollection) nil nil 944 nil 1 nil nil #report 560 nil 131169 nil 34 4 nil nil 706 1 1 nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 1 1 706 881 791 944 658 #contextMenu: 34 1 1152 944 658 #text: 34 1 8 'Class Name' 944 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 1 0 0 139 1 0 0] 8 #() 706 193 193 nil 27 8 'registry' nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 881 791 816 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 184 1 0 0 139 1 0 0] 34 1 944 1824 nil 27 1824 nil 27 )! !
!JadeRegistryPresenter class categoriesFor: #defaultModel!public! !
!JadeRegistryPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

