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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 0 517 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 576 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  2731 21 738 611 791 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 23 5 0 0 10 0 0 0 72 6 0 0 149 1 0 0] 98 1 410 432 98 15 0 416 98 2 8 1140850688 131073 848 0 0 0 5 0 0 0 848 530 234 240 576 32 234 256 98 2 410 8 ##(Smalltalk.ListView)  98 30 0 848 98 2 8 1409355853 1025 976 590662 2 ##(Smalltalk.ListModel)  202 208 576 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #deleteGsClassShape 8 'Delete Gs Class Shape' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 976 0 8 4294902979 459270 ##(Smalltalk.Message)  8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 'Class Name' 601 8 #left 787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 1568 8 'doIt' 8 '[:each | each]' 1779525917 1584 7 257 0 8 ##(Smalltalk.SortedCollection)  0 0 976 0 1 0 0 8 #report 576 0 131169 0 0 610 202 208 98 3 674 704 98 2 738 1 1 738 611 791 976 674 8 #contextMenu: 98 1 1200 976 674 8 #text: 98 1 8 'Class Name' 976 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 49 1 0 0 139 1 0 0] 98 0 738 193 193 0 27 8 'registry' 0 610 202 208 98 1 674 704 98 2 738 1 1 738 611 791 848 786 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 49 1 0 0 139 1 0 0] 98 1 976 1968 0 27 1968 0 27 )! !
!JadeRegistryPresenter class categoriesFor: #defaultModel!public! !
!JadeRegistryPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

