"Filed out from Dolphin Smalltalk 7"!

RowanShell subclass: #RowanProjectListShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectListShell guid: (GUID fromString: '{c41e9df2-548a-41c0-97e8-a12a2746e7b3}')!
RowanProjectListShell comment: ''!
!RowanProjectListShell categoriesForClass!Unclassified! !
!RowanProjectListShell methodsFor!

addProject

	self primaryPresenter addProject.
	
!

makeDefault

	self primaryPresenter makeDefault.
!

presenterClass
	
	^RowanProjectListPresenter!

saveProject

	self primaryPresenter saveProject.
	
!

shellName
	^'Rowan Project List Shell'! !
!RowanProjectListShell categoriesFor: #addProject!menu handlers!public! !
!RowanProjectListShell categoriesFor: #makeDefault!menu handlers!public! !
!RowanProjectListShell categoriesFor: #presenterClass!public! !
!RowanProjectListShell categoriesFor: #saveProject!menu handlers!public! !
!RowanProjectListShell categoriesFor: #shellName!private! !

!RowanProjectListShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 551 265030 4 ##(Smalltalk.Menu) nil true 34 15 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #makeDefault 8 'Make Default' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 594 1 626 #newGitProject 8 'Clone Git Project ...' 1 1 nil nil nil 594 1 626 #loadProject 8 'Load' 1 1 nil nil nil 594 1 626 #deleteProject 8 'Unload ...' 1 1 nil nil nil 674 4097 594 1 626 #saveProject 8 'Commit ...' 1 1 nil nil nil 594 1 626 #pushToGit 8 'Push to Git ...' 1 1 nil nil nil 674 4097 594 1 626 #projectChanges 8 'Changes' 1 1 nil nil nil 594 1 626 #revertProject 8 'Revert ...' 1 1 nil nil nil 674 4097 594 1 626 #writeProject 8 'Write ...' 1 1 nil nil nil 594 1 626 #projectLog 8 'Log' 1 1 nil nil nil 594 1 626 #inspectProject 8 'Inspect' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 416 nil 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 416 34 2 8 1140920653 1025 1280 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 nil 7 nil nil nil 1280 nil 8 4294903063 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 3 1447750 15 ##(Smalltalk.EditableListViewColumn) 8 'Project Name' 601 #left 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 1648 8 'doIt' 8 '[:project | project nameWithDefaultIndication]' 8 #[30 105 226 0 106] #nameWithDefaultIndication 1664 7 257 nil nil 1280 nil 1 nil nil false nil 1052998 20 ##(Smalltalk.EmbeddedTextEdit) nil nil 8 #(67174528 0) 1776 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil nil nil 5 nil nil nil 1776 nil nil 852486 ##(Smalltalk.NullConverter) nil 8 '' 3 nil nil nil nil nil nil nil nil nil 1554 8 'Loaded Sha' 301 #left 1602 #displayString 8 #() 1602 #<= 1968 1650 0 nil 1682 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:project | project sha]' 8 #[30 105 226 0 106] #sha 2000 7 257 nil nil 1280 nil 1 nil nil false nil 1762 nil nil 8 #(67174528 0) 2080 1810 nil false 1856 nil 466 8 4278190080 nil 5 nil nil nil 2080 nil nil 1874 nil 8 '' 3 nil nil nil nil nil nil nil nil nil 1554 8 'Branch' 301 #left 1602 #displayString 1968 1602 #<= 1968 1650 0 nil 1682 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:project | project branch]' 8 #[30 105 226 0 106] #branch 2256 7 257 nil nil 1280 nil 1 nil nil false nil 1762 nil nil 8 #(67174528 0) 2336 1810 nil false 1856 nil 466 2144 nil 5 nil nil nil 2336 nil nil 1874 nil 8 '' 3 nil nil nil nil nil nil nil nil nil #report 1392 nil 131171 514 129 129 34 4 nil nil 514 1 1 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 5 1 514 1165 723 1280 2546 #text: 34 1 8 'Project Name' 1280 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 0 0 0 0 72 2 0 0 105 1 0 0] 8 #() 514 193 193 nil 27 8 'projectList' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 2482 138 144 34 4 2546 #createAt:extent: 34 2 514 3839 21 514 1201 801 416 2546 #contextMenu: 34 1 560 416 2546 #text: 34 1 8 'Rowan Project List' 416 2546 #updateMenuBar 1392 416 2674 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 215 9 0 0 154 1 0 0] 34 1 1280 2736 nil 27 )! !
!RowanProjectListShell class categoriesFor: #resource_Default_view!public!resources-views! !

