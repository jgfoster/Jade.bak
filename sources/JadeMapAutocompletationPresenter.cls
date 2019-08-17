"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #JadeMapAutocompletationPresenter
	instanceVariableNames: 'namePresenter gsClassListPresenter mapPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMapAutocompletationPresenter guid: (GUID fromString: '{17f5f69a-c4a4-4311-bc87-175f9be8852c}')!
JadeMapAutocompletationPresenter comment: ''!
!JadeMapAutocompletationPresenter categoriesForClass!Unclassified! !
!JadeMapAutocompletationPresenter methodsFor!

addMap

	namePresenter value isEmpty ifTrue: [^self].
	gsClassListPresenter hasSelection ifFalse: [^self].

	self registry registerMap: namePresenter value gsClassName: gsClassListPresenter selection name.

	self onViewOpened.!

createComponents

	super createComponents.

	namePresenter := self add: TextPresenter new name: 'name'.
	gsClassListPresenter := self add: ListPresenter new name: 'gsClassList'.
	mapPresenter := self add: ListPresenter new name: 'map'.!

gsClassListPresenter
	^gsClassListPresenter!

gsClassListPresenter: anObject
	gsClassListPresenter := anObject!

mapPresenter
	^mapPresenter!

mapPresenter: anObject
	mapPresenter := anObject!

namePresenter
	^namePresenter!

namePresenter: anObject
	namePresenter := anObject!

onViewOpened

	super onViewOpened.

	mapPresenter model: (ListModel on: self registry mapToDisplay).

	gsClassListPresenter model: (ListModel on: self registry registryAsSortedCollection)!

removeMap

	mapPresenter hasSelection ifFalse: [^self].

	self registry unregisterMap: (mapPresenter selection subStrings: '>>') first.

	self onViewOpened.! !
!JadeMapAutocompletationPresenter categoriesFor: #addMap!public! !
!JadeMapAutocompletationPresenter categoriesFor: #createComponents!public! !
!JadeMapAutocompletationPresenter categoriesFor: #gsClassListPresenter!accessing!private! !
!JadeMapAutocompletationPresenter categoriesFor: #gsClassListPresenter:!accessing!private! !
!JadeMapAutocompletationPresenter categoriesFor: #mapPresenter!accessing!private! !
!JadeMapAutocompletationPresenter categoriesFor: #mapPresenter:!accessing!private! !
!JadeMapAutocompletationPresenter categoriesFor: #namePresenter!accessing!private! !
!JadeMapAutocompletationPresenter categoriesFor: #namePresenter:!accessing!private! !
!JadeMapAutocompletationPresenter categoriesFor: #onViewOpened!public! !
!JadeMapAutocompletationPresenter categoriesFor: #removeMap!public! !

!JadeMapAutocompletationPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil 721158 ##(Smalltalk.SystemColor) 31 nil 517 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 544 nil nil nil 5 nil nil nil 544 nil 170 192 34 4 410 ##(Smalltalk.ComboBox) 34 17 nil 544 34 2 8 1412498947 1025 640 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 nil nil nil 640 nil 8 1921008616 459270 ##(Smalltalk.Message) #name 8 #() 752 401 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 371 151 1026 481 51 640 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 75 0 0 0 169 1 0 0 100 0 0 0] 8 #() 1026 193 193 nil 27 8 'gsClassList' 410 ##(Smalltalk.TextEdit) 34 16 nil 544 34 2 8 1140916352 1025 1168 nil 816 nil 5 nil nil nil 1168 nil 8 1921358507 852486 ##(Smalltalk.NullConverter) nil nil 1 914 138 144 34 1 978 #createAt:extent: 34 2 1026 371 71 1026 481 51 1168 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 185 0 0 0 35 0 0 0 169 1 0 0 60 0 0 0] 8 #() 1136 nil 27 8 'name' nil 914 138 144 34 1 978 #createAt:extent: 34 2 1026 1 1 1026 951 311 544 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 219 1 0 0 155 0 0 0] 34 6 410 ##(Smalltalk.StaticText) 34 16 nil 544 34 2 8 1140850944 1 1616 nil nil nil 5 nil nil nil 1616 nil 8 1921133906 1250 nil nil nil 914 138 144 34 2 978 #createAt:extent: 34 2 1026 71 81 1026 211 41 1616 978 #text: 34 1 8 'Name to map' 1616 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 0 0 0 40 0 0 0 140 0 0 0 60 0 0 0] 8 #() 1136 nil 27 410 ##(Smalltalk.StaticText) 34 16 nil 544 34 2 8 1140850944 1 1920 nil nil nil 5 nil nil nil 1920 nil 8 1921133906 1250 nil nil nil 914 138 144 34 2 978 #createAt:extent: 34 2 1026 71 161 1026 241 39 1920 978 #text: 34 1 8 'GemStone Class' 1920 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 0 0 0 80 0 0 0 155 0 0 0 99 0 0 0] 8 #() 1136 nil 27 1168 640 410 ##(Smalltalk.PushButton) 34 20 nil 544 34 2 8 1140924416 1 2224 nil nil nil 5 nil nil nil 2224 nil 8 1921221004 1180998 4 ##(Smalltalk.CommandDescription) #addMap 8 'Add' 1 1 nil nil false nil nil nil 914 138 144 34 3 978 #createAt:extent: 34 2 1026 711 231 1026 141 51 2224 978 #isEnabled: 8 #(false) 2224 978 #text: 34 1 8 'Add' 2224 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 99 1 0 0 115 0 0 0 169 1 0 0 140 0 0 0] 8 #() 1136 nil 29 410 ##(Smalltalk.GroupBox) 34 14 nil 544 34 2 8 1140850695 65 2592 nil 816 nil 5 nil nil nil 2592 nil 8 1921221004 914 138 144 34 2 978 #createAt:extent: 34 2 1026 41 21 1026 841 271 2592 978 #text: 34 1 8 'Create New Map' 2592 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 0 0 0 10 0 0 0 184 1 0 0 145 0 0 0] 8 #() 1136 nil 27 1136 nil 27 nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 2880 nil nil nil 5 nil nil nil 2880 1180166 ##(Smalltalk.ProportionalLayout) 170 176 752 false 170 192 34 2 410 ##(Smalltalk.ListView) 34 30 nil 2880 34 2 8 1409355853 1025 3024 706 138 144 752 nil 784 816 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 2306 #removeMap 8 'Remove Map' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 3024 nil 8 1921377647 866 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Name to Map' 401 #left 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 4 1 864 8 'doIt' 8 '[:each | (each subStrings: ''>>'') first]' 8 #[32 105 17 29 177 160 106] 8 '>>' #subStrings: #first 3424 7 257 nil ##(Smalltalk.SortedCollection) nil nil 3024 nil 1 nil nil 3362 8 'Gs Class' 401 #left 3410 0 nil 3442 4 1 864 8 'doIt' 8 '[:each | (each subStrings: ''>>'') last]' 8 #[32 105 17 29 177 160 106] 8 '>>' #subStrings: #last 3568 7 257 nil 866 #<= 8 #() nil nil 3024 nil 1 nil nil #report 752 nil 131169 nil 8 #(nil) 914 138 144 34 3 978 #createAt:extent: 34 2 1026 1 1 1026 951 481 3024 978 #contextMenu: 34 1 3136 3024 978 #text: 34 1 8 'Name to Map' 3024 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 219 1 0 0 240 0 0 0] 8 #() 1136 nil 27 8 'map' nil 914 138 144 34 1 978 #createAt:extent: 34 2 1026 1 311 1026 951 481 2880 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 155 0 0 0 219 1 0 0 139 1 0 0] 34 1 3024 1136 nil 27 170 192 752 nil 914 138 144 34 1 978 #createAt:extent: 34 2 1026 2719 21 1026 951 791 416 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 79 5 0 0 10 0 0 0 42 7 0 0 149 1 0 0] 34 2 544 2880 1136 nil 27 )! !
!JadeMapAutocompletationPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

