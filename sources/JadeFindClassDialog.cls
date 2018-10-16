"Filed out from Dolphin Smalltalk 7"!

JadeValueDialog subclass: #JadeFindClassDialog
	instanceVariableNames: 'classListPresenter nameEntryPresenter availableClasses'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeFindClassDialog guid: (GUID fromString: '{0681b51c-d226-407a-8be0-49345512c4f9}')!
JadeFindClassDialog comment: ''!
!JadeFindClassDialog categoriesForClass!Unclassified! !
!JadeFindClassDialog methodsFor!

createComponents

	super createComponents.
	classListPresenter 		:= self add: ListPresenter		new name: 'classList'.
	nameEntryPresenter		:= self add: TextPresenter		new name: 'nameEntry'.
!

createSchematicWiring

	super createSchematicWiring.
	classListPresenter 		when: #actionPerformed send: #ok							to: self.
	nameEntryPresenter		when: #valueChanged 	send: #updateClassList 	to: self.
!

ok

	self model value: classListPresenter selectionOrNil.
	super ok.
!

onViewOpened

	super onViewOpened.
	availableClasses := self model value asSortedCollection.
	self model value: nil.
	nameEntryPresenter value: '*'.
!

updateClassList 

	| list |
	list := availableClasses select: [:each | nameEntryPresenter value , '*' match: each key].
	classListPresenter list: list.
	list size >= 1 ifTrue: [
		classListPresenter selectionByIndex: 1.
	].
! !
!JadeFindClassDialog categoriesFor: #createComponents!private! !
!JadeFindClassDialog categoriesFor: #createSchematicWiring!private! !
!JadeFindClassDialog categoriesFor: #ok!private! !
!JadeFindClassDialog categoriesFor: #onViewOpened!private! !
!JadeFindClassDialog categoriesFor: #updateClassList!private! !

!JadeFindClassDialog class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 560 0 0 0 7 0 0 0 560 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.Presenter)  8 #resource_OK_Cancel_button_block 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  21 437 834 469 71 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 218 0 0 0 244 0 0 0 253 0 0 0] 98 0 834 193 193 0 27 0 0 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 992 0 482 512 0 7 0 0 0 992 0 8 4294902907 852486 ##(Smalltalk.NullConverter)  0 0 5 706 202 208 98 4 770 800 98 2 834 211 1 834 301 41 992 770 8 #text: 98 1 8 '*' 992 770 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 770 8 #isTextModified: 98 1 32 992 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 0 0 0 0 255 0 0 0 20 0 0 0] 98 0 944 0 27 8 'nameEntry' 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 1504 590662 2 ##(Smalltalk.ListModel)  202 208 928 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 512 0 7 0 0 0 1504 0 8 4294902549 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 2 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 353 8 #left 1696 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  4 1 1872 8 'doIt' 8 '[:each | each key subStrings first]' 8 #[32 105 226 0 159 160 106] 8 #key 8 #subStrings 8 #first 1888 7 257 0 0 1504 0 3 0 0 1794 8 'Dictionary' 151 1840 1696 1856 1874 0 459302 ##(Smalltalk.Context)  1 1 0 0 1906 0 9 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 98 2 8 '[:each | each value]' 98 1 202 8 ##(Smalltalk.PoolDictionary)  928 8 #[252 1 0 1 1 5 0 17 229 32 142 106 105] 17 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 1 41 834 511 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 255 0 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 9033 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 2118378871 706 202 208 98 3 770 800 98 2 834 3359 21 834 521 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 143 6 0 0 10 0 0 0 147 7 0 0 49 1 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 2848 0 0 0 7 0 0 0 2848 0 8 4294902555 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 2848 770 1264 98 1 8 'Partial Name:' 2848 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )!

resource_ThreeColumnView
	"Answer the literal data from which the 'ThreeColumnView' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_ThreeColumnView)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.DialogView) 34 30 nil nil 8 #(13107200 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 165 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 8 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916352 1025 576 nil 480 nil 7 nil nil nil 576 nil 8 1736318464 852486 ##(Smalltalk.NullConverter) nil nil 5 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 211 21 802 939 41 576 754 #text: 34 1 8 '*' 576 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 105 0 0 0 10 0 0 0 62 2 0 0 30 0 0 0] 8 #() 802 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 191 1010 #fixedParentRight 1 1010 #fixedParentTop 1 1010 #fixedViewTop 41 410 ##(Smalltalk.ListView) 34 30 nil 416 34 2 8 1140920397 1025 1088 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 480 nil 7 nil nil nil 1088 nil 8 1736370288 ##(Smalltalk.BasicListAbstract) nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 4 920646 5 ##(Smalltalk.ListViewColumn) 8 'Name' 401 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 1 83886081 1376 8 'doIt' 8 '[:each | each value at: 1]' 8 #[29 105 17 142 63 148 106] 1392 7 257 nil nil 1088 nil 1 nil nil 1330 8 'Dictionary' 191 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1378 0 nil 1410 1 83886081 1376 8 'doIt' 8 '[:each | each value at: 2]' 8 #[29 105 17 142 64 148 106] 1520 7 257 nil nil 1088 nil 1 nil nil 1330 8 'Category' 271 #left 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.SortedCollection) 1378 0 nil 1410 1 83886081 1376 8 'doIt' 8 '[:each | each value at: 3]' 8 #[29 105 17 142 214 3 148 106] 1680 7 257 nil nil 1088 nil 1 nil nil 1330 8 'Package' 271 #left 1634 #displayString 8 #() 1634 #<= 1808 1378 0 nil 1410 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each value at: 4]' 8 #[29 105 17 142 214 4 148 106] 1840 7 257 nil nil 1088 nil 1 nil nil #report 1200 nil 131169 nil 34 4 nil nil 802 1 1 nil 690 138 144 34 2 754 #createAt:extent: 34 2 802 21 71 802 1129 373 1088 754 #text: 34 1 8 'Name' 1088 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 35 0 0 0 62 2 0 0 221 0 0 0] 8 #() 960 nil 27 978 1024 1 1040 1 1056 51 1010 #fixedParentBottom -49 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 2192 nil nil nil 7 nil nil nil 2192 nil 8 1736477344 658 nil nil nil 690 138 144 34 2 754 #createAt:extent: 34 2 802 21 21 802 181 41 2192 754 #text: 34 1 8 'Partial Name:' 2192 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 10 0 0 0 100 0 0 0 30 0 0 0] 8 #() 960 nil 27 978 1024 1 1010 #fixedViewLeft 181 1056 1 1072 41 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 2528 nil nil nil 7 nil nil nil 2528 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.Presenter) #resource_OK_Cancel_button_block nil 690 138 144 34 1 754 #createAt:extent: 34 2 802 21 443 802 1129 61 2528 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 221 0 0 0 62 2 0 0 251 0 0 0] 1200 960 nil 27 978 1024 1 1040 1 2176 -49 1072 61 170 192 34 4 576 8 'nameEntry' 1088 8 'classList' 590342 ##(Smalltalk.Rectangle) 802 21 21 802 21 21 nil nil nil nil 9139 nil nil nil nil 1 nil nil 590598 ##(Smalltalk.Semaphore) nil nil 1 nil 8 2007520816 690 138 144 34 3 754 #createAt:extent: 34 2 802 5119 21 802 1201 591 416 754 #text: 34 1 8 'Jade Find Class Dialog' 416 754 #updateMenuBar 1200 416 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 49 1 0 0] 34 4 2192 576 1088 2528 960 nil 27 )! !
!JadeFindClassDialog class categoriesFor: #resource_Default_view!public!resources-views! !
!JadeFindClassDialog class categoriesFor: #resource_ThreeColumnView!public!resources-views! !

