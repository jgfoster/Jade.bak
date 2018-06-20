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

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 560 0 0 0 7 0 0 0 560 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.Presenter)  8 #resource_OK_Cancel_button_block 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  21 445 834 1149 71 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 222 0 0 0 72 2 0 0 1 1 0 0] 98 0 834 193 193 0 27 0 0 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 992 0 482 512 0 7 0 0 0 992 0 8 4294902451 852486 ##(Smalltalk.NullConverter)  0 0 5 706 202 208 98 4 770 800 98 2 834 215 1 834 971 41 992 770 8 #text: 98 1 8 '*' 992 770 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 770 8 #isTextModified: 98 1 32 992 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 107 0 0 0 0 0 0 0 80 2 0 0 20 0 0 0] 98 0 944 0 27 8 'nameEntry' 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 1504 590662 2 ##(Smalltalk.ListModel)  202 208 928 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 512 0 7 0 0 0 1504 0 8 4294902253 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 4 920646 5 ##(Smalltalk.ListViewColumn)  8 'Name' 401 8 #left 1696 8 ##(Smalltalk.SortedCollection)  787814 3 ##(Smalltalk.BlockClosure)  0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 1872 8 'doIt' 8 '[:each | each value at: 1]' 8 #[29 105 17 142 63 148 106] 1888 7 257 0 0 1504 0 1 0 0 1794 8 'Dictionary' 191 1840 1696 1856 1874 0 0 1906 1 83886081 1872 8 'doIt' 8 '[:each | each value at: 2]' 8 #[29 105 17 142 64 148 106] 2016 7 257 0 0 1504 0 1 0 0 1794 8 'Category' 271 1840 459270 ##(Smalltalk.Message)  8 #displayString 98 0 1856 1874 0 0 1906 1 83886081 1872 8 'doIt' 8 '[:each | each value at: 3]' 8 #[29 105 17 142 214 3 148 106] 2192 7 257 0 0 1504 0 1 0 0 1794 8 'Package' 271 1840 2130 2160 98 0 2130 8 #<= 2320 1874 0 0 1906 1 83886081 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each value at: 4]' 8 #[29 105 17 142 214 4 148 106] 2368 7 257 0 0 1504 0 1 0 0 8 #report 928 0 131169 0 0 706 202 208 98 2 770 800 98 2 834 5 41 834 1181 391 1504 770 1264 98 1 8 'Name' 1504 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 20 0 0 0 80 2 0 0 215 0 0 0] 98 0 944 0 27 8 'classList' 590342 ##(Smalltalk.Rectangle)  834 21 21 834 21 21 0 0 0 0 10939 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 1987604964 706 202 208 98 3 770 800 98 2 834 5119 21 834 1201 591 416 770 1264 98 1 8 'Jade Find Class Dialog' 416 770 8 #updateMenuBar 928 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 49 1 0 0] 98 4 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 3056 0 0 0 7 0 0 0 3056 0 8 4294902375 1106 0 0 0 706 202 208 98 2 770 800 98 2 834 1 1 834 211 41 3056 770 1264 98 1 8 'Partial Name:' 3056 882 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 98 0 944 0 27 992 1504 560 944 0 27 )! !
!JadeFindClassDialog class categoriesFor: #resource_Default_view!public!resources-views! !
!JadeFindClassDialog class categoriesFor: #resource_ThreeColumnView!public!resources-views! !

