"Filed out from Dolphin Smalltalk 7"!

JadeFindClassDialog subclass: #JadeiteFindClassDialog
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteFindClassDialog guid: (GUID fromString: '{af450a9b-89b0-470b-a65c-530c80473618}')!
JadeiteFindClassDialog comment: ''!
!JadeiteFindClassDialog categoriesForClass!Unclassified! !
!JadeiteFindClassDialog methodsFor!

updateClassList 

	| list |
	list := availableClasses select: [:each | nameEntryPresenter value , '*' match: each].
	classListPresenter list: list.
	list size >= 1 ifTrue: [
		classListPresenter selectionByIndex: 1.
	].
! !
!JadeiteFindClassDialog categoriesFor: #updateClassList!private! !

!JadeiteFindClassDialog class methodsFor!

resource_DefaultView
	"Answer the literal data from which the 'DefaultView' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_DefaultView)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.DialogView) 34 30 nil nil 8 #(13107200 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 167 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 544 nil nil nil 7 nil nil nil 544 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.Presenter) #resource_OK_Cancel_button_block nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 21 413 754 1129 71 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 206 0 0 0 62 2 0 0 241 0 0 0] 8 #() 754 193 193 nil 27 nil nil nil 170 192 34 4 410 ##(Smalltalk.TextEdit) 34 16 nil 416 34 2 8 1140916352 1025 912 nil 466 496 nil 7 nil nil nil 912 nil 8 4294905961 852486 ##(Smalltalk.NullConverter) nil nil 5 642 138 144 34 2 706 #createAt:extent: 34 2 754 215 1 754 971 41 912 706 #text: 34 1 8 '*' 912 802 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 107 0 0 0 0 0 0 0 80 2 0 0 20 0 0 0] 8 #() 864 nil 27 8 'nameEntry' 410 ##(Smalltalk.ListView) 34 30 nil 416 34 2 8 1140920397 1025 1264 590662 2 ##(Smalltalk.ListModel) 138 144 848 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 496 nil 7 nil nil nil 1264 nil 8 4294910083 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 3 920646 5 ##(Smalltalk.ListViewColumn) 8 'Name' 401 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 1 83886081 1552 8 'doIt' 8 '[:service | service]' 1779525917 1568 7 257 nil nil 1264 nil 1 nil nil 1506 8 'Project' 401 #left 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.SortedCollection) 1554 0 nil 1586 1 83886081 1552 8 'doIt' 8 '[:service | service]' 1779525917 1728 7 257 nil nil 1264 nil 1 nil nil 1506 8 'Package' 401 #left 1682 #displayString 8 #() 1682 #<= 1840 1554 0 nil 1586 1 83886081 1552 8 'doIt' 8 '[:service | service]' 1779525917 1872 7 257 nil nil 1264 nil 1 nil nil #report 848 nil 131169 nil 34 4 nil nil 754 1 1 nil 642 138 144 34 2 706 #createAt:extent: 34 2 754 5 39 754 1201 391 1264 706 #text: 34 1 8 'Name' 1264 802 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 19 0 0 0 90 2 0 0 214 0 0 0] 8 #() 864 nil 27 8 'classList' 590342 ##(Smalltalk.Rectangle) 754 21 21 754 21 21 nil nil nil nil 28101 nil nil nil nil 1 nil nil 590598 ##(Smalltalk.Semaphore) nil nil 1 nil 8 1997221904 642 138 144 34 3 706 #createAt:extent: 34 2 754 7679 21 754 1201 581 416 706 #text: 34 1 8 'Jadeite Find Class Dialog' 416 706 #updateMenuBar 848 416 802 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 87 17 0 0 44 1 0 0] 34 4 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 1 2528 nil nil nil 7 nil nil nil 2528 nil 8 1660708144 1010 nil nil nil 642 138 144 34 2 706 #createAt:extent: 34 2 754 1 1 754 211 41 2528 706 #text: 34 1 8 'Partial Name:' 2528 802 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 105 0 0 0 20 0 0 0] 8 #() 864 nil 27 912 1264 544 864 nil 27 )! !
!JadeiteFindClassDialog class categoriesFor: #resource_DefaultView!public!resources-views! !

