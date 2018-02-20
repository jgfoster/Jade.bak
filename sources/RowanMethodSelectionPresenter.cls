"Filed out from Dolphin Smalltalk 7"!

RowanPresenter subclass: #RowanMethodSelectionPresenter
	instanceVariableNames: 'methodListPresenter methodSourcePresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodSelectionPresenter guid: (GUID fromString: '{b2c968e9-9b1a-4580-a488-111ffd359862}')!
RowanMethodSelectionPresenter comment: ''!
!RowanMethodSelectionPresenter categoriesForClass!Unclassified! !
!RowanMethodSelectionPresenter methodsFor!

createComponents

	super createComponents.
	methodListPresenter 	:= self add: RowanMethodListPresenter	new name: 'methodList'.
	methodSourcePresenter	:= (self add: RowanMethodSourcePresenter	new name: 'methodSource') menuTitle: '&Method Source'; yourself.
!

methodListPresenter
	^methodListPresenter!

methodSourcePresenter
	^methodSourcePresenter! !
!RowanMethodSelectionPresenter categoriesFor: #createComponents!public! !
!RowanMethodSelectionPresenter categoriesFor: #methodListPresenter!accessing!private! !
!RowanMethodSelectionPresenter categoriesFor: #methodSourcePresenter!accessing!private! !

!RowanMethodSelectionPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1140850688 131073 416 nil nil nil 7 nil nil nil 416 nil 170 192 8 #() nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 3839 21 626 1801 1201 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 3 11 0 0 98 2 0 0] 34 2 410 ##(Smalltalk.MultipleSelectionListView) 34 30 nil 416 34 2 8 1140920393 1025 736 590662 2 ##(Smalltalk.ListModel) 138 144 496 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 7 nil nil nil 736 nil 8 4294905043 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 3 920646 5 ##(Smalltalk.ListViewColumn) 8 'Class' 601 #left 946 #displayString 976 ##(Smalltalk.SortedCollection) nil nil 736 nil 1 nil nil 1058 8 'Selector' 601 #left 946 #displayString 8 #() 946 #<= 1168 nil nil 736 nil 1 nil nil 1058 8 'Package' 601 #left 946 #displayString 1168 946 #<= 1168 nil nil 736 nil 1 nil nil #report 496 nil 131169 nil 34 4 nil nil 626 1 1 nil 514 138 144 34 2 578 #createAt:extent: 34 2 626 1 1 626 1801 601 736 578 #text: 34 1 8 'Class' 736 674 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 132 3 0 0 44 1 0 0] 8 #() 626 193 193 nil 27 410 ##(Smalltalk.RichTextEdit) 34 18 nil 416 34 2 8 1140920644 1025 1520 nil 882 8 4278190080 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 10 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #chooseSelectionFont 8 '&Font...' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 1666 1 1698 #bePlain 8 '&Plain' 1 1 nil nil nil 1666 1 1698 #toggleBold 8 '&Bold' 1 1 nil nil nil 1666 1 1698 #toggleItalic 8 '&Italic' 1 1 nil nil nil 1666 1 1698 #toggleUnderlined 8 '&Underlined' 1 1 nil nil nil 1746 4097 1618 nil true 34 3 1666 1025 1698 #alignParagraphLeft 8 '&Left' 1 1 nil nil nil 1666 1025 1698 #alignParagraphCenter 8 '&Centre' 1 1 nil nil nil 1666 1025 1698 #alignParagraphRight 8 '&Right' 1 1 nil nil nil 8 '&Align' nil 1 nil nil nil nil nil 1746 4097 1666 1 1698 #chooseSelectionColor 8 '&Colour...' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 1520 nil 8 1692289568 852486 ##(Smalltalk.NullConverter) nil nil 9 nil 655622 ##(Smalltalk.EDITSTREAM) 8 #[0 0 0 0 0 0 0 0 48 0 90 3] 514 138 144 34 4 578 #createAt:extent: 34 2 626 1 601 626 1801 601 1520 578 #contextMenu: 34 1 1632 1520 578 #text: 34 1 524550 ##(Smalltalk.RichText) 8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\froman Times New Roman;}}
\viewkind4\uc1\pard\f0\fs22 
\par }
' 1520 578 #resetCharFormat 496 1520 674 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 44 1 0 0 132 3 0 0 88 2 0 0] 8 #() 1504 nil 27 1504 nil 27 )! !
!RowanMethodSelectionPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

