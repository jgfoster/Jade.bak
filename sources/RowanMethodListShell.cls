"Filed out from Dolphin Smalltalk 7"!

RowanShell subclass: #RowanMethodListShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanMethodListShell guid: (GUID fromString: '{68b0fe86-7704-4d47-bfa2-976beaad877a}')!
RowanMethodListShell comment: ''!
!RowanMethodListShell categoriesForClass!Unclassified! !
!RowanMethodListShell methodsFor!

presenterClass

	^RowanMethodListPresenter! !
!RowanMethodListShell categoriesFor: #presenterClass!public! !

!RowanMethodListShell class methodsFor!

browseMethodDefinitions: aCollection

	!

displaySampleMethod
	"display a sample method definition"

	| session |
	session := GciSession allInstances first.
	(RowanMethodListShell showOnSession: session) primaryPresenter
		browseImplementorsOf: 'size';
		yourself!

presenterClass

	^RowanMethodListPresenter!

resource_RowanMethodListShell
	"Answer the literal data from which the 'RowanMethodListShell' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_RowanMethodListShell)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 551 nil nil nil 416 nil 170 192 8 #() nil nil nil nil nil 1 nil nil nil nil 1 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 3839 21 658 1801 1201 416 610 #text: 34 1 8 'Rowan Method List' 416 610 #updateMenuBar 528 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 3 11 0 0 98 2 0 0] 34 1 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 832 nil nil nil 7 nil nil nil 832 nil 170 192 528 nil 546 138 144 34 1 610 #createAt:extent: 34 2 658 1 1 658 1801 1201 832 770 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 132 3 0 0 88 2 0 0] 34 2 410 ##(Smalltalk.MultipleSelectionListView) 34 30 nil 832 34 2 8 1140920393 1025 1072 590662 2 ##(Smalltalk.ListModel) 138 144 528 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 nil 7 nil nil nil 1072 nil 8 4294905043 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 3 920646 5 ##(Smalltalk.ListViewColumn) 8 'Class' 601 #left 1266 #displayString 1296 ##(Smalltalk.SortedCollection) nil nil 1072 nil 1 nil nil 1378 8 'Selector' 601 #left 1266 #displayString 8 #() 1266 #<= 1488 nil nil 1072 nil 1 nil nil 1378 8 'Package' 601 #left 1266 #displayString 1488 1266 #<= 1488 nil nil 1072 nil 1 nil nil #report 528 nil 131169 nil 34 4 nil nil 658 1 1 nil 546 138 144 34 2 610 #createAt:extent: 34 2 658 1 1 658 1801 601 1072 610 #text: 34 1 8 'Class' 1072 770 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 132 3 0 0 44 1 0 0] 8 #() 658 193 193 nil 27 410 ##(Smalltalk.RichTextEdit) 34 18 nil 832 34 2 8 1140920644 1025 1840 nil 466 8 4278190080 nil 7 265030 4 ##(Smalltalk.Menu) nil true 34 10 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #chooseSelectionFont 8 '&Font...' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 1986 1 2018 #bePlain 8 '&Plain' 1 1 nil nil nil 1986 1 2018 #toggleBold 8 '&Bold' 1 1 nil nil nil 1986 1 2018 #toggleItalic 8 '&Italic' 1 1 nil nil nil 1986 1 2018 #toggleUnderlined 8 '&Underlined' 1 1 nil nil nil 2066 4097 1938 nil true 34 3 1986 1025 2018 #alignParagraphLeft 8 '&Left' 1 1 nil nil nil 1986 1025 2018 #alignParagraphCenter 8 '&Centre' 1 1 nil nil nil 1986 1025 2018 #alignParagraphRight 8 '&Right' 1 1 nil nil nil 8 '&Align' nil 1 nil nil nil nil nil 2066 4097 1986 1 2018 #chooseSelectionColor 8 '&Colour...' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 1840 nil 8 1692289568 852486 ##(Smalltalk.NullConverter) nil nil 9 nil 655622 ##(Smalltalk.EDITSTREAM) 8 #[0 0 0 0 0 0 0 0 48 0 90 3] 546 138 144 34 4 610 #createAt:extent: 34 2 658 1 601 658 1801 601 1840 610 #contextMenu: 34 1 1952 1840 610 #text: 34 1 524550 ##(Smalltalk.RichText) 8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\froman Times New Roman;}}
\viewkind4\uc1\pard\f0\fs22 
\par }
' 1840 610 #resetCharFormat 528 1840 770 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 44 1 0 0 132 3 0 0 88 2 0 0] 8 #() 1824 nil 27 1824 nil 27 1824 nil 27 )! !
!RowanMethodListShell class categoriesFor: #browseMethodDefinitions:!instance creation!public! !
!RowanMethodListShell class categoriesFor: #displaySampleMethod!public! !
!RowanMethodListShell class categoriesFor: #presenterClass!private! !
!RowanMethodListShell class categoriesFor: #resource_RowanMethodListShell!public!resources-views! !

