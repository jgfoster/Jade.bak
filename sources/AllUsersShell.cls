"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #AllUsersShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AllUsersShell guid: (GUID fromString: '{2E98AD2C-4678-44A7-86F8-CD5D4C42CF12}')!
AllUsersShell comment: ''!
!AllUsersShell categoriesForClass!Unclassified! !
!AllUsersShell methodsFor!

presenterClass
 
	^AllUsersPresenter.
!

shellName
 
	^'User Browser'.
! !
!AllUsersShell categoriesFor: #presenterClass!private! !
!AllUsersShell categoriesFor: #shellName!overrides!private! !

!AllUsersShell class methodsFor!

icon

	^Icon fromFile: 'Jade\icons\GS32x32.ico'.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 608 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 5119 21 530 1201 801 416 706 8 #updateMenuBar 608 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 9 0 0 10 0 0 0 87 12 0 0 154 1 0 0] 98 1 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 896 0 482 8 4278190080 0 7 0 0 0 896 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.AllUsersPresenter)  8 #resource_Default_view 0 642 202 208 98 1 706 736 98 2 530 1 1 530 1169 725 896 834 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 106 1 0 0] 608 530 193 193 0 27 1216 0 27 )! !
!AllUsersShell class categoriesFor: #icon!public! !
!AllUsersShell class categoriesFor: #resource_Default_view!public!resources-views! !

