"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadePreferencesShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePreferencesShell guid: (GUID fromString: '{eb2890ef-3d05-4707-ad44-158b04a533af}')!
JadePreferencesShell comment: ''!
!JadePreferencesShell categoriesForClass!Unclassified! !
!JadePreferencesShell methodsFor!

presenterClass

	^JadePreferencesPresenter.
!

queryCommand: aCommandQuery

Transcript cr; show: aCommandQuery printString.
	^super queryCommand: aCommandQuery!

shellName

	^'Jade Preferences'.
! !
!JadePreferencesShell categoriesFor: #presenterClass!overrides!private! !
!JadePreferencesShell categoriesFor: #queryCommand:!public! !
!JadePreferencesShell categoriesFor: #shellName!overrides!private! !

!JadePreferencesShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 551 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 608 nil nil nil 7 nil nil nil 608 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadePreferencesPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 1169 723 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 105 1 0 0] 8 #() 514 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 962 #fixedParentRight 1 962 #fixedParentTop 1 962 #fixedParentBottom 1 170 192 896 nil nil nil nil nil 1 nil nil nil nil 1 nil nil 706 138 144 34 2 770 #createAt:extent: 34 2 514 2879 21 514 1201 801 416 770 #updateMenuBar 896 416 850 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 247 7 0 0 154 1 0 0] 34 1 608 912 nil 27 )! !
!JadePreferencesShell class categoriesFor: #resource_Default_view!public!resources-views! !

