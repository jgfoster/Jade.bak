"Filed out from Dolphin Smalltalk 7"!

JadeRewriteMethodsListShell subclass: #JadeRewriteReplaceMethodsListShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeRewriteReplaceMethodsListShell guid: (GUID fromString: '{a582417b-62e5-4c66-a984-c0d10965dd06}')!
JadeRewriteReplaceMethodsListShell comment: ''!
!JadeRewriteReplaceMethodsListShell categoriesForClass!Unclassified! !
!JadeRewriteReplaceMethodsListShell methodsFor!

defaultPresenterClass

	^JadeRewriteReplaceMethodsList!

isReplacing

	^parentPresenter isReplacing!

replaceText

	^parentPresenter replaceText! !
!JadeRewriteReplaceMethodsListShell categoriesFor: #defaultPresenterClass!public! !
!JadeRewriteReplaceMethodsListShell categoriesFor: #isReplacing!public! !
!JadeRewriteReplaceMethodsListShell categoriesFor: #replaceText!public! !

!JadeRewriteReplaceMethodsListShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 608 nil 480 nil 7 nil nil nil 608 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeRewriteReplaceMethodsList) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 1 818 1549 903 608 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 6 3 0 0 195 1 0 0] 560 818 193 193 nil 27 8 'rewriteMethodsList' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 706 138 144 34 3 770 #createAt:extent: 34 2 818 2731 21 818 1581 981 416 770 #text: 34 1 8 'Jade Rewrite Methods List' 416 770 #updateMenuBar 560 416 866 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 85 5 0 0 10 0 0 0 107 8 0 0 244 1 0 0] 34 1 608 912 nil 27 )! !
!JadeRewriteReplaceMethodsListShell class categoriesFor: #resource_Default_view!public!resources-views! !

