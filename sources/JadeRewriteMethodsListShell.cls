"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #JadeRewriteMethodsListShell
	instanceVariableNames: 'rewriteMethodsListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeRewriteMethodsListShell guid: (GUID fromString: '{490a546a-4bb8-4dac-a1d7-175f6b4b1088}')!
JadeRewriteMethodsListShell comment: ''!
!JadeRewriteMethodsListShell categoriesForClass!Unclassified! !
!JadeRewriteMethodsListShell methodsFor!

createComponents

	super createComponents.

	rewriteMethodsListPresenter := self add: self defaultPresenterClass new name: 'rewriteMethodsList'.!

currentRule

	^parentPresenter currentRule!

defaultPresenterClass

	self subclassResponsibility!

isMethodPresenterValue

	^parentPresenter isMethodPresenterValue!

model: aCollection

	super model: aCollection.

	rewriteMethodsListPresenter model: aCollection!

quoteSourceCode: aString

	^parentPresenter quoteSourceCode: aString!

shellName

	^'Jade Rewrite Methods List'!

updateCaption

	self caption: self shellName! !
!JadeRewriteMethodsListShell categoriesFor: #createComponents!public! !
!JadeRewriteMethodsListShell categoriesFor: #currentRule!public! !
!JadeRewriteMethodsListShell categoriesFor: #defaultPresenterClass!public! !
!JadeRewriteMethodsListShell categoriesFor: #isMethodPresenterValue!public! !
!JadeRewriteMethodsListShell categoriesFor: #model:!public! !
!JadeRewriteMethodsListShell categoriesFor: #quoteSourceCode:!public! !
!JadeRewriteMethodsListShell categoriesFor: #shellName!public! !
!JadeRewriteMethodsListShell categoriesFor: #updateCaption!public! !

