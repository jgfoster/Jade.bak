"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #JadeRewriteChangesBrowser
	instanceVariableNames: 'changedMethodsPresenter differencePresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeRewriteChangesBrowser guid: (GUID fromString: '{e6020731-064c-4d12-91db-6f8a9f8ce241}')!
JadeRewriteChangesBrowser comment: ''!
!JadeRewriteChangesBrowser categoriesForClass!MVP-Presenters! !
!JadeRewriteChangesBrowser methodsFor!

createComponents

	super createComponents.

"	changedMethodsPresenter := self add: aPresenter name: aNameString."

	differencePresenter := self add: DifferencesPresenter new name: 'diff'.
	! !
!JadeRewriteChangesBrowser categoriesFor: #createComponents!public! !

