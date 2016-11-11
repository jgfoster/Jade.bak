"Filed out from Dolphin Smalltalk 7"!

TextPresenter subclass: #JadeTextPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeTextPresenter guid: (GUID fromString: '{483A5019-CE38-4F6A-B12D-D39804FC00F4}')!
JadeTextPresenter comment: ''!
!JadeTextPresenter categoriesForClass!Unclassified! !
!JadeTextPresenter methodsFor!

onViewOpened

	super onViewOpened.
	(view isKindOf: ScintillaView) ifFalse: [^self].
	view textStyles: JadeTextStyles default textStyles.
!

selection

	^view selection.
! !
!JadeTextPresenter categoriesFor: #onViewOpened!public! !
!JadeTextPresenter categoriesFor: #selection!public! !

