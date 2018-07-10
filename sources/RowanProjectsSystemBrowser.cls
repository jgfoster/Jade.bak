"Filed out from Dolphin Smalltalk 7"!

RowanSystemBrowser subclass: #RowanProjectsSystemBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectsSystemBrowser guid: (GUID fromString: '{8b442830-2c48-4a43-aff6-375f4d49530d}')!
RowanProjectsSystemBrowser comment: ''!
!RowanProjectsSystemBrowser categoriesForClass!Unclassified! !
!RowanProjectsSystemBrowser methodsFor!

addSystemBrowserWithLayoutInfo: each
	| browser |
	browser := (self systemBrowserPresenter createIn: cardsPresenter gciSession: gciSession) layoutInfo: each.
	browser projectDictionaryChanged!

shellName

	^'Rowan Projects Browser'.
!

systemBrowserPresenter
	^RowanProjectsBrowserPresenter! !
!RowanProjectsSystemBrowser categoriesFor: #addSystemBrowserWithLayoutInfo:!public! !
!RowanProjectsSystemBrowser categoriesFor: #shellName!overrides!private! !
!RowanProjectsSystemBrowser categoriesFor: #systemBrowserPresenter!public! !

