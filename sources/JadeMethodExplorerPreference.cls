"Filed out from Dolphin Smalltalk 7"!

JadePreferenceObject subclass: #JadeMethodExplorerPreference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMethodExplorerPreference guid: (GUID fromString: '{5e27df7e-d866-4172-87a4-ada1587a4b00}')!
JadeMethodExplorerPreference comment: ''!
!JadeMethodExplorerPreference categoriesForClass!Unclassified! !
!JadeMethodExplorerPreference methodsFor!

displayString

	^'Method Explorer'!

icon

	^Icon fromId: 'MethodBrowser.ico'!

methodExplorerClass
	^JadeMethodListPresenter getJadeMethodBrowserPresenterClass!

methodExplorerClass: aJadeMethodListPresenterClass
	JadeMethodListPresenter setJadeMethodBrowserPresenterClass: aJadeMethodListPresenterClass!

publishedAspects
	"Answer a <LookupTable> of the <Aspect>s published by the receiver."

	| aspects |
	aspects := super publishedAspects.
	aspects
		add: (Aspect choice: #methodExplorerClass from: (Array with: JadeMethodListPresenter with: JadeTreeMethodListPresenter)).
	^aspects! !
!JadeMethodExplorerPreference categoriesFor: #displayString!public! !
!JadeMethodExplorerPreference categoriesFor: #icon!public! !
!JadeMethodExplorerPreference categoriesFor: #methodExplorerClass!public! !
!JadeMethodExplorerPreference categoriesFor: #methodExplorerClass:!public! !
!JadeMethodExplorerPreference categoriesFor: #publishedAspects!public! !

!JadeMethodExplorerPreference class methodsFor!

icon

	^Icon fromId: 'MethodBrowser.ico'! !
!JadeMethodExplorerPreference class categoriesFor: #icon!public! !

