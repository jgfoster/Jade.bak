"Filed out from Dolphin Smalltalk 7"!

JadePreferenceObject subclass: #JadeMethodExplorerPreference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeMethodExplorerPreference guid: (GUID fromString: '{5e27df7e-d866-4172-87a4-ada1587a4b00}')!
JadeMethodExplorerPreference comment: ''!
!JadeMethodExplorerPreference categoriesForClass!Unclassified! !
!JadeMethodExplorerPreference class methodsFor!

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
!JadeMethodExplorerPreference class categoriesFor: #displayString!public! !
!JadeMethodExplorerPreference class categoriesFor: #icon!public! !
!JadeMethodExplorerPreference class categoriesFor: #methodExplorerClass!public! !
!JadeMethodExplorerPreference class categoriesFor: #methodExplorerClass:!public! !
!JadeMethodExplorerPreference class categoriesFor: #publishedAspects!public! !

