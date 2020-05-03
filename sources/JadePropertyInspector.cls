"Filed out from Dolphin Smalltalk 7"!

PropertyInspector subclass: #JadePropertyInspector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePropertyInspector guid: (GUID fromString: '{1bb64f5c-9984-4c76-8a96-aef7b44f7940}')!
JadePropertyInspector comment: ''!
!JadePropertyInspector categoriesForClass!Unclassified! !
!JadePropertyInspector methodsFor!

popupEditAspect
	"Action the currently selected aspect using the current aspect presenter"

	parentPresenter openCustomPopUpFor: aspectTreePresenter selection name ifNone: [super popupEditAspect]! !
!JadePropertyInspector categoriesFor: #popupEditAspect!public! !

