"Filed out from Dolphin Smalltalk 7"!

RowanCommandResult subclass: #RowanProjectsUpdate
	instanceVariableNames: 'projects modifiedProjects selection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectsUpdate guid: (GUID fromString: '{c47a8cd5-d11c-486b-83a2-a1e1839bb959}')!
RowanProjectsUpdate comment: ''!
!RowanProjectsUpdate categoriesForClass!Unclassified! !
!RowanProjectsUpdate methodsFor!

projectSelectionUpdate: aPresenter
	aPresenter selections: selection!

projectsUpdate: aPresenter
	aPresenter list: projects! !
!RowanProjectsUpdate categoriesFor: #projectSelectionUpdate:!public!updating! !
!RowanProjectsUpdate categoriesFor: #projectsUpdate:!public!updating! !

!RowanProjectsUpdate class methodsFor!

new
	^super new! !
!RowanProjectsUpdate class categoriesFor: #new!instance creation!public! !

