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
	aPresenter selection: selection!

projectsUpdate: aPresenter
	| services |
	services := projects
				collect: [:project | (RowanProjectService new basicName: project) isDirty: (modifiedProjects includes: project)].
	aPresenter list: services! !
!RowanProjectsUpdate categoriesFor: #projectSelectionUpdate:!public! !
!RowanProjectsUpdate categoriesFor: #projectsUpdate:!public! !

!RowanProjectsUpdate class methodsFor!

new
	^super new! !
!RowanProjectsUpdate class categoriesFor: #new!instance creation!public! !

