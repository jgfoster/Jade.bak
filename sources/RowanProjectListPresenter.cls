"Filed out from Dolphin Smalltalk 7"!

RowanPresenter subclass: #RowanProjectListPresenter
	instanceVariableNames: 'projectListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectListPresenter guid: (GUID fromString: '{b13b9082-2fe0-487d-b717-84410faf12ea}')!
RowanProjectListPresenter comment: ''!
!RowanProjectListPresenter categoriesForClass!MVP-Presenters! !
!RowanProjectListPresenter methodsFor!

addProject
	| newProjectName service |
	newProjectName := Prompter prompt: 'Please enter the name of the Rowan project ...'
				caption: 'Add Rowan Project'.
	newProjectName isNil ifTrue: [^self].
	service := RowanProjectService new.
	service addProjectNamed: newProjectName.
	self list: service projects!

browseProjects
	self list: RowanProjectService new projects!

createComponents
	super createComponents.
	projectListPresenter := self add: ListPresenter new name: 'projectList'!

deleteProject

	MessageBox notify: 'This feature not yet implemented'!

list: aList
	projectListPresenter list: aList!

makeDefault
	RowanProjectService new defaultProjectName: projectListPresenter selection name.
	projectListPresenter view refreshContents!

primaryPresenter
	^projectListPresenter!

projectChanges

	| service |
	service := RowanProjectService named: projectListPresenter selection name.
	service changes inspect!

projectListPresenter
	^projectListPresenter!

projectLog

	MessageBox notify: 'This feature not yet implemented'!

pushToGit

	MessageBox notify: 'This feature not yet implemented'!

revertProject

	MessageBox notify: 'This feature not yet implemented'!

saveProject

	MessageBox notify: 'Save Project is not implemented yet'.
	! !
!RowanProjectListPresenter categoriesFor: #addProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #browseProjects!public! !
!RowanProjectListPresenter categoriesFor: #createComponents!public! !
!RowanProjectListPresenter categoriesFor: #deleteProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #list:!public! !
!RowanProjectListPresenter categoriesFor: #makeDefault!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #primaryPresenter!public! !
!RowanProjectListPresenter categoriesFor: #projectChanges!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #projectListPresenter!private! !
!RowanProjectListPresenter categoriesFor: #projectLog!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #pushToGit!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #revertProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #saveProject!menu handlers!public! !
