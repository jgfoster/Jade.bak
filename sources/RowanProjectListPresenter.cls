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

inspectProject

	| service |
	service := RowanProjectService named: projectListPresenter selection name.
	service inspectDefinition.!

list: aList
	projectListPresenter list: aList!

loadProject

	RowanProjectService new loadProjectNamed: projectListPresenter selection name!

makeDefault
	RowanProjectService new defaultProjectName: projectListPresenter selection name.
	projectListPresenter view refreshContents!

newGitProject
	| filePath |
	filePath := Prompter
				on: 'file:/foos2/users/ewinger/GsDevKit_home/shared/repos/RowanSampler/specs/RowanSample1.ston'
				prompt: 'Please select location of the code repository to clone:'
				caption: 'Clone Git Repository'.
	RowanProjectService new
		newGitProject: filePath
		root: '$GS_HOME/shared/repos/'
		useSsh: true!

openChangesShell: changesString
	| shell editor rs |
	editor := RichTextEdit new.
	editor value: changesString.
	shell := ShellView new
				create;
				yourself.
	shell extent: 600 @ 600.
	shell layoutManager: GridLayout new.
	editor parentView: shell.
	editor create show.
	shell caption: projectListPresenter selection name, ' project changes'.
	shell show.
	rs := ReadStream on: changesString.
	[rs atEnd] whileFalse: 
			[| line startPosition checkChar |
			startPosition := rs position.
			line := rs nextLine.
			checkChar := line isEmpty ifTrue: [nil] ifFalse: [line first].
			checkChar = $+
				ifTrue: 
					[editor selectionRange: (startPosition to: rs position - 1).
					editor selectionColor: Color darkGreen].
			checkChar = $-
				ifTrue: 
					[editor selectionRange: (startPosition to: rs position - 1).
					editor selectionColor: Color red]].
	editor selectionRange: editor defaultSelectionRange.
	editor invalidate!

primaryPresenter
	^projectListPresenter!

projectChanges
	| service changesString |
	service := RowanProjectService named: projectListPresenter selection name.
	changesString := service changes.
	self openChangesShell: changesString!

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
	!

writeProject

	| projectName |
	projectName := projectListPresenter selection name.
	(MessageBox confirm: 'Write project ', projectName, ' to disk without committing?') ifFalse:[^self].
	(RowanProjectService named: projectName) write.
	MessageBox notify: 'Project ', projectName, ' written to disk'! !
!RowanProjectListPresenter categoriesFor: #addProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #browseProjects!public! !
!RowanProjectListPresenter categoriesFor: #createComponents!public! !
!RowanProjectListPresenter categoriesFor: #deleteProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #inspectProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #list:!public! !
!RowanProjectListPresenter categoriesFor: #loadProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #makeDefault!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #newGitProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #openChangesShell:!menu handlers!private! !
!RowanProjectListPresenter categoriesFor: #primaryPresenter!public! !
!RowanProjectListPresenter categoriesFor: #projectChanges!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #projectListPresenter!private! !
!RowanProjectListPresenter categoriesFor: #projectLog!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #pushToGit!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #revertProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #saveProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #writeProject!menu handlers!public! !

