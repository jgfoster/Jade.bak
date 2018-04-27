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

browseProjects
	self list: RowanProjectService new projects!

checkout
	| service branchName |
	projectListPresenter hasSelection
		ifFalse: 
			[MessageBox notify: 'No project selected'.
			^false].
	service := RowanProjectService named: projectListPresenter selection name.
	branchName := ChoicePrompter choices: service branches caption: 'Select branch to checkout:'.
	branchName isNil ifTrue: [^false] ifFalse: [service checkout: branchName]!

commit
	| projectName message service promptResult |
	projectListPresenter hasSelection
		ifFalse: 
			[MessageBox notify: 'No project selected'.
			^false].
	projectName := projectListPresenter selection name.
	message := String new asValue.
	promptResult := StsMultilinePrompter
				on: message
				prompt: 'Enter commit message. (This will do a GemStone commit after the git commit as well)'
				caption: 'Committing Project ' , projectName.
	promptResult isNil ifTrue: [^self].
	message value isEmpty
		ifTrue: [^MessageBox notify: 'Commit message must not be empty. Commit to local repository not done.'].
	service := RowanProjectService named: projectName.
	service commitWithMessage: message value asString.
	MessageBox notify: 'Project ' , projectName , ' committed to local repository!!'!

createComponents
	super createComponents.
	projectListPresenter := self add: ListPresenter new name: 'projectList'!

inspectProject

	| service |
	service := RowanProjectService named: projectListPresenter selection name.
	service inspectDefinition.!

list: aList
	projectListPresenter list: aList!

loadProject
	| name |
	name := projectListPresenter selection
				ifNil: 
					[MessageBox notify: 'No project selected'.
					^false]
				ifNotNil: [:sel | sel name].
	(MessageBox confirm: 'Really load project ' , name , '? This will destroy any changes you may have.')
		ifFalse: [^false].
	RowanProjectService new loadProjectNamed: name!

makeDefault
	RowanProjectService new defaultProjectName: projectListPresenter selection name!

newGitProject
	| filePath |
	filePath := RowanKeyValuePrompter
				on: 'file:/export/korea2/users/ewinger/shared/repos/RowanSample1/specs/RowanSample1.ston' ->  '/export/korea2/users/ewinger/shared/repos/'
				prompt: 'Please select location of the code repository to clone:'
				caption: 'Clone Git Repository'.
	filePath isNil ifTrue:[^false].
	RowanProjectService new
		newGitProject: filePath key
		root: filePath value
		useSsh: true!

newService
	^RowanProjectService named: projectListPresenter selection name!

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
	| service changesString patch |
	projectListPresenter hasSelection
		ifFalse: 
			[MessageBox notify: 'No project selected'.
			^false].
	service := RowanProjectService named: projectListPresenter selection name.
	patch := service changes.
	RowanPatchBrowser showOn: patch name: projectListPresenter selection name!

projectListPresenter
	^projectListPresenter!

projectLog
	| service |
	projectListPresenter selection isNil
		ifTrue: 
			[MessageBox notify: 'No project selected'.
			^false].
	service := RowanProjectService named: projectListPresenter selection name.
	MessageBox notify: service log!

pullFromGit
	| service |
	projectListPresenter hasSelection
		ifFalse: 
			[MessageBox notify: 'No project selected'.
			^false].
	(MessageBox
		confirm: 'It is recommended that you commit changes before doing a pull from git. Proceed?')
			ifTrue: 
				[service := self newService.
				service pullFromGit]
			ifFalse: [^false]!

pushToGit
	| service |
	projectListPresenter hasSelection
		ifFalse: 
			[MessageBox notify: 'No project selected'.
			^false].
	(MessageBox
		confirm: 'This will push committed changes from the local repository to the remote server. Proceed?')
			ifTrue: 
				[service := self newService.
				service pushToGit]
			ifFalse: [^false]!

showPackages
	| service |
	projectListPresenter selection isNil
		ifTrue: 
			[MessageBox notify: 'No project selected'.
			^false].
	service := RowanProjectService named: projectListPresenter selection name.
	MessageBox notify: service packageNameString!

unloadProject
	MessageBox notify: 'This feature not yet implemented'!

writeProject
	| projectName |
	projectName := projectListPresenter selection name.
	(MessageBox confirm: 'Write project ' , projectName , ' to disk without committing?')
		ifFalse: [^false].
	(RowanProjectService named: projectName) write.
	MessageBox notify: 'Project ' , projectName , ' written to disk'! !
!RowanProjectListPresenter categoriesFor: #browseProjects!public! !
!RowanProjectListPresenter categoriesFor: #checkout!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #commit!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #createComponents!public! !
!RowanProjectListPresenter categoriesFor: #inspectProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #list:!public! !
!RowanProjectListPresenter categoriesFor: #loadProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #makeDefault!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #newGitProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #newService!menu handlers!private! !
!RowanProjectListPresenter categoriesFor: #openChangesShell:!menu handlers!private! !
!RowanProjectListPresenter categoriesFor: #primaryPresenter!public! !
!RowanProjectListPresenter categoriesFor: #projectChanges!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #projectListPresenter!private! !
!RowanProjectListPresenter categoriesFor: #projectLog!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #pullFromGit!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #pushToGit!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #showPackages!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #unloadProject!menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #writeProject!menu handlers!public! !

