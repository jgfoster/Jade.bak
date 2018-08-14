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
	self list: (RowanProjectService on: gciSession) projects!

checkout
	projectListPresenter selection checkoutUsing: self.!

cloneProject
	"Does a clone and 'attach'"
	| filePath |
	filePath := RowanKeyValuePrompter
				on: 'file:$ROWAN_PROJECTS_HOME/Rowan/samples/RowanSample1.ston' -> '$ROWAN_PROJECTS_HOME'
				prompt: 'Please select location of the code repository to clone:'
				caption: 'Clone Git Repository'.
	filePath isNil ifTrue: [^false].
	(RowanProjectService on: gciSession) 
		newGitProject: filePath key
		root: filePath value
		useSsh: true!

commit
	| projectName message service promptResult |
	projectListPresenter hasSelection ifFalse: [MessageBox notify: 'No project selected'].
	projectListPresenter selections size > 1
		ifTrue: [MessageBox notify: 'Only one project may be committed'].
	projectName := projectListPresenter selection name.
	message := String new asValue.
	promptResult := StsMultilinePrompter
				on: message
				prompt: 'Enter Git commit message. (Note - This will NOT do a GemStone commit)'
				caption: 'Committing Project ' , projectName.
	promptResult isNil ifTrue: [^self].
	message value isEmpty
		ifTrue: [^MessageBox notify: 'Commit message must not be empty. Commit to local repository NOT done.'].
	service := projectListPresenter selection.
	service
		command: #commitWithMessage:;
		commandArgs: (Array with: message value asString).
	self issueCommand: (Array with: service).
	MessageBox notify: 'Project ' , projectName , ' committed to local repository!!'!

createComponents
	super createComponents.
	projectListPresenter := self add: ListPresenter new name: 'projectList'!

list: aList
	projectListPresenter list: aList!

loadProject
	| selection |
	selection := projectListPresenter selection.
	(MessageBox
		confirm: 'Really load project ' , selection name , '? This will destroy any changes you may have.')
			ifFalse: [^false].
	selection loadProjectFrom: self!

makeDefault
	(RowanProjectService on: gciSession) defaultProjectName: projectListPresenter selection name!

newService
	^(RowanProjectService on: gciSession) name: projectListPresenter selection name!

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
	projectListPresenter selection changesUsing: self!

projectListPresenter
	^projectListPresenter!

projectLog
	projectListPresenter selection projectLogUsing: self!

pullFromGit
	| projectServices ws |
	projectServices := projectListPresenter selections.
	projectServices isEmpty ifTrue: [^MessageBox notify: 'No project selected'].
	ws := WriteStream on: String new.
	projectServices do: 
			[:service |
			ws
				nextPutAll: service name;
				space].
	(MessageBox
		confirm: 'It is recommended that you commit changes before doing a pulling projects - ' , ws contents
				, ' -  from git. Really proceed?')
			ifFalse: [^self].
	projectServices do: [:service | service command: #pushToGit].
	self issueCommand: projectServices!

pushToGit
	| projectServices ws |
	projectServices := projectListPresenter selections.
	projectServices isEmpty ifTrue: [^MessageBox notify: 'No project selected'].
	ws := WriteStream on: String new.
	projectServices do: 
			[:service |
			ws
				nextPutAll: service name;
				space].
	(MessageBox
		confirm: 'Push projects - ' , ws contents , '- from the local repository to the remote server?')
			ifFalse: [^self].
	projectServices do: [:service | service command: #pushToGit].
	self issueCommand: projectServices!

unloadProject
	MessageBox notify: 'This feature not yet implemented'!

writeProject
	| projectServices ws |
	projectServices := projectListPresenter selections.
	projectServices isEmpty ifTrue: [^MessageBox notify: 'No project selected'].
	ws := WriteStream on: String new.
	projectServices do: 
			[:service |
			ws
				nextPutAll: service name;
				space].
	(MessageBox confirm: 'Write projects - ' , ws contents , '- to disk without committing?')
		ifFalse: [^false].
	projectServices do: [:service | service command: #write].
	self issueCommand: projectServices.
	MessageBox notify: 'Projects ' ,  ws contents  , ' written to disk'! !
!RowanProjectListPresenter categoriesFor: #browseProjects!public! !
!RowanProjectListPresenter categoriesFor: #checkout!project menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #cloneProject!project menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #commit!project menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #createComponents!public! !
!RowanProjectListPresenter categoriesFor: #list:!public! !
!RowanProjectListPresenter categoriesFor: #loadProject!project menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #makeDefault!project menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #newService!private!project menu handlers! !
!RowanProjectListPresenter categoriesFor: #openChangesShell:!private!project menu handlers! !
!RowanProjectListPresenter categoriesFor: #primaryPresenter!public! !
!RowanProjectListPresenter categoriesFor: #projectChanges!project menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #projectListPresenter!private! !
!RowanProjectListPresenter categoriesFor: #projectLog!project menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #pullFromGit!project menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #pushToGit!project menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #unloadProject!project menu handlers!public! !
!RowanProjectListPresenter categoriesFor: #writeProject!project menu handlers!public! !

