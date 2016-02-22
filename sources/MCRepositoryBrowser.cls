"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #MCRepositoryBrowser
	instanceVariableNames: 'repositoryListPresenter packageListPresenter versionListPresenter repositoryCreationTemplatePresenter versionNamePresenter versionDatePresenter versionTimePresenter versionAuthorPresenter versionIDPresenter versionAncestorsPresenter versionStepChildrenPresenter versionMessagePresenter loadedVersionNames'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCRepositoryBrowser guid: (GUID fromString: '{ED52D4AC-3C47-4B09-830B-99228086125C}')!
MCRepositoryBrowser comment: ''!
!MCRepositoryBrowser categoriesForClass!Unclassified! !
!MCRepositoryBrowser methodsFor!

addDirectoryRepository

	| path |
	path := BrowseFolderDialog new 
		title: 'Please select a Monticello folder';
		showModal.
	path isNil ifTrue: [^self].
	gciSession
		serverPerform: #'mcNewDirectoryRepository:' 
		with: path.
	self updateRepositoryList.
!

addFileTreeRepository

	| path |
	(path := Prompter prompt: 'Enter server path:') isNil ifTrue: [^self].
	gciSession
		serverPerform: #'mcNewFileTreeRepository:' 
		with: path.
	self updateRepositoryList.
!

addGitHubRepository

	| path |
	(path := Prompter prompt: 'Enter location (e.g., ''github://glassdb/zinc:gemstone3.1/repository''):') isNil ifTrue: [^self].
	gciSession
		serverPerform: #'mcNewGitHubRepository:' 
		with: path.
	self updateRepositoryList.
!

addHttpRepository

	| info string delimiter |
	(info := MCHttpRepositoryInfoDialog showModalOn: MCHttpRepositoryInfo new) isNil ifTrue: [^self].
	delimiter := (Character codePoint: 255) asString.
	string := info location , delimiter , info user , delimiter , info password.
	gciSession
		serverPerform: #'mcAddHttpRepository:' 
		with: string.
	self updateRepositoryList.
!

addRepository

	| type |
	type := ChoicePrompter
		choices: #(#'HTTP' #'Client Directory' #'Server Directory' #'Server Directory with FileTree' #'Server Directory with GitHub' ) 
		caption: 'Choose Repository Type'.
	type isNil ifTrue: [^self].
	type = #'HTTP' ifTrue: [^self addHttpRepository].
	type = #'Client Directory' ifTrue: [^self addDirectoryRepository].
	type = #'Server Directory' ifTrue: [^self addServerDirectoryRepository].
	type = #'Server Directory with FileTree' ifTrue: [^self addFileTreeRepository].
	type = #'Server Directory with GitHub' ifTrue: [^self addGitHubRepository].
	self error: 'Unrecognized repository type: ' , type printString.
!

addServerDirectoryRepository

	| path |
	(path := Prompter prompt: 'Enter server path:') isNil ifTrue: [^self].
	gciSession
		serverPerform: #'mcNewServerDirectoryRepository:' 
		with: path.
	self updateRepositoryList.
!

compareVersion

	| selections left right patch |
	selections := versionListPresenter selections.
	(selections isEmpty or: [2 < selections size]) ifTrue: [
		MessageBox notify: 'Please select one or two versions!!'. 
		^self.
	].
	left := selections size = 1
		ifTrue: [nil]
		ifFalse: [packageListPresenter selection name , '-' , selections first name].
	right := packageListPresenter selection name , '-' , selections last name.
	right < left ifTrue: [
		| temp |
		temp := right.
		right := left.
		left := temp.
	].
	patch := repositoryListPresenter selection patchFrom: left to: right.
	MCPatchBrowser showOn: patch.
!

createComponents

	super createComponents.
	repositoryListPresenter 						:= self add: ListPresenter new name: 'repositoryList'.
	packageListPresenter 						:= self add: ListPresenter new name: 'packageList'.
	versionListPresenter 							:= self add: ListPresenter new name: 'versionList'.
	repositoryCreationTemplatePresenter	:= self add: TextPresenter new name: 'repositoryCreationTemplate'.
	versionNamePresenter 						:= self add: TextPresenter new name: 'versionName'.
	versionDatePresenter 						:= self add: TextPresenter new name: 'versionDate'.
	versionTimePresenter 						:= self add: TextPresenter new name: 'versionTime'.
	versionAuthorPresenter 					:= self add: TextPresenter new name: 'versionAuthor'.
	versionIDPresenter							:= self add: TextPresenter new name: 'versionID'.
	versionAncestorsPresenter 				:= self add: ListPresenter new name: 'versionAncestors'.
	versionStepChildrenPresenter 			:= self add: ListPresenter new name: 'versionStepChildren'.
	versionMessagePresenter 					:= self add: TextPresenter new name: 'versionMessage'.

!

createSchematicWiring

	super createSchematicWiring.
	repositoryListPresenter		when: #'selectionChanged' send: #'updatePackageList' to: self.
	repositoryListPresenter		when: #'selectionChanged' send: #'updateRepositoryCreationTemplate' to: self.
	packageListPresenter		when: #'selectionChanged' send: #'updateVersionList' to: self.
	versionListPresenter			when: #'selectionChanged' send: #'updateVersionInfo' to: self.
!

editRepository

	MessageBox notify: 'Sorry, we are not yet prepared to do that!!'.
	SessionManager current pause.
!

fileTypes

	^Array
		with: #('GemStone Files (*.gs)' '*.gs')
		with: #('Smalltalk Files (*.st)' '*.st')
		with: #('Topaz Files (*.tpz)' '*.tpz')
		with: FileDialog allFilesType.
!

getLoadedVersionNames

	| string list |
	(string := gciSession serverPerform: #'mcLoadedVersionNames') isNil ifTrue: [^self].
	loadedVersionNames := Dictionary new.
	list := string subStrings: Character lf.
	list := list collect: [:each | each subStrings: Character tab].
	list do: [:each | 
		| i name version |
		string := each at: 1.
		i := string findLast: [:char | char = $-].
		name := i = 0 
			ifTrue: [string]
			ifFalse: [string copyFrom: 1 to: i - 1].
		version := i = 0
			ifTrue: ['']
			ifFalse: [string copyFrom: i + 1 to: string size].
		loadedVersionNames
			at: name
			put: version -> ((each at: 2) = 'Y').
	].
!

loadVersion

	| repository package versionName |
	(repository := repositoryListPresenter selectionOrNil) isNil ifTrue: [^self].
	(package := packageListPresenter selectionOrNil) isNil ifTrue: [^self].
	(versionName := versionListPresenter selectionOrNil) isNil ifTrue: [^self].
	repository
		loadPackageNamed: package name
		versionName: versionName name.
	self getLoadedVersionNames.
	versionListPresenter list do: [:each | each isLoaded: false; isModified: false].
	versionName isLoaded: true.
	package loaded: versionName name -> false.
	packageListPresenter view refreshContents.
	packageListPresenter selection: package.
	versionListPresenter selection: versionName.
!

mergeVersion

	| repository package versionName versionFullName | 
	(repository := repositoryListPresenter selectionOrNil) isNil ifTrue: [^self].
	(package := packageListPresenter selectionOrNil) isNil ifTrue: [^self].
	(versionName := versionListPresenter selectionOrNil) isNil ifTrue: [^self].
	versionFullName := package name , '-' , versionName name.
	gciSession
		serverPerform: #'mcVersionMerge:from:autoMigrate:' 
		with: versionFullName
		with: repository
		with: true.
	self getLoadedVersionNames.
	versionListPresenter list do: [:each | each isLoaded: false; isModified: false].
	versionName isLoaded: true.
	package loaded: versionName name -> false.
	packageListPresenter view refreshContents.
	packageListPresenter selection: package.
	versionListPresenter selection: versionName.
!

onViewOpened

	super onViewOpened.
	self 
		getLoadedVersionNames;
		updateRepositoryList;
		yourself.
!

queryCommand: aCommandQuery

	(#(#removeRepository) includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: repositoryListPresenter hasSelection.
		^true.
	].
	(#(#removePackage) includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: packageListPresenter hasSelection.
		^true.
	].
	(#(#loadVersion) includes: aCommandQuery commandSymbol) ifTrue: [
		aCommandQuery isEnabled: versionListPresenter hasSelection.
		^true.
	].
	^super queryCommand: aCommandQuery.
!

removeRepository

	| repository |
	repository := repositoryListPresenter selection.
	gciSession
		serverPerform: #'mcRemoveRepository:'
		with: repository.
	self updateRepositoryList.
!

saveAsTopazFileIn

	| repository packageName index fileName path bytes |
	(repository := repositoryListPresenter selectionOrNil) ifNil: [^self].
	packageName := repository 
		fullNameOfPackage: packageListPresenter selection name 
		versionName: versionListPresenter selection name.
	index := (packageName includes: $.)
		ifTrue: [(packageName subStrings: $.) last size]
		ifFalse: [-1].
	fileName := packageName copyFrom: 1 to: packageName size - index - 1.
	path := FileSaveDialog new
		caption: 'File Out ' , fileName;
		fileTypes: self fileTypes;
		defaultExtension: 'gs';
		value: fileName;
		overwritePrompt;
		showModal.
	path isNil ifTrue: [^self].
	bytes := repository topazFrom: packageName.
	(FileStream 
		write: path
		text: false)
		nextPutAll: bytes;
		close.
!

shellName 

	^'Monticello Browser'
!

updatePackageList

	| repository list |
	packageListPresenter list: #().
	(repository := repositoryListPresenter selectionOrNil) isNil ifTrue: [^self].
	list := repository packageList.
	list do: [:each | 
		each loaded: (loadedVersionNames 
			at: each name
			ifAbsent: [nil]).
	].
	packageListPresenter list: list.
!

updateRepositoryCreationTemplate

	| repository string i j |
	repositoryCreationTemplatePresenter view ensureVisible.
	repositoryCreationTemplatePresenter value: ''.
	(repository := repositoryListPresenter selectionOrNil) isNil ifTrue: [^self].
	(string := repository creationTemplate) isNil ifTrue: [^self].
	0 < (i := string indexOfSubCollection: 'password:') ifTrue: [
		i := i + 10.
		j := string
			indexOfSubCollection: ''''
			startingAt: i + 1.
		i + 1 < j ifTrue: [
			string := (string copyFrom: 1 to: i) , '*****' , (string copyFrom: j to: string size).
		].
	].
	repositoryCreationTemplatePresenter value: string.
!

updateRepositoryList

	| list |
	repositoryCreationTemplatePresenter view ensureVisible.
	list := MCRepository allIn: gciSession.
	repositoryListPresenter list: list asSortedCollection.
!

updateVersionInfo

	| repository package versionName version |
	(self view viewNamed: 'versionInfo') ensureVisible.
	versionNamePresenter 				value: ''.
	versionDatePresenter 				value: ''.
	versionTimePresenter 				value: ''.
	versionAuthorPresenter 			value: ''.
	versionIDPresenter 					value: ''.
	versionAncestorsPresenter 		list: #().
	versionStepChildrenPresenter 	list: #().
	versionMessagePresenter 			value: ''.
	(repository := repositoryListPresenter selectionOrNil) isNil ifTrue: [^self].
	(package := packageListPresenter selectionOrNil) isNil ifTrue: [^self].
	(versionName := versionListPresenter selectionOrNil) isNil ifTrue: [^self].
	(version := package infoForVersion: versionName name) isNil ifTrue: [^self].
	versionNamePresenter 				value: 	version name.
	versionDatePresenter 				value: 	version date.
	versionTimePresenter 				value: 	version time.
	versionAuthorPresenter 			value: 	version author.
	versionIDPresenter 					value: 	version id.
	versionAncestorsPresenter 		list: 		version ancestors.
	versionStepChildrenPresenter 	list: 		version stepChildren.
	versionMessagePresenter 			value: 	version message.
!

updateVersionList

	| package list name |
	versionListPresenter list: #().
	(package := packageListPresenter selectionOrNil) isNil ifTrue: [^self].
	list := package versionNames.
	package isLoaded ifTrue: [
		name := package loadedEditionName.
		list do: [:each | 
			each printString = name ifTrue: [
				each 
					isLoaded: true;
					isModified: package isModified;
					yourself.
			].
		].
	].
	versionListPresenter list: list reverse.
! !
!MCRepositoryBrowser categoriesFor: #addDirectoryRepository!public! !
!MCRepositoryBrowser categoriesFor: #addFileTreeRepository!public! !
!MCRepositoryBrowser categoriesFor: #addGitHubRepository!public! !
!MCRepositoryBrowser categoriesFor: #addHttpRepository!public! !
!MCRepositoryBrowser categoriesFor: #addRepository!public! !
!MCRepositoryBrowser categoriesFor: #addServerDirectoryRepository!public! !
!MCRepositoryBrowser categoriesFor: #compareVersion!public! !
!MCRepositoryBrowser categoriesFor: #createComponents!public! !
!MCRepositoryBrowser categoriesFor: #createSchematicWiring!public! !
!MCRepositoryBrowser categoriesFor: #editRepository!public! !
!MCRepositoryBrowser categoriesFor: #fileTypes!public! !
!MCRepositoryBrowser categoriesFor: #getLoadedVersionNames!public! !
!MCRepositoryBrowser categoriesFor: #loadVersion!public! !
!MCRepositoryBrowser categoriesFor: #mergeVersion!public! !
!MCRepositoryBrowser categoriesFor: #onViewOpened!public! !
!MCRepositoryBrowser categoriesFor: #queryCommand:!public! !
!MCRepositoryBrowser categoriesFor: #removeRepository!public! !
!MCRepositoryBrowser categoriesFor: #saveAsTopazFileIn!public! !
!MCRepositoryBrowser categoriesFor: #shellName!public! !
!MCRepositoryBrowser categoriesFor: #updatePackageList!public! !
!MCRepositoryBrowser categoriesFor: #updateRepositoryCreationTemplate!public! !
!MCRepositoryBrowser categoriesFor: #updateRepositoryList!public! !
!MCRepositoryBrowser categoriesFor: #updateVersionInfo!public! !
!MCRepositoryBrowser categoriesFor: #updateVersionList!public! !

!MCRepositoryBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1401 1401 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 4 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1409286144 131073 624 0 482 8 4278190080 0 7 0 0 0 624 655878 ##(Smalltalk.CardLayout)  202 208 98 2 721414 ##(Smalltalk.Association)  8 'Version' 410 8 ##(Smalltalk.ContainerView)  98 15 0 624 98 2 8 1140850688 131073 848 0 0 0 7 0 0 0 848 852230 ##(Smalltalk.FramingLayout)  234 240 98 28 410 8 ##(Smalltalk.TextEdit)  98 16 0 848 98 2 8 1140916352 1025 992 0 482 8 4278190080 0 7 0 0 0 992 0 8 4294904477 852486 ##(Smalltalk.NullConverter)  0 0 1 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 141 1 530 341 39 992 1218 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 992 1218 8 #isTextModified: 98 1 32 992 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 0 0 0 0 240 0 0 0 19 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 141 1554 8 #fixedViewLeft 341 1554 8 #fixedParentTop 1 1554 8 #fixedViewTop 39 410 1008 98 16 0 848 98 2 8 1140916352 1025 1696 0 482 1088 0 7 0 0 0 1696 0 8 4294904477 1122 0 0 1 1154 202 208 98 3 1218 1248 98 2 530 141 123 530 341 39 1696 1218 1328 98 1 1362 3 1 3 1696 1218 1408 98 1 32 1696 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 61 0 0 0 240 0 0 0 80 0 0 0] 98 0 1504 0 27 1522 1568 141 1600 341 1632 123 1664 39 410 8 ##(Smalltalk.ListView)  98 30 0 848 98 2 8 1409355853 1025 2064 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  482 8 4278190080 0 7 0 0 0 2064 0 8 4294904065 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 'Ancestors' 573 8 #left 2290 2320 2336 8 ##(Smalltalk.SortedCollection)  0 0 2064 0 3 0 0 8 #report 2192 0 131169 0 0 1154 202 208 98 2 1218 1248 98 2 530 481 1 530 573 201 2064 1218 8 #text: 98 1 8 'Ancestors' 2064 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 0 0 0 0 0 0 0 14 2 0 0 100 0 0 0] 98 0 1504 0 27 1522 1568 481 1554 8 #fixedParentRight -299 1632 1 1664 201 410 1008 98 16 0 848 98 2 8 1140916352 1025 2800 0 482 1088 0 7 0 0 0 2800 0 8 4294904477 1122 0 0 1 1154 202 208 98 3 1218 1248 98 2 530 141 83 530 341 39 2800 1218 1328 98 1 1362 3 1 3 2800 1218 1408 98 1 32 2800 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 41 0 0 0 240 0 0 0 60 0 0 0] 98 0 1504 0 27 1522 1568 141 1600 341 1632 83 1664 39 410 8 ##(Smalltalk.StaticText)  98 16 0 848 98 2 8 1140850944 1 3168 0 0 0 7 0 0 0 3168 0 8 4294903569 1122 0 0 0 1154 202 208 98 2 1218 1248 98 2 530 1 1 530 141 39 3168 1218 2656 98 1 8 'Name:' 3168 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 70 0 0 0 19 0 0 0] 98 0 1504 0 27 1522 1568 1 1600 141 1632 1 1664 39 410 3184 98 16 0 848 98 2 8 1140850944 1 3504 0 0 0 7 0 0 0 3504 0 8 4294903569 1122 0 0 0 1154 202 208 98 2 1218 1248 98 2 530 1 201 530 141 39 3504 1218 2656 98 1 8 'Message:' 3504 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 100 0 0 0 70 0 0 0 119 0 0 0] 98 0 1504 0 27 1522 1568 1 1600 141 1632 201 1664 39 410 3184 98 16 0 848 98 2 8 1140850944 1 3824 0 0 0 7 0 0 0 3824 0 8 4294903569 1122 0 0 0 1154 202 208 98 2 1218 1248 98 2 530 1 121 530 141 39 3824 1218 2656 98 1 8 'Author:' 3824 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 60 0 0 0 70 0 0 0 79 0 0 0] 98 0 1504 0 27 1522 1568 1 1600 141 1632 121 1664 39 410 3184 98 16 0 848 98 2 8 1140850944 1 4144 0 0 0 7 0 0 0 4144 0 8 4294903569 1122 0 0 0 1154 202 208 98 2 1218 1248 98 2 530 1 161 530 141 39 4144 1218 2656 98 1 8 'ID:' 4144 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 80 0 0 0 70 0 0 0 99 0 0 0] 98 0 1504 0 27 1522 1568 1 1600 141 1632 161 1664 39 410 3184 98 16 0 848 98 2 8 1140850944 1 4464 0 0 0 7 0 0 0 4464 0 8 4294903569 1122 0 0 0 1154 202 208 98 2 1218 1248 98 2 530 1 81 530 141 39 4464 1218 2656 98 1 8 'Time:' 4464 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 40 0 0 0 70 0 0 0 59 0 0 0] 98 0 1504 0 27 1522 1568 1 1600 141 1632 81 1664 39 410 3184 98 16 0 848 98 2 8 1140850944 1 4784 0 0 0 7 0 0 0 4784 0 8 4294903569 1122 0 0 0 1154 202 208 98 2 1218 1248 98 2 530 1 41 530 141 39 4784 1218 2656 98 1 8 'Date:' 4784 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 70 0 0 0 39 0 0 0] 98 0 1504 0 27 1522 1568 1 1600 141 1632 41 1664 39 410 8 ##(Smalltalk.RichTextEdit)  98 18 0 848 98 2 8 1140920644 1025 5104 0 482 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 10 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #chooseSelectionFont 8 '&Font...' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 5266 1 5298 8 #bePlain 8 '&Plain' 1 1 0 0 0 5266 1 5298 8 #toggleBold 8 '&Bold' 1 1 0 0 0 5266 1 5298 8 #toggleItalic 8 '&Italic' 1 1 0 0 0 5266 1 5298 8 #toggleUnderlined 8 '&Underlined' 1 1 0 0 0 5362 4097 5218 0 16 98 3 5266 1025 5298 8 #alignParagraphLeft 8 '&Left' 1 1 0 0 0 5266 1025 5298 8 #alignParagraphCenter 8 '&Centre' 1 1 0 0 0 5266 1025 5298 8 #alignParagraphRight 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 5362 4097 5266 1 5298 8 #chooseSelectionColor 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 5104 0 8 1772716531 1122 0 0 11 0 655622 ##(Smalltalk.EDITSTREAM)  8 #[0 0 0 0 0 0 0 0 48 0 174 1] 1154 202 208 98 6 1218 1248 98 2 530 141 201 530 1213 469 5104 1218 8 #contextMenu: 98 1 5232 5104 1218 2656 98 1 524550 ##(Smalltalk.RichText)  8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\froman Times New Roman;}}
\viewkind4\uc1\pard\f0\fs22 
\par }
' 5104 1218 1328 98 1 1362 3 1 3 5104 1218 1408 98 1 32 5104 1218 8 #resetCharFormat 2192 5104 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 100 0 0 0 164 2 0 0 78 1 0 0] 98 0 1504 0 27 1522 1568 141 2768 1 1632 201 1554 8 #fixedParentBottom 1 410 1008 98 16 0 848 98 2 8 1140916352 1025 6528 0 482 1088 0 7 0 0 0 6528 0 8 4294904477 1122 0 0 1 1154 202 208 98 3 1218 1248 98 2 530 141 43 530 341 39 6528 1218 1328 98 1 1362 3 1 3 6528 1218 1408 98 1 32 6528 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 21 0 0 0 240 0 0 0 40 0 0 0] 98 0 1504 0 27 1522 1568 141 1600 341 1632 43 1664 39 410 1008 98 16 0 848 98 2 8 1140916352 1025 6896 0 482 1088 0 7 0 0 0 6896 0 8 4294904477 1122 0 0 1 1154 202 208 98 3 1218 1248 98 2 530 141 163 530 341 39 6896 1218 1328 98 1 1362 3 1 3 6896 1218 1408 98 1 32 6896 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 81 0 0 0 240 0 0 0 100 0 0 0] 98 0 1504 0 27 1522 1568 141 1600 341 1632 163 1664 39 410 2080 98 30 0 848 98 2 8 1409355853 1025 7264 2146 202 208 2192 0 2224 482 2256 0 7 0 0 0 7264 0 8 4294904065 2290 2320 98 0 0 2368 0 0 0 0 0 0 202 208 98 1 2418 8 'Step-Children' 301 2464 2290 2320 7408 2496 0 0 7264 0 3 0 0 2512 2192 0 131169 0 0 1154 202 208 98 2 1218 1248 98 2 530 1053 1 530 301 201 7264 1218 2656 98 1 8 'Step-Children' 7264 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 2 0 0 0 0 0 0 164 2 0 0 100 0 0 0] 98 0 1504 0 27 1522 2768 -299 1600 301 1632 1 1664 201 234 256 98 16 5104 8 'versionMessage' 7264 8 'versionStepChildren' 6528 8 'versionDate' 6896 8 'versionID' 1696 8 'versionAuthor' 992 8 'versionName' 2800 8 'versionTime' 2064 8 'versionAncestors' 0 1154 202 208 98 1 1218 1248 98 2 530 9 49 530 1353 669 848 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 168 2 0 0 102 1 0 0] 98 14 3168 4784 4464 3824 4144 3504 992 6528 2800 1696 6896 2064 7264 5104 1504 0 27 802 8 'Repository Creation Template' 410 5120 98 18 0 624 98 2 8 1140920644 1025 8080 0 482 5200 0 5 5218 0 16 98 10 5266 1 5298 5328 8 '&Font...' 1 1 0 0 0 5362 4097 5266 1 5298 5424 8 '&Plain' 1 1 0 0 0 5266 1 5298 5488 8 '&Bold' 1 1 0 0 0 5266 1 5298 5552 8 '&Italic' 1 1 0 0 0 5266 1 5298 5616 8 '&Underlined' 1 1 0 0 0 5362 4097 5218 0 16 98 3 5266 1025 5298 5728 8 '&Left' 1 1 0 0 0 5266 1025 5298 5792 8 '&Centre' 1 1 0 0 0 5266 1025 5298 5856 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 5362 4097 5266 1 5298 5952 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 8080 0 8 1772716531 1122 0 0 11 0 6034 8 #[0 0 0 0 0 0 0 0 48 0 174 1] 1154 202 208 98 6 1218 1248 98 2 530 9 49 530 1353 669 8080 1218 6208 98 1 8160 8080 1218 2656 98 1 6274 8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\froman Times New Roman;}}
\viewkind4\uc1\pard\f0\fs22 
\par }
' 8080 1218 1328 98 1 1362 3 1 3 8080 1218 1408 98 1 32 8080 1218 6416 2192 8080 1442 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 168 2 0 0 102 1 0 0] 98 0 1504 0 27 848 234 256 98 4 8080 8 'repositoryCreationTemplate' 848 8 'versionInfo' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 624 98 2 8 1140916736 1 9216 2146 202 208 98 2 8064 832 0 2224 0 0 1 0 0 0 9216 0 8 4294904395 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 2320 9376 7 257 0 9362 0 0 9394 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 9472 7 257 0 2368 0 0 0 0 0 8 #noIcons 0 0 0 0 0 1154 202 208 98 3 1218 1248 98 2 530 1 1 530 1369 725 9216 1218 8 #basicSelectionsByIndex: 98 1 98 1 5 9216 1218 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 9216 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 106 1 0 0] 98 0 1504 0 27 1154 202 208 98 1 1218 1248 98 2 530 1 561 530 1369 725 624 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 24 1 0 0 172 2 0 0 130 2 0 0] 98 3 8080 848 9216 1504 0 27 9 410 864 98 15 0 416 98 2 8 1140850688 131073 10016 0 0 0 7 0 0 0 10016 562 234 240 98 6 410 2080 98 30 0 10016 98 2 8 1409355853 1025 10128 2146 202 208 2192 0 2224 482 8 4278190080 0 7 5218 0 16 98 2 5266 1 5298 8 #addRepository 8 '&Add Repository...' 1 1 0 0 0 5266 1 5298 8 #removeRepository 8 '&Remove Repository...' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 10128 0 8 4294904065 2290 2320 98 0 0 2368 0 0 0 0 0 0 202 208 98 1 2418 8 'Repositories' 1001 2464 2290 2320 10464 2496 0 0 10128 0 1 0 0 2512 2192 0 131169 0 0 1154 202 208 98 3 1218 1248 98 2 530 1 1 530 621 543 10128 1218 6208 98 1 10256 10128 1218 2656 98 1 8 'Repositories' 10128 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 54 1 0 0 15 1 0 0] 98 0 1504 0 27 15 410 2080 98 30 0 10016 98 2 8 1409355849 1025 10800 2146 202 208 2192 0 2224 482 8 4278190080 0 7 5218 0 16 98 4 5266 1 5298 8 #loadVersion 8 '&Load Version' 1 1 0 0 0 5266 1 5298 8 #mergeVersion 8 '&Merge Version' 1 1 0 0 0 5266 1 5298 8 #compareVersion 8 '&Compare' 1 1 0 0 0 5266 1 5298 8 #saveAsTopazFileIn 8 'Save as &Topaz File-In' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 10800 0 8 4294904065 2290 2320 98 0 0 2368 0 0 0 0 0 0 202 208 98 1 2418 8 'Versions' 401 2464 2290 2320 11264 2496 0 0 10800 0 1 0 9362 0 0 1180966 ##(Smalltalk.CompiledExpression)  8 1 8 ##(Smalltalk.UndefinedObject)  8 'doIt' 8 '[:each | each item isLoaded ifTrue: [each font: each font beBold. each item isModified ifTrue: [each font: each font beItalic]]].' 8 #[36 105 226 0 159 221 18 17 226 2 161 180 97 226 0 163 123 17 226 2 164 180 106 60 106 60 106] 8 #item 8 #isLoaded 8 #font 8 #beBold 8 #font: 8 #isModified 8 #beItalic 11360 7 257 0 2512 2192 0 131169 0 0 1154 202 208 98 3 1218 1248 98 2 530 1101 1 530 269 543 10800 1218 6208 98 1 10928 10800 1218 2656 98 1 8 'Versions' 10800 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 2 0 0 0 0 0 0 172 2 0 0 15 1 0 0] 98 0 1504 0 27 7 410 2080 98 30 0 10016 98 2 8 1409355853 1025 11824 2146 202 208 2192 0 2224 482 10240 0 7 5218 0 16 98 2 5266 1 5298 8 #addPackage 8 '&Add Package...' 1 1 0 0 0 5266 1 5298 8 #removePackage 8 '&Remove Package' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 11824 0 8 4294904065 2290 2320 98 0 0 2368 0 0 0 0 0 0 202 208 98 1 2418 8 'Packages' 601 2464 9362 0 0 11378 2 1 9360 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] 8 #name 12224 7 257 0 2496 0 0 11824 0 1 0 9362 0 0 11378 8 1 9360 8 'doIt' 8 '[:each | each item isLoaded ifTrue: [each font: each font beBold. each item isModified ifTrue: [each font: each font beItalic]]].' 8 #[36 105 226 0 159 221 18 17 226 2 161 180 97 226 0 163 123 17 226 2 164 180 106 60 106 60 106] 11472 11488 11504 11520 11536 11552 11568 12320 7 257 0 2512 2192 0 131169 0 0 1154 202 208 98 3 1218 1248 98 2 530 639 1 530 445 543 11824 1218 6208 98 1 11936 11824 1218 2656 98 1 8 'Packages' 11824 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 63 1 0 0 0 0 0 0 29 2 0 0 15 1 0 0] 98 0 1504 0 27 11 32 234 256 98 6 10128 8 'repositoryList' 10800 8 'versionList' 11824 8 'packageList' 0 1154 202 208 98 1 1218 1248 98 2 530 1 1 530 1369 543 10016 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 15 1 0 0] 98 5 10128 410 8 ##(Smalltalk.Splitter)  98 12 0 10016 98 2 8 1140850688 1 12880 0 482 8 4278190080 0 519 0 0 0 12880 1154 202 208 98 1 1218 1248 98 2 530 621 1 530 19 543 12880 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 54 1 0 0 0 0 0 0 63 1 0 0 15 1 0 0] 98 0 1504 0 27 11824 410 12896 98 12 0 10016 98 2 8 1140850688 1 13152 0 482 12976 0 519 0 0 0 13152 1154 202 208 98 1 1218 1248 98 2 530 1083 1 530 19 543 13152 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 29 2 0 0 0 0 0 0 38 2 0 0 15 1 0 0] 98 0 1504 0 27 10800 1504 0 27 7 16 234 256 2192 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 3 5218 0 16 98 2 5266 1 5298 10320 8 '&Add...' 1 1 0 0 0 5266 1 5298 10384 8 '&Remove...' 1 1 0 0 0 8 '&Repository' 0 134217729 0 0 41903 0 0 5218 0 16 98 2 5266 1 5298 12000 8 '&Add...' 1 1 0 0 0 5266 1 5298 12064 8 'Remove...' 1 1 0 0 0 8 '&Package' 0 134217729 0 0 41909 0 0 5218 0 16 98 1 5266 1 5298 10992 8 '&Load Version' 1 1 0 0 0 8 '&Version' 0 134217729 0 0 41913 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1154 202 208 98 3 1218 1248 98 2 530 3359 21 530 1401 1401 416 1218 2656 98 1 8 'Monticello Repository Browser' 416 1218 8 #updateMenuBar 2192 416 1442 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 143 6 0 0 10 0 0 0 75 9 0 0 198 2 0 0] 98 3 10016 410 12896 98 12 0 416 98 2 8 1140850688 1 14096 0 482 12976 0 519 0 0 0 14096 1154 202 208 98 1 1218 1248 98 2 530 1 543 530 1369 19 14096 1442 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 15 1 0 0 172 2 0 0 24 1 0 0] 98 0 1504 0 27 624 1504 0 27 )! !
!MCRepositoryBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

