"Filed out from Dolphin Smalltalk 7"!

JadeShell subclass: #MCRepositoryBrowser
	instanceVariableNames: 'repositoryListPresenter packageListPresenter versionListPresenter repositoryCreationTemplatePresenter versionNamePresenter versionDatePresenter versionTimePresenter versionAuthorPresenter versionIDPresenter versionAncestorsPresenter versionStepChildrenPresenter versionMessagePresenter loadedVersionNames'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCRepositoryBrowser guid: (GUID fromString: '{ed52d4ac-3c47-4b09-830b-99228086125c}')!
MCRepositoryBrowser comment: ''!
!MCRepositoryBrowser categoriesForClass!Unclassified! !
!MCRepositoryBrowser methodsFor!

addDirectoryRepository

	| path |
	path := BrowseFolderDialog new 
		title: 'Please select a Monticello folder';
		showModal.
	path isNil ifTrue: [^nil].
	^gciSession
		serverPerform: #'mcNewDirectoryRepository:' 
		with: (gciSession encode: path)!

addFileTreeRepository

	| path |
	(path := Prompter prompt: 'Enter server path:') isNil ifTrue: [^nil].
	^gciSession
		serverPerform: #'mcNewFileTreeRepository:' 
		with: (gciSession encode: path)!

addGitHubRepository

	| path |
	(path := Prompter prompt: 'Enter location (e.g., ''github://glassdb/zinc:gemstone3.1/repository''):') isNil ifTrue: [^nil].
	^gciSession
		serverPerform: #'mcNewGitHubRepository:' 
		with: (gciSession encode: path)!

addHttpRepository

	| info string delimiter |
	(info := MCHttpRepositoryInfoDialog showModalOn: MCHttpRepositoryInfo new) isNil ifTrue: [^nil].
	delimiter := (Character codePoint: 255) asString.
	string := info location , delimiter , info user , delimiter , info password.
	^gciSession
		serverPerform: #'mcAddHttpRepository:' 
		with: (gciSession encode: string)!

addRepository

	| repository type |
	type := ChoicePrompter
		choices: #(#'HTTP' #'Client Directory' #'Server Directory' #'Server Directory with FileTree' #'Server Directory with GitHub' ) 
		caption: 'Choose Repository Type'.
	type isNil ifTrue: [^self].
	repository := type = #'HTTP' ifTrue: [self addHttpRepository] ifFalse: [
		type = #'Client Directory' ifTrue: [self addDirectoryRepository] ifFalse: [
		type = #'Server Directory' ifTrue: [self addServerDirectoryRepository] ifFalse: [
		type = #'Server Directory with FileTree' ifTrue: [self addFileTreeRepository] ifFalse: [
		type = #'Server Directory with GitHub' ifTrue: [self addGitHubRepository] ifFalse: [
		self error: 'Unrecognized repository type: ' , type printString]]]]].
	self updateRepositoryListAndSelect: repository.!

addServerDirectoryRepository

	| path |
	(path := Prompter prompt: 'Enter server path:') isNil ifTrue: [^nil].
	^gciSession
		serverPerform: #'mcNewServerDirectoryRepository:' 
		with: (gciSession encode: path)!

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
	repositoryListPresenter	when: #'selectionChanged' send: #'updatePackageList' to: self.
	repositoryListPresenter	when: #'selectionChanged' send: #'updateRepositoryCreationTemplate' to: self.
	packageListPresenter	when: #'selectionChanged' send: #'updateVersionList' to: self.
	versionListPresenter		when: #'selectionChanged' send: #'updateVersionInfo' to: self.
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
	list := string nonEmptySubStrings: Character lf.
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
		with: (gciSession encode: versionFullName)
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
		updateRepositoryListAndSelect: nil;
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
	self updateRepositoryListAndSelect: nil.
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

	packageListPresenter list: #().
	(repositoryListPresenter selectionOrNil) ifNotNil: [:repository | 
		| list |
		list := repository packageList.
		list do: [:each | 
			each loaded: (loadedVersionNames 
				at: each name
				ifAbsent: [nil]).
		].
		packageListPresenter list: list.
	].
	self updateVersionList.!

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

updateRepositoryListAndSelect: anOopType

	| list repository |
	list := MCRepository allIn: gciSession.
	repositoryListPresenter list: list asSortedCollection.
	anOopType ifNotNil: [
		repository := list detect: [:each | each oopType = anOopType] ifNone: [nil].
	].
	repositoryListPresenter selectionOrNil: repository.
	repositoryCreationTemplatePresenter view ensureVisible.
	self updatePackageList.!

updateVersionInfo

	| package versionName version |
	repositoryCreationTemplatePresenter view ensureVisible.
	versionNamePresenter 		value: ''.
	versionDatePresenter 		value: ''.
	versionTimePresenter 		value: ''.
	versionAuthorPresenter 		value: ''.
	versionIDPresenter 			value: ''.
	versionAncestorsPresenter 	list: #().
	versionStepChildrenPresenter 	list: #().
	versionMessagePresenter 		value: ''.
	((repositoryListPresenter selectionOrNil) notNil and: [
		(package := packageListPresenter selectionOrNil) notNil and: [
		(versionName := versionListPresenter selectionOrNil) notNil]]) ifTrue: [
			version := package infoForVersion: versionName name.
		].
	version ifNil: [
		repositoryCreationTemplatePresenter view ensureVisible.
		^self
	].
	versionNamePresenter 		value: 	version name.
	versionDatePresenter 		value: 	version date.
	versionTimePresenter 		value: 	version time.
	versionAuthorPresenter 		value: 	version author.
	versionIDPresenter 			value: 	version id.
	versionAncestorsPresenter 	list: 		version ancestors.
	versionStepChildrenPresenter 	list: 		version stepChildren.
	versionMessagePresenter 		value: 	version message.
	(self view viewNamed: 'versionInfo') ensureVisible.
!

updateVersionList

	versionListPresenter list: #().
	packageListPresenter selectionOrNil ifNotNil: [:package |
		| list |
		list := package versionNames.
		package isLoaded ifTrue: [
			| name |
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
		self updateVersionInfo.
	].
! !
!MCRepositoryBrowser categoriesFor: #addDirectoryRepository!add repository!public! !
!MCRepositoryBrowser categoriesFor: #addFileTreeRepository!add repository!public! !
!MCRepositoryBrowser categoriesFor: #addGitHubRepository!add repository!public! !
!MCRepositoryBrowser categoriesFor: #addHttpRepository!add repository!public! !
!MCRepositoryBrowser categoriesFor: #addRepository!add repository!public! !
!MCRepositoryBrowser categoriesFor: #addServerDirectoryRepository!add repository!public! !
!MCRepositoryBrowser categoriesFor: #createComponents!public! !
!MCRepositoryBrowser categoriesFor: #createSchematicWiring!public! !
!MCRepositoryBrowser categoriesFor: #editRepository!public! !
!MCRepositoryBrowser categoriesFor: #fileTypes!public! !
!MCRepositoryBrowser categoriesFor: #getLoadedVersionNames!public!version actions! !
!MCRepositoryBrowser categoriesFor: #loadVersion!public!version actions! !
!MCRepositoryBrowser categoriesFor: #mergeVersion!public!version actions! !
!MCRepositoryBrowser categoriesFor: #onViewOpened!public! !
!MCRepositoryBrowser categoriesFor: #queryCommand:!public! !
!MCRepositoryBrowser categoriesFor: #removeRepository!public! !
!MCRepositoryBrowser categoriesFor: #saveAsTopazFileIn!public!version actions! !
!MCRepositoryBrowser categoriesFor: #shellName!public! !
!MCRepositoryBrowser categoriesFor: #updatePackageList!public!update view! !
!MCRepositoryBrowser categoriesFor: #updateRepositoryCreationTemplate!public!update view! !
!MCRepositoryBrowser categoriesFor: #updateRepositoryListAndSelect:!public!update view! !
!MCRepositoryBrowser categoriesFor: #updateVersionInfo!public!update view! !
!MCRepositoryBrowser categoriesFor: #updateVersionList!public!update view! !

!MCRepositoryBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13565952 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 1401 1401 551 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 608 0 0 0 7 0 0 0 608 546 170 176 34 6 410 ##(Smalltalk.ListView) 34 30 0 608 34 2 8 1409355853 1025 720 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 466 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu) 0 true 34 2 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #addRepository 8 '&Add Repository...' 1 1 0 0 0 962 1 994 #removeRepository 8 '&Remove Repository...' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 720 0 8 4294903185 459270 ##(Smalltalk.Message) #displayString 8 #() 0 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Repositories' 1001 #left 1122 #displayString 1152 ##(Smalltalk.SortedCollection) 0 0 720 0 1 0 0 #report 832 0 131169 0 8 #(nil) 983302 ##(Smalltalk.MessageSequence) 138 144 34 3 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 629 545 720 1378 #contextMenu: 34 1 928 720 1378 #text: 34 1 8 'Repositories' 720 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 58 1 0 0 16 1 0 0] 8 #() 514 193 193 0 27 15 410 ##(Smalltalk.ListView) 34 30 0 608 34 2 8 1409355849 1025 1616 786 138 144 832 0 864 466 8 4278190080 0 7 914 0 true 34 4 962 1 994 #loadVersion 8 '&Load Version' 1 1 0 0 0 962 1 994 #mergeVersion 8 '&Merge Version' 1 1 0 0 0 962 1 994 #compareVersion 8 '&Compare' 1 1 0 0 0 962 1 994 #saveAsTopazFileIn 8 'Save as &Topaz File-In' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 1616 0 8 4294903185 1122 #displayString 8 #() 0 1184 0 0 0 0 0 0 138 144 34 1 1234 8 'Versions' 401 #left 1122 #displayString 2016 ##(Smalltalk.SortedCollection) 0 0 1616 0 1 0 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 8 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item isLoaded ifTrue: [each font: each font beBold. each item isModified ifTrue: [each font: each font beItalic]]].' 8 #[36 105 226 0 159 221 18 17 226 2 161 180 97 226 0 163 123 17 226 2 164 180 106 60 106 60 106] #item #isLoaded #font #beBold #font: #isModified #beItalic 2128 7 257 0 #report 832 0 131169 0 8 #(nil) 1314 138 144 34 3 1378 #createAt:extent: 34 2 514 1097 1 514 273 545 1616 1378 #contextMenu: 34 1 1744 1616 1378 #text: 34 1 8 'Versions' 1616 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 36 2 0 0 0 0 0 0 172 2 0 0 16 1 0 0] 8 #() 1600 0 27 7 410 ##(Smalltalk.ListView) 34 30 0 608 34 2 8 1409355853 1025 2480 786 138 144 832 0 864 466 896 0 7 914 0 true 34 2 962 1 994 #addPackage 8 '&Add Package...' 1 1 0 0 0 962 1 994 #removePackage 8 '&Remove Package' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 2480 0 8 4294903185 1122 #displayString 8 #() 0 1184 0 0 0 0 0 0 138 144 34 1 1234 8 'Packages' 601 #left 2114 0 0 2146 2 1 2112 8 'doIt' 8 '[:each | each name]' 8 #[30 105 226 0 106] #name 2848 7 257 0 ##(Smalltalk.SortedCollection) 0 0 2480 0 1 0 2114 0 0 2146 8 1 2112 8 'doIt' 8 '[:each | each item isLoaded ifTrue: [each font: each font beBold. each item isModified ifTrue: [each font: each font beItalic]]].' 8 #[36 105 226 0 159 221 18 17 226 2 161 180 97 226 0 163 123 17 226 2 164 180 106 60 106 60 106] #item #isLoaded #font #beBold #font: #isModified #beItalic 2928 7 257 0 #report 832 0 131169 0 8 #(nil) 1314 138 144 34 3 1378 #createAt:extent: 34 2 514 639 1 514 449 545 2480 1378 #contextMenu: 34 1 2592 2480 1378 #text: 34 1 8 'Packages' 2480 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 63 1 0 0 0 0 0 0 31 2 0 0 16 1 0 0] 8 #() 1600 0 27 11 false 170 192 34 6 720 8 'repositoryList' 1616 8 'versionList' 2480 8 'packageList' 0 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 1 1 514 1369 545 608 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 16 1 0 0] 34 5 720 410 ##(Smalltalk.Splitter) 34 12 0 608 34 2 8 1140850688 1 3504 0 466 8 4278190080 0 519 0 0 0 3504 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 629 1 514 11 545 3504 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 58 1 0 0 0 0 0 0 63 1 0 0 16 1 0 0] 8 #() 1600 0 27 2480 410 ##(Smalltalk.Splitter) 34 12 0 608 34 2 8 1140850688 1 3760 0 466 3584 0 519 0 0 0 3760 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 1087 1 514 11 545 3760 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 31 2 0 0 0 0 0 0 36 2 0 0 16 1 0 0] 8 #() 1600 0 27 1616 1600 0 27 7 410 ##(Smalltalk.CardContainer) 34 16 0 416 34 2 8 1409286144 131073 4000 0 466 8 4278190080 0 7 0 0 0 4000 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 8 'Version' 410 ##(Smalltalk.ContainerView) 34 15 0 4000 34 2 8 1140850688 131073 4208 0 0 0 7 0 0 0 4208 852230 ##(Smalltalk.FramingLayout) 170 176 34 28 410 ##(Smalltalk.TextEdit) 34 16 0 4208 34 2 8 1140916352 1025 4336 0 466 8 4278190080 0 7 0 0 0 4336 0 8 4294903565 852486 ##(Smalltalk.NullConverter) 0 0 1 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 141 1 514 341 39 4336 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 0 0 0 0 240 0 0 0 19 0 0 0] 8 #() 1600 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 141 4674 #fixedViewLeft 341 4674 #fixedParentTop 1 4674 #fixedViewTop 39 410 ##(Smalltalk.StaticText) 34 16 0 4208 34 2 8 1140850944 1 4752 0 0 0 7 0 0 0 4752 0 8 4294903545 4450 0 0 0 1314 138 144 34 2 1378 #createAt:extent: 34 2 514 1 1 514 141 39 4752 1378 #text: 34 1 8 'Name:' 4752 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 70 0 0 0 19 0 0 0] 8 #() 1600 0 27 4642 4688 1 4704 141 4720 1 4736 39 410 ##(Smalltalk.TextEdit) 34 16 0 4208 34 2 8 1140916352 1025 5072 0 466 4416 0 7 0 0 0 5072 0 8 4294903565 4450 0 0 1 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 141 83 514 341 39 5072 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 41 0 0 0 240 0 0 0 60 0 0 0] 8 #() 1600 0 27 4642 4688 141 4704 341 4720 83 4736 39 410 ##(Smalltalk.RichTextEdit) 34 18 0 4208 34 2 8 1140920644 1025 5360 0 466 8 4278190080 0 7 914 0 true 34 10 962 1 994 #chooseSelectionFont 8 '&Font...' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 962 1 994 #bePlain 8 '&Plain' 1 1 0 0 0 962 1 994 #toggleBold 8 '&Bold' 1 1 0 0 0 962 1 994 #toggleItalic 8 '&Italic' 1 1 0 0 0 962 1 994 #toggleUnderlined 8 '&Underlined' 1 1 0 0 0 5538 4097 914 0 true 34 3 962 1025 994 #alignParagraphLeft 8 '&Left' 1 1 0 0 0 962 1025 994 #alignParagraphCenter 8 '&Centre' 1 1 0 0 0 962 1025 994 #alignParagraphRight 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 5538 4097 962 1 994 #chooseSelectionColor 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 5360 0 8 1620804432 4450 0 0 11 0 655622 ##(Smalltalk.EDITSTREAM) 8 #[0 0 0 0 0 0 0 0 48 0 203 2] 1314 138 144 34 4 1378 #createAt:extent: 34 2 514 141 201 514 1213 467 5360 1378 #contextMenu: 34 1 5456 5360 1378 #text: 34 1 524550 ##(Smalltalk.RichText) 8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\froman Times New Roman;}}
\viewkind4\uc1\pard\f0\fs22 
\par }
' 5360 1378 #resetCharFormat 832 5360 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 100 0 0 0 164 2 0 0 77 1 0 0] 8 #() 1600 0 27 4642 4688 141 4674 #fixedParentRight 1 4720 201 4674 #fixedParentBottom 1 410 ##(Smalltalk.TextEdit) 34 16 0 4208 34 2 8 1140916352 1025 6464 0 466 4416 0 7 0 0 0 6464 0 8 4294903565 4450 0 0 1 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 141 43 514 341 39 6464 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 21 0 0 0 240 0 0 0 40 0 0 0] 8 #() 1600 0 27 4642 4688 141 4704 341 4720 43 4736 39 410 ##(Smalltalk.StaticText) 34 16 0 4208 34 2 8 1140850944 1 6752 0 0 0 7 0 0 0 6752 0 8 4294903545 4450 0 0 0 1314 138 144 34 2 1378 #createAt:extent: 34 2 514 1 161 514 141 39 6752 1378 #text: 34 1 8 'ID:' 6752 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 80 0 0 0 70 0 0 0 99 0 0 0] 8 #() 1600 0 27 4642 4688 1 4704 141 4720 161 4736 39 410 ##(Smalltalk.ListView) 34 30 0 4208 34 2 8 1409355853 1025 7072 786 138 144 832 0 864 466 8 4278190080 0 7 0 0 0 7072 0 8 4294903185 1122 #displayString 8 #() 0 1184 0 0 0 0 0 0 138 144 34 1 1234 8 'Step-Children' 301 #left 1122 #displayString 7232 ##(Smalltalk.SortedCollection) 0 0 7072 0 3 0 0 #report 832 0 131169 0 8 #(nil) 1314 138 144 34 2 1378 #createAt:extent: 34 2 514 1053 1 514 301 201 7072 1378 #text: 34 1 8 'Step-Children' 7072 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 2 0 0 0 0 0 0 164 2 0 0 100 0 0 0] 8 #() 1600 0 27 4642 6432 -299 4704 301 4720 1 4736 201 410 ##(Smalltalk.TextEdit) 34 16 0 4208 34 2 8 1140916352 1025 7568 0 466 4416 0 7 0 0 0 7568 0 8 4294903565 4450 0 0 1 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 141 163 514 341 39 7568 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 81 0 0 0 240 0 0 0 100 0 0 0] 8 #() 1600 0 27 4642 4688 141 4704 341 4720 163 4736 39 410 ##(Smalltalk.StaticText) 34 16 0 4208 34 2 8 1140850944 1 7856 0 0 0 7 0 0 0 7856 0 8 4294903545 4450 0 0 0 1314 138 144 34 2 1378 #createAt:extent: 34 2 514 1 201 514 141 39 7856 1378 #text: 34 1 8 'Message:' 7856 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 100 0 0 0 70 0 0 0 119 0 0 0] 8 #() 1600 0 27 4642 4688 1 4704 141 4720 201 4736 39 410 ##(Smalltalk.StaticText) 34 16 0 4208 34 2 8 1140850944 1 8176 0 0 0 7 0 0 0 8176 0 8 4294903545 4450 0 0 0 1314 138 144 34 2 1378 #createAt:extent: 34 2 514 1 81 514 141 39 8176 1378 #text: 34 1 8 'Time:' 8176 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 40 0 0 0 70 0 0 0 59 0 0 0] 8 #() 1600 0 27 4642 4688 1 4704 141 4720 81 4736 39 410 ##(Smalltalk.TextEdit) 34 16 0 4208 34 2 8 1140916352 1025 8496 0 466 4416 0 7 0 0 0 8496 0 8 4294903565 4450 0 0 1 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 141 123 514 341 39 8496 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 61 0 0 0 240 0 0 0 80 0 0 0] 8 #() 1600 0 27 4642 4688 141 4704 341 4720 123 4736 39 410 ##(Smalltalk.StaticText) 34 16 0 4208 34 2 8 1140850944 1 8784 0 0 0 7 0 0 0 8784 0 8 4294903545 4450 0 0 0 1314 138 144 34 2 1378 #createAt:extent: 34 2 514 1 121 514 141 39 8784 1378 #text: 34 1 8 'Author:' 8784 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 60 0 0 0 70 0 0 0 79 0 0 0] 8 #() 1600 0 27 4642 4688 1 4704 141 4720 121 4736 39 410 ##(Smalltalk.StaticText) 34 16 0 4208 34 2 8 1140850944 1 9104 0 0 0 7 0 0 0 9104 0 8 4294903545 4450 0 0 0 1314 138 144 34 2 1378 #createAt:extent: 34 2 514 1 41 514 141 39 9104 1378 #text: 34 1 8 'Date:' 9104 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 70 0 0 0 39 0 0 0] 8 #() 1600 0 27 4642 4688 1 4704 141 4720 41 4736 39 410 ##(Smalltalk.ListView) 34 30 0 4208 34 2 8 1409355853 1025 9424 786 138 144 832 0 864 466 7184 0 7 0 0 0 9424 0 8 4294903185 1122 #displayString 8 #() 0 1184 0 0 0 0 0 0 138 144 34 1 1234 8 'Ancestors' 573 #left 1122 #displayString 9568 ##(Smalltalk.SortedCollection) 0 0 9424 0 3 0 0 #report 832 0 131169 0 8 #(nil) 1314 138 144 34 2 1378 #createAt:extent: 34 2 514 481 1 514 573 201 9424 1378 #text: 34 1 8 'Ancestors' 9424 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 0 0 0 0 0 0 0 14 2 0 0 100 0 0 0] 8 #() 1600 0 27 4642 4688 481 6432 -299 4720 1 4736 201 170 192 34 16 4336 8 'versionName' 9424 8 'versionAncestors' 6464 8 'versionDate' 7568 8 'versionID' 7072 8 'versionStepChildren' 5360 8 'versionMessage' 8496 8 'versionAuthor' 5072 8 'versionTime' 0 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 9 55 514 1353 667 4208 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 168 2 0 0 104 1 0 0] 34 14 4752 9104 8176 8784 6752 7856 4336 6464 5072 8496 7568 9424 7072 5360 1600 0 27 4162 8 'Repository Creation Template' 410 ##(Smalltalk.RichTextEdit) 34 18 0 4000 34 2 8 1140920644 1025 10256 0 466 5440 0 5 914 0 true 34 10 962 1 994 #chooseSelectionFont 8 '&Font...' 1 1 0 0 0 5538 4097 962 1 994 #bePlain 8 '&Plain' 1 1 0 0 0 962 1 994 #toggleBold 8 '&Bold' 1 1 0 0 0 962 1 994 #toggleItalic 8 '&Italic' 1 1 0 0 0 962 1 994 #toggleUnderlined 8 '&Underlined' 1 1 0 0 0 5538 4097 914 0 true 34 3 962 1025 994 #alignParagraphLeft 8 '&Left' 1 1 0 0 0 962 1025 994 #alignParagraphCenter 8 '&Centre' 1 1 0 0 0 962 1025 994 #alignParagraphRight 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 5538 4097 962 1 994 #chooseSelectionColor 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 10256 0 8 1620804432 4450 0 0 11 0 6082 8 #[0 0 0 0 0 0 0 0 48 0 203 2] 1314 138 144 34 4 1378 #createAt:extent: 34 2 514 9 55 514 1353 667 10256 1378 #contextMenu: 34 1 10336 10256 1378 #text: 34 1 6306 8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\froman Times New Roman;}}
\viewkind4\uc1\pard\f0\fs22 
\par }
' 10256 1378 #resetCharFormat 832 10256 1538 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 168 2 0 0 104 1 0 0] 8 #() 1600 0 27 4208 170 192 34 4 10256 8 'repositoryCreationTemplate' 4208 8 'versionInfo' 0 410 ##(Smalltalk.TabViewXP) 34 28 0 4000 34 2 8 1140916736 1 11312 786 138 144 34 2 10240 4192 0 864 0 0 1 0 0 0 11312 0 8 4294903547 2114 0 0 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 11440 7 257 0 2114 0 0 11458 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 11504 7 257 0 1184 0 0 0 0 0 #noIcons 0 0 0 0 0 1314 138 144 34 3 1378 #createAt:extent: 34 2 514 1 1 514 1369 729 11312 1378 #basicSelectionsByIndex: 34 1 8 #(2) 11312 1378 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 11312 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 2 0 0 108 1 0 0] 8 #() 1600 0 27 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 1 555 514 1369 729 4000 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 21 1 0 0 172 2 0 0 129 2 0 0] 34 3 10256 4208 11312 1600 0 27 9 true 170 192 832 0 461638 4 ##(Smalltalk.MenuBar) 0 true 34 3 914 0 true 34 2 962 1 994 #addRepository 8 '&Add...' 1 1 0 0 0 962 1 994 #removeRepository 8 '&Remove...' 1 1 0 0 0 8 '&Repository' 0 134217729 0 0 37961 0 0 914 0 true 34 2 962 1 994 #addPackage 8 '&Add...' 1 1 0 0 0 962 1 994 #removePackage 8 'Remove...' 1 1 0 0 0 8 '&Package' 0 134217729 0 0 37967 0 0 914 0 true 34 1 962 1 994 #loadVersion 8 '&Load Version' 1 1 0 0 0 8 '&Version' 0 134217729 0 0 37971 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1314 138 144 34 3 1378 #createAt:extent: 34 2 514 2559 21 514 1401 1401 416 1378 #text: 34 1 8 'Monticello Repository Browser' 416 1378 #updateMenuBar 832 416 1538 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 187 7 0 0 198 2 0 0] 34 3 608 410 ##(Smalltalk.Splitter) 34 12 0 416 34 2 8 1140850688 1 12640 0 466 3584 0 519 0 0 0 12640 1314 138 144 34 1 1378 #createAt:extent: 34 2 514 1 545 514 1369 11 12640 1538 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 16 1 0 0 172 2 0 0 21 1 0 0] 8 #() 1600 0 27 4000 1600 0 27 )! !
!MCRepositoryBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

