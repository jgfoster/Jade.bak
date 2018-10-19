"Filed out from Dolphin Smalltalk 7"!

RowanService subclass: #RowanProjectService
	instanceVariableNames: 'sha branch isSkew packages changes existsOnDisk'
	classVariableNames: 'DefaultProjectName'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanProjectService guid: (GUID fromString: '{d65732a3-f4fb-489b-ae7e-d104cddbf3d8}')!
RowanProjectService comment: ''!
!RowanProjectService categoriesForClass!Unclassified! !
!RowanProjectService methodsFor!

= projectService

	^name = projectService name!

addPackageNamed: newPackageName using: presenter
	self
		command: #addPackageNamed:;
		commandArgs: (Array with: newPackageName).
	presenter issueCommand: (Array with: self).
	presenter refreshFromServer!

basicName: aString
	"don't round trip to the server to get information" 
	name := aString!

branch
	^branch!

branch: anObject
	branch := anObject!

changesUsing: presenter
	| operations session |
	command := #changes.
	presenter issueCommand: (Array with: self).
	changes isEmpty ifTrue: [^MessageBox notify: 'No changes!!'].
	session := presenter gciSession.
	operations := OrderedCollection new.
	changes
		do: [:changeString | operations addAll: (RowanPatch fromString: changeString session: session) operations].
	RowanPatchBrowser showOn: (RowanPatch new operations: operations; session: session) name: name!

checkoutUsing: presenter
	| branchString query branchName |
	query := RowanQueryService new
				command: #projectBranches:;
				commandArgs: (Array with: self name).
	branchString := (presenter issueCommand: (Array with: query)) first answer.
	branchString := branchString subStrings asOrderedCollection
				remove: '*';
				yourself.
	branchName := ChoicePrompter
				on: branch
				choices: branchString
				caption: 'Select branch to checkout:'.
	branchName isNil ifTrue: [^self].
	self
		command: #checkout:;
		commandArgs: (Array with: branchName).
	presenter issueCommand: (Array with: self)!

defaultProjectName
	^self class defaultProjectName!

displayName
	"display with dirty indication * and/or default indication (default)"

	| displayName |
	displayName := name.
	existsOnDisk ifFalse:[displayName := '(', displayName, ')']. 
	name = self class defaultProjectName ifTrue: [displayName := displayName , '*'].
	^displayName!

displayStringFor: displayThing
	name ifNil: [^self].
	self isDirty
		ifTrue: 
			[displayThing font
				beItalic;
				beBold.
			displayThing forecolor: Color black].
	self isLoaded
		ifTrue: 
			[self isSkew
				ifTrue: [displayThing forecolor: Color red]
				ifFalse: [displayThing forecolor: Color black]]
		ifFalse: [displayThing forecolor: Color black]!

hash
	^self name hash!

isLoaded

	^sha isEmpty not!

isProjectService

	^true!

isSkew
	^isSkew!

isSkew: anObject
	isSkew := anObject!

loadProjectFrom: presenter
	self command: #reloadProject.
	presenter issueCommand: (Array with: self).!

newTextView
	| edit |
	Shell create view
		layoutManager: BorderLayout new;
		hasMaximize: false;
		hasMinimize: false;
		extent: 800 @ 900;
		caption: 'About';
		addSubView: (edit := RichTextEdit new);
		show.
	edit
		arrangement: #center;
		isReadOnly: true;
		canVScroll: true;
		font: (Font name: 'Arial' pointSize: 10);
		alignment: #center.
	^edit!

projectLogUsing: presenter
	| textEdit projectName queryService |
	projectName := self name.
	queryService := RowanQueryService command: #projectLog: withArgs: (Array with: projectName).
	presenter issueCommand: (Array with: queryService).
	textEdit := self newTextView.
	textEdit topShell caption: (presenter gciSession titleBarFor: projectName , ' Project Log').
	textEdit text: queryService queryResults!

projectPackagesUpdate: presenter browser: browser
	presenter list isEmpty ifTrue: [self initializePresenterList: presenter].
	browser projectListPresenter selections detect: [:projectService | projectService name = name]
		ifNone: [^self].
	self updateList: presenter whilePreservingSelections: packages.
	browser isClassSelected ifFalse: [self emptyFilterListsIn: browser]!

projectSelectionUpdate: presenter
	| newSelections |
	newSelections := presenter list select: [:service | self name = service name].
	presenter selections: newSelections!

projectsUpdate: presenter
	| listProject |
	listProject := presenter list detect: [:listProj | listProj name = name] ifNone: [].
	listProject
		ifNotNil: 
			[:proj |
			proj sha: sha.
			proj isDirty: isDirty.
			proj isSkew: isSkew].
	!

releaseReferencedServices
	| copy |
	packages isNil ifTrue: [^self].
	copy := packages copy.
	packages := Array new.
	copy do: [:service | service releaseReferencedServices]!

remoteServiceName
	^'Rowan projectServiceClass'!

removedProject: presenter
	| removedProject |
	updateType == #removedProject: ifFalse:[^self].
	removedProject := presenter list detect: [:projectService | projectService name = name] ifNone: [].
	removedProject ifNotNil: [presenter model remove: removedProject]!

replicateFrom: newService
	^self isProjectService ifTrue: [super replicateFrom: newService]!

sha
	^sha!

sha: anObject
	sha := anObject! !
!RowanProjectService categoriesFor: #=!comparing!public! !
!RowanProjectService categoriesFor: #addPackageNamed:using:!presenter support!public! !
!RowanProjectService categoriesFor: #basicName:!accessing!public! !
!RowanProjectService categoriesFor: #branch!accessing!public! !
!RowanProjectService categoriesFor: #branch:!accessing!public! !
!RowanProjectService categoriesFor: #changesUsing:!presenter support!public! !
!RowanProjectService categoriesFor: #checkoutUsing:!presenter support!public! !
!RowanProjectService categoriesFor: #defaultProjectName!accessing!public! !
!RowanProjectService categoriesFor: #displayName!accessing!displaying!public! !
!RowanProjectService categoriesFor: #displayStringFor:!displaying!public! !
!RowanProjectService categoriesFor: #hash!comparing!public! !
!RowanProjectService categoriesFor: #isLoaded!public!testing! !
!RowanProjectService categoriesFor: #isProjectService!public!testing! !
!RowanProjectService categoriesFor: #isSkew!accessing!private! !
!RowanProjectService categoriesFor: #isSkew:!accessing!private! !
!RowanProjectService categoriesFor: #loadProjectFrom:!presenter support!public! !
!RowanProjectService categoriesFor: #newTextView!displaying!private! !
!RowanProjectService categoriesFor: #projectLogUsing:!presenter support!public! !
!RowanProjectService categoriesFor: #projectPackagesUpdate:browser:!public!updating! !
!RowanProjectService categoriesFor: #projectSelectionUpdate:!public!updating! !
!RowanProjectService categoriesFor: #projectsUpdate:!public!updating! !
!RowanProjectService categoriesFor: #releaseReferencedServices!Init / Release!public! !
!RowanProjectService categoriesFor: #remoteServiceName!must not strip!public! !
!RowanProjectService categoriesFor: #removedProject:!public!updating! !
!RowanProjectService categoriesFor: #replicateFrom:!public!updating! !
!RowanProjectService categoriesFor: #sha!accessing!public! !
!RowanProjectService categoriesFor: #sha:!accessing!public! !

!RowanProjectService class methodsFor!

defaultIconName
	"Answer a filename to use for an icon of this class."

	^File composeStem: 'Collection' extension: 'ico'.!

defaultProjectName
	"cached from the server class var"

	^DefaultProjectName!

defaultProjectName: aString

	DefaultProjectName := aString!

icon
	"Answers an Icon that can be used to represent this class"

	^##(self) defaultIcon!

initialize
	DefaultProjectName := nil! !
!RowanProjectService class categoriesFor: #defaultIconName!private! !
!RowanProjectService class categoriesFor: #defaultProjectName!public! !
!RowanProjectService class categoriesFor: #defaultProjectName:!public! !
!RowanProjectService class categoriesFor: #icon!private! !
!RowanProjectService class categoriesFor: #initialize!public! !

