"Filed out from Dolphin Smalltalk 7"!

Dialog subclass: #MCVersionDialog
	instanceVariableNames: 'namePresenter messagePresenter repositoryListPresenter httpUserPresenter httpPasswordPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCVersionDialog guid: (GUID fromString: '{781FA501-11CB-40EB-B958-CBA790F96317}')!
MCVersionDialog comment: ''!
!MCVersionDialog categoriesForClass!Unclassified! !
!MCVersionDialog methodsFor!

apply

	self subject
		at: #'httpPassword'		put: httpPasswordPresenter model value;
		at: #'httpUser'			put: httpUserPresenter model value;
		at: #'repository'			put: repositoryListPresenter selection;
		at: #'versionMessage' 	put: messagePresenter model value;
		at: #'versionName'		put: namePresenter model value;
		yourself.
	super apply.
!

createComponents

	super createComponents.
	namePresenter 				:= self add: TextPresenter 	new name: 'name'.
	messagePresenter 		:= self add: TextPresenter 	new name: 'message'.
	repositoryListPresenter 	:= self add: ListPresenter 		new name: 'repositoryList'.
	httpUserPresenter 		:= self add: TextPresenter 	new name: 'httpUser'.
	httpPasswordPresenter 	:= self add: TextPresenter 	new name: 'httpPassword'.
!

createSchematicWiring

	super createSchematicWiring.
	repositoryListPresenter when: #'selectionChanged' send: #'repositorySelectionChanged' to: self.
!

defaultRepository: aRepository

	repositoryListPresenter selection: aRepository.
!

httpPassword

	^httpPasswordPresenter value.
!

httpUser

	^httpUserPresenter value.
!

onViewOpened

	| dict |
	super onViewOpened.
	dict := model value.
	self caption: 'Save ' , (dict at: #'name').
	(dict at: #'isModified') ifTrue: [
		namePresenter value: (dict at: #'nextVersionName').
	] ifFalse: [
		namePresenter value: (dict at: #'priorVersionName').
		messagePresenter value: (dict at: #'versionMessage').
	].
	repositoryListPresenter 
		list: (dict at: #'repositoryList');
		selection: (dict at: #'repository');
		yourself.
!

repositoryList

	^self model repositories.
!

repositorySelectionChanged

	| array |
	array := repositoryListPresenter selection.
	array first = 'MCHttpRepository' ifTrue: [
		self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'httpTab').
		httpPasswordPresenter value: (array at: 4).
		httpUserPresenter value: (array at: 3).
		^self.
	].
	array first = 'MCDictionaryRepository' ifTrue: [
		self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'dictionaryTab').
		^self.
	].
	array first = 'MCDirectoryRepository' ifTrue: [
		self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'directoryTab').
		^self.
	].
	(#('MCServerDirectoryRepository' 'MCFileTreeRepository' 'MCGitHubRepository') includes: array first) ifTrue: [
		self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'directoryTab').
		^self.
	].
	"self halt."
	self error: 'Repository selection changed to unknown type: ' , array first printString.
!

repositoryTypeContainer

	^self view viewNamed: 'repositoryType'.
!

uniqueVersionName

	| string |
	string := self model uniqueVersionName.
	0 = (string indexOfSubCollection: 'seaside') ifTrue: [^string].
	(string := Prompter prompt: 'Author initials?') isNil ifTrue: [
		[self cancel] forkAt: Processor userBackgroundPriority.
		^nil.
	].
	self model authorInitials: string.
	^self model uniqueVersionName.
!

updateDictionaryTabWith: aRepository

	self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'dictionaryTab').
!

updateDirectoryTabWith: aRepository

	self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'directoryTab').
!

updateHttpTabWith: aRepository

	| userAndPassword |
	self repositoryTypeContainer ensureSubViewVisible: (self view viewNamed: 'httpTab').
	userAndPassword := aRepository userAndPassword subStrings: Character tab.
	httpUserPresenter value: userAndPassword first.
	httpPasswordPresenter value: (1 < userAndPassword size
		ifTrue: [userAndPassword at: 2]
		ifFalse: ['']).
! !
!MCVersionDialog categoriesFor: #apply!public! !
!MCVersionDialog categoriesFor: #createComponents!public! !
!MCVersionDialog categoriesFor: #createSchematicWiring!public! !
!MCVersionDialog categoriesFor: #defaultRepository:!public! !
!MCVersionDialog categoriesFor: #httpPassword!public! !
!MCVersionDialog categoriesFor: #httpUser!public! !
!MCVersionDialog categoriesFor: #onViewOpened!public! !
!MCVersionDialog categoriesFor: #repositoryList!public! !
!MCVersionDialog categoriesFor: #repositorySelectionChanged!public! !
!MCVersionDialog categoriesFor: #repositoryTypeContainer!public! !
!MCVersionDialog categoriesFor: #uniqueVersionName!public! !
!MCVersionDialog categoriesFor: #updateDictionaryTabWith:!public! !
!MCVersionDialog categoriesFor: #updateDirectoryTabWith:!public! !
!MCVersionDialog categoriesFor: #updateHttpTabWith:!public! !

!MCVersionDialog class methodsFor!

icon

	^Icon fromFile: 'icons\GS32.ico'.
!

package: aPackage defaultRepository: aRepository

	^(self createOn: aPackage) 
		defaultRepository: aRepository;
		showShell.
!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26738689 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 16 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 592 0 0 0 7 0 0 0 592 0 8 4294902469 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  21 345 850 241 39 592 786 8 #text: 98 1 8 'Repository:' 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 172 0 0 0 130 0 0 0 191 0 0 0] 98 0 850 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 1074 8 #fixedViewLeft 241 1074 8 #fixedParentBottom -359 1074 8 #fixedViewTop 39 410 608 98 16 0 416 98 2 8 1140850944 1 1216 0 0 0 7 0 0 0 1216 0 8 4294902469 690 0 0 0 722 202 208 98 2 786 816 98 2 850 21 111 850 241 39 1216 786 912 98 1 8 'Message:' 1216 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 55 0 0 0 130 0 0 0 74 0 0 0] 98 0 1024 0 27 1042 1088 1 1120 241 1074 8 #fixedParentTop 91 1184 39 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1409286144 131073 1568 0 482 8 4278190080 0 7 0 0 0 1568 655878 ##(Smalltalk.CardLayout)  202 208 98 3 721414 ##(Smalltalk.Association)  8 'HTTP' 410 8 ##(Smalltalk.ContainerView)  98 15 0 1568 98 2 8 1140850688 131073 1792 0 0 0 5 0 0 0 1792 0 234 256 98 4 410 8 ##(Smalltalk.TextEdit)  98 16 0 1792 98 2 8 1140916352 1025 1904 0 482 8 4278190080 0 5 0 0 0 1904 0 8 4294902723 690 0 0 1 722 202 208 98 3 786 816 98 2 850 141 7 850 241 39 1904 786 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1904 786 8 #isTextModified: 98 1 32 1904 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 3 0 0 0 190 0 0 0 22 0 0 0] 98 0 1024 0 27 8 'httpUser' 410 1920 98 16 0 1792 98 2 8 1140916384 1025 2352 0 482 8 4278190080 0 5 0 0 0 2352 0 8 4294902723 690 0 0 1 722 202 208 98 3 786 816 98 2 850 141 51 850 241 39 2352 786 2176 98 1 2210 3 1 3 2352 786 2256 98 1 32 2352 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 70 0 0 0 25 0 0 0 190 0 0 0 44 0 0 0] 98 0 1024 0 27 8 'httpPassword' 0 722 202 208 98 1 786 816 98 2 850 9 49 850 613 135 1792 962 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 54 1 0 0 91 0 0 0] 98 4 410 608 98 16 0 1792 98 2 8 1140850946 1 2896 0 0 0 5 0 0 0 2896 0 8 4294902469 690 0 0 0 722 202 208 98 2 786 816 98 2 850 1 11 850 141 41 2896 786 912 98 1 8 'User: ' 2896 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 5 0 0 0 70 0 0 0 25 0 0 0] 98 0 1024 0 27 410 608 98 16 0 1792 98 2 8 1140850946 1 3200 0 0 0 5 0 0 0 3200 0 8 4294902469 690 0 0 0 722 202 208 98 2 786 816 98 2 850 11 61 850 131 31 3200 786 912 98 1 8 'Password: ' 3200 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 30 0 0 0 70 0 0 0 45 0 0 0] 98 0 1024 0 27 1904 2352 1024 0 27 1746 8 'Dictionary' 410 1808 98 15 0 1568 98 2 8 1140850688 131073 3536 0 0 0 5 0 0 0 3536 0 234 256 98 0 0 722 202 208 98 1 786 816 98 2 850 9 49 850 613 135 3536 962 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 54 1 0 0 91 0 0 0] 98 0 1024 0 27 1746 8 'Directory' 410 1808 98 15 0 1568 98 2 8 1140850688 131073 3824 0 0 0 7 0 0 0 3824 0 234 256 3616 0 722 202 208 98 1 786 816 98 2 850 9 49 850 613 135 3824 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 24 0 0 0 54 1 0 0 91 0 0 0] 98 0 1024 0 27 3824 234 256 98 6 1792 8 'httpTab' 3824 8 'directoryTab' 3536 8 'dictionaryTab' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 1568 98 2 8 1140916736 1 4144 590662 2 ##(Smalltalk.ListModel)  202 208 98 3 3520 1776 3808 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  0 0 1 0 0 0 4144 0 8 4294903551 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 8 #displayString 4352 7 257 0 4338 0 0 4370 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 4464 7 257 0 1049670 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 8 #noIcons 0 0 0 0 0 722 202 208 98 3 786 816 98 2 850 1 1 850 629 191 4144 786 8 #basicSelectionsByIndex: 98 1 98 1 7 4144 786 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 4144 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 58 1 0 0 95 0 0 0] 98 0 1024 0 27 722 202 208 98 1 786 816 98 2 850 21 445 850 629 191 1568 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 222 0 0 0 68 1 0 0 61 1 0 0] 98 4 3536 1792 3824 4144 1024 0 27 1042 1088 1 1074 8 #fixedParentRight 1 1152 -259 1152 -69 410 608 98 16 0 416 98 2 8 1140850944 1 5088 0 0 0 7 0 0 0 5088 0 8 4294902469 690 0 0 0 722 202 208 98 2 786 816 98 2 850 21 21 850 241 39 5088 786 912 98 1 8 'Version Name:' 5088 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 10 0 0 0 130 0 0 0 29 0 0 0] 98 0 1024 0 27 1042 1088 1 1120 241 1536 1 1184 39 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 416 98 2 8 1143017796 1025 5408 0 482 8 4278190080 0 7 0 0 0 5408 0 8 4294902723 690 0 0 9 722 202 208 98 3 786 816 98 2 850 21 151 850 629 185 5408 786 2176 98 1 2210 3 1 3 5408 786 2256 98 1 32 5408 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 75 0 0 0 68 1 0 0 167 0 0 0] 98 0 1024 0 27 1042 1088 1 5056 1 1536 131 1152 -369 410 8 ##(Smalltalk.ComboBox)  98 17 0 416 98 2 8 1412498947 1025 5808 4226 202 208 3616 0 4304 482 8 4278190080 0 7 0 0 0 5808 0 8 4294902449 4338 0 0 1180966 ##(Smalltalk.CompiledExpression)  1 83886081 8 ##(Smalltalk.Message)  8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 5968 7 257 0 3616 401 722 202 208 98 1 786 816 98 2 850 21 385 850 629 47 5808 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 192 0 0 0 68 1 0 0 215 0 0 0] 98 0 1024 0 27 1042 1088 1 5056 1 1152 -319 1184 47 410 1920 98 16 0 416 98 2 8 1140916352 1025 6256 0 482 2000 0 7 0 0 0 6256 0 8 4294902723 690 0 0 1 722 202 208 98 3 786 816 98 2 850 21 61 850 629 41 6256 786 2176 98 1 2210 3 1 3 6256 786 2256 98 1 32 6256 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 30 0 0 0 68 1 0 0 50 0 0 0] 98 0 1024 0 27 1042 1088 1 5056 1 1536 41 1184 41 410 8 ##(Smalltalk.ReferenceView)  98 14 0 416 98 2 8 1140850688 131073 6624 0 0 0 7 0 0 0 6624 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.Presenter)  8 #resource_OK_Cancel_button_block 0 722 202 208 98 1 786 816 98 2 850 21 635 850 629 71 6624 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 10 0 0 0 61 1 0 0 68 1 0 0 96 1 0 0] 3616 1024 0 27 1042 1088 1 5056 1 1152 -69 1184 71 234 256 98 8 1568 8 'repositoryType' 5808 8 'repositoryList' 5408 8 'message' 6256 8 'name' 590342 ##(Smalltalk.Rectangle)  850 21 21 850 21 21 0 0 0 0 12051 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 1969254884 722 202 208 98 2 786 816 98 2 850 2879 21 850 701 801 416 786 8 #updateMenuBar 3616 416 962 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 5 0 0 10 0 0 0 253 6 0 0 154 1 0 0] 98 8 5088 6256 1216 5408 592 5808 6624 1568 1024 0 27 )! !
!MCVersionDialog class categoriesFor: #icon!public! !
!MCVersionDialog class categoriesFor: #package:defaultRepository:!public! !
!MCVersionDialog class categoriesFor: #resource_Default_view!public!resources-views! !

