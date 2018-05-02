"Filed out from Dolphin Smalltalk 7"!

JadeAutoSystemBrowserPresenter subclass: #RowanSystemBrowserPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanSystemBrowserPresenter guid: (GUID fromString: '{ad879b98-36f4-461e-8b30-f07a614dd9d2}')!
RowanSystemBrowserPresenter comment: ''!
!RowanSystemBrowserPresenter categoriesForClass!Unclassified! !
!RowanSystemBrowserPresenter methodsFor!

addPackage
	RowanProjectService defaultProjectName isNil
		ifTrue: 
			[^MessageBox notify: 'No default project has been created. Please specify in the project list.'].
	^super addPackage!

addSubclass
	classDefinitionPresenter
		ensureVisible;
		value: (self classService subclassCreationTemplate);
		yourself!

browseClassReferencesOf: className
	
	| string |
	string := 'browseClassReferences' , Character tab asString , self behaviorIdentifier , Character lf asString.
	(string := self updateCommand: string) = 'browseClassReferences' ifFalse: [self error: 'Unrecognized response'].
	self browseMethodsAndSelect: className.!

categoriesMenuStrings

	false ifTrue: [
		self addMethodCategory; removeMethodCategories.
	].
	^#(
		'&Categories'
		'&Add Method Category//addMethodCategory'
		'&Remove Method Categories//removeMethodCategories'
	).
!

classesMenuStrings

	false ifTrue: [
		self browseClassReferences; fileOutClass; addSubclass; removeClass;  runClassTests.
	].
	^#(
		'&Classes'
		'&Browse References//browseClassReferences'
		'&File Out Class//fileOutClass'
		'Add &Subclass//addSubclass'
		'&Remove//removeClass'
		'-'
		'Set Compiler &Environment ...//setEnvironment'
		'Run &Tests//runClassTests'
	).
!

classService
	^(RowanClassService on: gciSession)
		name: self selectedClassNameWithoutVersion;
		yourself!

compareAncestor

	| repository stream patch |
	repository := repositoryListPresenter hasSelection 
		ifTrue: [repositoryListPresenter selection]
		ifFalse: [repositoryListPresenter list first].
	stream := (WriteStream on: String new)
		nextPutAll: 'comparePackages'; tab;
		nextPutAll: packageListPresenter selections first key; tab;
		nextPutAll: (ancestorListPresenter selections first at: 2); tab;
		nextPutAll: (repository at: 2); tab;
		lf.
	(self updateCommand: stream contents) = 'comparePackages' ifFalse: [self error: 'Unexpected response!!'].
	patch := MCPatch
		fromString: readStream upToEnd
		session: self gciSession.
	patch operations isEmpty ifTrue: [
		MessageBox notify: 'No changes!!'.
		^self.
	].
	RowanPatchBrowser showOn: patch name: packageListPresenter selections first key
!

deletePackage
	| packageName |
	packageListPresenter selections isEmpty ifTrue:[^MessageBox notify: 'No package selected'].
	packageName := packageListPresenter selections first key.
	(MessageBox confirm: 'Do you want to delete Rowan package ''' , packageName , '''?' caption: 'Confirm Delete')
		ifFalse: [^self].
	self updateCommand: 'unloadPackage' , Character tab asString , packageName , Character lf asString
				, self requestString!

inspectPackage

	| service |
	service := RowanPackageService named: packageListPresenter selection key.
	service inspectDefinition!

labelOfClass: oldName changedTo: newName

	| stream |
	oldName = self selectedClassNameWithoutVersion.
	self browseClassReferencesOf: oldName.  "Jadeite gets in trouble deep in server code if the class isn't a class"
	stream := (WriteStream on: String new)
		nextPutAll: 'changeClassName'; tab;
		nextPutAll: self selectedClassNameWithoutVersion; tab;
		nextPutAll: selectedClassOop printString; tab;
		nextPutAll: newName; tab;
		lf; nextPutAll: self requestString;
		yourself.
	self selectedClassName: newName.
	self updateCommand: stream contents.

!

packageTabName
	^'Rowan Packages'!

pkgsMenuStrings

	0 == 1 ifTrue: [
		self findClass; addPackage; loadLatestVersion; savePackage; showPackageChanges; deletePackage; inspectDefinition.
	].
	^#(
		'&Packages'
		'&Find Class.../Ctrl+Shift+F/findClass'
		'&Add Package...//addPackage'
		'-'
		'&Load Latest Version//loadLatestVersion'
		'&Save Package..//savePackage'
		'Show &Changes//showPackageChanges'
		'&Delete Package...//deletePackage'
		'-'
		'&Inspect Package...//inspectPackage'
	).
!

possiblyShowGenericPackageClassTemplate: newDef
	"We got a template back from the server. However, it might not be
	in the rowan style. Get a new one under some conditions."

	| packageService newClassDef selections |
	newClassDef := newDef.
	(classListPresenter selections isEmpty and: [classHierarchyPresenter selections isEmpty])
		ifTrue: 
			[packageService := (self isPackageListTabSelected
						and: [(selections := packageListPresenter selections) notEmpty])
							ifTrue: [RowanPackageService new name: selections first key]
							ifFalse: 
								[classCategoryPresenter selections isEmpty
									ifTrue: [RowanPackageService new]
									ifFalse: [RowanPackageService named: classCategoryPresenter selections first value]].
			newClassDef := packageService genericClassCreationTemplate].
	^newClassDef!

resetClassDefinitionPresenter
	classDefinitionPresenter view
		backcolor: Color white;
		isModified: false;
		yourself!

showPackageChanges
	|  stream patch |
	stream := (WriteStream on: String new)
				nextPutAll: 'changesInPackage';
				tab;
				nextPutAll: packageListPresenter selections first key;
				tab;
				nextPutAll: RowanProjectService new sampleProjectName;
				tab;
				lf.	 
	(self updateCommand: stream contents) = 'changesInPackage'
		ifFalse: [self error: 'Unexpected response!!'].
	patch := MCPatch fromString: readStream upToEnd session: self gciSession. 
	patch operations isEmpty
		ifTrue: 
			[MessageBox notify: 'No changes!!'.
			^self].
	RowanPatchBrowser showOn: patch name: packageListPresenter selections first key!

updateClassInfo
	| index newClassDefinition |
	selectedClassOop := readStream nextLine asNumber.
	newClassDefinition := readStream nextParagraph. 
	classCommentPresenter value: readStream nextParagraph.
	classDefinitionPresenter view isModified
		ifTrue: 
			[newClassDefinition = classDefinition ifTrue: [^self].
			(MessageBox confirm: 'Copy changes to clipboard?' caption: 'Class has unsaved changes!!')
				ifTrue: [classDefinitionPresenter value copyToClipboard]].
	classDefinition := newClassDefinition.
	0 < (classDefinition indexOfSubCollection: 'MyNewClass')
		ifTrue: 
			["Template is based on Stream class, but we don't want to use 'Globals' in template"
			index := classDefinition indexOfSubCollection: ': Globals'.
			(0 < index and: [(classDefinition copyFrom: index - 2 to: index + 1) ~= 'User'])
				ifTrue: 
					[classDefinition := (classDefinition copyFrom: 1 to: index + 1)
								, (dictionaryListPresenter selectionIfNone: ['UserGlobals' -> false]) key
									, (classDefinition copyFrom: index + 9 to: classDefinition size).
					index := classDefinition indexOfSubCollection: 'User Classes'.
					0 < index
						ifTrue: 
							[classDefinition := (classDefinition copyFrom: 1 to: index - 1)
										, (packageListPresenter selectionIfNone: ['User Classes' -> nil]) key
											, (classDefinition copyFrom: index + 12 to: classDefinition size)]]].
	classDefinitionPresenter value: classDefinition.
	classDefinitionPresenter view
		backcolor: Color white;
		isModified: false;
		yourself!

updatePresenters
	
	[inUpdate := true.
	self
		updatePackageDictionaryList;
		updateClassCategoryTree;
		updateClassListOrHierarchy;
		updateClassInfo;
		updateSuperclassList;
		updateMethodFilter;
		updateMethodList;
		updateOverrideList;
		updateMethod;
		updateTabLabel;
		yourself]
			ensure: [inUpdate := false].
	classDefinitionPresenter value: (self possiblyShowGenericPackageClassTemplate: classDefinition).
	self resetClassDefinitionPresenter!

updateTabLabel

	| cardNumber newLabel |
	cardNumber := self parentPresenter view cards indexOf: self view.
	(newLabel := self selectedClassNameWithoutVersion) notEmpty ifTrue: [
		instanceClassTabs currentCard name = 'classTab' ifTrue: [
			newLabel := newLabel , ' class'.
		].
		0 < environment ifTrue: [newLabel := newLabel , ' [' , environment printString , ']'].
	] ifFalse: [
		| tabName selections |
		tabName := packageDictionaryTabs currentCard name.
		tabName = 'packageList' ifTrue: [
			newLabel := 'Rowan Packages'.
			selections := packageListPresenter selections.
		] ifFalse: [
			newLabel := 'Dictionaries'.
			selections := dictionaryListPresenter selections.
		].
		selections size = 1 ifTrue: [newLabel := selections first key].
	].
	self view arrangement: cardNumber printString , ' ' , newLabel.
	self parentPresenter view updateTabs.
! !
!RowanSystemBrowserPresenter categoriesFor: #addPackage!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #addSubclass!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #browseClassReferencesOf:!public! !
!RowanSystemBrowserPresenter categoriesFor: #categoriesMenuStrings!menus!public! !
!RowanSystemBrowserPresenter categoriesFor: #classesMenuStrings!menus!public! !
!RowanSystemBrowserPresenter categoriesFor: #classService!private!rowan! !
!RowanSystemBrowserPresenter categoriesFor: #compareAncestor!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #deletePackage!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #inspectPackage!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #labelOfClass:changedTo:!event handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #packageTabName!menus!overrides!private! !
!RowanSystemBrowserPresenter categoriesFor: #pkgsMenuStrings!menus!public! !
!RowanSystemBrowserPresenter categoriesFor: #possiblyShowGenericPackageClassTemplate:!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #resetClassDefinitionPresenter!private!updating! !
!RowanSystemBrowserPresenter categoriesFor: #showPackageChanges!menu handlers!public! !
!RowanSystemBrowserPresenter categoriesFor: #updateClassInfo!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updatePresenters!public!updating! !
!RowanSystemBrowserPresenter categoriesFor: #updateTabLabel!public!updating! !

!RowanSystemBrowserPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #savePackage 8 '&Save' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 688 nil nil nil 5 nil nil nil 688 626 170 176 34 4 410 ##(Smalltalk.ContainerView) 34 15 nil 688 34 2 8 1140850688 131073 800 nil nil nil 5 nil nil nil 800 626 170 176 8 #() false 170 192 896 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1075 1 1042 711 539 800 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 25 2 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 800 34 2 8 1409286144 131073 1152 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 517 nil nil nil 1152 655878 ##(Smalltalk.CardLayout) 138 144 34 3 721414 ##(Smalltalk.Association) 8 'Categories' 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 1152 34 2 8 1140850688 131073 1408 nil 1218 1248 nil 5 nil nil nil 1408 1280 170 192 34 6 1376 8 'categoryList' 410 ##(Smalltalk.ListView) 34 30 nil 1408 34 2 8 1409372233 1025 1536 590662 2 ##(Smalltalk.ListModel) 138 144 896 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 1218 8 4278190080 nil 21 nil nil nil 1536 nil 8 4294902733 459270 ##(Smalltalk.Message) #displayString 8 #() nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Column 1' 283 #left 1730 #displayString 1760 ##(Smalltalk.SortedCollection) nil nil 1536 nil 3 nil 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 9 1 1904 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 1920 7 257 nil #report 896 nil 131169 nil 34 4 nil nil 1042 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 1042 1 37 1042 291 487 1536 994 #text: 34 1 8 'Column 1' 1536 1090 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 1042 193 193 nil 27 8 'variableList' 410 ##(Smalltalk.ListView) 34 30 nil 1408 34 2 8 1409372233 1025 2288 1602 138 144 896 nil 1664 1218 1696 nil 21 nil nil nil 2288 nil 8 4294902733 1730 #displayString 1760 nil 1792 nil nil nil nil nil nil 138 144 34 1 1842 8 'Column 1' 283 #left 1730 #displayString 1760 ##(Smalltalk.SortedCollection) nil nil 2288 nil 3 nil 1906 0 nil 1938 9 1 1904 1968 1984 2000 #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 2512 7 257 nil #report 896 nil 131169 nil 34 4 nil nil 1042 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 1042 1 37 1042 291 487 2288 994 #text: 34 1 8 'Column 1' 2288 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2256 nil 27 8 'pragmaList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 1152 34 2 8 1140916864 1 2800 1602 138 144 34 3 1360 8 'Variables' 8 'Pragmas' nil 1664 1218 1248 nil 1 nil nil nil 2800 nil 8 4294902801 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1792 nil nil nil nil nil #noIcons nil nil nil nil nil 930 138 144 34 3 994 #createAt:extent: 34 2 1042 1 1 1042 351 539 2800 994 #basicSelectionsByIndex: 34 1 8 #(3) 2800 994 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 2800 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 8 #() 2256 nil 27 nil 1152 1042 33 33 1049862 ##(Smalltalk.ButtonInteractor) 1408 nil 590342 ##(Smalltalk.Rectangle) 1042 255 3 1042 287 35 1 562 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 53 9 1042 291 523 1408 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 171 0 0 0 9 1 0 0] 34 3 1376 1536 2288 2256 nil 27 34 2 8 1409372233 1025 1376 1602 138 144 896 nil 1664 1218 1696 nil 21 nil nil nil 1376 nil 8 4294902733 1730 #displayString 8 #() nil 1792 nil nil nil nil nil nil 138 144 34 1 1842 8 'Column 1' 283 #left 1730 #displayString 3632 ##(Smalltalk.SortedCollection) nil nil 1376 nil 3 nil 1906 0 nil 1938 9 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item first = Character space ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[37 105 226 0 159 31 161 132 221 9 17 226 4 100 163 97 164 183 106 60 106] #item #first ##(Smalltalk.Character) #space #font #beBold #beItalic #font: 3728 7 257 nil #report 896 nil 131169 nil 34 4 nil nil 1042 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 1042 1 37 1042 291 487 1376 994 #text: 34 1 8 'Column 1' 1376 1090 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 145 0 0 0 5 1 0 0] 8 #() 2256 nil 27 1330 2912 1536 1330 2928 2288 2288 170 192 34 2 1408 8 'methodFilterTabs' nil 2800 1408 1042 201 201 401 1 31 nil nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 1 1042 351 539 1152 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 13 1 0 0] 34 2 1408 2800 2256 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 800 34 2 8 1140850688 1 4304 nil 1218 8 4278190080 nil 517 nil nil nil 4304 930 138 144 34 1 994 #createAt:extent: 34 2 1042 351 1 1042 11 539 4304 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 175 0 0 0 0 0 0 0 180 0 0 0 13 1 0 0] 8 #() 2256 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 800 34 2 8 1140850688 131073 4560 nil nil nil 5 nil nil nil 4560 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ComboBox) 34 17 nil 4560 34 2 8 1412498947 1025 4688 1602 138 144 896 nil 1664 1218 8 4278190080 nil 5 nil nil nil 4688 nil 8 4294902579 1730 #displayString 8 #() 896 401 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 493 1042 351 53 4688 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 246 0 0 0 175 0 0 0 16 1 0 0] 8 #() 2256 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 5058 #fixedParentRight 1 5058 #fixedParentBottom -45 5058 #fixedViewTop 47 410 ##(Smalltalk.ComboBox) 34 17 nil 4560 34 2 8 1412498947 1025 5136 1602 138 144 896 nil 1664 1218 4800 nil 5 nil nil nil 5136 nil 8 4294902579 1730 #displayString 8 #() 896 401 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 1 1042 351 53 5136 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 175 0 0 0 26 0 0 0] 8 #() 2256 nil 27 5026 5072 1 5088 1 5058 #fixedParentTop 1 5120 47 410 ##(Smalltalk.ListView) 34 30 nil 4560 34 2 8 1409372233 1025 5488 1602 138 144 896 nil 1664 1218 1696 nil 13 nil nil nil 5488 nil 8 4294902733 1730 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1792 nil nil nil nil nil nil 138 144 34 2 1842 8 'Column 1' 311 #left 1730 #displayString 5632 ##(Smalltalk.SortedCollection) 1906 0 nil 1938 1 83886081 1904 8 'doIt' 8 '[:each | each at: 5]' 8 #[29 105 17 214 5 148 106] 5728 7 257 nil nil 5488 nil 3 nil 1906 0 nil 1938 5 1 1904 8 'doIt' 8 '[:each | (each item at: 4) ifTrue: [each font: each font beItalic]]' 8 #[33 105 226 0 214 4 148 123 17 226 1 160 179 106 60 106] #item #font #beItalic #font: 5808 7 257 nil 1842 8 '' 33 #center 994 #empty 8 #() 16 1730 #<= 8 #() nil nil 5488 1906 0 nil 1938 6 1 1904 8 'doIt' 8 '[:each | ((each at: 2) ifTrue: [RowanSystemBrowserPresenter overriddenIcon] ifFalse: [Icon blank]) 
	imageIndex]' 8 #[34 105 17 64 148 120 29 159 111 31 161 162 106] ##(Smalltalk.RowanSystemBrowserPresenter) #overriddenIcon ##(Smalltalk.Icon) #blank #imageIndex 5984 7 257 nil 1 nil nil #report 896 nil 131173 nil 8 #(nil) 930 138 144 34 3 994 #createAt:extent: 34 2 1042 1 47 1042 351 447 5488 994 #text: 34 1 8 'Column 1' 5488 994 #columnOrder: 34 1 8 #(2 1) 5488 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 23 0 0 0 175 0 0 0 246 0 0 0] 8 #() 2256 nil 27 5026 5072 1 5088 1 5472 47 5104 -45 170 192 34 6 4688 8 'overrideList' 5136 8 'superclassList' 5488 8 'methodList' nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 361 1 1042 351 539 4560 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 99 1 0 0 13 1 0 0] 34 3 5136 5488 4688 2256 nil 27 2256 nil 27 5 410 ##(Smalltalk.ContainerView) 34 15 nil 688 34 2 8 1140850688 131073 6592 nil nil nil 5 nil nil nil 6592 626 170 176 34 6 410 ##(Smalltalk.ContainerView) 34 15 nil 6592 34 2 8 1140850688 131073 6704 nil nil nil 5 nil nil nil 6704 4626 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 6704 34 2 8 1409286144 131073 6816 nil 1218 8 4278190080 nil 5 nil nil nil 6816 1266 138 144 34 2 1330 590662 1 ##(Smalltalk.CardLabel) 8 'Classes' 1906 0 nil 1938 7 1 16 8 'doIt' 8 '(CardLabel text: ''Classes'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 7008 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 7024 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 6816 34 2 8 1409372233 1025 7120 1602 138 144 896 nil 1664 1218 8 4278190080 nil 29 nil nil nil 7120 nil 8 4294902733 1730 #displayString 8 #() nil 1792 nil nil nil nil nil nil 138 144 34 2 1842 8 'Column 1' 359 #left 1906 0 nil 1938 5 1 1904 8 'doIt' 8 '[:each | (each subStrings: Character space) first]' 8 #[33 105 17 29 159 178 161 106] ##(Smalltalk.Character) #space #subStrings: #first 7360 7 257 nil ##(Smalltalk.SortedCollection) nil nil 7120 nil 3 nil nil 1842 8 '' 69 #left 1906 0 nil 1938 6 1 1904 8 'doIt' 8 '[:each | | index |
	index := each indexOf: Character space.
	0 < index ifTrue: [each copyFrom: index + 2 to: each size - 1] ifFalse: ['''']]' 8 #[34 105 17 29 159 178 90 62 18 128 221 9 233 1 64 126 17 145 99 193 106 33 106] ##(Smalltalk.Character) #space #indexOf: #copyFrom:to: 7456 7472 7 65793 nil 1730 #<= 8 #() nil nil 7120 nil 1 nil nil #report 896 nil 131169 nil 34 4 nil nil 1042 1 1 nil 930 138 144 34 3 994 #createAt:extent: 34 2 1042 9 55 1042 435 433 7120 994 #text: 34 1 8 'Column 1' 7120 994 #columnOrder: 34 1 8 #(2 1) 7120 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 221 0 0 0 243 0 0 0] 8 #() 2256 nil 27 1330 6978 8 'Hierarchy' 1906 0 nil 1938 7 1 16 8 'doIt' 8 '(CardLabel text: ''Hierarchy'' iconBlock: [Icon fromId: ''ClassHierarchyDiagram.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 7904 ##(Smalltalk.Icon) 8 'ClassHierarchyDiagram.ico' #fromId: #text:iconBlock: 7920 11 1 nil nil 410 ##(Smalltalk.TreeView) 34 27 nil 6816 34 2 8 1409352231 1025 8016 590918 3 ##(Smalltalk.TreeModel) nil 1664 525062 ##(Smalltalk.TreeNode) nil nil nil 170 192 896 1218 8 4278190080 nil 29 nil nil nil 8016 nil 8 4294902776 1906 0 nil 1938 2 1 1728 8 'doIt' 8 '[:each | each last]' 8 #[30 105 226 0 106] #last 8208 7 257 nil ##(Smalltalk.IconicListAbstract) 1792 nil nil nil nil nil 170 176 896 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 896 930 138 144 34 1 994 #createAt:extent: 34 2 1042 9 55 1042 435 433 8016 1090 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 221 0 0 0 243 0 0 0] 8 #() 2256 nil 27 7120 170 192 34 4 8016 8 'classHierarchy' 7120 8 'classList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 6816 34 2 8 1140916736 1 8544 1602 138 144 34 2 6992 7888 nil 1664 nil nil 1 nil nil nil 8544 nil 8 4294902801 1906 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 8672 7 257 nil 1906 0 nil 8690 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 8736 7 257 nil 1792 nil nil nil nil nil #smallIcons nil nil nil nil nil 930 138 144 34 3 994 #createAt:extent: 34 2 1042 1 1 1042 451 495 8544 994 #basicSelectionsByIndex: 34 1 8 #(1) 8544 994 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 8544 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 247 0 0 0] 8 #() 2256 nil 27 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 1 1042 451 495 6816 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 247 0 0 0] 34 3 7120 8016 8544 2256 nil 27 5026 5072 1 5088 1 5472 1 5104 -43 410 ##(Smalltalk.CardContainer) 34 16 nil 6704 34 2 8 1409286144 131073 9200 nil 1218 8 4278190080 nil 5 nil nil nil 9200 1266 138 144 34 2 1330 6978 8 'Instance' 1906 0 nil 1938 7 1 16 8 'doIt' 8 '(CardLabel text: ''Instance'' iconBlock: [Icon fromId: ''Class.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 9376 ##(Smalltalk.Icon) 8 'Class.ico' #fromId: #text:iconBlock: 9392 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 9200 34 2 8 1140850688 131073 9488 nil nil nil 5 nil nil nil 9488 nil 170 192 896 nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 9 9 1042 435 1 9488 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 221 0 0 0 4 0 0 0] 8 #() 2256 nil 27 1330 6978 8 'Class' 1906 0 nil 1938 7 1 16 8 'doIt' 8 '(CardLabel text: ''Class'' iconBlock: [Icon fromId: ''Metaclass.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 9760 ##(Smalltalk.Icon) 8 'Metaclass.ico' #fromId: #text:iconBlock: 9776 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 9200 34 2 8 1140850688 131073 9872 nil nil nil 5 nil nil nil 9872 nil 170 192 896 nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 9 9 1042 435 1 9872 1090 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 221 0 0 0 4 0 0 0] 8 #() 2256 nil 27 9488 170 192 34 4 9488 8 'instanceTab' 9872 8 'classTab' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 9200 34 2 8 1140916738 1 10176 1602 138 144 34 2 9360 9744 nil 1664 nil nil 1 nil nil nil 10176 nil 8 4294902801 1906 0 nil 8690 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 10304 7 257 nil 1906 0 nil 8690 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 10352 7 257 nil 1792 nil nil nil nil nil #smallIcons nil nil nil nil nil 930 138 144 34 3 994 #createAt:extent: 34 2 1042 1 1 1042 451 51 10176 994 #basicSelectionsByIndex: 34 1 8 #(1) 10176 994 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 10176 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 225 0 0 0 25 0 0 0] 8 #() 2256 nil 27 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 489 1042 451 51 9200 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 244 0 0 0 225 0 0 0 13 1 0 0] 34 3 9488 9872 10176 2256 nil 27 5026 5072 1 5088 1 5104 -49 5120 51 170 192 34 4 6816 8 'classHierarchyTabs' 9200 8 'instanceClassTabs' nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 615 1 1042 451 539 6704 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 51 1 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 2 6816 9200 2256 nil 27 13 410 ##(Smalltalk.SlideyInneyOuteyThing) 34 23 nil 6592 34 2 8 1409286144 131073 11040 nil 1218 8 4278190080 nil 517 nil nil nil 11040 1266 138 144 34 2 1330 6978 8 'Dictionaries' 1906 0 nil 1938 7 1 16 8 'doIt' 8 '(CardLabel text: ''Dictionaries'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 11216 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 11232 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 410 ##(Smalltalk.SlidingCardTray) 34 22 nil 11040 34 2 8 1140850688 131073 11360 nil 1218 11120 nil 5 nil nil nil 11360 11136 170 192 34 4 11328 8 'dictionaryList' 410 ##(Smalltalk.ListView) 34 30 nil 11360 34 2 8 1409405001 1025 11488 1602 138 144 896 nil 1664 1218 7232 nil 5 nil nil nil 11488 nil 8 4294902733 1730 #displayString 8 #() nil 1792 nil nil nil nil nil nil 138 144 34 1 1842 8 'Column 1' 305 #left 1730 #displayString 11632 ##(Smalltalk.SortedCollection) 1906 0 nil 1938 2 1 1904 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 11728 7 257 nil nil 11488 nil 3 nil 1906 0 nil 1938 6 1 1904 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold; beItalic)]]' 8 #[34 105 226 0 142 221 9 17 226 1 100 160 97 161 180 106 60 106] #item #font #beBold #beItalic #font: 11808 7 257 nil #report 896 nil 131169 nil 34 4 nil nil 1042 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 1042 1 37 1042 313 487 11488 994 #text: 34 1 8 'Column 1' 11488 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 156 0 0 0 5 1 0 0] 8 #() 2256 nil 27 8 'packageList' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 11040 34 2 8 1140916864 1 12144 1602 138 144 34 2 6978 8 'Rowan Packages' 1906 0 nil 1938 7 1 6976 8 'doIt' 8 '(CardLabel text: ''Rowan Packages'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[45 30 35 113 47 32 180 106 195 105] 983558 ##(Smalltalk.VariableBinding) #CardLabel 6976 12272 12370 #Icon ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 12288 11 1 nil nil 11200 nil 1664 1218 11120 nil 1 nil nil nil 12144 nil 8 4294902801 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1792 nil nil nil nil nil #smallIcons nil nil nil nil nil 930 138 144 34 3 994 #createAt:extent: 34 2 1042 1 1 1042 373 539 12144 994 #basicSelectionsByIndex: 34 1 8 #(1) 12144 994 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 12144 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 186 0 0 0 13 1 0 0] 8 #() 2256 nil 27 nil 11040 1042 33 33 3234 11360 nil 3266 1042 277 3 1042 309 35 1 562 #togglePin 8 'Pin or Unpin the tray' 1 1 nil nil nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 53 9 1042 313 523 11360 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 0 0 0 4 0 0 0 182 0 0 0 9 1 0 0] 34 2 11488 11328 2256 nil 27 34 2 8 1409372233 1025 11328 1602 138 144 896 nil 1664 1218 7232 nil 21 nil nil nil 11328 nil 8 4294902733 1730 #displayString 8 #() nil 1792 nil nil nil nil nil nil 138 144 34 1 1842 8 'Column 1' 305 #left 1730 #displayString 13088 ##(Smalltalk.SortedCollection) 1906 0 nil 1938 2 1 1904 8 'doIt' 8 '[:each | each key]' 8 #[30 105 226 0 106] #key 13184 7 257 nil nil 11328 nil 3 nil 1906 0 nil 1938 5 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each item value ifTrue: [each font: (each font beBold)]]' 8 #[33 105 226 0 142 123 17 226 1 160 179 106 60 106] #item #font #beBold #font: 13264 7 257 nil #report 896 nil 131169 nil 34 4 nil nil 1042 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 1042 1 37 1042 313 487 11328 994 #text: 34 1 8 'Column 1' 11328 1090 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 156 0 0 0 5 1 0 0] 8 #() 2256 nil 27 1330 12256 11488 11488 170 192 34 2 11360 8 'packageDictionaryTabs' nil 12144 11360 1042 201 201 401 1 31 nil nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 1 1042 373 539 11040 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 186 0 0 0 13 1 0 0] 34 2 11360 12144 2256 nil 27 11 410 ##(Smalltalk.ContainerView) 34 15 nil 6592 34 2 8 1140850688 131073 13824 nil nil nil 5 nil nil nil 13824 626 170 176 896 false 170 192 34 2 410 ##(Smalltalk.TreeView) 34 27 nil 13824 34 2 8 1409352231 1025 13952 8082 nil 1664 8114 nil nil nil 170 192 896 1218 8 4278190080 nil 21 nil nil nil 13952 nil 8 4294902776 1906 0 nil 1938 5 1 1904 8 'doIt' 8 '[:each | each key isEmpty ifTrue: [''--All Categories--''] ifFalse: [each key last]].' 8 #[33 105 226 0 159 119 31 106 226 0 161 106] #key #isEmpty 8 '--All Categories--' #last 14112 7 257 nil ##(Smalltalk.IconicListAbstract) 1792 nil nil nil nil nil 170 176 896 17 #noIcons 1 170 ##(Smalltalk.PluggableLookupTable) 896 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 1 1042 223 539 13952 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 111 0 0 0 13 1 0 0] 8 #() 2256 nil 27 8 'classCategoryList' nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 383 1 1042 223 539 13824 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 191 0 0 0 0 0 0 0 46 1 0 0 13 1 0 0] 34 1 13952 2256 nil 27 7 false 170 192 896 nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 1 1042 1065 539 6592 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 20 2 0 0 13 1 0 0] 34 5 11040 410 ##(Smalltalk.Splitter) 34 12 nil 6592 34 2 8 1140850688 1 14752 nil 1218 4384 nil 517 nil nil nil 14752 930 138 144 34 1 994 #createAt:extent: 34 2 1042 373 1 1042 11 539 14752 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 186 0 0 0 0 0 0 0 191 0 0 0 13 1 0 0] 8 #() 2256 nil 27 13824 410 ##(Smalltalk.Splitter) 34 12 nil 6592 34 2 8 1140850688 1 14992 nil 1218 4384 nil 517 nil nil nil 14992 930 138 144 34 1 994 #createAt:extent: 34 2 1042 605 1 1042 11 539 14992 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 46 1 0 0 0 0 0 0 51 1 0 0 13 1 0 0] 8 #() 2256 nil 27 6704 2256 nil 27 7 false 170 192 896 nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 1 1042 1785 539 688 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 13 1 0 0] 34 3 6592 410 ##(Smalltalk.Splitter) 34 12 nil 688 34 2 8 1140850688 1 15408 nil 1218 4384 nil 517 nil nil nil 15408 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1065 1 1042 11 539 15408 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 20 2 0 0 0 0 0 0 25 2 0 0 13 1 0 0] 8 #() 2256 nil 27 800 2256 nil 27 524806 ##(Smalltalk.Fraction) 3 5 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 15680 nil 1218 6896 nil 5 nil nil nil 15680 1266 138 144 34 7 1330 6978 8 'Package' 1906 0 nil 1938 7 1 6976 8 'doIt' 8 '(CardLabel text: ''Package'' iconBlock: [Icon fromId: ''Package.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 15840 ##(Smalltalk.Icon) 8 'Package.ico' #fromId: #text:iconBlock: 15856 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 15680 34 2 8 1140850688 131073 15952 nil nil nil 5 nil nil nil 15952 626 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 15952 34 2 8 1409355853 1025 16064 1602 138 144 896 nil 1664 1218 8 4278190080 nil 5 482 nil true 34 1 530 1 562 #compareAncestor 8 '&Compare' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 16064 nil 8 4294902733 1730 #displayString 8 #() nil 1792 nil nil nil nil nil nil 138 144 34 4 1842 8 'Ancestor' 131 #left 1730 #displayString 8 #() 1730 #<= 16416 1906 0 nil 1938 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 16448 7 257 nil nil 16064 nil 1 nil nil 1842 8 'Name' 401 #left 1730 #displayString 16320 ##(Smalltalk.SortedCollection) 1906 0 nil 1938 1 83886081 1904 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 16576 7 257 nil nil 16064 nil 1 nil nil 1842 8 'Timestamp' 301 #left 1730 #displayString 8 #() 1730 #<= 16704 1906 0 nil 1938 1 83886081 1904 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 16736 7 257 nil nil 16064 nil 1 nil nil 1842 8 'Message' 941 #left 1730 #displayString 16704 1730 #<= 16704 1906 0 nil 1938 1 83886081 1904 8 'doIt' 8 '[:each | each at: 4]' 8 #[29 105 17 214 4 148 106] 16880 7 257 nil nil 16064 nil 3 nil nil #report 896 nil 131169 nil 34 4 nil nil 1042 1 1 nil 930 138 144 34 3 994 #createAt:extent: 34 2 1042 1 1 1042 1769 263 16064 994 #contextMenu: 34 1 16192 16064 994 #text: 34 1 8 'Ancestor' 16064 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 116 3 0 0 131 0 0 0] 8 #() 2256 nil 27 23 410 ##(Smalltalk.ListView) 34 30 nil 15952 34 2 8 1409355853 1025 17232 1602 138 144 896 nil 1664 1218 16176 nil 5 482 nil true 34 5 530 1 562 #addRepository 8 'Add &Repository' 1 1 nil nil nil 530 1 562 #removeRepository 8 'Remo&ve Repository' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 530 1 562 #savePackage 8 '&Save Package' 1 1 nil nil nil 530 1 562 #showPackageChanges 8 'Show &Changes' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 17232 nil 8 4294902733 1730 #displayString 8 #() nil 1792 nil nil nil nil nil nil 138 144 34 4 1842 8 'Repository Type' 211 #left 1730 #displayString 17648 ##(Smalltalk.SortedCollection) 1906 0 nil 1938 2 1 1904 8 'doIt' 8 '[:each | (each at: 1) copyFrom: 3 to: (each at: 1) size - 10]' 8 #[30 105 17 63 148 214 3 17 63 148 145 214 10 127 190 106] #copyFrom:to: 17744 7 257 nil nil 17232 nil 1 nil nil 1842 8 'Description' 1161 #left 1730 #displayString 16704 1730 #<= 16704 1906 0 nil 1938 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 17888 7 257 nil nil 17232 nil 3 nil nil 1842 8 'User' 201 #left 1730 #displayString 16416 1730 #<= 16416 1906 0 nil 1938 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 18032 7 257 nil nil 17232 nil 1 nil nil 1842 8 'Password' 201 #left 1730 #displayString 16416 1730 #<= 16416 1906 0 nil 1938 3 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | (each at: 4) collect: [:char | $*]]' 8 #[30 105 17 214 4 148 31 112 215 42 106 176 106] #collect: 18176 1906 0 nil 18192 19 257 nil 7 257 nil nil 17232 nil 1 nil nil #report 896 nil 131169 nil 34 4 nil nil 1042 1 1 nil 930 138 144 34 3 994 #createAt:extent: 34 2 1042 1 263 1042 1769 217 17232 994 #contextMenu: 34 1 17344 17232 994 #text: 34 1 8 'Repository Type' 17232 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 131 0 0 0 116 3 0 0 239 0 0 0] 8 #() 2256 nil 27 19 true 170 192 34 4 16064 8 'ancestorList' 17232 8 'repositoryList' nil 930 138 144 34 2 994 #createAt:extent: 34 2 1042 9 55 1042 1769 479 15952 994 #text: 34 1 8 'Package' 15952 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 34 2 16064 17232 2256 nil 27 1330 6978 8 'Globals' 1906 0 nil 1938 7 1 16 8 'doIt' 8 '(CardLabel text: ''Globals'' iconBlock: [Icon fromId: ''Dictionary.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 18848 ##(Smalltalk.Icon) 8 'Dictionary.ico' #fromId: #text:iconBlock: 18864 11 1 nil nil 410 ##(Smalltalk.ListView) 34 30 nil 15680 34 2 8 1409355853 1025 18960 1602 138 144 896 nil 1664 1218 16176 nil 5 nil nil nil 18960 nil 8 4294902733 1730 #displayString 8 #() nil 1792 nil nil nil nil nil nil 138 144 34 3 1842 8 'Name' 587 #left 1730 #displayString 19104 ##(Smalltalk.SortedCollection) 1906 0 nil 1938 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 1]' 8 #[29 105 17 63 148 106] 19200 7 257 nil nil 18960 nil 3 nil nil 1842 8 'Class' 589 #left 1730 #displayString 16704 1730 #<= 16704 1906 0 nil 1938 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 2]' 8 #[29 105 17 64 148 106] 19344 7 257 nil nil 18960 nil 3 nil nil 1842 8 'Value' 587 #left 1730 #displayString 16704 1730 #<= 16704 1906 0 nil 1938 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each at: 3]' 8 #[29 105 17 214 3 148 106] 19488 7 257 nil nil 18960 nil 3 nil nil #report 896 nil 131169 nil 34 4 nil nil 1042 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 1042 9 55 1042 1769 479 18960 994 #text: 34 1 8 'Name' 18960 1090 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2256 nil 27 1330 6978 8 'Original Source' 1906 0 nil 1938 7 1 16 8 'doIt' 8 '(CardLabel text: ''Original Source'' iconBlock: [Icon fromId: ''COMPILEDMETHOD_PRIVATE.ico''])' 8 #[29 30 35 113 31 32 180 106 195 105] 6976 19840 ##(Smalltalk.Icon) 8 'COMPILEDMETHOD_PRIVATE.ico' #fromId: #text:iconBlock: 19856 11 1 nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15680 34 2 8 1445007428 1025 19952 721990 2 ##(Smalltalk.ValueHolder) nil false 1310726 ##(Smalltalk.EqualitySearchPolicy) nil 1218 8 4278190080 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1042 193 193 nil 19952 nil 8 4294902725 852486 ##(Smalltalk.NullConverter) nil nil 11 nil 170 192 34 4 #normal 1182790 1 ##(Smalltalk.ScintillaTextStyle) 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20290 77 786694 ##(Smalltalk.IndexedColor) 33554457 20338 33554471 1 nil nil nil nil #callTip nil nil nil 34 256 20304 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20320 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1377542 ##(Smalltalk.SmalltalkMethodStyler) 1 nil nil false 138 144 896 170 192 34 2 #default 1641542 1 ##(Smalltalk.ScintillaMarkerDefinition) 1 nil nil nil 19952 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 896 nil 170 176 896 9215 nil nil nil nil 20338 33554447 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 20304 nil nil nil nil 1 nil 170 192 34 6 1 1510470 2 ##(Smalltalk.ScintillaIndicatorStyle) 1 19952 65025 3 false 1 nil nil nil nil nil nil 3 20754 3 19952 33423361 5 false 3 nil nil nil nil nil nil 5 20754 5 19952 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20290 1029 nil 196934 1 ##(Smalltalk.RGB) 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20290 1027 20866 60239 20866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20290 1031 20866 335 20866 31317503 1 nil nil nil nil #Error nil nil nil nil 930 138 144 34 7 994 #createAt:extent: 34 2 1042 9 55 1042 1769 479 19952 994 #modificationEventMask: 8 #(4607) 19952 994 #caretPeriod: 8 #(530) 19952 994 #wordWrap: 8 #(true) 19952 994 #margins: 34 1 34 3 985158 2 ##(Smalltalk.ScintillaMargin) 1 19952 61 3 nil nil 5 nil 21250 3 19952 1 nil 3 67108863 5 nil 21250 5 19952 1 nil 3 -67108863 5 nil 19952 994 #tabWidth: 8 #(4) 19952 994 #maxCompletionListHeight: 8 #(9) 19952 1090 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2256 nil 29 1330 6978 8 'SUnit' 1906 0 nil 1938 4 1 1904 8 'doIt' 8 '[Icon fromId: ''icons\UnitTestPlugin.ico'']' 8 #[32 105 45 30 178 106] 12370 #Icon ##(Smalltalk.Icon) 8 'icons\UnitTestPlugin.ico' #fromId: 21472 7 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 15680 34 2 8 1140850688 131073 21584 nil 1218 8 4278190080 nil 5 nil nil nil 21584 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitInClassPresenter) #resource_Default_view nil 930 138 144 34 1 994 #createAt:extent: 34 2 1042 9 55 1042 1769 479 21584 1090 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 896 2256 nil 27 1330 6978 8 'Class Definition' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15680 34 2 8 1445007428 1025 21904 20018 nil false 20064 nil 1218 8 4278190080 nil 5 482 nil true 34 11 530 1 562 #editSave 8 '&Save' 9383 1 nil nil nil 17474 4097 530 1 562 #editUndo 8 '&Undo' 9397 1 nil nil nil 530 1 562 #editRedo 8 '&Redo' 9395 1 nil nil nil 17474 4097 530 1 562 #editCut 8 'Cu&t' 9393 1 nil nil nil 530 1 562 #editCopy 8 '&Copy' 9351 1 nil nil nil 530 1 562 #editPaste 8 '&Paste' 9389 1 nil nil nil 530 1 562 #editDelete 8 '&Delete' 1629 1 nil nil nil 17474 4097 530 1 562 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil 20114 nil true 20146 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1042 193 193 nil 21904 nil 8 4294902725 20226 nil nil 13 nil 170 192 34 4 #normal 20290 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20290 77 20352 20368 1 nil nil nil nil #callTip nil nil nil 34 256 22624 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 22640 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1639686 ##(Smalltalk.SmalltalkExpressionStyler) 1 nil nil false 138 144 896 170 192 34 2 #default 20482 1 nil nil nil 21904 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 896 nil 170 176 896 9215 nil nil nil nil 20544 nil nil 65 nil nil 170 192 34 6 #specialCharacter 8 '()[]<>' #literalArray 8 '()' #literalBytes 8 '[]' 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 22624 nil nil nil nil 1 nil 170 192 34 6 1 20754 1 21904 65025 3 false 1 nil nil nil nil nil nil 3 20754 3 21904 33423361 5 false 3 nil nil nil nil nil nil 5 20754 5 21904 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20290 1029 nil 20866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20290 1027 20866 60239 20866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20290 1031 20866 335 20866 31317503 1 nil nil nil nil #Error nil nil nil nil 930 138 144 34 8 994 #createAt:extent: 34 2 1042 9 55 1042 1769 479 21904 994 #contextMenu: 34 1 22016 21904 994 #modificationEventMask: 8 #(4607) 21904 994 #caretPeriod: 8 #(530) 21904 994 #wordWrap: 8 #(true) 21904 994 #margins: 34 1 34 3 21250 1 21904 1 3 nil nil 5 nil 21250 3 21904 33 nil 3 67108863 5 nil 21250 5 21904 1 nil 3 -67108863 5 nil 21904 994 #tabWidth: 8 #(4) 21904 994 #maxCompletionListHeight: 8 #(9) 21904 1090 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2256 nil 29 1330 6978 8 'Class Documentation' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15680 34 2 8 1445007428 1025 23696 20018 nil false 20064 nil 1218 22000 nil 5 nil 20114 nil true 20146 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1042 193 193 nil 23696 nil 8 4294902725 20226 nil nil 11 nil 170 192 34 4 #normal 20290 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20290 77 20352 20368 1 nil nil nil nil #callTip nil nil nil 34 256 23920 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 23936 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 1245510 1 ##(Smalltalk.NullScintillaStyler) #normal 170 192 34 2 #default 20482 1 nil nil nil 23696 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 896 nil 170 176 896 9215 nil nil nil nil 20544 nil nil 65 nil nil nil 8 '' 3 170 192 34 2 #container 170 192 34 2 #normal 23920 nil nil nil nil 1 nil 170 192 34 6 1 20754 1 23696 65025 3 false 1 nil nil nil nil nil nil 3 20754 3 23696 33423361 5 false 3 nil nil nil nil nil nil 5 20754 5 23696 511 1 false 5 nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20290 1029 nil 20866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20290 1027 20866 60239 20866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20290 1031 20866 335 20866 31317503 1 nil nil nil nil #Error nil nil nil nil 930 138 144 34 7 994 #createAt:extent: 34 2 1042 9 55 1042 1769 479 23696 994 #modificationEventMask: 8 #(4607) 23696 994 #caretPeriod: 8 #(530) 23696 994 #wordWrap: 8 #(true) 23696 994 #margins: 34 1 34 3 21250 1 23696 1 3 nil nil 5 nil 21250 3 23696 33 nil 3 67108863 5 nil 21250 5 23696 1 nil 3 -67108863 5 nil 23696 994 #tabWidth: 8 #(4) 23696 994 #maxCompletionListHeight: 8 #(9) 23696 1090 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2256 nil 29 1330 6978 8 'Method Source' nil nil 410 ##(Smalltalk.ScintillaView) 34 50 nil 15680 34 2 8 1445007428 1025 24864 20018 nil false 20064 nil 1218 8 4278190080 nil 5 nil 20114 nil true 20146 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1042 193 193 nil 24864 nil 8 4294902725 20226 nil nil 13 nil 170 192 34 4 #normal 20290 1 nil nil 1 nil nil nil nil #normal nil nil nil #callTip 20290 77 20352 20368 1 nil nil nil nil #callTip nil nil nil 34 256 25104 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 25120 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 20402 1 nil nil false 138 144 896 170 192 34 2 #default 20482 1 nil nil nil 24864 #circle nil nil nil nil nil nil 138 ##(Smalltalk.IdentitySet) 896 nil 170 176 896 9215 nil nil nil nil 20544 nil nil 65 nil nil 22800 8 '' 3 170 192 34 2 #container 170 192 34 4 #normal 25104 #callTip 25120 nil nil nil nil 1 nil 170 192 34 12 1 20754 1 24864 65025 3 false 1 nil nil nil nil nil nil 3 20754 3 24864 33423361 5 false 3 nil nil nil nil nil nil 5 20754 5 24864 511 1 false 5 nil nil nil nil nil nil 8 'indicator8' 20754 17 24864 33554447 1 nil nil nil nil nil nil nil nil 8 'indicator9' 20754 19 24864 33554459 13 nil nil nil nil nil nil nil nil 8 'indicator10' 20754 21 24864 33554459 3 nil nil nil nil nil nil nil nil nil nil 170 192 34 6 #Notification 20290 1029 nil 20866 31185883 1 nil nil nil nil #Notification nil nil nil #Warning 20290 1027 20866 60239 20866 31326207 1 nil nil nil nil #Warning nil nil nil #Error 20290 1031 20866 335 20866 31317503 1 nil nil nil nil #Error nil nil nil nil 930 138 144 34 7 994 #createAt:extent: 34 2 1042 9 55 1042 1769 479 24864 994 #modificationEventMask: 8 #(4607) 24864 994 #hoverTime: 8 #(200) 24864 994 #caretPeriod: 8 #(530) 24864 994 #margins: 34 1 34 3 21250 1 24864 61 3 nil nil 5 nil 21250 3 24864 1 nil 3 67108863 nil nil 21250 5 24864 1 nil 3 -67108863 nil nil 24864 994 #tabWidth: 8 #(4) 24864 994 #maxCompletionListHeight: 8 #(9) 24864 1090 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 120 3 0 0 10 1 0 0] 8 #() 2256 nil 29 15952 170 192 34 14 18960 8 'globals' 21584 8 'sunit' 23696 8 'classDocumentation' 19952 8 'originalSource' 15952 8 'packageInfo' 21904 8 'classDefinition' 24864 8 'methodSource' nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 15680 34 2 8 1140916736 1 26240 1602 138 144 34 7 15824 18832 21872 23664 19824 24832 21440 nil 1664 nil nil 1 nil nil nil 26240 nil 8 4294902801 1906 0 nil 8690 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 26368 7 257 nil 1906 0 nil 8690 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 26416 7 257 nil 1792 nil nil nil nil nil #smallIcons nil nil nil nil nil 930 138 144 34 3 994 #createAt:extent: 34 2 1042 1 1 1042 1785 541 26240 994 #basicSelectionsByIndex: 34 1 8 #(1) 26240 994 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 26240 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 124 3 0 0 14 1 0 0] 8 #() 2256 nil 27 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 549 1042 1785 541 15680 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 1 0 0 124 3 0 0 32 2 0 0] 34 8 15952 18960 21904 23696 19952 24864 21584 26240 2256 nil 27 15650 3 5 true 170 192 34 4 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 26912 nil 1218 4384 nil 517 nil nil nil 26912 930 138 144 34 1 994 #createAt:extent: 34 2 1042 1 539 1042 1785 11 26912 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 13 1 0 0 124 3 0 0 18 1 0 0] 8 #() 2256 nil 27 8 'splitter' 15680 8 'textAreaTabs' nil 930 138 144 34 2 994 #createAt:extent: 34 2 1042 3839 21 1042 1785 1089 416 994 #contextMenu: 34 1 496 416 1090 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 251 10 0 0 42 2 0 0] 34 3 688 26912 15680 2256 nil 27 )! !
!RowanSystemBrowserPresenter class categoriesFor: #resource_Default_view!private!resources-views! !

