"Filed out from Dolphin Smalltalk 7"!

JadeSUnitAbstractPresenter subclass: #JadeSUnitPresenter
	instanceVariableNames: 'packagesPresenter testClassesPresenter status testCounter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSUnitPresenter guid: (GUID fromString: '{f0d7615e-1a5a-4d8a-b979-c2ad927a9f74}')!
JadeSUnitPresenter comment: ''!
!JadeSUnitPresenter categoriesForClass!Unclassified! !
!JadeSUnitPresenter methodsFor!

createComponents

	super createComponents.

	packagesPresenter := self add: ListPresenter new name: 'packages'.
	testClassesPresenter := self add: ListPresenter new name: 'testClasses'.!

createSchematicWiring

	super createSchematicWiring.

	packagesPresenter when: #selectionChanged send: #onPackageChanged to: self.

	testClassesPresenter when: #selectionChanged send: #onClassesChanged to: self.
!

debug

	testClassesPresenter hasSelection ifFalse: [^self].
	(testCasesPresenter selection size = 1) ifFalse: [^MessageBox  notify: 'Select only one TestCase to debug'].

	^model serverPerform: #debugTestNamed:of: with: testCasesPresenter selection last methodName with: testClassesPresenter selection!

getTestCounter

	^'Total Tests: ', testCounter first printString!

model: aGciSession
	| gsResult packagesNames |

	super model: aGciSession.

"	gsResult := model serverPerform: #getPackagesNamesWithTestClasses.
	packagesNames := gsResult subStrings: ','.

	packagesPresenter model: (ListModel on: packagesNames asSortedCollection). "!

onClassesChanged
	| gsResult testMethods |

	testClassesPresenter hasSelection ifFalse: [status value: self. ^testCasesPresenter model: ListModel  new].

	gsResult := model serverPerform: #getTestMethodsNamesOf: with: testClassesPresenter selection.
	testMethods := (gsResult  subStrings: ',') collect: [:each | GsTestMethod name: each].

	testCasesPresenter model: (ListModel on: testMethods).

	(testCounter second includes: testClassesPresenter selection)
	ifFalse: [testCounter second add: testClassesPresenter selection.
			testCounter at: 1 put: ((testCounter at: 1) +  testCasesPresenter model size)].
	status value: nil.
	status value: self.
!

onPackageChanged
	| gsResult |

	testCounter := Array with: 0 with: OrderedCollection new.

	packagesPresenter hasSelection ifFalse: [testClassesPresenter model: ListModel new. testCasesPresenter model: ListModel  new. ^status value: self.].

	gsResult := model serverPerform: #getTestCaseClassesNamesOf: with: packagesPresenter selection.

	testClassesPresenter model: (ListModel on: (gsResult subStrings: ',') asSortedCollection).
	status value: nil.
	status value: self.!

onViewOpened
	| gsResult packagesNames |

	testCounter := Array with: 0 with: OrderedCollection new.
	super onViewOpened.

 	status := ValueHolder new.

	gsResult := model serverPerform: #getPackagesNamesWithTestClasses.
	packagesNames := gsResult subStrings: ','.

	packagesPresenter model: (ListModel on: packagesNames asSortedCollection).

	(self view viewNamed: 'statusPackages') model: status.
	(self view viewNamed: 'statusTestsClasses') model: status.
	(self view viewNamed: 'statusTestCases') model: status.
	(self view viewNamed: 'statusTestCounter') model: status.!

packagesSize

	^'Packages: ', packagesPresenter model size printString!

runAll

	self runTests: testCasesPresenter model in: testClassesPresenter selection.!

runSelected

	self runTests: testCasesPresenter selection in: testClassesPresenter selection.
!

status
	^status!

status: anObject
	status := anObject!

testCasesSize

	^'Test Cases:', testCasesPresenter model size printString!

testClassesSize

	^'Test Classes:', testClassesPresenter model size printString! !
!JadeSUnitPresenter categoriesFor: #createComponents!public! !
!JadeSUnitPresenter categoriesFor: #createSchematicWiring!public! !
!JadeSUnitPresenter categoriesFor: #debug!public! !
!JadeSUnitPresenter categoriesFor: #getTestCounter!public! !
!JadeSUnitPresenter categoriesFor: #model:!public! !
!JadeSUnitPresenter categoriesFor: #onClassesChanged!public! !
!JadeSUnitPresenter categoriesFor: #onPackageChanged!public! !
!JadeSUnitPresenter categoriesFor: #onViewOpened!public! !
!JadeSUnitPresenter categoriesFor: #packagesSize!public! !
!JadeSUnitPresenter categoriesFor: #runAll!public! !
!JadeSUnitPresenter categoriesFor: #runSelected!public! !
!JadeSUnitPresenter categoriesFor: #status!accessing!private! !
!JadeSUnitPresenter categoriesFor: #status:!accessing!private! !
!JadeSUnitPresenter categoriesFor: #testCasesSize!public! !
!JadeSUnitPresenter categoriesFor: #testClassesSize!public! !

!JadeSUnitPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ContainerView) 98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 432 98 15 0 416 98 2 8 1140850688 131073 528 0 0 0 5 0 0 0 528 1180166 ##(Smalltalk.ProportionalLayout) 234 240 98 0 16 234 256 98 2 410 8 ##(Smalltalk.StaticText) 98 16 0 528 98 2 8 1140850944 1 688 0 721158 ##(Smalltalk.SystemColor) 31 0 5 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 1 2 1 34 83 121 115 116 101 109 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 688 0 8 4294904227 852486 ##(Smalltalk.NullConverter) 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 1 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 882 1 55 882 1241 57 688 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 0 0 0 108 2 0 0 55 0 0 0] 98 0 882 193 193 0 27 8 'textResult' 0 962 202 208 98 1 1026 1056 98 2 882 1 1 882 1241 111 528 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 55 0 0 0] 98 2 410 432 98 15 0 528 98 2 8 1140850688 131073 1376 0 770 31 0 5 0 0 0 1376 498 1 1 0 0 410 432 98 15 0 1376 98 2 8 1140850688 131073 1472 0 0 0 5 0 0 0 1472 594 234 240 640 32 234 256 640 0 962 202 208 98 1 1026 1056 98 2 882 1021 1 882 221 55 1472 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 254 1 0 0 0 0 0 0 108 2 0 0 27 0 0 0] 98 1 410 8 ##(Smalltalk.SUnitToolbar) 98 25 0 1472 98 2 8 1140853580 131137 1744 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 517 0 0 0 1744 1826 1856 8 4294904853 0 234 256 98 8 55167 853766 ##(Smalltalk.ToolbarButton) 55167 0 1744 1 1180998 4 ##(Smalltalk.CommandDescription) 8 #runAll 8 'Run All' 1 1 0 395334 3 ##(Smalltalk.Bitmap) 0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 0 7 882 1857 33 51 55169 1938 55169 0 1744 1 1970 8 #runSelected 8 'Run Selected' 1 1 0 2048 83 55171 1938 55171 0 1744 1 1970 8 #debug 8 'Debug' 1 1 0 2048 15 55173 1246982 ##(Smalltalk.ToolbarSystemButton) 55173 0 1744 1 1970 8 #reset 8 'Reset' 1 1 0 1 9 98 6 1952 2176 1050118 ##(Smalltalk.ToolbarSeparator) 0 0 1744 3 0 1 2240 2402 0 0 1744 3 0 1 2320 234 240 98 4 1 117 2048 1 0 1 0 882 33 33 882 45 45 0 0 962 202 208 98 2 1026 1056 98 2 882 1 1 882 221 55 1744 1026 8 #updateSizePosted 640 1744 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 110 0 0 0 27 0 0 0] 98 0 1184 0 27 1184 0 27 0 410 432 98 15 0 1376 98 2 8 1140850688 131073 2704 0 770 31 0 5 0 0 0 2704 594 234 240 640 32 234 256 640 0 962 202 208 98 1 1026 1056 98 2 882 1 1 882 1021 55 2704 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 1 0 0 27 0 0 0] 98 1 410 8 ##(Smalltalk.Toolbar) 98 25 0 2704 98 2 8 1409288972 131137 2992 0 1826 8 4278190080 0 517 0 802 0 16 834 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 882 193 193 0 2992 1826 3088 8 4294904853 234 256 640 234 256 98 4 55165 1938 55165 0 2992 1 1970 8 #commitTransaction 8 'Commit Transaction' 1 1 0 2048 27 55163 1938 55163 0 2992 1 1970 8 #abortTransaction 8 'Abort Transaction' 1 1 0 2048 1 98 2 3312 3248 234 240 98 2 2048 1 0 1 0 882 33 33 882 45 45 0 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 962 202 208 98 2 1026 1056 98 2 882 1 1 882 1021 55 2992 1026 2640 640 2992 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 1 0 0 27 0 0 0] 98 0 1184 0 27 1184 0 27 234 256 640 0 962 202 208 98 1 1026 1056 98 2 882 1 1 882 1241 55 1376 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 27 0 0 0] 98 2 2704 1472 1184 0 27 688 1184 0 27 410 432 98 15 0 416 98 2 8 1140850688 131073 3840 0 770 31 0 5 0 0 0 3840 594 234 240 640 32 234 256 640 0 962 202 208 98 1 1026 1056 98 2 882 1 811 882 1241 61 3840 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 149 1 0 0 108 2 0 0 179 1 0 0] 98 1 410 8 ##(Smalltalk.StatusBar) 98 18 0 3840 98 2 8 1409288460 1 4128 0 1826 8 4278190080 0 5 0 802 0 16 834 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 882 193 193 0 4128 0 8 4294903785 234 256 98 8 853766 ##(Smalltalk.StatusBarItem) 1 401 4128 0 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 2 1 8 ##(Smalltalk.Message) 8 'doIt' 8 '[:each | each packagesSize]' 8 #[30 105 226 0 106] 8 #packagesSize 4400 7 257 0 4450 8 #iconImageIndex 98 0 1049926 1 ##(Smalltalk.IconImageManager) 8 'statusPackages' 4354 1 401 4128 0 4386 0 0 4418 2 1 4448 8 'doIt' 8 '[:each | each testClassesSize]' 8 #[30 105 226 0 106] 8 #testClassesSize 4640 7 257 0 4386 0 0 918822 ##(Smalltalk.CompiledMethod) 2 3 4352 8 #defaultGetImageBlock 405994851 8 #[30 105 226 0 106] 4544 4736 7 257 0 0 8 'statusTestsClasses' 4354 1 401 4128 0 4386 0 0 4418 2 1 4448 8 'doIt' 8 '[:each | each getTestCounter]' 8 #[30 105 226 0 106] 8 #getTestCounter 4848 7 257 0 4386 0 0 4754 2 3 4352 4784 405994851 8 #[30 105 226 0 106] 4544 4944 7 257 0 0 8 'statusTestCounter' 4354 1 401 4128 0 4386 0 0 4418 2 1 4448 8 'doIt' 8 '[:each | each testCasesSize]' 8 #[30 105 226 0 106] 8 #testCasesSize 5024 7 257 0 4736 0 8 'statusTestCases' 98 4 4368 4624 5008 4832 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 4128 0 594 234 240 640 32 962 202 208 98 1 1026 1056 98 2 882 1 1 882 1241 61 4128 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 30 0 0 0] 98 0 1184 0 27 1184 0 27 0 0 410 432 98 15 0 416 98 2 8 1140850688 131073 5376 0 0 0 5 0 0 0 5376 594 234 240 640 32 234 256 98 6 410 8 ##(Smalltalk.MultipleSelectionListView) 98 30 0 5376 98 2 8 1140920393 1025 5504 590662 2 ##(Smalltalk.ListModel) 202 208 640 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 1826 8 4278190080 0 5 265030 4 ##(Smalltalk.Menu) 0 16 98 6 984134 2 ##(Smalltalk.CommandMenuItem) 1 1970 2000 8 'Run All' 9347 1 0 0 0 5746 1 1970 2208 8 'Run Selected' 9383 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 5746 1 1970 2272 8 'Debug' 9349 1 0 0 0 5858 4097 5746 1 1970 2352 8 'Reset' 9381 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 5504 0 8 4294904651 4450 8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract) 4592 0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Test Cases' 401 8 #left 4450 8 #methodName 98 0 4386 0 0 4418 2 1 170 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a methodName <= b methodName]' 8 #[30 105 226 0 226 32 130 106] 6208 6240 7 513 0 0 0 5504 0 3 0 0 8 #report 640 0 131169 0 98 1 0 962 202 208 98 3 1026 1056 98 2 882 833 1 882 409 701 5504 1026 8 #contextMenu: 98 1 5712 5504 1026 8 #text: 98 1 8 'Test Cases' 5504 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 1 0 0 0 0 0 0 108 2 0 0 94 1 0 0] 98 0 1184 0 27 8 'testCases' 410 8 ##(Smalltalk.ListView) 98 30 0 5376 98 2 8 1409355853 1025 6688 5586 202 208 640 0 5648 1826 8 4278190080 0 5 0 0 0 6688 0 8 4294904651 4450 6048 98 0 6080 4592 0 0 0 0 0 0 202 208 98 1 6130 8 'Test Classes' 399 6176 4450 6048 6864 8 ##(Smalltalk.SortedCollection) 0 0 6688 0 3 0 0 6368 640 0 131169 0 98 4 0 0 882 1 1 0 962 202 208 98 2 1026 1056 98 2 882 417 1 882 407 701 6688 1026 6576 98 1 8 'Test Classes' 6688 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 208 0 0 0 0 0 0 0 155 1 0 0 94 1 0 0] 98 0 1184 0 27 8 'testClasses' 410 6704 98 30 0 5376 98 2 8 1409355853 1025 7232 5586 202 208 640 0 5648 1826 6816 0 5 0 0 0 7232 0 8 4294904651 4450 6048 98 0 6080 4592 0 0 0 0 0 0 202 208 98 1 6130 8 'GemStone Packages' 399 6176 4450 6048 7376 6960 0 0 7232 0 3 0 0 6368 640 0 131169 0 98 4 0 0 882 1 1 0 962 202 208 98 2 1026 1056 98 2 882 1 1 882 407 701 7232 1026 6576 98 1 8 'GemStone Packages' 7232 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 203 0 0 0 94 1 0 0] 98 0 1184 0 27 8 'packages' 0 962 202 208 98 1 1026 1056 98 2 882 1 111 882 1241 701 5376 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 55 0 0 0 108 2 0 0 149 1 0 0] 98 5 7232 410 8 ##(Smalltalk.Splitter) 98 12 0 5376 98 2 8 1140850688 1 7888 0 1826 8 4278190080 0 517 0 0 0 7888 962 202 208 98 1 1026 1056 98 2 882 407 1 882 11 701 7888 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 203 0 0 0 0 0 0 0 208 0 0 0 94 1 0 0] 98 0 1184 0 27 6688 410 7904 98 12 0 5376 98 2 8 1140850688 1 8160 0 1826 7984 0 517 0 0 0 8160 962 202 208 98 1 1026 1056 98 2 882 823 1 882 11 701 8160 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 1 0 0 0 0 0 0 160 1 0 0 94 1 0 0] 98 0 1184 0 27 5504 1184 0 27 234 256 640 0 962 202 208 98 1 1026 1056 98 2 882 2719 21 882 1241 871 416 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 17 5 0 0 10 0 0 0 125 7 0 0 189 1 0 0] 98 3 528 5376 3840 1184 0 27 )! !
!JadeSUnitPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

