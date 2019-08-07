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

	^gciSession serverPerform: #debugTestNamed:of: with: testCasesPresenter selection last methodName with: testCasesPresenter selection last className!

getTestCounter

	^'Total Tests: ', testCounter first printString!

onClassesChanged
	| gsResult gsTestMethods |
	testClassesPresenter hasSelection
		ifFalse: 
			[status value: self.
			^testCasesPresenter model: ListModel new].

	gsTestMethods := OrderedCollection new.
	testClassesPresenter selections do: [:className | 
		gsResult := gciSession serverPerform: #getTestMethodsNamesOf: with: (gciSession encode: className).
		gsResult := gciSession decode: gsResult.
		gsTestMethods addAll: ((gsResult subStrings: ',') collect: [:each | GsTestMethod newOn: className methodName: each]).

	].
	testCasesPresenter model: (ListModel on: gsTestMethods).
	testCounter at: 2 put: (OrderedCollection withAll: testClassesPresenter selections).
	testCounter at: 1 put: testCasesPresenter model size.
	status value: nil.
	status value: self!

onPackageChanged
	| gsResult resultStream |

	testCounter := Array with: 0 with: OrderedCollection new.
	packagesPresenter hasSelection ifFalse: [testClassesPresenter model: ListModel new. testCasesPresenter model: ListModel  new. ^status value: self.].

	resultStream := WriteStream on: String new.
	packagesPresenter selections do: [:packageName | 
		resultStream nextPutAll: (gciSession serverPerform: #getTestCaseClassesNamesOf: with: (gciSession encode: packageName))].
	
	testClassesPresenter model: (ListModel on: (resultStream contents subStrings: ',') asSortedCollection).
	status value: nil.
	status value: self.!

onViewOpened
	| gsResult packagesNames |

	testCounter := Array with: 0 with: OrderedCollection new.
	super onViewOpened.

 	status := ValueHolder new.

	gsResult := gciSession serverPerform: #getPackagesNamesWithTestClasses.
	gsResult := gciSession decode: gsResult.
	packagesNames := gsResult subStrings: ','.

	packagesPresenter model: (ListModel on: packagesNames asSortedCollection).

	(self view viewNamed: 'statusPackages') model: status.
	(self view viewNamed: 'statusTestsClasses') model: status.
	(self view viewNamed: 'statusTestCases') model: status.
	(self view viewNamed: 'statusTestCounter') model: status.!

packagesSize

	^'Packages: ', packagesPresenter model size printString!

runAll
	| result |

	result := JadeTestResult new.
	testClassesPresenter selections do: 
			[:className |
			self
				runTests: (testCasesPresenter model select: [:each | each className = className])
				in: className
				result: result ]!

runSelected
	| result |
	result := JadeTestResult new.
	testClassesPresenter selections do: 
			[:className |
			self
				runTests: (testCasesPresenter model
						select: [:each | each className = className and: [testCasesPresenter selections includes: each]])
				in: className
				result: result]!

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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 512 nil nil nil 5 nil nil nil 512 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 34 2 410 ##(Smalltalk.StaticText) 34 16 nil 512 34 2 8 1140850944 1 672 nil 721158 ##(Smalltalk.SystemColor) 31 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 1 2 1 34 83 121 115 116 101 109 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 nil 672 nil 8 1921133906 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 850 1 55 850 1241 57 672 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 0 0 0 108 2 0 0 55 0 0 0] 8 #() 850 193 193 nil 27 8 'textResult' nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1241 111 512 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 55 0 0 0] 34 2 410 ##(Smalltalk.ContainerView) 34 15 nil 512 34 2 8 1140850688 131073 1328 nil 752 nil 5 nil nil nil 1328 482 1 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 1328 34 2 8 1140850688 131073 1408 nil nil nil 5 nil nil nil 1408 578 170 176 624 false 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1021 1 850 221 55 1408 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 254 1 0 0 0 0 0 0 108 2 0 0 27 0 0 0] 34 1 410 ##(Smalltalk.Toolbar) 34 25 nil 1408 34 2 8 1140853580 131137 1680 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 517 nil nil nil 1680 1760 8 1921323185 nil 170 192 34 8 11815 1246790 1 ##(Smalltalk.ToolbarSystemButton) 1680 1 1180998 4 ##(Smalltalk.CommandDescription) #reset 8 'Reset' 1 1 nil 11815 1 9 11809 1246790 1 ##(Smalltalk.ToolbarBitmapButton) 1680 1 1874 #runAll 8 'Run All' 1 1 nil 11809 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 850 1857 33 51 11811 1922 1680 1 1874 #runSelected 8 'Run Selected' 1 1 nil 11811 2000 83 11813 1922 1680 1 1874 #debug 8 'Debug' 1 1 nil 11813 2000 15 34 6 1936 2128 1049158 1 ##(Smalltalk.ToolbarSeparator) 1680 1 2176 2242 1680 1 1856 nil nil 1 nil 850 33 33 850 45 45 nil nil 930 138 144 34 2 994 #createAt:extent: 34 2 850 1 1 850 221 55 1680 994 #updateSizePosted 624 1680 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 110 0 0 0 27 0 0 0] 8 #() 1136 nil 27 1136 nil 27 nil 410 ##(Smalltalk.ContainerView) 34 15 nil 1328 34 2 8 1140850688 131073 2496 nil 752 nil 5 nil nil nil 2496 578 170 176 624 false 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1021 55 2496 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 1 0 0 27 0 0 0] 34 1 410 ##(Smalltalk.Toolbar) 34 25 nil 2496 34 2 8 1409288972 131137 2768 nil 1760 nil 517 nil 770 nil true 802 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 850 193 193 nil 2768 1760 8 1921323185 170 192 624 170 192 34 4 11805 1922 2768 1 1874 #abortTransaction 8 'Abort Transaction' 1 1 nil 11805 2000 1 11807 1922 2768 1 1874 #commitTransaction 8 'Commit Transaction' 1 1 nil 11807 2000 27 34 2 2960 3008 nil nil 1 nil 850 33 33 850 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 930 138 144 34 2 994 #createAt:extent: 34 2 850 1 1 850 1021 55 2768 994 #updateSizePosted 624 2768 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 1 0 0 27 0 0 0] 8 #() 1136 nil 27 1136 nil 27 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1241 55 1328 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 27 0 0 0] 34 2 2496 1408 1136 nil 27 672 1136 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 3488 nil 752 nil 5 nil nil nil 3488 578 170 176 624 false 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 811 850 1241 61 3488 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 149 1 0 0 108 2 0 0 179 1 0 0] 34 1 410 ##(Smalltalk.StatusBar) 34 18 nil 3488 34 2 8 1409288460 1 3760 nil 1760 nil 5 nil 770 nil true 802 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 850 193 193 nil 3760 nil 8 1921162360 170 192 34 8 853766 ##(Smalltalk.StatusBarItem) 1 401 3760 nil 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.Message) 8 'doIt' 8 '[:each | each testCasesSize]' 8 #[30 105 226 0 106] #testCasesSize 3984 7 257 nil 3970 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 3936 #defaultGetImageBlock 405994851 8 #[30 105 226 0 106] #iconImageIndex 4080 7 257 nil nil 8 'statusTestCases' 3938 1 401 3760 nil 3970 0 nil 4002 2 1 ##(Smalltalk.Message) 8 'doIt' 8 '[:each | each testClassesSize]' 8 #[30 105 226 0 106] #testClassesSize 4176 7 257 nil 4080 nil 8 'statusTestsClasses' 3938 1 401 3760 nil 3970 0 nil 4002 2 1 ##(Smalltalk.Message) 8 'doIt' 8 '[:each | each getTestCounter]' 8 #[30 105 226 0 106] #getTestCounter 4288 7 257 nil 3970 0 nil 4098 2 3 3936 #defaultGetImageBlock 405994851 8 #[30 105 226 0 106] #iconImageIndex 4368 7 257 nil nil 8 'statusTestCounter' 3938 1 401 3760 nil 3970 0 nil 4002 2 1 ##(Smalltalk.Message) 8 'doIt' 8 '[:each | each packagesSize]' 8 #[30 105 226 0 106] #packagesSize 4448 7 257 nil 459270 ##(Smalltalk.Message) #iconImageIndex 8 #() 1049926 1 ##(Smalltalk.IconImageManager) 8 'statusPackages' 34 4 4432 4160 3952 4272 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 3760 nil 578 170 176 624 false 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1241 61 3760 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 30 0 0 0] 8 #() 1136 nil 27 1136 nil 27 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 4864 nil nil nil 5 nil nil nil 4864 578 170 176 624 false 170 192 34 6 410 ##(Smalltalk.ListView) 34 30 nil 4864 34 2 8 1409355849 1025 4992 590662 2 ##(Smalltalk.ListModel) 138 144 624 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 1760 nil 5 nil nil nil 4992 nil 8 1921377647 4530 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 4592 nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'GemStone Packages' 399 #left 4530 #displayString 5168 ##(Smalltalk.SortedCollection) nil nil 4992 nil 3 nil nil #report 624 nil 131169 nil 34 4 nil nil 850 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 850 1 1 850 407 701 4992 994 #text: 34 1 8 'GemStone Packages' 4992 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 203 0 0 0 94 1 0 0] 8 #() 1136 nil 27 8 'packages' 410 ##(Smalltalk.MultipleSelectionListView) 34 30 nil 4864 34 2 8 1140920393 1025 5536 5058 138 144 624 nil 5120 1760 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 6 984134 2 ##(Smalltalk.CommandMenuItem) 1 1874 #runAll 8 'Run All' 9347 1 nil nil nil 5682 1 1874 #runSelected 8 'Run Selected' 9383 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 5682 1 1874 #debug 8 'Debug' 9349 1 nil nil nil 5794 4097 5682 1 1874 #reset 8 'Reset' 9381 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 5536 nil 8 1921377647 4530 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 4592 nil nil nil nil nil nil 138 144 34 1 5218 8 'Test Cases' 401 #left 4530 #methodName 8 #() 3970 0 nil 4002 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a methodName <= b methodName]' 8 #[30 105 226 0 226 32 130 106] #methodName 6096 7 513 nil nil nil 5536 nil 3 nil nil #report 624 nil 131169 nil 8 #(nil) 930 138 144 34 3 994 #createAt:extent: 34 2 850 833 1 850 409 701 5536 994 #contextMenu: 34 1 5648 5536 994 #text: 34 1 8 'Test Cases' 5536 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 1 0 0 0 0 0 0 108 2 0 0 94 1 0 0] 8 #() 1136 nil 27 8 'testCases' 410 ##(Smalltalk.ListView) 34 30 nil 4864 34 2 8 1409355849 1025 6496 5058 138 144 624 nil 5120 1760 nil 5 nil nil nil 6496 nil 8 1921377647 4530 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 4592 nil nil nil nil nil nil 138 144 34 1 5218 8 'Test Classes' 399 #left 4530 #displayString 6624 ##(Smalltalk.SortedCollection) nil nil 6496 nil 3 nil nil #report 624 nil 131169 nil 34 4 nil nil 850 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 850 417 1 850 407 701 6496 994 #text: 34 1 8 'Test Classes' 6496 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 208 0 0 0 0 0 0 0 155 1 0 0 94 1 0 0] 8 #() 1136 nil 27 8 'testClasses' nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 111 850 1241 701 4864 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 55 0 0 0 108 2 0 0 149 1 0 0] 34 5 4992 410 ##(Smalltalk.Splitter) 34 12 nil 4864 34 2 8 1140850688 1 7136 nil 1760 nil 517 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 7136 nil 1 #left nil nil nil 850 1 1 850 9 9 nil 7232 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 407 1 850 11 701 7136 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 203 0 0 0 0 0 0 0 208 0 0 0 94 1 0 0] 8 #() 1136 nil 27 6496 410 ##(Smalltalk.Splitter) 34 12 nil 4864 34 2 8 1140850688 1 7424 nil 1760 nil 517 nil nil nil 7202 7424 nil 1 #left nil nil nil 7232 7248 nil 7232 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 823 1 850 11 701 7424 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 1 0 0 0 0 0 0 160 1 0 0 94 1 0 0] 8 #() 1136 nil 27 5536 1136 nil 27 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1493 21 850 1241 871 416 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 234 2 0 0 10 0 0 0 86 5 0 0 189 1 0 0] 34 3 512 4864 3488 1136 nil 27 )! !
!JadeSUnitPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

