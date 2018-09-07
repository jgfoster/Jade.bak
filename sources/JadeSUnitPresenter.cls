"Filed out from Dolphin Smalltalk 7"!

JadeSUnitAbstractPresenter subclass: #JadeSUnitPresenter
	instanceVariableNames: 'packageListPresenter classListPresenter status testCounter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSUnitPresenter guid: (GUID fromString: '{f0d7615e-1a5a-4d8a-b979-c2ad927a9f74}')!
JadeSUnitPresenter comment: ''!
!JadeSUnitPresenter categoriesForClass!Unclassified! !
!JadeSUnitPresenter methodsFor!

createComponents
	super createComponents.
	packageListPresenter := self add: ListPresenter new name: 'packages'.
	classListPresenter := self add: ListPresenter new name: 'testClasses'!

createSchematicWiring
	super createSchematicWiring.
	packageListPresenter
		when: #selectionChanged
		send: #onPackageChanged
		to: self.
	classListPresenter
		when: #selectionChanged
		send: #onClassesChanged
		to: self!

debug
	classListPresenter hasSelection ifFalse: [^self].
	methodListPresenter selection size = 1
		ifFalse: [^MessageBox notify: 'Select only one TestCase to debug'].
	^gciSession
		serverPerform: #debugTestNamed:of:
		with: methodListPresenter selection last selector
		with: classListPresenter selection name!

getTestCounter

	^'Total Tests: ', testCounter first printString!

onClassesChanged
	self updateMethods.
	status value: nil.
	status value: self.
!

onPackageChanged
	Cursor wait showWhile: 
			[self updateClasses.
			status value: nil.
			status value: self]!

packagesSize
	^'Packages: ' , packageListPresenter model size printString!

runAll
	self runTests: methodListPresenter model in: classListPresenter selection name!

runSelected
	self runTests: methodListPresenter selection in: classListPresenter selection name!

status
	^status!

status: anObject
	status := anObject!

testCasesSize
	^'Test Cases:' , methodListPresenter model size printString!

testClassesSize
	^'Test Classes:' , classListPresenter model size printString! !
!JadeSUnitPresenter categoriesFor: #createComponents!public! !
!JadeSUnitPresenter categoriesFor: #createSchematicWiring!public! !
!JadeSUnitPresenter categoriesFor: #debug!public! !
!JadeSUnitPresenter categoriesFor: #getTestCounter!public! !
!JadeSUnitPresenter categoriesFor: #onClassesChanged!event handlers!public! !
!JadeSUnitPresenter categoriesFor: #onPackageChanged!event handlers!public! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 512 nil nil nil 5 nil nil nil 512 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 34 2 410 ##(Smalltalk.StaticText) 34 16 nil 512 34 2 8 1140850944 1 672 nil 721158 ##(Smalltalk.SystemColor) 31 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 1 2 1 34 83 121 115 116 101 109 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 nil 672 nil 8 1695245616 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 850 1 55 850 1241 57 672 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 0 0 0 108 2 0 0 55 0 0 0] 8 #() 850 193 193 nil 27 8 'textResult' nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1241 111 512 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 55 0 0 0] 34 2 410 ##(Smalltalk.ContainerView) 34 15 nil 512 34 2 8 1140850688 131073 1328 nil 738 31 nil 5 nil nil nil 1328 482 1 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 1328 34 2 8 1140850688 131073 1424 nil nil nil 5 nil nil nil 1424 578 170 176 624 false 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1021 1 850 221 55 1424 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 254 1 0 0 0 0 0 0 108 2 0 0 27 0 0 0] 34 1 410 ##(Smalltalk.SUnitToolbar) 34 25 nil 1424 34 2 8 1140853516 131137 1696 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 517 nil nil nil 1696 1762 1792 8 4294907491 nil 170 192 34 8 38293 853766 ##(Smalltalk.ToolbarButton) 38293 nil 1696 1 1180998 4 ##(Smalltalk.CommandDescription) #runAll 8 'Run All' 1 1 nil 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 850 1857 33 51 38295 1874 38295 nil 1696 1 1906 #runSelected 8 'Run Selected' 1 1 nil 1968 83 38297 1874 38297 nil 1696 1 1906 #debug 8 'Debug' 1 1 nil 1968 15 38299 1246982 ##(Smalltalk.ToolbarSystemButton) 38299 nil 1696 1 1906 #reset 8 'Reset' 1 1 nil 1 9 34 6 1888 2096 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 1696 3 nil 1 2144 2274 nil nil 1696 3 nil 1 2208 170 176 34 4 1 117 1968 1 nil 1 nil 850 33 33 850 45 45 nil nil 930 138 144 34 2 994 #createAt:extent: 34 2 850 1 1 850 221 55 1696 994 #updateSizePosted 624 1696 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 110 0 0 0 27 0 0 0] 8 #() 1136 nil 27 1136 nil 27 nil 410 ##(Smalltalk.ContainerView) 34 15 nil 1328 34 2 8 1140850688 131073 2560 nil 738 31 nil 5 nil nil nil 2560 578 170 176 624 false 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1021 55 2560 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 1 0 0 27 0 0 0] 34 1 410 ##(Smalltalk.Toolbar) 34 25 nil 2560 34 2 8 1409288972 131137 2848 nil 1762 8 4278190080 nil 517 nil 770 nil true 802 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 850 193 193 nil 2848 1762 2928 8 4294907491 170 192 624 170 192 34 4 38291 1874 38291 nil 2848 1 1906 #commitTransaction 8 'Commit Transaction' 1 1 nil 1968 27 38289 1874 38289 nil 2848 1 1906 #abortTransaction 8 'Abort Transaction' 1 1 nil 1968 1 34 2 3136 3088 170 176 34 2 1968 1 nil 1 nil 850 33 33 850 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 930 138 144 34 2 994 #createAt:extent: 34 2 850 1 1 850 1021 55 2848 994 #updateSizePosted 624 2848 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 1 0 0 27 0 0 0] 8 #() 1136 nil 27 1136 nil 27 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1241 55 1328 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 27 0 0 0] 34 2 2560 1424 1136 nil 27 672 1136 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 3648 nil 738 31 nil 5 nil nil nil 3648 578 170 176 624 false 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 811 850 1241 61 3648 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 149 1 0 0 108 2 0 0 179 1 0 0] 34 1 410 ##(Smalltalk.StatusBar) 34 18 nil 3648 34 2 8 1409288460 1 3936 nil 1762 8 4278190080 nil 5 nil 770 nil true 802 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 850 193 193 nil 3936 nil 8 4294906937 170 192 34 8 853766 ##(Smalltalk.StatusBarItem) 1 401 3936 nil 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.Message) 8 'doIt' 8 '[:each | each packagesSize]' 8 #[30 105 226 0 106] #packagesSize 4192 7 257 nil 459270 ##(Smalltalk.Message) #iconImageIndex 8 #() 1049926 1 ##(Smalltalk.IconImageManager) 8 'statusPackages' 4146 1 401 3936 nil 4178 0 nil 4210 2 1 4288 8 'doIt' 8 '[:each | each testCasesSize]' 8 #[30 105 226 0 106] #testCasesSize 4400 7 257 nil 4178 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 4144 #defaultGetImageBlock 405994851 8 #[30 105 226 0 106] #iconImageIndex 4480 7 257 nil nil 8 'statusTestCases' 4146 1 401 3936 nil 4178 0 nil 4210 2 1 4288 8 'doIt' 8 '[:each | each testClassesSize]' 8 #[30 105 226 0 106] #testClassesSize 4576 7 257 nil 4480 nil 8 'statusTestsClasses' 4146 1 401 3936 nil 4178 0 nil 4210 2 1 4288 8 'doIt' 8 '[:each | each getTestCounter]' 8 #[30 105 226 0 106] #getTestCounter 4688 7 257 nil 4178 0 nil 4498 2 3 4144 #defaultGetImageBlock 405994851 8 #[30 105 226 0 106] #iconImageIndex 4768 7 257 nil nil 8 'statusTestCounter' 34 4 4160 4560 4384 4672 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 3936 nil 578 170 176 624 false 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1241 61 3936 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 30 0 0 0] 8 #() 1136 nil 27 1136 nil 27 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 5072 nil nil nil 5 nil nil nil 5072 578 170 176 624 false 170 192 34 6 410 ##(Smalltalk.ListView) 34 30 nil 5072 34 2 8 1409388621 1025 5200 590662 2 ##(Smalltalk.ListModel) 138 144 624 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 1762 8 4278190080 nil 5 nil nil nil 5200 nil 8 4294907417 4290 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 4352 nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Packages' 399 #left 4178 0 nil 4210 2 1 4288 8 'doIt' 8 '[:packageService | packageService name]' 8 #[30 105 226 0 106] #name 5504 7 257 nil ##(Smalltalk.SortedCollection) nil nil 5200 nil 3 nil nil #report 624 nil 131169 nil 34 4 nil nil 850 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 850 1 1 850 407 701 5200 994 #text: 34 1 8 'Packages' 5200 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 203 0 0 0 94 1 0 0] 8 #() 1136 nil 27 8 'packages' 410 ##(Smalltalk.MultipleSelectionListView) 34 30 nil 5072 34 2 8 1140953161 1025 5840 5266 138 144 624 nil 5328 1762 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 6 984134 2 ##(Smalltalk.CommandMenuItem) 1 1906 #runAll 8 'Run All' 9347 1 nil nil nil 6018 1 1906 #runSelected 8 'Run Selected' 9383 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 6018 1 1906 #debug 8 'Debug' 9349 1 nil nil nil 6130 4097 6018 1 1906 #reset 8 'Reset' 9381 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 5840 nil 8 4294907417 4290 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 4352 nil nil nil nil nil nil 138 144 34 1 5458 8 'Methods' 401 #left 4178 0 nil 4210 2 1 4288 8 'doIt' 8 '[:methodService | methodService name]' 8 #[30 105 226 0 106] #name 6400 7 257 nil 4178 0 nil 4210 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a methodName <= b methodName]' 8 #[30 105 226 0 226 32 130 106] #methodName 6480 7 513 nil nil nil 5840 nil 3 nil nil #report 624 850 65 65 131169 nil 34 4 nil nil 850 1 1 nil 930 138 144 34 3 994 #createAt:extent: 34 2 850 833 1 850 409 701 5840 994 #contextMenu: 34 1 5984 5840 994 #text: 34 1 8 'Methods' 5840 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 1 0 0 0 0 0 0 108 2 0 0 94 1 0 0] 8 #() 1136 nil 27 8 'testCases' 410 ##(Smalltalk.ListView) 34 30 nil 5072 34 2 8 1409388621 1025 6912 5266 138 144 624 nil 5328 1762 5360 nil 5 nil nil nil 6912 nil 8 4294907417 4290 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 4352 nil nil nil nil nil nil 138 144 34 1 5458 8 'Classes' 399 #left 4178 0 nil 4210 2 1 4288 8 'doIt' 8 '[:classService | classService name]' 8 #[30 105 226 0 106] #name 7136 7 257 nil ##(Smalltalk.SortedCollection) nil nil 6912 nil 3 nil nil #report 624 nil 131169 nil 34 4 nil nil 850 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 850 417 1 850 407 701 6912 994 #text: 34 1 8 'Classes' 6912 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 208 0 0 0 0 0 0 0 155 1 0 0 94 1 0 0] 8 #() 1136 nil 27 8 'testClasses' nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 111 850 1241 701 5072 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 55 0 0 0 108 2 0 0 149 1 0 0] 34 5 5200 410 ##(Smalltalk.Splitter) 34 12 nil 5072 34 2 8 1140850688 1 7632 nil 1762 8 4278190080 nil 517 nil nil nil 7632 930 138 144 34 1 994 #createAt:extent: 34 2 850 407 1 850 11 701 7632 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 203 0 0 0 0 0 0 0 208 0 0 0 94 1 0 0] 8 #() 1136 nil 27 6912 410 ##(Smalltalk.Splitter) 34 12 nil 5072 34 2 8 1140850688 1 7888 nil 1762 7712 nil 517 nil nil nil 7888 930 138 144 34 1 994 #createAt:extent: 34 2 850 823 1 850 11 701 7888 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 1 0 0 0 0 0 0 160 1 0 0 94 1 0 0] 8 #() 1136 nil 27 5840 1136 nil 27 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 7679 21 850 1241 871 416 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 107 17 0 0 189 1 0 0] 34 3 512 5072 3648 1136 nil 27 )! !
!JadeSUnitPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

