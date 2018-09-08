"Filed out from Dolphin Smalltalk 7"!

JadeSUnitAbstractPresenter subclass: #JadeSUnitInClassPresenter
	instanceVariableNames: 'testClassService'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSUnitInClassPresenter guid: (GUID fromString: '{a1eac98a-259a-424e-bfe5-973e32a9b4c6}')!
JadeSUnitInClassPresenter comment: ''!
!JadeSUnitInClassPresenter categoriesForClass!Unclassified! !
!JadeSUnitInClassPresenter methodsFor!

debug
	testClassService ifNil: [^self].
	methodListPresenter selection size = 1
		ifFalse: [^MessageBox notify: 'Select only one TestCase to debug'].
	^gciSession
		serverPerform: #debugTestNamed:of:
		with: methodListPresenter selection last methodName
		with: testClassService name!

runAll
	self runTests: methodListPresenter model in: testClassService name!

runAndMesureTime
	| millisecondsToRun  timeMessage time |

	"(testCasesPresenter selection size = 1) ifFalse: [^MessageBox notify: 'Select only one Test' ]."

	timeMessage := WriteStream on: String new.
	timeMessage nextPutAll: 'Total Time'.
	millisecondsToRun := Time millisecondsToRun: [self runSelected].
	time := Time fromMilliseconds: millisecondsToRun.
	(time asSeconds = 0) 
	ifTrue: [timeMessage nextPutAll: 'milliseconds - ', millisecondsToRun printString]
	ifFalse: [timeMessage nextPutAll: String lineDelimiter,time minutes printString, ' minutes or'.
			timeMessage nextPutAll: String lineDelimiter, time asSeconds printString, ' seconds or'.
			timeMessage nextPutAll: String lineDelimiter, millisecondsToRun printString, ' milliseconds'.
			].

	MessageBox notify: timeMessage contents
!

runSelected
	self runTests: methodListPresenter selection in: testClassService name!

testClassService: classService
	| testMethodServices |
	classService = testClassService ifTrue: [^self].
	testClassService := classService.
	self reset.
	testClassService isNil
		ifTrue: 
			[methodListPresenter model: (ListModel on: ListModel new).
			^self].
	testMethodServices := classService methods select: [:methodService | methodService isTestMethod].
	methodListPresenter model: (ListModel on: testMethodServices)! !
!JadeSUnitInClassPresenter categoriesFor: #debug!public! !
!JadeSUnitInClassPresenter categoriesFor: #runAll!public! !
!JadeSUnitInClassPresenter categoriesFor: #runAndMesureTime!public! !
!JadeSUnitInClassPresenter categoriesFor: #runSelected!public! !
!JadeSUnitInClassPresenter categoriesFor: #testClassService:!accessing!public! !

!JadeSUnitInClassPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 nil 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 512 nil nil nil 5 nil nil nil 512 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 2 410 ##(Smalltalk.StaticText) 34 16 nil 512 34 2 8 1140850944 1 672 nil 721158 ##(Smalltalk.SystemColor) 31 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 1 2 1 34 83 121 115 116 101 109 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 nil 672 nil 8 1695245616 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 850 1 1 850 1241 71 672 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 35 0 0 0] 8 #() 850 193 193 nil 27 8 'textResult' nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 801 850 1241 71 512 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 144 1 0 0 108 2 0 0 179 1 0 0] 34 1 672 1136 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 1328 nil nil nil 5 nil nil nil 1328 578 170 176 624 true 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1181 1 850 61 801 1328 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 78 2 0 0 0 0 0 0 108 2 0 0 144 1 0 0] 34 1 410 ##(Smalltalk.ContainerView) 34 15 nil 1328 34 2 8 1140850688 131073 1600 nil 738 31 nil 5 nil nil nil 1600 578 170 176 624 false 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 61 801 1600 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 30 0 0 0 144 1 0 0] 34 1 410 ##(Smalltalk.ContainerView) 34 15 nil 1600 34 2 8 1140850688 131073 1888 nil nil nil 5 nil nil nil 1888 578 170 176 624 false 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 61 801 1888 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 30 0 0 0 144 1 0 0] 34 1 410 ##(Smalltalk.SUnitToolbar) 34 25 nil 1888 34 2 8 1140853708 131137 2160 nil 524550 ##(Smalltalk.ColorRef) 8 4278190080 nil 517 nil nil nil 2160 2226 2256 8 4294907491 nil 170 192 34 10 54591 853766 ##(Smalltalk.ToolbarButton) 54591 nil 2160 1 1180998 4 ##(Smalltalk.CommandDescription) #runAll 8 'Run All' 1 1 nil 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 850 1857 33 51 54593 2338 54593 nil 2160 1 2370 #runSelected 8 'Run Selected' 1 1 nil 2432 83 54595 1115910 ##(Smalltalk.ToolbarIconButton) 54595 nil 2160 1 2370 #runAndMesureTime 8 'Run and Mesure Time' 1 1 263494 3 ##(Smalltalk.Icon) nil true 2464 8 'Time.ico' 2512 2418 nil true nil nil nil nil 3 850 33 33 1 54597 2338 54597 nil 2160 1 2370 #debug 8 'Debug' 1 1 nil 2432 15 54599 1246982 ##(Smalltalk.ToolbarSystemButton) 54599 nil 2160 1 2370 #reset 8 'Reset' 1 1 nil 1 9 34 8 2352 2560 1050118 ##(Smalltalk.ToolbarSeparator) nil nil 2160 3 nil 1 2624 2882 nil nil 2160 3 nil 1 2752 2882 nil nil 2160 3 nil 1 2816 170 176 34 6 1 119 2432 1 2720 117 nil 1 nil 850 33 33 850 45 45 nil nil 930 138 144 34 2 994 #createAt:extent: 34 2 850 1 1 850 61 801 2160 994 #updateSizePosted 624 2160 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 30 0 0 0 144 1 0 0] 8 #() 1136 nil 27 1136 nil 27 1136 nil 27 1136 nil 27 nil 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 3184 nil nil nil 5 nil nil nil 3184 578 170 176 624 false 170 192 34 2 410 ##(Smalltalk.MultipleSelectionListView) 34 30 nil 3184 34 2 8 1140920393 1025 3312 590662 2 ##(Smalltalk.ListModel) 138 144 624 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 2226 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 8 984134 2 ##(Smalltalk.CommandMenuItem) 1 2370 #runAll 8 'Run All' 9347 1 nil nil nil 3538 1 2370 #runSelected 8 'Run Selected' 9383 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 3538 1 2370 #runAndMesureTime 8 'Run and Mesure Time' 1 1 nil nil nil 3650 4097 3538 1 2370 #debug 8 'Debug' 9349 1 nil nil nil 3650 4097 3538 1 2370 #reset 8 'Reset' 9381 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 3312 nil 8 4294907417 459270 ##(Smalltalk.Message) #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Test Cases' 1181 #left 3890 #name 624 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a methodName <= b methodName]' 8 #[30 105 226 0 226 32 130 106] #methodName 4080 7 513 nil nil nil 3312 nil 3 nil nil #report 624 nil 131169 nil 34 4 nil nil 850 1 1 nil 930 138 144 34 3 994 #createAt:extent: 34 2 850 1 1 850 1181 801 3312 994 #contextMenu: 34 1 3504 3312 994 #text: 34 1 8 'Test Cases' 3312 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 78 2 0 0 144 1 0 0] 8 #() 1136 nil 27 8 'testCases' nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1181 801 3184 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 78 2 0 0 144 1 0 0] 34 1 3312 1136 nil 27 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 7679 21 850 1241 871 416 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 0 0 10 0 0 0 107 17 0 0 189 1 0 0] 34 3 3184 1328 512 1136 nil 27 )! !
!JadeSUnitInClassPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

