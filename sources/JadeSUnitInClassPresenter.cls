"Filed out from Dolphin Smalltalk 7"!

JadeSUnitAbstractPresenter subclass: #JadeSUnitInClassPresenter
	instanceVariableNames: 'testClassName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSUnitInClassPresenter guid: (GUID fromString: '{A1EAC98A-259A-424E-BFE5-973E32A9B4C6}')!
JadeSUnitInClassPresenter comment: ''!
!JadeSUnitInClassPresenter categoriesForClass!Unclassified! !
!JadeSUnitInClassPresenter methodsFor!

debug

	testClassName ifNil: [^self].

	(testCasesPresenter selection size = 1) ifFalse: [^MessageBox  notify: 'Select only one TestCase to debug'].

	^parentPresenter gciSession serverPerform: #debugTestNamed:of: with: testCasesPresenter selection last methodName with: testClassName!

runAll

	self runTests: testCasesPresenter model in: testClassName!

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

	self runTests: testCasesPresenter selection in: testClassName!

testClassName
	^testClassName!

testClassName: anObject
	| tests |

	(anObject notNil and: [testClassName = anObject]) ifTrue: [^self].

	testClassName := anObject.

	self reset.

	[tests := ((self topShell gciSession serverPerform: #getTestMethodsNamesOf: with: testClassName) subStrings: ',') collect: [:each | GsTestMethod name: each]] 
	on: Error do: [:ex | ^ex]."fix error on system history navigation, sometime GS answer error #2106"

	testClassName 
	ifNotNil: [testCasesPresenter model: (ListModel on: tests asOrderedCollection)].! !
!JadeSUnitInClassPresenter categoriesFor: #debug!public! !
!JadeSUnitInClassPresenter categoriesFor: #runAll!public! !
!JadeSUnitInClassPresenter categoriesFor: #runAndMesureTime!public! !
!JadeSUnitInClassPresenter categoriesFor: #runSelected!public! !
!JadeSUnitInClassPresenter categoriesFor: #testClassName!accessing!private! !
!JadeSUnitInClassPresenter categoriesFor: #testClassName:!accessing!public! !

!JadeSUnitInClassPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ContainerView) 98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 788230 ##(Smalltalk.BorderLayout) 1 1 0 410 432 98 15 0 416 98 2 8 1140850688 131073 528 0 0 0 5 0 0 0 528 1180166 ##(Smalltalk.ProportionalLayout) 234 240 98 0 32 234 256 98 2 410 8 ##(Smalltalk.StaticText) 98 16 0 528 98 2 8 1140850944 1 688 0 721158 ##(Smalltalk.SystemColor) 31 0 5 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 1 2 1 34 83 121 115 116 101 109 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 0 688 0 8 4294903410 852486 ##(Smalltalk.NullConverter) 0 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 1 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 882 1 1 882 1241 71 688 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 35 0 0 0] 98 0 882 193 193 0 27 8 'textResult' 0 962 202 208 98 1 1026 1056 98 2 882 1 801 882 1241 71 528 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 144 1 0 0 108 2 0 0 179 1 0 0] 98 1 688 1184 0 27 410 432 98 15 0 416 98 2 8 1140850688 131073 1376 0 0 0 5 0 0 0 1376 594 234 240 640 16 234 256 640 0 962 202 208 98 1 1026 1056 98 2 882 1181 1 882 61 801 1376 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 78 2 0 0 0 0 0 0 108 2 0 0 144 1 0 0] 98 1 410 432 98 15 0 1376 98 2 8 1140850688 131073 1648 0 770 31 0 5 0 0 0 1648 594 234 240 640 32 234 256 640 0 962 202 208 98 1 1026 1056 98 2 882 1 1 882 61 801 1648 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 30 0 0 0 144 1 0 0] 98 1 410 432 98 15 0 1648 98 2 8 1140850688 131073 1936 0 0 0 5 0 0 0 1936 594 234 240 640 32 234 256 640 0 962 202 208 98 1 1026 1056 98 2 882 1 1 882 61 801 1936 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 30 0 0 0 144 1 0 0] 98 1 410 8 ##(Smalltalk.SUnitToolbar) 98 25 0 1936 98 2 8 1140853708 131137 2208 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 517 0 0 0 2208 2290 2320 8 4294903443 0 234 256 98 10 56507 853766 ##(Smalltalk.ToolbarButton) 56507 0 2208 1 1180998 4 ##(Smalltalk.CommandDescription) 8 #runAll 8 'Run All' 1 1 0 395334 3 ##(Smalltalk.Bitmap) 0 16 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' 0 0 7 882 1857 33 51 56509 2402 56509 0 2208 1 2434 8 #runSelected 8 'Run Selected' 1 1 0 2512 83 56511 2402 56511 0 2208 1 2434 8 #debug 8 'Debug' 1 1 0 2512 15 56645 1115910 ##(Smalltalk.ToolbarIconButton) 56645 0 2208 1 2434 8 #runAndMesureTime 8 'Run and Mesure Time' 1 1 263494 3 ##(Smalltalk.Icon) 0 16 2544 8 'Time.ico' 2592 2498 0 16 0 0 0 0 3 882 33 33 1 56513 1246982 ##(Smalltalk.ToolbarSystemButton) 56513 0 2208 1 2434 8 #reset 8 'Reset' 1 1 0 1 9 98 8 2416 2640 1050118 ##(Smalltalk.ToolbarSeparator) 0 0 2208 3 0 1 2784 3026 0 0 2208 3 0 1 2704 3026 0 0 2208 3 0 1 2944 234 240 98 6 1 119 2896 117 2512 1 0 1 0 882 33 33 882 45 45 0 0 962 202 208 98 2 1026 1056 98 2 882 1 1 882 61 801 2208 1026 8 #updateSizePosted 640 2208 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 30 0 0 0 144 1 0 0] 98 0 1184 0 27 1184 0 27 1184 0 27 1184 0 27 0 410 432 98 15 0 416 98 2 8 1140850688 131073 3344 0 0 0 5 0 0 0 3344 594 234 240 640 32 234 256 98 2 410 8 ##(Smalltalk.MultipleSelectionListView) 98 30 0 3344 98 2 8 1140920393 1025 3472 590662 2 ##(Smalltalk.ListModel) 202 208 640 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 2290 8 4278190080 0 5 265030 4 ##(Smalltalk.Menu) 0 16 98 8 984134 2 ##(Smalltalk.CommandMenuItem) 1 2434 2464 8 'Run All' 9347 1 0 0 0 3714 1 2434 2672 8 'Run Selected' 9383 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 3714 1 2434 2816 8 'Run and Mesure Time' 1 1 0 0 0 3826 4097 3714 1 2434 2736 8 'Debug' 9349 1 0 0 0 3826 4097 3714 1 2434 2976 8 'Reset' 9381 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 3472 0 8 4294903461 459270 ##(Smalltalk.Message) 8 #displayString 98 0 8 ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'Test Cases' 1181 8 #left 4066 8 #methodName 98 0 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 2 1 170 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a methodName <= b methodName]' 8 #[30 105 226 0 226 32 130 106] 4288 4336 7 513 0 0 0 3472 0 3 0 0 8 #report 640 0 131169 0 98 4 0 0 882 1 1 0 962 202 208 98 3 1026 1056 98 2 882 1 1 882 1181 801 3472 1026 8 #contextMenu: 98 1 3680 3472 1026 8 #text: 98 1 8 'Test Cases' 3472 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 78 2 0 0 144 1 0 0] 98 0 1184 0 27 8 'testCases' 0 962 202 208 98 1 1026 1056 98 2 882 1 1 882 1181 801 3344 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 78 2 0 0 144 1 0 0] 98 1 3472 1184 0 27 234 256 640 0 962 202 208 98 1 1026 1056 98 2 882 2719 21 882 1241 871 416 1122 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 16 5 0 0 10 0 0 0 124 7 0 0 189 1 0 0] 98 3 3344 1376 528 1184 0 27 )! !
!JadeSUnitInClassPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

