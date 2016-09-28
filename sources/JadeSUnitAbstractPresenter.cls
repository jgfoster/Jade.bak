"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #JadeSUnitAbstractPresenter
	instanceVariableNames: 'testCasesPresenter textResultPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSUnitAbstractPresenter guid: (GUID fromString: '{472418D5-E1B6-4D52-AA6F-01DE7F19D38F}')!
JadeSUnitAbstractPresenter comment: ''!
!JadeSUnitAbstractPresenter categoriesForClass!Unclassified! !
!JadeSUnitAbstractPresenter methodsFor!

abortTransaction

	self model abort!

commitTransaction

	self model commit!

createComponents

	super createComponents.

	testCasesPresenter := self add: ListPresenter new name: 'testCases'.
	textResultPresenter := self add: TextPresenter new name: 'textResult'.!

model: aGciSession

	super model: aGciSession.!

reset

	testCasesPresenter model do: [:each | each result: nil].

	textResultPresenter value: ''.

	testCasesPresenter view invalidate.

	textResultPresenter view backcolor: Color face3d.!

runTests: anOrderedCollection in: testClassName
	|  jadeResult |

	testCasesPresenter selection isEmpty ifTrue: [^self].

	jadeResult := JadeTestResult new.
	anOrderedCollection do: [:gsTestMethod | | result resultArray | 
		result := self topShell getGciSession serverPerform: #runTestNamed:in: with: gsTestMethod methodName with: testClassName.
		resultArray := result subStrings: ','.
		(resultArray first ~= '0') ifTrue: [gsTestMethod result: 'error'. jadeResult addError: gsTestMethod methodName].
		(resultArray second ~= '0') ifTrue: [gsTestMethod result: 'failure'. jadeResult addFailure: gsTestMethod methodName].
		(resultArray third ~= '0') ifTrue: [gsTestMethod result: 'passed'. jadeResult addPassed: gsTestMethod methodName].].

	jadeResult setSummary.

	self setColorFor: jadeResult.

	^jadeResult!

server
	"Answer a pointer to an instance of GemStone/S class <JadeServer64bit32>"

	^model _server!

setColorFor: aJadeTestResult

	textResultPresenter value: aJadeTestResult summary.
	aJadeTestResult failures do: [:eachSymbol | testCasesPresenter model do: [:eachTest | (eachTest methodName = eachSymbol) ifTrue: [eachTest setFailure]]].
	aJadeTestResult errors do: [:eachSymbol | testCasesPresenter model do: [:eachTest | (eachTest methodName = eachSymbol) ifTrue: [eachTest setError]]].
	aJadeTestResult passed do: [:eachSymbol | testCasesPresenter model do: [:eachTest | (eachTest methodName = eachSymbol) ifTrue: [eachTest setPassed]]].
	testCasesPresenter view invalidate.
	textResultPresenter view backcolor: aJadeTestResult backgroundColor.! !
!JadeSUnitAbstractPresenter categoriesFor: #abortTransaction!public! !
!JadeSUnitAbstractPresenter categoriesFor: #commitTransaction!public! !
!JadeSUnitAbstractPresenter categoriesFor: #createComponents!public! !
!JadeSUnitAbstractPresenter categoriesFor: #model:!public! !
!JadeSUnitAbstractPresenter categoriesFor: #reset!public! !
!JadeSUnitAbstractPresenter categoriesFor: #runTests:in:!public! !
!JadeSUnitAbstractPresenter categoriesFor: #server!public! !
!JadeSUnitAbstractPresenter categoriesFor: #setColorFor:!public! !

