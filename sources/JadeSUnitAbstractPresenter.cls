"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #JadeSUnitAbstractPresenter
	instanceVariableNames: 'testCasesPresenter textResultPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSUnitAbstractPresenter guid: (GUID fromString: '{472418d5-e1b6-4d52-aa6f-01de7f19d38f}')!
JadeSUnitAbstractPresenter comment: ''!
!JadeSUnitAbstractPresenter categoriesForClass!Unclassified! !
!JadeSUnitAbstractPresenter methodsFor!

abortTransaction

	gciSession abort!

commitTransaction

	gciSession commit!

createComponents

	super createComponents.

	testCasesPresenter := self add: ListPresenter new name: 'testCases'.
	textResultPresenter := self add: TextPresenter new name: 'textResult'.!

reset

	testCasesPresenter model do: [:each | each result: nil].

	textResultPresenter value: ''.

	testCasesPresenter view invalidate.

	textResultPresenter view backcolor: Color face3d.!

runTests: anOrderedCollection in: testClassName

	^self runTests: anOrderedCollection in: testClassName result: JadeTestResult new!

runTests: anOrderedCollection in: testClassName result: aJadeTestResult

	testCasesPresenter selection isEmpty ifTrue: [^self].
	anOrderedCollection do: 
			[:gsTestMethod |
			| result resultArray |
			result := gciSession
						serverPerform: #runTestNamed:in:
						with: (gciSession encode: gsTestMethod methodName)
						with: testClassName.
			resultArray := result subStrings: ','.
			resultArray first ~= '0'
				ifTrue: 
					[gsTestMethod result: 'error'.
					aJadeTestResult addError: gsTestMethod methodName].
			resultArray second ~= '0'
				ifTrue: 
					[gsTestMethod result: 'failure'.
					aJadeTestResult addFailure: gsTestMethod methodName].
			resultArray third ~= '0'
				ifTrue: 
					[gsTestMethod result: 'passed'.
					aJadeTestResult addPassed: gsTestMethod methodName]].
	aJadeTestResult setSummary.
	self setColorFor: aJadeTestResult.
	^aJadeTestResult!

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
!JadeSUnitAbstractPresenter categoriesFor: #reset!public! !
!JadeSUnitAbstractPresenter categoriesFor: #runTests:in:!public! !
!JadeSUnitAbstractPresenter categoriesFor: #runTests:in:result:!public! !
!JadeSUnitAbstractPresenter categoriesFor: #setColorFor:!public! !

