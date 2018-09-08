"Filed out from Dolphin Smalltalk 7"!

JadePresenter subclass: #JadeSUnitAbstractPresenter
	instanceVariableNames: 'methodListPresenter textResultPresenter'
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
	methodListPresenter := self add: ListPresenter new name: 'testCases'.
	textResultPresenter := self add: TextPresenter new name: 'textResult'!

reset
	methodListPresenter model do: [:each | each testResult: nil].
	textResultPresenter value: String new.
	methodListPresenter view invalidate.
	textResultPresenter view backcolor: Color face3d!

runTests: anOrderedCollection in: className
	| jadeResult |
	methodListPresenter selection isEmpty ifTrue: [^self].
	jadeResult := JadeTestResult new.
	anOrderedCollection do: 
			[:methodService |
			| result resultArray |
			result := gciSession
						serverPerform: #runTestNamed:in:
						with: methodService selector
						with: className.
			resultArray := result subStrings: ','.
			resultArray first ~= '0'
				ifTrue: 
					[methodService testResult: 'error'.
					jadeResult addError: methodService selector].
			resultArray second ~= '0'
				ifTrue: 
					[methodService testResult: 'failure'.
					jadeResult addFailure: methodService selector].
			resultArray third ~= '0'
				ifTrue: 
					[methodService testResult: 'passed'.
					jadeResult addPassed: methodService selector]].
	self updateResult: jadeResult.
	^jadeResult!

setColorFor: aJadeTestResult
	textResultPresenter value: aJadeTestResult summary.
	aJadeTestResult failures do: 
			[:eachSymbol |
			methodListPresenter model 
				do: [:eachTest | eachTest selector = eachSymbol ifTrue: [eachTest setFailure]]].
	aJadeTestResult errors do: 
			[:eachSymbol |
			methodListPresenter model
				do: [:eachTest | eachTest selector = eachSymbol ifTrue: [eachTest setError]]].
	aJadeTestResult passed do: 
			[:eachSymbol |
			methodListPresenter model
				do: [:eachTest | eachTest selector = eachSymbol ifTrue: [eachTest setPassed]]].
	methodListPresenter view invalidate.
	textResultPresenter view backcolor: aJadeTestResult backgroundColor!

textResultPresenter
	^textResultPresenter!

textResultPresenter: anObject
	textResultPresenter := anObject!

updateResult: jadeResult
	jadeResult setSummary.
	self setColorFor: jadeResult! !
!JadeSUnitAbstractPresenter categoriesFor: #abortTransaction!public! !
!JadeSUnitAbstractPresenter categoriesFor: #commitTransaction!public! !
!JadeSUnitAbstractPresenter categoriesFor: #createComponents!public! !
!JadeSUnitAbstractPresenter categoriesFor: #reset!public! !
!JadeSUnitAbstractPresenter categoriesFor: #runTests:in:!public! !
!JadeSUnitAbstractPresenter categoriesFor: #setColorFor:!public! !
!JadeSUnitAbstractPresenter categoriesFor: #textResultPresenter!accessing!private! !
!JadeSUnitAbstractPresenter categoriesFor: #textResultPresenter:!accessing!private! !
!JadeSUnitAbstractPresenter categoriesFor: #updateResult:!public! !

