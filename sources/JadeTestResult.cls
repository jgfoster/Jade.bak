"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadeTestResult
	instanceVariableNames: 'failures errors passed summary'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeTestResult guid: (GUID fromString: '{8b60bb8a-1adb-469b-9149-b9f15f68562e}')!
JadeTestResult comment: ''!
!JadeTestResult categoriesForClass!Unclassified! !
!JadeTestResult methodsFor!

addError: selector

	errors add: selector!

addFailure: selector

	failures add: selector!

addPassed: selector

	passed add: selector!

backgroundColor

	self hasErrors ifTrue: [^Color red].

	self hasWarnings ifTrue: [^Color yellow].

	self hasPassed ifTrue: [^Color darkGreen].

	^Color face3d!

errors
	^errors!

errors: anObject
	errors := anObject!

failures
	^failures!

failures: anObject
	failures := anObject!

hasErrors

	^errors size > 0!

hasPassed

	^(errors size = 0) and: [failures size = 0]!

hasWarnings

	^failures size > 0!

initialize

	super initialize.

	errors := OrderedCollection new.
	failures := OrderedCollection new.
	passed := OrderedCollection new.!

numberOfTests

	^failures size + errors size + passed size!

passed
	^passed!

passed: anObject
	passed := anObject!

setSummary

	summary := 	self numberOfTests printString, ' run, ', passed size printString, ' passed, ', failures size printString, ' failures, ', errors size printString, ' errors'!

summary
	^summary!

summary: anObject
	summary := anObject! !
!JadeTestResult categoriesFor: #addError:!public! !
!JadeTestResult categoriesFor: #addFailure:!public! !
!JadeTestResult categoriesFor: #addPassed:!public! !
!JadeTestResult categoriesFor: #backgroundColor!public! !
!JadeTestResult categoriesFor: #errors!accessing!private! !
!JadeTestResult categoriesFor: #errors:!accessing!private! !
!JadeTestResult categoriesFor: #failures!accessing!private! !
!JadeTestResult categoriesFor: #failures:!accessing!private! !
!JadeTestResult categoriesFor: #hasErrors!public! !
!JadeTestResult categoriesFor: #hasPassed!public! !
!JadeTestResult categoriesFor: #hasWarnings!public! !
!JadeTestResult categoriesFor: #initialize!public! !
!JadeTestResult categoriesFor: #numberOfTests!public! !
!JadeTestResult categoriesFor: #passed!accessing!private! !
!JadeTestResult categoriesFor: #passed:!accessing!private! !
!JadeTestResult categoriesFor: #setSummary!public! !
!JadeTestResult categoriesFor: #summary!accessing!private! !
!JadeTestResult categoriesFor: #summary:!accessing!private! !

!JadeTestResult class methodsFor!

new

	^super new initialize! !
!JadeTestResult class categoriesFor: #new!public! !

