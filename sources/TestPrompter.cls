"Filed out from Dolphin Smalltalk 7"!

Prompter subclass: #TestPrompter
	instanceVariableNames: ''
	classVariableNames: 'PlannedResult'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestPrompter guid: (GUID fromString: '{b5c4525d-f1b1-422f-97b5-4896d8d2ecf7}')!
TestPrompter comment: ''!
!TestPrompter categoriesForClass!MVP-Presenters! !
!TestPrompter methodsFor!

result

	^self class plannedResult! !
!TestPrompter categoriesFor: #result!public! !

!TestPrompter class methodsFor!

plannedResult
	"test needs to setup expected result each time.
	This helps ensure we don't leave a result hanging around
	causing unknown chaos."

	[^PlannedResult] ensure: [self plannedResult: nil]!

plannedResult: answer
	"tests can insert the desired result to simulate various
	MessageBox behaviors. Class variable is reset to nil after
	each use."

	PlannedResult := answer! !
!TestPrompter class categoriesFor: #plannedResult!public! !
!TestPrompter class categoriesFor: #plannedResult:!public! !

