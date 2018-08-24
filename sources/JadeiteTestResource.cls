"Filed out from Dolphin Smalltalk 7"!

TestResource subclass: #JadeiteTestResource
	instanceVariableNames: 'session transcript'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeiteTestResource guid: (GUID fromString: '{81a2cf60-8ff0-4d5a-95b7-c3e863ced71c}')!
JadeiteTestResource comment: ''!
!JadeiteTestResource categoriesForClass!Unclassified! !
!JadeiteTestResource methodsFor!

isAvailable
	^session notNil and: [transcript notNil]!

isSampleClassPresentIn: aSession
	^
	[aSession
		executeString: '(System myUserProfile resolveSymbol: #' , JadeiteProjectBrowserTestCase sampleClass
				, ') notNil']
			on: Error
			do: [:ex | false]!

session

	^session!

setUp
	super setUp.
	session := GciSession allInstances
				detect: [:each | each isValidSession and: [self isSampleClassPresentIn: each]]
				ifNone: [].
	transcript := RowanTranscript allInstances detect: [:tran | tran gciSession == session] ifNone: []!

transcript

	^transcript! !
!JadeiteTestResource categoriesFor: #isAvailable!public!testing! !
!JadeiteTestResource categoriesFor: #isSampleClassPresentIn:!public!testing! !
!JadeiteTestResource categoriesFor: #session!accessing!public! !
!JadeiteTestResource categoriesFor: #setUp!public! !
!JadeiteTestResource categoriesFor: #transcript!accessing!public! !

!JadeiteTestResource class methodsFor!

isAvailable
	^self current isAvailable!

signalInitializationError
	"TestSuite>>run: in the SUnit pacakge sends this method
	to the class, but implements it on the instance. That appears
	to be a bug."

	self reset.
	^TestResult signalErrorWith: 'Resource ' , self name , ' could not be initialized'! !
!JadeiteTestResource class categoriesFor: #isAvailable!public! !
!JadeiteTestResource class categoriesFor: #signalInitializationError!error handling!public! !

