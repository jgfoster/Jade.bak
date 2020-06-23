"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GciActivity
	instanceVariableNames: 'index process sessionId time request msStart msSemaphore msResult response window'
	classVariableNames: 'Counter'
	poolDictionaries: ''
	classInstanceVariableNames: 'activity'!
GciActivity guid: (GUID fromString: '{40ea9406-2b07-457f-9e85-9418c45fa80e}')!
GciActivity comment: ''!
!GciActivity categoriesForClass!Unclassified! !
!GciActivity methodsFor!

canBreak

	^msResult isNil
!

enableWindow

	window enable.
!

gotSemaphore

	msSemaphore := Time millisecondClockValue.
	self triggerActivityChanged.
!

index

	^index
!

initializeSessionId: anInteger description: aString window: aView

	Counter ifNil: [Counter := 0].
	index := Counter := Counter + 1.
	sessionId := anInteger.
	time := Time now.
	process := Processor activeProcess id.
	request := aString.
	self class activity add: self.
	self triggerActivityChanged.
	msStart := Time millisecondClockValue.
	window := aView.
!

msForResponse

	^msResult ifNil: [nil] ifNotNil: [msResult - msSemaphore]
!

msForSemaphore

	^msSemaphore ifNil: [nil] ifNotNil: [msSemaphore - msStart]
!

processId

	^process 
!

request

	^self trimString: request 
!

response

	^response
!

result: aString

	msResult := Time millisecondClockValue.
	response := aString.
	self triggerActivityChanged.
!

session: anInteger activity: aString critical: aBlock

	| activityEntry result t1 t2 t3 trimString semaphore |
	t1 := Time millisecondClockValue.
	semaphore critical: [
		t2 := Time millisecondClockValue.
		activityEntry at: 4 put: t2 - t1.
		self triggerActivityChanged.
		anInteger ifNotNil: [self gciSetSessionId: anInteger].
		[[
			result := aBlock value.
			t3 := Time millisecondClockValue.
			activityEntry
				at: 5 put: t3 - t2;
				at: 6 put: (result == self ifTrue: ['self'] ifFalse: [(trimString value: result printString)]);
				yourself.
			self triggerActivityChanged.
		] ifCurtailed: [
			t3 := Time millisecondClockValue.
			activityEntry
				at: 5 put: t3 - t2;
				at: 6 put: 'returned';
				yourself.
			self triggerActivityChanged.
		]
		] on: Error do: [:ex | 
			t3 := Time millisecondClockValue.
			activityEntry
				at: 5 put: t3 - t2;
				at: 6 put: (trimString value: ex description);
				yourself.
			self triggerActivityChanged.
			ex pass.
		]
	].
	^result

!

sessionId

	^sessionId
!

time

	^time
!

triggerActivityChanged

	self class activity trigger: #'changed'.
	SessionManager inputState pumpMessages.
!

trimString: inString

	| outString |
	outString := inString copyFrom: 1 to: (50 min: inString size).
	outString := outString copyReplaceAll: Character cr asString with: ''.
	outString := outString copyReplaceAll: Character lf asString with: '\n'.
	^outString. 
!

writeOn: aStream

	aStream
		print: index; tab;
		print: process; tab;
		print: sessionId; tab;
		print: time; tab;
		print: request; tab;
		print: self msForSemaphore; tab;
		print: self msForResponse; tab;
		print: response; cr.
! !
!GciActivity categoriesFor: #canBreak!public! !
!GciActivity categoriesFor: #enableWindow!public! !
!GciActivity categoriesFor: #gotSemaphore!public! !
!GciActivity categoriesFor: #index!accessing!public! !
!GciActivity categoriesFor: #initializeSessionId:description:window:!public! !
!GciActivity categoriesFor: #msForResponse!accessing!public! !
!GciActivity categoriesFor: #msForSemaphore!accessing!public! !
!GciActivity categoriesFor: #processId!accessing!public! !
!GciActivity categoriesFor: #request!accessing!public! !
!GciActivity categoriesFor: #response!accessing!public! !
!GciActivity categoriesFor: #result:!public! !
!GciActivity categoriesFor: #session:activity:critical:!public! !
!GciActivity categoriesFor: #sessionId!accessing!public! !
!GciActivity categoriesFor: #time!accessing!public! !
!GciActivity categoriesFor: #triggerActivityChanged!public! !
!GciActivity categoriesFor: #trimString:!public! !
!GciActivity categoriesFor: #writeOn:!public! !

!GciActivity class methodsFor!

activity

	activity ifNil: [activity := OrderedCollection new].
	^activity
!

new

	self error: 'use #newWithSessionId:description:'.
!

newWithSessionId: anInteger description: aString window: aView

	^self basicNew
		initializeSessionId: anInteger description: aString window: aView;
		yourself
! !
!GciActivity class categoriesFor: #activity!public! !
!GciActivity class categoriesFor: #new!public! !
!GciActivity class categoriesFor: #newWithSessionId:description:window:!public! !

