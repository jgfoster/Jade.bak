"Filed out from Dolphin Smalltalk 7"!

Error subclass: #GsError
	instanceVariableNames: 'gciErrSType gsProcess'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsError guid: (GUID fromString: '{7299eb14-ee00-4bec-8a87-e9ec616fab36}')!
GsError comment: ''!
!GsError categoriesForClass!Unclassified! !
!GsError methodsFor!

defaultAction

	^self reportError.
!

errorReport 

	^gciErrSType.
!

gatherData

	gciErrSType notNil ifTrue: [
		messageText := gciErrSType message.
	].
!

gciErrSType: anObject
	gciErrSType := anObject!

gciSession

	^tag.
!

gsProcess

	gsProcess isNil ifTrue: [
		gsProcess := GsProcess 
			session: self gciSession 
			oopType: self errorReport contextOop.
	].
	^gsProcess.
!

isResumableInGem

	^tag
		serverPerform: #'isResumableCategory:number:context:'
		with: (tag oopTypeWithOop: gciErrSType category)
		with: gciErrSType number
		with: (tag oopTypeWithOop: gciErrSType context).

!

isStackBreakpoint

	^false.
!

processOop 

	^gciErrSType context.
!

reportError

	"The error can occur before code starts executing (see bug #44232)"
	gciErrSType contextOop isGsNil ifTrue: [
		self reportErrorMessage.
		Processor activeProcess terminate.
	].
	[
		^self debugError.
	] on: Error do: [:ex | 
		(ex isKindOf: GsError) ifTrue: [ex pass].
		SessionManager current logError: ex.
		MessageBox warning: 'Error while processing error: ' , ex description.
		SessionManager current pause.
		self reportErrorMessage.
	].
!

reportErrorMessage

	| stream  |
	(stream := WriteStream on: String new)
		nextPutAll: gciErrSType message; cr; cr;
		nextPutAll: (self gciSession printString: gciErrSType contextOop);
		yourself.
	MessageBox 
		errorMsg: stream contents
		caption: 'GemStone Error #' , gciErrSType number printString.
	SessionManager current pause.
	self gciSession terminate: gciErrSType contextOop.
!

signal

	self gatherData.
	^super signal.
!

terminateProcess

	self gciSession terminate: self errorReport contextOop.
	self error: 'We should never get here!!'.
! !
!GsError categoriesFor: #defaultAction!public! !
!GsError categoriesFor: #errorReport!public! !
!GsError categoriesFor: #gatherData!public! !
!GsError categoriesFor: #gciErrSType:!accessing!private! !
!GsError categoriesFor: #gciSession!accessing!public! !
!GsError categoriesFor: #gsProcess!accessing!private! !
!GsError categoriesFor: #isResumableInGem!public! !
!GsError categoriesFor: #isStackBreakpoint!public! !
!GsError categoriesFor: #processOop!public! !
!GsError categoriesFor: #reportError!public! !
!GsError categoriesFor: #reportErrorMessage!public! !
!GsError categoriesFor: #signal!public! !
!GsError categoriesFor: #terminateProcess!accessing!public! !

!GsError class methodsFor!

classToHandle: aGciErrorSType session: aGciSession

	self subclasses do: [:each | | myClass |
		(myClass := each classToHandle: aGciErrorSType session: aGciSession) notNil ifTrue: [^myClass].
	].
	(self wantsToHandle: aGciErrorSType session: aGciSession) ifTrue: [^self].
	^nil.
!

forSession: aGciSession

	self error: 'use #forSession:gciErrSType:'.!

forSession: aGciSession gciErrSType: aGciErrSType

	| aClass |
	[
		aClass := self classToHandle: aGciErrSType session: aGciSession.
	] on: Error do: [:ex | 
		self error: aGciErrSType message.
	].
	^aClass new
		tag: aGciSession;
		gciErrSType: aGciErrSType;
		yourself.
!

signal: aString

	self error: 'use #signalGCI:gciErrSType:'.
!

signal: aString with: anObject

	self error: 'use #signalGCI:gciErrSType:'.
!

signalGCI: aGciSession

	self error: 'use #signalGCI:gciErrSType:'.
!

signalGCI: aGciSession gciErrSType: aGciErrSType

	^(self 
		forSession: aGciSession
		gciErrSType: aGciErrSType)
		signal.
!

signalWith: anObject

	self error: 'use #signalGCI:gciErrSType:'.
!

wantsToHandle: aGciErrorSType session: aGciSession

	^true.
! !
!GsError class categoriesFor: #classToHandle:session:!public! !
!GsError class categoriesFor: #forSession:!public! !
!GsError class categoriesFor: #forSession:gciErrSType:!public! !
!GsError class categoriesFor: #signal:!public! !
!GsError class categoriesFor: #signal:with:!public! !
!GsError class categoriesFor: #signalGCI:!public! !
!GsError class categoriesFor: #signalGCI:gciErrSType:!public! !
!GsError class categoriesFor: #signalWith:!public! !
!GsError class categoriesFor: #wantsToHandle:session:!public! !

