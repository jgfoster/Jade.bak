"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadeLog
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'debugLog'!
JadeLog guid: (GUID fromString: '{a8c5b384-c19b-42d4-9c19-5fcf97de74de}')!
JadeLog comment: ''!
!JadeLog categoriesForClass!Unclassified! !
!JadeLog class methodsFor!

debug: aBoolean

	debugLog notNil == aBoolean ifTrue: [^self].
	aBoolean ifTrue: [
		debugLog := FileStream write: 'Jade.log' mode: #append.
		self log: 'Enabled at ' , DateAndTime now printString.
	] ifFalse: [
		self log: 'Disabled at ' , DateAndTime now printString.
		debugLog close. 
		debugLog := nil.
	].!

debugFlag

	^debugLog notNil!

log: aString

	debugLog ifNotNil: [debugLog nextPutAll: Processor activeProcess id printString , ':' , Time millisecondClockValue printString , ':' , aString; cr; flush].
! !
!JadeLog class categoriesFor: #debug:!public! !
!JadeLog class categoriesFor: #debugFlag!public! !
!JadeLog class categoriesFor: #log:!public! !

