"Filed out from Dolphin Smalltalk 7"!

JadeServer64bit subclass: #JadeServer64bit24
	instanceVariableNames: ''
	classVariableNames: 'Reflection'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer64bit24 guid: (GUID fromString: '{1af3d6eb-c974-4e19-b3cf-46098cdd8c6d}')!
JadeServer64bit24 comment: ''!
!JadeServer64bit24 categoriesForClass!Unclassified! !
!JadeServer64bit24 methodsFor!

_isClientForwarder: anObject

	^(Reflection classOf: anObject) name == #'ClientForwarder' 
!

gemCacheStatisticsForSessionId: anInteger

^System cacheStatisticsForSessionId: anInteger!

oopOf: anObject

	^Reflection oopOf: anObject.
! !
!JadeServer64bit24 categoriesFor: #_isClientForwarder:!private! !
!JadeServer64bit24 categoriesFor: #gemCacheStatisticsForSessionId:!public!Sessions! !
!JadeServer64bit24 categoriesFor: #oopOf:!private! !

!JadeServer64bit24 class methodsFor!

addRangeStopTo: stepPoints withSource: source
	| aNotAsciiCharIndexes |
	aNotAsciiCharIndexes := OrderedCollection new.
	1 to: source size do:
		[:i |
		(source at: i) isAscii
		ifFalse:
			[aNotAsciiCharIndexes add: i]
		].
	1 to: stepPoints size do: [:stepPoint |
		| range start char length utf8Offset |
		range := (stepPoints at: stepPoint) key.
		start := range start.
		utf8Offset := (aNotAsciiCharIndexes select: [:each | each < start]) size / 2.
		start := start + utf8Offset.
		char := source at: start.
		length := (char isAlphaNumeric or: [char = $_])
			ifTrue: [(source copyFrom: start + 1 to: source size) findFirst: [:eachChar | (eachChar isAlphaNumeric or: [eachChar = $_ or: [eachChar = $:]]) not]]
			ifFalse: [2].
		length = 0 ifTrue: [length := source size - start].
		[
			2 < length and: [(source at: start) = $_].
		] whileTrue: [
			start := start + 1.
			length := length - 1.
		].
		range
			start: start;
			stop: start + length - 1;
			yourself.
	].! !
!JadeServer64bit24 class categoriesFor: #addRangeStopTo:withSource:!public! !

