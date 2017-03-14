"Filed out from Dolphin Smalltalk 7"!

Object subclass: #VWMethods
	instanceVariableNames: 'package className isMeta category methods'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VWMethods guid: (GUID fromString: '{2ad4f4de-e467-49de-bbb0-15e24abee6df}')!
VWMethods comment: ''!
!VWMethods categoriesForClass!Unclassified! !
!VWMethods methodsFor!

className
	^className!

initialize: anElement

	methods := OrderedCollection new.
	anElement entitiesDo: [:key :value |
		self processElement: key value: value.
	].
!

package

	^package.
!

printTopazFileInOn: aStream

	aStream
		nextPutAll: 'category: ';
		nextPutAll: category printString; cr;
		yourself.
	methods do: [:each | 
		isMeta ifTrue: [aStream nextPutAll: 'class'].
		aStream
			nextPutAll: 'method: ';
			nextPutAll: className; cr;
			nextPutAll: each; cr;
			nextPut: $%; cr;
			yourself.
	].
!

processBody: anElement

	| text |
	package := anElement attributes at: 'package' ifAbsent: [nil].
	text := anElement elements first string.
	methods add: text.
!

processCategory: anElement

	category isNil ifFalse: [self error: 'Category isNil!!?'].
	category := anElement elements first string.
!

processClassName: anElement

	className isNil ifFalse: [self error: 'Class name is nil!!?'].
	className := anElement elements first string.
	(isMeta := className endsWith: ' class') ifTrue: [
		className := className copyFrom: 1 to: className size - 6.
	].
	(className includes: $.) ifFalse: [^self].
	className := (className subStrings: $.) last.
!

processElement: aKey value: aValue

	aKey = #'class-id' 	ifTrue: [^self processClassName: 	aValue].
	aKey = #'category' 	ifTrue: [^self processCategory: 		aValue].
	aKey = #'body'			ifTrue: [^self processBody:			aValue].
	MessageBox notify: 'Sorry, we are not yet prepared to handle ' , aKey printString , ' (we only support the #''class-id'', #''category'', and #''body'' keys)!!'.
	Keyboard default isShiftDown ifTrue: [self halt].
! !
!VWMethods categoriesFor: #className!public! !
!VWMethods categoriesFor: #initialize:!public! !
!VWMethods categoriesFor: #package!public! !
!VWMethods categoriesFor: #printTopazFileInOn:!public! !
!VWMethods categoriesFor: #processBody:!public! !
!VWMethods categoriesFor: #processCategory:!public! !
!VWMethods categoriesFor: #processClassName:!public! !
!VWMethods categoriesFor: #processElement:value:!public! !

