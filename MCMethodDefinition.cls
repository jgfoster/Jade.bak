"Filed out from Dolphin Smalltalk 7"!

MCDefinition subclass: #MCMethodDefinition
	instanceVariableNames: 'classIsMeta source category selector className timeStamp'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCMethodDefinition guid: (GUID fromString: '{2FC81943-2846-4631-ABF1-F40BD2DA0274}')!
MCMethodDefinition comment: ''!
!MCMethodDefinition categoriesForClass!Unclassified! !
!MCMethodDefinition methodsFor!

category
	^category!

classIsMeta
	^classIsMeta!

className

	^className , (classIsMeta ifTrue: [' class'] ifFalse: ['']).
!

detailsString

	^category , ' - ' , timeStamp.
!

displayMemo

	^source.
!

displayText

	^category, ' - ' , timeStamp.
!

initialize: aStream

	| size |
	timeStamp:= aStream upTo: Character tab.
	className := aStream upTo: Character tab.
	classIsMeta := (aStream upTo: Character tab) = 'true'.
	category := aStream upTo: Character tab.
	selector := aStream upTo: Character tab.
	size := (aStream upTo: Character tab) asNumber + 1.
	source := (aStream next: size) copyReplacing: Character cr withObject: Character lf.
!

selector
	^selector!

source
	^source!

timeStamp
	^timeStamp! !
!MCMethodDefinition categoriesFor: #category!accessing!public! !
!MCMethodDefinition categoriesFor: #classIsMeta!accessing!public! !
!MCMethodDefinition categoriesFor: #className!accessing!public! !
!MCMethodDefinition categoriesFor: #detailsString!public! !
!MCMethodDefinition categoriesFor: #displayMemo!public! !
!MCMethodDefinition categoriesFor: #displayText!public! !
!MCMethodDefinition categoriesFor: #initialize:!private! !
!MCMethodDefinition categoriesFor: #selector!accessing!public! !
!MCMethodDefinition categoriesFor: #source!accessing!public! !
!MCMethodDefinition categoriesFor: #timeStamp!accessing!public! !

