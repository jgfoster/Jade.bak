"Filed out from Dolphin Smalltalk 7"!

MCDefinition subclass: #MCClassDefinition
	instanceVariableNames: 'className definition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCClassDefinition guid: (GUID fromString: '{4d46b46e-f290-4992-93c5-79caee623c11}')!
MCClassDefinition comment: ''!
!MCClassDefinition categoriesForClass!Unclassified! !
!MCClassDefinition methodsFor!

className

	^className.
!

detailsString

	^'Class Definition'.
!

displayMemo

	^definition.
!

displayText

	^'displayText'.
!

initialize: aStream

	| i j |
	definition := (aStream upTo: Character lf) 
		copyReplaceAll: Character cr asString 
		with: Character cr asString , Character lf asString.
	i := definition indexOf: $#.
	j := definition indexOf: Character cr.
	className := definition copyFrom: i + 1 to: j - 1.
!

source

	^self displayMemo! !
!MCClassDefinition categoriesFor: #className!public! !
!MCClassDefinition categoriesFor: #detailsString!public! !
!MCClassDefinition categoriesFor: #displayMemo!public! !
!MCClassDefinition categoriesFor: #displayText!public! !
!MCClassDefinition categoriesFor: #initialize:!public! !
!MCClassDefinition categoriesFor: #source!public! !

