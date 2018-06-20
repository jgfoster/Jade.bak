"Filed out from Dolphin Smalltalk 7"!

JadeServer subclass: #JadeServer32bit
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer32bit guid: (GUID fromString: '{6bd4ac2a-d6a4-438a-9b0b-e050dd50b3a2}')!
JadeServer32bit comment: ''!
!JadeServer32bit categoriesForClass!Unclassified! !
!JadeServer32bit methodsFor!

asString: anObject

	Exception
		category: nil 
		number: nil 
		do: [:ex :cat :num :args | ^'????'].
	^super asString: anObject.
!

installTranscript

	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | ^self].
	super installTranscript.
!

objectForOop: anInteger

	^Object _objectForOop: anInteger
! !
!JadeServer32bit categoriesFor: #asString:!public!Transcript! !
!JadeServer32bit categoriesFor: #installTranscript!public!Transcript! !
!JadeServer32bit categoriesFor: #objectForOop:!public!System Browser! !

!JadeServer32bit class methodsFor!

gsClassDefinitionStringFor: aClass
	"Some class variables exist only in Dolphin and map to globals in GemStone; others exist only in GemStone and map to globals in Dolphin!!"

	^'class subclass: ''' , aClass name , '''
		instVarNames: ' , aClass instVarNames printString , '
		classVars: #(' , aClass classVarsForGemStone , ')
		classInstVars: #()
		poolDictionaries: (#() class withAll: symbolList)
		inDictionary: SymbolDictionary new
		constraints: #()
		instancesInvariant: false
		isModifiable: false.'
! !
!JadeServer32bit class categoriesFor: #gsClassDefinitionStringFor:!public! !

