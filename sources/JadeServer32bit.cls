"Filed out from Dolphin Smalltalk 7"!

JadeServer subclass: #JadeServer32bit
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer32bit guid: (GUID fromString: '{6BD4AC2A-D6A4-438A-9B0B-E050DD50B3A2}')!
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
		poolDictionaries: #()
		inDictionary: SymbolDictionary new
		constraints: #()
		instancesInvariant: false
		isModifiable: false.'
! !
!JadeServer32bit class categoriesFor: #gsClassDefinitionStringFor:!public! !

