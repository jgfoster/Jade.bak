"Filed out from Dolphin Smalltalk 7"!

JadeServer subclass: #JadeServer64bit
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer64bit guid: (GUID fromString: '{36fd8c46-21b4-4852-977c-1a9889969313}')!
JadeServer64bit comment: ''!
!JadeServer64bit categoriesForClass!Unclassified! !
!JadeServer64bit methodsFor!

asString: anObject

	^[
		super asString: anObject.
	] on: Error do: [:ex | 
		ex return: '???'.
	].
!

homeMethodFor: aGsMethod

	^[aGsMethod homeMethod] on: Error do: [:ex | ex return: aGsMethod]!

installTranscript

	[
		super installTranscript.
	] on: Error do: [:ex | 
		ex return.
	].
!

objectForOop: anInteger

	^Object _objectForOop: anInteger.
! !
!JadeServer64bit categoriesFor: #asString:!public!Transcript! !
!JadeServer64bit categoriesFor: #homeMethodFor:!public! !
!JadeServer64bit categoriesFor: #installTranscript!public!Transcript! !
!JadeServer64bit categoriesFor: #objectForOop:!public!System Browser! !

!JadeServer64bit class methodsFor!

gsClassDefinitionStringFor: aClass
	"Some class variables exist only in Dolphin and map to globals in GemStone; others exist only in GemStone and map to globals in Dolphin!!"

	^'class subclass: ''' , aClass name , '''
		instVarNames: ' , aClass instVarNames printString , '
		classVars: #(' , aClass classVarsForGemStone , ')
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: SymbolDictionary new
		instancesInvariant: false
		isModifiable: false.'
! !
!JadeServer64bit class categoriesFor: #gsClassDefinitionStringFor:!public! !

