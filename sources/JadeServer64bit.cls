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

_asString: anObject

	^[
		super _asString: anObject.
	] on: Error do: [:ex | 
		ex return: '?<#printString error: ' , ex description , '>?'.
	].
!

_installTranscript

	[
		super _installTranscript.
	] on: Error do: [:ex | 
		ex return.
	].
!

_mcInitialsA: aString

	^[
		super _mcInitialsA: aString.
		true.
	] on: Error do: [:ex | 
		ex return: false.
	].
!

_objectForOop: anInteger

	^Object _objectForOop: anInteger.
!

homeMethodFor: aGsMethod

	^[aGsMethod homeMethod] on: Error do: [:ex | ex return: aGsMethod]! !
!JadeServer64bit categoriesFor: #_asString:!private!Transcript! !
!JadeServer64bit categoriesFor: #_installTranscript!private!Transcript! !
!JadeServer64bit categoriesFor: #_mcInitialsA:!private! !
!JadeServer64bit categoriesFor: #_objectForOop:!private!System Browser! !
!JadeServer64bit categoriesFor: #homeMethodFor:!private! !

!JadeServer64bit class methodsFor!

gsClassDefinitionStringFor: aClass
	"Some class variables exist only in Dolphin and map to globals in GemStone; others exist only in GemStone and map to globals in Dolphin!!"

	^'class subclass: ''' , aClass name , '''
		instVarNames: ' , aClass instVarNames printString , '
		classVars: #(' , aClass classVarsForGemStone , ')
		classInstVars: #()
		poolDictionaries: (#() class withAll: symbolList)
		inDictionary: SymbolDictionary new
		instancesInvariant: false
		isModifiable: false.'
! !
!JadeServer64bit class categoriesFor: #gsClassDefinitionStringFor:!public! !

