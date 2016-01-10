"Filed out from Dolphin Smalltalk 7"!

JadeServer subclass: #JadeServer64bit
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer64bit guid: (GUID fromString: '{36FD8C46-21B4-4852-977C-1A9889969313}')!
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
!JadeServer64bit categoriesFor: #installTranscript!public!Transcript! !
!JadeServer64bit categoriesFor: #objectForOop:!public!System Browser! !

!JadeServer64bit class methodsFor!

gsClassDefinitionBlock
	"Some class variables exist only in Dolphin and map to globals in GemStone; others exist only in GemStone and map to globals in Dolphin!!"

	^[:aClass | 'class subclass: ''' , aClass name , '''
		instVarNames: ' , aClass instVarNames printString , '
		classVars: #(' , aClass classVarsForGemStone , ')
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: SymbolDictionary new
		instancesInvariant: false
		isModifiable: false.'].
! !
!JadeServer64bit class categoriesFor: #gsClassDefinitionBlock!public! !

