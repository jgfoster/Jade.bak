"Filed out from Dolphin Smalltalk 7"!

JadeServer64bit3x subclass: #JadeServer64bit32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer64bit32 guid: (GUID fromString: '{B2CB5F19-4347-4B6E-922E-705E99F7CB99}')!
JadeServer64bit32 comment: ''!
!JadeServer64bit32 categoriesForClass!Unclassified! !
!JadeServer64bit32 class methodsFor!

gsClassDefinitionBlock
	"Some class variables exist only in Dolphin and map to globals in GemStone; others exist only in GemStone and map to globals in Dolphin!!"

	^[:aClass | 'class subclass: ''' , aClass name , '''
		instVarNames: ' , aClass instVarNames printString , '
		classVars: #(' , aClass classVarsForGemStone , ')
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: SymbolDictionary new.'].
!

sessionStateCode
	"Avoid deprecated method and still use 'Topaz session state' (since we know Topaz isn't running!!)"

	^'System __sessionStateAt: 3 put: server.'! !
!JadeServer64bit32 class categoriesFor: #gsClassDefinitionBlock!public! !
!JadeServer64bit32 class categoriesFor: #sessionStateCode!public! !

