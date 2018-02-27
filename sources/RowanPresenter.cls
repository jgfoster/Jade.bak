"Filed out from Dolphin Smalltalk 7"!

JadePresenterA subclass: #RowanPresenter
	instanceVariableNames: 'projectToolsOop'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanPresenter guid: (GUID fromString: '{470d7090-f9aa-42cf-95da-55ffdf27f367}')!
RowanPresenter comment: ''!
!RowanPresenter categoriesForClass!Unclassified! !
!RowanPresenter methodsFor!

defaultProjectName
	^'RowanProject'!

defaultSymbolDictionaryName

	^ 'Rowan_GUI_Data'
!

projectSpecOop

	^gciSession
		send: #spec
		to: self projectToolsOop.
!

projectToolsOop
	
	^projectToolsOop ifNil:[projectToolsOop := gciSession executeString: 'Rowan projectTools']! !
!RowanPresenter categoriesFor: #defaultProjectName!private! !
!RowanPresenter categoriesFor: #defaultSymbolDictionaryName!private! !
!RowanPresenter categoriesFor: #projectSpecOop!private! !
!RowanPresenter categoriesFor: #projectToolsOop!private! !

