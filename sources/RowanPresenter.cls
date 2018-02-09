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

	^'RowanProject'.
!

defaultProjectSpecOop
	"one brute force round trip to create a default project spec"
	^gciSession executeString: ' | projectTools projectName projectSpec defaultSymbolDictionary |
	projectTools := self first.
	projectName := self at: 2.
	defaultSymbolDictionary := self last. 
	projectSpec := projectTools newPackageSpecNamed: projectName useGit: true.
	projectSpec 
		comment: ''Default Project''.
	(projectSpec platformSpec at: ''gemstone'') defaultSymbolDictName: defaultSymbolDictionary.
	RwGsImage newOrExistingSymbolDictionaryNamed: defaultSymbolDictionary.
	^projectSpec
	'
	fromContext: (Array with: self projectToolsOop with: self defaultProjectName with: 'Rowan_GUI_Data' with: self defaultSymbolDictionaryName)
	environment: 0.


	!

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
!RowanPresenter categoriesFor: #defaultProjectSpecOop!private! !
!RowanPresenter categoriesFor: #defaultSymbolDictionaryName!private! !
!RowanPresenter categoriesFor: #projectSpecOop!private! !
!RowanPresenter categoriesFor: #projectToolsOop!private! !

