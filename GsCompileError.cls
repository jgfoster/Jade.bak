"Filed out from Dolphin Smalltalk 7"!

GsError subclass: #GsCompileError
	instanceVariableNames: 'list'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsCompileError guid: (GUID fromString: '{E49BE376-E1AF-4368-A429-B691F6E547CA}')!
GsCompileError comment: ''!
!GsCompileError categoriesForClass!Unclassified! !
!GsCompileError methodsFor!

gatherData

	super gatherData.
	list := self class 
		errorListFor: gciErrSType args first 
		inSession: self gciSession.
!

list

	^list.
! !
!GsCompileError categoriesFor: #gatherData!public! !
!GsCompileError categoriesFor: #list!public! !

!GsCompileError class methodsFor!

errorListFor: anOop inSession: aGciSession
 
	| result list |
	result := aGciSession
		serverPerform: #'errorListFor:'
		with: anOop.
	list := result subStrings: Character lf.
	list := list collect: [:each | each , '	' subStrings: Character tab].
	list := list collect: [:each | 
		Array
			with: (each at: 1) asNumber
			with: (each at: 2) asNumber
			with: (each at: 3).
	].
	^list.
!

wantsToHandle: aGciErrorSType session: aGciSession

	^aGciErrorSType isCompileErrorInSession: aGciSession.
! !
!GsCompileError class categoriesFor: #errorListFor:inSession:!public! !
!GsCompileError class categoriesFor: #wantsToHandle:session:!public! !

