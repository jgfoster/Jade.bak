"Filed out from Dolphin Smalltalk 7"!

GsHostProcess subclass: #NetLDI
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NetLDI guid: (GUID fromString: '{80d5192f-b67f-4ec3-b641-8853ba42a584}')!
NetLDI comment: ''!
!NetLDI categoriesForClass!Unclassified! !
!NetLDI methodsFor!

account

	^'??'.
!

applyToLogin: aJadeLogin

	aJadeLogin
		gemHost: 	self gemHost;
		gemService: self gemService;
		gemType: 	self gemType;
		yourself.
!

created

	^'??'.
!

creator

	^'??'.
!

gemHost

	^self gemType = #'remote'
		ifTrue:  [(self name subStrings: $:) first]
		ifFalse: [''].
!

gemService

	^(self name subStrings: $:) last.
!

gemType

	^(self name includes: $:)
		ifTrue:  [#'remote']
		ifFalse: [#'local'].
!

path

	^'??'.
!

processType

	^'NetLDI'.
!

startup

	^'??'.
! !
!NetLDI categoriesFor: #account!public! !
!NetLDI categoriesFor: #applyToLogin:!public! !
!NetLDI categoriesFor: #created!public! !
!NetLDI categoriesFor: #creator!public! !
!NetLDI categoriesFor: #gemHost!public! !
!NetLDI categoriesFor: #gemService!public! !
!NetLDI categoriesFor: #gemType!public! !
!NetLDI categoriesFor: #path!public! !
!NetLDI categoriesFor: #processType!public! !
!NetLDI categoriesFor: #startup!public! !

!NetLDI class methodsFor!

type

	^'Netldi'.
! !
!NetLDI class categoriesFor: #type!public! !

