"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadePreferenceObject
	instanceVariableNames: ''
	classVariableNames: 'GCISession'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePreferenceObject guid: (GUID fromString: '{de90c1cb-ce50-4954-ab40-04d64c17817b}')!
JadePreferenceObject comment: ''!
!JadePreferenceObject categoriesForClass!Unclassified! !
!JadePreferenceObject methodsFor!

gciSession
	^self class gciSession! !
!JadePreferenceObject categoriesFor: #gciSession!public! !

!JadePreferenceObject class methodsFor!

gciSession
	^GCISession!

gciSession: aGciSession

	^GCISession := aGciSession! !
!JadePreferenceObject class categoriesFor: #gciSession!public! !
!JadePreferenceObject class categoriesFor: #gciSession:!public! !

