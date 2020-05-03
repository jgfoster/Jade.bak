"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadePreferenceObject
	instanceVariableNames: ''
	classVariableNames: 'GCISession'
	poolDictionaries: ''
	classInstanceVariableNames: 'default'!
JadePreferenceObject guid: (GUID fromString: '{de90c1cb-ce50-4954-ab40-04d64c17817b}')!
JadePreferenceObject comment: ''!
!JadePreferenceObject categoriesForClass!Unclassified! !
!JadePreferenceObject methodsFor!

default

	^self class default!

gciSession
	^self class gciSession!

openCustomPopUpFor: symbolName ifNone: aNiladicValuable
	"The receiver open a custom popup window for selection <symbolName> if there is no custom popup then evaluate <aNiladicValuable>.
	Subclasses with normal options will follow Dolphin aspect mechanism to set options.
	Subclasses with special options (can no be implemented using Dolphin Aspects mechanism) should rewrite this method  (see JadeAutoCompletationRegistryPreference)."

	^aNiladicValuable value! !
!JadePreferenceObject categoriesFor: #default!public! !
!JadePreferenceObject categoriesFor: #gciSession!public! !
!JadePreferenceObject categoriesFor: #openCustomPopUpFor:ifNone:!custom shell!public! !

!JadePreferenceObject class methodsFor!

default

	default ifNil: [default := self new initialize].

	^default!

gciSession
	^GCISession!

gciSession: aGciSession

	^GCISession := aGciSession! !
!JadePreferenceObject class categoriesFor: #default!public! !
!JadePreferenceObject class categoriesFor: #gciSession!public! !
!JadePreferenceObject class categoriesFor: #gciSession:!public! !

