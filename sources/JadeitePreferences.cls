"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JadeitePreferences
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeitePreferences guid: (GUID fromString: '{80b64814-9abd-4128-b728-ba94645715b1}')!
JadeitePreferences comment: ''!
!JadeitePreferences categoriesForClass!Unclassified! !
!JadeitePreferences methodsFor!

transcriptEnabled: booleanString session: session
	"file syntax - transcriptEnabled false"

	| answeringService boolean |
	boolean := booleanString = 'true'.
	answeringService := RowanAnsweringService new.
	(answeringService isTranscriptInstalledIn: session) == boolean
		ifFalse: [answeringService flipTranscriptIn: session]! !
!JadeitePreferences categoriesFor: #transcriptEnabled:session:!accessing!public! !

