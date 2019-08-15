"Filed out from Dolphin Smalltalk 7"!

JadePreferenceObject subclass: #JadeFileOutEncodingPreference
	instanceVariableNames: ''
	classVariableNames: 'JadeFileOutEncoding'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeFileOutEncodingPreference guid: (GUID fromString: '{0468e6df-2d50-4ee7-b37a-b81ce2f159bd}')!
JadeFileOutEncodingPreference comment: ''!
!JadeFileOutEncodingPreference categoriesForClass!Unclassified! !
!JadeFileOutEncodingPreference class methodsFor!

displayString
	^'File Out Encoding'!

icon
	^String icon!

jadeFileOutEncoding

	JadeFileOutEncoding ifNil: [JadeFileOutEncoding := #utf8].

	^JadeFileOutEncoding!

jadeFileOutEncoding: encodingSymbol

	JadeFileOutEncoding := encodingSymbol!

publishedAspects
	"Answer a <LookupTable> of the <Aspect>s published by the receiver."

	| aspects |
	aspects := super publishedAspects.
	aspects add: (Aspect choice: #jadeFileOutEncoding from: StdioFileStream getEncodingNames).
	^aspects! !
!JadeFileOutEncodingPreference class categoriesFor: #displayString!public! !
!JadeFileOutEncodingPreference class categoriesFor: #icon!public! !
!JadeFileOutEncodingPreference class categoriesFor: #jadeFileOutEncoding!public! !
!JadeFileOutEncodingPreference class categoriesFor: #jadeFileOutEncoding:!public! !
!JadeFileOutEncodingPreference class categoriesFor: #publishedAspects!public! !

