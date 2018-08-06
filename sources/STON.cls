"Filed out from Dolphin Smalltalk 7"!

Object subclass: #STON
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STON guid: (GUID fromString: '{22c8c7aa-152f-45c6-b5f2-9843ed78d322}')!
STON comment: ''!
!STON categoriesForClass!Kernel-Objects! !
!STON class methodsFor!

fromStream: readStream
	^ (self reader on: readStream) next
!

fromString: string
	1 = 0 ifTrue:[self stonOn: self]. "image stripping issues in Dolphin" 
	^self fromStream: string readStream!

jsonWriter
	^STONWriter new
		jsonMode: true;
		yourself!

listClass
	^Array!

mapClass
	^Dictionary!

put: object asJsonOnStream: stream
	(self jsonWriter on: stream) nextPut: object!

put: object asJsonOnStreamPretty: stream
	(self jsonWriter on: stream)
		prettyPrint: true;
		nextPut: object!

put: object onStream: stream
	(self writer on: stream) nextPut: object
!

put: object onStreamPretty: stream
	(self writer on: stream)
		prettyPrint: true;
		nextPut: object!

reader
	^STONReader new allowComplexMapKeys: true!

toJsonString: object
	^String streamContents: [:stream | self put: object asJsonOnStream: stream]!

toJsonStringPretty: object
	^String streamContents: [:stream | self put: object asJsonOnStreamPretty: stream]!

toString: object
	^String streamContents: [:stream | self put: object onStream: stream]!

toStringPretty: object
	^String streamContents: [:stream | self put: object onStreamPretty: stream]!

writer
	^STONWriter new! !
!STON class categoriesFor: #fromStream:!public! !
!STON class categoriesFor: #fromString:!public! !
!STON class categoriesFor: #jsonWriter!public! !
!STON class categoriesFor: #listClass!public! !
!STON class categoriesFor: #mapClass!public! !
!STON class categoriesFor: #put:asJsonOnStream:!public! !
!STON class categoriesFor: #put:asJsonOnStreamPretty:!public! !
!STON class categoriesFor: #put:onStream:!public! !
!STON class categoriesFor: #put:onStreamPretty:!public! !
!STON class categoriesFor: #reader!public! !
!STON class categoriesFor: #toJsonString:!public! !
!STON class categoriesFor: #toJsonStringPretty:!public! !
!STON class categoriesFor: #toString:!public! !
!STON class categoriesFor: #toStringPretty:!public! !
!STON class categoriesFor: #writer!public! !

