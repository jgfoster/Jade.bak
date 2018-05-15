"Filed out from Dolphin Smalltalk 7"!

Object subclass: #STON
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STON guid: (GUID fromString: '{22c8c7aa-152f-45c6-b5f2-9843ed78d322}')!
STON comment: ''!
!STON categoriesForClass!Kernel-Objects! !
!STON methodsFor!

fromStream: readStream
	^ (self reader on: readStream) next
!

fromString: string
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
	^STONReader new!

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
!STON categoriesFor: #fromStream:!public! !
!STON categoriesFor: #fromString:!public! !
!STON categoriesFor: #jsonWriter!public! !
!STON categoriesFor: #listClass!public! !
!STON categoriesFor: #mapClass!public! !
!STON categoriesFor: #put:asJsonOnStream:!public! !
!STON categoriesFor: #put:asJsonOnStreamPretty:!public! !
!STON categoriesFor: #put:onStream:!public! !
!STON categoriesFor: #put:onStreamPretty:!public! !
!STON categoriesFor: #reader!public! !
!STON categoriesFor: #toJsonString:!public! !
!STON categoriesFor: #toJsonStringPretty:!public! !
!STON categoriesFor: #toString:!public! !
!STON categoriesFor: #toStringPretty:!public! !
!STON categoriesFor: #writer!public! !

