"Filed out from Dolphin Smalltalk 7"!

LibGciRpc64_3_1_0 subclass: #LibGciRpc64_3_3_0
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LibGciRpc64_3_3_0 guid: (GUID fromString: '{905626b7-882d-4151-8165-7b33d4bc9095}')!
LibGciRpc64_3_3_0 comment: ''!
!LibGciRpc64_3_3_0 categoriesForClass!Unclassified! !
!LibGciRpc64_3_3_0 methodsFor!

decode: aString

	^aString asUtf8String!

encode: aString

	^aString asUtf8String!

fetchBytes: anOopType classOop: oopClass

	oopClass = self oopClassUtf8 ifTrue: [^self fetchBytes: anOopType class: Utf8String].
	^super 
		fetchBytes: anOopType 
		classOop: oopClass!

gciNbExecute_: source _: symbolList _: environmentId
	"GciNbExecute_(OopType source, OopType symbolList, ushort environmentId);"

	<cdecl: OopType64 GciNbExecute_ OopType64 OopType64 word>
	^self invalidCall
!

gciNbExecuteStrFromContextDbg_: source _: sourceSize _: sourceClass _: contextObject _: symbolList _: flags _: environmentId
	"GciNbExecuteStrFromContextDbg_(
		const char source[], 
		int64 sourceSize, 
		OopType sourceClass,
		OopType contextObject, 
		OopType symbolList ,
		int flags /* same as flags to GciPerformNoDebug */, 
		ushort environmentId );"

	<cdecl: OopType64 GciNbExecuteStrFromContextDbg_ lpvoid sqword OopType64 OopType64 OopType64 sdword word>
	^self invalidCall
!

gciNewUtf8String_: utf8data _: nBytes _: convertToUnicode
	"OopType GciNewUtf8String_(const char* utf8data, size_t nBytes, BoolType convertToUnicode);"

	<cdecl: OopType64 GciNewUtf8String_ lpvoid sqword bool>
 	^self invalidCall
!

gsHasPragmas

	^true!

oopClassUtf8

	^OopType64 fromInteger: 154113!

session: session execute: aString context: contextOop environment: anInteger
	"http://kermit.gemtalksystems.com/bug?bug=47634"
	| stringOop utf8 |
	self verifyEnvironment: anInteger.
	utf8 := aString asUtf8String.
	self critical: [
		self gciSetSessionId: session.
		stringOop := self 
			gciNewUtf8String_: utf8 
			_: utf8 size 
			_: 1.
		contextOop ifNil: [
			self gciNbExecute_: stringOop _: self oopNil _: anInteger.
		] ifNotNil: [
			(aString asCharacterCollection allSatisfy: [:each | each codePoint <= 255]) ifFalse: [self error: 'Unable to execute string!!'].
			self
				gciNbExecuteStrFromContextDbg_: utf8 
				_: utf8 size
				_: self oopClassString
				_: contextOop 
				_: self oopNil 
				_: 0 
				_: anInteger.
		].
		^self nbResult
	].
!

session: session oopForString: aString

	| utf8 |
	utf8 := aString asUtf8String.
	self critical: [
		self gciSetSessionId: session.
		^self 
			gciNewUtf8String_: utf8
			_: utf8 size 
			_: false
	].
!

specialFromOop: anOop

	anOop isSmallFraction ifTrue: [
		^anOop asFraction.
	].
	^super specialFromOop: anOop
!

verifyEnvironment: anInteger

	anInteger == 0 ifFalse: [self error: 'Only environment 0 is supported in this version!!!!!!!!'].! !
!LibGciRpc64_3_3_0 categoriesFor: #decode:!public! !
!LibGciRpc64_3_3_0 categoriesFor: #encode:!public! !
!LibGciRpc64_3_3_0 categoriesFor: #fetchBytes:classOop:!public! !
!LibGciRpc64_3_3_0 categoriesFor: #gciNbExecute_:_:_:!private! !
!LibGciRpc64_3_3_0 categoriesFor: #gciNbExecuteStrFromContextDbg_:_:_:_:_:_:_:!private! !
!LibGciRpc64_3_3_0 categoriesFor: #gciNewUtf8String_:_:_:!private! !
!LibGciRpc64_3_3_0 categoriesFor: #gsHasPragmas!public! !
!LibGciRpc64_3_3_0 categoriesFor: #oopClassUtf8!public! !
!LibGciRpc64_3_3_0 categoriesFor: #session:execute:context:environment:!public! !
!LibGciRpc64_3_3_0 categoriesFor: #session:oopForString:!public! !
!LibGciRpc64_3_3_0 categoriesFor: #specialFromOop:!public! !
!LibGciRpc64_3_3_0 categoriesFor: #verifyEnvironment:!public! !

