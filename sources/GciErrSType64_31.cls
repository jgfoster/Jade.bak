"Filed out from Dolphin Smalltalk 7"!

GciErrSType subclass: #GciErrSType64_31
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciErrSType64_31 guid: (GUID fromString: '{673bb5d8-2105-4a66-baeb-712876fb246c}')!
GciErrSType64_31 comment: ''!
!GciErrSType64_31 categoriesForClass!Unclassified! !
!GciErrSType64_31 methodsFor!

argCount
	"Answer the receiver's argCount field as a Smalltalk object."

	^(bytes dwordAtOffset: 108)!

argCount: anObject
	"Set the receiver's argCount field to the value of anObject."

	bytes dwordAtOffset: 108 put: anObject!

args

	args ifNotNil: [^args].
	^OopType64Array fromAddress: (bytes yourAddress + 24) length: 10!

category
	"Answer the receiver's category field as a Smalltalk object."

	^(bytes qwordAtOffset: 0)!

category: anObject
	"Set the receiver's category field to the value of anObject."

	bytes qwordAtOffset: 0 put: anObject!

categoryOop

	^OopType64 fromInteger: self category.!

context
	"Answer the receiver's context field as a Smalltalk object."

	^(bytes qwordAtOffset: 8)!

context: anObject
	"Set the receiver's context field to the value of anObject."

	bytes qwordAtOffset: 8 put: anObject!

contextOop

	^OopType64 fromInteger: self context.
!

exceptionObj
	"Answer the receiver's exceptionObj field as a Smalltalk object."

	^(bytes qwordAtOffset: 16)!

exceptionObj: anObject
	"Set the receiver's exceptionObj field to the value of anObject."

	bytes qwordAtOffset: 16 put: anObject!

exceptionObjOop

	^OopType64 fromInteger: self exceptionObj.!

fatal
	"Answer the receiver's fatal field as a Smalltalk object."

	^(bytes byteAtOffset: 112)!

fatal: anObject
	"Set the receiver's fatal field to the value of anObject."

	bytes byteAtOffset: 112 put: anObject!

message
	"Answer the receiver's message field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 113)!

message: anObject
	"Set the receiver's message field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (1024 * 1).
	anObject replaceBytesOf: bytes from: 114 to: 113 + size startingAt: 1.
	bytes at: size+114 put: 0!

number
	"Answer the receiver's number field as a Smalltalk object."

	^(bytes dwordAtOffset: 104)!

number: anObject
	"Set the receiver's number field to the value of anObject."

	bytes dwordAtOffset: 104 put: anObject!

oopTypeClass

	^OopType64.
!

reason
	"Answer the receiver's reason field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 1138)!

reason: anObject
	"Set the receiver's reason field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (1024 * 1).
	anObject replaceBytesOf: bytes from: 1139 to: 1138 + size startingAt: 1.
	bytes at: size+1139 put: 0! !
!GciErrSType64_31 categoriesFor: #argCount!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #argCount:!public! !
!GciErrSType64_31 categoriesFor: #args!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #category!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #category:!public! !
!GciErrSType64_31 categoriesFor: #categoryOop!public! !
!GciErrSType64_31 categoriesFor: #context!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #context:!public! !
!GciErrSType64_31 categoriesFor: #contextOop!public! !
!GciErrSType64_31 categoriesFor: #exceptionObj!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #exceptionObj:!public! !
!GciErrSType64_31 categoriesFor: #exceptionObjOop!public! !
!GciErrSType64_31 categoriesFor: #fatal!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #fatal:!public! !
!GciErrSType64_31 categoriesFor: #message!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #message:!public! !
!GciErrSType64_31 categoriesFor: #number!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #number:!public! !
!GciErrSType64_31 categoriesFor: #oopTypeClass!public! !
!GciErrSType64_31 categoriesFor: #reason!**compiled accessors**!public! !
!GciErrSType64_31 categoriesFor: #reason:!public! !

!GciErrSType64_31 class methodsFor!

defineFields
	"typedef struct {
		OopType			category;												/* error dictionary       */
		OopType			context;												/* GemStone Smalltalk execution state , a GsProcess */
		OopType			exceptionObj;										/* an instance of Exception, or nil; may be nil if error was not signaled from Smalltalk execution */
		OopType			args[GCI_MAX_ERR_ARGS];			/* arguments to error text */
		int					number;												/* GemStone error number  */
		int					argCount;											/* num of arg in the args[]*/
		unsigned char	fatal;													/* nonzero if err is fatal */
		char					message[GCI_ERR_STR_SIZE + 1];	/* null-term. str of err text */		// GCI_ERR_STR_SIZE = 1024
		char					reason[GCI_ERR_reasonSize + 1];	// GCI_ERR_reasonSize = GCI_ERR_STR_SIZE = 1024

  // If you change this struct,  Smalltalk client FFI code may
  //  need smalltalk code changes.
} GciErrSType;

		GciErrSType64_31 compileDefinition
"

	| arrayField messageField reasonField |
	arrayField := ArrayField type: OopType64Array length: 10.
	messageField := StringField length: 1025.
	reasonField := StringField length: 1025.
	self
		defineField: #category 			type: OopType64Field 	new	;
		defineField: #context				type: OopType64Field	new	;
		defineField: #exceptionObj		type: OopType64Field	new	;
		defineField: #args					type: arrayField						;
		defineField: #number				type: DWORDField 		new	;
		defineField: #argCount			type: DWORDField		new	;
		defineField: #fatal					type: BYTEField			new	;
		defineField: #message			type: messageField				;
		defineField: #reason				type: reasonField					;
		yourself.
! !
!GciErrSType64_31 class categoriesFor: #defineFields!public! !

