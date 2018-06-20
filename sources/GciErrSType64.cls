"Filed out from Dolphin Smalltalk 7"!

GciErrSType subclass: #GciErrSType64
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciErrSType64 guid: (GUID fromString: '{04defb15-dfdd-4009-acb0-548eb82e4aea}')!
GciErrSType64 comment: ''!
!GciErrSType64 categoriesForClass!Unclassified! !
!GciErrSType64 methodsFor!

argCount
	"Answer the receiver's argCount field as a Smalltalk object."

	^(bytes dwordAtOffset: 100)!

argCount: anObject
	"Set the receiver's argCount field to the value of anObject."

	bytes dwordAtOffset: 100 put: anObject!

args

	args ifNotNil: [^args].
	^OopType64Array fromAddress: (bytes yourAddress + 16) length: 10!

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

fatal
	"Answer the receiver's fatal field as a Smalltalk object."

	^(bytes byteAtOffset: 104)!

fatal: anObject
	"Set the receiver's fatal field to the value of anObject."

	bytes byteAtOffset: 104 put: anObject!

message
	"Answer the receiver's message field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 105)!

message: anObject
	"Set the receiver's message field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (1024 * 1).
	anObject replaceBytesOf: bytes from: 106 to: 105 + size startingAt: 1.
	bytes at: size+106 put: 0!

number
	"Answer the receiver's number field as a Smalltalk object."

	^(bytes dwordAtOffset: 96)!

number: anObject
	"Set the receiver's number field to the value of anObject."

	bytes dwordAtOffset: 96 put: anObject!

oopTypeClass

	^OopType64.
! !
!GciErrSType64 categoriesFor: #argCount!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #argCount:!public! !
!GciErrSType64 categoriesFor: #args!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #category!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #category:!public! !
!GciErrSType64 categoriesFor: #categoryOop!public! !
!GciErrSType64 categoriesFor: #context!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #context:!public! !
!GciErrSType64 categoriesFor: #contextOop!public! !
!GciErrSType64 categoriesFor: #fatal!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #fatal:!public! !
!GciErrSType64 categoriesFor: #message!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #message:!public! !
!GciErrSType64 categoriesFor: #number!**compiled accessors**!public! !
!GciErrSType64 categoriesFor: #number:!public! !
!GciErrSType64 categoriesFor: #oopTypeClass!public! !

!GciErrSType64 class methodsFor!

defineFields
	"typedef struct {
		OopType			category;                      /* error dictionary       */
		OopType			context;  /* GemStone Smalltalk execution state , a GsProcess */
		OopType			args[GCI_MAX_ERR_ARGS];        /* arguments to error text */
		int				number;                        /* GemStone error number  */
		int				argCount;                      /* num of arg in the args[]*/
		unsigned char	fatal;                         /* nonzero if err is fatal */
		char			message[GCI_ERR_STR_SIZE + 1]; /* null-term. str of err text */
} GciErrSType;

		GciErrSType64 compileDefinition
	"

	| arrayField stringField |
	arrayField := ArrayField type: OopType64Array length: 10.
	stringField := StringField length: 1025.
	self
		defineField: #category 	type: OopType64Field 	new	;
		defineField: #context		type: OopType64Field	new	;
		defineField: #args			type: arrayField						;
		defineField: #number		type: DWORDField 		new	;
		defineField: #argCount	type: DWORDField		new	;
		defineField: #fatal			type: BYTEField			new	;
		defineField: #message	type: stringField						;
		yourself.
! !
!GciErrSType64 class categoriesFor: #defineFields!public! !

