"Filed out from Dolphin Smalltalk 7"!

GciErrSType subclass: #GciErrSType32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciErrSType32 guid: (GUID fromString: '{a4559093-6277-4975-a45b-6f8fa8285429}')!
GciErrSType32 comment: 'GciErrSType
	<C: typedef struct {
			OopType category;
			long number;
			OopType context;
			char message[401];
			OopType args[10];
			long argCount;
			BoolType fatal;
		} GciErrSType>'!
!GciErrSType32 categoriesForClass!Unclassified! !
!GciErrSType32 methodsFor!

argCount
	"Answer the receiver's argCount field as a Smalltalk object."

	^(bytes dwordAtOffset: 456)!

argCount: anObject
	"Set the receiver's argCount field to the value of anObject."

	bytes dwordAtOffset: 456 put: anObject!

args
	args ifNotNil: [^args].
	^OopType32Array fromAddress: (bytes yourAddress + 416) length: 10!

category
	"Answer the receiver's category field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

category: anObject
	"Set the receiver's category field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

categoryOop

	^OopType32 fromInteger: self category.
!

context
	"Answer the receiver's context field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

context: anObject
	"Set the receiver's context field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

contextOop

	^OopType32 fromInteger: self context.!

fatal
	"Answer the receiver's fatal field as a Smalltalk object."

	^(bytes dwordAtOffset: 460) asBoolean!

fatal: anObject
	"Set the receiver's fatal field to the value of anObject."

	bytes dwordAtOffset: 460 put: anObject asParameter!

message
	"Answer the receiver's message field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 12)!

message: anObject
	"Set the receiver's message field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (400 * 1).
	anObject replaceBytesOf: bytes from: 13 to: 12 + size startingAt: 1.
	bytes at: size+13 put: 0!

number
	"Answer the receiver's number field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

number: anObject
	"Set the receiver's number field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

oopTypeClass

	^OopType32.
! !
!GciErrSType32 categoriesFor: #argCount!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #argCount:!public! !
!GciErrSType32 categoriesFor: #args!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #category!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #category:!public! !
!GciErrSType32 categoriesFor: #categoryOop!public! !
!GciErrSType32 categoriesFor: #context!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #context:!public! !
!GciErrSType32 categoriesFor: #contextOop!public! !
!GciErrSType32 categoriesFor: #fatal!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #fatal:!public! !
!GciErrSType32 categoriesFor: #message!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #message:!public! !
!GciErrSType32 categoriesFor: #number!**compiled accessors**!public! !
!GciErrSType32 categoriesFor: #number:!public! !
!GciErrSType32 categoriesFor: #oopTypeClass!public! !

!GciErrSType32 class methodsFor!

defineFields
	"	typedef struct {
			OopType category;
			long number;
			OopType context;
			char message[401];
			OopType args[10];
			long argCount;
			BoolType fatal;
		} GciErrSType;

		GciErrSType32 compileDefinition
	"

	| arrayField stringField |
	arrayField := ArrayField type: OopType32Array length: 10.
	stringField := StringField length: 401.
	self
		defineField: #category 	type: OopType32Field 	new	;
		defineField: #number		type: DWORDField 		new	;
		defineField: #context		type: OopType32Field	new	;
		defineField: #message	type: stringField						;
		defineField: #args			type: arrayField						;
		defineField: #argCount	type: DWORDField		new	;
		defineField: #fatal			type: BOOLField			new	;
		yourself.
! !
!GciErrSType32 class categoriesFor: #defineFields!public! !

