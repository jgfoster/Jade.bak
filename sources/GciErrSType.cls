"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #GciErrSType
	instanceVariableNames: 'args stack'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciErrSType guid: (GUID fromString: '{9932de27-acc2-4e27-ad64-9a3ff8b6ecf9}')!
GciErrSType comment: ''!
!GciErrSType categoriesForClass!Unclassified! !
!GciErrSType methodsFor!

args: anArray

	| list |
	list := anArray asOrderedCollection.
	[list size < 10] whileTrue: [list add: (self oopTypeClass fromInteger: 1)].
	args := list asArray.
!

isApplicationErrorInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 2318].
!

isClientForwarderSendInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 2336].
!

isCompileErrorInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number > 1000 and: [
		self number < 2000]].
!

isDoesNotUnderstandInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 2010].
!

isEventErrorInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number > 6000 and: [
		self number < 7000]].
!

isGemStoneErrorCategoryInSession: aGciSession

	^self categoryOop = aGciSession oopGemStoneError.
!

isHardBreakInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 6004].
!

isInvalidSessionInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 4100].
!

isOverlappingGCIErrorInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 2203].
!

isPauseInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 6001].
!

isRuntimeErrorInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number > 2000 and: [
		self number < 3000]].
!

isSoftBreakInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 6003].
!

isStackBreakpointInSession: aGciSession

	^(self isGemStoneErrorCategoryInSession: aGciSession) and: [
		self number = 6006].
!

number

	self subclassResponsibility.
!

oopTypeClass

	self subclassResponsibility.
!

stack
	^stack.
!

stack: anObject
	stack := anObject.
! !
!GciErrSType categoriesFor: #args:!public! !
!GciErrSType categoriesFor: #isApplicationErrorInSession:!public! !
!GciErrSType categoriesFor: #isClientForwarderSendInSession:!public! !
!GciErrSType categoriesFor: #isCompileErrorInSession:!public! !
!GciErrSType categoriesFor: #isDoesNotUnderstandInSession:!public! !
!GciErrSType categoriesFor: #isEventErrorInSession:!public! !
!GciErrSType categoriesFor: #isGemStoneErrorCategoryInSession:!public! !
!GciErrSType categoriesFor: #isHardBreakInSession:!public! !
!GciErrSType categoriesFor: #isInvalidSessionInSession:!public! !
!GciErrSType categoriesFor: #isOverlappingGCIErrorInSession:!public! !
!GciErrSType categoriesFor: #isPauseInSession:!public! !
!GciErrSType categoriesFor: #isRuntimeErrorInSession:!public! !
!GciErrSType categoriesFor: #isSoftBreakInSession:!public! !
!GciErrSType categoriesFor: #isStackBreakpointInSession:!public! !
!GciErrSType categoriesFor: #number!public! !
!GciErrSType categoriesFor: #oopTypeClass!public! !
!GciErrSType categoriesFor: #stack!public! !
!GciErrSType categoriesFor: #stack:!public! !

