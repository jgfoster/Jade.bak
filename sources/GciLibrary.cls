"Filed out from Dolphin Smalltalk 7"!

ExternalLibrary subclass: #GciLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciLibrary guid: (GUID fromString: '{ad9a79e0-d6ef-4d77-83fc-015b118b2331}')!
GciLibrary comment: ''!
!GciLibrary categoriesForClass!Unclassified! !
!GciLibrary methodsFor!

abortSession: anInteger

	self subclassResponsibility.
!

beginSession: anInteger

	self subclassResponsibility.
!

commitSession: anInteger

	self subclassResponsibility.
!

errorStructureClass

	self subclassResponsibility.
!

hardBreakSession: anInteger

	self subclassResponsibility.
!

loginHostUser: hostUser hostPassword: hostPassword gsUser: gsUser gsPassword: gsPassword gemNRS: gemString stoneNRS: stoneString

	self subclassResponsibility.
!

logoutSession: anInteger

	self subclassResponsibility.
!

nbResult

	self subclassResponsibility.
!

oopAsciiNul

	self subclassResponsibility.
!

oopAt: anExternalAddress

	self subclassResponsibility.
!

oopClassArray

	self subclassResponsibility.
!

oopClassByteArray

	self subclassResponsibility.
!

oopClassDoubleByteString

	self subclassResponsibility.
!

oopClassQuadByteString

	self subclassResponsibility.
!

oopClassString

	self subclassResponsibility.
!

oopClassSymbol

	self subclassResponsibility.
!

oopClassSystem

	self subclassResponsibility.
!

oopClassUnicode16

	self subclassResponsibility.
!

oopClassUnicode32

	self subclassResponsibility.
!

oopClassUnicode7

	self subclassResponsibility.
!

oopFalse

	self subclassResponsibility.
!

oopForInteger: anInteger

	self subclassResponsibility.
!

oopGemStoneError

	self subclassResponsibility.
!

oopIllegal

	self subclassResponsibility.
!

oopMaxSmallInteger

	self subclassResponsibility.
!

oopMinSmallInteger

	self subclassResponsibility.
!

oopMinusOne

	self subclassResponsibility.
!

oopNil

	self subclassResponsibility.
!

oopOne

	self subclassResponsibility.
!

oopRemoteNil

	self subclassResponsibility.
!

oopTrue

	self subclassResponsibility.
!

oopTwo

	self subclassResponsibility.
!

oopTypeArrayClass

	self subclassResponsibility.
!

oopTypeClass

	self subclassResponsibility.
!

oopTypeWithOop: anInteger

	self subclassResponsibility.
!

oopZero

	self subclassResponsibility.
!

pollForSignalSession: anInteger

	self subclassResponsibility.
!

sendInterpreted: aString to: anOopType with: anArray session: anInteger

	self subclassResponsibility.
!

session: session clearStack: processOop

	self subclassResponsibility.
!

session: session continue: gsProcessOop

	^self 
		session: session 
		continue: gsProcessOop 
		withObject: self oopIllegal
!

session: session continue: gsProcessOop withObject: anOop

	self subclassResponsibility.
!

session: session execute: stringOrOop context: contextOop

	self subclassResponsibility.
!

session: session fetchBytes: anOop

	self subclassResponsibility.
!

session: session oopForInteger: anInteger

	self subclassResponsibility.
!

session: session oopForString: aString

	self subclassResponsibility.
!

session: session releaseOops: oopList

	self subclassResponsibility.
!

session: anInteger send: aString to: anOopType with: anArray 

	self subclassResponsibility.
!

softBreakSession: anInteger

	self subclassResponsibility.
!

version

	self subclassResponsibility.
! !
!GciLibrary categoriesFor: #abortSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #beginSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #commitSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #errorStructureClass!private!subclassResponsibility! !
!GciLibrary categoriesFor: #hardBreakSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #loginHostUser:hostPassword:gsUser:gsPassword:gemNRS:stoneNRS:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #logoutSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #nbResult!public!subclassResponsibility! !
!GciLibrary categoriesFor: #oopAsciiNul!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopAt:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassArray!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassByteArray!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassDoubleByteString!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassQuadByteString!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassString!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassSymbol!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassSystem!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassUnicode16!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassUnicode32!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopClassUnicode7!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopFalse!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopForInteger:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #oopGemStoneError!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopIllegal!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopMaxSmallInteger!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopMinSmallInteger!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopMinusOne!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopNil!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopOne!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopRemoteNil!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopTrue!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopTwo!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopTypeArrayClass!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopTypeClass!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #oopTypeWithOop:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #oopZero!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #pollForSignalSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #sendInterpreted:to:with:session:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:clearStack:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:continue:!public! !
!GciLibrary categoriesFor: #session:continue:withObject:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:execute:context:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:fetchBytes:!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #session:oopForInteger:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:oopForString:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:releaseOops:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #session:send:to:with:!public!Reserved OOPs!subclassResponsibility! !
!GciLibrary categoriesFor: #softBreakSession:!public!subclassResponsibility! !
!GciLibrary categoriesFor: #version!public!subclassResponsibility! !

!GciLibrary class methodsFor!

addMissingMethods
	"The external function address is cached in the method and if it is inherited we will use the first one called.
	Having our own method allows us to have multiple libraries loaded and to call the proper function."

	SessionManager current class name == #'DevelopmentSessionManager' ifTrue: [^self].
	self allSelectors do: [:eachSelector |
		| class method |
		class := self whichClassIncludesSelector: eachSelector.
		class ~~ self ifTrue: [
			method := class methodDictionary at: eachSelector.
			method isExternalCall ifTrue: [
				self addSelector: eachSelector withMethod: method copy clear; yourself.
			].
		].
	].!

displayName

	^nil.
!

fileName

	| list |
	list := OrderedCollection new.
	File
		for: self fileNameSearch
		in: SessionManager current imageBase , 'bin'
		do: [:each | list add: each fileName].
	list := list reject: [:each | each = '.'].	"This is needed after upgrading to Fusion 2 beta 1"
	list isEmpty ifTrue: [self error: 'library not found!!'].
	^list first.
!

fileNameSearch

	self subclassResponsibility.
!

onStartup2

	default := nil.
!

open: pathString

	| currentWorkingDirectory result |
	(default notNil and: [default handle notNil]) ifTrue: [^default].
	self addMissingMethods.
	currentWorkingDirectory := File workingDirectory.
	[
		File workingDirectory: SessionManager current imageBase , 'bin'.
		result := super open: pathString.
		self default: result.
	] ensure: [
		File workingDirectory: currentWorkingDirectory.
	].
	^result.
!

sessionStarted
"
	GciLibrary sessionStarted.
"
	GciErrSType32 new.
	GciErrSType64 new.
	GciErrSType64_30 new.
	GciErrSType64_31 new.
	GciTsObjInfo new.
	OopType32Array new.
	OopType64Array new.
	OopType32 new.
	OopType64 new.
!

withDisplayName: aString

	^self allSubclasses detect: [:each | each displayName = aString].
! !
!GciLibrary class categoriesFor: #addMissingMethods!public! !
!GciLibrary class categoriesFor: #displayName!public! !
!GciLibrary class categoriesFor: #fileName!public! !
!GciLibrary class categoriesFor: #fileNameSearch!public! !
!GciLibrary class categoriesFor: #onStartup2!public! !
!GciLibrary class categoriesFor: #open:!public! !
!GciLibrary class categoriesFor: #sessionStarted!public! !
!GciLibrary class categoriesFor: #withDisplayName:!public! !

