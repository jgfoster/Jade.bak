"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #GsProcess
	instanceVariableNames: 'stack type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsProcess guid: (GUID fromString: '{0CB5149E-F4A3-413C-8AC6-2CAF13E5FB15}')!
GsProcess comment: ''!
!GsProcess categoriesForClass!Unclassified! !
!GsProcess methodsFor!

description

	^gciSession printString: self oop.
!

frameForLevel: anInteger
 
	| string |
	anInteger = 0 ifTrue: [self error: 'No such frame level!!'].
	[
		string := gciSession 
			serverPerform: #'sourceForProcess:frame:'
			with: self 
			with: anInteger.
		(string beginsWith: '?????') ifTrue: [self error: (string copyFrom: 6 to: string size)].
	] on: Error do: [:ex | 
		MessageBox warning: 
'Error when attempting to read stack frame.
Maybe an object got an error on a #printString method.
' , ex description.
		Keyboard default isShiftDown ifTrue: [self halt].
		^nil.
	].
	^GsStackFrame
		fromStringXML: string
		session: gciSession.
!

gciSession: aGciSession 
	gciSession := aGciSession!

oop: anOopType

	oopType := anOopType.
!

printOn: aStream

	aStream
		nextPutAll: 'aGsProcess(';
		print: oopType asInteger;
		nextPutAll: ((type isNil or: [type isEmpty]) ifTrue: [''] ifFalse: [', ' , type]);
		nextPut: $);
		yourself.
!

stack

	stack notNil ifTrue: [^stack].
	self oopType = (gciSession oopTypeFor: nil) ifTrue: [^#()].
	stack := gciSession 
		serverPerform: #'stackForProcess:'
		with: oopType.
	stack := stack
		copyReplaceAll: ' | '
		with: '>>#'.
	stack := stack subStrings: Character lf.
	^stack.
!

type
	^type!

type: anObject
	type := anObject! !
!GsProcess categoriesFor: #description!public! !
!GsProcess categoriesFor: #frameForLevel:!public! !
!GsProcess categoriesFor: #gciSession:!public! !
!GsProcess categoriesFor: #oop:!public! !
!GsProcess categoriesFor: #printOn:!public! !
!GsProcess categoriesFor: #stack!public! !
!GsProcess categoriesFor: #type!accessing!public! !
!GsProcess categoriesFor: #type:!accessing!public! !

!GsProcess class methodsFor!

codeToGetProcess

^'	| list stream |
	stream := WriteStream on: String new.
	stream nextPutAll: ''<?xml version=''''1.0'''' ?><sessions>''.
	list := System currentSessionNames subStrings: Character lf.
	list := list collect: [:each | (each subStrings at: 3) asNumber].
	list do: [:each | 
		[' , GsSession codeToGetSession , '] 
			value: each 
			value: stream.
	].
	stream 
		nextPutAll: ''</sessions>'';
		contents'.

!

session: aSession oop: anOop

	^self new
		gciSession: aSession;
		oop: anOop; 
		yourself.
!

xmlBaseName

	^'process'.
! !
!GsProcess class categoriesFor: #codeToGetProcess!public! !
!GsProcess class categoriesFor: #session:oop:!public! !
!GsProcess class categoriesFor: #xmlBaseName!public! !

