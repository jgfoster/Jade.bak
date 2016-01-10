"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #GsStackFrame
	instanceVariableNames: 'ipOffset frameOffset stepPoint vars offsets breaks source'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsStackFrame guid: (GUID fromString: '{06285EC9-259C-47B7-980F-C6ECA7CED95E}')!
GsStackFrame comment: ''!
!GsStackFrame categoriesForClass!Unclassified! !
!GsStackFrame methodsFor!

breaks

	^breaks.
!

frameOffset
	^frameOffset!

frameOffset: anObject
	frameOffset := anObject!

initialize: xmlNode

	self error: 'use #initializeXML:'.
!

initializeXML: xmlNode

	super initializeXML: xmlNode.
	ipOffset := (xmlNode getAttribute: 'ipOffset') asNumber.
	frameOffset := (xmlNode getAttribute: 'frameOffset') asNumber.
	stepPoint := (xmlNode getAttribute: 'stepPoint') asNumber.
	vars := OrderedCollection new.
	(xmlNode getElementsByTagName: 'var') do: [:each | 
		vars add: (each getAttribute: 'oop') -> (each getAttribute: 'name')  -> each text.
	].
	offsets := xmlNode getElementsByTagName: 'offset'.
	offsets := offsets collect: [:each | (each getAttribute: 'x') asNumber].
	breaks := xmlNode getElementsByTagName: 'break'.
	breaks := breaks collect: [:each | (each getAttribute: 'x') asNumber].
	source := (xmlNode getElementsByTagName: 'source') first text.
!

ipOffset
	^ipOffset!

ipOffset: anObject
	ipOffset := anObject!

offsets
	^offsets!

source
	^source!

source: anObject
	source := anObject!

stepPoint
	^stepPoint!

stepPoint: anObject
	stepPoint := anObject!

vars
	^vars!

vars: anObject
	vars := anObject! !
!GsStackFrame categoriesFor: #breaks!public! !
!GsStackFrame categoriesFor: #frameOffset!accessing!private! !
!GsStackFrame categoriesFor: #frameOffset:!accessing!private! !
!GsStackFrame categoriesFor: #initialize:!public! !
!GsStackFrame categoriesFor: #initializeXML:!public! !
!GsStackFrame categoriesFor: #ipOffset!accessing!private! !
!GsStackFrame categoriesFor: #ipOffset:!accessing!private! !
!GsStackFrame categoriesFor: #offsets!accessing!private! !
!GsStackFrame categoriesFor: #source!accessing!private! !
!GsStackFrame categoriesFor: #source:!accessing!private! !
!GsStackFrame categoriesFor: #stepPoint!accessing!private! !
!GsStackFrame categoriesFor: #stepPoint:!accessing!private! !
!GsStackFrame categoriesFor: #vars!accessing!private! !
!GsStackFrame categoriesFor: #vars:!accessing!private! !

!GsStackFrame class methodsFor!

codeForStackFrame

^':gsProcess :level | | frame keys values gsMethod stream receiver value |
	Exception
		category: nil
		number: nil
		do: [:ex:cat:num:args | ''?????''].
	frame := gsProcess _frameContentsAt: level.
	stream := WriteStream on: String new.
	gsMethod := frame at: 1.
	stream
		nextPutAll: ''<?xml version=''''1.0'''' ?><frame oop='';
		nextPutAll: frame asOop printString printString;
		nextPutAll: '' ipOffset='';
		nextPutAll: (frame at: 2) printString printString;
		nextPutAll: '' frameOffset='';
		nextPutAll: (frame at: 3) printString printString;
		nextPutAll: '' stepPoint='';
		nextPutAll: ((frame at: 1) _stepPointForIp: (frame at: 2) level: level quick: false) printString printString;
		nextPutAll: ''>''; cr;
		yourself.
	receiver := frame at: 10.
	values := OrderedCollection new.
	((receiver isKindOf: BlockClosure) or: [receiver isKindOf: Class]) ifTrue: [
		keys := OrderedCollection new.
	] ifFalse: [
		keys := receiver class allInstVarNames asOrderedCollection collect: [:each | ''-'' , each].
		1 to: keys size do: [:i |
			values add: (receiver instVarAt: i).
		].
	].
	keys addFirst: #self.
	values addFirst: receiver.
	keys addAll: (frame at: 9).
	values addAll: (frame size >= 11
		ifTrue: [frame copyFrom: 11 to: frame size]
		ifFalse: [#()]).
	1 to: keys size do: [:i |
		value := values at: i.
		value := value printString.
		value size > 500 ifTrue: [value := (value copyFrom: 1 to: 500) , ''...''].
		value := value collect: [:each | 
			(each asciiValue > 31 and: [each asciiValue < 128])
				ifTrue: [each] 
				ifFalse: [$?].
		].
		stream
			nextPutAll: ''<var oop='';
			nextPutAll: (values at: i) asOop asString printString;
			nextPutAll: '' name='';
			nextPutAll: (keys at: i) asString printString;
			nextPutAll: '' ><'';
			nextPutAll: ''!!['';
			nextPutAll: ''CDATA'';
			nextPutAll: ''['';
			nextPutAll: value;
			nextPutAll: '']'';
			nextPutAll: '']'';
			nextPutAll: ''></var>''; cr;
			yourself.
	].
	gsMethod _sourceOffsets do: [:each | 
		stream
			nextPutAll: ''<offset x='';
			nextPutAll: each printString printString;
			nextPutAll: ''/>''; cr;
			yourself.
	].
	stream 
		nextPutAll: ''<source'';
		nextPutAll: '' ><'';
		nextPutAll: ''!!['';
		nextPutAll: ''CDATA'';
		nextPutAll: ''['';
		nextPutAll: gsMethod _sourceString;
		nextPutAll: '']'';
		nextPutAll: '']'';
		nextPutAll: ''></source>'';
		nextPutAll: ''</frame>''; cr;
		yourself.
	stream contents.'
!

xmlBaseName

	^'frame'.
! !
!GsStackFrame class categoriesFor: #codeForStackFrame!public! !
!GsStackFrame class categoriesFor: #xmlBaseName!public! !

