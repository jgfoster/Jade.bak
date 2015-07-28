| package |
package := Package name: 'GemStone Objects'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.052'.

package basicScriptAt: #postinstall put: '''Loaded: GemStone Objects'' yourself.'.

package classNames
	add: #GsGlobal;
	add: #GsMethod;
	add: #GsObject;
	add: #GsProcess;
	add: #GsSession;
	add: #GsStackFrame;
	add: #GsString;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\ActiveX\Components\XML DOM\XML DOM';
	yourself).

package!

"Class Definitions"!

Object subclass: #GsObject
	instanceVariableNames: 'gciSession name oopType'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #GsGlobal
	instanceVariableNames: 'gsClassName data'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #GsMethod
	instanceVariableNames: 'gsBehavior category source instVarReferences package isDirty symbolDictionaryName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #GsProcess
	instanceVariableNames: 'stack type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #GsSession
	instanceVariableNames: 'process host primitive viewAge state transaction hasOldestCR serial id ip priority hostId quietTime lifeTime backlog description objects pages voteState'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #GsStackFrame
	instanceVariableNames: 'ipOffset frameOffset stepPoint vars offsets breaks source'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsObject subclass: #GsString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

GsObject guid: (GUID fromString: '{86A78FBD-348D-46B8-9A1B-E4C91D698EEB}')!
GsObject comment: ''!
!GsObject categoriesForClass!Unclassified! !
!GsObject methodsFor!

<= aGsObject2

	^self name <= aGsObject2 name.
!

= aGsObject2

	^self class = aGsObject2 class and: [self oopType = aGsObject2 oopType].
!

gciSession

	^gciSession.
!

gciSession: aGciSession

	gciSession := aGciSession.
!

hash

	^oopType hash.
!

initialize: aList
	"override if you have something beyond OOP and name"!

initialize: aString session: aGciSession

	| list |
	gciSession := aGciSession.
	list := aString subStrings: Character tab.
	oopType := gciSession oopTypeWithOop: (list at: 1) asNumber.
	name := list at: 2.
	self initialize: list.
!

initializeXML: xmlElement 

	name := xmlElement getAttribute: 'name'.
!

isGsNil

	^oopType isGsNil.
!

name

	^name.
!

oopType

	^oopType.

!

printOn: aStream

	name isNil ifTrue: [^super printOn: aStream].
	aStream nextPutAll: name.
! !
!GsObject categoriesFor: #<=!public! !
!GsObject categoriesFor: #=!public! !
!GsObject categoriesFor: #gciSession!public! !
!GsObject categoriesFor: #gciSession:!public! !
!GsObject categoriesFor: #hash!public! !
!GsObject categoriesFor: #initialize:!public! !
!GsObject categoriesFor: #initialize:session:!public! !
!GsObject categoriesFor: #initializeXML:!public! !
!GsObject categoriesFor: #isGsNil!public! !
!GsObject categoriesFor: #name!public! !
!GsObject categoriesFor: #oopType!public! !
!GsObject categoriesFor: #printOn:!public! !

!GsObject class methodsFor!

fromString: aString session: aGciSession

	^self new 
		initialize: aString
		session:  aGciSession.
!

fromStringXML: aString session: gciSession

	[
		| xmlElement |
		xmlElement := (IXMLDOMDocument new loadText: aString) documentElement.
		xmlElement baseName = self xmlBaseName ifFalse: [self error: ''].
		^self fromXML: xmlElement session: gciSession.
	] on: DOMParseError do: [:ex | 
		MessageBox notify: ex description.
		Keyboard default isShiftDown ifTrue: [ex halt].
		SessionManager current logError: ex.
	].
!

fromXML: xmlElement session: gciSession 

	^self new
		gciSession: gciSession;
		initializeXML: xmlElement; 
		yourself.
!

listFromString: aString session: aGciSession

	| list |
	list := aString subStrings: Character lf.
	list := list collect: [:each | self fromString: each session:  aGciSession].
	^list.
!

xmlBaseName

	^self subclassResponsibility.
! !
!GsObject class categoriesFor: #fromString:session:!public! !
!GsObject class categoriesFor: #fromStringXML:session:!public! !
!GsObject class categoriesFor: #fromXML:session:!public! !
!GsObject class categoriesFor: #listFromString:session:!public! !
!GsObject class categoriesFor: #xmlBaseName!public! !

GsGlobal guid: (GUID fromString: '{E6F20781-FF1E-4553-A0B9-5A06B141A564}')!
GsGlobal comment: ''!
!GsGlobal categoriesForClass!Unclassified! !
!GsGlobal methodsFor!

data

	^data.
!

gsClassName

	^gsClassName.
!

initialize: aList
	"override if you have something beyond OOP and name"

	super initialize: aList.
	gsClassName := aList at: 3.
	data := aList at: 4.
! !
!GsGlobal categoriesFor: #data!public! !
!GsGlobal categoriesFor: #gsClassName!public! !
!GsGlobal categoriesFor: #initialize:!public! !

GsMethod guid: (GUID fromString: '{B1E7FC81-607C-4011-8820-B81AAA95A565}')!
GsMethod comment: ''!
!GsMethod categoriesForClass!Unclassified! !
!GsMethod methodsFor!

<= aGsMethod

	^self printString <= aGsMethod printString.
!

= aGsMethod

	^self class == aGsMethod class and: [
		self gsBehavior = aGsMethod gsBehavior and: [
		self name = aGsMethod name]].
!

category
	^category!

category: anObject
	category := anObject!

changeToCategory: aString

	| string |
	string := '| gsBehavior |
	gsBehavior := ' , gsBehavior , '.
	(gsBehavior categoryNames includes: #' , aString printString , ') ifFalse: [gsBehavior addCategory: ' , aString printString , '].
	gsBehavior
		moveMethod: #' , name printString , ' 
		toCategory: ' , aString printString.
	gciSession executeString: string.
	self category: aString.
	self isDirty: true.
!

changeToPackage: aPackage

	package removeMethod: self.
	aPackage addMethod: self.
!

codeForObject

	^'(' , gsBehavior codeForObject , ' compiledMethodAt: #' , name , ')'.
!

fileOutTo: aStream

	aStream 
		nextPutAll: 'category: ';
		nextPutAll: category printString; lf;
		nextPutAll: gsBehavior methodPrefix;
		nextPutAll: gsBehavior thisClass name; lf;
		nextPutAll: self source replaceCrLfWithLf trimBlanks; lf;
		nextPutAll: '%'; lf.
!

gsBehavior

	^gsBehavior!

gsBehavior: aGsBehavior

	gsBehavior := aGsBehavior.
!

hash

	^self name hash.
!

initialize: xmlElement

	self error: 'use #initializeXML:'.
!

initializeXML: xmlElement

	super initializeXML: xmlElement.
	name := (xmlElement getElementsByTagName: 'selector') first text.
	category := xmlElement getAttribute: 'category'.
	instVarReferences := (xmlElement getElementsByTagName: 'instVar') collect: [:each | each text].
	isDirty := false.
!

instVarReferences
	^instVarReferences!

instVarReferences: anObject
	instVarReferences := anObject!

isDirty
	^isDirty == true.
!

isDirty: anObject
	isDirty := anObject!

package
	^package!

package: anObject
	package := anObject!

printOn: aStream

	gsBehavior printOn: aStream.
	aStream
		nextPutAll: '>>';
		nextPutAll: name.
!

remove

	| string |
	string := gsBehavior codeForObject , ' removeSelector: ' , name printString , ' ifAbsent: []'.
	gciSession executeString: string.
	gsBehavior removeMethod: self.
	package removeMethod: self.
!

source

	| gsCode result |
	source isNil ifTrue: [
		gsCode := self codeForObject , ' sourceString'.
		(result := gciSession executeString: gsCode) isNil ifTrue: [^self].
		source := result.
	].
	^source.
!

source: anObject
	source := anObject!

symbolDictionaryName

	^symbolDictionaryName.
!

symbolDictionaryName: aString

	symbolDictionaryName := aString.
! !
!GsMethod categoriesFor: #<=!public! !
!GsMethod categoriesFor: #=!public! !
!GsMethod categoriesFor: #category!accessing!public! !
!GsMethod categoriesFor: #category:!accessing!public! !
!GsMethod categoriesFor: #changeToCategory:!public! !
!GsMethod categoriesFor: #changeToPackage:!public! !
!GsMethod categoriesFor: #codeForObject!public! !
!GsMethod categoriesFor: #fileOutTo:!public! !
!GsMethod categoriesFor: #gsBehavior!public! !
!GsMethod categoriesFor: #gsBehavior:!public! !
!GsMethod categoriesFor: #hash!public! !
!GsMethod categoriesFor: #initialize:!private! !
!GsMethod categoriesFor: #initializeXML:!private! !
!GsMethod categoriesFor: #instVarReferences!accessing!public! !
!GsMethod categoriesFor: #instVarReferences:!accessing!public! !
!GsMethod categoriesFor: #isDirty!accessing!public! !
!GsMethod categoriesFor: #isDirty:!accessing!public! !
!GsMethod categoriesFor: #package!accessing!public! !
!GsMethod categoriesFor: #package:!accessing!public! !
!GsMethod categoriesFor: #printOn:!public! !
!GsMethod categoriesFor: #remove!public! !
!GsMethod categoriesFor: #source!accessing!public! !
!GsMethod categoriesFor: #source:!accessing!public! !
!GsMethod categoriesFor: #symbolDictionaryName!public! !
!GsMethod categoriesFor: #symbolDictionaryName:!public! !

!GsMethod class methodsFor!

codeToGetMethod 

^':gsClass :aSelector :aStream |
	| compiledMethod |
	compiledMethod := gsClass compiledMethodAt: aSelector.
	aStream 
		nextPut: Character cr; nextPut: Character lf; tab;
		nextPutAll: ''<method oop='';
		nextPutAll: compiledMethod asOop printString printString;
		nextPutAll: '' category='';
		nextPutAll: (gsClass categoryOfSelector: aSelector) asString printString;
		nextPutAll: '' >''.
	compiledMethod instVarsAccessed asSortedCollection do: [:each |
		aStream
			nextPutAll: ''<instVar>'';
			nextPutAll: each;
			nextPutAll: ''</instVar>''.
	].
	aStream
		nextPutAll: ''<selector'';
		nextPutAll: '' ><'';
		nextPutAll: ''!!['';
		nextPutAll: ''CDATA'';
		nextPutAll: ''['';
		nextPutAll: aSelector;
		nextPutAll: '']'';
		nextPutAll: '']'';
		nextPutAll: ''></selector></method>''.
'.
! !
!GsMethod class categoriesFor: #codeToGetMethod!public! !

GsProcess guid: (GUID fromString: '{0CB5149E-F4A3-413C-8AC6-2CAF13E5FB15}')!
GsProcess comment: ''!
!GsProcess categoriesForClass!Unclassified! !
!GsProcess methodsFor!

aboutToDebug
"	Try the following in a workspace.
	nil halt. 2 + 3.
	[nil halt. 2 + 3] value.
	nil pause. 2 + 3.
	[nil pause. 2 + 3] value.
	nil foo. 2 + 3.
	[nil foo. 2 + 3] value.
	nil error: 'foo'. 2 + 3.
	[nil error: 'foo'. 2 + 3] value.
"
	| result |
	result := gciSession 
		serverPerform: #'aboutToDebugProcess:'
		with: self.
	result = oopType ifFalse: [self halt].
!

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
!GsProcess categoriesFor: #aboutToDebug!public! !
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
		aboutToDebug;
		yourself.
!

xmlBaseName

	^'process'.
! !
!GsProcess class categoriesFor: #codeToGetProcess!public! !
!GsProcess class categoriesFor: #session:oop:!public! !
!GsProcess class categoriesFor: #xmlBaseName!public! !

GsSession guid: (GUID fromString: '{15F90553-CCCF-4F17-BF73-85DC91C7F6D0}')!
GsSession comment: ''!
!GsSession categoriesForClass!Unclassified! !
!GsSession methodsFor!

backlog

	^backlog!

description

	^description!

hasOldestCR
	^hasOldestCR!

host
	^host!

hostId

	^hostId!

id
	^id!

initialize: xmlElement 

	self error: 'use #initializeXML:'.
!

initializeXML: xmlElement 

	| value |
	super initializeXML: xmlElement.
	(value := xmlElement getAttribute: 'process'			) notNil ifTrue: [process 		:= value asNumber].
	(value := xmlElement getAttribute: 'host'				) notNil ifTrue: [host 				:= value].
	(value := xmlElement getAttribute: 'primitive'			) notNil ifTrue: [primitive 		:= value. self updatePrimitive].
	(value := xmlElement getAttribute: 'viewAge'			) notNil ifTrue: [viewAge 		:= value asNumber].
	(value := xmlElement getAttribute: 'state'				) notNil ifTrue: [state 			:= value asNumber].
	(value := xmlElement getAttribute: 'transaction'		) notNil ifTrue: [transaction	:= value asNumber].
	(value := xmlElement getAttribute: 'hasOldestCR'	) notNil ifTrue: [hasOldestCR := value].
	(value := xmlElement getAttribute: 'serial'				) notNil ifTrue: [serial 			:= value asNumber].
	(value := xmlElement getAttribute: 'id'					) notNil ifTrue: [id 				:= value asNumber].
	(value := xmlElement getAttribute: 'ip'					) notNil ifTrue: [ip 				:= value].
	(value := xmlElement getAttribute: 'priority'			) notNil ifTrue: [priority 			:= value asNumber].
	(value := xmlElement getAttribute: 'hostId'				) notNil ifTrue: [hostId			:= value asNumber].
	(value := xmlElement getAttribute: 'quietTime'		) notNil ifTrue: [quietTime 	:= value asNumber].
	(value := xmlElement getAttribute: 'lifeTime'			) notNil ifTrue: [lifeTime 		:= value asNumber].
	(value := xmlElement getAttribute: 'backlog'			) notNil ifTrue: [backlog 		:= value asNumber].
	(value := xmlElement getAttribute: 'description'		) notNil ifTrue: [description  	:= value].
	(value := xmlElement getAttribute: 'objects'			) notNil ifTrue: [objects  		:= value asNumber].
	(value := xmlElement getAttribute: 'pages'				) notNil ifTrue: [pages  			:= value asNumber].
	(value := xmlElement getAttribute: 'voteState'		) notNil ifTrue: [voteState 		:= value asNumber].
!

ip
	^ip!

lifeTime

	^self stringFromSeconds: lifeTime!

objects

	^objects!

pages

	^pages!

primitive
	^primitive!

priority
	^priority!

process
	^process!

quietTime

	^self stringFromSeconds: quietTime!

serial
	^serial!

state
	^state!

stringFromSeconds: anInteger

	| x |
	x := anInteger.
	x < 120 ifTrue: [^x printString , ' secs'].
	(x := x // 60) < 120 ifTrue: [^x printString , ' mins'].
	(x := x // 60) < 48 ifTrue: [^x printString , ' hrs'].
	x := x // 24.
	^x printString , ' days'.
!

transaction
	^transaction!

updatePrimitive

	primitive = '431' ifTrue: [primitive := 'MFC'].
!

viewAge

	^self stringFromSeconds: viewAge!

voteState
	^voteState! !
!GsSession categoriesFor: #backlog!accessing!public! !
!GsSession categoriesFor: #description!accessing!public! !
!GsSession categoriesFor: #hasOldestCR!accessing!public! !
!GsSession categoriesFor: #host!accessing!public! !
!GsSession categoriesFor: #hostId!accessing!public! !
!GsSession categoriesFor: #id!accessing!public! !
!GsSession categoriesFor: #initialize:!public! !
!GsSession categoriesFor: #initializeXML:!private! !
!GsSession categoriesFor: #ip!accessing!public! !
!GsSession categoriesFor: #lifeTime!accessing!public! !
!GsSession categoriesFor: #objects!accessing!public! !
!GsSession categoriesFor: #pages!accessing!public! !
!GsSession categoriesFor: #primitive!accessing!public! !
!GsSession categoriesFor: #priority!accessing!public! !
!GsSession categoriesFor: #process!accessing!public! !
!GsSession categoriesFor: #quietTime!accessing!public! !
!GsSession categoriesFor: #serial!accessing!public! !
!GsSession categoriesFor: #state!accessing!public! !
!GsSession categoriesFor: #stringFromSeconds:!accessing!public! !
!GsSession categoriesFor: #transaction!accessing!public! !
!GsSession categoriesFor: #updatePrimitive!public! !
!GsSession categoriesFor: #viewAge!accessing!public! !
!GsSession categoriesFor: #voteState!accessing!public! !

!GsSession class methodsFor!

codeToGetSession

^':id :aStream | | array gsSession |
	Exception
		category: nil
		number: nil
		do: [:ex:cat:num:args | ''?????''].
	array := System descriptionOfSession: id.
	gsSession := GsSession sessionWithSerialNumber: (array at: 9).
	stream
		nextPutAll: ''<session oop='';
		nextPutAll: gsSession asOop printString printString;
		nextPutAll: '' name='';
		nextPutAll: (array at: 1) userId printString;
		nextPutAll: '' process='';
		nextPutAll: (array at: 2) printString printString;
		nextPutAll: '' host='';
		nextPutAll: (array at: 3) printString;
		nextPutAll: '' primitive='';
		nextPutAll: (array at: 4) printString printString;
		nextPutAll: '' time='';
		nextPutAll: (System timeGmt - (array at: 5)) printString printString;
		nextPutAll: '' state='';
		nextPutAll: (array at: 6) printString printString;
		nextPutAll: '' transaction='';
		nextPutAll: (array at: 7) printString printString;
		nextPutAll: '' hasOldestCR='';
		nextPutAll: (array at: 8) printString printString;
		nextPutAll: '' serial='';
		nextPutAll: (array at: 9) printString printString;
		nextPutAll: '' id='';
		nextPutAll: (array at: 10) printString printString;
		nextPutAll: '' ip='';
		nextPutAll: (array at: 11) printString;
		nextPutAll: '' />'';
		yourself.
'.
!

fromXML: xmlElement session: gciSession

	| list |
	list := xmlElement getElementsByTagName: 'session'.
	list := list collect: [:each | 
		super
			fromXML: each 
			session: gciSession.
	].
	^list.
!

xmlBaseName

	^'sessions'.
! !
!GsSession class categoriesFor: #codeToGetSession!public! !
!GsSession class categoriesFor: #fromXML:session:!public! !
!GsSession class categoriesFor: #xmlBaseName!public! !

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

GsString guid: (GUID fromString: '{25DD76E1-50A2-41D5-88A6-75FA0C975700}')!
GsString comment: ''!
!GsString categoriesForClass!Unclassified! !
"Binary Globals"!

