"Filed out from Dolphin Smalltalk 7"!

GsObject subclass: #GsSession
	instanceVariableNames: 'process host primitive viewAge state transaction hasOldestCR serial id ip priority hostId quietTime lifeTime backlog description objects pages voteState'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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
	(value := xmlElement getAttribute: 'priority'			) notNil ifTrue: [priority 			:= value = '' ifTrue: [nil] ifFalse: [value asNumber]].
	(value := xmlElement getAttribute: 'hostId'				) notNil ifTrue: [hostId			:= value = '' ifTrue: [nil] ifFalse: [value asNumber]].
	(value := xmlElement getAttribute: 'quietTime'		) notNil ifTrue: [quietTime 	:= value = '' ifTrue: [nil] ifFalse: [value asNumber]].
	(value := xmlElement getAttribute: 'lifeTime'			) notNil ifTrue: [lifeTime 		:= value = '' ifTrue: [nil] ifFalse: [value asNumber]].
	(value := xmlElement getAttribute: 'backlog'			) notNil ifTrue: [backlog 		:= value = '' ifTrue: [nil] ifFalse: [value asNumber]].
	(value := xmlElement getAttribute: 'description'		) notNil ifTrue: [description  	:= value].
	(value := xmlElement getAttribute: 'objects'			) notNil ifTrue: [objects  		:= value = '' ifTrue: [nil] ifFalse: [value asNumber]].
	(value := xmlElement getAttribute: 'pages'				) notNil ifTrue: [pages  			:= value = '' ifTrue: [nil] ifFalse: [value asNumber]].
	(value := xmlElement getAttribute: 'voteState'		) notNil ifTrue: [voteState 		:= value = '' ifTrue: [nil] ifFalse: [value asNumber]].
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

sendSigAbort

	gciSession 
		serverPerform: #'sendSigAbortToSession:'
		with: id.
!

sendSigUsr1

	gciSession 
		serverPerform: #'sendSigUsr1ToSession:'
		with: id.
!

serial
	^serial!

state
	^state!

stopSession

	gciSession 
		serverPerform: #'stopSession:'
		with: id.
!

stringFromSeconds: anInteger

	| x |
	(x := anInteger) ifNil: [^''].
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
!GsSession categoriesFor: #sendSigAbort!accessing!public! !
!GsSession categoriesFor: #sendSigUsr1!accessing!public! !
!GsSession categoriesFor: #serial!accessing!public! !
!GsSession categoriesFor: #state!accessing!public! !
!GsSession categoriesFor: #stopSession!accessing!public! !
!GsSession categoriesFor: #stringFromSeconds:!accessing!public! !
!GsSession categoriesFor: #transaction!accessing!public! !
!GsSession categoriesFor: #updatePrimitive!public! !
!GsSession categoriesFor: #viewAge!accessing!public! !
!GsSession categoriesFor: #voteState!accessing!public! !

!GsSession class methodsFor!

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
!GsSession class categoriesFor: #fromXML:session:!public! !
!GsSession class categoriesFor: #xmlBaseName!public! !

