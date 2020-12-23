"Filed out from Dolphin Smalltalk 7"!

GciSession30 subclass: #GciSession33
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GciSession33 guid: (GUID fromString: '{085e991d-a17a-4475-bae7-c9c88012e5e4}')!
GciSession33 comment: ''!
!GciSession33 categoriesForClass!Unclassified! !
!GciSession33 methodsFor!

attemptSocket

	Keyboard default isShiftDown ifTrue: [
		| host port tempSocket time |
		host := ((gemNRS subStrings: $#) first subStrings: $@) at: 2.
		(port := self serverPerform: #'makeListener') ifNil: [^self].
		tempSocket := Socket port: port host: host.
		tempSocket connectNoWait.
		time := self serverPerform: #'acceptConnection'.
		time ifNil: [
			tempSocket close.
			^self.
		].
		^self become: (GciSessionWithSocket from: self withSocket: tempSocket).
	]!

decode: aString
	"asAnsiString before 3.3.0; asUtf8String from 3.3.0 on."

	^aString asUtf8String!

encode: aString
	"asAnsiString before 3.3.0; asUtf8String from 3.3.0 on."

	^aString asUtf8String!

gsHasPragmas

	^true! !
!GciSession33 categoriesFor: #attemptSocket!Jade!public! !
!GciSession33 categoriesFor: #decode:!public! !
!GciSession33 categoriesFor: #encode:!public! !
!GciSession33 categoriesFor: #gsHasPragmas!public! !

