"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GsObject
	instanceVariableNames: 'gciSession name oopType'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

