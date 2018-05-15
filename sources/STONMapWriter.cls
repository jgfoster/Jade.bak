"Filed out from Dolphin Smalltalk 7"!

STONStreamWriter subclass: #STONMapWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STONMapWriter guid: (GUID fromString: '{ef9d3cb7-5e92-4239-b27d-0326fb011723}')!
STONMapWriter comment: ''!
!STONMapWriter categoriesForClass!Unclassified! !
!STONMapWriter methodsFor!

at: key put: value
	first ifTrue: [ first := false ] ifFalse: [ writer mapElementSeparator ].
	writer encodeKey: key value: value
! !
!STONMapWriter categoriesFor: #at:put:!accessing!public! !

