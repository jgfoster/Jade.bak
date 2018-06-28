"Filed out from Dolphin Smalltalk 7"!

JadeServer64bit subclass: #JadeServer64bit24
	instanceVariableNames: ''
	classVariableNames: 'Reflection'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer64bit24 guid: (GUID fromString: '{1af3d6eb-c974-4e19-b3cf-46098cdd8c6d}')!
JadeServer64bit24 comment: ''!
!JadeServer64bit24 categoriesForClass!Unclassified! !
!JadeServer64bit24 methodsFor!

_isClientForwarder: anObject

	^(Reflection classOf: anObject) name == #'ClientForwarder' 
!

oopOf: anObject

	^Reflection oopOf: anObject.
! !
!JadeServer64bit24 categoriesFor: #_isClientForwarder:!private! !
!JadeServer64bit24 categoriesFor: #oopOf:!private! !

