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

oopOf: anObject

	^Reflection oopOf: anObject.
!

registerOBNotificationsForPlatform: platform clientForwarder: clientForwarder

	super
		registerOBNotificationsForPlatform: platform 
		clientForwarder: clientForwarder.
	platform 
		registerMultipleChoiceClientForwarder: clientForwarder;
		yourself.
! !
!JadeServer64bit24 categoriesFor: #oopOf:!private! !
!JadeServer64bit24 categoriesFor: #registerOBNotificationsForPlatform:clientForwarder:!public! !

