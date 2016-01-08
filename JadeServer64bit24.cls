"Filed out from Dolphin Smalltalk 7"!

JadeServer64bit subclass: #JadeServer64bit24
	instanceVariableNames: ''
	classVariableNames: 'Reflection'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeServer64bit24 guid: (GUID fromString: '{1AF3D6EB-C974-4E19-B3CF-46098CDD8C6D}')!
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

