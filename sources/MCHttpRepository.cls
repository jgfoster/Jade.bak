"Filed out from Dolphin Smalltalk 7"!

MCFileBasedRepository subclass: #MCHttpRepository
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MCHttpRepository guid: (GUID fromString: '{140baba7-58f1-4dc6-96f5-510365c3df9c}')!
MCHttpRepository comment: ''!
!MCHttpRepository categoriesForClass!Unclassified! !
!MCHttpRepository methodsFor!

storeVersion: anMCWorkingCopy name: nameString message: messageString from: aMCVersionDialog

	self 
		user: aMCVersionDialog httpUser
		password: aMCVersionDialog httpPassword.
	gciSession
		serverPerform: #'mcStore:name:message:repository:'
		with: anMCWorkingCopy
		with: nameString 
		with: messageString 
		with: self.
!

updateVersionDialogTabIn: aMCVersionDialog

	aMCVersionDialog updateHttpTabWith: self.
!

user: userString password: passwordString

	gciSession
		serverPerform: #'mcHttpRepository:user:password:'
		with: self
		with: userString 
		with: passwordString.
!

userAndPassword

	^gciSession
		serverPerform: #'mcUserAndPasswordInHTTP:' 
		with: self.
! !
!MCHttpRepository categoriesFor: #storeVersion:name:message:from:!public! !
!MCHttpRepository categoriesFor: #updateVersionDialogTabIn:!public! !
!MCHttpRepository categoriesFor: #user:password:!public! !
!MCHttpRepository categoriesFor: #userAndPassword!public! !

