"Filed out from Dolphin Smalltalk 7"!

JadeLoginShell subclass: #ContactsLoginShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ContactsLoginShell guid: (GUID fromString: '{8d6109e0-73a1-41f4-8afa-ed30a9a140eb}')!
ContactsLoginShell comment: ''!
!ContactsLoginShell categoriesForClass!Unclassified! !
!ContactsLoginShell methodsFor!

initialSetupCode

	^'| segment dict newUser list |
(AllUsers anySatisfy: [:each | each userId = ''Staff'']) ifTrue: [
	System transactionMode: #manualBegin.
	^self
].
segment := Segment newInRepository: SystemRepository.
segment group: ''Publishers'' authorization: #''write''.
segment group: ''Subscribers'' authorization: #''read''.
UserGlobals
	at: #''ContactsSecurityPolicy''
	put: segment.
System commitTransaction ifFalse: [nil error: ''commit failed''].

list := IdentityBag new.
list assignToSegment: segment.
list
		add: (Array with: ''James Foster'' 	with: ''GemTalk VP'' 		with: ''+1 503-766-4714'');
		add: (Array with: ''Timothy Cook''	with: ''Apple CEO'' 			with: ''+1 408-996-1010'');
		add: (Array with: ''Bill Gates'' 		with: ''Microsoft Founder'' 	with: ''+1 800-Microsoft'');
		add: (Array with: ''Larry Ellison''	with: ''Oracle CEO''			with: ''+1 650-506-7000'');
		yourself.
(dict := SymbolDictionary new)
	at: #''ContactsGlobals'' put: dict;
	at: #''ContactsSecurityPolicy'' put: segment;
	at: #ContactList put: list;
	assignToSegment: segment;
	yourself.
(AllUsers userWithId: ''DataCurator'') insertDictionary: dict at: 1.
System commitTransaction ifFalse: [nil error: ''commit failed''].

newUser := AllUsers
	addNewUserWithId: ''Staff''
	password: ''swordfish''. 
newUser addGroup: ''Subscribers''.
((System stoneVersionReport at: #''gsVersion'') first = $6) ifFalse: [
	newUser addPrivilege: ''CodeModification''.
].
newUser insertDictionary: dict at: 1.
System commitTransaction ifFalse: [nil error: ''commit failed''].
System transactionMode: #manualBegin.
'!

onViewOpened

	super onViewOpened.
	self caption: 'Contacts Demo Login'.
!

postLogin: aGciSession

	Keyboard default isShiftDown ifTrue: [
		super postLogin: aGciSession.
	] ifFalse: [
		aGciSession executeString: self initialSetupCode.
		ContactsBrowser showOn: aGciSession.
	].
! !
!ContactsLoginShell categoriesFor: #initialSetupCode!public! !
!ContactsLoginShell categoriesFor: #onViewOpened!public! !
!ContactsLoginShell categoriesFor: #postLogin:!public! !

