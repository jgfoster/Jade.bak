"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GsFile
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsFile guid: (GUID fromString: '{728CD08F-BD29-424A-A492-C0127A9CD064}')!
GsFile comment: ''!
!GsFile categoriesForClass!Unclassified! !
!GsFile class methodsFor!

existsOnServer: aPathName

	^File exists: aPathName.
!

openWriteOnServer: aPathName

	^FileStream write: aPathName.
!

removeServerFile: aPathName

	^File delete: aPathName.
! !
!GsFile class categoriesFor: #existsOnServer:!public! !
!GsFile class categoriesFor: #openWriteOnServer:!public! !
!GsFile class categoriesFor: #removeServerFile:!public! !

