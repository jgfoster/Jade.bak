"Filed out from Dolphin Smalltalk 7"!

Model subclass: #RowanCommandResult
	instanceVariableNames: 'executionTime'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RowanCommandResult guid: (GUID fromString: '{4ed93b8d-f706-49f3-b224-97cd3ebf739c}')!
RowanCommandResult comment: 'Needs to be in GemStone Objects package (for the moment) so GsObject subclass won''t have cyclical dependencies.'!
!RowanCommandResult categoriesForClass!Unclassified! !
!RowanCommandResult methodsFor!

doNothing: anObject

	"as the name implies"!

prepareForReplication!

updateSelector
	^#doNothing:! !
!RowanCommandResult categoriesFor: #doNothing:!private! !
!RowanCommandResult categoriesFor: #prepareForReplication!public!replication! !
!RowanCommandResult categoriesFor: #updateSelector!public! !

