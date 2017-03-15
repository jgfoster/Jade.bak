"Filed out from Dolphin Smalltalk 7"!

GsShellCommand subclass: #PageAudit
	instanceVariableNames: 'stoneName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PageAudit guid: (GUID fromString: '{cb7bb9f1-3586-495f-9e8c-a09d1fdf20f5}')!
PageAudit comment: ''!
!PageAudit categoriesForClass!Unclassified! !
!PageAudit methodsFor!

addArgumentStringTo: aStream

	stoneName notNil ifTrue: [
		aStream space; nextPutAll: stoneName.
	].
	super addArgumentStringTo: aStream.
!

commandName

	^'stone audit'.
!

initialize

	super initialize.
	stoneName := ''.
!

stoneName: aString 

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	stoneName := aString.
! !
!PageAudit categoriesFor: #addArgumentStringTo:!public! !
!PageAudit categoriesFor: #commandName!public! !
!PageAudit categoriesFor: #initialize!public! !
!PageAudit categoriesFor: #stoneName:!accessing!public! !

!PageAudit class methodsFor!

exeName

	^'stoned.exe'.
!

sample1
	^(PageAudit new)
		stoneName: 'gemserver60';
		result! !
!PageAudit class categoriesFor: #exeName!public! !
!PageAudit class categoriesFor: #sample1!public! !

