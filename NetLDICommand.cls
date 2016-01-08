"Filed out from Dolphin Smalltalk 7"!

GsShellCommand subclass: #NetLDICommand
	instanceVariableNames: 'command'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NetLDICommand guid: (GUID fromString: '{FE6BC418-DBBC-4042-B124-01C2C1D2F276}')!
NetLDICommand comment: ''!
!NetLDICommand categoriesForClass!Unclassified! !
!NetLDICommand methodsFor!

addArgumentStringTo: aStream

	command isNil ifTrue: [self error: 'Missing command!!'].
	aStream space; nextPutAll: command.
	super addArgumentStringTo: aStream.
!

command: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	(#('help' 'create' 'delete' 'start' 'stop' 'query' ) includes: aString) 
		ifFalse: [self error: 'Invalid resource'].
	command := aString.
!

commandName

	^'netldi'.
!

query

	command := 'query'.
	^self result.
!

serviceClass

	^NetLDIService.
! !
!NetLDICommand categoriesFor: #addArgumentStringTo:!public! !
!NetLDICommand categoriesFor: #command:!public! !
!NetLDICommand categoriesFor: #commandName!public! !
!NetLDICommand categoriesFor: #query!public! !
!NetLDICommand categoriesFor: #serviceClass!public! !

!NetLDICommand class methodsFor!

exeName

	^'netldi.exe'.
!

sample1

	^NetLDICommand new query.
!

sample2
	^(NetLDICommand new)
		command: 'help';
		result!

sample3
	^(NetLDICommand new)
		command: 'query';
		help! !
!NetLDICommand class categoriesFor: #exeName!public! !
!NetLDICommand class categoriesFor: #sample1!public! !
!NetLDICommand class categoriesFor: #sample2!public! !
!NetLDICommand class categoriesFor: #sample3!public! !

