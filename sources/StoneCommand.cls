"Filed out from Dolphin Smalltalk 7"!

GsShellCommand subclass: #StoneCommand
	instanceVariableNames: 'command stoneName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StoneCommand guid: (GUID fromString: '{90ce9fe9-c045-4cae-929d-5e834b1e310a}')!
StoneCommand comment: ''!
!StoneCommand categoriesForClass!Unclassified! !
!StoneCommand methodsFor!

addArgumentStringTo: aStream

	command isNil ifTrue: [self error: 'Missing command!!'].
	aStream space; nextPutAll: command.
	stoneName notNil ifTrue: [
		aStream space; nextPutAll: stoneName.
	].
	super addArgumentStringTo: aStream.
!

command: aString

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	(#('help' 'create' 'delete' 'start' 'tmpstart' 'stop' 'tmpstop' 'pause' 'continue' 'query' ) includes: aString) 
		ifFalse: [self error: 'Invalid resource'].
	command := aString!

commandName

	^'stone'.
!

help

	arguments add: '/h'.
	^self result.
!

initialize

	super initialize.
	stoneName := ''.
!

query

	command := 'query'.
	^self result.
!

serviceClass

	^StoneService.
!

stoneName: aString 

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	stoneName := aString.
! !
!StoneCommand categoriesFor: #addArgumentStringTo:!public! !
!StoneCommand categoriesFor: #command:!accessing!public! !
!StoneCommand categoriesFor: #commandName!public! !
!StoneCommand categoriesFor: #help!public! !
!StoneCommand categoriesFor: #initialize!public! !
!StoneCommand categoriesFor: #query!public! !
!StoneCommand categoriesFor: #serviceClass!public! !
!StoneCommand categoriesFor: #stoneName:!accessing!public! !

!StoneCommand class methodsFor!

exeName

	^'stoned.exe'.
!

sample1

	^StoneCommand new query.
!

sample2
	^(StoneCommand new)
		command: 'help';
		result!

sample3
	^(StoneCommand new)
		command: 'query';
		help! !
!StoneCommand class categoriesFor: #exeName!public! !
!StoneCommand class categoriesFor: #sample1!public! !
!StoneCommand class categoriesFor: #sample2!public! !
!StoneCommand class categoriesFor: #sample3!public! !

