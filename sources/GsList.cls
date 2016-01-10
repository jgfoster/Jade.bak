"Filed out from Dolphin Smalltalk 7"!

GsShellCommand subclass: #GsList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsList guid: (GUID fromString: '{E2BCCE18-152C-4329-BBAA-8EF9FC8F29DA}')!
GsList comment: ''!
!GsList categoriesForClass!Unclassified! !
!GsList methodsFor!

clearLocks
	"removes locks left by servers that have been killed"

	arguments add: '/c'.
!

commandName
	^'gslist'!

exhaustive
	"exhaustive listing with each item on a separate line"

	arguments add: '/x'.
!

help
	"On Windows, the help is sent to the console rather than to standard output"

	arguments add: '/h'.
!

long
	"include pid and port"

	arguments add: '/l'.
!

machine: aString
	"only list servers on machine <aString>; default is the local host"

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/m ' , aString.
!

name: aString
	"only list server <aString>"

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/n ' , aString.
!

pid
	"pid only"

	arguments add: '/p'.
!

quiet
	"exclude header"

	arguments add: '/q'.
!

servers

	| stream list item |
	stream := ReadStream on: (self exhaustive; result).
	list := OrderedCollection new.
	[
		stream atEnd not.
	] whileTrue: [
		item := GsHostProcess fromStream: stream.
		item notNil ifTrue: [list add: item].
	].
	^list.

!

services

	| string stream list item |
	string := self exhaustive; result.
	(string beginsWith: 'gslist[Warning]:') ifTrue: [
		MessageBox warning: string.
		^#().
	].
	stream := ReadStream on: string.
	list := OrderedCollection new.
	[
		stream atEnd not.
	] whileTrue: [
		item := GsHostProcess fromStream: stream.
		item notNil ifTrue: [list add: item].
	].
	^list.

!

timeout: anInteger
	"wait <anInteger> seconds for server to respond (only with /v); default is 2 seconds"

	(anInteger isKindOf: Integer) ifFalse: [self error: 'parameter must be an Integer'].
	arguments add: '/t ' , anInteger printString.
!

user: aString
	"only list servers started by user <aString>"

	(aString isKindOf: String) ifFalse: [self error: 'parameter must be a String'].
	arguments add: '/u ' , aString.
!

verify
	"verify the status of each server"

	arguments add: '/v'.
! !
!GsList categoriesFor: #clearLocks!public! !
!GsList categoriesFor: #commandName!public! !
!GsList categoriesFor: #exhaustive!public! !
!GsList categoriesFor: #help!public! !
!GsList categoriesFor: #long!public! !
!GsList categoriesFor: #machine:!public! !
!GsList categoriesFor: #name:!public! !
!GsList categoriesFor: #pid!public! !
!GsList categoriesFor: #quiet!public! !
!GsList categoriesFor: #servers!public! !
!GsList categoriesFor: #services!public! !
!GsList categoriesFor: #timeout:!public! !
!GsList categoriesFor: #user:!public! !
!GsList categoriesFor: #verify!public! !

!GsList class methodsFor!

exeName

	^'gslist.exe'.
!

servers

	^GsList new servers.
!

services

	^GsList new services.
! !
!GsList class categoriesFor: #exeName!public! !
!GsList class categoriesFor: #servers!public! !
!GsList class categoriesFor: #services!public! !

