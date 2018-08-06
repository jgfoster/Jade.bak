"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GsShellCommand
	instanceVariableNames: 'arguments'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsShellCommand guid: (GUID fromString: '{272e69bf-5238-421b-bba3-faa16a2a432d}')!
GsShellCommand comment: ''!
!GsShellCommand categoriesForClass!Unclassified! !
!GsShellCommand methodsFor!

addArgumentStringTo: aStream

	arguments do: [:each | aStream space nextPutAll: each].
!

argumentString

	| stream |
	stream := WriteStream on: String new.
	arguments do: [:each | stream nextPutAll: each; space].
	^stream contents.
!

commandName

	self subclassResponsibility.
!

initialize

	arguments := OrderedCollection new.
!

result

	| stream |
	stream := WriteStream on: String new.
	stream nextPutAll: self commandName.
	self addArgumentStringTo: stream.
	^System performOnServer: stream contents.
!

services

	| stream list |
	stream := ReadStream on: self query.
	list := OrderedCollection new.
	[
		stream atEnd not.
	] whileTrue: [
		list add: (self serviceClass new
			initializeFrom: stream;
			yourself).
	].
	^list.
! !
!GsShellCommand categoriesFor: #addArgumentStringTo:!public! !
!GsShellCommand categoriesFor: #argumentString!public! !
!GsShellCommand categoriesFor: #commandName!public! !
!GsShellCommand categoriesFor: #initialize!private! !
!GsShellCommand categoriesFor: #result!public! !
!GsShellCommand categoriesFor: #services!public! !

!GsShellCommand class methodsFor!

executables

	^self find: self exeName.
!

find

	^self find: self exeName.
!

find: aString

	| path list |
	(path := System clientEnvironmentVariable: 'PATH') isNil ifTrue: [^#()].
	list := path subStrings: $;.
	list := list collect: [:each | each , '\' , aString].
	list := list select: [:each | GsFile existsOnServer: each].
	^list.
!

new

	^super new
		initialize;
		yourself.
! !
!GsShellCommand class categoriesFor: #executables!public! !
!GsShellCommand class categoriesFor: #find!public! !
!GsShellCommand class categoriesFor: #find:!public! !
!GsShellCommand class categoriesFor: #new!private! !

