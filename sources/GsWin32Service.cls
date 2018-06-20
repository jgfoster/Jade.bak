"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GsWin32Service
	instanceVariableNames: 'name startup account path creator created version options'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsWin32Service guid: (GUID fromString: '{c6efb87c-a8a3-4e98-a917-61c6a02a96d9}')!
GsWin32Service comment: ''!
!GsWin32Service categoriesForClass!Unclassified! !
!GsWin32Service methodsFor!

account
	^account!

created
	^created!

creator
	^creator!

initializeFrom: aStream

	| stream dict line |
	dict := Dictionary new.
	[
		aStream atEnd not and: [
		(line := aStream nextLine) notEmpty].
	] whileTrue: [
		stream := ReadStream on: line.
		dict
			at: (stream upTo: $:) trimBlanks
			put: stream nextLine trimBlanks.
	].
	name 	:= dict at: 'Name' 		ifAbsent: [''].
	startup 	:= dict at: 'Startup' 	ifAbsent: [''].
	account 	:= dict at: 'Account' 	ifAbsent: [''].
	path 	:= dict at: 'Path' 		ifAbsent: [''].
	creator 	:= dict at: 'Creator' 	ifAbsent: [''].
	created 	:= dict at: 'Created' 	ifAbsent: [''].
	version 	:= dict at: 'Version' 	ifAbsent: [''].
	options 	:= dict at: 'Options' 	ifAbsent: [''].
!

name
	^name!

options
	^options!

path
	^path!

serviceName

	self subclassResponsibility.
!

startup
	^startup!

version
	^version! !
!GsWin32Service categoriesFor: #account!public! !
!GsWin32Service categoriesFor: #created!public! !
!GsWin32Service categoriesFor: #creator!public! !
!GsWin32Service categoriesFor: #initializeFrom:!public! !
!GsWin32Service categoriesFor: #name!public! !
!GsWin32Service categoriesFor: #options!public! !
!GsWin32Service categoriesFor: #path!public! !
!GsWin32Service categoriesFor: #serviceName!public! !
!GsWin32Service categoriesFor: #startup!public! !
!GsWin32Service categoriesFor: #version!public! !

